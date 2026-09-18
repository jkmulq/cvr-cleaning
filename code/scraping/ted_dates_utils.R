# Shared helpers for the TED date-panel chain, sourced by:
#   ted_dates_1_fetch.R    (staged XML fetching: award -> competition -> planning)
#   ted_dates_2_lineage.R  (assemble notice_links from the cached XMLs)
#   ted_dates_3_extract.R  (extract every date, tied back to tender/lot)
#
# NOT run on its own - sourced by the above. Loads config + the award scraper's fetch
# machinery (ted_1_extract_notices.R), defines the per-level cache paths, the
# prior-publication parser shared by every hop, and a cache-first parallel fetch.
#
# The two hops both use the same prior-publication reference in the TED XML:
#   (1) newer F03/F14:  <NOTICE_NUMBER_OJ>2022/S 036-094091</NOTICE_NUMBER_OJ>
#   (2) older forms:    <REF_NOTICE> ... <NO_DOC_OJS>2010/S 117-176699</NO_DOC_OJS> ... </REF_NOTICE>
#   (3) eForms (2024+): cac:TenderingProcess/cac:NoticeDocumentReference/cbc:ID, as either an OJS
#       publication-number (directly fetchable) or a notice-id-ref UUID. UUIDs are mapped UUID -> OJS via
#       the TED v3 search API (query by notice-identifier), cached to lineage_id_map.rds. See extract_prior()
#       + resolve_notice_ids() below. (Before this, eForms notices linked to nothing, so all 2024+ non-winner
#       rows -- which are ~100% eForms -- had no competition/planning dates.)

source("config.R")
# SKIP_TED_RUN loads 2_extract's functions + config without running its pipeline:
# fetch_notice_xml, derive_notice_id, ted_dir, cache_dir, n_workers, max_retries,
# base_delay, and the packages (xml2/httr/dplyr/furrr/progressr).
SKIP_TED_RUN <- TRUE
source(file.path(PROJECT_DIR, "code", "scraping", "ted_1_extract_notices.R"))
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(xml2)       # eForms prior-ref parsing (namespaced UBL)
  library(jsonlite)   # TED search API (UUID -> OJS resolution)
}))

# eForms UBL namespaces (2024+ notices). The prior-publication ref lives at a
# namespaced XPath, not an uppercase TED tag, so the legacy grab1() finds nothing.
.EF_NS <- c(cbc = "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2",
            cac = "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2")

# ── Per-level cache dirs + output paths ───────────────────────────────────────
award_cache_dir    <- cache_dir                              # = ted_dir/raw_xml
comp_cache_dir     <- file.path(ted_dir, "competition_xml")
planning_cache_dir <- file.path(ted_dir, "planning_xml")
links_rds <- file.path(ted_dir, "notice_links.rds")
links_csv <- file.path(ted_dir, "notice_links.csv")
for (d in c(ted_dir, award_cache_dir, comp_cache_dir, planning_cache_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ── KFST award map: (tender_id, lot_id, award_url, award_notice_id) from the raw KFST xlsx ─────
# Mirrors the OpenTender award source so the date chain can cover KFST notices too. Reads only the
# tender/lot/award-url columns from sheet "2.0 Udbudsdata". Returns an empty table if the file is absent.
kfst_award_map <- function() {
  empty <- data.table(tender_id = character(), lot_id = character(),
                      award_url = character(), award_notice_id = character())
  xlsx <- file.path(dirs$raw_data, "kfst", "udbudsdata_kfst.xlsx")
  if (!file.exists(xlsx)) return(empty)
  sheets <- readxl::excel_sheets(xlsx)
  # Read one KFST sheet's (tender_id, lot_id, award_url), prefixing the ids. The 2.1
  # Profylaksebekendtgørelser (direct-award) sheet numbers its ids independently from 1, so its
  # tender_id/lot_id are "P"-namespaced -- EXACTLY as in the 1_1 bind (code/processing/1_1_process_kfst.R)
  # -- so the panel keys line up with the cleaned KFST rows on the (tender_id, lot_id) join.
  read_sheet <- function(pattern, prefix) {
    nm <- grep(pattern, sheets, value = TRUE)[1]
    if (is.na(nm)) return(empty[, .(tender_id, lot_id, award_url)])
    d <- as.data.table(readxl::read_excel(xlsx, sheet = nm, col_types = "text"))
    d <- d[, .(tender_id = paste0(prefix, `Løbenummer`),
               lot_id    = paste0(prefix, `Nummerplade`),
               award_url = `Link til bekendtgørelse om indgået kontrakt`)]
    d[!is.na(award_url) & trimws(award_url) != ""]
  }
  k <- rbindlist(list(read_sheet("^2\\.0 Udbudsdata", ""),   # ordinary tenders
                      read_sheet("Profylakse",        "P")))  # direct awards
  if (!nrow(k)) return(empty)
  k[, award_notice_id := derive_notice_id(award_url)]
  unique(k[!is.na(award_notice_id)], by = c("tender_id", "lot_id", "award_notice_id"))
}

# ── Award universe: distinct award-notice ids + URLs from OpenTender AND KFST ──────────────────
# Used by BOTH ted_dates_1_fetch and ted_dates_2_lineage so they agree on the set (and on
# NOTICE_LINEAGE_SAMPLE_SIZE). Notices shared by the two sources are deduped by award_notice_id, so
# each is fetched/linked once (no double-pull; fetch_notices() is cache-first on top of that).
award_universe <- function() {
  raw_dir <- file.path(dirs$raw_data, "OpenTender")
  files   <- list.files(raw_dir, pattern = "[.]csv$", full.names = TRUE)
  u <- rbindlist(lapply(files, fread, sep = ";",
                        select = "tender_publications_lastContractAwardUrl", colClasses = "character"))
  setnames(u, "award_url")
  u <- u[!is.na(award_url) & trimws(award_url) != ""]
  u[, award_notice_id := derive_notice_id(award_url)]
  u <- u[!is.na(award_notice_id), .(award_url, award_notice_id)]
  # Union in the KFST award notices (they overlap OT heavily; dedup keeps each notice once).
  u <- unique(rbindlist(list(u, kfst_award_map()[, .(award_url, award_notice_id)])),
              by = "award_notice_id")
  sample_n <- suppressWarnings(as.integer(Sys.getenv("NOTICE_LINEAGE_SAMPLE_SIZE", "")))
  if (!is.na(sample_n) && sample_n > 0L) {
    u <- head(u, sample_n)
    message(sprintf("NOTICE_LINEAGE_SAMPLE_SIZE set: limiting to %d award notices", nrow(u)))
  }
  u[]
}

# ── Parsing helpers ───────────────────────────────────────────────────────────
grab1 <- function(txt, tag) {                       # first <TAG>value</TAG>
  m <- regmatches(txt, regexec(sprintf("<%s[^>]*>\\s*([^<]+?)\\s*</%s>", tag, tag), txt, perl = TRUE))[[1]]
  if (length(m) >= 2) m[2] else NA_character_
}
ojs_to_id <- function(s) {                          # "2022/S 036-094091" -> "094091-2022"
  m <- regmatches(s, regexec("([0-9]{4})/S[^-]*-([0-9]{4,7})", s))[[1]]
  if (length(m) == 3) sprintf("%s-%s", m[3], m[2]) else NA_character_
}
read_txt <- function(nid, cache) {                  # cached notice XML as text ("" if absent)
  f <- file.path(cache, paste0(nid, ".xml"))
  if (!file.exists(f) || file.info(f)$size == 0) return("")
  tryCatch(readChar(f, file.info(f)$size, useBytes = TRUE), error = function(e) "")
}
extract_prior <- function(txt) {                    # two-tier prior-publication id, or NA
  if (!nzchar(txt)) return(NA_character_)
  # eForms (2024+): the prior notice is cited at cac:TenderingProcess/cac:NoticeDocumentReference/cbc:ID,
  # as EITHER an OJS publication-number (nnnn-yyyy, directly fetchable) OR a notice-id-ref UUID (needs the
  # TED API to map UUID -> publication-number). We return the RAW ref here; resolve_notice_ids() turns it
  # into a fetchable OJS id (UUIDs resolved via the API, OJS numbers passed through). Legacy TED_EXPORT
  # notices carry an OJS ref directly and fall through to the uppercase-tag path below.
  if (grepl("urn:oasis:names:specification:ubl:schema:xsd:", txt, fixed = TRUE)) {
    x <- tryCatch(read_xml(charToRaw(txt)), error = function(e) NULL)   # charToRaw: honour UTF-8 (Danish chars)
    if (is.null(x)) return(NA_character_)
    n <- xml_find_first(x, "//cac:TenderingProcess/cac:NoticeDocumentReference/cbc:ID", .EF_NS)
    if (is.na(n)) return(NA_character_)
    ref <- trimws(xml_text(n))
    return(if (nzchar(ref)) ref else NA_character_)
  }
  val <- grab1(txt, "NOTICE_NUMBER_OJ")
  if (is.na(val)) {
    rn <- regmatches(txt, regexpr("(?s)<REF_NOTICE[^>]*>.*?</REF_NOTICE>", txt, perl = TRUE))
    if (length(rn) && nzchar(rn)) val <- grab1(rn, "NO_DOC_OJS")
  }
  if (is.na(val)) NA_character_ else ojs_to_id(val)
}
is_direct_award <- function(txt) {                  # awarded without a call for competition
  grepl("PT_AWARD_CONTRACT_WITHOUT_CALL|PT_NEGOTIATED_WITHOUT_PUBLICATION|AWARD_WITHOUT_PRIOR_PUBLICATION|neg-wo-call", txt)
}
prior_ref_id <- function(nid, cache) {              # prior id for one cached notice, self-ref dropped
  pid <- extract_prior(read_txt(nid, cache))
  if (!is.na(pid) && identical(pid, nid)) NA_character_ else pid
}
map_priors <- function(ids, cache) {                # prior id for each of `ids`
  if (!length(ids)) return(character(0))
  vapply(ids, prior_ref_id, character(1), cache = cache, USE.NAMES = FALSE)
}
# Award level parse -> competition id (NA for direct awards) + direct_award flag.
parse_award_level <- function(award_ids, cache) {
  n <- length(award_ids); comp <- character(n); direct <- logical(n)
  for (i in seq_len(n)) {
    txt <- read_txt(award_ids[i], cache)
    direct[i] <- is_direct_award(txt)
    comp[i]   <- extract_prior(txt)
    if (i %% 5000 == 0) message(sprintf("  ...parsed %d / %d award XMLs", i, n))
  }
  data.table(award_notice_id       = award_ids,
             competition_notice_id = fifelse(direct, NA_character_, comp),
             direct_award          = direct)
}

# ── Competition-notice metadata: procedure type + DPS / framework flags ───────
# Procedure type is a single empty <PT_*/> flag inside <PROCEDURE> (2014-directive
# forms; ~93% of notices). Older/utilities forms carry none -> procedure_type NA.
# DPS and framework agreements are separate empty presence flags, not procedure
# types. Vocabulary + rates verified on the cached competition XMLs.
PROC_GROUP <- c(
  PT_OPEN = "open", PT_ACCELERATED_OPEN = "open",
  PT_RESTRICTED = "restricted", PT_ACCELERATED_RESTRICTED = "restricted",
  PT_ACCELERATED_RESTRICTED_CHOICE = "restricted",
  PT_COMPETITIVE_NEGOTIATION = "negotiated", PT_NEGOTIATED_WITH_PRIOR_CALL = "negotiated",
  PT_INVOLVING_NEGOTIATION = "negotiated", PT_NEGOTIATED_CHOICE = "negotiated",
  PT_COMPETITIVE_DIALOGUE = "competitive_dialogue",
  PT_INNOVATION_PARTNERSHIP = "innovation_partnership",
  PT_NEGOTIATED_WITHOUT_PUBLICATION = "without_call", PT_AWARD_CONTRACT_WITHOUT_CALL = "without_call")
# eForms procurement-procedure-type codes -> the SAME procedure_group vocabulary as PROC_GROUP above.
# (oth-mult / oth-single = "other", left unmapped -> NA.)
PROC_GROUP_EF <- c(open = "open", restricted = "restricted",
                   `neg-w-call` = "negotiated", `neg-wo-call` = "without_call",
                   `comp-dial` = "competitive_dialogue", innovation = "innovation_partnership")
procedure_type_of  <- function(txt) { m <- regmatches(txt, regexpr("<PT_[A-Z_]+", txt, perl = TRUE)); if (length(m)) sub("<", "", m[1]) else NA_character_ }
procedure_group_of <- function(pt)  if (is.na(pt)) NA_character_ else unname(c(PROC_GROUP, PROC_GROUP_EF)[pt])
is_dps_notice       <- function(txt) grepl("<DPS[ >/]|<SETTING_UP_DPS[ >/]", txt, perl = TRUE)
# framework agreement (empty flag; newer <FRAMEWORK> or older ESTABLISHMENT/AGREEMENT
# form) - deliberately excludes INFORMATION_REGULATORY_FRAMEWORK.
is_framework_notice <- function(txt) grepl("<FRAMEWORK[ >/]|ESTABLISHMENT_FRAMEWORK_AGREEMENT|<FRAMEWORK_AGREEMENT[ >/]", txt, perl = TRUE)

# Longest contract/framework DURATION in a notice, in days. TED encodes it as
# <DURATION TYPE="MONTH|DAY|YEAR">N</DURATION> (one per lot/OBJECT_DESCR); take the
# max as a notice-level proxy for annualising framework amounts. NA if none/untyped.
duration_days_of <- function(txt) {
  hits <- regmatches(txt, gregexpr("<DURATION[^>]*TYPE=\"[A-Z]+\"[^>]*>\\s*[0-9]+", txt, perl = TRUE))[[1]]
  if (!length(hits)) return(NA_real_)
  types <- sub(".*TYPE=\"([A-Z]+)\".*", "\\1", hits)
  nums  <- as.numeric(sub(".*>\\s*([0-9]+)$", "\\1", hits))
  mult  <- fcase(types == "DAY", 1, types == "MONTH", 30, types == "YEAR", 365, default = NA_real_)
  days  <- nums * mult
  if (all(is.na(days))) NA_real_ else max(days, na.rm = TRUE)
}
# eForms equivalent: longest cac:PlannedPeriod/cbc:DurationMeasure (unitCode), falling back to an explicit
# StartDate..EndDate span. Takes a parsed xml doc (competition_meta already parsed it once). NA if none.
ef_duration_days <- function(x) {
  days <- numeric(0)
  dm <- xml_find_all(x, "//cac:PlannedPeriod/cbc:DurationMeasure", .EF_NS)
  if (length(dm)) {
    units <- xml_attr(dm, "unitCode"); nums <- suppressWarnings(as.numeric(xml_text(dm)))
    mult  <- fcase(units == "DAY", 1, units == "WEEK", 7, units == "MONTH", 30, units == "YEAR", 365, default = NA_real_)
    days  <- c(days, nums * mult)
  }
  sd <- xml_text(xml_find_all(x, "//cac:PlannedPeriod/cbc:StartDate", .EF_NS))
  ed <- xml_text(xml_find_all(x, "//cac:PlannedPeriod/cbc:EndDate",   .EF_NS))
  if (length(sd) && length(ed) && length(sd) == length(ed)) {
    span <- suppressWarnings(as.numeric(as.Date(sub("([0-9-]{10}).*", "\\1", ed)) -
                                        as.Date(sub("([0-9-]{10}).*", "\\1", sd))))
    days <- c(days, span)
  }
  days <- days[!is.na(days) & days >= 0]
  if (length(days)) max(days) else NA_real_
}

# One read of a competition notice -> its prior (planning) ref + procedure + flags + duration.
competition_meta <- function(nid, cache) {
  txt <- read_txt(nid, cache)
  na_out <- list(planning_notice_id = NA_character_, procedure_type = NA_character_,
                 procedure_group = NA_character_, is_dps = NA, is_framework = NA,
                 framework_duration_days = NA_real_)
  if (!nzchar(txt)) return(na_out)
  # eForms competition notices: procedure / framework / DPS / duration live in namespaced code elements, not
  # the uppercase tags the legacy helpers match. Parse the doc ONCE and read them via the eForms codelists.
  # (planning_notice_id may be a UUID here; map_competition_meta resolves it against the API afterwards.)
  if (grepl("urn:oasis:names:specification:ubl:schema:xsd:", txt, fixed = TRUE)) {
    x <- tryCatch(read_xml(charToRaw(txt)), error = function(e) NULL)
    if (is.null(x)) return(na_out)
    g1   <- function(xp) { n <- xml_find_first(x, xp, .EF_NS); if (is.na(n)) NA_character_ else trimws(xml_text(n)) }
    gall <- function(xp) xml_text(xml_find_all(x, xp, .EF_NS))
    pid <- g1("//cac:TenderingProcess/cac:NoticeDocumentReference/cbc:ID")
    if (!is.na(pid) && identical(pid, nid)) pid <- NA_character_
    pt  <- g1("//cbc:ProcedureCode[@listName='procurement-procedure-type']")
    fa  <- gall("//cbc:ContractingSystemTypeCode[@listName='framework-agreement']")
    dp  <- gall("//cbc:ContractingSystemTypeCode[@listName='dps-usage']")
    return(list(planning_notice_id = pid, procedure_type = pt, procedure_group = procedure_group_of(pt),
                is_dps = any(dp %in% c("dps-list", "dps-nlist")),
                is_framework = any(grepl("^fa-", fa)),
                framework_duration_days = ef_duration_days(x)))
  }
  pid <- extract_prior(txt); if (!is.na(pid) && identical(pid, nid)) pid <- NA_character_
  pt  <- procedure_type_of(txt)
  list(planning_notice_id = pid, procedure_type = pt, procedure_group = procedure_group_of(pt),
       is_dps = is_dps_notice(txt), is_framework = is_framework_notice(txt),
       framework_duration_days = duration_days_of(txt))
}
map_competition_meta <- function(ids, cache) {
  if (!length(ids)) return(data.table())
  dt <- rbindlist(lapply(ids, function(id) c(list(competition_notice_id = id), competition_meta(id, cache))))
  dt[, planning_notice_id := resolve_notice_ids(planning_notice_id)]   # eForms UUID planning refs -> OJS
  dt[]
}

detail_url <- function(id) fifelse(is.na(id), NA_character_, sprintf("https://ted.europa.eu/en/notice/-/detail/%s", id))
xml_url    <- function(id) fifelse(is.na(id), NA_character_, sprintf("https://ted.europa.eu/en/notice/%s/xml", id))

# ── Cache-first parallel fetch ────────────────────────────────────────────────
is_cached <- function(ids, cache) {
  p <- file.path(cache, paste0(ids, ".xml")); s <- file.info(p)$size
  file.exists(p) & !is.na(s) & s > 0
}
# Fetch every id not already on disk (low concurrency; TED throttles). Returns a
# data.table(id, status): "ok" if on disk afterwards, else this run's outcome.
fetch_notices <- function(ids, cache, label = "notices") {
  ids  <- unique(ids[!is.na(ids)])
  todo <- ids[!is_cached(ids, cache)]
  message(sprintf("%s: %d distinct | cached: %d | to fetch: %d",
                  label, length(ids), length(ids) - length(todo), length(todo)))
  fetched <- NULL
  if (length(todo) > 0) {
    plan(multisession, workers = n_workers)
    handlers(global = TRUE); handlers("progress")
    with_progress({
      p <- progressor(along = todo)
      s <- unlist(future_map(todo, function(id) {
        p(); fetch_notice_xml(id, cache, max_retries, base_delay)$status
      }, .options = furrr_options(seed = TRUE)))
    })
    plan(sequential)
    fetched <- data.table(id = todo, s = s)
  }
  out <- data.table(id = ids, status = fifelse(is_cached(ids, cache), "ok", NA_character_))
  if (!is.null(fetched)) out[fetched, on = "id", status := fcoalesce(status, i.s)]
  out
}
