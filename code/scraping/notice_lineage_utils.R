# Shared helpers for the TED notice-lineage pair:
#   1_2a_fetch_notices.R        (staged XML fetching: award -> competition -> planning)
#   1_2b_build_notice_lineage.R (assemble notice_links from the cached XMLs)
#
# NOT run on its own - sourced by both. Loads config + the award scraper's fetch
# machinery (2_extract_ted_notices.R), defines the per-level cache paths, the
# prior-publication parser shared by every hop, and a cache-first parallel fetch.
#
# The two hops both use the same prior-publication reference in the TED XML:
#   (1) newer F03/F14:  <NOTICE_NUMBER_OJ>2022/S 036-094091</NOTICE_NUMBER_OJ>
#   (2) older forms:    <REF_NOTICE> ... <NO_DOC_OJS>2010/S 117-176699</NO_DOC_OJS> ... </REF_NOTICE>
# eForms notices carry only an internal UUID (no OJS ref) -> no link.

source("config.R")
# SKIP_TED_RUN loads 2_extract's functions + config without running its pipeline:
# fetch_notice_xml, derive_notice_id, ted_dir, cache_dir, n_workers, max_retries,
# base_delay, and the packages (xml2/httr/dplyr/furrr/progressr).
SKIP_TED_RUN <- TRUE
source(file.path(PROJECT_DIR, "code", "scraping", "2_extract_ted_notices.R"))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

# ── Per-level cache dirs + output paths ───────────────────────────────────────
award_cache_dir    <- cache_dir                              # = ted_dir/raw_xml
comp_cache_dir     <- file.path(ted_dir, "competition_xml")
planning_cache_dir <- file.path(ted_dir, "planning_xml")
links_rds <- file.path(ted_dir, "notice_links.rds")
links_csv <- file.path(ted_dir, "notice_links.csv")
for (d in c(ted_dir, award_cache_dir, comp_cache_dir, planning_cache_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ── Award universe: distinct award-notice ids + URLs from OpenTender ───────────
# Used by BOTH scripts so they agree on the set (and on NOTICE_LINEAGE_SAMPLE_SIZE).
award_universe <- function() {
  raw_dir <- file.path(dirs$raw_data, "OpenTender")
  files   <- list.files(raw_dir, pattern = "[.]csv$", full.names = TRUE)
  u <- rbindlist(lapply(files, fread, sep = ";",
                        select = "tender_publications_lastContractAwardUrl", colClasses = "character"))
  setnames(u, "award_url")
  u <- u[!is.na(award_url) & trimws(award_url) != ""]
  u[, award_notice_id := derive_notice_id(award_url)]
  u <- unique(u[!is.na(award_notice_id)], by = "award_notice_id")
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
procedure_type_of  <- function(txt) { m <- regmatches(txt, regexpr("<PT_[A-Z_]+", txt, perl = TRUE)); if (length(m)) sub("<", "", m[1]) else NA_character_ }
procedure_group_of <- function(pt)  if (is.na(pt)) NA_character_ else unname(PROC_GROUP[pt])
is_dps_notice       <- function(txt) grepl("<DPS[ >/]|<SETTING_UP_DPS[ >/]", txt, perl = TRUE)
# framework agreement (empty flag; newer <FRAMEWORK> or older ESTABLISHMENT/AGREEMENT
# form) - deliberately excludes INFORMATION_REGULATORY_FRAMEWORK.
is_framework_notice <- function(txt) grepl("<FRAMEWORK[ >/]|ESTABLISHMENT_FRAMEWORK_AGREEMENT|<FRAMEWORK_AGREEMENT[ >/]", txt, perl = TRUE)

# One read of a competition notice -> its prior (planning) ref + procedure + flags.
competition_meta <- function(nid, cache) {
  txt <- read_txt(nid, cache)
  if (!nzchar(txt)) {
    return(list(planning_notice_id = NA_character_, procedure_type = NA_character_,
                procedure_group = NA_character_, is_dps = NA, is_framework = NA))
  }
  pid <- extract_prior(txt); if (!is.na(pid) && identical(pid, nid)) pid <- NA_character_
  pt  <- procedure_type_of(txt)
  list(planning_notice_id = pid, procedure_type = pt, procedure_group = procedure_group_of(pt),
       is_dps = is_dps_notice(txt), is_framework = is_framework_notice(txt))
}
map_competition_meta <- function(ids, cache) {
  if (!length(ids)) return(data.table())
  rbindlist(lapply(ids, function(id) c(list(competition_notice_id = id), competition_meta(id, cache))))
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
