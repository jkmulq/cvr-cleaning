# Extract EVERY date from every notice in the lineage, tied back to tender/lot.
#
# For each award notice (whose URL came from OpenTender) and its linked
# competition + planning notices, pull every date-bearing field from the cached
# XML, then attach the tender_id / lot_id the award URL belongs to.
#
# INPUT
#   raw OpenTender CSVs           tender_id, lot_lotId (=lot_id), award URL
#   data/intermediates/ted/notice_links.rds   award -> competition -> planning ids (from xb)
#   cached XML in raw_xml/ , competition_xml/ , planning_xml/
# OUTPUT  (long / tidy: one row per tender-lot x notice x date)
#   data/intermediates/ted/notice_dates.{rds,csv}
#     tender_id, lot_id, notice_level (award|competition|planning), notice_id,
#     date_field, date_value (raw as in XML), date_iso (YYYY-MM-DD, or NA)
#
# Optional env var: NOTICE_LINEAGE_SAMPLE_SIZE  (limit award universe; for testing)

source("code/scraping/ted_dates_utils.R")
suppressWarnings(suppressPackageStartupMessages({library(parallel); library(xml2)}))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

if (!file.exists(links_rds)) stop("notice_links.rds not found; run ted_dates_2_lineage.R first.", call. = FALSE)

# ── eForms date extraction (schema-exact XPaths from the eForms SDK fields.json) ────────────────
# The legacy extractor below matches uppercase TED tags (DS_DATE_DISPATCH, DATE_PUB, ...) which eForms
# notices (2024+) do NOT contain, so every eForms notice came out date-less. eForms uses namespaced,
# context-dependent tags; the SAME lexical tag (cbc:IssueDate) means different things by parent element, so
# we use the official BT->XPath mappings (validated ~100% against the TED API at award/competition/planning
# levels). We emit the SAME canonical date_field names the panel (ted_dates_5_panel) already maps, so no
# downstream change is needed. Values are cleaned to YYYY-MM-DD (to_iso handles them) and the 2000-01-01
# "no award date" placeholder is dropped.
.EF_NS <- c(cbc  = "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2",
            cac  = "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2",
            efbc = "http://data.europa.eu/p27/eforms-ubl-extension-basic-components/1",
            efac = "http://data.europa.eu/p27/eforms-ubl-extension-aggregate-components/1")
.ef_clean <- function(s) { s <- s[!is.na(s)]; s <- sub("([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", s); unique(s[nzchar(s) & s != "2000-01-01"]) }
extract_eforms_dates <- function(txt) {
  empty <- data.table(date_field = character(), date_value = character())
  # Parse from raw BYTES (charToRaw), not the R string: read_txt() reads with useBytes=TRUE, so txt holds
  # the file's UTF-8 bytes under an unknown encoding. Handing that string to read_xml() makes libxml2
  # mis-read multi-byte chars (Danish ø/å/æ) and throw a tag-mismatch, which silently dropped every eForms
  # notice with non-ASCII content (i.e. almost all of them). charToRaw lets libxml2 honour the XML declaration.
  x <- tryCatch(read_xml(charToRaw(txt)), error = function(e) NULL); if (is.null(x)) return(empty)
  vals <- function(xp) { n <- xml_find_all(x, xp, .EF_NS); if (!length(n)) character(0) else .ef_clean(xml_text(n)) }
  add  <- function(field, xp) { v <- vals(xp); if (length(v)) data.table(date_field = field, date_value = v) else NULL }
  out <- rbindlist(list(
    add("DS_DATE_DISPATCH",     "/*/cbc:IssueDate"),                                         # BT-05  notice dispatch
    add("DATE_PUB",             "//efac:Publication/efbc:PublicationDate"),                  # TED publication date
    add("DATE_RECEIPT_TENDERS", "//cac:TenderSubmissionDeadlinePeriod/cbc:EndDate"),         # BT-131 tender deadline (competition/planning)
    add("CONTRACT_AWARD_DATE",  "//efac:NoticeResult/efac:SettledContract/cbc:IssueDate")    # BT-145 contract conclusion (award)
  ), use.names = TRUE)
  if (is.null(out) || !nrow(out)) empty else out
}

# ── Every date in one notice's XML: data.table(date_field, date_value) ─────────
# Handles both flat text (YYYYMMDD[ HH:MM]) and nested <YEAR>/<MONTH>/<DAY>, and
# keeps every distinct value per field (a field can appear more than once).
extract_all_dates <- function(txt) {
  empty <- data.table(date_field = character(), date_value = character())
  if (!nzchar(txt)) return(empty)
  # eForms notices carry the UBL namespace; route them to the schema-XPath extractor (the legacy
  # uppercase-tag regex below finds nothing in eForms XML). Legacy TED_EXPORT notices fall through.
  if (grepl("urn:oasis:names:specification:ubl:schema:xsd:", txt, fixed = TRUE)) return(extract_eforms_dates(txt))
  tags <- unique(gsub("^<|[ >/]$", "",
                      regmatches(txt, gregexpr("<([A-Z0-9_]*(?:DATE|DEADLINE)[A-Z0-9_]*)[ >/]", txt, perl = TRUE))[[1]]))
  if (!length(tags)) return(empty)
  out <- vector("list", length(tags))
  for (k in seq_along(tags)) {
    tg <- tags[k]
    blocks <- regmatches(txt, gregexpr(sprintf("(?s)<%s[^>]*>.*?</%s>", tg, tg), txt, perl = TRUE))[[1]]
    vals <- character(0)
    for (b in blocks) {
      v <- grab1(b, tg)                                   # direct text?
      if (is.na(v) || !grepl("[0-9]", v)) {               # else nested Y/M/D
        y <- grab1(b, "YEAR"); m <- grab1(b, "MONTH"); d <- grab1(b, "DAY")
        v <- if (!is.na(y)) paste(y, m %||% "", d %||% "", sep = "-") else NA_character_
      }
      if (!is.na(v) && nzchar(v)) vals <- c(vals, v)
    }
    if (length(vals)) out[[k]] <- data.table(date_field = tg, date_value = unique(vals))
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out)) rbindlist(out) else empty
}

# ── Normalise a raw date value to YYYY-MM-DD (NA if not parseable) ─────────────
to_iso <- function(v) {
  v <- trimws(as.character(v)); out <- rep(NA_character_, length(v))
  flat <- grepl("^[0-9]{8}", v)
  out[flat] <- sprintf("%s-%s-%s", substr(v[flat], 1, 4), substr(v[flat], 5, 6), substr(v[flat], 7, 8))
  nest <- !flat & grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", v)
  if (any(nest)) {
    p <- tstrsplit(v[nest], "-", fixed = TRUE)
    out[nest] <- sprintf("%04d-%02d-%02d", as.integer(p[[1]]), as.integer(p[[2]]), as.integer(p[[3]]))
  }
  out
}

# Parallel date extraction over a set of notice ids in one cache.
extract_dates_for <- function(ids, cache, level) {
  ids <- unique(ids[!is.na(ids)])
  if (!length(ids)) return(data.table())
  message(sprintf("Extracting dates from %d %s notices...", length(ids), level))
  setDTthreads(1L)
  res <- mclapply(ids, function(id) {
    dt <- extract_all_dates(read_txt(id, cache))
    if (!nrow(dt)) return(NULL)
    dt[, `:=`(notice_id = id, notice_level = level)][]
  }, mc.cores = max(1L, detectCores() - 1L), mc.preschedule = TRUE)
  setDTthreads(0L)
  rbindlist(res[!vapply(res, is.null, logical(1))], use.names = TRUE)
}

# ── 1. tender_id / lot_id -> award notice, from the raw OpenTender CSVs ────────
raw_dir <- file.path(dirs$raw_data, "OpenTender")
files   <- list.files(raw_dir, pattern = "[.]csv$", full.names = TRUE)
map <- rbindlist(lapply(files, fread, sep = ";",
                        select = c("tender_id", "lot_lotId", "tender_publications_lastContractAwardUrl"),
                        colClasses = "character"), fill = TRUE)
setnames(map, c("tender_id", "lot_id", "award_url"))
map <- map[!is.na(award_url) & trimws(award_url) != ""]
map[, award_notice_id := derive_notice_id(award_url)]
map <- unique(map[!is.na(award_notice_id)], by = c("tender_id", "lot_id", "award_notice_id"))
map[, source := "ot"]

# KFST tender/lot -> award notice, from the raw KFST xlsx. KFST is a SEPARATE tender/lot ID space, kept
# apart via `source`; the two sources share many notices, but the notice-level extraction below runs over
# DISTINCT notice ids, so a shared notice is fetched/parsed once (no double-pull). Helper: kfst_award_map().
map_kfst <- kfst_award_map()[, .(tender_id, lot_id, award_url, award_notice_id)]
map_kfst[, source := "kfst"]
map <- rbindlist(list(map, map_kfst), use.names = TRUE)

# API-only award notices (parked by ted_dates_0_api_universe.R): notice-keyed only, no OT/KFST tender/lot.
# Include them so their dates land in notice_dates and build_ted_notice_panel() (which keys by notice_id) gives
# them the same lineage dates as every other notice. source="api", tender_id = notice id, lot_id = NA -> the
# OT/KFST panels (built from source=="ot"/"kfst") ignore them; only the TED notice-keyed panel picks them up.
if (include_api_only()) {
  .apf <- file.path(ted_dir, "api_only_award_ids.rds")
  if (file.exists(.apf)) {
    .ap <- as.data.table(readRDS(.apf))
    map <- rbindlist(list(map, data.table(tender_id = .ap$publication_number, lot_id = NA_character_,
                                          award_url = xml_url(.ap$publication_number),
                                          award_notice_id = .ap$publication_number, source = "api")),
                     use.names = TRUE)
  }
}

sample_n <- suppressWarnings(as.integer(Sys.getenv("NOTICE_LINEAGE_SAMPLE_SIZE", "")))
if (!is.na(sample_n) && sample_n > 0L) {
  keep <- head(unique(map$award_notice_id), sample_n)
  map <- map[award_notice_id %chin% keep]
  message(sprintf("NOTICE_LINEAGE_SAMPLE_SIZE set: limiting to %d award notices", length(keep)))
}

# ── 2. attach competition + planning notice ids via the lineage ───────────────
links <- as.data.table(readRDS(links_rds))
tenderlot <- merge(map, links[, .(award_notice_id, competition_notice_id, planning_notice_id)],
                   by = "award_notice_id", all.x = TRUE)
message(sprintf("Tender-lot rows: %d (%d distinct award notices)",
                nrow(tenderlot), uniqueN(tenderlot$award_notice_id)))

# ── 3. extract every date per distinct notice, per level ──────────────────────
notice_dates <- rbindlist(list(
  extract_dates_for(tenderlot$award_notice_id,       award_cache_dir,    "award"),
  extract_dates_for(tenderlot$competition_notice_id, comp_cache_dir,     "competition"),
  extract_dates_for(tenderlot$planning_notice_id,    planning_cache_dir, "planning")
), use.names = TRUE)
notice_dates[, date_iso := to_iso(date_value)]

# ── 4. join dates back onto tender_id / lot_id, per level ─────────────────────
join_level <- function(level, id_col) {
  d <- notice_dates[notice_level == level]
  if (!nrow(d)) return(NULL)
  tl <- unique(tenderlot[!is.na(get(id_col)), .(source, tender_id, lot_id, notice_id = get(id_col))])
  merge(tl, d, by = "notice_id", allow.cartesian = TRUE)[
    , .(source, tender_id, lot_id, notice_level, notice_id, date_field, date_value, date_iso)]
}
out <- rbindlist(list(
  join_level("award",       "award_notice_id"),
  join_level("competition", "competition_notice_id"),
  join_level("planning",    "planning_notice_id")
), use.names = TRUE)
setorder(out, source, tender_id, lot_id, notice_level, date_field)

# ── 5. save + summary ─────────────────────────────────────────────────────────
out_rds <- file.path(ted_dir, "notice_dates.rds")
out_csv <- file.path(ted_dir, "notice_dates.csv")
saveRDS(out, out_rds); fwrite(out, out_csv)

message("\nNotice dates extracted.")
message(sprintf("  rows (tender-lot x notice x date): %d", nrow(out)))
message(sprintf("  tender-lots covered:               %d", out[, uniqueN(paste(tender_id, lot_id))]))
message("  distinct date fields per level:")
print(out[, .(fields = uniqueN(date_field), rows = .N), by = notice_level])
message(sprintf("  ISO-parseable date values: %.1f%%", 100 * mean(!is.na(out$date_iso))))
message(sprintf("Written: %s\n         %s", out_rds, out_csv))
