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
suppressWarnings(suppressPackageStartupMessages(library(parallel)))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

if (!file.exists(links_rds)) stop("notice_links.rds not found; run ted_dates_2_lineage.R first.", call. = FALSE)

# ── Every date in one notice's XML: data.table(date_field, date_value) ─────────
# Handles both flat text (YYYYMMDD[ HH:MM]) and nested <YEAR>/<MONTH>/<DAY>, and
# keeps every distinct value per field (a field can appear more than once).
extract_all_dates <- function(txt) {
  empty <- data.table(date_field = character(), date_value = character())
  if (!nzchar(txt)) return(empty)
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
