# TED date panel, stage 0 (OPTIONAL, standalone): park the DK award notices that the OpenTender/KFST award
# URLs MISS but the TED API knows about -- fetch their XML into the shared Box cache, once.
#
# WHY: OT + KFST only give us the award URLs they happen to carry. The TED search API knows several thousand
# more recent Danish award notices (overwhelmingly 2024+ eForms) that those two sources never linked. This
# script discovers that extra set and pulls its XML into the shared award cache, CACHE-FIRST, so the expensive
# one-time fetch is decoupled from any pipeline run: the data lives on Box and is never re-fetched.
#
# PROVENANCE: the XML goes into the SAME raw_xml cache as everything else (so ted_1 picks it up with no
# double-fetch), and the api_only_award_ids.rds manifest records exactly which notice ids are API-discovered.
# That manifest is the single source of truth for the url/api flag downstream -- ted_1 unions these ids into
# its notice universe and 4_combine tags the resulting TED rows notice_source = "api".
#
# Re-runnable / resumable: the API sweep re-derives the set each run; fetch_notices() is cache-first, so only
# not-yet-cached notices are fetched. Safe to stop and restart.
#
# Optional env var: API_UNIVERSE_SAMPLE (limit to first N ids, for a quick end-to-end test).
#
# OUTPUT (on Box):
#   <raw_xml>/<id>.xml               the extra notices' XML, in the shared award cache (= ted/raw_xml).
#   <ted>/api_only_award_ids.rds     manifest: publication_number (padded), norm_id, year, notice_type, discovered

source("code/scraping/ted_dates_utils.R")

norm <- function(x) sub("^0+", "", x)                        # strip leading zeros for set comparison
pad8 <- function(p) {                                        # canonical 8-digit-padded id (eForms URL convention)
  m <- regmatches(p, regexec("^0*([0-9]+)-([0-9]{4})$", p))
  vapply(seq_along(p), function(i) { mm <- m[[i]]
    if (length(mm) == 3) sprintf("%08d-%s", as.integer(mm[2]), mm[3]) else p[i] }, character(1))
}

# ── 1. our URL set: OpenTender + KFST award notice ids (normalized) ───────────
# NB: compute from the raw OT csvs + kfst_award_map() directly, NOT award_universe() -- award_universe() now
# folds in this script's own API-only manifest when it exists, so using it here would make the sweep subtract
# its own prior output on re-runs (shrinking the manifest toward empty).
ot_files <- list.files(file.path(dirs$raw_data, "OpenTender"), pattern = "[.]csv$", full.names = TRUE)
ot_urls  <- rbindlist(lapply(ot_files, fread, sep = ";",
                             select = "tender_publications_lastContractAwardUrl", colClasses = "character"))
url_set <- unique(na.omit(norm(c(derive_notice_id(ot_urls[[1]]), kfst_award_map()$award_notice_id))))
message(sprintf("URL set (OpenTender + KFST) unique award notice ids: %d", length(url_set)))

# ── 2. API: every DK award notice, paginated via iterationNextToken (with retries) ─
api_query <- 'buyer-country=DNK AND notice-type IN ("can-standard" "can-social")'
api_page <- function(token) {
  body <- jsonlite::toJSON(c(list(query = api_query, fields = I("publication-number"),
                                  limit = 250L, paginationMode = "ITERATION"),
                             if (!is.null(token)) list(iterationNextToken = token)), auto_unbox = TRUE)
  for (try in 1:6) {                                         # ITERATION needs query+fields+token each call; retry transient
    r <- tryCatch(httr::POST("https://api.ted.europa.eu/v3/notices/search",
                             httr::content_type_json(), httr::user_agent("Mozilla/5.0"),
                             body = body, encode = "raw", httr::timeout(90)), error = function(e) NULL)
    if (!is.null(r) && httr::status_code(r) == 200) {
      j <- tryCatch(jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(j) && !is.null(j$notices)) return(j)
    }
    Sys.sleep(3)
  }
  NULL
}
pubs <- character(0); token <- NULL; total <- Inf; pg <- 0L
repeat {
  j <- api_page(token)
  if (is.null(j)) { message(sprintf("  API paging stopped at page %d after retries; have %d", pg + 1L, length(pubs))); break }
  total <- j$totalNoticeCount
  pubs  <- c(pubs, vapply(j$notices, function(n) n[["publication-number"]], character(1)))
  token <- j$iterationNextToken; pg <- pg + 1L
  if (pg %% 10L == 0L) message(sprintf("  ...API page %d: collected %d / %d", pg, length(pubs), total))
  if (is.null(token) || length(pubs) >= total || pg > 400L) break
  Sys.sleep(0.1)
}
api_set <- unique(norm(pubs))
message(sprintf("API DK-award set: %d unique ids (%d rows over %d pages; reported total %s)",
                length(api_set), length(pubs), pg, format(total, big.mark = ",")))

# ── 3. the extra set: in the API, not in our URLs -> manifest on Box ──────────
api_only <- setdiff(api_set, url_set)
message(sprintf("API-only award notices (missed by OT/KFST URLs): %d", length(api_only)))
manifest <- data.table(publication_number = pad8(api_only), norm_id = api_only,
                       year = sub(".*-", "", api_only), notice_type = "can",
                       discovered = as.character(Sys.Date()))
setorder(manifest, -year, publication_number)
saveRDS(manifest, file.path(ted_dir, "api_only_award_ids.rds"))
message(sprintf("Wrote manifest: %s (%d ids)", file.path(ted_dir, "api_only_award_ids.rds"), nrow(manifest)))
message("  by year:"); print(manifest[, .N, keyby = year])

# ── 4. fetch their XML into the shared award cache (cache-first, resumable) ───
ids <- manifest$publication_number
smp <- suppressWarnings(as.integer(Sys.getenv("API_UNIVERSE_SAMPLE", "")))
if (!is.na(smp) && smp > 0L) { ids <- head(ids, smp); message(sprintf("API_UNIVERSE_SAMPLE=%d: limiting fetch to %d ids", smp, length(ids))) }
fetch_notices(ids, award_cache_dir, "API-only award notices")   # award_cache_dir == cache_dir == ted/raw_xml

on_disk <- sum(is_cached(manifest$publication_number, award_cache_dir))
message(sprintf("\nParked on Box: %d / %d API-only award notices now cached in %s", on_disk, nrow(manifest), award_cache_dir))
message("Manifest is the url/api provenance key: ted_1 unions these ids into its notice universe (unless")
message("TED_INCLUDE_API_ONLY=false) and 4_combine tags the resulting TED rows notice_source = \"api\".")
