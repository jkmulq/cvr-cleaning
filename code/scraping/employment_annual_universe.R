#!/usr/bin/env Rscript
# Phase A of match-then-pull: pull a cheap, whole-history ANNUAL employment panel for every EVER-EMPLOYED
# firm in Virk, server-filtered so we never retrieve the ~1.8M never-employed shells. This is the candidate
# base for control eligibility -- eligibility itself (sector + kommune + employed-in-event-window, with the
# broad->drop-kommune->drop-sector cascade) is then computed LOCALLY per event on this panel (no per-event
# Virk calls), and the expensive quarterly/monthly series is pulled only for the resulting eligible union.
#
# Server-side filter: nested query -> ANY yearly record with antalAnsatte >= 1 (i.e. ever employed, judged
# on the WHOLE history -- not the terminal snapshot, so no wind-down bias). _source is limited to the annual
# array + current sector + HQ kommune, so each hit is light. Uses the company endpoint only (no production
# units), so it is cheap. Resumable: each scroll chunk is appended to the output CSV.
#
# Requires Virk credentials. Run:
#   LC_ALL=en_US.UTF-8 Rscript code/scraping/employment_annual_universe.R
# Options (env):
#   CVR_ANNUAL_BATCH_SIZE   scroll size (default 250 -- larger destabilises the scroll with nested payloads)
#   CVR_ANNUAL_SCROLL       scroll keepalive (default 10m)
#   CVR_ANNUAL_OUTPUT_FILE  output CSV (default <cvr_key>/cvr_annual_employment_virk_<stamp>.csv)
#   CVR_ANNUAL_OVERWRITE    "true" restarts from scratch (default false: resume/append)

rm(list = ls())
report_project_dir <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(d, "cvr-cleaning.Rproj")) && dirname(d) != d) d <- dirname(d)
  d
})
setwd(report_project_dir)
source(file.path(report_project_dir, "config.R"))
source(file.path(report_project_dir, "code", "functions.R"))
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(httr); library(jsonlite)}))

batch_size <- as.integer(Sys.getenv("CVR_ANNUAL_BATCH_SIZE", "250"))
max_firms  <- as.integer(Sys.getenv("CVR_ANNUAL_MAX_FIRMS", "0"))   # 0 = no cap; >0 stops early (testing)
scroll     <- Sys.getenv("CVR_ANNUAL_SCROLL", "10m")
overwrite  <- tolower(Sys.getenv("CVR_ANNUAL_OVERWRITE", "false")) == "true"
out_file   <- Sys.getenv("CVR_ANNUAL_OUTPUT_FILE",
                         unset = file.path(dirs$cvr_key,
                                           paste0("cvr_annual_employment_virk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))
if (overwrite && file.exists(out_file)) file.remove(out_file)

cred <- get_virk_credentials()
search_url <- "http://distribution.virk.dk/cvr-permanent/virksomhed/_search"
scroll_url <- "http://distribution.virk.dk/_search/scroll"

# Only fields we need: cvr, the annual employment array, current main sector, HQ kommune.
source_fields <- c("Vrvirksomhed.cvrNummer", "Vrvirksomhed.aarsbeskaeftigelse",
                   "Vrvirksomhed.virksomhedMetadata.nyesteHovedbranche.branchekode",
                   "Vrvirksomhed.virksomhedMetadata.nyesteBeliggenhedsadresse.kommune.kommuneKode")
# Ever-employed: any yearly record with antalAnsatte >= 1 (whole-history, unbiased).
ever_employed_q <- list(nested = list(
  path  = "Vrvirksomhed.aarsbeskaeftigelse",
  query = list(range = list(`Vrvirksomhed.aarsbeskaeftigelse.antalAnsatte` = list(gte = 1)))))

post_json <- function(url, body, params = NULL, max_tries = 6) {
  args <- list(url, authenticate(cred$user, cred$password), content_type_json(),
               body = toJSON(body, auto_unbox = TRUE), timeout(90))   # hard cap so a stalled response fails fast
  if (!is.null(params)) args$query <- params            # URL query params (e.g. ?scroll=10m on initial search)
  for (attempt in seq_len(max_tries)) {
    r <- tryCatch(do.call(POST, args), error = function(e) e)         # network/timeout errors -> retry
    if (inherits(r, "error")) {
      if (attempt == max_tries) stop("Virk request failed after ", max_tries, " tries: ", conditionMessage(r), call. = FALSE)
      Sys.sleep(min(2^attempt, 30)); next
    }
    ct <- content(r, as = "text", encoding = "UTF-8")
    if (status_code(r) == 200) return(fromJSON(ct, simplifyVector = FALSE))
    if (status_code(r) %in% c(429L, 502L, 503L, 504L) && attempt < max_tries) { Sys.sleep(min(2^attempt, 30)); next }
    stop("Virk HTTP ", status_code(r), ": ", substr(ct, 1, 300), call. = FALSE)   # non-retryable
  }
}

# Parse one firm's _source into annual rows: (cvr, year, antal_ansatte, antal_aarsvaerk, band, fte_band, sector, kommune).
parse_firm <- function(src) {
  v <- src$Vrvirksomhed
  cvr <- format_virk_cvr(v$cvrNummer)
  sector  <- virk_scalar(v$virksomhedMetadata$nyesteHovedbranche$branchekode)
  kommune <- virk_scalar(v$virksomhedMetadata$nyesteBeliggenhedsadresse$kommune$kommuneKode)
  aa <- v$aarsbeskaeftigelse
  if (is.null(aa) || length(aa) == 0) return(NULL)
  rbindlist(lapply(aa, function(r) data.table(
    cvr = cvr,
    year = suppressWarnings(as.integer(virk_scalar(r$aar))),
    antal_ansatte = suppressWarnings(as.integer(virk_scalar(r$antalAnsatte))),
    antal_aarsvaerk = suppressWarnings(as.numeric(virk_scalar(r$antalAarsvaerk))),
    employee_interval = virk_scalar(r$intervalKodeAntalAnsatte),
    fte_interval = virk_scalar(r$intervalKodeAntalAarsvaerk),
    sector = sector, kommune = kommune)), fill = TRUE)
}

append_chunk <- function(dt) {
  if (is.null(dt) || !nrow(dt)) return(invisible())
  fwrite(dt, out_file, append = file.exists(out_file), col.names = !file.exists(out_file), na = "")
}

cat(sprintf("Annual employment pull (ever-employed firms) -> %s\n  batch=%d scroll=%s\n", out_file, batch_size, scroll))
res <- post_json(search_url, list(size = batch_size, `_source` = source_fields, query = ever_employed_q),
                 params = list(scroll = scroll))
sid <- res$`_scroll_id`
n_firms <- 0L; n_rows <- 0L
repeat {
  hits <- res$hits$hits
  if (length(hits) == 0) break
  chunk <- rbindlist(lapply(hits, function(h) parse_firm(h$`_source`)), fill = TRUE)
  append_chunk(chunk)
  n_firms <- n_firms + length(hits); n_rows <- n_rows + nrow(chunk)
  if (n_firms %% 5000 < batch_size) cat(sprintf("  ...%d firms, %d firm-year rows\n", n_firms, n_rows))
  if (max_firms > 0L && n_firms >= max_firms) { cat("  (stopping early: CVR_ANNUAL_MAX_FIRMS reached)\n"); break }
  res <- post_json(scroll_url, list(scroll = scroll, scroll_id = sid))
  sid <- res$`_scroll_id`
}
cat(sprintf("DONE: %d ever-employed firms -> %d firm-year rows -> %s\n", n_firms, n_rows, out_file))
