#!/usr/bin/env Rscript
# FULL-UNIVERSE control employment pull -- the unbiased replacement for the 10% random sample in
# employment_2_controls.R (which is LEFT UNTOUCHED). Emits the SAME spliced employment panel schema
# (actual employment series, not metadata), reusing the splice logic via code/scraping/employment_functions.R.
#
# Three-step "one-hit filter-then-pull" design (deterministic + parallelised on ncores-2):
#   STEP 1  LOCAL, no API: from the CVR name registry, keep candidates whose sector division AND HQ kommune
#           appear among the tender WINNERS (the only sectors/kommunes any event can match). Never-winners.
#   STEP 2  API, streamed in CVR batches: for each candidate batch, the query carries the FULL 4-array
#           ever-employed filter (company aars|kvartals|maaneds >=1  OR  production-unit erstMaaned >=1),
#           evaluated server-side -- shells return nothing, so we never pull their data.
#   STEP 3  ONE HIT: the same filtered query also requests the full employment _source, so a CVR that passes
#           is returned WITH its complete series; we then splice (company historical + production-unit recent)
#           exactly as employment_1_winners.R does. No second round-trip, no re-query.
#
# Equivalence + speed of the filter were validated against full-pull-then-filter (byte-identical CVR set;
# ~5x faster / ~100x lighter per firm). See scratch prototypes.
#
# Requires Virk credentials. Run:  LC_ALL=en_US.UTF-8 Rscript code/scraping/employment_controls_full.R
# Options (env):
#   CVR_EMPFULL_CORES          parallel workers (default detectCores()-2)
#   CVR_EMPFULL_BATCH_SIZE     candidate CVRs per API batch (default 500)
#   CVR_EMPFULL_PU_SCROLL      production-unit scroll page size (default 500)
#   CVR_EMPFULL_SECTOR_LEVEL   "division" (2-digit, default; matches the eligibility cascade) or "full" (6-digit)
#   CVR_EMPFULL_MAX_CANDIDATES cap the candidate list for testing (0 = no cap)
#   CVR_EMPFULL_EXCLUDE_WINNERS "true" (default) drops winner CVRs from the candidate pool
#   CVR_EMPFULL_OVERWRITE      "true" rebuilds from scratch (default false: resume -- skip batches with a shard)
#   CVR_EMPFULL_OUTPUT_FILE / _NAME_FILE / _STATUS_FILE   output paths (defaults under dirs$employment)

rm(list = ls())
report_project_dir <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(d, "cvr-cleaning.Rproj")) && dirname(d) != d) d <- dirname(d)
  d
})
setwd(report_project_dir)
source(file.path(report_project_dir, "config.R"))
source(file.path(report_project_dir, "code", "functions.R"))
source(file.path(report_project_dir, "code", "scraping", "employment_functions.R"))   # splice logic
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(httr); library(jsonlite); library(parallel)}))

# ---- params ----
n_cores    <- as.integer(Sys.getenv("CVR_EMPFULL_CORES", as.character(max(1L, parallel::detectCores() - 2L))))
batch_size <- as.integer(Sys.getenv("CVR_EMPFULL_BATCH_SIZE", "500"))
pu_scroll  <- as.integer(Sys.getenv("CVR_EMPFULL_PU_SCROLL", "500"))
sector_lvl <- Sys.getenv("CVR_EMPFULL_SECTOR_LEVEL", "division")
max_cand   <- as.integer(Sys.getenv("CVR_EMPFULL_MAX_CANDIDATES", "0"))
excl_win   <- tolower(Sys.getenv("CVR_EMPFULL_EXCLUDE_WINNERS", "true")) == "true"
overwrite  <- tolower(Sys.getenv("CVR_EMPFULL_OVERWRITE", "false")) == "true"
emp_dir    <- dirs$employment
output_file      <- Sys.getenv("CVR_EMPFULL_OUTPUT_FILE",  file.path(emp_dir, "cvr_employment_history_control_full.csv"))
name_output_file <- Sys.getenv("CVR_EMPFULL_NAME_FILE",    file.path(emp_dir, "cvr_name_history_control_full.csv"))
status_file      <- Sys.getenv("CVR_EMPFULL_STATUS_FILE",  file.path(emp_dir, "cvr_employment_status_control_full.csv"))
rds_output_file      <- sub("\\.csv$", ".rds", output_file)
rds_name_output_file <- sub("\\.csv$", ".rds", name_output_file)
shard_dir  <- file.path(dirname(output_file), "_shards_control_full")
employment_pull_schema <- "spliced_production_units_v2_location"
cred <- get_virk_credentials()
company_url <- "http://distribution.virk.dk/cvr-permanent/virksomhed/_search"
pu_url      <- "http://distribution.virk.dk/cvr-permanent/produktionsenhed/_search"
scroll_url  <- "http://distribution.virk.dk/_search/scroll"

# ---- API helpers (retry/backoff; scroll param on initial search) ----
post_json <- function(url, body, params = NULL, max_tries = 6L) {
  args <- list(url, authenticate(cred$user, cred$password), content_type_json(),
               body = toJSON(body, auto_unbox = TRUE), timeout(120))
  if (!is.null(params)) args$query <- params
  for (attempt in seq_len(max_tries)) {
    r <- tryCatch(do.call(POST, args), error = function(e) e)
    if (inherits(r, "error")) { if (attempt == max_tries) stop(conditionMessage(r)); Sys.sleep(min(2^attempt, 30)); next }
    ct <- content(r, as = "text", encoding = "UTF-8")
    if (status_code(r) == 200L) return(fromJSON(ct, simplifyVector = FALSE))
    if (status_code(r) %in% c(429L,502L,503L,504L) && attempt < max_tries) { Sys.sleep(min(2^attempt, 30)); next }
    stop("Virk HTTP ", status_code(r), ": ", substr(ct, 1, 200))
  }
}
hits_total <- function(res) { t <- res$hits$total; as.integer(if (is.list(t)) t$value else t) }
nested_ge1 <- function(path) list(nested = list(path = path,
                query = list(range = setNames(list(list(gte = 1)), paste0(path, ".antalAnsatte")))))
# STEP 2 filter: ever-employed by ANY of the three company arrays (the PU side is a separate index/query).
comp_emp_filter <- list(bool = list(minimum_should_match = 1, should = list(
  nested_ge1("Vrvirksomhed.aarsbeskaeftigelse"), nested_ge1("Vrvirksomhed.kvartalsbeskaeftigelse"),
  nested_ge1("Vrvirksomhed.maanedsbeskaeftigelse"))))
pu_emp_filter <- nested_ge1("VrproduktionsEnhed.erstMaanedsbeskaeftigelse")
company_source_fields <- function() c("Vrvirksomhed.cvrNummer","Vrvirksomhed.virksomhedMetadata.nyesteNavn",
  "Vrvirksomhed.navne","Vrvirksomhed.binavne","Vrvirksomhed.stiftelsesDato","Vrvirksomhed.livsforloeb",
  "Vrvirksomhed.status","Vrvirksomhed.virksomhedsstatus","Vrvirksomhed.aarsbeskaeftigelse",
  "Vrvirksomhed.kvartalsbeskaeftigelse","Vrvirksomhed.maanedsbeskaeftigelse","Vrvirksomhed.virksomhedsform",
  "Vrvirksomhed.hovedbranche","Vrvirksomhed.bibranche1","Vrvirksomhed.bibranche2","Vrvirksomhed.bibranche3",
  "Vrvirksomhed.beliggenhedsadresse","Vrvirksomhed.virksomhedMetadata.nyesteBeliggenhedsadresse","Vrvirksomhed.attributter")
production_unit_source_fields <- function() c("VrproduktionsEnhed.pNummer",
  "VrproduktionsEnhed.virksomhedsrelation","VrproduktionsEnhed.erstMaanedsbeskaeftigelse")

fetch_company_filtered <- function(cvr_batch) {      # STEP 2+3: filter + full data, one hit
  res <- post_json(company_url, list(size = length(cvr_batch), `_source` = company_source_fields(),
    query = list(bool = list(must = list(
      list(terms = setNames(list(I(as.integer(cvr_batch))), "Vrvirksomhed.cvrNummer")), comp_emp_filter)))))
  lapply(res$hits$hits, function(h) h$`_source`$Vrvirksomhed)
}
fetch_company_context <- function(cvrs) {            # recent-only firms: context (no emp filter)
  res <- post_json(company_url, list(size = length(cvrs), `_source` = company_source_fields(),
    query = list(terms = setNames(list(I(as.integer(cvrs))), "Vrvirksomhed.cvrNummer"))))
  lapply(res$hits$hits, function(h) h$`_source`$Vrvirksomhed)
}
fetch_pu_filtered <- function(cvr_batch) {           # PU side: filter + full data, scrolled
  res <- post_json(pu_url, list(size = pu_scroll, sort = list("_doc"), `_source` = production_unit_source_fields(),
    query = list(bool = list(must = list(
      list(terms = setNames(list(I(as.integer(cvr_batch))), "VrproduktionsEnhed.virksomhedsrelation.cvrNummer")), pu_emp_filter)))),
    params = list(scroll = "5m"))
  all <- res$hits$hits; total <- hits_total(res); sid <- res$`_scroll_id`
  while (length(all) < total) {
    r <- post_json(scroll_url, list(scroll = "5m", scroll_id = sid)); h <- r$hits$hits
    if (length(h) == 0) break; all <- c(all, h); sid <- r$`_scroll_id`
  }
  lapply(all, function(h) h$`_source`$VrproduktionsEnhed)
}

# =====================================================================================================
# STEP 1 (LOCAL, no API): candidate CVRs = registry firms in a winner sector-division AND winner kommune.
# =====================================================================================================
divof <- function(x) { x <- suppressWarnings(as.integer(x)); ifelse(is.na(x), NA_character_, substr(sprintf("%06d", x), 1, 2)) }
cd <- dirs$clean_data
key <- as.data.table(readRDS(file.path(cd, "clean_cvr_name_key.rds")))
reg <- unique(key[, .(cvr = sprintf("%08d", as.integer(cvr)),
                      sector6 = suppressWarnings(as.integer(hovedbranche_code)),
                      kommune = as.character(hq_kommune_code))], by = "cvr")
rm(key); invisible(gc())
reg[, sector_key := if (sector_lvl == "full") sprintf("%06d", sector6) else divof(sector6)]

win_files <- c("clean_winner_data_kfst_name_matched.rds","clean_winner_data_ot_name_matched.rds","clean_winner_data_ted_name_matched.rds")
winners <- unique(unlist(lapply(win_files, function(f) {
  p <- file.path(cd, f); if (!file.exists(p)) return(character(0))
  v <- as.data.table(readRDS(p))$winner_cvr_final; sprintf("%08d", as.integer(v[!is.na(v) & v != ""]))
})))
winners <- winners[grepl("^[0-9]{8}$", winners)]
w <- reg[cvr %chin% winners]
S <- unique(w$sector_key[!is.na(w$sector_key)]); K <- unique(w$kommune[!is.na(w$kommune)])

candidates <- reg[sector_key %chin% S & kommune %chin% K]
if (excl_win) candidates <- candidates[!(cvr %chin% winners)]
cand_cvrs <- sort(unique(candidates$cvr))
if (max_cand > 0L && length(cand_cvrs) > max_cand) cand_cvrs <- cand_cvrs[seq_len(max_cand)]
cat(sprintf("STEP 1: winners=%d | sector-%s union=%d | kommune union=%d\n", length(winners), sector_lvl, length(S), length(K)))
cat(sprintf("STEP 1: candidate CVRs (sector AND kommune%s) = %d\n\n", if (excl_win) ", never-winner" else "", length(cand_cvrs)))

# ---- batch + resume setup ----
if (overwrite && dir.exists(shard_dir)) unlink(shard_dir, recursive = TRUE)
dir.create(shard_dir, recursive = TRUE, showWarnings = FALSE)
batches <- split(cand_cvrs, ceiling(seq_along(cand_cvrs) / batch_size))
names(batches) <- sprintf("%06d", seq_along(batches))
todo <- names(batches)[!file.exists(file.path(shard_dir, paste0("status_", names(batches), ".csv")))]
cat(sprintf("STEP 2/3: %d batches (size %d), %d remaining | %d workers\n\n", length(batches), batch_size, length(todo), n_cores))

# Free the big STEP-1 objects BEFORE forking. The workers only need `batches`; if the ~2.27M-row registry and
# candidate tables stay resident, every per-batch fork copy-on-write-copies them under GC -> memory blows up
# and the machine swaps (the observed ~7x slowdown). Dropping them keeps each fork lean.
rm(list = intersect(c("reg", "candidates", "cand_cvrs", "key", "w", "winners", "S", "K"), ls()))
invisible(gc())

# =====================================================================================================
# STEP 2 + 3 (parallel worker): filtered one-hit pull per candidate batch, then splice. Writes shards.
# =====================================================================================================
process_batch <- function(bid) {
  cvr_batch <- batches[[bid]]
  emp_sh <- file.path(shard_dir, paste0("emp_",    bid, ".csv"))
  nm_sh  <- file.path(shard_dir, paste0("name_",   bid, ".csv"))
  st_sh  <- file.path(shard_dir, paste0("status_", bid, ".csv"))
  firms_emp <- fetch_company_filtered(cvr_batch)
  returned_cvrs <- vapply(firms_emp, function(f) format_virk_cvr(f$cvrNummer), character(1))
  punits <- fetch_pu_filtered(cvr_batch)

  new_monthly <- aggregate_production_unit_monthly(punits)
  if (nrow(new_monthly) > 0) new_monthly <- new_monthly[cvr %chin% cvr_batch]   # drop out-of-batch PU attributions
  recent_only <- setdiff(unique(new_monthly$cvr), returned_cvrs)                # PU-employed but no company arrays
  firms_extra <- if (length(recent_only)) fetch_company_context(recent_only) else list()
  firms <- c(firms_emp, firms_extra)
  firms_by_cvr <- setNames(firms, vapply(firms, function(f) format_virk_cvr(f$cvrNummer), character(1)))

  historical_data <- if (length(firms) == 0) empty_employment_table() else
    rbindlist(lapply(firms, extract_virk_employment_history), use.names = TRUE, fill = TRUE)
  new_monthly_data <- build_new_monthly_rows(new_monthly, firms_by_cvr)
  native_data <- collapse_employment_sources(rbindlist(list(historical_data, new_monthly_data), use.names = TRUE, fill = TRUE))
  employment_data <- add_spliced_frequencies(add_derived_frequencies(native_data, firms_by_cvr))
  if (nrow(employment_data)) setorder(employment_data, cvr, frequency, year, quarter, month)
  name_history_data <- if (length(firms) == 0) empty_name_history_table() else
    rbindlist(lapply(firms, extract_name_history), use.names = TRUE, fill = TRUE)

  status_data <- data.table(cvr = cvr_batch,
    found_in_virk = cvr_batch %in% returned_cvrs,
    found_in_production_units = cvr_batch %chin% unique(new_monthly$cvr),
    recent_only = cvr_batch %chin% recent_only,
    pulled_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    employment_pull_schema = employment_pull_schema)
  # write shards (status LAST -> its presence marks the batch complete for resume)
  fwrite(employment_data,  emp_sh, na = "")
  fwrite(name_history_data, nm_sh, na = "")
  fwrite(status_data,      st_sh, na = "")
  data.table(bid = bid, firms = length(firms), pus = length(punits), rows = nrow(employment_data), recent_only = length(recent_only))
}

if (length(todo)) {
  timed <- system.time({
    res <- mclapply(todo, function(bid) tryCatch(process_batch(bid),
                    error = function(e) data.table(bid = bid, error = conditionMessage(e))),
                    mc.cores = n_cores, mc.preschedule = FALSE)
  })
  summ <- rbindlist(res, use.names = TRUE, fill = TRUE)
  errs <- if ("error" %in% names(summ)) summ[!is.na(error)] else summ[0]
  cat(sprintf("STEP 2/3 done in %.1f min | batches ok: %d | errors: %d\n",
              timed[["elapsed"]]/60, nrow(summ) - nrow(errs), nrow(errs)))
  if (nrow(errs)) { cat("FAILED batches (rerun to resume):\n"); print(head(errs, 20)) }
} else cat("All batches already have shards; nothing to pull.\n")

# =====================================================================================================
# COMBINE shards -> final CSV, MEMORY-SAFE. The per-batch shards ARE the durable copy: they survive any
# crash, resume skips them, and this combine NEVER deletes them until the full artifact set (CSV + gzip .rds)
# is written and row-count-verified. So an OOM or interruption mid-combine loses nothing -- just rerun the
# script (the pull is skipped, and the combine redoes itself from the shards). Only COMPLETED batches (those
# with a status shard, written last) are combined, so a half-written batch is never included.
# =====================================================================================================
done_bids <- sort(sub("^status_", "", sub("\\.csv$", "",
              basename(Sys.glob(file.path(shard_dir, "status_*.csv"))))))

if (!length(done_bids)) {
  cat("\nCOMBINE: no shards present -- outputs already finalized (nothing to do).\n")
} else {
  cat(sprintf("\nCOMBINE: %d completed batch shards -> streaming (one shard in RAM at a time)\n", length(done_bids)))

  # Stream: read one shard, append to the CSV, drop it. Peak memory = a single shard, not the whole panel.
  combine_stream <- function(prefix, out_csv) {
    if (file.exists(out_csv)) file.remove(out_csv)
    rows <- 0L; ok <- TRUE; wrote <- FALSE
    for (bid in done_bids) {
      f <- file.path(shard_dir, paste0(prefix, "_", bid, ".csv"))
      if (!file.exists(f)) { ok <- FALSE; warning("missing shard: ", f); next }
      dt <- tryCatch(fread(f, na.strings = "", colClasses = list(character = "cvr")),
                     error = function(e) { ok <<- FALSE; warning("unreadable shard ", f, ": ", conditionMessage(e)); NULL })
      if (is.null(dt)) next
      okw <- tryCatch({ fwrite(dt, out_csv, append = wrote, col.names = !wrote, na = ""); TRUE },
                      error = function(e) { ok <<- FALSE; warning("append failed ", f, ": ", conditionMessage(e)); FALSE })
      if (okw) { rows <- rows + nrow(dt); wrote <- TRUE }
      rm(dt)
    }
    list(rows = rows, ok = ok)
  }
  # verify the written CSV has exactly the streamed row count (reads a single column, not the whole table)
  verify_csv <- function(out_csv, expect) {
    if (!file.exists(out_csv)) return(FALSE)
    n <- tryCatch(nrow(fread(out_csv, select = 1L)), error = function(e) NA_integer_)
    isTRUE(n == expect)
  }

  emp_c  <- combine_stream("emp",    output_file)
  name_c <- combine_stream("name",   name_output_file)
  stat_c <- combine_stream("status", status_file)
  emp_ok  <- emp_c$ok  && verify_csv(output_file,      emp_c$rows)
  name_ok <- name_c$ok && verify_csv(name_output_file, name_c$rows)
  stat_ok <- stat_c$ok && verify_csv(status_file,      stat_c$rows)
  cat(sprintf("COMBINE verify: emp=%s (%d rows) | name=%s (%d) | status=%s (%d)\n",
              emp_ok, emp_c$rows, name_ok, name_c$rows, stat_ok, stat_c$rows))

  # Compact artifact. The full employment panel (~62M rows) is far too large to saveRDS in 24 GB RAM (the old
  # saveRDS(fread(csv)) blew R's vector-memory limit), so stream the CSV -> a single PARQUET via arrow
  # (C++-streamed, low R memory; read_clean() prefers parquet). The small name-history file still goes to rds.
  emp_parquet_file <- sub("\\.csv$", ".parquet", output_file)
  write_parquet_stream <- function(csv, out_parquet, schema) tryCatch({
    tmpdir <- paste0(sub("\\.parquet$", "", out_parquet), "_parqdir")
    if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)
    ds <- arrow::open_csv_dataset(csv, schema = schema, skip = 1)   # skip=1: schema replaces the header row
    arrow::write_dataset(ds, tmpdir, format = "parquet",
                         max_rows_per_group = 1000000L, max_rows_per_file = 2000000000L,
                         basename_template = "part-{i}.parquet")
    parts <- Sys.glob(file.path(tmpdir, "*.parquet"))
    if (length(parts) != 1L) stop("expected 1 parquet part, got ", length(parts))
    if (file.exists(out_parquet)) file.remove(out_parquet)
    file.rename(parts[1], out_parquet); unlink(tmpdir, recursive = TRUE); TRUE
  }, error = function(e) { warning("parquet write failed for ", basename(out_parquet), " (CSV intact): ", conditionMessage(e)); FALSE })
  write_rds <- function(csv, rds) tryCatch({
    saveRDS(fread(csv, na.strings = "", colClasses = list(character = "cvr")), rds, compress = "gzip"); TRUE
  }, error = function(e) { warning("rds write failed for ", basename(rds), " (CSV intact): ", conditionMessage(e)); FALSE })

  emp_art  <- emp_ok && requireNamespace("arrow", quietly = TRUE) &&
              write_parquet_stream(output_file, emp_parquet_file, arrow::as_arrow_table(empty_employment_table())$schema)
  name_art <- name_ok && write_rds(name_output_file, rds_name_output_file)

  # AUTO-CLEANUP: only when the pull is COMPLETE (every batch has a shard) AND every artifact is verified.
  # Otherwise keep shards so a rerun can pull the missing batches / retry the combine with zero data loss.
  all_batches_done <- length(done_bids) == length(batches)
  if (all_batches_done && emp_ok && name_ok && stat_ok && emp_art && name_art) {
    unlink(shard_dir, recursive = TRUE)
    cat(sprintf("CLEANUP: all %d batches present + artifacts verified -> removed shard dir %s\n", length(batches), shard_dir))
  } else {
    cat(sprintf("CLEANUP SKIPPED: %d/%d batches present, artifacts ok=%s -- shards KEPT at %s (rerun to finish, no data lost)\n",
                length(done_bids), length(batches), emp_art && name_art, shard_dir))
  }

  cat(sprintf("\nDONE. employment rows: %d -> %s%s\n", emp_c$rows, output_file,
              if (file.exists(emp_parquet_file)) " (+ .parquet)" else " (.parquet PENDING -- rerun)"))
  if (stat_ok) cat(sprintf("distinct firms pulled: %d\n",
      fread(status_file)[found_in_virk == TRUE | found_in_production_units == TRUE, uniqueN(cvr)]))
}
