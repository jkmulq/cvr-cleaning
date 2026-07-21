# ─────────────────────────────────────────────────────────────────────────────
# Build CVR employment-history data for matched winners and buyers.
#
# For every unique valid CVR across the four matched winner/buyer datasets, pull
# annual, quarterly, and monthly employment counts from the Virk CVR
# system-to-system API and write them to a long CSV (one row per firm × period ×
# frequency). Each row is enriched with the firm's lifecycle (existence) and
# legal status for that period, so later analysis can tell active firms from
# dormant/closed ones.
#
# Writes incrementally and is resumable: a companion "*_status.csv" records which
# CVRs have been pulled, so a re-run only fetches the remainder.
#
# Optional; requires Virk credentials (see .Renviron.example). Controlled by
# environment variables:
#   CVR_EMPLOYMENT_BATCH_SIZE   CVRs per API request          (default 1000)
#   CVR_EMPLOYMENT_SAMPLE_SIZE  pull a random N CVRs, not all (default: all)
#   CVR_EMPLOYMENT_OVERWRITE    "true" rebuilds from scratch  (default false)
#   CVR_EMPLOYMENT_OUTPUT_FILE  output path (default data/clean/cvr_employment_history_virk.csv)
# ─────────────────────────────────────────────────────────────────────────────

# ── Setup ────────────────────────────────────────────────────────────────────

rm(list = ls())

source("config.R")

suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
}))

source(file.path(PROJECT_DIR, "code", "functions.R"))

# ── Inputs: matched datasets to pull CVRs from, and the final-CVR column in each ──
matched_cvr_files <- list(
  kfst_winners = list(
    path = file.path(dirs$clean_data, "clean_winner_data_kfst_name_matched.rds"),
    column = "winner_cvr_final"
  ),
  kfst_buyers = list(
    path = file.path(dirs$clean_data, "clean_buyer_data_kfst_name_matched.rds"),
    column = "buyer_cvr_final"
  ),
  opentender_winners = list(
    path = file.path(dirs$clean_data, "clean_winner_data_ot_name_matched.rds"),
    column = "winner_cvr_final"
  ),
  opentender_buyers = list(
    path = file.path(dirs$clean_data, "clean_buyer_data_ot_name_matched.rds"),
    column = "buyer_cvr_final"
  )
)

# ── Runtime options (environment variables) and output paths ──
batch_size <- as.integer(Sys.getenv("CVR_EMPLOYMENT_BATCH_SIZE", "1000"))
sample_size <- Sys.getenv("CVR_EMPLOYMENT_SAMPLE_SIZE")
use_sample <- nzchar(sample_size)
overwrite <- tolower(Sys.getenv("CVR_EMPLOYMENT_OVERWRITE", "false")) == "true"
output_file <- Sys.getenv("CVR_EMPLOYMENT_OUTPUT_FILE")

if (!nzchar(output_file)) {
  output_file <- file.path(dirs$clean_data, "cvr_employment_history_virk.csv")
}
status_file <- sub("[.]csv$", "_status.csv", output_file)

if (is.na(batch_size) || batch_size < 1L) {
  stop("CVR_EMPLOYMENT_BATCH_SIZE must be a positive integer.", call. = FALSE)
}

if (use_sample) {
  sample_size <- as.integer(sample_size)

  if (is.na(sample_size) || sample_size < 1L) {
    stop("CVR_EMPLOYMENT_SAMPLE_SIZE must be a positive integer.", call. = FALSE)
  }
}

# ── Output schema: defines the columns and types of every emitted row ──
empty_employment_table <- function() {
  data.table(
    cvr = character(),
    firm_name = character(),
    registration_date = character(),
    frequency = character(),
    year = integer(),
    quarter = integer(),
    month = integer(),
    employees = numeric(),
    fte = numeric(),
    employees_including_owners = numeric(),
    employee_interval = character(),
    fte_interval = character(),
    employees_including_owners_interval = character(),
    period_start = character(),
    period_end = character(),
    lifecycle_start = character(),
    lifecycle_end = character(),
    exists_at_period_start = logical(),
    exists_at_period_end = logical(),
    exists_during_period = logical(),
    status_code = character(),
    status_text = character(),
    updated_at = character()
  )
}

# ── Parse Virk firm records into employment rows ──────────────────────────────

# Parse a Virk timestamp (YYYY-MM-DD...) into a Date; NA when absent.
virk_date <- function(x) {
  value <- virk_scalar(x)

  if (is.na(value) || !nzchar(value)) {
    return(as.IDate(NA_character_))
  }

  as.IDate(substr(value, 1L, 10L))
}

# Calendar start/end of a record's reporting period (annual/quarterly/monthly).
employment_period_bounds <- function(record, frequency) {
  year <- as.integer(virk_scalar(record$aar))
  quarter <- as.integer(virk_scalar(record$kvartal))
  month <- as.integer(virk_scalar(record$maaned))

  if (is.na(year)) {
    return(list(
      period_start = as.IDate(NA_character_),
      period_end = as.IDate(NA_character_)
    ))
  }

  if (frequency == "monthly" && !is.na(month)) {
    period_start <- as.IDate(sprintf("%04d-%02d-01", year, month))
    period_end <- seq(period_start, by = "1 month", length.out = 2L)[2L] - 1L
  } else if (frequency == "quarterly" && !is.na(quarter)) {
    quarter_start_month <- (quarter - 1L) * 3L + 1L
    period_start <- as.IDate(sprintf("%04d-%02d-01", year, quarter_start_month))
    period_end <- seq(period_start, by = "3 months", length.out = 2L)[2L] - 1L
  } else {
    period_start <- as.IDate(sprintf("%04d-01-01", year))
    period_end <- as.IDate(sprintf("%04d-12-31", year))
  }

  list(
    period_start = period_start,
    period_end = period_end
  )
}

# Firm existence (lifecycle) intervals, used by the "was it active?" checks.
extract_lifecycle_periods <- function(firm) {
  if (is.null(firm$livsforloeb) || length(firm$livsforloeb) == 0) {
    return(data.table(
      lifecycle_start = virk_date(firm$stiftelsesDato),
      lifecycle_end = as.IDate(NA_character_)
    ))
  }

  rbindlist(
    lapply(firm$livsforloeb, function(record) {
      data.table(
        lifecycle_start = virk_date(record$periode$gyldigFra),
        lifecycle_end = virk_date(record$periode$gyldigTil)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
}

# Was the firm alive on a given date / at any point within a period?
firm_exists_on <- function(lifecycle_periods, date) {
  if (is.na(date)) {
    return(NA)
  }

  any(
    !is.na(lifecycle_periods$lifecycle_start) &
      lifecycle_periods$lifecycle_start <= date &
      (is.na(lifecycle_periods$lifecycle_end) | lifecycle_periods$lifecycle_end >= date)
  )
}

firm_exists_during <- function(lifecycle_periods, period_start, period_end) {
  if (is.na(period_start) || is.na(period_end)) {
    return(NA)
  }

  any(
    !is.na(lifecycle_periods$lifecycle_start) &
      lifecycle_periods$lifecycle_start <= period_end &
      (is.na(lifecycle_periods$lifecycle_end) | lifecycle_periods$lifecycle_end >= period_start)
  )
}

# Firm's registered status (active, bankrupt, ...) effective on a date.
status_at_date <- function(firm, date) {
  out <- list(
    status_code = NA_character_,
    status_text = NA_character_
  )

  if (is.null(firm$status) || length(firm$status) == 0 || is.na(date)) {
    return(out)
  }

  status_periods <- rbindlist(
    lapply(firm$status, function(record) {
      data.table(
        status_start = virk_date(record$periode$gyldigFra),
        status_end = virk_date(record$periode$gyldigTil),
        status_code = virk_scalar(record$statuskode),
        status_text = virk_scalar(record$statustekst)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  matching_status <- status_periods[
    !is.na(status_start) &
      status_start <= date &
      (is.na(status_end) | status_end >= date)
  ]

  if (nrow(matching_status) == 0) {
    return(out)
  }

  list(
    status_code = matching_status$status_code[1],
    status_text = matching_status$status_text[1]
  )
}

# Flatten one frequency's records into output rows, adding lifecycle + status.
extract_employment_rows <- function(firm, records, frequency) {
  if (is.null(records) || length(records) == 0) {
    return(empty_employment_table())
  }

  lifecycle_periods <- extract_lifecycle_periods(firm)
  lifecycle_start_values <- lifecycle_periods$lifecycle_start[
    !is.na(lifecycle_periods$lifecycle_start)
  ]
  lifecycle_end_values <- lifecycle_periods$lifecycle_end[
    !is.na(lifecycle_periods$lifecycle_end)
  ]

  if (length(lifecycle_start_values) == 0) {
    lifecycle_start <- as.IDate(NA_character_)
  } else {
    lifecycle_start <- min(lifecycle_start_values)
  }

  if (length(lifecycle_end_values) == 0) {
    lifecycle_end <- as.IDate(NA_character_)
  } else {
    lifecycle_end <- max(lifecycle_end_values)
  }
  registration_date <- virk_scalar(firm$stiftelsesDato)

  if (is.na(registration_date) || registration_date == "") {
    registration_date <- as.character(lifecycle_start)
  }

  out <- rbindlist(
    lapply(records, function(record) {
      period_bounds <- employment_period_bounds(record, frequency)
      status <- status_at_date(firm, period_bounds$period_end)

      data.table(
        cvr = format_virk_cvr(firm$cvrNummer),
        firm_name = virk_scalar(firm$virksomhedMetadata$nyesteNavn$navn),
        registration_date = registration_date,
        frequency = frequency,
        year = as.integer(virk_scalar(record$aar)),
        quarter = as.integer(virk_scalar(record$kvartal)),
        month = as.integer(virk_scalar(record$maaned)),
        employees = as.numeric(virk_scalar(record$antalAnsatte)),
        fte = as.numeric(virk_scalar(record$antalAarsvaerk)),
        employees_including_owners = as.numeric(virk_scalar(record$antalInklusivEjere)),
        employee_interval = virk_scalar(record$intervalKodeAntalAnsatte),
        fte_interval = virk_scalar(record$intervalKodeAntalAarsvaerk),
        employees_including_owners_interval = virk_scalar(record$intervalKodeAntalInklusivEjere),
        period_start = as.character(period_bounds$period_start),
        period_end = as.character(period_bounds$period_end),
        lifecycle_start = as.character(lifecycle_start),
        lifecycle_end = as.character(lifecycle_end),
        exists_at_period_start = firm_exists_on(
          lifecycle_periods,
          period_bounds$period_start
        ),
        exists_at_period_end = firm_exists_on(
          lifecycle_periods,
          period_bounds$period_end
        ),
        exists_during_period = firm_exists_during(
          lifecycle_periods,
          period_bounds$period_start,
          period_bounds$period_end
        ),
        status_code = status$status_code,
        status_text = status$status_text,
        updated_at = virk_scalar(record$sidstOpdateret)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  setcolorder(out, names(empty_employment_table()))
  out
}

# All three frequencies (annual + quarterly + monthly) for one firm.
extract_virk_employment_history <- function(firm) {
  rbindlist(
    list(
      extract_employment_rows(firm, firm$aarsbeskaeftigelse, "annual"),
      extract_employment_rows(firm, firm$kvartalsbeskaeftigelse, "quarterly"),
      extract_employment_rows(firm, firm$maanedsbeskaeftigelse, "monthly")
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

# ── Collect input CVRs and write output ──────────────────────────────────────

# Unique, valid 8-digit CVRs across the four matched datasets.
read_matched_cvrs <- function(file_specs) {
  cvrs <- unlist(lapply(file_specs, function(spec) {
    if (!file.exists(spec$path)) {
      stop("Missing matched dataset: ", spec$path, call. = FALSE)
    }

    data <- readRDS(spec$path)

    if (!spec$column %in% names(data)) {
      stop(
        "Missing column ", spec$column, " in ", spec$path,
        call. = FALSE
      )
    }

    data[[spec$column]]
  }))

  cvrs <- unique(na.omit(as.character(cvrs)))
  cvrs <- cvrs[grepl("^[0-9]{8}$", cvrs)]
  sort(cvrs)
}

# Append rows to a CSV, writing the header only on the first write.
append_employment_chunk <- function(data, path) {
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  fwrite(
    data,
    path,
    append = file.exists(path),
    col.names = !file.exists(path),
    na = ""
  )

  invisible(NULL)
}

# ── Build the Virk API request: fields to return + CVR term query ──
virk_employment_source_fields <- function() {
  c(
    "Vrvirksomhed.cvrNummer",
    "Vrvirksomhed.virksomhedMetadata.nyesteNavn",
    "Vrvirksomhed.stiftelsesDato",
    "Vrvirksomhed.livsforloeb",
    "Vrvirksomhed.status",
    "Vrvirksomhed.aarsbeskaeftigelse",
    "Vrvirksomhed.kvartalsbeskaeftigelse",
    "Vrvirksomhed.maanedsbeskaeftigelse"
  )
}

virk_employment_query_body <- function(cvrs) {
  body <- list(
    size = length(cvrs),
    query = list(
      terms = setNames(
        list(as.integer(cvrs)),
        "Vrvirksomhed.cvrNummer"
      )
    )
  )

  body[["_source"]] <- virk_employment_source_fields()
  body
}

# CVRs already recorded in the status file — used to resume an interrupted run.
already_processed_cvrs <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  unique(fread(path, select = "cvr", colClasses = "character")$cvr)
}

append_status_chunk <- function(data, path) {
  fwrite(
    data,
    path,
    append = file.exists(path),
    col.names = !file.exists(path),
    na = ""
  )

  invisible(NULL)
}

# ── Run: pull employment history in batches, writing results + status as we go ──

all_cvrs <- read_matched_cvrs(matched_cvr_files)

if (use_sample) {
  set.seed(123)
  all_cvrs <- sample(all_cvrs, min(sample_size, length(all_cvrs)))
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

if (file.exists(output_file) && overwrite) {
  file.remove(output_file)
}
if (file.exists(status_file) && overwrite) {
  file.remove(status_file)
}

processed_cvrs <- already_processed_cvrs(status_file)
cvrs_to_pull <- setdiff(all_cvrs, processed_cvrs)

cat("Matched CVRs found:", length(all_cvrs), "\n")
cat("Already processed:", length(processed_cvrs), "\n")
cat("Remaining CVRs to pull:", length(cvrs_to_pull), "\n")
cat("Batch size:", batch_size, "\n")
cat("Output file:", output_file, "\n")
cat("Status file:", status_file, "\n")

if (length(cvrs_to_pull) == 0) {
  cat("No CVRs left to pull.\n")
  quit(save = "no")
}

search_url <- "http://distribution.virk.dk/cvr-permanent/virksomhed/_search"
credentials <- get_virk_credentials()
timed <- system.time({
  for (start in seq(1L, length(cvrs_to_pull), by = batch_size)) {
    end <- min(start + batch_size - 1L, length(cvrs_to_pull))
    cvr_batch <- cvrs_to_pull[start:end]

    result <- virk_post_json(
      search_url,
      virk_employment_query_body(cvr_batch),
      credentials = credentials
    )

    firms <- lapply(result$hits$hits, function(hit) {
      hit$`_source`$Vrvirksomhed
    })

    employment_data <- if (length(firms) == 0) {
      empty_employment_table()
    } else {
      rbindlist(
        lapply(firms, extract_virk_employment_history),
        use.names = TRUE,
        fill = TRUE
      )
    }

    append_employment_chunk(employment_data, output_file)

    # Per-CVR outcome for this batch (found in Virk?, rows written) -> status file.
    returned_cvrs <- vapply(firms, function(firm) {
      format_virk_cvr(firm$cvrNummer)
    }, character(1))

    employment_rows_by_cvr <- employment_data[, .(employment_rows = .N), by = cvr]
    status_data <- data.table(cvr = cvr_batch)
    status_data[, pulled_at := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
    status_data[, found_in_virk := cvr %in% returned_cvrs]
    status_data <- employment_rows_by_cvr[
      status_data,
      on = "cvr"
    ]
    status_data[is.na(employment_rows), employment_rows := 0L]
    status_data <- status_data[, .(
      cvr,
      pulled_at,
      found_in_virk,
      employment_rows
    )]

    append_status_chunk(status_data, status_file)

    cat(
      "Processed batch", start, "-", end,
      "| firms returned:", length(firms),
      "| rows written:", nrow(employment_data), "\n"
    )
  }
})

cat("Finished employment-history pull.\n")
cat("Elapsed seconds:", unname(timed[["elapsed"]]), "\n")
