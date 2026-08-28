# Build CVR employment-history data for a CONTROL group of firms that NEVER win a
# tender. A mirror of employment_1_winners.R: identical Virk pull and
# output schema; only the input CVR set differs.
#
# The universe of firms is every distinct CVR in the Virk name key. We drop every
# tender winner (winner_cvr_final across the KFST + OpenTender winner datasets) to
# leave the never-win universe, then pull employment for a controllable SHARE of
# it. For every selected CVR we pull historical company employment from the Virk
# company endpoint and newer monthly employment from the production-unit endpoint,
# aggregate production units to the legal CVR-month, and splice with the historical
# company series -- exactly as the winner pull does.
#
# ITERATIVE TARGET: the goal is that the share of firms we actually RECOVER
# employment data for (>=1 employment row) reaches the target share of the never-win
# universe -- not merely that we queried that many. Not every firm has reported
# employment, so each round we measure the realised recovery, and if it is below
# target we query 1.5 x the remaining deficit of additional (previously un-queried)
# firms, repeating until recovered >= target (or the pool is exhausted). A fixed
# shuffle of the pool makes the draw order deterministic and resumable.
#
# Writes incrementally and is resumable: each batch is saved as its own compressed
# RDS chunk, and the chunks are combined into the single output RDS at the end (to
# save space vs a large CSV, and to preserve the exact column schema/types). The
# companion "*_status.csv" ledger records CVRs completed by this schema version, so
# old company-only status files are not treated as complete.
#
# Optional; requires Virk credentials (see .Renviron.example). Controlled by
# environment variables:
#   CVR_EMPLOYMENT_BATCH_SIZE    CVRs per API request               (default 1000)
#   CVR_EMPLOYMENT_SAMPLE_SHARE  target recovered share of universe (default 0.10)
#   CVR_EMPLOYMENT_SAMPLE_SEED   seed for the shuffled draw order    (default 123)
#   CVR_EMPLOYMENT_OVERWRITE     "true" rebuilds from scratch        (default false)
#   CVR_EMPLOYMENT_OUTPUT_FILE   output RDS path (default data/employment/cvr_employment_history_control.rds)
#   CVR_EMPLOYMENT_SCROLL_SIZE   production-unit scroll size          (default 1000)
#   CVR_EMPLOYMENT_SCROLL        scroll lifetime                      (default 5m)

# -- Setup --------------------------------------------------------------------

rm(list = ls())

source("config.R")

suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
}))

source(file.path(PROJECT_DIR, "code", "functions.R"))

employment_pull_schema <- "spliced_production_units_v2_location"

# -- Inputs: firm universe (Virk) minus tender winners ------------------------

# The universe of firms: every distinct CVR in the Virk name key.
universe_file <- file.path(dirs$clean_data, "clean_cvr_name_key.rds")

# Firms to EXCLUDE so the control group never wins a tender: winner_cvr_final
# across both winner datasets. (Buyers are not excluded -- a firm that only ever
# buys still never wins.)
winner_cvr_files <- list(
  kfst_winners = list(
    path = file.path(dirs$clean_data, "clean_winner_data_kfst_name_matched.rds"),
    column = "winner_cvr_final"
  ),
  opentender_winners = list(
    path = file.path(dirs$clean_data, "clean_winner_data_ot_name_matched.rds"),
    column = "winner_cvr_final"
  )
)

# -- Runtime options and output paths -----------------------------------------

batch_size <- as.integer(Sys.getenv("CVR_EMPLOYMENT_BATCH_SIZE", "1000"))
scroll_size <- as.integer(Sys.getenv("CVR_EMPLOYMENT_SCROLL_SIZE", "1000"))
scroll_keepalive <- Sys.getenv("CVR_EMPLOYMENT_SCROLL", "5m")
sample_share <- as.numeric(Sys.getenv("CVR_EMPLOYMENT_SAMPLE_SHARE", "0.10"))
sample_seed <- as.integer(Sys.getenv("CVR_EMPLOYMENT_SAMPLE_SEED", "123"))
overwrite <- tolower(Sys.getenv("CVR_EMPLOYMENT_OVERWRITE", "false")) == "true"
output_file <- Sys.getenv("CVR_EMPLOYMENT_OUTPUT_FILE")

if (!nzchar(output_file)) {
  output_file <- file.path(dirs$employment, "cvr_employment_history_control.rds")
}
# Derive companion paths by stripping the output extension. The employment data
# and name history are saved as (compressed) RDS to save space; the status ledger
# stays CSV because it is appended to for resume and is small.
status_file <- sub("[.][^.]+$", "_status.csv", output_file)
name_output_file <- sub("[.][^.]+$", "_names.rds", output_file)
# During the run each batch is written as its own compressed RDS chunk (so disk
# stays compressed and the pull is resumable without holding everything in memory);
# the chunks are combined into the single output RDS at the end.
emp_chunks_dir <- sub("[.][^.]+$", "_chunks", output_file)
name_chunks_dir <- sub("[.][^.]+$", "_names_chunks", output_file)

if (is.na(batch_size) || batch_size < 1L || batch_size > 3000L) {
  stop("CVR_EMPLOYMENT_BATCH_SIZE must be between 1 and 3000.", call. = FALSE)
}

if (is.na(scroll_size) || scroll_size < 1L || scroll_size > 3000L) {
  stop("CVR_EMPLOYMENT_SCROLL_SIZE must be between 1 and 3000.", call. = FALSE)
}

if (is.na(sample_share) || sample_share <= 0 || sample_share > 1) {
  stop("CVR_EMPLOYMENT_SAMPLE_SHARE must be in (0, 1].", call. = FALSE)
}

if (is.na(sample_seed)) {
  stop("CVR_EMPLOYMENT_SAMPLE_SEED must be an integer.", call. = FALSE)
}

# -- Output schema -------------------------------------------------------------

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
    employees_historical = numeric(),
    fte_historical = numeric(),
    employees_including_owners_historical = numeric(),
    employees_new = numeric(),
    fte_new = numeric(),
    employees_including_owners_new = numeric(),
    employee_interval = character(),
    fte_interval = character(),
    employees_including_owners_interval = character(),
    employee_interval_historical = character(),
    fte_interval_historical = character(),
    employees_including_owners_interval_historical = character(),
    employee_interval_new = character(),
    fte_interval_new = character(),
    employees_including_owners_interval_new = character(),
    employment_source = character(),
    has_historical_new_overlap = logical(),
    n_production_units_aggregated = integer(),
    n_production_units_nonmissing_employees = integer(),
    n_production_units_nonmissing_fte = integer(),
    period_start = character(),
    period_end = character(),
    lifecycle_start = character(),
    lifecycle_end = character(),
    exists_at_period_start = logical(),
    exists_at_period_end = logical(),
    exists_during_period = logical(),
    status_code = character(),
    status_text = character(),
    legal_form_code = integer(),
    legal_form_short = character(),
    legal_form_text = character(),
    industry_code = character(),
    industry_text = character(),
    secondary_industry_1_code = character(),
    secondary_industry_1_text = character(),
    secondary_industry_2_code = character(),
    secondary_industry_2_text = character(),
    secondary_industry_3_code = character(),
    secondary_industry_3_text = character(),
    # Location as of the period (time-varying; from historical beliggenhedsadresse)
    kommune_code = character(),
    kommune_name = character(),
    postnummer = character(),
    city = character(),
    # Current headquarters location (static; from nyesteBeliggenhedsadresse)
    hq_kommune_code = character(),
    hq_kommune_name = character(),
    hq_postnummer = character(),
    hq_city = character(),
    capital_amount = numeric(),
    capital_currency = character(),
    derived_from_monthly = logical(),
    derived_months_observed = integer(),
    derived_months_expected = integer(),
    derived_period_complete = logical(),
    updated_at = character(),
    updated_at_historical = character(),
    updated_at_new = character()
  )
}

empty_production_unit_employment_table <- function() {
  data.table(
    cvr = character(),
    p_nummer = character(),
    year = integer(),
    month = integer(),
    employees_new = numeric(),
    fte_new = numeric(),
    employees_including_owners_new = numeric(),
    employee_interval_new = character(),
    fte_interval_new = character(),
    employees_including_owners_interval_new = character(),
    updated_at_new = character(),
    period_start = character(),
    period_end = character()
  )
}

# Separate "key" of a firm's historical names: one row per (name, validity period).
# name_type = "primary" (Vrvirksomhed.navne) or "secondary" (binavne). is_current
# TRUE where the name has no end date (gyldigTil is null), i.e. the current name.
empty_name_history_table <- function() {
  data.table(
    cvr = character(),
    name_type = character(),
    firm_name = character(),
    gyldig_fra = character(),
    gyldig_til = character(),
    is_current = logical()
  )
}

# -- Small utilities -----------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

first_nonmissing <- function(x) {
  if (is.character(x)) {
    keep <- !is.na(x) & nzchar(x)
  } else {
    keep <- !is.na(x)
  }

  if (any(keep)) {
    x[which(keep)[1L]]
  } else {
    x[NA_integer_][1L]
  }
}

safe_sum <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_max_int <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_integer_ else as.integer(max(x))
}

safe_max_char <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) NA_character_ else max(x)
}

spliced_source <- function(has_historical, has_new) {
  if (has_historical && has_new) {
    "historical_and_new"
  } else if (has_historical) {
    "historical"
  } else if (has_new) {
    "new"
  } else {
    NA_character_
  }
}

derive_source <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])

  if (length(x) == 0) {
    NA_character_
  } else if (all(x == "historical")) {
    "derived_historical_monthly"
  } else if (all(x == "new")) {
    "derived_new_monthly"
  } else {
    "derived_spliced_monthly"
  }
}

virk_date <- function(x) {
  value <- virk_scalar(x)

  if (is.na(value) || !nzchar(value)) {
    return(as.IDate(NA_character_))
  }

  as.IDate(substr(value, 1L, 10L))
}

period_contains <- function(record, date) {
  if (is.na(date)) {
    return(FALSE)
  }

  start <- virk_date(record$periode$gyldigFra)
  end <- virk_date(record$periode$gyldigTil)

  (is.na(start) || start <= date) && (is.na(end) || end >= date)
}

record_at_date <- function(records, date) {
  if (is.null(records) || length(records) == 0 || is.na(date)) {
    return(NULL)
  }

  matches <- vapply(records, period_contains, logical(1), date = date)
  if (!any(matches)) {
    return(NULL)
  }

  matching_records <- records[matches]
  starts <- vapply(
    matching_records,
    function(record) as.character(virk_date(record$periode$gyldigFra)),
    character(1)
  )
  starts[is.na(starts)] <- "0001-01-01"
  matching_records[[which(starts == max(starts))[1L]]]
}

# Extract the location fields we keep from a CVR address object. Works for both
# the current headquarters address (virksomhedMetadata$nyesteBeliggenhedsadresse,
# a single object) and a single period record from the historical
# beliggenhedsadresse array. `kommune` is nested (kommuneKode / kommuneNavn).
address_parts <- function(addr) {
  if (is.null(addr) || length(addr) == 0) {
    return(list(
      kommune_code = NA_character_, kommune_name = NA_character_,
      postnummer   = NA_character_, city         = NA_character_
    ))
  }

  city <- virk_scalar(addr$postdistrikt)
  if (is.na(city) || !nzchar(city)) {
    city <- virk_scalar(addr$bynavn)
  }

  list(
    kommune_code = as.character(virk_scalar(addr$kommune$kommuneKode)),
    kommune_name = virk_scalar(addr$kommune$kommuneNavn),
    postnummer   = as.character(virk_scalar(addr$postnummer)),
    city         = city
  )
}

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

calendar_period_bounds <- function(year, frequency, quarter = NA_integer_) {
  if (frequency == "quarterly_derived") {
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

# -- Company metadata parsing -------------------------------------------------

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

firm_lifecycle_bounds <- function(lifecycle_periods) {
  lifecycle_start_values <- lifecycle_periods$lifecycle_start[
    !is.na(lifecycle_periods$lifecycle_start)
  ]
  lifecycle_end_values <- lifecycle_periods$lifecycle_end[
    !is.na(lifecycle_periods$lifecycle_end)
  ]

  list(
    lifecycle_start = if (length(lifecycle_start_values) == 0) {
      as.IDate(NA_character_)
    } else {
      min(lifecycle_start_values)
    },
    lifecycle_end = if (length(lifecycle_end_values) == 0) {
      as.IDate(NA_character_)
    } else {
      max(lifecycle_end_values)
    }
  )
}

status_at_date <- function(firm, date) {
  out <- list(
    status_code = NA_character_,
    status_text = NA_character_
  )
  status_records <- firm$status %||% firm$virksomhedsstatus

  if (is.null(status_records) || length(status_records) == 0 || is.na(date)) {
    return(out)
  }

  status_periods <- rbindlist(
    lapply(status_records, function(record) {
      data.table(
        status_start = virk_date(record$periode$gyldigFra),
        status_end = virk_date(record$periode$gyldigTil),
        status_code = virk_scalar(record$statuskode %||% record$status),
        status_text = virk_scalar(record$statustekst %||% record$status)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  matching_status <- status_periods[
    (is.na(status_start) | status_start <= date) &
      (is.na(status_end) | status_end >= date)
  ]

  if (nrow(matching_status) == 0) {
    return(out)
  }

  setorder(matching_status, -status_start, na.last = TRUE)
  list(
    status_code = matching_status$status_code[1],
    status_text = matching_status$status_text[1]
  )
}

extract_attribute_periods <- function(firm, attribute_type) {
  if (is.null(firm$attributter) || length(firm$attributter) == 0) {
    return(data.table(
      value = character(),
      period_start = as.IDate(character()),
      period_end = as.IDate(character())
    ))
  }

  attrs <- firm$attributter[
    vapply(
      firm$attributter,
      function(attribute) identical(virk_scalar(attribute$type), attribute_type),
      logical(1)
    )
  ]

  if (length(attrs) == 0) {
    return(data.table(
      value = character(),
      period_start = as.IDate(character()),
      period_end = as.IDate(character())
    ))
  }

  rbindlist(
    lapply(attrs, function(attribute) {
      values <- attribute$vaerdier
      if (is.null(values) || length(values) == 0) {
        return(data.table())
      }

      rbindlist(
        lapply(values, function(value_record) {
          start <- virk_date(value_record$periode$gyldigFra)
          end <- virk_date(value_record$periode$gyldigTil)

          if (is.na(start)) {
            start <- virk_date(attribute$periode$gyldigFra)
          }
          if (is.na(end)) {
            end <- virk_date(attribute$periode$gyldigTil)
          }

          data.table(
            value = virk_scalar(value_record$vaerdi),
            period_start = start,
            period_end = end
          )
        }),
        use.names = TRUE,
        fill = TRUE
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
}

attribute_value_at <- function(attribute_periods, date) {
  if (nrow(attribute_periods) == 0 || is.na(date)) {
    return(NA_character_)
  }

  matches <- attribute_periods[
    (is.na(period_start) | period_start <= date) &
      (is.na(period_end) | period_end >= date)
  ]

  if (nrow(matches) == 0) {
    return(NA_character_)
  }

  setorder(matches, -period_start, na.last = TRUE)
  matches$value[1]
}

company_context <- function(firm) {
  lifecycle_periods <- extract_lifecycle_periods(firm)
  lifecycle_bounds <- firm_lifecycle_bounds(lifecycle_periods)
  registration_date <- virk_scalar(firm$stiftelsesDato)

  if (is.na(registration_date) || registration_date == "") {
    registration_date <- as.character(lifecycle_bounds$lifecycle_start)
  }

  hq_address <- address_parts(firm$virksomhedMetadata$nyesteBeliggenhedsadresse)

  list(
    cvr = format_virk_cvr(firm$cvrNummer),
    firm_name = virk_scalar(firm$virksomhedMetadata$nyesteNavn$navn),
    registration_date = registration_date,
    hq_kommune_code = hq_address$kommune_code,
    hq_kommune_name = hq_address$kommune_name,
    hq_postnummer = hq_address$postnummer,
    hq_city = hq_address$city,
    lifecycle_periods = lifecycle_periods,
    lifecycle_start = lifecycle_bounds$lifecycle_start,
    lifecycle_end = lifecycle_bounds$lifecycle_end,
    capital_periods = extract_attribute_periods(firm, "KAPITAL"),
    capital_currency_periods = extract_attribute_periods(firm, "KAPITALVALUTA")
  )
}

empty_company_context <- function(cvr) {
  lifecycle_periods <- data.table(
    lifecycle_start = as.IDate(NA_character_),
    lifecycle_end = as.IDate(NA_character_)
  )

  list(
    cvr = cvr,
    firm_name = NA_character_,
    registration_date = NA_character_,
    hq_kommune_code = NA_character_,
    hq_kommune_name = NA_character_,
    hq_postnummer = NA_character_,
    hq_city = NA_character_,
    lifecycle_periods = lifecycle_periods,
    lifecycle_start = as.IDate(NA_character_),
    lifecycle_end = as.IDate(NA_character_),
    capital_periods = data.table(
      value = character(),
      period_start = as.IDate(character()),
      period_end = as.IDate(character())
    ),
    capital_currency_periods = data.table(
      value = character(),
      period_start = as.IDate(character()),
      period_end = as.IDate(character())
    )
  )
}

set_context_fields <- function(rows, firm, context) {
  if (nrow(rows) == 0) {
    return(rows)
  }

  for (i in seq_len(nrow(rows))) {
    period_start <- as.IDate(rows$period_start[i])
    period_end <- as.IDate(rows$period_end[i])
    status <- if (is.null(firm)) {
      list(status_code = NA_character_, status_text = NA_character_)
    } else {
      status_at_date(firm, period_end)
    }

    set(rows, i, "firm_name", context$firm_name)
    set(rows, i, "registration_date", context$registration_date)
    set(rows, i, "hq_kommune_code", context$hq_kommune_code)
    set(rows, i, "hq_kommune_name", context$hq_kommune_name)
    set(rows, i, "hq_postnummer", context$hq_postnummer)
    set(rows, i, "hq_city", context$hq_city)
    set(rows, i, "lifecycle_start", as.character(context$lifecycle_start))
    set(rows, i, "lifecycle_end", as.character(context$lifecycle_end))
    set(rows, i, "exists_at_period_start", firm_exists_on(context$lifecycle_periods, period_start))
    set(rows, i, "exists_at_period_end", firm_exists_on(context$lifecycle_periods, period_end))
    set(rows, i, "exists_during_period", firm_exists_during(
      context$lifecycle_periods,
      period_start,
      period_end
    ))
    set(rows, i, "status_code", status$status_code)
    set(rows, i, "status_text", status$status_text)

    if (!is.null(firm)) {
      form <- record_at_date(firm$virksomhedsform, period_end)
      industry <- record_at_date(firm$hovedbranche, period_end)
      secondary_1 <- record_at_date(firm$bibranche1, period_end)
      secondary_2 <- record_at_date(firm$bibranche2, period_end)
      secondary_3 <- record_at_date(firm$bibranche3, period_end)

      set(rows, i, "legal_form_code", as.integer(virk_scalar(form$virksomhedsformkode)))
      set(rows, i, "legal_form_short", virk_scalar(form$kortBeskrivelse))
      set(rows, i, "legal_form_text", virk_scalar(form$langBeskrivelse))
      set(rows, i, "industry_code", virk_scalar(industry$branchekode))
      set(rows, i, "industry_text", virk_scalar(industry$branchetekst))
      set(rows, i, "secondary_industry_1_code", virk_scalar(secondary_1$branchekode))
      set(rows, i, "secondary_industry_1_text", virk_scalar(secondary_1$branchetekst))
      set(rows, i, "secondary_industry_2_code", virk_scalar(secondary_2$branchekode))
      set(rows, i, "secondary_industry_2_text", virk_scalar(secondary_2$branchetekst))
      set(rows, i, "secondary_industry_3_code", virk_scalar(secondary_3$branchekode))
      set(rows, i, "secondary_industry_3_text", virk_scalar(secondary_3$branchetekst))

      address <- record_at_date(firm$beliggenhedsadresse, period_end)
      ap <- address_parts(address)
      set(rows, i, "kommune_code", ap$kommune_code)
      set(rows, i, "kommune_name", ap$kommune_name)
      set(rows, i, "postnummer", ap$postnummer)
      set(rows, i, "city", ap$city)
    }

    capital_value <- attribute_value_at(context$capital_periods, period_end)
    set(rows, i, "capital_amount", suppressWarnings(as.numeric(capital_value)))
    set(rows, i, "capital_currency", attribute_value_at(
      context$capital_currency_periods,
      period_end
    ))
  }

  rows
}

# -- Name-history parsing ------------------------------------------------------

# Flatten a firm's name arrays (navne = primary, binavne = secondary) into the
# name-history key. Each element carries a validity period exactly like the other
# CVR arrays (periode$gyldigFra / gyldigTil), so we reuse virk_date() and treat a
# null gyldigTil as "still current".
extract_name_history <- function(firm) {
  cvr <- format_virk_cvr(firm$cvrNummer)

  parse_names <- function(records, name_type) {
    if (is.null(records) || length(records) == 0) {
      return(empty_name_history_table())
    }

    rbindlist(
      lapply(records, function(record) {
        gyldig_til <- virk_date(record$periode$gyldigTil)
        data.table(
          cvr = cvr,
          name_type = name_type,
          firm_name = virk_scalar(record$navn),
          gyldig_fra = as.character(virk_date(record$periode$gyldigFra)),
          gyldig_til = as.character(gyldig_til),
          is_current = is.na(gyldig_til)
        )
      }),
      use.names = TRUE,
      fill = TRUE
    )
  }

  out <- rbindlist(
    list(
      parse_names(firm$navne, "primary"),
      parse_names(firm$binavne, "secondary")
    ),
    use.names = TRUE,
    fill = TRUE
  )

  # Drop blank names; keep genuine history (a name can recur across periods).
  out <- out[!is.na(firm_name) & nzchar(firm_name)]
  if (nrow(out) == 0) {
    return(empty_name_history_table())
  }

  setcolorder(out, names(empty_name_history_table()))
  out[]
}

# -- Employment parsing --------------------------------------------------------

extract_historical_employment_rows <- function(firm, records, frequency) {
  if (is.null(records) || length(records) == 0) {
    return(empty_employment_table())
  }

  context <- company_context(firm)
  out <- rbindlist(
    lapply(records, function(record) {
      period_bounds <- employment_period_bounds(record, frequency)
      employees <- as.numeric(virk_scalar(record$antalAnsatte))
      fte <- as.numeric(virk_scalar(record$antalAarsvaerk))
      employees_with_owners <- as.numeric(virk_scalar(record$antalInklusivEjere))
      employee_interval <- virk_scalar(record$intervalKodeAntalAnsatte)
      fte_interval <- virk_scalar(record$intervalKodeAntalAarsvaerk)
      employees_with_owners_interval <- virk_scalar(record$intervalKodeAntalInklusivEjere)

      data.table(
        cvr = context$cvr,
        firm_name = NA_character_,
        registration_date = NA_character_,
        frequency = frequency,
        year = as.integer(virk_scalar(record$aar)),
        quarter = as.integer(virk_scalar(record$kvartal)),
        month = as.integer(virk_scalar(record$maaned)),
        employees = employees,
        fte = fte,
        employees_including_owners = employees_with_owners,
        employees_historical = employees,
        fte_historical = fte,
        employees_including_owners_historical = employees_with_owners,
        employees_new = NA_real_,
        fte_new = NA_real_,
        employees_including_owners_new = NA_real_,
        employee_interval = employee_interval,
        fte_interval = fte_interval,
        employees_including_owners_interval = employees_with_owners_interval,
        employee_interval_historical = employee_interval,
        fte_interval_historical = fte_interval,
        employees_including_owners_interval_historical = employees_with_owners_interval,
        employee_interval_new = NA_character_,
        fte_interval_new = NA_character_,
        employees_including_owners_interval_new = NA_character_,
        employment_source = "historical",
        has_historical_new_overlap = FALSE,
        n_production_units_aggregated = NA_integer_,
        n_production_units_nonmissing_employees = NA_integer_,
        n_production_units_nonmissing_fte = NA_integer_,
        period_start = as.character(period_bounds$period_start),
        period_end = as.character(period_bounds$period_end),
        lifecycle_start = NA_character_,
        lifecycle_end = NA_character_,
        exists_at_period_start = NA,
        exists_at_period_end = NA,
        exists_during_period = NA,
        status_code = NA_character_,
        status_text = NA_character_,
        legal_form_code = NA_integer_,
        legal_form_short = NA_character_,
        legal_form_text = NA_character_,
        industry_code = NA_character_,
        industry_text = NA_character_,
        secondary_industry_1_code = NA_character_,
        secondary_industry_1_text = NA_character_,
        secondary_industry_2_code = NA_character_,
        secondary_industry_2_text = NA_character_,
        secondary_industry_3_code = NA_character_,
        secondary_industry_3_text = NA_character_,
        kommune_code = NA_character_,
        kommune_name = NA_character_,
        postnummer = NA_character_,
        city = NA_character_,
        hq_kommune_code = NA_character_,
        hq_kommune_name = NA_character_,
        hq_postnummer = NA_character_,
        hq_city = NA_character_,
        capital_amount = NA_real_,
        capital_currency = NA_character_,
        derived_from_monthly = FALSE,
        derived_months_observed = NA_integer_,
        derived_months_expected = NA_integer_,
        derived_period_complete = NA,
        updated_at = virk_scalar(record$sidstOpdateret),
        updated_at_historical = virk_scalar(record$sidstOpdateret),
        updated_at_new = NA_character_
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  set_context_fields(out, firm, context)
  setcolorder(out, names(empty_employment_table()))
  out
}

extract_virk_employment_history <- function(firm) {
  rbindlist(
    list(
      extract_historical_employment_rows(firm, firm$aarsbeskaeftigelse, "annual"),
      extract_historical_employment_rows(firm, firm$kvartalsbeskaeftigelse, "quarterly"),
      extract_historical_employment_rows(firm, firm$maanedsbeskaeftigelse, "monthly")
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

production_unit_cvr_at <- function(unit, date) {
  relation <- record_at_date(unit$virksomhedsrelation, date)

  if (is.null(relation) && !is.null(unit$virksomhedsrelation) &&
      length(unit$virksomhedsrelation) > 0) {
    relation <- unit$virksomhedsrelation[[1]]
  }

  format_virk_cvr(relation$cvrNummer)
}

extract_production_unit_new_employment <- function(unit) {
  records <- unit$erstMaanedsbeskaeftigelse

  if (is.null(records) || length(records) == 0) {
    return(empty_production_unit_employment_table())
  }

  out <- rbindlist(
    lapply(records, function(record) {
      period_bounds <- employment_period_bounds(record, "monthly")
      data.table(
        cvr = production_unit_cvr_at(unit, period_bounds$period_end),
        p_nummer = virk_scalar(unit$pNummer),
        year = as.integer(virk_scalar(record$aar)),
        month = as.integer(virk_scalar(record$maaned)),
        employees_new = as.numeric(virk_scalar(record$antalAnsatte)),
        fte_new = as.numeric(virk_scalar(record$antalAarsvaerk)),
        employees_including_owners_new = as.numeric(virk_scalar(record$antalInklusivEjere)),
        employee_interval_new = virk_scalar(record$intervalKodeAntalAnsatte),
        fte_interval_new = virk_scalar(record$intervalKodeAntalAarsvaerk),
        employees_including_owners_interval_new = virk_scalar(record$intervalKodeAntalInklusivEjere),
        updated_at_new = virk_scalar(record$sidstOpdateret),
        period_start = as.character(period_bounds$period_start),
        period_end = as.character(period_bounds$period_end)
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  out[!is.na(cvr) & grepl("^[0-9]{8}$", cvr)]
}

aggregate_production_unit_monthly <- function(production_units) {
  if (length(production_units) == 0) {
    return(data.table())
  }

  unit_rows <- rbindlist(
    lapply(production_units, extract_production_unit_new_employment),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(unit_rows) == 0) {
    return(data.table())
  }

  unit_rows[
    ,
    .(
      employees_new = safe_sum(employees_new),
      fte_new = safe_sum(fte_new),
      employees_including_owners_new = safe_sum(employees_including_owners_new),
      employee_interval_new = if (uniqueN(p_nummer) == 1L) {
        first_nonmissing(employee_interval_new)
      } else {
        NA_character_
      },
      fte_interval_new = if (uniqueN(p_nummer) == 1L) {
        first_nonmissing(fte_interval_new)
      } else {
        NA_character_
      },
      employees_including_owners_interval_new = if (uniqueN(p_nummer) == 1L) {
        first_nonmissing(employees_including_owners_interval_new)
      } else {
        NA_character_
      },
      n_production_units_aggregated = uniqueN(p_nummer),
      n_production_units_nonmissing_employees = uniqueN(p_nummer[!is.na(employees_new)]),
      n_production_units_nonmissing_fte = uniqueN(p_nummer[!is.na(fte_new)]),
      updated_at_new = safe_max_char(updated_at_new)
    ),
    by = .(cvr, year, month, period_start, period_end)
  ]
}

build_new_monthly_rows <- function(new_monthly, firms_by_cvr) {
  if (nrow(new_monthly) == 0) {
    return(empty_employment_table())
  }

  rows <- rbindlist(
    lapply(split(new_monthly, by = "cvr", keep.by = TRUE), function(cvr_rows) {
      cvr <- cvr_rows$cvr[1]
      firm <- firms_by_cvr[[cvr]]
      context <- if (is.null(firm)) empty_company_context(cvr) else company_context(firm)

      out <- cvr_rows[
        ,
        .(
          cvr = cvr,
          firm_name = NA_character_,
          registration_date = NA_character_,
          frequency = "monthly",
          year = year,
          quarter = NA_integer_,
          month = month,
          employees = employees_new,
          fte = fte_new,
          employees_including_owners = employees_including_owners_new,
          employees_historical = NA_real_,
          fte_historical = NA_real_,
          employees_including_owners_historical = NA_real_,
          employees_new = employees_new,
          fte_new = fte_new,
          employees_including_owners_new = employees_including_owners_new,
          employee_interval = employee_interval_new,
          fte_interval = fte_interval_new,
          employees_including_owners_interval = employees_including_owners_interval_new,
          employee_interval_historical = NA_character_,
          fte_interval_historical = NA_character_,
          employees_including_owners_interval_historical = NA_character_,
          employee_interval_new = employee_interval_new,
          fte_interval_new = fte_interval_new,
          employees_including_owners_interval_new = employees_including_owners_interval_new,
          employment_source = "new",
          has_historical_new_overlap = FALSE,
          n_production_units_aggregated = n_production_units_aggregated,
          n_production_units_nonmissing_employees = n_production_units_nonmissing_employees,
          n_production_units_nonmissing_fte = n_production_units_nonmissing_fte,
          period_start = period_start,
          period_end = period_end,
          lifecycle_start = NA_character_,
          lifecycle_end = NA_character_,
          exists_at_period_start = NA,
          exists_at_period_end = NA,
          exists_during_period = NA,
          status_code = NA_character_,
          status_text = NA_character_,
          legal_form_code = NA_integer_,
          legal_form_short = NA_character_,
          legal_form_text = NA_character_,
          industry_code = NA_character_,
          industry_text = NA_character_,
          secondary_industry_1_code = NA_character_,
          secondary_industry_1_text = NA_character_,
          secondary_industry_2_code = NA_character_,
          secondary_industry_2_text = NA_character_,
          secondary_industry_3_code = NA_character_,
          secondary_industry_3_text = NA_character_,
          kommune_code = NA_character_,
          kommune_name = NA_character_,
          postnummer = NA_character_,
          city = NA_character_,
          hq_kommune_code = NA_character_,
          hq_kommune_name = NA_character_,
          hq_postnummer = NA_character_,
          hq_city = NA_character_,
          capital_amount = NA_real_,
          capital_currency = NA_character_,
          derived_from_monthly = FALSE,
          derived_months_observed = NA_integer_,
          derived_months_expected = NA_integer_,
          derived_period_complete = NA,
          updated_at = updated_at_new,
          updated_at_historical = NA_character_,
          updated_at_new = updated_at_new
        )
      ]

      set_context_fields(out, firm, context)
    }),
    use.names = TRUE,
    fill = TRUE
  )

  setcolorder(rows, names(empty_employment_table()))
  rows
}

collapse_employment_sources <- function(data) {
  if (nrow(data) == 0) {
    return(empty_employment_table())
  }

  key_cols <- c(
    "cvr", "frequency", "year", "quarter", "month",
    "period_start", "period_end"
  )

  collapse_cols <- setdiff(
    names(empty_employment_table()),
    c(
      key_cols,
      "employees", "fte", "employees_including_owners",
      "employee_interval", "fte_interval", "employees_including_owners_interval",
      "employment_source", "has_historical_new_overlap", "updated_at"
    )
  )

  out <- data[
    ,
    lapply(.SD, first_nonmissing),
    by = key_cols,
    .SDcols = collapse_cols
  ]

  out[, employees := fifelse(!is.na(employees_historical), employees_historical, employees_new)]
  out[, fte := fifelse(!is.na(fte_historical), fte_historical, fte_new)]
  out[, employees_including_owners := fifelse(
    !is.na(employees_including_owners_historical),
    employees_including_owners_historical,
    employees_including_owners_new
  )]
  out[, employee_interval := fifelse(
    !is.na(employee_interval_historical),
    employee_interval_historical,
    employee_interval_new
  )]
  out[, fte_interval := fifelse(
    !is.na(fte_interval_historical),
    fte_interval_historical,
    fte_interval_new
  )]
  out[, employees_including_owners_interval := fifelse(
    !is.na(employees_including_owners_interval_historical),
    employees_including_owners_interval_historical,
    employees_including_owners_interval_new
  )]
  out[
    ,
    has_historical_new_overlap :=
      (!is.na(employees_historical) | !is.na(fte_historical) |
         !is.na(employees_including_owners_historical)) &
      (!is.na(employees_new) | !is.na(fte_new) |
         !is.na(employees_including_owners_new))
  ]
  out[
    ,
    employment_source := mapply(
      spliced_source,
      !is.na(employees_historical) | !is.na(fte_historical) |
        !is.na(employees_including_owners_historical),
      !is.na(employees_new) | !is.na(fte_new) |
        !is.na(employees_including_owners_new)
    )
  ]
  out[, updated_at := fifelse(
    !is.na(updated_at_historical),
    updated_at_historical,
    updated_at_new
  )]

  setcolorder(out, names(empty_employment_table()))
  out[]
}

make_derived_rows <- function(monthly_rows, firms_by_cvr, frequency) {
  if (nrow(monthly_rows) == 0) {
    return(empty_employment_table())
  }

  monthly <- copy(monthly_rows)

  if (frequency == "quarterly_derived") {
    monthly[, derived_quarter := as.integer((month - 1L) %/% 3L + 1L)]
    grouping <- c("cvr", "year", "derived_quarter")
    expected <- 3L
  } else {
    monthly[, derived_quarter := NA_integer_]
    grouping <- c("cvr", "year")
    expected <- 12L
  }

  derived <- monthly[
    ,
    .(
      employees = safe_mean(employees),
      fte = safe_mean(fte),
      employees_including_owners = safe_mean(employees_including_owners),
      employees_historical = safe_mean(employees_historical),
      fte_historical = safe_mean(fte_historical),
      employees_including_owners_historical = safe_mean(employees_including_owners_historical),
      employees_new = safe_mean(employees_new),
      fte_new = safe_mean(fte_new),
      employees_including_owners_new = safe_mean(employees_including_owners_new),
      employment_source = derive_source(employment_source),
      has_historical_new_overlap = any(has_historical_new_overlap, na.rm = TRUE),
      n_production_units_aggregated = safe_max_int(n_production_units_aggregated),
      n_production_units_nonmissing_employees = safe_max_int(n_production_units_nonmissing_employees),
      n_production_units_nonmissing_fte = safe_max_int(n_production_units_nonmissing_fte),
      derived_months_observed = uniqueN(month),
      updated_at_historical = safe_max_char(updated_at_historical),
      updated_at_new = safe_max_char(updated_at_new)
    ),
    by = grouping
  ]

  if (frequency == "quarterly_derived") {
    setnames(derived, "derived_quarter", "quarter")
    derived[, month := NA_integer_]
  } else {
    derived[, quarter := NA_integer_]
    derived[, month := NA_integer_]
  }

  derived[, frequency := frequency]
  derived[, derived_from_monthly := TRUE]
  derived[, derived_months_expected := expected]
  derived[, derived_period_complete := derived_months_observed == expected]
  derived[, employee_interval := NA_character_]
  derived[, fte_interval := NA_character_]
  derived[, employees_including_owners_interval := NA_character_]
  derived[, employee_interval_historical := NA_character_]
  derived[, fte_interval_historical := NA_character_]
  derived[, employees_including_owners_interval_historical := NA_character_]
  derived[, employee_interval_new := NA_character_]
  derived[, fte_interval_new := NA_character_]
  derived[, employees_including_owners_interval_new := NA_character_]
  derived[, updated_at := fifelse(
    !is.na(updated_at_historical),
    updated_at_historical,
    updated_at_new
  )]

  bounds <- mapply(
    calendar_period_bounds,
    derived$year,
    derived$frequency,
    derived$quarter,
    SIMPLIFY = FALSE
  )
  derived[, period_start := vapply(bounds, function(x) as.character(x$period_start), character(1))]
  derived[, period_end := vapply(bounds, function(x) as.character(x$period_end), character(1))]

  derived[, `:=`(
    firm_name = NA_character_,
    registration_date = NA_character_,
    lifecycle_start = NA_character_,
    lifecycle_end = NA_character_,
    exists_at_period_start = NA,
    exists_at_period_end = NA,
    exists_during_period = NA,
    status_code = NA_character_,
    status_text = NA_character_,
    legal_form_code = NA_integer_,
    legal_form_short = NA_character_,
    legal_form_text = NA_character_,
    industry_code = NA_character_,
    industry_text = NA_character_,
    secondary_industry_1_code = NA_character_,
    secondary_industry_1_text = NA_character_,
    secondary_industry_2_code = NA_character_,
    secondary_industry_2_text = NA_character_,
    secondary_industry_3_code = NA_character_,
    secondary_industry_3_text = NA_character_,
    kommune_code = NA_character_,
    kommune_name = NA_character_,
    postnummer = NA_character_,
    city = NA_character_,
    hq_kommune_code = NA_character_,
    hq_kommune_name = NA_character_,
    hq_postnummer = NA_character_,
    hq_city = NA_character_,
    capital_amount = NA_real_,
    capital_currency = NA_character_
  )]

  out <- rbindlist(
    lapply(split(derived, by = "cvr", keep.by = TRUE), function(cvr_rows) {
      cvr <- cvr_rows$cvr[1]
      firm <- firms_by_cvr[[cvr]]
      context <- if (is.null(firm)) empty_company_context(cvr) else company_context(firm)
      set_context_fields(cvr_rows, firm, context)
    }),
    use.names = TRUE,
    fill = TRUE
  )

  setcolorder(out, names(empty_employment_table()))
  out[]
}

add_derived_frequencies <- function(native_data, firms_by_cvr) {
  monthly_rows <- native_data[frequency == "monthly"]

  rbindlist(
    list(
      native_data,
      make_derived_rows(monthly_rows, firms_by_cvr, "quarterly_derived"),
      make_derived_rows(monthly_rows, firms_by_cvr, "annual_derived")
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

# Splice the native and derived annual/quarterly series into a single continuous
# per-firm series (annual_spliced / quarterly_spliced). For each period, prefer
# the official native row where it exists; otherwise take the monthly-derived
# row (which extends coverage past the native series' end, ~2019/2020 onward, and
# fills any native gaps from 2015 on). The whole row is carried through, so
# `derived_from_monthly` on a spliced row flags whether that observation came
# from the derived series (TRUE) or the official native series (FALSE), and
# `employment_source` keeps the underlying provenance.
add_spliced_frequencies <- function(data) {
  splice_one <- function(native_freq, derived_freq, out_freq, key) {
    native  <- data[frequency == native_freq]
    derived <- data[frequency == derived_freq]

    if (nrow(native) == 0L && nrow(derived) == 0L) {
      return(empty_employment_table())
    }

    # Derived rows for periods the native series does not already cover.
    derived_fill <- if (nrow(native) == 0L) derived else derived[!native, on = key]

    spliced <- rbindlist(list(native, derived_fill), use.names = TRUE, fill = TRUE)
    spliced[, frequency := out_freq]
    setcolorder(spliced, names(empty_employment_table()))
    spliced
  }

  rbindlist(
    list(
      data,
      splice_one("annual",    "annual_derived",    "annual_spliced",    c("cvr", "year")),
      splice_one("quarterly", "quarterly_derived", "quarterly_spliced", c("cvr", "year", "quarter"))
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

# -- Input CVRs and output writers --------------------------------------------

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

# Write one batch's employment/name data as its own compressed RDS chunk, named by
# a stable id (the batch's first CVR). Chunks accumulate during the run and are
# combined into the single output RDS at the end. Saving the real data.table (not a
# CSV round-trip) keeps the exact column schema/types of the current output.
append_employment_chunk <- function(data, dir, chunk_id) {
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, file.path(dir, paste0(chunk_id, ".rds")))

  invisible(NULL)
}

# Combine every chunk in `dir` into a single compressed RDS. The chunk directory is
# only removed AFTER a successful save, so an out-of-memory combine leaves the
# chunks intact for a manual retry / larger-memory run.
combine_chunks_to_rds <- function(dir, out_path) {
  if (!dir.exists(dir)) {
    return(invisible(0L))
  }

  files <- list.files(dir, pattern = "[.]rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(invisible(0L))
  }

  combined <- rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
  saveRDS(combined, out_path)
  unlink(dir, recursive = TRUE)

  invisible(nrow(combined))
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

append_name_chunk <- function(data, dir, chunk_id) {
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, file.path(dir, paste0(chunk_id, ".rds")))

  invisible(NULL)
}

already_processed_cvrs <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  header <- names(fread(path, nrows = 0))
  if (!"employment_pull_schema" %in% header) {
    stop(
      paste(
        "Existing status file was created by an older employment pull schema:",
        path,
        "Set CVR_EMPLOYMENT_OVERWRITE=true or write to a new CVR_EMPLOYMENT_OUTPUT_FILE.",
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  status <- fread(
    path,
    select = c("cvr", "employment_pull_schema"),
    colClasses = "character"
  )

  unique(status[status$employment_pull_schema == employment_pull_schema, cvr])
}

# CVRs for which we actually recovered employment data (>=1 employment row) under
# the current schema. This is the "realised" set the iterative target is measured
# against -- firms found in Virk but with no reported employment do not count.
recovered_from_status <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  header <- names(fread(path, nrows = 0))
  if (!all(c("employment_pull_schema", "employment_rows") %in% header)) {
    return(character())
  }

  status <- fread(
    path,
    select = c("cvr", "employment_pull_schema", "employment_rows"),
    colClasses = list(character = "cvr")
  )

  unique(status[
    status$employment_pull_schema == employment_pull_schema & employment_rows > 0,
    cvr
  ])
}

validate_resume_state <- function(output_path, status_path, chunk_dirs, overwrite) {
  if (overwrite) {
    return(invisible(NULL))
  }

  # The status ledger is the source of truth for what has been processed. If any
  # output (final RDS or accumulated chunks) exists but the status file does not,
  # we cannot resume safely -- force an explicit rebuild rather than duplicate work.
  chunks_exist <- any(vapply(
    chunk_dirs,
    function(d) dir.exists(d) && length(list.files(d, pattern = "[.]rds$")) > 0,
    logical(1)
  ))

  if ((file.exists(output_path) || chunks_exist) && !file.exists(status_path)) {
    stop(
      paste(
        "Output/chunks exist but the status file is missing:",
        status_path,
        "Set CVR_EMPLOYMENT_OVERWRITE=true or write to a new CVR_EMPLOYMENT_OUTPUT_FILE.",
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

# -- API query helpers ---------------------------------------------------------

company_source_fields <- function() {
  c(
    "Vrvirksomhed.cvrNummer",
    "Vrvirksomhed.virksomhedMetadata.nyesteNavn",
    "Vrvirksomhed.navne",
    "Vrvirksomhed.binavne",
    "Vrvirksomhed.stiftelsesDato",
    "Vrvirksomhed.livsforloeb",
    "Vrvirksomhed.status",
    "Vrvirksomhed.virksomhedsstatus",
    "Vrvirksomhed.aarsbeskaeftigelse",
    "Vrvirksomhed.kvartalsbeskaeftigelse",
    "Vrvirksomhed.maanedsbeskaeftigelse",
    "Vrvirksomhed.virksomhedsform",
    "Vrvirksomhed.hovedbranche",
    "Vrvirksomhed.bibranche1",
    "Vrvirksomhed.bibranche2",
    "Vrvirksomhed.bibranche3",
    "Vrvirksomhed.beliggenhedsadresse",
    "Vrvirksomhed.virksomhedMetadata.nyesteBeliggenhedsadresse",
    "Vrvirksomhed.attributter"
  )
}

company_query_body <- function(cvrs) {
  body <- list(
    size = length(cvrs),
    query = list(
      terms = setNames(
        list(as.integer(cvrs)),
        "Vrvirksomhed.cvrNummer"
      )
    )
  )

  body[["_source"]] <- company_source_fields()
  body
}

production_unit_source_fields <- function() {
  c(
    "VrproduktionsEnhed.pNummer",
    "VrproduktionsEnhed.virksomhedsrelation",
    "VrproduktionsEnhed.erstMaanedsbeskaeftigelse"
  )
}

production_unit_query_body <- function(cvrs, size) {
  body <- list(
    size = size,
    query = list(
      terms = setNames(
        list(as.integer(cvrs)),
        "VrproduktionsEnhed.virksomhedsrelation.cvrNummer"
      )
    ),
    sort = list("_doc")
  )

  body[["_source"]] <- production_unit_source_fields()
  body
}

virk_post_json_retry <- function(url,
                                 body,
                                 query = list(),
                                 credentials,
                                 max_attempts = 3L,
                                 initial_sleep = 2) {
  last_error <- NULL

  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      virk_post_json(url, body, query = query, credentials = credentials),
      error = function(error) {
        last_error <<- error
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    if (attempt < max_attempts) {
      Sys.sleep(initial_sleep * attempt)
    }
  }

  stop(last_error)
}

validate_search_result <- function(result, context) {
  if (isTRUE(result$timed_out)) {
    stop("Virk query timed out: ", context, call. = FALSE)
  }

  if (is.null(result$hits) || is.null(result$hits$hits)) {
    stop("Unexpected Virk response structure: ", context, call. = FALSE)
  }

  invisible(TRUE)
}

fetch_companies <- function(cvrs, credentials) {
  result <- virk_post_json_retry(
    company_search_url,
    company_query_body(cvrs),
    credentials = credentials
  )
  validate_search_result(result, "company batch")

  lapply(result$hits$hits, function(hit) {
    hit$`_source`$Vrvirksomhed
  })
}

fetch_production_units <- function(cvrs, credentials) {
  result <- virk_post_json_retry(
    production_unit_search_url,
    production_unit_query_body(cvrs, scroll_size),
    query = list(scroll = scroll_keepalive),
    credentials = credentials
  )
  validate_search_result(result, "production-unit initial batch")

  total_hits <- as.integer(result$hits$total %||% length(result$hits$hits))
  scroll_id <- result$`_scroll_id`
  hits <- result$hits$hits
  all_hits <- hits

  while (length(all_hits) < total_hits) {
    if (is.null(scroll_id) || !nzchar(scroll_id)) {
      stop("Missing scroll id before all production-unit hits were fetched.", call. = FALSE)
    }

    result <- virk_post_json_retry(
      scroll_url,
      list(scroll = scroll_keepalive, scroll_id = scroll_id),
      credentials = credentials
    )
    validate_search_result(result, "production-unit scroll")

    scroll_id <- result$`_scroll_id`
    hits <- result$hits$hits

    if (length(hits) == 0) {
      break
    }

    all_hits <- c(all_hits, hits)
  }

  if (length(all_hits) != total_hits) {
    stop(
      "Production-unit scroll returned ", length(all_hits),
      " hits, expected ", total_hits, ".",
      call. = FALSE
    )
  }

  lapply(all_hits, function(hit) {
    hit$`_source`$VrproduktionsEnhed
  })
}

# -- Run -----------------------------------------------------------------------

# Universe of firms = distinct CVRs in the Virk name key.
universe_key <- readRDS(universe_file)
universe_cvrs <- unique(sprintf("%08d", as.integer(universe_key$cvr)))
universe_cvrs <- universe_cvrs[grepl("^[0-9]{8}$", universe_cvrs)]
rm(universe_key)

# Exclude tender winners -> the never-win universe.
winner_cvrs <- read_matched_cvrs(winner_cvr_files)
never_win_cvrs <- sort(setdiff(universe_cvrs, winner_cvrs))
n_universe <- length(never_win_cvrs)

# Fixed shuffle -> deterministic, resumable draw order; each round takes the next
# un-queried slice. Target = recover employment for >= sample_share of the universe.
# Pin the RNG algorithm explicitly so the shuffle is identical on any R >= 3.6 and
# any machine (not just under the current default RNG); combined with the sorted,
# deterministic never_win_cvrs input, the sample is fully reproducible.
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
set.seed(sample_seed)
shuffled_cvrs <- sample(never_win_cvrs)
target_count <- ceiling(sample_share * n_universe)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

if (overwrite) {
  if (file.exists(output_file)) file.remove(output_file)
  if (file.exists(status_file)) file.remove(status_file)
  if (file.exists(name_output_file)) file.remove(name_output_file)
  unlink(emp_chunks_dir, recursive = TRUE)
  unlink(name_chunks_dir, recursive = TRUE)
}

validate_resume_state(output_file, status_file, c(emp_chunks_dir, name_chunks_dir), overwrite)

company_search_url <- "http://distribution.virk.dk/cvr-permanent/virksomhed/_search"
production_unit_search_url <- "http://distribution.virk.dk/cvr-permanent/produktionsenhed/_search"
scroll_url <- "http://distribution.virk.dk/_search/scroll"
credentials <- get_virk_credentials()

cat("Firm universe (name key):", length(universe_cvrs), "\n")
cat("Tender winners excluded:", length(winner_cvrs), "\n")
cat("Never-win universe:", n_universe, "\n")
cat("Target share:", sample_share, "-> target recovered firms:", target_count, "\n")
cat("Batch size:", batch_size, "\n")
cat("Production-unit scroll size:", scroll_size, "\n")
cat("Output file:", output_file, "\n")
cat("Status file:", status_file, "\n")
cat("Name-history file:", name_output_file, "\n")

# Pull one set of CVRs through the Virk endpoints, writing output / name-history /
# status incrementally -- byte-for-byte the same per-batch work as the winner pull.
pull_cvrs <- function(cvrs_to_pull) {
  for (start in seq(1L, length(cvrs_to_pull), by = batch_size)) {
    end <- min(start + batch_size - 1L, length(cvrs_to_pull))
    cvr_batch <- cvrs_to_pull[start:end]

    firms <- fetch_companies(cvr_batch, credentials)
    returned_cvrs <- vapply(firms, function(firm) {
      format_virk_cvr(firm$cvrNummer)
    }, character(1))
    firms_by_cvr <- setNames(firms, returned_cvrs)

    production_units <- fetch_production_units(cvr_batch, credentials)
    unit_cvrs <- vapply(production_units, function(unit) {
      relation <- unit$virksomhedsrelation
      if (is.null(relation) || length(relation) == 0) {
        return(NA_character_)
      }
      format_virk_cvr(relation[[1]]$cvrNummer)
    }, character(1))
    production_units_by_cvr <- data.table(
      cvr = unit_cvrs,
      p_nummer = vapply(production_units, function(unit) {
        virk_scalar(unit$pNummer)
      }, character(1))
    )[!is.na(cvr)]

    historical_data <- if (length(firms) == 0) {
      empty_employment_table()
    } else {
      rbindlist(
        lapply(firms, extract_virk_employment_history),
        use.names = TRUE,
        fill = TRUE
      )
    }

    new_monthly <- aggregate_production_unit_monthly(production_units)
    # Keep only CVRs in this batch. A shared/transferred production unit is
    # matched under every CVR it was ever related to, and each month is
    # attributed to the CVR that owned the unit then - which may be an
    # out-of-batch CVR. Writing those out-of-batch rows here duplicates that
    # CVR's employment when it is later processed in its own batch (its own
    # batch already returns all its production units and attributes its own
    # months). So drop the leaked, out-of-batch attributions.
    if (nrow(new_monthly) > 0) {
      new_monthly <- new_monthly[cvr %chin% cvr_batch]
    }
    new_monthly_data <- build_new_monthly_rows(new_monthly, firms_by_cvr)
    native_data <- collapse_employment_sources(
      rbindlist(
        list(historical_data, new_monthly_data),
        use.names = TRUE,
        fill = TRUE
      )
    )
    employment_data <- add_derived_frequencies(native_data, firms_by_cvr)
    employment_data <- add_spliced_frequencies(employment_data)
    setorder(employment_data, cvr, frequency, year, quarter, month)

    append_employment_chunk(employment_data, emp_chunks_dir, cvr_batch[1])

    # Separate name-history key (from the same company documents already fetched).
    name_history_data <- if (length(firms) == 0) {
      empty_name_history_table()
    } else {
      rbindlist(lapply(firms, extract_name_history), use.names = TRUE, fill = TRUE)
    }
    append_name_chunk(name_history_data, name_chunks_dir, cvr_batch[1])

    rows_by_cvr <- employment_data[
      ,
      .(
        employment_rows = .N,
        historical_rows = sum(
          !is.na(employees_historical) |
            !is.na(fte_historical) |
            !is.na(employees_including_owners_historical)
        ),
        new_monthly_rows = sum(
          frequency == "monthly" &
            (!is.na(employees_new) |
               !is.na(fte_new) |
               !is.na(employees_including_owners_new))
        ),
        derived_rows = sum(frequency %chin% c("annual_derived", "quarterly_derived"))
      ),
      by = cvr
    ]
    production_units_returned <- production_units_by_cvr[
      ,
      .(production_units_returned = uniqueN(p_nummer)),
      by = cvr
    ]

    status_data <- data.table(cvr = cvr_batch)
    schema_version <- employment_pull_schema
    status_data[, pulled_at := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
    status_data[, employment_pull_schema := schema_version]
    status_data[, found_in_virk := cvr %in% returned_cvrs]
    status_data[, found_in_production_units := cvr %in% production_units_by_cvr$cvr]
    status_data <- rows_by_cvr[status_data, on = "cvr"]
    status_data <- production_units_returned[status_data, on = "cvr"]
    status_data[is.na(employment_rows), employment_rows := 0L]
    status_data[is.na(historical_rows), historical_rows := 0L]
    status_data[is.na(new_monthly_rows), new_monthly_rows := 0L]
    status_data[is.na(derived_rows), derived_rows := 0L]
    status_data[is.na(production_units_returned), production_units_returned := 0L]
    status_data <- status_data[
      ,
      .(
        cvr,
        pulled_at,
        employment_pull_schema,
        found_in_virk,
        found_in_production_units,
        employment_rows,
        historical_rows,
        new_monthly_rows,
        derived_rows,
        production_units_returned
      )
    ]

    append_status_chunk(status_data, status_file)

    cat(
      "Processed batch", start, "-", end,
      "| firms returned:", length(firms),
      "| production units returned:", length(production_units),
      "| rows written:", nrow(employment_data), "\n"
    )
  }
}

# -- Iterate: query, measure realised recovery, top up until target reached ----

timed <- system.time({
  round_i <- 0L
  repeat {
    processed_cvrs <- already_processed_cvrs(status_file)
    recovered_cvrs <- recovered_from_status(status_file)
    n_recovered <- length(recovered_cvrs)

    if (n_recovered >= target_count) {
      cat(sprintf(
        "Target reached: recovered %d >= target %d (%.2f%% of %d).\n",
        n_recovered, target_count, 100 * n_recovered / n_universe, n_universe
      ))
      break
    }

    # Next un-queried firms, in the fixed shuffle order.
    available <- setdiff(shuffled_cvrs, processed_cvrs)
    if (length(available) == 0L) {
      cat(sprintf(
        "Pool exhausted: recovered %d of target %d (%.2f%%); no more firms to query.\n",
        n_recovered, target_count, 100 * n_recovered / n_universe
      ))
      break
    }

    # Query 1.5 x the remaining deficit (a buffer so we converge in a few rounds),
    # capped by what is left in the pool.
    deficit <- target_count - n_recovered
    next_n <- min(ceiling(1.5 * deficit), length(available))
    cvr_round <- head(available, next_n)
    round_i <- round_i + 1L

    cat(sprintf(
      "Round %d: recovered %d / target %d (%.2f%%) | querying %d more (1.5 x deficit %d) | pool left %d\n",
      round_i, n_recovered, target_count, 100 * n_recovered / n_universe,
      next_n, deficit, length(available)
    ))

    pull_cvrs(cvr_round)
  }
})

recovered_cvrs <- recovered_from_status(status_file)
processed_cvrs <- already_processed_cvrs(status_file)
cat("Finished control employment-history pull.\n")
cat(sprintf(
  "Rounds: %d | firms queried: %d | recovered (>=1 employment row): %d | realised share: %.2f%% (target %.2f%%)\n",
  round_i, length(processed_cvrs), length(recovered_cvrs),
  100 * length(recovered_cvrs) / n_universe, 100 * sample_share
))
cat("Elapsed seconds:", unname(timed[["elapsed"]]), "\n")

# -- Combine per-batch chunks into the single output RDS -----------------------
# (Only removes the chunk dirs after a successful save, so a memory-limited combine
# leaves the chunks intact to retry.)
cat("Combining chunks into output RDS...\n")
n_emp_rows <- combine_chunks_to_rds(emp_chunks_dir, output_file)
n_name_rows <- combine_chunks_to_rds(name_chunks_dir, name_output_file)
cat("Employment RDS:", output_file, "(", n_emp_rows, "rows )\n")
cat("Name-history RDS:", name_output_file, "(", n_name_rows, "rows )\n")
