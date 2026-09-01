#!/usr/bin/env Rscript
# Build the matched control group for the firm-employment event study.
# For every awarded-tender winner-event, find one control firm (same sector,
# closest pre-award FTE and age) that did NOT win a tender in the event window,
# and assemble a stacked treated/control quarterly panel.
#
# Plain-R / non-interactive; meant to run in the background:
#   LC_ALL=en_US.UTF-8 Rscript code/analysis/find_control_firms.R
# Output: data/clean/control_event_list.rds

# ---- setup ----
rm(list = ls())
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(lubridate)
  library(parallel)
}))

# Project root (walk up to the .Rproj marker), then anchor paths off config.
report_project_dir <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(d, "cvr-cleaning.Rproj")) && dirname(d) != d) {
    d <- dirname(d)
  }
  d
})
setwd(report_project_dir)
source(file.path(report_project_dir, "config.R"))
clean_data_dir <- dirs$clean_data
emp_dir <- dirs$employment

# Read an employment-history table from the event-study dir, preferring the compact gzip .rds,
# then the working .csv, then (pre-migration) the legacy copy in clean/.
read_emp <- function(base) {
  rds <- file.path(emp_dir, paste0(base, ".rds"))
  if (file.exists(rds)) {
    d <- data.table::as.data.table(readRDS(rds))
  } else {
    csv <- file.path(emp_dir, paste0(base, ".csv"))
    if (!file.exists(csv)) csv <- file.path(clean_data_dir, paste0(base, ".csv"))
    d <- data.table::fread(csv, na.strings = "")
  }
  if ("cvr" %in% names(d)) d[, cvr := as.character(cvr)]
  d[]
}

valid_cvrs <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^0-9]", "", x)
  x <- x[grepl("^[0-9]{1,8}$", x)]
  unique(sprintf("%08d", as.integer(x)))
}

# ---- run toggle ----
# 1 = find and construct control data for each event (SLOW; the reason to run in
#     the background). 0 = read in pre-constructed control data.
construct_control_data <- 1

# ---- Load data ----
## Winner and never-winner employment panel
winner_emp_data <- read_emp("cvr_employment_history_virk")
control_emp_data <- readRDS(file.path(dirs$employment, "cvr_employment_history_control.rds"))
setDT(winner_emp_data); setDT(control_emp_data)

## Flag and bind; delete
winner_emp_data[, firm_type := "winner"]
control_emp_data[, firm_type := "never winner"]
firm_data <- data.table::rbindlist(list(winner_emp_data, control_emp_data))
rm(winner_emp_data); rm(control_emp_data)
gc()

## Tender winners (OpenTender + KFST) -> award events
data_ot <- readRDS(file.path(clean_data_dir, "clean_winner_data_ot_name_matched.rds")) %>%
  mutate(data_source = "OpenTender")
data_kfst <- readRDS(file.path(clean_data_dir, "clean_winner_data_kfst_name_matched.rds")) %>%
  mutate(data_source = "KFST")
data_tender <- rbindlist(list(data_ot, data_kfst), fill = TRUE, ignore.attr = TRUE) %>%
  select(data_source, winner_cvr_final, award_date, flag_awarded)

# Valid (8-digit) winner CVRs used to restrict the event universe.
valid_winner_cvrs <- valid_cvrs(c(data_ot$winner_cvr_final, data_kfst$winner_cvr_final))

# ---- Define universe of events ----
setDT(data_tender)
setorder(data_tender, winner_cvr_final, data_source, award_date)

# Awarded contracts only (drop annulled / never awarded), valid winner CVR only.
valid_winner_events <- data_tender[winner_cvr_final %in% valid_winner_cvrs & flag_awarded == TRUE, ]
valid_winner_events <- unique(valid_winner_events, by = c("winner_cvr_final", "award_date"))
valid_winner_events[, award_qidx := year(award_date) * 4 + quarter(award_date)]
valid_winner_events[, event_year := year(award_date)]
valid_winner_events[, event_quarter := quarter(award_date)]

# ---- Keys/types for fast matching ----
firm_data[, qidx := year * 4 + quarter]
firm_data[, cvr := as.character(cvr)]
firm_data[, industry_code := as.character(industry_code)]
firm_data[, hq_kommune_code := as.character(hq_kommune_code)]  
valid_winner_events[, winner_cvr_final := as.character(winner_cvr_final)]

# ---- Recreate separate objects ---- 
winner_data <- firm_data[firm_type == "winner",]
control_data <- firm_data[firm_type == "never winner",]

# Ensure we drop any accidental winning firms from the control set
n_before_drop <- control_data[, uniqueN(cvr)]
control_data <- control_data[!(cvr %chin% unique(winner_data[, unique(cvr)])), ]
n_after_drop <- control_data[, uniqueN(cvr)]
n_dropped <- n_before_drop - n_after_drop
message(paste0(n_dropped, " winning firms snuck into control data. They have been dropped."))
if (any(winner_data[, unique(cvr)] %in% control_data[, unique(cvr)])) {
  stop("some winning firms still remain in the control data!!!")
}
# ---- Find control firm match for each firm-event ----
# Control firm = closest FTE history in the 4 pre-award quarters, closest age at
# award, same sector (same location intended but not in the panel).
build_control_firm_data <- function(winning_firm_cvr,
                                    award_year,
                                    award_quarter,
                                    lookback = 4,
                                    freq = "quarterly_spliced",
                                    control_data,
                                    winner_data,
                                    event_data = valid_winner_events,
                                    staggered_attributes = TRUE) {
  
  winning_qidx <- award_year * 4 + award_quarter
  
  if (!(winning_firm_cvr %in% unique(winner_data$cvr))) {
    print(paste0("Winning firm CVR ", winning_firm_cvr, " not found in firm_data."))
    return(NULL)
  }
  
  # Ensure winning CVR doesn't appear in the control_data
  if (winning_firm_cvr %in% control_data[, unique(cvr)]) {
    stop(paste0("problem: winning firm ", winning_firm_cvr, " appears in the control data."))
  }
  
  # Treated firm's own pre-window quarters. Needed by BOTH branches and by the
  # merge / n_pre after the if/else, so compute it up front. (It previously lived
  # only inside the staggered branch, so the FTE-only `else` path -- and the merge
  # -- hit "object 'winning_info' not found" and every event errored to NULL.)
  winning_info <- winner_data[
    frequency == freq &
      cvr == winning_firm_cvr &
      qidx %between% c(winning_qidx - lookback, winning_qidx - 1),
    .(cvr = cvr, frequency = frequency, year = year, quarter = quarter, qidx = qidx,
      firm_sector = industry_code, hq_kommune_code = hq_kommune_code, employees = employees, fte = fte)
  ]
  
  # If staggered attributes is true, run through stricter -> looser control sets.
  if (staggered_attributes) {
    eligible_obs <- control_data[
      frequency == freq &
        qidx %between% c(winning_qidx - lookback, winning_qidx - 1) &
        industry_code %chin% unique(winning_info$firm_sector) &
        hq_kommune_code %chin% unique(winning_info$hq_kommune_code),
      .(cvr = cvr, year = year, quarter = quarter, qidx = qidx,
        firm_sector = industry_code, employees = employees, fte = fte)
    ]
    eligible_obs$control_protocol <- "sector, kommune, pre-award FTE"
    
    # Check a control was found with the above
    if (nrow(eligible_obs) == 0) {
      print(paste0("No eligible control firms found for winning firm CVR ", 
                   winning_firm_cvr, " in sector ", unique(winning_info$firm_sector), 
                   " and kommune ", unique(winning_info$hq_kommune_code), ". \n Defaulting to just sector."))
      eligible_obs <- firm_data[
        cvr != winning_firm_cvr &
          frequency == freq &
          qidx %between% c(winning_qidx - lookback, winning_qidx - 1) &
          industry_code %chin% unique(winning_info$firm_sector),
        .(cvr = cvr, year = year, quarter = quarter, qidx = qidx,
          firm_sector = industry_code, employees = employees, fte = fte)
      ]
      
      eligible_obs$control_protocol <- "sector, pre-award FTE"
    }
    
    # Check again that eligible_obs is not empty. If it is, do not match on sector
    if (nrow(eligible_obs) == 0) {
      print(paste0("No eligible control firms found for winning firm CVR ", 
                   winning_firm_cvr, " in sector ", unique(winning_info$firm_sector), 
                   ". \n Defaulting to all firms."))
      eligible_obs <- firm_data[
        cvr != winning_firm_cvr &
          frequency == freq &
          qidx %between% c(winning_qidx - lookback, winning_qidx - 1),
        .(cvr = cvr, year = year, quarter = quarter, qidx = qidx,
          firm_sector = industry_code, employees = employees, fte = fte)
      ]
      eligible_obs$control_protocol <- "pre-award FTE"
      
    }
  } else { # Otherwise, just match on FTE
    
    # Check again that eligible_obs is not empty. If it is, do not match on sector
    print(paste0("No eligible control firms found for winning firm CVR ", 
                 winning_firm_cvr, " in sector ", unique(winning_info$firm_sector), 
                 ". \n Defaulting to all firms."))
    eligible_obs <- firm_data[
      cvr != winning_firm_cvr &
        frequency == freq &
        qidx %between% c(winning_qidx - lookback, winning_qidx - 1),
      .(cvr = cvr, year = year, quarter = quarter, qidx = qidx,
        firm_sector = industry_code, employees = employees, fte = fte)
    ]
    eligible_obs$control_protocol <- "pre-award FTE"
  }
  
  
  # Create group counter
  eligible_obs[, firm_counter_control := .GRP, by = cvr]
  
  # Bind on winning_info to calculate differences
  eligible_obs <- merge(eligible_obs, winning_info, by = "qidx",
                        suffixes = c("_control", "_treatment"), allow.cartesian = TRUE)
  setorder(eligible_obs, firm_counter_control, qidx)
  
  # Squared FTE gap at each shared pre-period quarter.
  eligible_obs[, fte_diff_sq := (fte_control - fte_treatment)^2]
  
  # Require a computable gap for every pre-period observation. n_pre is the treated
  # firm's number of pre-window quarters with a valid FTE.
  n_pre <- winning_info[!is.na(fte), uniqueN(qidx)]
  
  # Case A: the treated firm itself has no usable pre-period FTE -> cannot match.
  if (n_pre == 0) {
    print(paste0("Discarding winning firm CVR ", winning_firm_cvr,
                 ": treated firm has no valid pre-period FTE observations."))
    return(NULL)
  }
  
  # Eligibility: keep only controls that, in EVERY one of the n_pre pre-period
  # quarters, have a computable gap and positive FTE. 
  eligible_obs[, n_gap := sum(!is.na(fte_diff_sq) & fte_control > 0),
               by = .(firm_counter_control, cvr_control)]
  eligible_obs <- eligible_obs[n_gap == n_pre, ]
  
  # controls exist but none have strictly positive FTE with a computable gap
  # across the whole pre-period -> discard (distinct message).
  if (nrow(eligible_obs) == 0) {
    print(paste0("Discarding winning firm CVR ", winning_firm_cvr,
                 ": no control has strictly positive FTE with a computable gap across all ",
                 n_pre, " pre-period observations."))
    return(NULL)
  }
  
  # Closest control by mean squared gap over the (now complete) pre-period.
  eligible_obs[, mean_fte_diff_sq := mean(fte_diff_sq, na.rm = TRUE),
               by = .(firm_counter_control, cvr_control)]
  eligible_obs[, control_qscore := sqrt(mean(fte_diff_sq, na.rm = TRUE)) / mean(fte_treatment, na.rm = TRUE),
               by = .(firm_counter_control, cvr_control)]
  #control_firm_data <- eligible_obs[mean_fte_diff_sq == min(mean_fte_diff_sq, na.rm = TRUE), ]
  matched_firm_cvr <- eligible_obs[control_qscore == min(control_qscore, na.rm = TRUE), ]
  
  # Create event_study data
  control_firm <- matched_firm_cvr[, .(qidx, cvr = cvr_control, 
                                       year = year_control, quarter = quarter_control, 
                                       employees = employees_control, 
                                       fte = fte_control,
                                       control_protocol, 
                                       control_quality = mean_fte_diff_sq,
                                       control_qscore)]
  treated_firm <- matched_firm_cvr[, .(qidx, cvr = cvr_treatment, 
                                       year = year_treatment, quarter = quarter_treatment, 
                                       employees = employees_treatment, 
                                       fte = fte_treatment,
                                       control_protocol, 
                                       control_quality = mean_fte_diff_sq,
                                       control_qscore)]
  control_event_data <- rbindlist(list(control_firm, treated_firm))
  control_event_data[, treatment := fifelse(cvr == winning_firm_cvr, "treated", "control")]
  control_event_data$event_year <- award_year
  control_event_data$event_quarter <- award_quarter
  control_event_data$event_qidx <- winning_qidx
  control_event_data$flag_found_control <- (nrow(matched_firm_cvr) > 0)
  control_event_data$n_eligible_controls <- length(unique(eligible_obs$cvr_control))
  control_event_data$n_controls <- length(control_firm[, uniqueN(cvr)])
  
  # Returned data
  list(control_firm_decision_data = matched_firm_cvr,
       all_eligible_controls = unique(eligible_obs$cvr_control),
       estudy_data = control_event_data)
}


# ---- Build the full treatment/control set ----

# Arguments
h <- 8
use_staggered <- TRUE
staggered_label <- ifelse(use_staggered, "staggered", "fteonly")
n_events <- nrow(valid_winner_events)

# Optional subset for a quick end-to-end test: set CONTROL_TEST_N to run only the first N events.
# Unset / 0 -> the full run. Test runs get a distinct `_testN` filename so they never collide with the
# real output.
.test_n   <- suppressWarnings(as.integer(Sys.getenv("CONTROL_TEST_N", "")))
test_mode <- !is.na(.test_n) && .test_n > 0L
event_idx <- if (test_mode) seq_len(min(.test_n, n_events)) else seq_len(n_events)
save_name <- paste0("never_winner_h", h, "_type", staggered_label,
                    if (test_mode) paste0("_test", length(event_idx)) else "", ".rds")

# Non-destructive path: file.path(dir, name), or name_2/_3/... if it already exists, so no save on
# disk is ever overwritten.
free_path <- function(dir, name) {
  p <- file.path(dir, name)
  if (!file.exists(p)) return(p)
  stem <- sub("\\.rds$", "", name); i <- 2L
  repeat {
    cand <- file.path(dir, sprintf("%s_%d.rds", stem, i))
    if (!file.exists(cand)) return(cand)
    i <- i + 1L
  }
}

# Parallel over events
n_cores <- max(1L, detectCores() - 3L)
setDTthreads(1L) 
message(sprintf("Constructing controls for %d of %d events on %d cores%s...",
                length(event_idx), n_events, n_cores,
                if (test_mode) " [CONTROL_TEST_N subset]" else ""))
control_event_list <- mclapply(event_idx, function(i) {
  e <- valid_winner_events[i]
  tryCatch(
    build_control_firm_data(
      winning_firm_cvr = e$winner_cvr_final,
      award_year = e$event_year, award_quarter = e$event_quarter,
      lookback = h, freq = "quarterly_spliced",
      winner_data = winner_data, 
      control_data = control_data, 
      event_data = valid_winner_events,
      staggered_attributes = use_staggered),
    error = function(err) NULL)
}, mc.cores = n_cores, mc.preschedule = TRUE)
setDTthreads(0L)  # restore data.table's default threading for the rest of the script

# Checkpoint the expensive mclapply output BEFORE the (cheap) assembly/filter/save, so a bug in the tail
# never forces re-running the parallel control build -- re-run the tail against this cached list instead:
#   cl <- readRDS(file.path(emp_dir, "never_winner_h8_typestaggered_rawlist.rds"))
raw_list_path <- free_path(emp_dir, sub("\\.rds$", "_rawlist.rds", save_name))
saveRDS(control_event_list, raw_list_path)
message(sprintf("Checkpointed raw control_event_list (%d elements) -> %s",
                length(control_event_list), raw_list_path))

# Bind, tagging each stack; skip events that produced no estudy_data.
control_event_data <- imap(control_event_list, function(x, i) {
  if (is.null(x) || is.null(x$estudy_data) || nrow(x$estudy_data) == 0) return(NULL)
  out <- x$estudy_data
  out[, stack_id := i]
  out
}) %>%
  purrr::compact() %>%
  rbindlist(fill = TRUE, use.names = TRUE)

# No frequency filter needed: build_control_firm_data was called with freq = "quarterly_spliced", so
# estudy_data is already entirely quarterly-spliced (and it carries no `frequency` column to filter on).

# Non-destructive save: never overwrite an existing file on disk (see free_path above).
out_path <- free_path(emp_dir, save_name)
if (basename(out_path) != save_name)
  message(sprintf("'%s' already exists -- saving to '%s' instead (no overwrite).",
                  save_name, basename(out_path)))
saveRDS(control_event_data, out_path)
message(sprintf("Saved control_event_data: %d rows, %d stacks -> %s",
                nrow(control_event_data), uniqueN(control_event_data$stack_id),
                out_path))
