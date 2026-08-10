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
## Firm employment panel
firm_data <- fread(file.path(clean_data_dir, "cvr_employment_history_virk.csv"))
setDT(firm_data)
firm_data[, firm_age := as.integer(floor(as.numeric(period_end - lifecycle_start) / 365.25))]

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

# ---- Find control firm match for each firm-event ----
# Control firm = closest FTE history in the 4 pre-award quarters, closest age at
# award, same sector (same location intended but not in the panel).
build_control_firm_data <- function(winning_firm_cvr,
                                    award_year,
                                    award_quarter,
                                    lookback = 4,
                                    freq = "quarterly_spliced",
                                    firm_data = firm_data,
                                    event_data = valid_winner_events) {

  winning_qidx <- award_year * 4 + award_quarter

  if (!(winning_firm_cvr %in% unique(firm_data$cvr))) {
    print(paste0("Winning firm CVR ", winning_firm_cvr, " not found in firm_data."))
    return(NULL)
  }

  winning_info <- firm_data[
    frequency == freq &
      cvr == winning_firm_cvr &
      qidx %between% c(winning_qidx - lookback, winning_qidx - 1),
    .(cvr = cvr, frequency = frequency, year = year, quarter = quarter, qidx = qidx,
      firm_sector = industry_code, hq_kommune_code = hq_kommune_code, 
      firm_age = firm_age, employees = employees, fte = fte)
  ]

  eligible_obs <- firm_data[
    cvr != winning_firm_cvr &
      frequency == freq &
      qidx %between% c(winning_qidx - lookback, winning_qidx - 1) &
      industry_code %chin% unique(winning_info$firm_sector) &
      hq_kommune_code %chin% unique(winning_info$hq_kommune_code),
    .(cvr = cvr, year = year, quarter = quarter, qidx = qidx,
      firm_sector = industry_code, firm_age = firm_age, employees = employees, fte = fte)
  ]
  
  # Eligible controls: firms that win at least one tender (winners are preferred to
  # never-winners as controls), but that do NOT win one inside this event's window
  # [award_qidx +/- lookback]. NB: exclude a firm if it wins *anywhere* in the window
  # -- not merely "has some award outside it" (a firm winning both inside and outside
  # the window must still be dropped).
  all_winner_cvrs   <- event_data[, unique(winner_cvr_final)]
  winners_in_window <- event_data[award_qidx %between% c(winning_qidx - lookback, 
                                                         winning_qidx + lookback),
                                  unique(winner_cvr_final)]
  eligible_obs <- eligible_obs[cvr %chin% all_winner_cvrs & !(cvr %chin% winners_in_window), ]
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
        firm_sector = industry_code, firm_age = firm_age, employees = employees, fte = fte)
    ]
    
    # Find no winners in window
    all_winner_cvrs   <- event_data[, unique(winner_cvr_final)]
    winners_in_window <- event_data[award_qidx %between% c(winning_qidx - lookback, 
                                                           winning_qidx + lookback),
                                    unique(winner_cvr_final)]
    eligible_obs <- eligible_obs[cvr %chin% all_winner_cvrs & !(cvr %chin% winners_in_window), ]
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
        firm_sector = industry_code, firm_age = firm_age, employees = employees, fte = fte)
    ]
    
    # Find no winners in window
    all_winner_cvrs   <- event_data[, unique(winner_cvr_final)]
    winners_in_window <- event_data[award_qidx %between% c(winning_qidx - lookback, 
                                                           winning_qidx + lookback),
                                    unique(winner_cvr_final)]
    eligible_obs <- eligible_obs[cvr %chin% all_winner_cvrs & !(cvr %chin% winners_in_window), ]
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

  # Keep only controls that cover ALL n_pre pre-period observations (a shared
  # quarter with a non-missing gap for each).
  eligible_obs[, n_gap := sum(!is.na(fte_diff_sq)), by = .(firm_counter_control, cvr_control)]
  eligible_obs <- eligible_obs[n_gap == n_pre]

  # Case B: controls exist but none span the full pre-period -> discard (distinct message).
  if (nrow(eligible_obs) == 0) {
    print(paste0("Discarding winning firm CVR ", winning_firm_cvr,
                 ": no control has a computable FTE gap across all ", n_pre,
                 " pre-period observations."))
    return(NULL)
  }

  # Closest control by mean squared gap over the (now complete) pre-period.
  eligible_obs[, mean_fte_diff_sq := mean(fte_diff_sq, na.rm = TRUE),
               by = .(firm_counter_control, cvr_control)]
  control_firm_data <- eligible_obs[mean_fte_diff_sq == min(mean_fte_diff_sq, na.rm = TRUE), ]

  # Create event_study data
  control_event_data <- firm_data[
    cvr %in% unique(control_firm_data$cvr_control) | cvr == winning_firm_cvr,
    .(cvr, frequency, year, quarter, qidx, firm_age, employees, fte)
  ]
  control_event_data[, treatment := fifelse(cvr == winning_firm_cvr, "treated", "control")]
  control_event_data$event_year <- award_year
  control_event_data$event_quarter <- award_quarter
  control_event_data$event_qidx <- winning_qidx
  control_event_data$control_quality <- control_firm_data$mean_fte_diff_sq[1]
  control_event_data$flag_found_control <- (nrow(control_firm_data) > 0)
  control_event_data$control_protocol <- unique(control_firm_data$control_protocol)

  list(control_firm_decision_data = control_firm_data,
       all_eligible_controls = unique(eligible_obs$cvr_control),
       estudy_data = control_event_data)
}

# ---- Build the full treatment/control set ----
if (construct_control_data == 1) {

  n_events <- nrow(valid_winner_events)
  # Parallel over events (each event is independent). Fork-based mclapply lets
  # workers share the large read-only firm_data / valid_winner_events via
  # copy-on-write (no per-worker export cost). Fork works on macOS/Linux, not
  # Windows -- on Windows set n_cores <- 1L (or switch to a PSOCK cluster).
  n_cores <- max(1L, detectCores() - 1L)
  setDTthreads(1L)  # one data.table thread per worker -> avoid CPU oversubscription
  message(sprintf("Constructing controls for %d events on %d cores...", n_events, n_cores))
  control_event_list <- mclapply(seq_len(n_events), function(i) {
    e <- valid_winner_events[i]
    # tryCatch so a single bad event doesn't kill a long background run.
    tryCatch(
      build_control_firm_data(
        winning_firm_cvr = e$winner_cvr_final,
        award_year = e$event_year, award_quarter = e$event_quarter,
        lookback = 8, freq = "quarterly_spliced",
        firm_data = firm_data, event_data = valid_winner_events),
      error = function(err) NULL)
  }, mc.cores = n_cores, mc.preschedule = TRUE)
  setDTthreads(0L)  # restore data.table's default threading for the rest of the script

  # Bind, tagging each stack; skip events that produced no estudy_data.
  control_event_data <- imap(control_event_list, function(x, i) {
    if (is.null(x) || is.null(x$estudy_data) || nrow(x$estudy_data) == 0) return(NULL)
    out <- x$estudy_data
    out[, stack_id := i]
    out
  }) %>%
    purrr::compact() %>%
    rbindlist(fill = TRUE, use.names = TRUE)

  # Guard: an all-NULL batch yields a 0-column table; only filter if we have rows.
  if (nrow(control_event_data) > 0) {
    control_event_data <- control_event_data[frequency == "quarterly_spliced", ]
  }
  saveRDS(control_event_data, file.path(clean_data_dir, "control_event_list.rds"))
  message(sprintf("Saved control_event_data: %d rows, %d stacks -> %s",
                  nrow(control_event_data), uniqueN(control_event_data$stack_id),
                  file.path(clean_data_dir, "control_event_list.rds")))
} else {
  control_event_data <- readRDS(file.path(clean_data_dir, "control_event_list.rds"))
  message(sprintf("Loaded control_event_data: %d rows", nrow(control_event_data)))
}
