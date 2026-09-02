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

# ---- Load / assemble the full all-firms employment panel (cached) ----
# firm_data = winners (cvr_employment_history_virk) + never-winners (cvr_employment_history_control): the
# full employment history for EVERY firm in the universe. Re-reading + re-binding these (~8 GB) is slow, so
# persist the fully typed panel ONCE and reuse it -- both for re-runs of this script and, more importantly,
# for any ad-hoc analysis: just `readRDS(file.path(dirs$employment, "firm_employment_panel_all.rds"))`
# elsewhere; you never need to run the matching to get at the employment data. The save happens before the
# (slow, sometimes failing) matching, so the panel persists regardless of how a matching run ends.
# Refresh it after the source files change with FIRM_PANEL_REBUILD=1.
firm_panel_path <- file.path(emp_dir, "firm_employment_panel_all.rds")
if (file.exists(firm_panel_path) && !nzchar(Sys.getenv("FIRM_PANEL_REBUILD"))) {
  message("Loading cached all-firms employment panel: ", firm_panel_path)
  firm_data <- data.table::as.data.table(readRDS(firm_panel_path))
} else {
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

  ## Types baked into the cached panel so it loads analysis-ready.
  firm_data[, qidx := year * 4 + quarter]
  firm_data[, cvr := as.character(cvr)]
  firm_data[, industry_code := as.character(industry_code)]
  firm_data[, hq_kommune_code := as.character(hq_kommune_code)]

  saveRDS(firm_data, firm_panel_path)
  message("Saved all-firms employment panel (", nrow(firm_data), " rows) -> ", firm_panel_path)
}

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

# ---- Key type on the event frame (firm_data types are baked into the cached panel above) ----
valid_winner_events[, winner_cvr_final := as.character(winner_cvr_final)]

# ---- Recreate separate objects ----
winner_data <- firm_data[firm_type == "winner",]
control_data <- firm_data[firm_type == "never winner",]

# Key firm_data on cvr so each event can pull the treated + matched-control FULL series (pre AND post)
# with a fast binary join (build_control_firm_data does `firm_data[.(cvrs)]`). Done once, before the
# parallel loop, so every worker inherits the keyed table.
setkey(firm_data, cvr)

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
  } else {
    # FTE-only: no sector/kommune screen. Every firm (except the winner) with pre-award-window
    # observations is eligible; the closest control is then chosen purely on the pre-award FTE gap
    # (the shared ranking below). This is the intended path when use_staggered = FALSE -- not a
    # fallback -- so there is no per-event warning here; the mode is announced once before the run.
    eligible_obs <- control_data[
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
  eligible_obs[, n_gap := sum(!is.na(fte_treatment) & !is.na(fte_control) & fte_control > 0 & fte_treatment),
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
  
  
  # Squared FTE gap at each shared pre-period quarter.
  eligible_obs[, fte_diff_sq := (fte_control - fte_treatment)^2]
  
  # Compute slope = cov(qidx, fte) / var(qidx); for a within-group vectorized version:
  eligible_obs[, trend_control := {
    qc <- qidx - mean(qidx)
    sum(qc * fte_control) / sum(qc^2)
  }, by = .(firm_counter_control, cvr_control)]
  eligible_obs[, trend_treated := {
    qt <- qidx - mean(qidx)
    sum(qt * fte_treatment) / sum(qt^2)
  }, by = .(firm_counter_control, cvr_control)]
  
  # Closest control by mean squared gap over the (now complete) pre-period.
  eligible_obs[, mean_fte_diff_sq := mean(fte_diff_sq, na.rm = TRUE),
               by = .(firm_counter_control, cvr_control)]
  eligible_obs[, level_qscore := sqrt(mean(fte_diff_sq, na.rm = TRUE)) / mean(fte_treatment, na.rm = TRUE),
               by = .(firm_counter_control, cvr_control)]
  eligible_obs[, trend_qscore := abs(trend_control - trend_treated)]

  # Standardise qscores
  eligible_obs[qidx == min(qidx), level_qscore_sd := scale(level_qscore)]
  eligible_obs[qidx == min(qidx), trend_qscore_sd := scale(trend_qscore)]
  eligible_obs[, level_qscore_sd := mean(level_qscore_sd, na.rm = TRUE), by = .(firm_counter_control, cvr_control)]
  eligible_obs[, trend_qscore_sd := mean(trend_qscore_sd, na.rm = TRUE), by = .(firm_counter_control, cvr_control)]
  
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

  
  # Compact MATCH RECORD (one row per matched firm): the treated firm + its matched never-winner
  # control(s), the event timing, and the match quality. The full pre+post series is NOT pulled here --
  # that happens once in the tail, straight from firm_data (a single keyed join), so the expensive
  # matching can be checkpointed and reused without re-running. All matched controls share the same
  # (minimal) gap/qscore/protocol.
  control_cvrs <- unique(matched_firm_cvr$cvr_control)
  control_event_data <- data.table(
    cvr                 = c(winning_firm_cvr, control_cvrs),
    treatment           = c("treated", rep("control", length(control_cvrs))),
    event_qidx          = winning_qidx,
    event_year          = award_year,
    event_quarter       = award_quarter,
    control_protocol    = matched_firm_cvr$control_protocol[1],
    control_quality     = matched_firm_cvr$mean_fte_diff_sq[1],
    control_qscore      = matched_firm_cvr$control_qscore[1],
    flag_found_control  = TRUE,
    n_eligible_controls = length(unique(eligible_obs$cvr_control)),
    n_controls          = length(control_cvrs))
  
  # Returned data
  # Return ONLY the compact match record. (It previously also returned the full matched-rows table and the
  # entire eligible-control CVR vector. Under FTE-only matching that vector is ~all firms per event and,
  # accumulated across the thousands of events each mclapply fork handles, exhausted memory -> the workers
  # were OS-killed with "fatal error in wrapper code". Everything the panel rebuild needs -- the treated +
  # matched-control cvrs, event timing, match quality, and n_eligible_controls -- is already in estudy_data.)
  list(estudy_data = control_event_data)
}

# The full pre+post event-study panel is assembled once in the tail (after matching), via a single keyed
# join of firm_data onto the compact match records -- see the "Build the event-study panel" block below.
# Keeping the series pull out of the per-event matching lets the expensive matching be checkpointed and
# reused (reloaded rawlist) without ever re-running it to (re)build the panel.


# ---- Build the full treatment/control set ----

# Arguments
h <- 8
use_staggered <- FALSE
staggered_label <- ifelse(use_staggered, "staggered", "fteonly")
n_events <- nrow(valid_winner_events)

# DRY RUN: run only the first N events end-to-end to check the pipeline works. Test runs write a distinct
# `_testN` output (and their own rawlist), so they NEVER touch the full-run files. **Set to NULL for the
# full run.** The CONTROL_TEST_N env var, if set, overrides this in-code default.
test_n_events <- 500L

.env_test_n <- suppressWarnings(as.integer(Sys.getenv("CONTROL_TEST_N", "")))
.test_n   <- if (!is.na(.env_test_n)) .env_test_n else test_n_events   # env overrides the in-code default
test_mode <- !is.null(.test_n) && !is.na(.test_n) && .test_n > 0L
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

# Match (the expensive step) OR reuse a prior run's checkpoint. The rawlist stores the compact match
# records (treated + matched-control cvrs + event timing per event); the event-study panel is rebuilt from
# firm_data afterwards, so fixing/updating the panel never needs a re-match. Force a fresh match with
# CONTROL_FORCE_REMATCH=1. Test runs (CONTROL_TEST_N) always match their own subset.
raw_list_path <- file.path(emp_dir, sub("\\.rds$", "_rawlist.rds", save_name))
if (file.exists(raw_list_path) && !nzchar(Sys.getenv("CONTROL_FORCE_REMATCH")) && !test_mode) {
  message("Reusing matched controls from checkpoint (skipping the match): ", raw_list_path)
  control_event_list <- readRDS(raw_list_path)
} else {
  n_cores <- max(1L, detectCores() - 3L)
  setDTthreads(1L)
  message(sprintf("Constructing controls for %d of %d events on %d cores [match: %s]%s...",
                  length(event_idx), n_events, n_cores,
                  if (use_staggered) "sector+kommune, then pre-award FTE" else "pre-award FTE only (no sector/kommune screen)",
                  if (test_mode) " [CONTROL_TEST_N subset]" else ""))
  control_event_list <- mclapply(event_idx+10000, function(i) {
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
  setDTthreads(0L)
  saveRDS(control_event_list, raw_list_path)   # overwrite: the checkpoint is a resumable match cache
  message(sprintf("Checkpointed match records (%d events) -> %s", length(control_event_list), raw_list_path))
}

# Surface matching failures early and clearly. A killed mclapply worker leaves an atomic try-error in place
# of a match record (the per-event tryCatch cannot catch an OS-killed fork), which otherwise only shows up
# cryptically downstream ("$ operator is invalid for atomic vectors").
n_failed <- sum(!vapply(control_event_list, is.list, logical(1)))
if (n_failed == length(control_event_list))
  stop(sprintf("All %d events failed in matching (mclapply workers crashed -- likely out of memory). Delete the rawlist checkpoint and re-run.", n_failed))
if (n_failed > 0)
  message(sprintf("WARNING: %d of %d events failed in matching and are skipped.", n_failed, length(control_event_list)))

# Build the event-study panel efficiently. First collect the compact match records into ONE table (a row
# per event x firm: treated + matched control(s), event timing, match quality). Then a SINGLE keyed join
# to firm_data brings the full pre+post series for every firm x event (a control reused across events is
# joined to each). This replaces per-event pulls + a 40k-element accumulation, which was slow and blew the
# 24 GB vector limit.
setDTthreads(0L)
match_tab <- rbindlist(purrr::compact(imap(control_event_list, function(x, i) {
  if (!is.list(x) || is.null(x$estudy_data) || !nrow(x$estudy_data)) return(NULL)  # skip failed events
  ed <- x$estudy_data
  unique(ed[, .(cvr, treatment, event_qidx, event_year, event_quarter,
                control_protocol, control_quality, control_qscore,
                n_eligible_controls, n_controls)])[, stack_id := i]
})), fill = TRUE)
message(sprintf("Match records: %d firm-by-event rows across %d events",
                nrow(match_tab), uniqueN(match_tab$stack_id)))
# firm_data holds several frequencies per period; keep the analysis frequency, then join the full series.
control_event_data <- firm_data[frequency == "quarterly_spliced"][
  match_tab, on = "cvr", allow.cartesian = TRUE, nomatch = NULL]
control_event_data[, event_time := qidx - event_qidx]
control_event_data[, flag_found_control := TRUE]

# Non-destructive save: never overwrite an existing file on disk (see free_path above).
out_path <- free_path(emp_dir, save_name)
if (basename(out_path) != save_name)
  message(sprintf("'%s' already exists -- saving to '%s' instead (no overwrite).",
                  save_name, basename(out_path)))
saveRDS(control_event_data, out_path)
message(sprintf("Saved control_event_data: %d rows, %d stacks -> %s",
                nrow(control_event_data), uniqueN(control_event_data$stack_id),
                out_path))
