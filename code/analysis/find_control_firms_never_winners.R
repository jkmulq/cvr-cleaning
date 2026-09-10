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

# ---- Industry hierarchy (up front) ----
# DB07 branchekode is hierarchical: the leading digits give coarser levels. Zero-pad the (numeric) 6-digit
# code, then truncate to class (4), group (3), division (2). Derived here -- whether firm_data came from
# cache or a fresh build -- so the matching can stagger from fine (6-digit) to coarse (2-digit) on industry.
firm_data[, industry_code6 := {
  ii <- suppressWarnings(as.integer(industry_code)); fifelse(is.na(ii), NA_character_, sprintf("%06d", ii))
}]
firm_data[, `:=`(industry_class    = substr(industry_code6, 1L, 4L),
                 industry_group    = substr(industry_code6, 1L, 3L),
                 industry_division = substr(industry_code6, 1L, 2L))]

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
# Keep ONLY the columns matching needs. The panel has ~69 columns, but build_control_firm_data uses just
# these 9 -- carrying all of them makes each pool ~a full copy of the 8 GB panel, which exhausted the 24 GB
# vector limit (every worker's allocation then failed and was caught as NULL -> "all events failed"). The
# full series is pulled later from the cached panel by build_event_study_panel(), not from these pools.
match_cols <- c("cvr", "frequency", "qidx", "fte",
                "industry_code6", "industry_class", "industry_group", "industry_division", "hq_kommune_code")
winner_data  <- firm_data[firm_type == "winner",       ..match_cols]
control_data <- firm_data[firm_type == "never winner", ..match_cols]

# (firm_data is no longer keyed or joined here -- matching draws from the winner/never-winner pools and
# phase 2 stores only the compact match table. firm_data is freed just below, once the pools are set up.)

# Ensure we drop any accidental winning firms from the control set
n_before_drop <- control_data[, uniqueN(cvr)]
control_data <- control_data[!(cvr %chin% unique(winner_data[, unique(cvr)])), ]
n_after_drop <- control_data[, uniqueN(cvr)]
n_dropped <- n_before_drop - n_after_drop
message(paste0(n_dropped, " winning firms snuck into control data. They have been dropped."))
if (any(winner_data[, unique(cvr)] %in% control_data[, unique(cvr)])) {
  stop("some winning firms still remain in the control data!!!")
}

# firm_data is not used again -- matching draws from the pools and phase 2 stores only the match table -- so
# free it now (~8 GB) before the parallel matching, especially with more cores. The on-disk cache is intact.
rm(firm_data); gc()

# ---- Find eligible control firms for each firm-event (two arms) ----
# Per event, find ALL eligible controls in TWO arms and stack them, flagged by control_type:
#   never_winner -- firms that never win a tender (from control_data)
#   winner       -- firms that win a tender somewhere but NOT within [event +/- lookback] (from winner_data)
# Each arm runs the same staggered cascade (sector+kommune -> sector -> all firms in the pool)
# independently, so control_protocol (the tier used) can differ between arms. Eligibility requires
# positive FTE and a computable pre-period FTE gap in every pre-window quarter; selection is deferred.
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
  
  # Treated firm's contemporaneous industry / kommune (from its pre-window) -- used only to define the
  # matching keys. No FTE is read here; all employment/FTE work is deferred to phase 2.
  winning_info <- winner_data[
    frequency == freq &
      cvr == winning_firm_cvr &
      qidx %between% c(winning_qidx - lookback, winning_qidx - 1),
    .(industry_code6 = industry_code6, industry_class = industry_class,
      industry_group = industry_group, industry_division = industry_division,
      hq_kommune_code = hq_kommune_code)
  ]

  # Balanced-window eligibility (NO FTE): a firm qualifies only if it is observed for at least `lookback`
  # distinct quarters on EACH side of the event -- in [wq-lookback, wq-1] AND [wq+1, wq+lookback]. This
  # bounds the eligible list and guarantees a usable +/-h panel; the actual FTE matching/selection happens
  # later via joins. covered() returns which of `cand` (or the whole pool) meet it.
  covered <- function(pool, cand) {
    cov <- pool[cvr %chin% cand & frequency == freq &
                  qidx %between% c(winning_qidx - lookback, winning_qidx + lookback),
                .(npre  = uniqueN(qidx[qidx <= winning_qidx - 1L]),
                  npost = uniqueN(qidx[qidx >= winning_qidx + 1L])), by = cvr]
    cov[npre >= lookback & npost >= lookback, cvr]
  }

  # The treated firm itself must have the balanced +/-h window, else the event cannot be studied at this h.
  if (!(winning_firm_cvr %chin% covered(winner_data, winning_firm_cvr))) {
    print(paste0("Discarding winning firm CVR ", winning_firm_cvr,
                 ": treated firm lacks ", lookback, " quarters of data on each side of the event."))
    return(NULL)
  }

  # Find eligible controls from a pool. In staggered mode the cascade STAGGERS ON INDUSTRY from fine to
  # coarse -- DB07 6-digit -> class(4) -> group(3) -> division(2), each while holding kommune, then division
  # without kommune, then any industry -- recording the rung that fired in `protocol`. Each arm runs
  # independently, so a never-winner control may match at industry6+kommune while a winner control only
  # matches at division. `winner_exclude = TRUE` keeps only firms that win a tender SOMEWHERE but NOT within
  # [event +/- lookback]. Every control must also pass the balanced +/-h coverage (covered()); no FTE used.
  find_eligible_controls <- function(pool, winner_exclude) {
    komm <- unique(winning_info$hq_kommune_code)
    if (winner_exclude) {
      all_w  <- event_data[, unique(winner_cvr_final)]
      in_win <- event_data[award_qidx %between% c(winning_qidx - lookback, winning_qidx + lookback),
                           unique(winner_cvr_final)]
      keep_winner <- function(cvrs) cvrs[cvrs %chin% all_w & !(cvrs %chin% in_win)]
    } else {
      keep_winner <- function(cvrs) cvrs
    }
    # One rung: control CVRs whose pre-window industry (+/- kommune) matches the treated, restricted to the
    # winner arm if requested, then filtered to those with the balanced +/-h coverage. ind_col = NULL means
    # no industry screen (the final any-industry rung).
    tier <- function(ind_col, ind_vals, use_komm) {
      cand <- pool[cvr != winning_firm_cvr & frequency == freq &
                     qidx %between% c(winning_qidx - lookback, winning_qidx - 1) &
                     (if (is.null(ind_col)) TRUE else get(ind_col) %chin% ind_vals) &
                     (if (use_komm) hq_kommune_code %chin% komm else TRUE),
                   unique(cvr)]
      cand <- keep_winner(cand)
      if (!length(cand)) return(character(0))
      covered(pool, cand)
    }
    if (staggered_attributes) {
      rungs <- list(
        list(col = "industry_code6",    vals = unique(winning_info$industry_code6),    komm = TRUE,  prot = "industry6, kommune"),
        list(col = "industry_class",    vals = unique(winning_info$industry_class),    komm = TRUE,  prot = "industry4 (class), kommune"),
        list(col = "industry_group",    vals = unique(winning_info$industry_group),    komm = TRUE,  prot = "industry3 (group), kommune"),
        list(col = "industry_division", vals = unique(winning_info$industry_division), komm = TRUE,  prot = "industry2 (division), kommune"),
        list(col = "industry_division", vals = unique(winning_info$industry_division), komm = FALSE, prot = "industry2 (division)"),
        list(col = NULL,                vals = NULL,                                   komm = FALSE, prot = "any industry"))
      cvrs <- character(0); protocol <- NA_character_
      for (r in rungs) {
        cvrs <- tier(r$col, r$vals, r$komm)
        if (length(cvrs) > 0) { protocol <- r$prot; break }
      }
    } else {
      cvrs <- tier(NULL, NULL, FALSE)   # FTE-only mode: any industry, just the balanced-window coverage
      protocol <- "any industry"
    }
    list(cvrs = cvrs, protocol = protocol)
  }

  # Two arms: never-winners (control_data) and sometimes-winners (winner_data, excluding any that win within
  # [event +/- lookback]). The pools are disjoint, so no control is double-counted; each arm runs its own
  # cascade, so control_protocol can differ between arms.
  nw <- find_eligible_controls(control_data, winner_exclude = FALSE)
  wc <- find_eligible_controls(winner_data,  winner_exclude = TRUE)

  if (length(nw$cvrs) == 0 && length(wc$cvrs) == 0) {
    print(paste0("Discarding winning firm CVR ", winning_firm_cvr,
                 ": no eligible control in either arm (same industry + balanced +/-", lookback, "q window)."))
    return(NULL)
  }

  # One row per firm: the treated firm once, then ALL eligible controls, flagged by control_type
  # (never_winner / winner) and tagged with the matching tier each arm used (control_protocol).
  # Selection and the FTE-gap quantities are deferred to phase 2 (computed from the merged panel).
  control_event_data <- data.table(
    cvr                     = c(winning_firm_cvr, nw$cvrs, wc$cvrs),
    treatment               = c("treated", rep("control", length(nw$cvrs) + length(wc$cvrs))),
    control_type            = c("treated", rep("never_winner", length(nw$cvrs)),
                                rep("winner", length(wc$cvrs))),
    control_protocol        = c(NA_character_, rep(nw$protocol, length(nw$cvrs)),
                                rep(wc$protocol, length(wc$cvrs))),
    event_qidx              = winning_qidx,
    event_year              = award_year,
    event_quarter           = award_quarter,
    flag_found_control      = TRUE,
    n_never_winner_controls = length(nw$cvrs),
    n_winner_controls       = length(wc$cvrs))

  list(control_data = control_event_data)
}

# The full pre+post event-study panel is assembled once in the tail (after matching), via a single keyed
# join of firm_data onto the compact match records -- see the "Build the event-study panel" block below.
# Keeping the series pull out of the per-event matching lets the expensive matching be checkpointed and
# reused (reloaded rawlist) without ever re-running it to (re)build the panel.


# ---- Build the full treatment/control set ----

# Arguments
h <- 8
use_staggered <- TRUE
staggered_label <- ifelse(use_staggered, "staggered", "fteonly")
n_events <- nrow(valid_winner_events)

# DRY RUN: run only the first N events end-to-end to check the pipeline works. Test runs write a distinct
# `_testN` output (and their own rawlist), so they NEVER touch the full-run files. **Set to NULL for the
# full run.** The CONTROL_TEST_N env var, if set, overrides this in-code default.
test_n_events <- NULL   # full run (set to a positive integer for a dry run on the first N events)

.env_test_n <- suppressWarnings(as.integer(Sys.getenv("CONTROL_TEST_N", "")))
.test_n   <- if (!is.na(.env_test_n)) .env_test_n else test_n_events   # env overrides the in-code default
test_mode <- !is.null(.test_n) && !is.na(.test_n) && .test_n > 0L
event_idx <- if (test_mode) seq_len(min(.test_n, n_events)) else seq_len(n_events)
save_name <- paste0("matched_controls_h", h, "_type", staggered_label,
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
  n_cores <- max(1L, detectCores() - 1L)
  setDTthreads(1L)
  message(sprintf("Constructing controls for %d of %d events on %d cores [match: %s]%s...",
                  length(event_idx), n_events, n_cores,
                  if (use_staggered) "sector+kommune, then pre-award FTE" else "pre-award FTE only (no sector/kommune screen)",
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
  setDTthreads(0L)
  saveRDS(control_event_list, raw_list_path)   # overwrite: the checkpoint is a resumable match cache
  message(sprintf("Checkpointed match records (%d events) -> %s", length(control_event_list), raw_list_path))
}

# Surface matching failures early and clearly. A killed mclapply worker leaves an atomic try-error in place
# of a match record (the per-event tryCatch cannot catch an OS-killed fork), which otherwise only shows up
# cryptically downstream ("$ operator is invalid for atomic vectors").
# Distinguish the two failure modes: NULL = a guard fired or an R error was caught by the per-event tryCatch
# (NOT a crash); try-error = an OS-killed worker (a real crash, usually OOM). Reporting them separately
# avoids the misleading "out of memory" when the real cause is a caught error in every event.
n_null  <- sum(vapply(control_event_list, is.null, logical(1)))
n_crash <- sum(vapply(control_event_list, function(x) inherits(x, "try-error"), logical(1)))
n_ok    <- length(control_event_list) - n_null - n_crash
if (n_ok == 0)
  stop(sprintf(paste0("No event produced a match record (of %d: %d returned NULL, %d workers crashed). ",
                      "If NULL dominates, a guard fired or an R error was caught for every event -- run one ",
                      "event through build_control_firm_data() WITHOUT tryCatch to see it. If crashes dominate, ",
                      "it is likely OOM: lower n_cores and re-run (the rawlist checkpoint resumes)."),
               length(control_event_list), n_null, n_crash))
if (n_null + n_crash > 0)
  message(sprintf("WARNING: %d of %d events produced no record (%d NULL, %d crashed) and are skipped.",
                  n_null + n_crash, length(control_event_list), n_null, n_crash))

# Matching is done -- free the large objects it needed (winner_data + control_data together are ~a full copy
# of firm_data; the tender frames are large too). (intersect(...) so this is a no-op for anything already
# gone, e.g. on the resume path.)
rm(list = intersect(c("winner_data", "control_data", "data_ot", "data_kfst", "data_tender"), ls()))
gc()

# Build the event-study panel efficiently. First collect the compact match records into ONE table (a row
# per event x firm: treated + matched control(s), event timing, match quality). Then a SINGLE keyed join
# to firm_data brings the full pre+post series for every firm x event (a control reused across events is
# joined to each). This replaces per-event pulls + a 40k-element accumulation, which was slow and blew the
# 24 GB vector limit.
setDTthreads(0L)
match_tab <- rbindlist(purrr::compact(imap(control_event_list, function(x, i) {
  if (!is.list(x) || is.null(x$control_data) || !nrow(x$control_data)) return(NULL)  # skip failed events
  ed <- x$control_data
  unique(ed[, .(cvr, treatment, control_type, control_protocol, event_qidx, event_year, event_quarter,
                n_never_winner_controls, n_winner_controls)])[, stack_id := i]
})), fill = TRUE)
message(sprintf("Match records: %d firm-by-event rows across %d events",
                nrow(match_tab), uniqueN(match_tab$stack_id)))
rm(control_event_list); gc()   # the raw match list is now distilled into match_tab

# Save the COMPACT match table -- one row per (event, firm): the treated firm plus all its eligible
# never-winner and sometimes-winner controls, flagged by control_type and the matching tier
# (control_protocol). We deliberately DO NOT join the employment series here -- that cross product (every
# event x control x full series) is ~100M+ rows. Keep this small "list of controls"; build a WINDOWED
# event-study panel on demand at analysis time with build_event_study_panel()
# (code/analysis/build_event_study_panel.R), which joins this table to firm_employment_panel_all.rds.
out_path <- free_path(emp_dir, save_name)
if (basename(out_path) != save_name)
  message(sprintf("'%s' already exists -- saving to '%s' instead (no overwrite).",
                  save_name, basename(out_path)))
saveRDS(match_tab, out_path)
message(sprintf("Saved match table: %d rows (event x firm) | %d events | %d never-winner + %d winner controls -> %s",
                nrow(match_tab), uniqueN(match_tab$stack_id),
                match_tab[control_type == "never_winner", .N],
                match_tab[control_type == "winner", .N], out_path))
