#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 3 -- turn the matched panel into estimation-ready regression datasets.
#
# Reads ONLY stage 2's artefacts (02_matched_panel.parquet, 02_match_table.rds).
# Writes  03_reg_data_h{h}.parquet + 03_reg_meta.rds, where h is the stage-2 matching window.
#
# THE WINDOW IS NOT A CHOICE HERE. It is read from stage 2's report and is whatever the MATCHING used.
# An earlier version emitted several windows (8 and 4) from one match, which was misleading: the controls
# were selected on the +/-8 pre-period either way, so a "+/-4" dataset was an h=8 match trimmed to 4, not
# an h=4 match. Retention under the quality cutoff was identical (54.2% vs 54.2%, 51.9% vs 51.9%) precisely
# because the qscores were the same numbers. For a genuine h=4 design, rerun stage 2 with MATCH_H=4 --
# that changes eligibility and scoring, and admits events that cannot support 8 post-quarters.
#
# A STACK is one (event, scoring_protocol, arm) triple: the treated firm plus the controls selected for it
# under that protocol in that arm. Every operation below -- balancing, weighting, the both-arms-present
# test -- happens WITHIN a stack, so protocols and arms never contaminate one another and a single
# dataset can be sliced to any (protocol, arm) at estimation time.
#
# BALANCE. A firm is kept only with FTE > MATCH_MIN_FTE in every one of the 2h+1 event quarters:
# exactly 2h+1 rows AND 2h+1 distinct periods spanning -h..h (the pair of tests catches gaps AND
# duplicates, which a row count alone would not). industry_grp must also be present, because it enters
# the fixed effects and feols would otherwise drop those rows and silently unbalance the sample.
#
# THE FTE SCREEN IS THIS STAGE'S ALONE, AND ITS COST IS MEASURED. Stage 2 matches on the FTE path without
# requiring positivity, so every firm the screen removes is removed HERE -- which means it can be
# attributed. Balance is evaluated twice, once on ROW PRESENCE alone and once with FTE > MATCH_MIN_FTE
# also required; the difference between the two is the screen's bite, cleanly separated from ordinary
# holes in the employment panel. Reported to the console, stored in 03_reg_meta$fte_screen, and written
# per stack to 03_fte_screen_attrition.parquet: firms dropped (split treated/control), stacks that lost
# at least one firm, stacks lost outright and which side went, events affected, and the failure share by
# event time.
#
# READ THE EVENT-TIME PROFILE FIRST. A screen whose failure share is flat in event time is defining a
# population. One whose failure share RISES with event time is removing post-treatment outcomes, and the
# estimates are then conditioned on survival -- which for an undefined-at-zero outcome (job quality) is
# defensible but has to be stated and bounded, and for FTE itself is not. MATCH_MIN_FTE=-1 disables the
# screen (zeros retained, NAs still dropped) if the unconditioned sample is wanted.
#
# WEIGHTS. 1 / (# firms in the (stack, treatment) cell), so the treated side and the control side of
# each stack each sum to 1 and a stack that happened to tie 40 controls at rank 1 does not outvote a
# stack that matched one.
#
#   Rscript code/matching/3_build_reg_data.R
# Options (env):
#   MATCH_MIN_FTE     strict lower bound on FTE (default 0)
#   MATCH_IND_DIGITS  industry FE granularity: 2=division, 3=group, 4=class, 6=full (default 2)
#   MATCH_MIN_STACKS  skip a (window, protocol, arm) cell with fewer stacks (default 20)
#   MATCH_TEST_N, MATCH_OVERWRITE
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))
match_setup()

MIN_FTE    <- match_env_num("MATCH_MIN_FTE", 0)
IND_DIGITS <- match_env_int("MATCH_IND_DIGITS", 2L)
MIN_STACKS <- match_env_int("MATCH_MIN_STACKS", 20L)
P          <- match_paths()

match_rule_banner("1. input")
mp <- read_tab(P$matched_panel)
# The window comes from the match, never from the environment -- see the header.
rep2 <- read_obj(P$match_report)
h    <- rep2$h
cat(sprintf("STAGE 3 | window=+/-%d (from the stage-2 match) | min_fte=%s | industry FE=%d-digit | min_stacks=%d\n",
            h, MIN_FTE, IND_DIGITS, MIN_STACKS))
cat(sprintf("  matched panel: %d rows | %d events | %d firms\n", nrow(mp), uniqueN(mp$ev), uniqueN(mp$cvr)))

mp[, industry_grp := substr(industry_code6, 1L, IND_DIGITS)]
mp[, stack_id := .GRP, by = .(ev, scoring_protocol, arm)]
mp[, treated  := as.integer(treatment == "treated")]

meta <- list(run_at = Sys.time(), h = h, min_fte = MIN_FTE, ind_digits = IND_DIGITS,
             min_stacks = MIN_STACKS)

{
  match_rule_banner(sprintf("2. build the +/-%d panel", h))
  NP <- 2L * h + 1L

  # Everything EXCEPT the FTE screen. Holding this separately is what makes the screen's cost
  # attributable: without it, a firm lost to a zero-FTE quarter and a firm lost to a hole in the
  # employment panel are indistinguishable in the retention count.
  base <- mp[event_time %between% c(-h, h) &
               !is.na(industry_grp) & industry_grp != ""]

  # One row per (stack, firm), carrying both verdicts: balanced on row presence alone (bal_row) and
  # balanced once FTE > MIN_FTE is also demanded (bal_fte). drop_fte is exactly the set the screen costs
  # -- firms that would have been estimable and are not.
  fs <- base[, .(n_row  = .N,
                 n_per  = uniqueN(event_time),
                 tmin   = min(event_time),
                 tmax   = max(event_time),
                 n_ok   = sum(!is.na(fte) & fte > MIN_FTE),
                 n_bad  = sum(is.na(fte) | fte <= MIN_FTE),
                 et_bad = {  # first event time at which this firm fails the screen, NA if it never does
                   b <- event_time[is.na(fte) | fte <= MIN_FTE]
                   if (length(b)) min(b) else NA_integer_ }),
             by = .(stack_id, ev, scoring_protocol, arm, cvr, treated)]
  fs[, bal_row  := n_row == NP & n_per == NP & tmin == -h & tmax == h]
  fs[, bal_fte  := bal_row & n_ok == NP]
  fs[, drop_fte := bal_row & !bal_fte]

  st <- fs[, .(firms_base = .N,
               firms_row  = sum(bal_row),          # estimable on row presence alone
               firms_kept = sum(bal_fte),          # estimable once the screen is applied
               drop_fte   = sum(drop_fte),
               drop_fte_t = sum(drop_fte & treated == 1L),
               drop_fte_c = sum(drop_fte & treated == 0L),
               # dropped firms whose FIRST failing quarter is at or after the award: the screen is
               # removing a post-treatment outcome for these, not excluding them from the population
               drop_fte_post = sum(drop_fte & et_bad >= 0L),
               drop_gap   = sum(!bal_row),         # lost to panel holes, NOT to the screen
               t_row = sum(bal_row & treated == 1L), c_row = sum(bal_row & treated == 0L),
               t_fte = sum(bal_fte & treated == 1L), c_fte = sum(bal_fte & treated == 0L)),
           by = .(stack_id, ev, scoring_protocol, arm)]
  st[, usable_row  := t_row > 0L & c_row > 0L]     # both arms present before the screen
  st[, usable_fte  := t_fte > 0L & c_fte > 0L]     # both arms still present after it
  st[, lost_to_fte := usable_row & !usable_fte]
  st[, lost_side   := fifelse(!lost_to_fte, NA_character_,
                       fifelse(t_fte == 0L & c_fte == 0L, "both",
                        fifelse(t_fte == 0L, "treated", "control")))]

  # ---- what the screen costs -------------------------------------------------------------------------
  n_row_bal  <- fs[, sum(bal_row)]
  n_drop_fte <- fs[, sum(drop_fte)]
  n_stk_row  <- st[, sum(usable_row)]
  n_stk_lost <- st[, sum(lost_to_fte)]
  ev_roll    <- st[, .(stacks_usable = sum(usable_row), stacks_lost = sum(lost_to_fte),
                       firms_dropped = sum(drop_fte)), by = ev]
  ev_touched <- ev_roll[firms_dropped > 0L, .N]
  ev_allgone <- ev_roll[stacks_usable > 0L & stacks_lost == stacks_usable, .N]
  pct <- function(a, b) 100 * a / max(1L, b)

  cat(sprintf("\n  FTE screen (fte > %s) -- cost, attributed:\n", MIN_FTE))
  cat(sprintf("    firm-stacks: %d balanced on row presence | %d dropped BY the screen (%.1f%%)\n",
              n_row_bal, n_drop_fte, pct(n_drop_fte, n_row_bal)))
  cat(sprintf("                 of those, %d treated-side and %d control-side | a further %d were already\n",
              fs[, sum(drop_fte & treated == 1L)], fs[, sum(drop_fte & treated == 0L)],
              fs[, sum(!bal_row)]))
  cat("                 unbalanced from panel holes and are NOT the screen's doing\n")
  cat(sprintf("    stacks     : %d usable before | %d lost >=1 firm | %d LOST ENTIRELY (%.1f%%)\n",
              n_stk_row, st[drop_fte > 0L, .N], n_stk_lost, pct(n_stk_lost, n_stk_row)))
  if (n_stk_lost > 0L) {
    cat("                 side that emptied out:\n")
    print(st[lost_to_fte == TRUE, .(stacks = .N), by = lost_side][order(-stacks)])
  }
  cat(sprintf("    events     : %d total | %d lost >=1 firm | %d lost EVERY usable stack (%.1f%%)\n",
              nrow(ev_roll), ev_touched, ev_allgone, pct(ev_allgone, nrow(ev_roll))))
  cat(sprintf("    firms dropped per event: mean %.2f | median %.0f | p90 %.0f | max %d\n",
              mean(ev_roll$firms_dropped), quantile(ev_roll$firms_dropped, .5),
              quantile(ev_roll$firms_dropped, .9), max(ev_roll$firms_dropped)))
  if (n_drop_fte > 0L)
    cat(sprintf("    of the %d dropped, %d first fail at event_time >= 0 (%.1f%%) and they fail a median\n                 of %.0f of the %d quarters -- a high post share means survival conditioning\n",
                n_drop_fte, fs[, sum(drop_fte & et_bad >= 0L)],
                pct(fs[, sum(drop_fte & et_bad >= 0L)], n_drop_fte),
                fs[drop_fte == TRUE, median(n_bad)], NP))

  # The diagnostic that decides whether this is a population screen or a survival screen: among firms
  # that ARE balanced on row presence, what share fail the FTE test at each event time? Flat = population.
  # Rising in event time = the screen is deleting post-treatment outcomes.
  etp <- base[fs[bal_row == TRUE, .(stack_id, cvr)], on = .(stack_id, cvr), nomatch = NULL][
    , .(firms = .N, fail = sum(is.na(fte) | fte <= MIN_FTE)), by = .(event_time, treated)]
  etp[, share := fail / firms]
  etw <- dcast(etp, event_time ~ treated, value.var = "share")
  setnames(etw, c("0", "1"), c("control", "treated"), skip_absent = TRUE)
  cat("    failure share by event time (firms balanced on row presence):\n")
  print(etw[order(event_time)])

  # ---- apply the screen ------------------------------------------------------------------------------
  d <- base[!is.na(fte) & fte > MIN_FTE][
    fs[bal_fte == TRUE, .(stack_id, cvr)], on = .(stack_id, cvr), nomatch = NULL]
  d <- d[stack_id %in% st[usable_fte == TRUE, stack_id]]
  # The join above returns rows in the key order of `fs`, not the panel order stage 2 wrote. Restoring
  # stage 2's sort keeps this artefact byte-comparable with one built by the pre-instrumentation code.
  setorder(d, ev, scoring_protocol, arm, treatment, cvr, qidx)
  cat(sprintf("\n  stacks: %d -> %d after balancing and the both-arms test\n",
              n_stk_row, uniqueN(d$stack_id)))
  if (!nrow(d)) stop("nothing survives balancing at this window", call. = FALSE)

  d[, weight := 1 / uniqueN(cvr), by = .(stack_id, treated)]

  # per-cell viability, reported rather than silently estimated on thin data
  cells <- d[, .(stacks = uniqueN(stack_id), rows = .N,
                 treated_firms = uniqueN(cvr[treated == 1L]),
                 control_firms = uniqueN(cvr[treated == 0L])),
             by = .(scoring_protocol, arm)][order(scoring_protocol, arm)]
  cells[, usable := stacks >= MIN_STACKS]
  print(cells)
  if (any(!cells$usable))
    cat(sprintf("  note: %d cell(s) below MATCH_MIN_STACKS=%d -- kept in the data, flagged for stage 4\n",
                sum(!cells$usable), MIN_STACKS))

  # ---- checks ----
  aud <- d[, .(n = .N, u = uniqueN(event_time), lo = min(event_time), hi = max(event_time)),
           by = .(stack_id, cvr)]
  stopifnot(all(aud$n == 2L * h + 1L), all(aud$u == 2L * h + 1L),
            all(aud$lo == -h), all(aud$hi == h))
  cat("  OK  every retained firm is exactly balanced on -h..h\n")

  wsum <- d[event_time == 0L, .(w = sum(weight)), by = .(stack_id, treated)]
  stopifnot(all(abs(wsum$w - 1) < 1e-9))
  cat("  OK  weights sum to 1 per (stack, treatment) in every period\n")

  expect <- d[, uniqueN(paste(stack_id, cvr))] * (2L * h + 1L)
  stopifnot(nrow(d) == expect)
  cat(sprintf("  OK  row count equals firms x periods (%d)\n", expect))

  out <- P$reg_data(h)
  write_tab(d, out)
  cat(sprintf("  -> %s (%.0f MB, %d rows)\n", basename(out), file.size(out) / 1e6, nrow(d)))
  meta$cells <- cells

  # Persist the screen's cost next to the data it shaped, so the number in any PI discussion is the one
  # this run actually produced rather than a remembered figure from an earlier build.
  meta$fte_screen <- list(
    min_fte                 = MIN_FTE,
    firm_stacks_row_balance = n_row_bal,
    firm_stacks_dropped     = n_drop_fte,
    firm_stacks_dropped_t   = fs[, sum(drop_fte & treated == 1L)],
    firm_stacks_dropped_c   = fs[, sum(drop_fte & treated == 0L)],
    firm_stacks_dropped_post = fs[, sum(drop_fte & et_bad >= 0L)],
    firm_stacks_panel_holes = fs[, sum(!bal_row)],
    stacks_usable_before    = n_stk_row,
    stacks_touched          = st[drop_fte > 0L, .N],
    stacks_lost             = n_stk_lost,
    stacks_lost_by_side     = st[lost_to_fte == TRUE, .(stacks = .N), by = lost_side],
    events_total            = nrow(ev_roll),
    events_touched          = ev_touched,
    events_lost_entirely    = ev_allgone,
    by_event_time           = etp[order(treated, event_time)])
  write_tab(st, P$reg_attrition)
  cat(sprintf("  -> %s (per-stack attrition, %d stacks)\n", basename(P$reg_attrition), nrow(st)))
}

write_obj(meta, P$reg_meta)
cat(sprintf("\n  -> %s\n", basename(P$reg_meta)))
cat("\nSTAGE 3 complete.\n")
