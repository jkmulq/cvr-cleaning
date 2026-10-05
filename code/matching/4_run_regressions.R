#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 4 -- restrict to good matches, run the event-study regressions, emit estimates and figures.
#
# Reads ONLY stage 3's artefacts (03_reg_data_h*.parquet, 03_reg_meta.rds).
# Writes  04_estimates.rds (incl. $pretrend_tests), 04_coefs.parquet, 04_qscore_sweep.parquet, 04_figures/.
#
# MATCH QUALITY. Stage 2 keeps the BEST available control for every event, but "best available" is not
# the same as "good": a degenerate event whose only eligible firms are poor matches still yields a
# control, just with a large control_qscore. This stage drops those, following the approach in
# 6a_estudy_control.Rmd -- sweep a grid of cutoffs to see the sample-size/match-quality trade-off, then
# estimate on stacks under MATCH_QSCORE_BOUND.
#
# The cutoff is a STACK-level filter, and stage 2 makes that expressible in one predicate: every row
# of a stack, treated included, carries the stack's control_qscore. (All controls in a stack sit at the
# same minimum, so the score describes the stack, not the firm.) `d[control_qscore <= bound]` therefore
# keeps or drops a stack whole. An earlier version left the treated row NA, which meant a bare filter
# silently deleted every treated firm, since NA <= x is NA.
#
# WEIGHTS ARE BUILT ONCE, here, on reading stage 3's data: 1 / (firms in the (stack, treatment) cell), so
# each side of a stack sums to 1 and a stack that tied 40 controls does not outvote one with a single
# match. Computing them before the cutoff is valid because the cutoff is STACK-level -- it keeps or
# drops a stack whole, so no surviving stack's counts change. The sum-to-1 check before each feols
# enforces that; if the cutoff ever becomes per-firm, that check fails rather than letting stale weights
# through. Stacks that lose their whole control side are dropped outright -- a treated firm with no
# surviving counterfactual is not an observation.
#
# SPECIFICATION (the one validated against real co-bidders in
# code/analysis/estudy_matched_vs_cobidder_validation.Rmd, per cell):
#   feols(c(fte, log(fte)) ~ i(event_time, treated, ref = -1)
#         | treated + event_time,
#         cluster = ~ cvr, weights = ~ weight, fixef.rm = "none")
# With only the group level and the event-time profile absorbed, beta_k is exactly the weighted
# difference in means relative to k = -1 -- that report asserts the identity. No firm or
# calendar-quarter-by-industry FE: the earlier `cvr + qidx^industry_grp` version was dropped so the
# headline estimate is the one the validation actually checks. Clustered on the firm, because a
# firm can sit in many stacks. fixef.rm="none" is kept as a guard: with these FEs no cell can be a
# singleton, so it should never bind, and the nobs() check below confirms nothing was dropped.
#
# THE WINDOW IS NOT A CHOICE HERE. It is read from stage 3's metadata, which in turn took it from the
# stage-2 match. For a different window, rerun stage 2 with MATCH_H=<n> -- see the note in stage 3.
#
# One estimation per (scoring_protocol x arm). Protocol is a comparison axis: under
# full_firsthalf the quarters [-h/2, -1] never entered the match, so their coefficients are an HONEST
# pre-trend test; under full_full the same quarters are near-flat by construction.
#
#   Rscript code/matching/4_run_regressions.R
# Options (env):
#   MATCH_QSCORE_BOUND   keep stacks with control_qscore <= this, LEVEL-score protocols (default 0.30)
#   MATCH_QSCORE_BOUND_LOG   the same for LOG-score protocols (default 0.30). The two scores are on
#                        similar scales for moderate gaps (a log RMS of 0.30 is roughly a 26-35%
#                        proportional gap), but they are different quantities -- set from the sweep.
#   MATCH_QSCORE_GRID_LO / _HI / _BY   the sweep grid (defaults 0.01 / 1 / 0.01)
#   MATCH_MIN_STACKS     skip a cell with fewer stacks (default 20)
#   MATCH_SAVE_MODELS    1 to keep the fitted feols objects (default 1; they are large)
#   MATCH_TEST_N, MATCH_OVERWRITE
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))
match_setup(extra_libs = c("fixest", "ggplot2"))

MIN_STACKS  <- match_env_int("MATCH_MIN_STACKS", 20L)
SAVE_MODELS <- match_env_lgl("MATCH_SAVE_MODELS", TRUE)
QBOUND      <- match_env_num("MATCH_QSCORE_BOUND", 0.30)
QBOUND_LOG  <- match_env_num("MATCH_QSCORE_BOUND_LOG", 0.30)
# The cutoff for a protocol follows its score (MATCH_PROTOCOLS$<p>$score, 0_matching_utils.R).
bound_for <- function(pr) if (MATCH_PROTOCOLS[[pr]]$score == "log") QBOUND_LOG else QBOUND
QGRID       <- seq(match_env_num("MATCH_QSCORE_GRID_LO", 0.01),
                   match_env_num("MATCH_QSCORE_GRID_HI", 1.00),
                   by = match_env_num("MATCH_QSCORE_GRID_BY", 0.01))
P           <- match_paths()
dir.create(P$figures, recursive = TRUE, showWarnings = FALSE)

meta <- read_obj(P$reg_meta)
h    <- meta$h                      # the window comes from the match, never the environment
cat(sprintf("STAGE 4 | window=+/-%d (from the stage-2 match) | qscore bound level=%.2f log=%.2f | grid %.2f..%.2f | min_stacks=%d\n",
            h, QBOUND, QBOUND_LOG, min(QGRID), max(QGRID), MIN_STACKS))

# args:
#   d      a stage-3 regression dataset (or one cell of it)
#   bound  keep stacks whose control_qscore is within bound. The treated row carries its stack's
#          score (see the trt block in stage 2), so one predicate keeps both sides.
# returns: d restricted to good matches, with stacks that lost an entire arm removed. Weights are
#          carried through untouched (see the header). Empty data.table if nothing survives.
apply_qscore_cutoff <- function(d, bound) {
  x <- d[control_qscore <= bound]
  if (!nrow(x)) return(x)
  x[, `:=`(has_t = any(treated == 1L), has_c = any(treated == 0L)), by = stack_id]
  x <- x[has_t == TRUE & has_c == TRUE]
  if (!nrow(x)) return(x)
  x[, c("has_t", "has_c") := NULL]
  x[]
}

# args:
#   d     one (window, protocol, arm) cell of a stage-3 dataset
#   grid  cutoffs to evaluate
# returns: one row per cutoff: surviving stacks, whether the panel is still balanced, and the
#          weighted PRE-PERIOD mean FTE of each arm -- the trade-off curve behind the cutoff choice.
sweep_qscore <- function(d, grid) {
  rbindlist(lapply(grid, function(s) {
    x <- apply_qscore_cutoff(d, s)
    if (!nrow(x)) return(data.table(cutoff = s, n_stacks = 0L, balanced = NA,
                                    mean_fte_treated = NA_real_, mean_fte_control = NA_real_))
    pp <- x[event_time < 0L, .(m = sum(weight * fte) / sum(weight)), by = treatment]
    np <- x[, .N, by = event_time]$N
    data.table(cutoff           = s,
               n_stacks         = uniqueN(x$stack_id),
               balanced         = length(unique(np)) == 1L,
               mean_fte_treated = pp[treatment == "treated", m][1],
               mean_fte_control = pp[treatment == "control", m][1])
  }))
}

# PRE-TREND TEST for full_firsthalf. That protocol ranks controls on [-h, -(floor(h/2)+1)] only, so
# the quarters [-floor(h/2), -1] never entered the match and can be tested honestly. H0: every beta_k
# in that unmatched window is zero, i.e. the treated-control gap is flat from -floor(h/2) to the k = -1
# reference. k = -1 itself is the omitted base, so at h = 8 the restrictions are k = -4, -3, -2 (3 df).
# Chi-squared on the model's own clustered vcov, the same matrix behind the plotted CIs.
PT_PROTOCOLS <- c("full_firsthalf", "full_firsthalf_log")
# args:  m = one fitted fixest model (a single outcome); ks = the event times to test jointly
# returns: one-row data.table with the Wald statistic, its df and p-value
pretrend_test <- function(m, ks) {
  nm <- sprintf("event_time::%d:treated", ks)
  stopifnot(all(nm %in% names(coef(m))))
  b <- coef(m)[nm]
  V <- vcov(m)[nm, nm, drop = FALSE]
  W <- drop(t(b) %*% solve(V) %*% b)
  data.table(k_tested = paste(ks, collapse = ","), chisq = W, df = length(ks),
             p = pchisq(W, df = length(ks), lower.tail = FALSE))
}

all_coefs <- list(); all_models <- list(); summary_rows <- list(); all_sweeps <- list()
all_pt <- list()
est_panels <- list()   # the exact rows each feols saw, persisted for downstream manipulation

{
  f <- P$reg_data(h)
  match_rule_banner(sprintf("window +/-%d", h))
  d <- read_tab(f)
  # THE one place weights are computed. stack_id is unique to a (event, protocol, arm), so slicing to a
  # cell below never changes a stack's counts.
  d[, weight := 1 / uniqueN(cvr), by = .(stack_id, treated)]
  cells <- unique(d[, .(scoring_protocol, arm)]); setorder(cells, scoring_protocol, arm)

  # ---- the cutoff sweep, before any restriction ------------------------------------------------------
  cat("  sweeping the quality cutoff ...\n")
  sw <- rbindlist(lapply(seq_len(nrow(cells)), function(i) {
    s <- sweep_qscore(d[scoring_protocol == cells$scoring_protocol[i] & arm == cells$arm[i]], QGRID)
    s[, `:=`(window = h, scoring_protocol = cells$scoring_protocol[i], arm = cells$arm[i])]
  }))
  all_sweeps[[as.character(h)]] <- sw
  # one dashed line per protocol facet, at that protocol's own bound
  bl <- data.table(scoring_protocol = unique(sw$scoring_protocol))
  bl[, bound := vapply(scoring_protocol, bound_for, numeric(1))]

  # graph 1 -- stacks retained as the cutoff loosens
  p1 <- ggplot(sw, aes(cutoff, n_stacks)) +
    geom_line(linewidth = 0.5, colour = "#2c7fb8") +
    geom_vline(data = bl, aes(xintercept = bound), linetype = "dashed", colour = "grey40") +
    facet_grid(arm ~ scoring_protocol, scales = "free_y") +
    labs(title = sprintf("Number of matched stacks by control quality cutoff (+/-%d quarters)", h),
         x = "Control quality cutoff (control_qscore)", y = "Number of matched stacks",
         caption = sprintf("Dashed line = the protocol's bound: level %.2f (MATCH_QSCORE_BOUND), log %.2f (MATCH_QSCORE_BOUND_LOG)",
                           QBOUND, QBOUND_LOG)) +
    theme_light(base_size = 11) + theme(plot.title = element_text(face = "bold"),
                                        plot.caption = element_text(hjust = 0))
  ggsave(file.path(P$figures, sprintf("qscore_stacks_h%d.png", h)), p1, width = 10, height = 6, dpi = 150)

  # graph 2 -- pre-period FTE of each arm as the cutoff loosens (the two lines should track)
  swl <- melt(sw, id.vars = c("cutoff", "window", "scoring_protocol", "arm"),
              measure.vars = c("mean_fte_treated", "mean_fte_control"),
              variable.name = "series", value.name = "mean_fte")
  p2 <- ggplot(swl, aes(cutoff, mean_fte, colour = series)) +
    geom_line(linewidth = 0.5) +
    geom_vline(data = bl, aes(xintercept = bound), linetype = "dashed", colour = "grey40") +
    facet_grid(arm ~ scoring_protocol, scales = "free_y") +
    labs(title = sprintf("Pre-period FTE by control quality cutoff (+/-%d quarters)", h),
         x = "Control quality cutoff (control_qscore)", y = "Weighted pre-period mean FTE",
         colour = NULL,
         caption = "Divergence between the lines as the cutoff loosens is the match degrading.") +
    theme_light(base_size = 11) + theme(plot.title = element_text(face = "bold"),
                                        plot.caption = element_text(hjust = 0),
                                        legend.position = "bottom")
  ggsave(file.path(P$figures, sprintf("qscore_prefte_h%d.png", h)), p2, width = 10, height = 6, dpi = 150)
  cat(sprintf("  -> qscore_stacks_h%d.png | qscore_prefte_h%d.png\n", h, h))

  # ---- restrict, then estimate -------------------------------------------------------------------------
  levels_all <- list()
  for (i in seq_len(nrow(cells))) {
    pr <- cells$scoring_protocol[i]; ar <- cells$arm[i]
    d0 <- d[scoring_protocol == pr & arm == ar]
    n_stk_all <- uniqueN(d0$stack_id)
    qb <- bound_for(pr)
    dd <- apply_qscore_cutoff(d0, qb)
    tag <- sprintf("h%d | %s | %s", h, pr, ar)
    if (!nrow(dd)) { cat(sprintf("\n  %s: nothing survives the cutoff -- skipped\n", tag)); next }
    n_stk <- uniqueN(dd$stack_id)
    if (n_stk < MIN_STACKS) {
      cat(sprintf("\n  %s: %d stacks after cutoff (< %d) -- skipped\n", tag, n_stk, MIN_STACKS)); next
    }
    cat(sprintf("\n  %s: %d of %d stacks kept (%.1f%%) | %d rows | treated %d / control %d firms\n",
                tag, n_stk, n_stk_all, 100 * n_stk / n_stk_all, nrow(dd),
                dd[treated == 1L, uniqueN(cvr)], dd[treated == 0L, uniqueN(cvr)]))

    # the cutoff must not have unbalanced the panel
    aud <- dd[, .(n = .N, u = uniqueN(event_time)), by = .(stack_id, cvr)]
    stopifnot(all(aud$n == 2L * h + 1L), all(aud$u == 2L * h + 1L))
    # Every period, not just t = 0: this is what makes the coefficient a stack-equal-weighted
    # difference in means, the estimator the co-bidder report validates.
    wsum <- dd[, .(w = sum(weight)), by = .(stack_id, treated, event_time)]
    stopifnot(all(abs(wsum$w - 1) < 1e-9))
    cat("    OK  still balanced, weights sum to 1 per (stack, arm) in every period\n")

    # graph 3 -- weighted mean FTE levels by event time
    wm <- dd[, .(mean_fte = sum(weight * fte) / sum(weight)), by = .(event_time, treatment)]
    wm[, `:=`(window = h, scoring_protocol = pr, arm = ar,
              caption = sprintf("%s qscore <= %.2f: %s of %s stacks (%.2f)", pr, qb,
                                format(n_stk, big.mark = ","), format(n_stk_all, big.mark = ","),
                                n_stk / n_stk_all))]
    levels_all[[paste(pr, ar)]] <- wm

    # i(event_time, treated) gives one interaction per k only if treated is numeric 0/1. A factor gets an
    # interaction for each level, collinear with the event_time FE, and fixest may drop the wrong ones --
    # silently moving the omitted period.
    stopifnot(is.numeric(dd$treated), all(dd$treated %in% c(0, 1)))
    est <- feols(c(fte, log(fte)) ~ i(event_time, treated, ref = -1) |
                   treated + event_time,
                 data = dd, cluster = ~ cvr, weights = ~ weight, fixef.rm = "none")
    stopifnot(nobs(est[[1]]) == nrow(dd), nobs(est[[2]]) == nrow(dd))
    cat("    OK  fixest retained every observation\n")
    print(etable(est))

    for (m in seq_along(est)) {
      ct <- as.data.table(coeftable(est[[m]]), keep.rownames = "term")[grepl("event_time", term)]
      ct[, event_time := as.integer(gsub(".*event_time::(-?[0-9]+).*", "\\1", term))]
      stopifnot(!(-1L %in% ct$event_time))          # t = -1 is the omitted base period
      ct <- rbind(ct[, .(event_time, est = Estimate, se = `Std. Error`)],
                  data.table(event_time = -1L, est = 0, se = 0))
      ct[, `:=`(window = h, scoring_protocol = pr, arm = ar,
                outcome = c("FTE (level)", "log FTE")[m],
                lo = est - 1.96 * se, hi = est + 1.96 * se)]
      all_coefs[[paste(h, pr, ar, m)]] <- ct

      if (pr %chin% PT_PROTOCOLS) {
        ks <- setdiff(setdiff(seq.int(-h, -1L), MATCH_PROTOCOLS[[pr]]$rank(h)), -1L)
        if (length(ks)) {
          pt <- pretrend_test(est[[m]], ks)
          pt[, `:=`(window = h, scoring_protocol = pr, arm = ar,
                    outcome = c("FTE (level)", "log FTE")[m])]
          all_pt[[paste(h, pr, ar, m)]] <- pt
          cat(sprintf("    pre-trend (%s): unmatched k = %s jointly 0 | chisq(%d) = %.2f | p = %.4f\n",
                      pt$outcome, pt$k_tested, pt$df, pt$chisq, pt$p))
        }
      }
    }
    if (SAVE_MODELS) all_models[[paste(h, pr, ar)]] <- est
    est_panels[[paste(pr, ar)]] <- copy(dd)   # post-cutoff, with the weights feols actually used
    summary_rows[[paste(h, pr, ar)]] <- data.table(
      window = h, scoring_protocol = pr, arm = ar, qscore_bound = qb,
      stacks_before = n_stk_all, stacks = n_stk, rows = nrow(dd),
      treated_firms = dd[treated == 1L, uniqueN(cvr)], control_firms = dd[treated == 0L, uniqueN(cvr)])
  }

  if (length(levels_all)) {
    lv <- rbindlist(levels_all)
    for (ar in unique(lv$arm)) {
      la <- lv[arm == ar]
      p3 <- ggplot(la, aes(event_time, mean_fte, colour = treatment)) +
        geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey60") +
        geom_line(linewidth = 0.5) + geom_point(size = 1.4) +
        facet_wrap(~ scoring_protocol, scales = "free_y") +
        scale_x_continuous(breaks = seq(-h, h, 2)) +
        labs(title = sprintf("FTE by treatment status (+/-%d quarters, arm: %s)", h, ar),
             y = "FTE (level)", x = "Event time (t = 0 is the award quarter)", colour = NULL,
             caption = paste(unique(la$caption), collapse = "  |  ")) +
        theme_light(base_size = 11) + theme(plot.title = element_text(face = "bold"),
                                            plot.caption = element_text(hjust = 0),
                                            legend.position = "bottom")
      ggsave(file.path(P$figures, sprintf("levels_h%d_%s.png", h, ar)), p3, width = 10, height = 5, dpi = 150)
      cat(sprintf("  -> levels_h%d_%s.png\n", h, ar))
    }
  }
}

if (!length(all_coefs)) stop("no cell was estimable -- loosen MATCH_QSCORE_BOUND or lower MATCH_MIN_STACKS",
                             call. = FALSE)

coefs <- rbindlist(all_coefs, use.names = TRUE)
setorder(coefs, window, scoring_protocol, arm, outcome, event_time)
summ  <- rbindlist(summary_rows, use.names = TRUE)   # built here: the figure captions quote it
pt_tests <- rbindlist(all_pt, use.names = TRUE)      # empty if no full_firsthalf cell was estimated
if (nrow(pt_tests)) setcolorder(pt_tests, c("window", "scoring_protocol", "arm", "outcome"))

# ---- coefficient figures -------------------------------------------------------------------------------
match_rule_banner("coefficient figures")
for (ar in unique(coefs$arm)) {
  cc <- coefs[arm == ar]
  if (!nrow(cc)) next
  # How much sample the quality filter cost, per protocol -- so a reader of the figure can see the
  # estimate's base without opening the summary table.
  ss  <- summ[arm == ar][order(scoring_protocol)]
  stk <- paste(sprintf("%s (qscore <= %.2f): %s of %s stacks kept (%.1f%%)", ss$scoring_protocol,
                       ss$qscore_bound,
                       format(ss$stacks, big.mark = ","), format(ss$stacks_before, big.mark = ","),
                       100 * ss$stacks / ss$stacks_before), collapse = "\n")
  # One label per facet (outcome), carrying the outcome column so facet_wrap places it.
  pt_lab <- if (nrow(pt_tests)) pt_tests[arm == ar, .(label = paste(sprintf(
    "%s pre-trend: beta_k = 0 for k = %s\nchisq(%d) = %.2f, p = %.3f",
    scoring_protocol, k_tested, df, chisq, p), collapse = "\n")), by = outcome] else NULL
  p <- ggplot(cc, aes(event_time, est, colour = scoring_protocol, fill = scoring_protocol)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey60") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.5) + geom_point(size = 1.6) +
    facet_wrap(~ outcome, scales = "free_y") +
    scale_x_continuous(breaks = seq(-h, h, 2)) +
    (if (!is.null(pt_lab) && nrow(pt_lab))
       geom_text(data = pt_lab, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
                 hjust = -0.03, vjust = 1.2, size = 3, lineheight = 0.95, colour = "grey20")) +
    labs(title = sprintf("Competitive award: employment response (+/-%d quarters)", h),
         subtitle = sprintf("Arm: %s | weighted, treated + event_time FE", ar),
         x = "Quarters relative to award (t = -1 omitted)", y = "Effect (treated - control)",
         colour = "match protocol", fill = "match protocol",
         caption = paste0(stk, "\n",
                          "Shaded = 95% CI, clustered by firm. *_log protocols match on the log-gap score.\n",
                          "Under *_firsthalf* the quarters after -h/2 did NOT enter the match; ",
                          "the pre-trend test (top left) is the joint Wald test that they are all 0.")) +
    theme_light(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0),
          legend.position = "bottom")
  ggsave(file.path(P$figures, sprintf("estudy_h%d_%s.png", h, ar)), p, width = 10, height = 6.5, dpi = 150)
  cat(sprintf("  -> estudy_h%d_%s.png\n", h, ar))
}

# ---- write ------------------------------------------------------------------------------------------------
match_rule_banner("write")
sweeps <- rbindlist(all_sweeps, use.names = TRUE)
print(summ)
if (nrow(pt_tests)) { cat("\n  pre-trend tests (unmatched pre-period, jointly 0):\n"); print(pt_tests) }
write_tab(coefs,  P$coefs)
write_tab(sweeps, P$sweep)
# The estimation samples, stacked and long in (scoring_protocol, arm). This is the artefact to reach for
# when you want to re-plot, re-weight, or re-estimate by hand: load_estimation_panel() reads it back.
if (length(est_panels)) {
  ep <- rbindlist(est_panels, use.names = TRUE, fill = TRUE)
  write_tab(ep, P$est_panel)
  cat(sprintf("  estimation panel: %d rows | %d cells -> %s\n",
              nrow(ep), uniqueN(ep[, paste(scoring_protocol, arm)]), basename(P$est_panel)))
}
write_obj(list(run_at = Sys.time(), qscore_bound = QBOUND, qscore_bound_log = QBOUND_LOG, summary = summ, coefs = coefs,
               pretrend_tests = pt_tests, sweep = sweeps, models = if (SAVE_MODELS) all_models else NULL), P$estimates)
cat(sprintf("  -> %s | %s | %s | %s/\n", basename(P$coefs), basename(P$estimates),
            basename(P$sweep), basename(P$figures)))
cat("\nSTAGE 4 complete.\n")
