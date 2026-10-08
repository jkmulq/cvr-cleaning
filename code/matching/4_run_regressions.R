#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 4 -- restrict to good matches, run the event-study regressions, emit estimates and figures.
#
# Reads stage 3's artefacts (03_reg_data_h*.parquet, 03_reg_meta.rds). For the pscore caliper graph only,
# also 01b_pscore_model.rds and 02_match_report.rds: the real losers' index, and the model stage 2 used.
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
# THE pscore PROTOCOL HAS NO qscore CUTOFF. Its controls are the top K by bidding propensity, not the
# closest pre-period path, so a path-score bound would quietly re-impose path matching; control_qscore is
# NA on its rows. bound_for() returns Inf for it, apply_qscore_cutoff() treats a non-finite bound as "keep
# every stack", and the qscore sweep skips it. Its pre-trend test covers the pre-period quarters the
# propensity characteristics did not read (MATCH_PROTOCOLS$pscore$rank).
# THE TRIM, ALWAYS ON (MATCH_PS_TRIM, default 0.25; the user's standing choice): in each pscore cell
# (protocol x arm), drop the control firm-stacks whose propensity index (control_pscore) is below the 25th
# percentile of that cell's control firm-stacks, then the stacks left with no control. A per-FIRM filter,
# unlike the qscore cutoff; the dotted line on the sweep graph marks each pool's cutoff.
# A CALIPER can cut further (MATCH_PS_CALIPER = c, off by default): the cut is then at whichever of the two
# is higher. The index has no intercept, so it reads as log odds against a baseline firm: index 0 = outside
# the winner's 2-digit industry, ApS, outside the buyers' kommuner and the winner's, with the winner's
# average size over the pre-period; a firm at index c is e^c times as likely to have bid as that firm. The
# caliper sweep (pscore_caliper_stacks_h{h}.png) shows the stacks kept at each c, next to the share of 1b's
# real losing bidders scoring at least c.
# pscore_age (opt-in: the no-FTE model, firm age in place of size) is handled exactly like
# pscore, with its own sweep graph (pscore_age_caliper_stacks_h{h}.png). Its baseline firm has the winner's
# age instead, and its coefficients are its own, so the same index means different things under the two
# models: MATCH_PS_CALIPER applies one cutoff to every pscore protocol, so read both sweeps if both ran.
#
# WEIGHTS: 1 / (firms in the (stack, treatment) cell), so each side of a stack sums to 1 and a stack that
# tied 40 controls does not outvote one with a single match. They are computed on reading stage 3's data
# (for the sweeps), and AGAIN for each cell on the rows actually fitted, after the cutoff, trim and caliper.
# The cutoff keeps or drops a stack whole, so for the cascade protocols the second pass reproduces the
# first; the caliper drops single controls, so for pscore it is what keeps every (stack, side) at 1. The
# sum-to-1 check before each feols then guards balance: a firm missing a period would leave it short.
# Stacks that lose their whole control side are dropped outright -- a treated firm with no surviving
# counterfactual is not an observation.
#
# SPECIFICATION (the one validated against real co-bidders in
# code/analysis/14_estudy_matched_vs_cobidder_validation.Rmd, per cell):
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
#   MATCH_PS_CALIPER     pscore protocols only (pscore* -- the same cutoff for each): drop control
#                        firm-stacks whose propensity index is below this before estimating (unset = no
#                        caliper; see the header)
#   MATCH_PS_CALIPER_GRID_BY   step of the caliper sweep, in index units, over the controls' range (default 0.05)
#   MATCH_PS_TRIM        pscore protocols only: drop the bottom share of each cell's control firm-stacks by
#                        index before estimating (default 0.25 -- always on unless set to 0; see the header).
#                        Any other value writes to suffixed outputs: 0.5 -> 04_*_trim50.*, 04_figures_trim50/
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
# pscore: Inf, i.e. no cutoff (see the header).
bound_for <- function(pr) switch(MATCH_PROTOCOLS[[pr]]$score, log = QBOUND_LOG, pscore = Inf, QBOUND)
QGRID       <- seq(match_env_num("MATCH_QSCORE_GRID_LO", 0.01),
                   match_env_num("MATCH_QSCORE_GRID_HI", 1.00),
                   by = match_env_num("MATCH_QSCORE_GRID_BY", 0.01))
# pscore caliper: the minimum propensity index a control must reach. Unset = no caliper (see the header).
PS_CALIPER  <- match_env_num("MATCH_PS_CALIPER", NA_real_)
if (nzchar(Sys.getenv("MATCH_PS_CALIPER")) && !is.finite(PS_CALIPER))
  stop("MATCH_PS_CALIPER must be a number: the minimum propensity index a control must reach", call. = FALSE)
PS_CAL_ON   <- is.finite(PS_CALIPER)
# pscore trim (the user's standing choice, 7 Oct 2026: "i always want the bottom 25% removed"): drop the
# bottom MATCH_PS_TRIM share of each pscore cell's control firm-stacks by index, before MATCH_PS_CALIPER.
PS_TRIM     <- match_env_num("MATCH_PS_TRIM", PS_TRIM_DEFAULT)
# A trim other than the default writes its own set of 04_* files and 04_figures* (match_s4_tag(), e.g.
# MATCH_PS_TRIM=0.5 -> 04_estimates_trim50.rds, 04_figures_trim50/), so the default run is never overwritten.
if (!is.finite(PS_TRIM) || PS_TRIM < 0 || PS_TRIM >= 1)
  stop("MATCH_PS_TRIM must be a share in [0, 1): the bottom share of control firm-stacks to drop", call. = FALSE)
PS_CGRID_BY <- match_env_num("MATCH_PS_CALIPER_GRID_BY", 0.05)
is_pscore   <- function(pr) MATCH_PROTOCOLS[[pr]]$score == "pscore"
P           <- match_paths()
dir.create(P$figures, recursive = TRUE, showWarnings = FALSE)

meta <- read_obj(P$reg_meta)
h    <- meta$h                      # the window comes from the match, never the environment
# The pscore protocol's K, for captions only ("K" if this match had none or predates it)
K_LABEL <- if (is.null(meta$ps_topk) || is.na(meta$ps_topk)) "K" else as.character(meta$ps_topk)
cat(sprintf("STAGE 4 | window=+/-%d (from the stage-2 match) | qscore bound level=%.2f log=%.2f | grid %.2f..%.2f | pscore trim=bottom %.0f%% | pscore caliper=%s | min_stacks=%d\n",
            h, QBOUND, QBOUND_LOG, min(QGRID), max(QGRID), 100 * PS_TRIM,
            if (PS_CAL_ON) sprintf("index >= %.2f", PS_CALIPER) else "none", MIN_STACKS))
cat(sprintf("  outputs: %s | %s\n", basename(P$estimates), basename(P$figures)))

# How a pscore cell was cut, in words, for tags and captions. bound = the index it was cut at.
ps_cut_words <- function(bound) {
  parts <- c(if (PS_TRIM > 0) sprintf("bottom %.0f%% of control firm-stacks trimmed", 100 * PS_TRIM),
             if (PS_CAL_ON) sprintf("caliper %.2f", PS_CALIPER))
  sprintf("index >= %.2f: %s", bound, paste(parts, collapse = ", "))
}

# args:
#   d      a stage-3 regression dataset (or one cell of it)
#   bound  keep stacks whose control_qscore is within bound. The treated row carries its stack's
#          score (see the trt block in stage 2), so one predicate keeps both sides. A non-finite bound
#          means NO cutoff (pscore, where control_qscore is NA and NA <= Inf would drop every row).
# returns: d restricted to good matches, with stacks that lost an entire arm removed. Weights are
#          carried through untouched (see the header). Empty data.table if nothing survives.
apply_qscore_cutoff <- function(d, bound) {
  x <- if (is.finite(bound)) d[control_qscore <= bound] else copy(d)
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

# THE pscore CALIPER (see the header).
# args:    d  one pscore (protocol, arm) cell;  bound  the minimum propensity index a control must reach
# returns: d without the control firm-stacks below bound, and without the stacks left with no control
#          (their treated rows go too). Weights are NOT touched here: the caller re-weights the rows it fits.
apply_ps_caliper <- function(d, bound) {
  x <- d[treated == 1L | control_pscore >= bound]
  x[, has_c := any(treated == 0L), by = stack_id]
  x <- x[has_c == TRUE]
  x[, has_c := NULL]
  x[]
}

# args:    d  one pscore cell;  grid  index cutoffs;  ref  real losing bidders' indices (1b), or NULL
# returns: one row per cutoff: stacks kept, mean controls per kept stack, the share of the cell's control
#          firm-stacks at or above the cutoff, and the same share among the real losing bidders. A stack
#          survives while its best control clears the cutoff, so this matches apply_ps_caliper() without
#          refitting. One value per control firm-stack, read at k = -1 (every balanced firm-stack has it).
sweep_ps_caliper <- function(d, grid, ref = NULL) {
  cs <- d[treated == 0L & event_time == -1L, .(stack_id, s = control_pscore)]
  rbindlist(lapply(grid, function(b) {
    k <- cs[s >= b, .N, by = stack_id]
    data.table(index_cutoff = b, n_stacks = nrow(k), mean_controls = if (nrow(k)) mean(k$N) else NA_real_,
               share_controls = mean(cs$s >= b),
               share_losers   = if (length(ref)) mean(ref >= b) else NA_real_)
  }))
}

# PRE-TREND TEST for full_firsthalf. That protocol ranks controls on [-h, -(floor(h/2)+1)] only, so
# the quarters [-floor(h/2), -1] never entered the match and can be tested honestly. H0: every beta_k
# in that unmatched window is zero, i.e. the treated-control gap is flat from -floor(h/2) to the k = -1
# reference. k = -1 itself is the omitted base, so at h = 8 the restrictions are k = -4, -3, -2 (3 df).
# Chi-squared on the model's own clustered vcov, the same matrix behind the plotted CIs.
# pscore matched on no path at all, and on size only through the AVERAGE log FTE over t-8 .. t-1: that
# pins no single quarter and no slope, so its `rank` is empty and its test covers every pre-period quarter
# but the reference, k = -8 .. -2 at h = 8 (-4 .. -2 at h = 4). So do pscore_age's and pscore_nofte's.
# pscore_fte read FTE at t-2 and t-6, so its test leaves them out: k = -8, -7, -5, -4, -3 at h = 8.
PT_PROTOCOLS <- c("full_firsthalf", "full_firsthalf_log", pscore_protocols())
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
all_pt <- list(); all_ps_sweeps <- list()
est_panels <- list()   # the exact rows each feols saw, persisted for downstream manipulation

{
  f <- P$reg_data(h)
  match_rule_banner(sprintf("window +/-%d", h))
  d <- read_tab(f)
  # Weights for the sweeps. stack_id is unique to a (event, protocol, arm), so slicing to a cell never
  # changes a stack's counts; each cell is re-weighted after its cutoff and caliper (see the header).
  d[, weight := 1 / uniqueN(cvr), by = .(stack_id, treated)]
  cells <- unique(d[, .(scoring_protocol, arm)]); setorder(cells, scoring_protocol, arm)

  # ---- the cutoff sweep, before any restriction ------------------------------------------------------
  # Only for protocols that HAVE a cutoff: pscore stacks are never cut (see the header).
  cat("  sweeping the quality cutoff ...\n")
  sw_cells <- cells[is.finite(vapply(scoring_protocol, bound_for, numeric(1)))]
  sw <- rbindlist(lapply(seq_len(nrow(sw_cells)), function(i) {
    s <- sweep_qscore(d[scoring_protocol == sw_cells$scoring_protocol[i] & arm == sw_cells$arm[i]], QGRID)
    s[, `:=`(window = h, scoring_protocol = sw_cells$scoring_protocol[i], arm = sw_cells$arm[i])]
  }))
  all_sweeps[[as.character(h)]] <- sw
  if (nrow(sw)) {
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
  } else cat("  no protocol in this match has a qscore cutoff -- no sweep\n")

  # ---- the pscore caliper sweep ----------------------------------------------------------------------------
  # How many stacks survive as the minimum index rises (see the header). The reference for reading a cutoff
  # is the real losing bidders' own index in 1b's training strata -- taken only from the model stage 2
  # matched with, so the two sets of scores are on the same scale. One model per pscore protocol.
  ps_cells <- cells[vapply(scoring_protocol, is_pscore, logical(1))]
  # The trim's cutoff per pscore cell: the MATCH_PS_TRIM quantile of its control firm-stacks' index, one
  # value each, read at k = -1 as the sweep reads them. Controls below it are dropped before fitting.
  ps_trim_cut <- d[scoring_protocol %chin% ps_cells$scoring_protocol & treated == 0L & event_time == -1L,
                   .(trim_cut = if (PS_TRIM > 0) quantile(control_pscore, PS_TRIM, names = FALSE, na.rm = TRUE) else -Inf),
                   by = .(scoring_protocol, arm)]
  ps_ref   <- list()
  m_at_all <- if (nrow(ps_cells)) read_obj(P$match_report)$ps_model_run_at else NULL
  for (pr in unique(ps_cells$scoring_protocol)) {
    pv <- MATCH_PROTOCOLS[[pr]]$variant
    mf <- P$ps_model_for(pv)
    if (!file.exists(mf)) next
    psm  <- read_pscore_model(mf, pv)
    m_at <- if (is.list(m_at_all)) m_at_all[[pr]] else m_at_all   # a report from before several pscore protocols
    if (!is.null(m_at) && identical(format(psm$run_at), format(m_at))) ps_ref[[pr]] <- psm$ranks$idx else
      cat(sprintf("  note: the %s model on disk is not the one stage 2 matched with -- no real-loser reference\n", pr))
  }
  psw <- NULL
  if (nrow(ps_cells)) {
    sc <- d[scoring_protocol %chin% ps_cells$scoring_protocol & treated == 0L & event_time == -1L, control_pscore]
    grid <- seq(floor(min(sc, na.rm = TRUE) / PS_CGRID_BY) * PS_CGRID_BY,
                ceiling(max(sc, na.rm = TRUE) / PS_CGRID_BY) * PS_CGRID_BY, by = PS_CGRID_BY)
    psw <- rbindlist(lapply(seq_len(nrow(ps_cells)), function(i) {
      s <- sweep_ps_caliper(d[scoring_protocol == ps_cells$scoring_protocol[i] & arm == ps_cells$arm[i]], grid,
                            ps_ref[[ps_cells$scoring_protocol[i]]])
      s[, `:=`(window = h, scoring_protocol = ps_cells$scoring_protocol[i], arm = ps_cells$arm[i])]
    }))
  }
  all_ps_sweeps[[as.character(h)]] <- psw
  if (length(psw) && nrow(psw)) {
    cat("  pscore caliper sweep (whole-number cutoffs):\n")
    print(psw[abs(index_cutoff - round(index_cutoff)) < 1e-9])
  }
  # graph 4, one per pscore protocol -- stacks kept, controls per kept stack, and the share of controls (and
  # of real losing bidders) at or above the cutoff, as the minimum index rises
  for (pr in if (length(psw)) unique(psw$scoring_protocol) else character()) {
    pp <- psw[scoring_protocol == pr]
    pl <- rbind(pp[, .(arm, index_cutoff, series = "Stacks kept",                    group = "pscore controls", value = n_stacks)],
                pp[, .(arm, index_cutoff, series = "Controls per kept stack (mean)", group = "pscore controls", value = mean_controls)],
                pp[, .(arm, index_cutoff, series = "Share at or above the cutoff",  group = "pscore controls", value = share_controls)],
                pp[!is.na(share_losers),
                   .(arm, index_cutoff, series = "Share at or above the cutoff", group = "real losing bidders (1b)", value = share_losers)])
    pl[, series := factor(series, levels = c("Stacks kept", "Controls per kept stack (mean)", "Share at or above the cutoff"))]
    p4 <- ggplot(pl, aes(index_cutoff, value, colour = group)) +
      geom_line(linewidth = 0.5) +
      (if (PS_CAL_ON) geom_vline(xintercept = PS_CALIPER, linetype = "dashed", colour = "grey40")) +
      (if (PS_TRIM > 0) geom_vline(data = ps_trim_cut[scoring_protocol == pr], aes(xintercept = trim_cut),
                                   linetype = "dotted", colour = "grey20")) +
      facet_grid(series ~ arm, scales = "free_y") +
      scale_x_continuous(sec.axis = sec_axis(~ exp(.), breaks = 10^(-2:4),
                                             labels = c("x0.01", "x0.1", "x1", "x10", "x100", "x1,000", "x10,000"),
                                             name = "Odds of having bid, relative to the baseline firm (e^index)")) +
      scale_colour_manual(values = c("pscore controls" = "#2c7fb8", "real losing bidders (1b)" = "#d95f02")) +
      labs(title = sprintf("%s: stacks kept by caliper on the propensity index (+/-%d quarters)", pr, h),
           x = "Caliper: minimum propensity index a control must reach", y = NULL, colour = NULL,
           caption = paste0(
             "Index 0 = the baseline firm: outside the winner's 2-digit industry, ApS, outside the buyers' kommuner,\n",
             ps_variant(MATCH_PROTOCOLS[[pr]]$variant)$baseline,
             ". A firm at index s is e^s times as likely to have bid as that firm (top axis). A stack is kept while its best\n",
             "control clears the cutoff. Real losing bidders = 1b's training losers, each scored in its own stratum (in-sample).",
             if (PS_TRIM > 0) sprintf("\nDotted line = this pool's trim cutoff: the bottom %.0f%% of control firm-stacks are dropped.", 100 * PS_TRIM) else "",
             if (PS_CAL_ON) sprintf("\nDashed line = MATCH_PS_CALIPER (%.2f).", PS_CALIPER) else "")) +
      theme_light(base_size = 11) + theme(plot.title = element_text(face = "bold"),
                                          plot.caption = element_text(hjust = 0), legend.position = "bottom")
    fig <- sprintf("%s_caliper_stacks_h%d.png", pr, h)
    ggsave(file.path(P$figures, fig), p4, width = 10, height = 8, dpi = 150)
    cat(sprintf("  -> %s\n", fig))
  }

  # ---- restrict, then estimate -------------------------------------------------------------------------
  levels_all <- list()
  for (i in seq_len(nrow(cells))) {
    pr <- cells$scoring_protocol[i]; ar <- cells$arm[i]
    d0 <- d[scoring_protocol == pr & arm == ar]
    n_stk_all <- uniqueN(d0$stack_id)
    qb <- bound_for(pr)
    dd <- apply_qscore_cutoff(d0, qb)
    # pscore: the trim's cutoff, or the caliper if it is higher; NA = no cut (trim 0, no caliper)
    tc     <- if (is_pscore(pr)) ps_trim_cut[scoring_protocol == pr & arm == ar, trim_cut] else numeric()
    ps_cal <- if (is_pscore(pr)) max(c(tc, if (PS_CAL_ON) PS_CALIPER, -Inf)) else NA_real_
    if (!is.finite(ps_cal)) ps_cal <- NA_real_
    if (!is.na(ps_cal) && nrow(dd)) dd <- apply_ps_caliper(dd, ps_cal)
    # Re-weight on the rows actually fitted, so every (stack, side) sums to 1 after the cutoff and caliper
    if (nrow(dd)) dd[, weight := 1 / uniqueN(cvr), by = .(stack_id, treated)]
    tag <- sprintf("h%d | %s | %s", h, pr, ar)
    if (!is.na(ps_cal)) tag <- sprintf("%s | %s", tag, ps_cut_words(ps_cal))
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
              caption = if (is.finite(qb))
                sprintf("%s qscore <= %.2f: %s of %s stacks (%.2f)", pr, qb,
                        format(n_stk, big.mark = ","), format(n_stk_all, big.mark = ","), n_stk / n_stk_all)
              else if (!is.na(ps_cal))
                sprintf("%s top-%s by propensity, %s: %s of %s stacks (%.2f)",
                        pr, K_LABEL, ps_cut_words(ps_cal), format(n_stk, big.mark = ","),
                        format(n_stk_all, big.mark = ","), n_stk / n_stk_all)
              else sprintf("%s top-%s by propensity, no cutoff: %s stacks", pr, K_LABEL,
                           format(n_stk, big.mark = ",")))]
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
      window = h, scoring_protocol = pr, arm = ar, qscore_bound = qb, ps_caliper = ps_cal,
      ps_trim = if (is_pscore(pr)) PS_TRIM else NA_real_,
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
# One figure per (arm, family): the cascade protocols in estudy_h{h}_{arm}_cascade.png, the propensity
# models in estudy_h{h}_{arm}_pscore.png. Each quarter's estimates sit side by side as point-ranges (95% CI
# bars, clustered by firm). Kept deliberately bare -- the event studies only: the stack counts and the
# protocol notes are in the log and in 04_estimates, and only the cascade figure carries its pre-trend
# test. Each protocol keeps one colour in every figure (the dataviz skill's validated categorical slots,
# assigned by name).
PROTO_COL <- c(full_full = "#2a78d6", full_firsthalf = "#eb6834", full_full_log = "#1baf7a",
               full_firsthalf_log = "#eda100", full_lasthalf = "#e87ba4",
               pscore = "#2a78d6", pscore_fte = "#eb6834", pscore_age = "#1baf7a", pscore_nofte = "#eda100")
FIG_TITLE <- c(cascade = "Cascade matches", pscore = "Bidding-propensity matches")
ARM_TITLE <- c(never_winner = "never-winner controls", winner = "sometimes-winner controls")
match_rule_banner("coefficient figures")
for (ar in unique(coefs$arm)) for (fam in names(FIG_TITLE)) {
  in_fam <- function(pr) vapply(pr, is_pscore, logical(1)) == (fam == "pscore")
  cc <- coefs[arm == ar & in_fam(scoring_protocol)]
  if (!nrow(cc)) next
  # The cascade's pre-trend test, one label per facet (outcome) so facet_wrap places it
  pt_ar  <- if (fam == "cascade" && nrow(pt_tests)) pt_tests[arm == ar & in_fam(scoring_protocol)] else NULL
  pt_lab <- if (length(pt_ar) && nrow(pt_ar)) pt_ar[, .(label = paste(sprintf(
    "%s pre-trend: chisq(%d) = %.1f, p = %.3f", scoring_protocol, df, chisq, p), collapse = "\n")),
    by = outcome] else NULL
  p <- ggplot(cc, aes(event_time, est, colour = scoring_protocol)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey60") +
    geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.7),
                    size = 0.25, linewidth = 0.5) +
    facet_wrap(~ outcome, scales = "free_y") +
    scale_x_continuous(breaks = seq(-h, h, 1)) +
    scale_colour_manual(values = PROTO_COL) +
    (if (!is.null(pt_lab))
       geom_text(data = pt_lab, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
                 hjust = -0.03, vjust = 1.2, size = 3, lineheight = 0.95, colour = "grey20")) +
    labs(title = sprintf("%s: %s", FIG_TITLE[[fam]], if (is.na(ARM_TITLE[ar])) ar else ARM_TITLE[[ar]]),
         x = "Quarters relative to award", y = "Effect (treated - control)", colour = NULL) +
    theme_light(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  fig <- sprintf("estudy_h%d_%s_%s.png", h, ar, fam)
  ggsave(file.path(P$figures, fig), p, width = 11, height = 5.5, dpi = 150)
  cat(sprintf("  -> %s\n", fig))
}

# ---- write ------------------------------------------------------------------------------------------------
match_rule_banner("write")
sweeps <- rbindlist(all_sweeps, use.names = TRUE)
ps_sweeps <- rbindlist(all_ps_sweeps, use.names = TRUE)   # empty if no pscore cell
print(summ)
if (nrow(pt_tests)) { cat("\n  pre-trend tests (unmatched pre-period, jointly 0):\n"); print(pt_tests) }
write_tab(coefs,  P$coefs)
write_tab(sweeps, P$sweep)
# The estimation samples, stacked and long in (scoring_protocol, arm). This is the artefact to reach for
# when you want to re-plot, re-weight, or re-estimate by hand: load_estimation_panel() reads it back.
if (length(est_panels)) {
  # The last window's full panel is not read again, and this stacking is the run's largest copy: free it
  # first (several pscore protocols push the panel past 30M rows -- see 3_build_reg_data.R)
  if (exists("d", inherits = FALSE)) rm(d)
  invisible(gc())
  ep <- rbindlist(est_panels, use.names = TRUE, fill = TRUE)
  rm(est_panels); invisible(gc())
  write_tab(ep, P$est_panel)
  cat(sprintf("  estimation panel: %d rows | %d cells -> %s\n",
              nrow(ep), uniqueN(ep[, paste(scoring_protocol, arm)]), basename(P$est_panel)))
}
write_obj(list(run_at = Sys.time(), qscore_bound = QBOUND, qscore_bound_log = QBOUND_LOG,
               ps_topk = meta$ps_topk, ps_caliper = PS_CALIPER, ps_trim = PS_TRIM, summary = summ, coefs = coefs,
               pretrend_tests = pt_tests, sweep = sweeps, ps_caliper_sweep = ps_sweeps,
               models = if (SAVE_MODELS) all_models else NULL), P$estimates)
cat(sprintf("  -> %s | %s | %s | %s/\n", basename(P$coefs), basename(P$estimates),
            basename(P$sweep), basename(P$figures)))
cat("\nSTAGE 4 complete.\n")
