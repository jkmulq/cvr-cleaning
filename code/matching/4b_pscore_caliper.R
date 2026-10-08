# =====================================================================================================
# 4b_pscore_caliper.R -- OPTIONAL, after stage 4: the pscore event study at several calipers, side by side
# -----------------------------------------------------------------------------------------------------
# The pscore protocol keeps the top K candidates of every event, however weak the best of them is. This
# script drops the control firm-stacks whose propensity index (control_pscore) is below a caliper, for
# several calipers, and re-fits stage 4's event study at each, so their effect can be read off one figure.
# Stage 4 itself fits one caliper (MATCH_PS_CALIPER) and draws the stacks kept at every cutoff; this is the
# companion for comparing fits.
#
# Reading the index: it has no intercept, so it is log odds against a baseline firm -- index 0 = outside
# the winner's 2-digit industry, ApS, outside the buyers' kommuner and the winner's, with the winner's
# average size over the pre-period.
# A control at index c is e^c times as likely to have bid as that firm. A stack that loses every control
# is dropped; a stack that keeps some has its control side re-weighted to sum to 1, as stage 4 weights.
#
# pscore_age (the opt-in no-FTE model) works the same way, with MATCH_PS_PROTOCOL=pscore_age: its baseline
# firm has the winner's age instead of its size, and its own coefficients, so read its own sweep.
#
# Input:   04_estimation_panel.parquet (the rows stage 4 fitted, via load_estimation_panel()). Stage 4 must
#          have run WITHOUT a caliper, or the cuts here would compound on its cut. Its bottom-share trim
#          (MATCH_PS_TRIM, on by default) is already in those rows: the calipers here cut on top of it,
#          and "none" reproduces stage 4.
# Output:  printed fits and tables, and 04_figures/<protocol>_caliper_h{h}.png. No data file is written.
# Options: MATCH_PS_CALIPERS  minimum indices, space-separated; "none" = no caliper (default "none 6").
#                             6 is only an example: read <protocol>_caliper_stacks_h{h}.png to choose.
#          MATCH_PS_PROTOCOL  the pscore protocol to cut: pscore (default), pscore_fte, pscore_age or pscore_nofte
#          MATCH_TEST_N       reads the tagged run, as every stage does
# Run from the repo root, after stage 4:  Rscript code/matching/4b_pscore_caliper.R 2>&1 | tee /tmp/s4b.log
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))
match_setup(extra_libs = c("fixest", "ggplot2"))

TOKENS   <- strsplit(trimws(match_env_chr("MATCH_PS_CALIPERS", "none 6")), "[ ,]+")[[1]]
CALIPERS <- suppressWarnings(ifelse(TOKENS == "none", NA_real_, as.numeric(TOKENS)))   # NA = no caliper
if (!length(TOKENS) || any(is.na(CALIPERS) & TOKENS != "none"))
  stop("MATCH_PS_CALIPERS must be minimum indices and/or \"none\", e.g. \"none 5.5 6 6.5\"", call. = FALSE)
PROTO <- match_env_chr("MATCH_PS_PROTOCOL", "pscore")
if (!PROTO %in% pscore_protocols())
  stop("MATCH_PS_PROTOCOL must be a pscore protocol: ", paste(pscore_protocols(), collapse = ", "), call. = FALSE)
P  <- match_paths()
h  <- read_obj(P$reg_meta)$h
KS <- setdiff(setdiff(seq.int(-h, -1L), MATCH_PROTOCOLS[[PROTO]]$rank(h)), -1L)   # stage 4's pre-trend quarters

d <- load_estimation_panel(protocol = PROTO)
if (!nrow(d)) stop(sprintf("no %s rows in the estimation panel: run stage 2 with %s, then stages 3-4", PROTO, PROTO),
                   call. = FALSE)
cal4 <- read_obj(P$estimates)$ps_caliper
if (!is.null(cal4) && !is.na(cal4))
  stop(sprintf(paste("stage 4 already applied a pscore caliper (MATCH_PS_CALIPER = %.2f), so its estimation panel",
                     "is cut. Re-run stage 4 without MATCH_PS_CALIPER first."), cal4), call. = FALSE)
trim4 <- read_obj(P$estimates)$ps_trim
cat(sprintf("STAGE 4b | window=+/-%d | %s rows %d | calipers (minimum index): %s | stage 4 trim: %s\n",
            h, PROTO, nrow(d), paste(TOKENS, collapse = ", "),
            if (is.null(trim4) || !trim4 > 0) "none" else sprintf("bottom %.0f%% already dropped", 100 * trim4)))

# One caliper in one arm: drop the control firm-stacks below the minimum index, then the stacks left with
# no control, then re-weight each side to 1. bound = NA keeps everything (stage 4's own fit).
apply_caliper <- function(x, bound) {
  if (!is.na(bound)) x <- x[treated == 1L | control_pscore >= bound]
  x[, has_c := any(treated == 0L), by = stack_id]
  x <- x[has_c == TRUE]
  x[, has_c := NULL]
  x[, weight := 1 / uniqueN(cvr), by = .(stack_id, treated)]
  x[]
}

# Same as 4_run_regressions.R: H0 every beta_k in ks is zero, chi-squared on the clustered vcov.
pretrend_test <- function(m, ks) {
  nm <- sprintf("event_time::%d:treated", ks)
  stopifnot(all(nm %in% names(coef(m))))
  b <- coef(m)[nm]
  V <- vcov(m)[nm, nm, drop = FALSE]
  W <- drop(t(b) %*% solve(V) %*% b)
  data.table(k_tested = paste(ks, collapse = ","), chisq = W, df = length(ks),
             p = pchisq(W, df = length(ks), lower.tail = FALSE))
}

coefs <- list(); tests <- list(); scope <- list()
for (ar in sort(unique(d$arm))) for (j in seq_along(CALIPERS)) {
  b   <- CALIPERS[j]
  x   <- apply_caliper(d[arm == ar], b)
  lab <- if (is.na(b)) "no caliper (stage 4)" else sprintf("index >= %.2f", b)
  key <- paste(ar, j)
  scope[[key]] <- data.table(
    arm = ar, caliper = lab, odds_vs_baseline = if (is.na(b)) NA_real_ else exp(b), stacks = uniqueN(x$stack_id),
    control_firm_stacks = x[treated == 0L & event_time == -1L, .N], control_firms = x[treated == 0L, uniqueN(cvr)])
  if (!nrow(x)) { cat(sprintf("\n  %s | %s: nothing survives -- skipped\n", ar, lab)); next }

  # stage 4's checks: still balanced, and weights sum to 1 per (stack, side) in every period
  stopifnot(x[, .N, by = .(stack_id, cvr)][, all(N == 2L * h + 1L)],
            x[, .(w = sum(weight)), by = .(stack_id, treated, event_time)][, all(abs(w - 1) < 1e-9)])

  # stage 4's specification
  est <- feols(c(fte, log(fte)) ~ i(event_time, treated, ref = -1) | treated + event_time,
               data = x, cluster = ~ cvr, weights = ~ weight, fixef.rm = "none")
  cat(sprintf("\n  %s | %s: %d stacks\n", ar, lab, uniqueN(x$stack_id)))
  print(etable(est))

  for (m in seq_along(est)) {
    oc <- c("FTE (level)", "log FTE")[m]
    ct <- as.data.table(coeftable(est[[m]]), keep.rownames = "term")[grepl("event_time", term)]
    ct[, event_time := as.integer(gsub(".*event_time::(-?[0-9]+).*", "\\1", term))]
    ct <- rbind(ct[, .(event_time, est = Estimate, se = `Std. Error`)], data.table(event_time = -1L, est = 0, se = 0))
    coefs[[paste(key, m)]] <- ct[, `:=`(arm = ar, caliper = lab, outcome = oc)]
    tests[[paste(key, m)]] <- pretrend_test(est[[m]], KS)[, `:=`(arm = ar, caliper = lab, outcome = oc)]
  }
}
scope <- rbindlist(scope); tests <- rbindlist(tests); coefs <- rbindlist(coefs)
setcolorder(tests, c("arm", "caliper", "outcome"))

cat("\n  what each caliper keeps (odds_vs_baseline = e^caliper):\n"); print(scope)
cat("\n  pre-trend tests (the pre-period quarters the score did not read, jointly 0):\n"); print(tests)

# One figure: a column per arm, a row per outcome, a line per caliper
coefs[, caliper := factor(caliper, levels = unique(scope$caliper))]
p <- ggplot(coefs, aes(event_time, est, colour = caliper)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  position = position_dodge(width = 0.5), size = 0.2) +
  geom_line(position = position_dodge(width = 0.5)) +
  facet_grid(outcome ~ arm, scales = "free_y") +
  scale_x_continuous(breaks = seq(-h, h, 2)) +
  labs(title = sprintf("%s event study by caliper on the propensity index (+/-%d quarters)", PROTO, h),
       x = "Event time (t = 0 is the award quarter)", y = "Winner minus controls", colour = NULL) +
  theme_light(base_size = 11) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
f <- file.path(P$figures, sprintf("%s_caliper_h%d.png", PROTO, h))
ggsave(f, p, width = 10, height = 7, dpi = 150)
cat(sprintf("\n  -> %s\n", basename(f)))
cat("\nSTAGE 4b complete.\n")
