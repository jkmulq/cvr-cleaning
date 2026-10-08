#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 5 (a BRANCH off stage 1, not a fifth step) -- one stack per TED contract notice, holding the
# winner, its synthetic matched controls, and its REAL losing co-bidders, with employment for all three.
#
# THE QUESTION. The matched design says "here is a firm that looked like the winner did". The co-bidder
# design says "here is a firm that actually bid and lost". This builds both for the SAME lot, scores
# every firm on the SAME qscore, and emits one regression-ready panel, so the two control groups can be
# compared without joining two separately-built datasets.
#
# Reads:
#   01_firm_panel.parquet        the firm-quarter panel (stage 1)
#   01_events.rds                the FULL competitive award history -- the winner buffer only, never
#                                the study set
#   01_eligible_controls.rds     the screened never-winner pool
#   <clean>/tender_data_2006_2026.*   the combined dataset, for the TED winner/non-winner lots
# Writes (h is MATCH_H, so windows never overwrite one another):
#   05_cobidder_roster_h{h}.rds       one row per (event, firm): qscore, eligibility verdict, flags;
#                                     plus `event_fate`, one row per event: the stage and reason it
#                                     was dropped (or "kept"), summing to every event built
#   05_cobidder_panel_h{h}.parquet    the regression-ready panel: one row per (event, firm, quarter)
#
# SECOND ENTRY POINT, DELIBERATELY. The pipeline's rule is that only stage 1 opens external inputs.
# This breaks it to stay standalone, which is the point: the lot universe it needs is a TED-specific
# slice nothing else wants. It writes no stage-1 artefact and nothing downstream reads it.
#
# THE EVENT IS THE (LOT, WINNER) PAIR, not (firm, quarter), so NOTHING is lost to the "earliest award
# of the quarter" collapse the main pipeline applies. A framework lot with three winners gives three
# events; a winner with four lots in one quarter appears four times, once per notice. The cost is that
# those four stacks share one employment path and therefore select the same controls -- which is why
# any estimation on this panel must cluster on the firm as well as the event.
#
# ONE CONTROL POOL, NOT TWO ARMS. The main pipeline cascades the never-winner and sometimes-winner
# pools separately. Here they are UNIONED into a single candidate set, because the interesting
# possibility is that the matcher picks the REAL losing bidder as its own lot's synthetic control --
# and a real loser that has won competitively somewhere else would be invisible if only the
# never-winner pool were searched. The sometimes-winner half still has to clear MATCH_WINNER_BUFFER, or
# a "control" could be treated inside the very window it is a control for. Which half each selected
# control came from is recorded in `pool_side`, so the union can always be split after the fact.
#
# THE MATCHER IS UNCHANGED IN SUBSTANCE. match_one_lot() is a copy of match_one_event() from
# 2_match_controls.R with the protocol loop removed (full_full only: elig = rank = [-h,-1]) and the arm
# loop replaced by the single union pool above. Same cascade, same fte > 0 eligibility, same post-
# coverage test, same qscore, same keep-all-ties rule. Duplicated rather than shared for now: if the
# two ever disagree that is a bug, so diff them before trusting a surprising result.
#
# EVERY REAL LOSER IS ALSO SCORED AND SCREENED. qscore_table() is the one implementation of the score,
# used for synthetic controls and real losers alike, so "the loser scored 0.42, the synthetic control
# scored 0.11" is a like-for-like sentence. And because synthetic controls pass every eligibility test
# BY CONSTRUCTION, each real loser carries the same tests separately (pool_side, buffer_ok, elig_pre,
# elig_post) -- otherwise the comparison is a screened set against an unscreened one.
#
# BOTH WINDOWS BY DEFAULT, h IN THE FILENAME. With MATCH_H unset, the script builds every window in
# MATCH_WINDOWS (default "8 4") by re-launching itself once per window as a separate Rscript process.
# Each window is still an independent build -- a failure in one does not lose the others, and nothing
# is shared between them. Set MATCH_H to build a single window, exactly as before:
#
#   MATCH_OVERWRITE=1 [MATCH_SCORE=log] Rscript code/matching/5_cobidder_stacks.R   # h = 8 and 4
#   MATCH_OVERWRITE=1 MATCH_H=4 Rscript code/matching/5_cobidder_stacks.R          # h = 4 only
#
# An h=4 design must be BUILT at h=4, never trimmed from an h=8 panel -- the same point
# 3_build_reg_data.R makes at its head. At h=4 the pre-window is [-4,-1], so the eligibility screen
# admits firms that cannot supply eight clean quarters, the candidate pool is larger, the qscores are
# different numbers and DIFFERENT CONTROLS WIN. Filtering an h=8 panel to k in [-4,4] keeps the h=8
# controls and answers a different question.
#
#   Rscript code/matching/5_cobidder_stacks.R
# Options (env):
#   MATCH_H              build ONE window of this many quarters. Unset = build all of MATCH_WINDOWS
#   MATCH_WINDOWS        windows built when MATCH_H is unset (default "8 4", what the report expects)
#   MATCH_MIN_RUNG       eligible candidates a rung needs to fire (default 10)
#   MATCH_WINNER_BUFFER  quarters either side in which a sometimes-winner control may not have an
#                        award of its own (default 12)
#   MATCH_MIN_FTE        strict lower bound on FTE in the panel (default 0)
#   MATCH_SCORE          level (default) | log -- the qscore, as in MATCH_PROTOCOLS (0_matching_utils.R).
#                        log writes 05_cobidder_*_h{h}_log.*, so it never overwrites the level run.
#                        | pscore -- the propensity algorithm instead of the cascade: every eligible
#                        candidate in the union pool ranked by the bidding-propensity index against the
#                        lot's winner and buyers, the top MATCH_PS_TOPK kept (ties kept). The lot's own
#                        losers may be picked (in_both_roles says so); its buyers may not. Real losers get
#                        the same index and pscore_rank = where they would have ranked among the
#                        candidates. Needs 01b_pscore_model.rds (1b_fit_pscore.R) and 01_buyers.rds.
#                        Writes 05_cobidder_*_h{h}_pscore.*
#                        | pscore_fte -- the same on the opt-in model with FTE at t-2 and growth t-6 -> t-2.
#                        Needs 01b_pscore_fte_model.rds. Writes 05_cobidder_*_h{h}_pscore_fte.*
#                        | pscore_age -- the same on the opt-in no-FTE model (firm age). Needs
#                        01b_pscore_age_model.rds (MATCH_PS_VARIANT=pscore_age in 1b). Writes
#                        05_cobidder_*_h{h}_pscore_age.*
#                        | pscore_nofte -- the same on the opt-in model with neither FTE nor age (0/1
#                        characteristics only; ties at the K-th drawn at random, MATCH_PS_SEED + ev). Needs
#                        01b_pscore_nofte_model.rds. Writes 05_cobidder_*_h{h}_pscore_nofte.*
#   MATCH_PS_TOPK        controls kept per lot under MATCH_SCORE=pscore* (default 5)
#   MATCH_PS_SEED        seed for the random tie-break of pscore_nofte (default 20261005; use stage 2's)
#   MATCH_IND_DIGITS     industry FE granularity (default 2 = division)
#   MATCH_KOMMUNE_COL    the cascade's kommune: kommune_code (default, contemporaneous) or hq_kommune_code
#                        (current head office, as before 6 Oct 2026). Use the same value as stage 2.
#   MATCH_TENDER_FILE    override the combined dataset stem
#   MATCH_TEST_N, MATCH_OVERWRITE   as elsewhere; the tag suffixes both outputs
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))

# ---- 0. driver: no MATCH_H means build every window, one child process each ----------------------------
# A child inherits this environment (MATCH_SCORE, MATCH_OVERWRITE, ...) plus its own MATCH_H, so it
# takes the single-window path below. Children run in order; all are attempted, then any failure stops.
if (!nzchar(Sys.getenv("MATCH_H", ""))) {
  # quit() below would close an interactive session, so the driver is Rscript-only.
  if (interactive()) stop("set MATCH_H (e.g. Sys.setenv(MATCH_H = 8)) to run one window interactively",
                          call. = FALSE)
  wins   <- as.integer(match_env_list("MATCH_WINDOWS", "8 4"))
  if (!length(wins) || anyNA(wins)) stop("MATCH_WINDOWS must be integers, e.g. \"8 4\"", call. = FALSE)
  script <- file.path(getwd(), "code", "matching", "5_cobidder_stacks.R")
  status <- vapply(wins, function(h) {
    cat(sprintf("\n========== STAGE 5 driver: building h = %d ==========\n", h))
    system2(file.path(R.home("bin"), "Rscript"), shQuote(script), env = sprintf("MATCH_H=%d", h))
  }, integer(1))
  if (any(status != 0L))
    stop(sprintf("stage 5 failed for h = %s", paste(wins[status != 0L], collapse = ", ")), call. = FALSE)
  cat(sprintf("\nSTAGE 5 driver complete: h = %s\n", paste(wins, collapse = ", ")))
  quit(save = "no", status = 0L)
}

match_setup()

H          <- match_env_int("MATCH_H", 8L)
SCORE      <- match_env_chr("MATCH_SCORE", "level")
if (!SCORE %in% c("level", "log", pscore_protocols()))
  stop("MATCH_SCORE must be level, log, ", paste(pscore_protocols(), collapse = " or "), ", not ", SCORE, call. = FALSE)
PROTOCOL   <- switch(SCORE, level = "full_full", log = "full_full_log", SCORE)   # MATCH_PROTOCOLS name
IS_PS      <- MATCH_PROTOCOLS[[PROTOCOL]]$score == "pscore"     # the propensity algorithm, on either model
PS_V       <- if (IS_PS) MATCH_PROTOCOLS[[PROTOCOL]]$variant else NA_character_   # its model (PSCORE_VARIANTS)
PS_AGE     <- IS_PS && "a2" %in% ps_variant(PS_V)$needs
PS_AVG     <- IS_PS && "lbar" %in% ps_variant(PS_V)$needs     # pre-period average FTE (pscore)
PS_K       <- match_env_int("MATCH_PS_TOPK", 5L)
PS_SEED    <- match_env_int("MATCH_PS_SEED", 20261005L)   # per-event seed (+ ev) where a model breaks ties at random
if (IS_PS && (is.na(PS_K) || PS_K < 1L)) stop("MATCH_PS_TOPK must be a positive integer", call. = FALSE)
MIN_RUNG   <- match_env_int("MATCH_MIN_RUNG", 10L)
WBUF       <- match_env_int("MATCH_WINNER_BUFFER", 12L)
MIN_FTE    <- match_env_num("MATCH_MIN_FTE", 0)
IND_DIGITS <- match_env_int("MATCH_IND_DIGITS", 2L)
KCOL       <- match_kommune_col()   # the cascade's kommune, as in stage 2 (contemporaneous unless set)
P          <- match_paths()

OFFSETS <- seq.int(-H, -1L)          # full_full: the eligibility and ranking windows are the same
RUNGS   <- MATCH_RULES[["staggered_industry_kommune"]]()
NP      <- 2L * H + 1L               # rows a balanced firm must have

if (WBUF < H) stop(sprintf("MATCH_WINNER_BUFFER=%d is narrower than h=%d", WBUF, H), call. = FALSE)
cat(sprintf("STAGE 5 | h=%d | score=%s | min_rung=%d | winner_buffer=%d | min_fte=%s | pool=union | kommune=%s | protocol=%s%s\n",
            H, SCORE, MIN_RUNG, WBUF, MIN_FTE, KCOL, PROTOCOL,
            if (IS_PS) sprintf(" | top %d, no cascade", PS_K) else ""))

# ---- 1. inputs -----------------------------------------------------------------------------------------
match_rule_banner("1. inputs")
panel    <- read_tab(P$firm_panel)
hist_ev  <- as.data.table(read_obj(P$events))
eligible <- as.data.table(read_obj(P$eligible))
cat(sprintf("  firm panel: %d rows | %d firms\n", nrow(panel), uniqueN(panel$cvr)))
if (IS_PS) {
  for (cc in c("legal_form_short", "kommune_code", if (PS_AGE) "registration_date"))
    if (!cc %in% names(panel)) stop("the firm panel has no ", cc, " -- re-run stage 1", call. = FALSE)
  PSM <- read_pscore_model(P$ps_model_for(PS_V), PS_V)
  BUY <- read_obj(P$buyers)
  cat(sprintf("  %s model: fitted %s | %d strata | concordance %.3f\n",
              PS_V, format(PSM$run_at), PSM$n_strata, PSM$concordance))
}
# pscore_age's firm age: firm-level founding dates, for the candidates and the real losers alike
FOUNDED <- if (PS_AGE) unique(panel[!is.na(registration_date), .(cvr, registration_date)]) else NULL

# Every competitive award quarter of every competitive winner. From the FULL history, not this
# script's lot universe: a firm that won a KFST lot in the same quarter must still be barred, even
# though that award is not a TED co-bidder lot.
award_idx  <- unique(hist_ev[, .(cvr, event_qidx)])
all_comp_w <- unique(hist_ev$cvr)
cat(sprintf("  award history: %d (firm, quarter) awards | %d competitive winners\n",
            nrow(award_idx), length(all_comp_w)))

# ---- 2. the TED lot universe ---------------------------------------------------------------------------
# Filters copied from 13_estudy_winner_vs_nonwinner_matched.Rmd so both designs cover the SAME lots.
# Change one and you must change the other, or the comparison stops being like-for-like.
match_rule_banner("2. TED winner / non-winner lots")
cmb <- as.data.table(read_clean(match_env_chr("MATCH_TENDER_FILE",
                                              file.path(dirs$clean_data, "tender_data_2006_2026"))))
wn <- cmb[data_source == "TED" & entity %chin% c("winner", "non-winner") &
            !is.na(cvr_final) & cvr_final != "" &
            !is.na(tender_id) & tender_id != "" & !is.na(lot_id) & lot_id != ""]
rm(cmb); invisible(gc())

wn[, cvr := as_cvr8(cvr_final)]
wn <- wn[!is.na(cvr)]
wn[, award_date := as.Date(award_date)]
# The event date comes from the WINNER, so only winners need a dated award; non-winners inherit the
# lot's date, which keeps more of them.
wn <- wn[entity == "non-winner" | !is.na(award_date)]
# A firm that won a lot is not one of its own non-winning bidders: drop only that spurious row.
wn[, is_dual := ("winner" %in% entity) & ("non-winner" %in% entity), by = .(tender_id, lot_id, cvr)]
wn <- wn[!(entity == "non-winner" & is_dual == TRUE)]
# Keep only lots with both sides -- with no real loser there is nothing to compare against.
wn[, has_both := ("winner" %in% entity) & ("non-winner" %in% entity), by = .(tender_id, lot_id)]
wn <- wn[has_both == TRUE]
if (!nrow(wn)) stop("no TED lot has both a winner and a non-winner", call. = FALSE)

if (!"ted_notice_id" %in% names(wn)) wn[, ted_notice_id := NA_character_]
wn[, lot_key := paste("TED", tender_id, lot_id, sep = "|")]
# One award quarter per LOT, from the earliest winner award date on it. Deliberately the lot's date
# rather than each winner's own, so the winner, its controls and its real co-bidders all sit at the
# same t = 0 -- and so this matches event_tidx in the co-bidder .Rmd exactly.
wn[, wdate := min(award_date[entity == "winner"]), by = lot_key]
wn[, event_qidx := qidx_of(year(wdate), quarter(wdate))]

events <- unique(wn[entity == "winner", .(lot_key, tender_id, lot_id, ted_notice_id,
                                          winner_cvr = cvr, wdate, event_qidx)])
setorder(events, lot_key, winner_cvr)
events[, ev := .I]
losers <- unique(wn[entity == "non-winner", .(lot_key, cvr)])
setkey(losers, lot_key)

cat(sprintf("  lots with both sides: %d | events (lot x winner): %d | real losing bids: %d\n",
            uniqueN(wn$lot_key), nrow(events), nrow(losers)))
# pscore: each event's buyers and their kommune at t - 2 (keyed on ev). Buyers are tender-level.
if (IS_PS) {
  EV_BUY <- buyer_kommunes(events[, .(key = ev, data_source = "TED", tender_id = as.character(tender_id),
                                      q = event_qidx - PSCORE_LAGS[1])], BUY)
  setkey(EV_BUY, key)
  cat(sprintf("  events with a known buyer: %d of %d (%d with a buyer kommune)\n",
              uniqueN(EV_BUY$key), nrow(events), uniqueN(EV_BUY[!is.na(buyer_kommune), key])))
}
cat(sprintf("  distinct winners %d | distinct losers %d | award quarters %d\n",
            uniqueN(events$winner_cvr), uniqueN(losers$cvr), uniqueN(events$event_qidx)))

# ---- 3. the candidate pool -----------------------------------------------------------------------------
# Same construction as 2_match_controls.R, but the two pools sit in ONE table and are unioned at match
# time. Attributes stay row-level (not one row per firm) because the matcher compares the candidate's
# industry/kommune AS OBSERVED IN THE PRE-WINDOW.
match_rule_banner("3. candidate pool")
elig_cvrs <- eligible[has_employment == TRUE, unique(cvr)]
POOL_COLS <- c("cvr", "qidx", "fte", "firm_type",
               "industry_code6", "industry_class", "industry_group", "industry_division", KCOL)
if (IS_PS) POOL_COLS <- unique(c(POOL_COLS, "legal_form_short", "kommune_code"))   # the characteristics
missing_cols <- setdiff(POOL_COLS, names(panel))
if (length(missing_cols)) stop("panel is missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)

pool <- panel[, ..POOL_COLS]
pool[, pool_side := fifelse(firm_type == "winner", "winner",
                     fifelse(cvr %chin% elig_cvrs, "never_winner", NA_character_))]
pool <- pool[!is.na(pool_side)]
setkey(pool, qidx)
cat(sprintf("  pool: %d rows | never_winner %d firms | sometimes_winner %d firms\n", nrow(pool),
            pool[pool_side == "never_winner", uniqueN(cvr)], pool[pool_side == "winner", uniqueN(cvr)]))

firm_side <- unique(pool[, .(cvr, pool_side)])   # firm-level lookup, so the event loop never rescans

# Real losers are scored from the FULL panel, not from `pool`: a losing bidder outside the registry
# screen, or with no employment anywhere, is still a real bidder we want a score and a verdict for.
# Firms absent from the panel entirely cannot be scored -- they stay in the roster with
# in_panel = FALSE rather than vanishing, so the loss is counted instead of silent.
LP_COLS <- c("cvr", "qidx", "fte",
             if (IS_PS) c("industry_division", "industry_class", "legal_form_short", "kommune_code"))
loser_panel <- panel[cvr %chin% unique(losers$cvr), ..LP_COLS]
setkey(loser_panel, qidx)
in_panel_cvrs <- unique(loser_panel$cvr)
cat(sprintf("  real losers present in the firm panel: %d of %d (%.1f%%)\n",
            length(intersect(unique(losers$cvr), in_panel_cvrs)), uniqueN(losers$cvr),
            100 * length(intersect(unique(losers$cvr), in_panel_cvrs)) / uniqueN(losers$cvr)))

# ---- 4. the shared score -------------------------------------------------------------------------------
# THE ONE implementation of the qscore, used for synthetic controls AND real losers. Identical
# arithmetic to 2_match_controls.R, under either MATCH_SCORE:
#   level  squared FTE gap per scored quarter, root-mean, divided by the treated firm's mean FTE
#   log    squared log-FTE gap per scored quarter, root-mean
# Both scale-free, so comparable across events. Under log, quarters where either firm has FTE <= 0
# have no log and are dropped from scoring -- visible in n_scored, exactly as NA quarters are. That
# can only bite for real losers: match_one_lot() discards a treated firm with a non-positive scored
# quarter, and eligible candidates are positive on every one.
# args:  cand = (cvr, et, fte) candidate rows over the ranked quarters, NA fte already removed
#        tv   = (et, fte_t), exactly one row per ranked quarter of the treated firm
# returns: one row per cvr. n_scored is how many quarters contributed: always h for an eligible
#          control (no gaps, by definition), possibly fewer for a real loser -- which is precisely the
#          asymmetry the eligibility flags make explicit.
qscore_table <- function(cand, tv) {
  if (!nrow(cand)) return(data.table(cvr = character(), mean_fte_diff_sq = numeric(),
                                     mean_fte_treated = numeric(), n_scored = integer(),
                                     qscore = numeric()))
  x <- merge(cand, tv, by = "et", allow.cartesian = TRUE)
  if (SCORE == "log") {
    x <- x[fte > 0 & fte_t > 0]
    if (!nrow(x)) return(qscore_table(cand[0], tv))
    s <- x[, .(mean_fte_diff_sq = mean((fte - fte_t)^2),
               mean_fte_treated = mean(fte_t),
               mean_log_diff_sq = mean((log(fte) - log(fte_t))^2),
               n_scored         = .N), by = cvr]
    s[, qscore := fifelse(is.finite(mean_log_diff_sq), sqrt(mean_log_diff_sq), NA_real_)]
    s[, mean_log_diff_sq := NULL]
    return(s[])
  }
  s <- x[, .(mean_fte_diff_sq = mean((fte - fte_t)^2, na.rm = TRUE),
             mean_fte_treated = mean(fte_t, na.rm = TRUE),
             n_scored         = .N), by = cvr]
  # mean_fte_treated > 0 is a DENOMINATOR guard, not a screen: a treated firm averaging zero FTE over
  # the scored quarters has no scale to normalise by, so the score is undefined rather than enormous.
  s[, qscore := fifelse(is.finite(mean_fte_diff_sq) & is.finite(mean_fte_treated) & mean_fte_treated > 0,
                        sqrt(mean_fte_diff_sq) / mean_fte_treated, NA_real_)]
  s[]
}

# ---- 5a. the pscore algorithm (MATCH_SCORE=pscore) -------------------------------------------------------
# A copy of stage 2's ps_group()/ps_select() with the union pool, kept duplicated like the rest of the
# matcher -- diff them against 2_match_controls.R before trusting a surprising result.
# The event-independent part, once per quarter group: the union pool's eligible firms (positive FTE in
# every pre quarter, >= h post quarters; sometimes-winners clear of the buffer) and every pool firm's
# characteristics at Q-2, read from `pool` by key because Q-6 is outside the slice when h < 6.
ps_group5 <- function(Q, pre, win, keep_w) {
  ok   <- pre[!is.na(fte) & fte > 0, .N, by = cvr][N == H, cvr]
  ok   <- win[cvr %chin% ok & et >= 1L & et <= H, uniqueN(et), by = cvr][V1 >= H, cvr]
  elig <- unique(pre[cvr %chin% ok, .(cvr, pool_side)])
  elig <- elig[pool_side == "never_winner" | (pool_side == "winner" & cvr %chin% keep_w)]
  f2 <- pool[.(Q - PSCORE_LAGS[1]), .(cvr, fte2 = fte, industry_division, industry_class,
                                      legal_form_short, kommune_code), nomatch = 0L]
  f6 <- pool[.(Q - PSCORE_LAGS[2]), .(cvr, fte6 = fte), nomatch = 0L]
  fr <- merge(f2, f6, by = "cvr", all.x = TRUE)
  if (PS_AGE) pscore_add_age(fr, FOUNDED, Q - PSCORE_LAGS[1])
  if (PS_AVG) pscore_add_avg(fr, pool, Q)
  fv <- pscore_firm_vars(fr)
  list(elig = elig, cand = merge(elig, fv[ps_scorable(fv, PS_V)], by = "cvr"), fv = fv)
}
# One lot's pscore controls: every eligible candidate except the winner and the tender's buyers -- the
# lot's own losers INCLUDED -- ranked by the index; the top PS_K kept, ties at the K-th kept.
# returns: list(rows = treated + controls, scores = every ranked candidate's index, sorted -- for ranking
#          the real losers) or list(discard = "<reason>")
ps_select_lot <- function(e, psg, n_pre) {
  W <- e$winner_cvr
  w <- psg$fv[cvr == W]
  if (!ps_winner_ok(w, PS_V))
    return(list(discard = sprintf("treated: %s (%s)", ps_variant(PS_V)$w_skip, PROTOCOL)))
  n_elig <- psg$elig[cvr != W, .N]
  b <- EV_BUY[.(e$ev), nomatch = 0L]
  x <- psg$cand[cvr != W & !(cvr %chin% b$buyer_cvr)]
  if (!nrow(x)) return(list(discard = sprintf("no scorable eligible candidate (%s)", PROTOCOL)))
  pscore_features(x, w, b$buyer_kommune, PS_V)
  x[, pscore := pscore_index(x, PSM$coef, PS_V)]
  sel <- x[ps_top_k(x$pscore, PS_K, PS_V, PS_SEED + e$ev)]   # ties kept, or drawn at random (pscore_nofte)
  sel[, pscore_rank := as.integer(frank(-pscore, ties.method = "min"))]
  lab <- MATCH_PROTOCOLS[[PROTOCOL]]$label
  ctrl <- data.table(ev = e$ev, category = "found_control", cvr = sel$cvr,
                     qscore = NA_real_, mean_fte_diff_sq = NA_real_, n_scored = NA_integer_,
                     control_protocol = lab, rung_idx = NA_integer_, n_eligible = n_elig,
                     n_tied = nrow(sel), stack_qscore = NA_real_,
                     pscore = sel$pscore, pscore_rank = sel$pscore_rank, n_ranked = nrow(x))
  # As in stage 2, the treated row carries the stack's attributes; it has no index against itself.
  trt <- data.table(ev = e$ev, category = "treated", cvr = W,
                    qscore = NA_real_, mean_fte_diff_sq = NA_real_, n_scored = NA_integer_,
                    control_protocol = lab, rung_idx = NA_integer_, n_eligible = n_elig,
                    n_tied = nrow(sel), stack_qscore = NA_real_,
                    pscore = NA_real_, pscore_rank = NA_integer_, n_ranked = nrow(x))
  list(rows = rbindlist(list(trt, ctrl), use.names = TRUE), scores = sort(x$pscore))
}

# ---- 5. the matcher (match_one_event(), full_full, one union pool) --------------------------------------
# args:  e = one row of `events`; pre = [-h,-1] rows for every pooled firm in this quarter group;
#        win = the full [-h,h] rows; keep_w = sometimes-winners with no award inside the buffer;
#        psg = ps_group5() for this group (MATCH_SCORE=pscore only).
# returns: list(rows = treated + selected controls) or list(discard = "<reason>").
match_one_lot <- function(e, pre, win, keep_w, psg = NULL) {
  W  <- e$winner_cvr
  tw <- pre[cvr == W]
  if (!nrow(tw))            return(list(discard = "treated absent from the pre-window"))
  if (anyDuplicated(tw$et)) return(list(discard = "treated has duplicate rows at an event time"))
  if (win[cvr == W & et >= 1L & et <= H, uniqueN(et)] < H)
    return(list(discard = "treated lacks h post-event quarters"))

  T_et  <- tw[!is.na(fte), unique(et)]
  n_pre <- length(T_et)
  if (n_pre == 0L) return(list(discard = "treated has no valid pre-period FTE"))
  if (n_pre < H)   return(list(discard = "treated has an incomplete pre-period"))
  # The propensity algorithm replaces everything below (cascade + path score).
  if (IS_PS) return(ps_select_lot(e, psg, n_pre))

  ind_vals <- list(
    industry_code6    = unique(tw$industry_code6[!is.na(tw$industry_code6)]),
    industry_class    = unique(tw$industry_class[!is.na(tw$industry_class)]),
    industry_group    = unique(tw$industry_group[!is.na(tw$industry_group)]),
    industry_division = unique(tw$industry_division[!is.na(tw$industry_division)]))
  komm <- unique(tw[[KCOL]][!is.na(tw[[KCOL]])])   # every kommune the treated firm had in the pre-window
  tv   <- tw[et %in% T_et, .(et, fte_t = fte)]
  # Log score only: a zero quarter has no log, so the treated firm cannot be scored on every quarter.
  # Mirrors stage 2, where the event is unscorable under the log protocols.
  if (SCORE == "log" && any(tv$fte_t <= 0))
    return(list(discard = "treated has non-positive FTE in a scored quarter (log score)"))

  # THE UNION POOL: never-winners, plus sometimes-winners clear of the buffer. One set, one cascade,
  # one score -- so a real losing bidder can win the competition whichever half it sits in.
  cand_all <- pre[pool_side == "never_winner" | (pool_side == "winner" & cvr %chin% keep_w),
                  unique(cvr)]
  cand_all <- setdiff(cand_all, W)                  # a winner cannot be its own control
  if (!length(cand_all)) return(list(discard = "empty candidate pool"))

  # The cascade, fine -> coarse. A rung fires only on >= MIN_RUNG ELIGIBLE candidates; the last rung
  # keeps a floor of 1, because there is nothing coarser to fall through to.
  hit <- NULL
  for (ri in seq_along(RUNGS)) {
    r    <- RUNGS[[ri]]
    vals <- if (is.null(r$col)) NULL else ind_vals[[r$col]]
    if (!is.null(r$col) && !length(vals)) next

    cand <- pre[cvr %chin% cand_all &
                  (if (is.null(r$col)) TRUE else get(r$col) %chin% vals) &
                  (if (r$komm) get(KCOL) %chin% komm else TRUE), unique(cvr)]
    if (!length(cand)) next

    ok <- pre[cvr %chin% cand & et %in% T_et & !is.na(fte) & fte > 0, .N, by = cvr][N == n_pre, unique(cvr)]
    if (!length(ok)) next
    ok <- win[cvr %chin% ok & et >= 1L & et <= H, uniqueN(et), by = cvr][V1 >= H, unique(cvr)]
    if (!length(ok)) next

    floor_ok <- if (ri == length(RUNGS)) length(ok) >= 1L else length(ok) >= MIN_RUNG
    if (floor_ok) { hit <- list(idx = ri, label = r$label, cvrs = ok); break }
  }
  if (is.null(hit)) return(list(discard = "no eligible candidate at any rung"))

  sc <- qscore_table(pre[cvr %chin% hit$cvrs & et %in% T_et, .(cvr, et, fte)], tv)
  sc <- sc[!is.na(qscore)]
  if (!nrow(sc)) return(list(discard = "no scorable candidate"))
  sel <- sc[qscore == min(qscore)]                  # keep ALL ties at rank 1

  ctrl <- data.table(ev = e$ev, category = "found_control", cvr = sel$cvr,
                     qscore = sel$qscore, mean_fte_diff_sq = sel$mean_fte_diff_sq,
                     n_scored = sel$n_scored, control_protocol = hit$label, rung_idx = hit$idx,
                     n_eligible = length(hit$cvrs), n_tied = nrow(sel),
                     stack_qscore = sel$qscore)
  # The treated row carries the STACK's attributes rather than NAs, exactly as 2_match_controls.R
  # does, so a stack-level filter stays one predicate. Its OWN qscore is NA: a firm has no distance to
  # itself, and non_winner rows carry a real per-firm qscore, so the two must not share a column.
  trt <- data.table(ev = e$ev, category = "treated", cvr = W,
                    qscore = NA_real_, mean_fte_diff_sq = NA_real_, n_scored = NA_integer_,
                    control_protocol = hit$label, rung_idx = hit$idx,
                    n_eligible = length(hit$cvrs), n_tied = nrow(sel),
                    stack_qscore = min(sel$qscore))
  list(rows = rbindlist(list(trt, ctrl), use.names = TRUE))
}

# ---- 6. score and screen the real losers ----------------------------------------------------------------
# Same qscore, plus the tests a synthetic control passes BY CONSTRUCTION, each recorded separately so
# the report can say which one bites:
#   in_panel   any employment record for this firm at all
#   pool_side  never_winner / winner / none -- would it have been in the candidate pool
#   buffer_ok  sometimes-winners only: no award of its own inside +/-WBUF of this event
#   elig_pre   positive, non-missing FTE in every one of the treated firm's pre-period quarters
#   elig_post  observed in >= h post-event quarters
# The cascade rung is NOT tested, so `eligible` is a CEILING on being pickable: a firm can clear every
# test here and still never be offered, because the rung that fired was finer than its industry.
# Under MATCH_SCORE=pscore there is no rung: a loser gets the candidates' index (pscore) and pscore_rank,
# where it would have ranked among them (1 = above every candidate), and `eligible` also requires that
# it could be scored. psg, lfv (the losers' values at Q-2) and scores come from the group loop.
score_losers <- function(e, pre, lp_pre, lp_win, keep_w, lot_losers, psg = NULL, lfv = NULL, scores = NULL) {
  if (!length(lot_losers)) return(NULL)
  tw   <- pre[cvr == e$winner_cvr]
  T_et <- tw[!is.na(fte), unique(et)]
  if (!length(T_et)) return(NULL)
  tv    <- tw[et %in% T_et, .(et, fte_t = fte)]
  n_pre <- length(T_et)

  # Under pscore the path qscore is left NA (an empty input): it would be a level score in a run that
  # never used one, and qscore_table() falls through to level for any SCORE other than log.
  s <- qscore_table(if (IS_PS) lp_pre[0L, .(cvr, et, fte)] else
                      lp_pre[cvr %chin% lot_losers & et %in% T_et & !is.na(fte), .(cvr, et, fte)], tv)
  d <- merge(data.table(cvr = lot_losers), s,         by = "cvr", all.x = TRUE)
  d <- merge(d,                            firm_side, by = "cvr", all.x = TRUE)
  d[is.na(pool_side), pool_side := "none"]

  pre_ok  <- lp_pre[cvr %chin% lot_losers & et %in% T_et & !is.na(fte) & fte > 0,
                    .N, by = cvr][N == n_pre, cvr]
  post_ok <- lp_win[cvr %chin% lot_losers & et >= 1L & et <= H,
                    uniqueN(et), by = cvr][V1 >= H, cvr]

  d[, `:=`(ev        = e$ev,
           category  = "non_winner",
           in_panel  = cvr %chin% in_panel_cvrs,
           buffer_ok = fifelse(pool_side == "winner", cvr %chin% keep_w, TRUE),
           elig_pre  = cvr %chin% pre_ok,
           elig_post = cvr %chin% post_ok)]
  d[, eligible := elig_pre & elig_post & pool_side != "none" & buffer_ok]

  if (IS_PS) {
    d[, `:=`(pscore = NA_real_, pscore_rank = NA_integer_)]
    w  <- psg$fv[cvr == e$winner_cvr]          # scorable: match_one_lot() succeeded for this event
    lx <- lfv[cvr %chin% lot_losers & ps_scorable(lfv, PS_V)]
    if (nrow(lx) && nrow(w) == 1L) {
      pscore_features(lx, w, EV_BUY[.(e$ev), nomatch = 0L]$buyer_kommune, PS_V)
      lx[, pscore := pscore_index(lx, PSM$coef, PS_V)]
      lx[, pscore_rank := as.integer(1L + length(scores) - findInterval(pscore, scores))]
      d[lx, on = "cvr", `:=`(pscore = i.pscore, pscore_rank = i.pscore_rank)]
    }
    d[, eligible := eligible & !is.na(pscore)]
  }
  d[]
}

# ---- 7. run over award-quarter groups --------------------------------------------------------------------
# Serial, grouped by event_qidx so the panel is sliced once per quarter rather than once per event.
# Purely a performance device: results are identical event by event.
match_rule_banner("4. matching")
groups <- sort(unique(events$event_qidx))
cat(sprintf("  %d events across %d award quarters\n", nrow(events), length(groups)))

res <- vector("list", length(groups))
dsc <- vector("list", length(groups))
for (gi in seq_along(groups)) {
  Q   <- groups[gi]
  win <- pool[.(seq.int(Q - H, Q + H)), nomatch = 0L]
  if (!nrow(win)) next
  win[, et := qidx - Q]
  pre <- win[et %in% OFFSETS]

  lp_win <- loser_panel[.(seq.int(Q - H, Q + H)), nomatch = 0L]
  lp_win[, et := qidx - Q]
  lp_pre <- lp_win[et %in% OFFSETS]

  in_win <- award_idx[event_qidx %between% c(Q - WBUF, Q + WBUF), unique(cvr)]
  keep_w <- setdiff(all_comp_w, in_win)

  # pscore: the candidate set and characteristics once per group, and the real losers' values at Q-2
  psg <- if (IS_PS) ps_group5(Q, pre, win, keep_w) else NULL
  lfv <- NULL
  if (IS_PS) {
    lfv <- merge(loser_panel[.(Q - PSCORE_LAGS[1]), .(cvr, fte2 = fte, industry_division, industry_class,
                                                      legal_form_short, kommune_code), nomatch = 0L],
                 loser_panel[.(Q - PSCORE_LAGS[2]), .(cvr, fte6 = fte), nomatch = 0L], by = "cvr", all.x = TRUE)
    if (PS_AGE) pscore_add_age(lfv, FOUNDED, Q - PSCORE_LAGS[1])
    if (PS_AVG) pscore_add_avg(lfv, loser_panel, Q)
    pscore_firm_vars(lfv)
  }

  evs <- events[event_qidx == Q]
  rr  <- vector("list", nrow(evs)); dd <- vector("list", nrow(evs))
  for (i in seq_len(nrow(evs))) {
    e <- evs[i]
    m <- match_one_lot(e, pre, win, keep_w, psg)
    if (!is.null(m$discard)) { dd[[i]] <- data.table(ev = e$ev, reason = m$discard); next }
    lot_losers <- losers[.(e$lot_key), unique(cvr), nomatch = 0L]
    nw <- score_losers(e, pre, lp_pre, lp_win, keep_w, lot_losers, psg, lfv, m$scores)
    rr[[i]] <- rbindlist(list(m$rows, nw), use.names = TRUE, fill = TRUE)
  }
  res[[gi]] <- rbindlist(Filter(Negate(is.null), rr), use.names = TRUE, fill = TRUE)
  dsc[[gi]] <- rbindlist(Filter(Negate(is.null), dd), use.names = TRUE, fill = TRUE)
  if (gi %% 20L == 0L) cat(sprintf("    %d/%d quarters\n", gi, length(groups)))
}

roster   <- rbindlist(Filter(Negate(is.null), res), use.names = TRUE, fill = TRUE)
discards <- rbindlist(Filter(Negate(is.null), dsc), use.names = TRUE, fill = TRUE)
if (!nrow(roster)) stop("no event produced a stack", call. = FALSE)

# ---- 8. the roster ----------------------------------------------------------------------------------------
match_rule_banner("5. roster")
roster <- merge(roster, events[, .(ev, lot_key, tender_id, lot_id, ted_notice_id,
                                   winner_cvr, wdate, event_qidx)], by = "ev")
# score_losers() attached pool_side to the losers only; attach it to every row from the same lookup.
if ("pool_side" %in% names(roster)) roster[, pool_side := NULL]
roster <- merge(roster, firm_side, by = "cvr", all.x = TRUE)
roster[is.na(pool_side), pool_side := "none"]
# The pscore columns on every roster, typed NA when this run did not score them, so the panel merge
# below has a single column list whatever MATCH_SCORE was.
if (!"pscore"      %in% names(roster)) roster[, pscore      := NA_real_]
if (!"pscore_rank" %in% names(roster)) roster[, pscore_rank := NA_integer_]
if (!"n_ranked"    %in% names(roster)) roster[, n_ranked    := NA_integer_]

# THE RESULT THIS SCRIPT EXISTS TO SURFACE: a real co-bidder sits in the candidate pool like any other
# firm, so the matcher can select it as its own lot's synthetic control. Flagged on both of its rows.
roster[, in_both_roles := (cvr %chin% cvr[category == "found_control"]) &
                          (cvr %chin% cvr[category == "non_winner"]), by = ev]
# Treated firms and synthetic controls clear every screen by construction -- match_one_lot() discards
# the event otherwise -- so fill the verdicts in. `eligible` then means the same thing for all three
# categories and works as a single predicate.
roster[category %chin% c("treated", "found_control"),
       `:=`(in_panel = TRUE, buffer_ok = TRUE, elig_pre = TRUE, elig_post = TRUE, eligible = TRUE)]

setcolorder(roster, c("ev", "lot_key", "tender_id", "lot_id", "ted_notice_id", "winner_cvr",
                      "event_qidx", "wdate", "category", "cvr", "pool_side", "qscore",
                      "stack_qscore", "eligible", "in_both_roles"))
setorder(roster, ev, category, cvr)

cat("\n  roster by category:\n")
print(roster[, .(rows = .N, firms = uniqueN(cvr), events = uniqueN(ev)), by = category][order(category)])
cat(sprintf("\n  events with a stack: %d of %d | discarded: %d\n",
            uniqueN(roster$ev), nrow(events), nrow(discards)))
if (nrow(discards)) { cat("  discard reasons:\n"); print(discards[, .N, by = reason][order(-N)]) }
cat("\n  selected controls, by which half of the union they came from:\n")
print(roster[category == "found_control", .(controls = .N), by = pool_side][order(-controls)])
cat("\n  real losers -- where the screen bites:\n")
print(roster[category == "non_winner",
             .(losers = .N, in_panel = sum(in_panel), in_a_pool = sum(pool_side != "none"),
               buffer_ok = sum(buffer_ok), pre_ok = sum(elig_pre), post_ok = sum(elig_post),
               eligible = sum(eligible),
               scored = sum(!is.na(if (IS_PS) pscore else qscore)))])
cat(sprintf("\n  firms holding BOTH roles on the same lot: %d (%d roster rows)\n",
            roster[in_both_roles == TRUE, uniqueN(cvr)], roster[in_both_roles == TRUE, .N]))
if (IS_PS) {
  cat(sprintf("\n  where the eligible real losers would have ranked among the candidates (the top %d are picked):\n", PS_K))
  print(roster[category == "non_winner" & eligible == TRUE,
               .(losers = .N, median_rank = as.numeric(median(pscore_rank)),
                 ranked_in_top_k = sum(pscore_rank <= PS_K), picked = sum(in_both_roles))])
}

# ---- 9. the regression-ready panel ---------------------------------------------------------------------
# Same construction as 3_build_reg_data.R, with three categories instead of two arms.
match_rule_banner("6. regression panel")
pan <- panel[, .(cvr, qidx, year, quarter, fte, employees, industry_code6)]
rm(panel); invisible(gc())

d <- merge(roster[, .(ev, lot_key, tender_id, lot_id, ted_notice_id, winner_cvr, event_qidx,
                      category, cvr, pool_side, qscore, stack_qscore, pscore, pscore_rank,
                      eligible, in_both_roles)],
           pan, by = "cvr", allow.cartesian = TRUE)
d[, event_time   := qidx - event_qidx]
d[, industry_grp := substr(industry_code6, 1L, IND_DIGITS)]
# industry_grp enters the fixed effects, so a missing value would be dropped by feols later and
# silently unbalance the sample. Screen it here, with the window, BEFORE counting rows.
d <- d[event_time %between% c(-H, H) & !is.na(industry_grp) & industry_grp != ""]

# Balanced firm: exactly 2h+1 rows AND 2h+1 distinct periods spanning -h..h (the pair catches gaps AND
# duplicates, which a row count alone would not), with FTE above the floor throughout.
d[, `:=`(n_row = .N, n_per = uniqueN(event_time),
         tmin  = min(event_time), tmax = max(event_time),
         n_ok  = sum(!is.na(fte) & fte > MIN_FTE)), by = .(ev, category, cvr)]
n_firm_before <- d[, uniqueN(paste(ev, category, cvr))]
d <- d[n_row == NP & n_per == NP & tmin == -H & tmax == H & n_ok == NP]
if (!nrow(d)) stop("nothing survives balancing at this window", call. = FALSE)
cat(sprintf("  firm-stacks balanced on -%d..%d with fte > %s: %d of %d\n",
            H, H, MIN_FTE, d[, uniqueN(paste(ev, category, cvr))], n_firm_before))

# complete_stack is the set the comparison rests on: an event still holding ALL THREE sides after
# balancing. Flagged, not filtered, so nothing disappears silently -- but estimate on it, because
# balancing each design separately would let the two control groups run on different event sets, and
# the difference between them would then mix control choice with sample composition.
d[, `:=`(has_t = any(category == "treated"),
         has_c = any(category == "found_control"),
         has_n = any(category == "non_winner")), by = ev]
d[, complete_stack := has_t & has_c & has_n]

# EVENT FATE: one row per built event, saying where it left the funnel. First failure wins, so the
# reasons are mutually exclusive and the counts sum to nrow(events). Labels are window-free on purpose,
# so the report can line the h=8 and h=4 runs up row by row. An event absent from `d` altogether lost
# every firm to balancing; it is charged to the winner, the first side checked.
if (!nrow(discards)) discards <- data.table(ev = integer(), reason = character())
bal <- unique(d[, .(ev, has_t, has_c, has_n)])
nw_panel <- roster[category == "non_winner", .(nw_in_panel = any(in_panel)), by = ev]
event_fate <- merge(events[, .(ev, lot_key, winner_cvr, event_qidx)], discards, by = "ev", all.x = TRUE)
event_fate <- merge(event_fate, bal,      by = "ev", all.x = TRUE)
event_fate <- merge(event_fate, nw_panel, by = "ev", all.x = TRUE)
event_fate[, `:=`(has_t = fcoalesce(has_t, FALSE), has_c = fcoalesce(has_c, FALSE),
                  has_n = fcoalesce(has_n, FALSE), nw_in_panel = fcoalesce(nw_in_panel, FALSE))]
event_fate[, stage := fcase(!is.na(reason), "matching",
                            !(has_t & has_c & has_n), "balancing",
                            default = "kept")]
event_fate[stage == "balancing", reason := fcase(
  !has_t,        "winner not balanced on the window",
  !has_c,        "no matched control balanced on the window",
  !nw_in_panel,  "no real co-bidder in the firm panel",
  default =      "no real co-bidder balanced on the window")]
event_fate[stage == "kept", reason := "complete stack"]
event_fate[, c("has_t", "has_c", "has_n", "nw_in_panel") := NULL]
setorder(event_fate, ev)
stopifnot(nrow(event_fate) == nrow(events), !anyNA(event_fate$reason),
          event_fate[stage == "kept", .N] == d[complete_stack == TRUE, uniqueN(ev)])

cat("\n  event fate (first failure wins):\n")
print(event_fate[, .N, by = .(stage, reason)][order(factor(stage, c("matching", "balancing", "kept")), -N)])

d[, treated := as.integer(category == "treated")]
# 1 / (firms in this (event, category) cell), so each of the three sides of a stack sums to 1. The
# property survives any two-category filter, so a lot that tied 40 controls cannot outvote a lot with
# one real loser whichever contrast is estimated.
d[, weight := 1 / uniqueN(cvr), by = .(ev, category)]
d[, c("n_row", "n_per", "tmin", "tmax", "n_ok", "has_t", "has_c", "has_n") := NULL]
setorder(d, ev, category, cvr, qidx)

cat("\n  panel by category (complete stacks only):\n")
print(d[complete_stack == TRUE, .(rows = .N, firms = uniqueN(cvr), events = uniqueN(ev)),
        by = category][order(category)])
cat(sprintf("\n  events: %d balanced | %d complete (all three sides) | %.1f%%\n",
            uniqueN(d$ev), d[complete_stack == TRUE, uniqueN(ev)],
            100 * d[complete_stack == TRUE, uniqueN(ev)] / max(1L, uniqueN(d$ev))))

# SIZE RELATIVE TO THE WINNER -- the readout for comparing MATCH_SCORE=level against log. Per complete
# stack, each side's weighted mean FTE over the winner's, then the median across stacks. The level
# score's asymmetry predicts synthetic controls below 1; the log score should pull them toward it.
sz <- d[complete_stack == TRUE & event_time %in% c(-1L, H),
        .(fte = sum(weight * fte)), by = .(ev, category, event_time)]
sz <- dcast(sz, ev + event_time ~ category, value.var = "fte")
cat(sprintf("\n  size relative to the winner (median over complete stacks, score = %s):\n", SCORE))
print(sz[, .(matched_control = round(median(found_control / treated), 3),
             real_cobidder   = round(median(non_winner    / treated), 3)), by = event_time])

# ---- 10. checks --------------------------------------------------------------------------------------------
match_rule_banner("7. checks")
stopifnot(roster[category == "treated", .N, by = ev][, all(N == 1L)])
cat("  OK  exactly one treated row per event\n")
if (IS_PS) {
  stopifnot(roster[category == "found_control", all(!is.na(pscore))])
  pst <- roster[category == "found_control", .(N = .N, n_tied = n_tied[1L], n_ranked = n_ranked[1L]), by = ev]
  stopifnot(pst[, all(N == n_tied & n_tied >= pmin(PS_K, n_ranked))])
  cat(sprintf("  OK  every control carries an index; each stack holds its top %d (ties %s)\n", PS_K,
              if (ps_variant(PS_V)$ties == "random") "drawn at random" else "kept"))
  stopifnot(nrow(roster[category == "non_winner" & eligible == TRUE & is.na(pscore)]) == 0L)
} else {
  stopifnot(roster[category == "found_control", all(!is.na(qscore))])
  stopifnot(roster[category == "found_control", .(u = uniqueN(stack_qscore)), by = ev][, all(u == 1L)])
  cat("  OK  every control carries a qscore and sits at its stack's minimum\n")
  stopifnot(nrow(roster[category == "non_winner" & eligible == TRUE & is.na(qscore)]) == 0L)
}
cat("  OK  every eligible real loser is scored\n")
aud <- d[, .(n = .N, u = uniqueN(event_time), lo = min(event_time), hi = max(event_time)),
         by = .(ev, category, cvr)]
stopifnot(all(aud$n == NP), all(aud$u == NP), all(aud$lo == -H), all(aud$hi == H))
cat("  OK  every retained firm is exactly balanced on -h..h\n")
wsum <- d[event_time == 0L, .(w = sum(weight)), by = .(ev, category)]
stopifnot(all(abs(wsum$w - 1) < 1e-9))
cat("  OK  weights sum to 1 per (event, category) in every period\n")

# ---- 11. write ---------------------------------------------------------------------------------------------
match_rule_banner("8. write")
write_obj(list(roster = roster, events = events, discards = discards, event_fate = event_fate,
               h = H, min_rung = MIN_RUNG, winner_buffer = WBUF, min_fte = MIN_FTE,
               ind_digits = IND_DIGITS, pool = "union", protocol = PROTOCOL, score = SCORE, komm_col = KCOL,
               ps_topk = if (IS_PS) PS_K else NA_integer_,
               ps_model_run_at = if (IS_PS) PSM$run_at else NULL,
               run_at = Sys.time()),
          P$cobid_roster(H, SCORE))
write_tab(d, P$cobid_panel(H, SCORE))
cat(sprintf("  -> %s (%d roster rows)\n", basename(P$cobid_roster(H, SCORE)), nrow(roster)))
cat(sprintf("  -> %s (%d panel rows, %.0f MB)\n", basename(P$cobid_panel(H, SCORE)), nrow(d),
            file.size(P$cobid_panel(H, SCORE)) / 1e6))
cat("\nSTAGE 5 complete.\n")

# The exploratory event-study block that used to sit here was removed. It ran on every invocation
# (so once per window), opened a graphics device under Rscript, and fitted
# `cvr + qidx^industry_grp + treated + event_time` clustered on `lot_key + cvr` -- a specification
# this design deliberately does NOT use. The estimation lives in
# code/analysis/14_estudy_matched_vs_cobidder_validation.Rmd, which is the one place it is defined.
