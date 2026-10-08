#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 2 -- find the best-matched controls for every competitive award event.
#
# Reads ONLY stage 1's artefacts (01_firm_panel.parquet, 01_events.rds, 01_eligible_controls.rds), plus,
# for the pscore protocols only, 01_buyers.rds and 1b_fit_pscore.R's 01b_pscore_model.rds (pscore) /
# 01b_pscore_age_model.rds (pscore_age).
#
# CONTROL POOLS. Both run by default, each cascading independently so control_protocol can differ:
#   never_winner  never won a COMPETITIVE award. Because stage 1's exclusion set is competitive-only,
#                 this pool also holds firms whose wins were all DIRECT awards, and their direct-award
#                 dates are ignored entirely. So "never-winner" here means "never won competitively",
#                 NOT "absent from the procurement data" -- say so in any figure legend.
#   winner        won competitively somewhere, but NOT within +/-h of this event.
#
# CASCADE (fine -> coarse): industry6 -> class4 -> group3 -> division2, each holding kommune; then
# division alone; then any industry. A rung fires only when it yields >= MATCH_MIN_RUNG ELIGIBLE
# candidates (default 5). The original broke at the first NON-EMPTY rung, so an event could "match" on
# industry6+kommune against one candidate that then won rank 1 uncontested. The last rung keeps a >0
# floor -- there is nothing coarser to fall through to.
#
# ELIGIBILITY (unchanged in substance from find_control_firms.R:228-263 + the balanced-window test in
# find_control_firms_never_winners.R:203-209):
#   n_pre  = the quarters of the protocol's ELIGIBILITY window in which the TREATED firm has a valid FTE.
#            The treated firm must have ALL of them -- an event whose winner is missing any pre-period
#            quarter is discarded here rather than matched and then destroyed by stage 3's balance test.
#            So n_pre == h for every matched event, which is also what makes qscore comparable across
#            events (it normalises by treated FTE, not by the number of quarters scored).
#   a control must have a positive, non-missing FTE in every one of those n_pre quarters (so the gap is
#   computable throughout), and must additionally be observed in >= h post-event quarters.
#
# WHAT THE fte > 0 PART OF THAT COSTS is now counted rather than assumed. Per (event, arm) the matcher
# also computes the eligible set it WOULD have had without the positivity requirement, and reports the
# difference: candidates that clear every other eligibility test and are excluded solely by fte > 0.
# Two limits on how far that number can be read, both structural:
#   - it is a CANDIDATE-POOL count, not a sample-loss count. Losing candidates costs nothing while
#     enough remain to fire the same rung, so a large number here can have no effect on any estimate.
#   - it is PRE-PERIOD only (the screen runs over T_et in [-h,-1]). It says nothing about firms that go
#     to zero AFTER the award, which is the survival-conditioning question. That one is measured in
#     stage 3, which sees the full +/-h window.
# Note also that the TREATED firm is not positivity-screened here (only !is.na below); stage 3 applies
# fte > MATCH_MIN_FTE to both arms, so treated-side zeros are dropped there, not here.
#
# SELECTION (no k-nearest). The score is a property of the protocol (MATCH_PROTOCOLS$<p>$score):
#   level protocols (full_full, full_firsthalf) -- preserved exactly from the original:
#     fte_diff_sq    = (fte_control - fte_treatment)^2 per scored quarter
#     control_qscore = sqrt(mean(fte_diff_sq)) / mean(fte_treatment)
#   log protocols (full_full_log, full_firsthalf_log):
#     control_qscore = sqrt(mean((log fte_control - log fte_treatment)^2))
#     symmetric in proportional size; see the MATCH_PROTOCOLS note in 0_matching_utils.R
#   keep every control tied at min(control_qscore) -- usually one firm, sometimes several.
#   mean_fte_diff_sq is recorded under both scores as a diagnostic.
# find_control_firms.R:263 is the live criterion; the mean_fte_diff_sq line at :262 is commented out.
# The two orderings agree (mean(fte_treatment) is constant within an event) but qscore is scale-free and
# therefore comparable ACROSS events, so it is the one to keep. Note that file's header claim of matching
# on "FTE and age" is wrong: firm_age was only ever carried into the output, never scored. It is now
# dropped from the pipeline entirely, so nothing implies age was part of the match.
#
# MATCH PROTOCOLS -- the second axis, (eligibility window x ranking window). Protocols that share an
# eligibility window share the candidate set, so eligibility is computed ONCE and merely re-scored per
# ranking window; adding a protocol costs one mean() over a subset of quarters, not another cascade.
#
# THE pscore PROTOCOL IS A DIFFERENT ALGORITHM (opt-in: add "pscore" to MATCH_PROTOCOLS). No cascade and
# no path score: every eligible candidate in the arm -- the same eligibility, buffer and pools as above --
# is ranked by a bidding-propensity index (1b_fit_pscore.R; definitions in 0_matching_utils.R) against
# this winner and its tender's buyers, and the top MATCH_PS_TOPK are kept, ties at the K-th kept. The
# event's own buyers are never candidates. Its rows carry control_pscore (the index; higher = more like a
# bidder, comparable only within a stack), pscore_rank and n_ranked; control_qscore is NA. Because every
# event that reaches the arm loop has the full pre-period, eligibility is the same for every event in a
# group, so it and the candidates' characteristics are computed ONCE per group (ps_group()).
# pscore_fte, pscore_age and pscore_nofte (opt-in) are the same algorithm on other models: FTE at t-2 and
# growth t-6 -> t-2 (the 5 Oct terms), firm age in place of FTE, or nothing (0/1 characteristics only, ties
# at the K-th drawn at random). Any number of pscore protocols can run together, each on its own model, and
# the cascade is unaffected.
#
# EVENT PROVENANCE. Every output row -- treated and control alike -- carries the identifiers of the
# procurement its event came from (EVENT_META_COLS, documented in 0_matching_utils.R), so a matched
# event joins to the co-bidder design by identifier instead of by (winner cvr, event quarter).
#
# NOTE ON REPEAT EVENTS. The event grain is the award QUARTER, so one firm can be treated in several
# events, and two of them can sit close enough that each is inside the other's window. The firm fixed
# effect pools them; it does not separate them.
#
# PERFORMANCE. Events are processed in groups sharing an event_qidx (~100 distinct values, not 33k), so
# the panel is sliced ~100 times instead of once per event. Only the columns matching needs are carried:
# find_control_firms_never_winners.R:133-135 records that hauling all ~69 columns "exhausted the 24 GB
# vector limit". The finished match is checkpointed once, at the end (an interrupted run does NOT resume).
#
#   Rscript code/matching/2_match_controls.R
# Options (env):
#   MATCH_H              half-window in quarters (default 8)
#   MATCH_MIN_RUNG       eligible candidates a rung needs to fire (default 10)
#   MATCH_PROTOCOLS      default "full_full full_firsthalf full_full_log full_firsthalf_log"
#   MATCH_RULES          default "staggered_industry_kommune"
#   MATCH_KOMMUNE_COL    the kommune the cascade's kommune rungs hold: kommune_code (default, the address at
#                        each quarter's end) or hq_kommune_code (the current head office, as before 6 Oct 2026)
#   MATCH_ARMS           default "never_winner winner"
#   MATCH_WINNER_BUFFER  quarters either side of the event in which a sometimes-winner control may not
#                        have an award of its own (default 12, i.e. a 3-year buffer)
#   MATCH_MAX_TIES       >0 flags events whose tie set exceeds it (default 0 = report only)
#   MATCH_PS_TOPK        controls kept per (event, arm) by each pscore protocol (default 5)
#   MATCH_PS_SEED        seed for the random tie-break of models with ties = "random" (pscore_nofte); the
#                        draw for an event uses MATCH_PS_SEED + ev (default 20261005)
#   MATCH_FORCE_REMATCH  1 to ignore the checkpoint. A checkpoint written under different settings
#                        (window, protocols, arms, buffer, rung floor, K, pscore model, 01_events) stops
#                        the run instead of being reused.
#   MATCH_CORES, MATCH_TEST_N, MATCH_OVERWRITE
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))
match_setup(extra_libs = "parallel")

H        <- match_env_int("MATCH_H", 8L)
MIN_RUNG <- match_env_int("MATCH_MIN_RUNG", 10L)
WBUF     <- match_env_int("MATCH_WINNER_BUFFER", 12L)   # see CONTROL POOLS in the header
MAX_TIES <- match_env_int("MATCH_MAX_TIES", 0L)
ARMS     <- match_env_list("MATCH_ARMS", "never_winner winner")
PROTOS   <- match_protocols()
RULES    <- match_rules()
KCOL     <- match_kommune_col()   # the cascade's kommune: contemporaneous kommune_code unless set
N_CORES  <- match_env_int("MATCH_CORES", max(1L, parallel::detectCores() - 1L))
P        <- match_paths()

# The pscore protocol runs its own selection (no cascade); every other protocol shares the cascade.
PS_PROTOS <- PROTOS[vapply(PROTOS, function(p) MATCH_PROTOCOLS[[p]]$score == "pscore", logical(1))]
QS_PROTOS <- setdiff(PROTOS, PS_PROTOS)
PS_K      <- match_env_int("MATCH_PS_TOPK", 5L)
PS_SEED   <- match_env_int("MATCH_PS_SEED", 20261005L)   # per-event seed (+ ev) where a model breaks ties at random
if (length(PS_PROTOS) && (is.na(PS_K) || PS_K < 1L))
  stop("MATCH_PS_TOPK must be a positive integer", call. = FALSE)
# Each pscore protocol's model variant (PSCORE_VARIANTS), and whether any of them reads the firm's age
PS_VAR   <- vapply(setNames(PS_PROTOS, PS_PROTOS), function(p) MATCH_PROTOCOLS[[p]]$variant, character(1))
PS_AGE   <- any(vapply(PS_VAR, function(v) "a2" %in% ps_variant(v)$needs, logical(1)))
PS_AVG   <- any(vapply(PS_VAR, function(v) "lbar" %in% ps_variant(v)$needs, logical(1)))   # pre-period average FTE

if (WBUF < H) stop(sprintf(paste0(
  "MATCH_WINNER_BUFFER=%d is narrower than the study window h=%d. A sometimes-winner control\n",
  "  could then be treated inside the very window it is a control for. Set it to at least %d."),
  WBUF, H, H), call. = FALSE)
cat(sprintf("STAGE 2 | h=%d | min_rung=%d | winner_buffer=%d | arms=%s\n",
            H, MIN_RUNG, WBUF, paste(ARMS, collapse = ",")))
cat(sprintf("  protocols: %s\n", paste(sprintf("%s (%s)", PROTOS,
            vapply(PROTOS, function(p) MATCH_PROTOCOLS[[p]]$label, character(1))), collapse = " | ")))
cat(sprintf("  rules    : %s | kommune rungs hold %s\n", paste(RULES, collapse = ", "), KCOL))
for (p in PS_PROTOS) cat(sprintf("  %s : top %d by bidding propensity per (event, arm), no cascade | model %s (%s)\n",
                                 p, PS_K, PS_VAR[[p]], paste(ps_variant(PS_VAR[[p]])$features, collapse = ", ")))

# ---- inputs -------------------------------------------------------------------------------------------
match_rule_banner("1. inputs (stage 1 artefacts only)")
panel    <- read_tab(P$firm_panel)
events   <- as.data.table(read_obj(P$events))
eligible <- as.data.table(read_obj(P$eligible))
cat(sprintf("  panel  : %d rows | %d firms\n", nrow(panel), uniqueN(panel$cvr)))
cat(sprintf("  events : %d total | %d placeable | %d in the study subset\n",
            nrow(events), events[placeable == TRUE, .N],
            events[placeable == TRUE & test_subset == TRUE, .N]))

study <- events[placeable == TRUE & test_subset == TRUE]
if (!nrow(study)) stop("no study events -- check stage 1 output", call. = FALSE)

# Every competitive award date, INCLUDING unplaceable events: the sometimes-winner arm has to know about
# awards it cannot itself study, or it would offer a firm as a control during its own award window.
# This is complete only because stage 1's event grain is the QUARTER. Under the old (cvr, award_year)
# grain a firm's second and later award quarters in a year were never in `events` at all, so a firm with
# a Q1 and a Q3 award read as having no Q3 award and could be offered as a clean control in a quarter it
# had actually won in. Nothing here changed -- the input did.
award_idx  <- unique(events[, .(cvr, event_qidx)])
all_comp_w <- unique(events$cvr)

# The never-winner pool is the stage-1 registry screen, intersected with firms that actually have data.
# has_employment is an EVER flag (fte > 0 or employees > 0 in ANY quarter of the panel -- see
# 1_build_universe.R:258), so this is a coarse "is this a real operating firm" gate, NOT the eligibility
# test. Per-event eligibility is stricter and lives in match_one_event(): positive, non-missing FTE in
# every one of the treated firm's pre-period quarters, plus full post-period coverage.
elig_cvrs <- eligible[has_employment == TRUE, unique(cvr)]
cat(sprintf("  eligible never-winner pool with data: %d\n", length(elig_cvrs)))

# The pscore protocol's inputs: the fitted model, and each study event's buyers with their kommune at
# t - 2 (keyed on ev; the forked workers read these globals, as they read `pool`).
if (length(PS_PROTOS)) {
  PSM <- lapply(PS_VAR, function(v) read_pscore_model(P$ps_model_for(v), v))   # by protocol
  BUY <- read_obj(P$buyers)
  if (!all(c("event_data_source", "event_tender_id") %in% names(study)))
    stop("01_events.rds has no event provenance -- re-run stage 1 (the pscore buyers need it)", call. = FALSE)
  EV_BUY <- buyer_kommunes(study[, .(key = ev, data_source = event_data_source, tender_id = event_tender_id,
                                     q = event_qidx - PSCORE_LAGS[1])], BUY)
  setkey(EV_BUY, key)
  for (p in PS_PROTOS)
    cat(sprintf("  %s model: fitted %s | %d strata | %d losing bidders | concordance %.3f\n",
                p, format(PSM[[p]]$run_at), PSM[[p]]$n_strata, PSM[[p]]$nevent, PSM[[p]]$concordance))
  cat(sprintf("  study events with a known buyer: %d of %d (%d with a buyer kommune)\n",
              uniqueN(EV_BUY$key), nrow(study), uniqueN(EV_BUY[!is.na(buyer_kommune), key])))
}

# ---- compact pool -------------------------------------------------------------------------------------
# Carry ONLY what matching needs. Attributes stay row-level (not collapsed to one per firm) because the
# original matched on the candidate's industry/kommune AS OBSERVED IN THE PRE-WINDOW, and that is what
# slicing by window reproduces for free.
POOL_COLS <- c("cvr", "qidx", "fte", "firm_type",
               "industry_code6", "industry_class", "industry_group", "industry_division", KCOL)
# The pscore characteristics also need the legal form and the address kommune (time-varying). Carried
# only when pscore is requested: two character columns on every pool row are not free.
if (length(PS_PROTOS)) {
  if (!"legal_form_short" %in% names(panel))
    stop("the firm panel has no legal_form_short -- re-run stage 1 (pscore needs it)", call. = FALSE)
  POOL_COLS <- unique(c(POOL_COLS, "legal_form_short", "kommune_code"))
}
missing_cols <- setdiff(POOL_COLS, names(panel))
if (length(missing_cols)) stop("panel is missing required columns: ", paste(missing_cols, collapse = ", "),
                               call. = FALSE)
# pscore_age's firm age: the founding date is firm-level, so one row per firm rather than a pool column
if (PS_AGE && !"registration_date" %in% names(panel))
  stop("the firm panel has no registration_date -- re-run stage 1 (pscore_age needs it)", call. = FALSE)
FOUNDED <- if (PS_AGE) unique(panel[!is.na(registration_date), .(cvr, registration_date)]) else NULL
pool <- panel[, ..POOL_COLS]
rm(panel); invisible(gc())
pool[, arm_pool := fifelse(firm_type == "winner", "winner",
                    fifelse(cvr %chin% elig_cvrs, "never_winner", NA_character_))]
pool <- pool[!is.na(arm_pool)]   # every event cvr is a competitive winner, so no treated firm is dropped
setkey(pool, qidx)
cat(sprintf("  pool   : %d rows | never_winner %d firms | winner %d firms\n", nrow(pool),
            pool[arm_pool == "never_winner", uniqueN(cvr)], pool[arm_pool == "winner", uniqueN(cvr)]))

elig_windows <- unique(lapply(PROTOS, function(p) MATCH_PROTOCOLS[[p]]$elig(H)))
if (length(elig_windows) > 1L)
  stop("the selected MATCH_PROTOCOLS do not share an eligibility window, but the matcher slices the\n",
       "  pre-window once per event_qidx group on that assumption. Run them as separate invocations.",
       call. = FALSE)
cat(sprintf("  shared eligibility window: [%d, %d]\n", min(elig_windows[[1]]), max(elig_windows[[1]])))

RUNGS <- MATCH_RULES[[RULES[1]]]()
if (length(RULES) > 1) cat("  note: only the first rule is applied per run; rerun with MATCH_RULES=<other>\n")

# ---- the pscore protocol: once per group, then once per (event, arm) --------------------------------------
# Everything about the pscore candidate set that does NOT depend on the event. Every event that reaches
# the arm loop has T_et == [-h,-1] (match_one_event() discards the rest), so the eligibility tests are the
# same for all of them -- positive FTE in every pre quarter, >= h observed post quarters -- exactly the
# tests the cascade's last rung ("any industry") applies. That equality is checked after the match.
# args:  Q = the group's event_qidx; pre, win = its slices; keep_w = the sometimes-winner set
# returns: list(elig = eligible (cvr, arm_pool) firms,
#               cand = by pscore protocol, elig + characteristics for the firms its variant can score,
#               fv   = firm values for every pool firm at Q-2, winners included -- the winner side)
ps_group <- function(Q, pre, win, keep_w) {
  ok   <- pre[!is.na(fte) & fte > 0, .N, by = cvr][N == H, cvr]
  ok   <- win[cvr %chin% ok & et >= 1L & et <= H, uniqueN(et), by = cvr][V1 >= H, cvr]
  elig <- unique(pre[cvr %chin% ok, .(cvr, arm_pool)])
  elig <- elig[arm_pool == "never_winner" | (arm_pool == "winner" & cvr %chin% keep_w)]
  # Read from `pool` by key rather than from the slice: at h < 6, Q-6 is outside the +/-h window.
  f2 <- pool[.(Q - PSCORE_LAGS[1]), .(cvr, fte2 = fte, industry_division, industry_class,
                                      legal_form_short, kommune_code), nomatch = 0L]
  f6 <- pool[.(Q - PSCORE_LAGS[2]), .(cvr, fte6 = fte), nomatch = 0L]
  fr <- merge(f2, f6, by = "cvr", all.x = TRUE)
  if (PS_AGE) pscore_add_age(fr, FOUNDED, Q - PSCORE_LAGS[1])
  if (PS_AVG) pscore_add_avg(fr, pool, Q)                     # by key from `pool`: t-8 can be outside the slice
  fv <- pscore_firm_vars(fr)
  list(elig = elig, cand = lapply(PS_VAR, function(v) merge(elig, fv[ps_scorable(fv, v)], by = "cvr")), fv = fv)
}

# One event's controls in one arm under pscore protocol pr: every eligible candidate, ranked by the
# propensity index against this winner and its tender's buyers; the top PS_K kept, ties at the K-th kept.
# args:  e = the event row; a = the arm; psg = ps_group() for e's group; n_pre = the treated pre quarters;
#        pr = the pscore protocol (its model is PSM[[pr]], its variant PS_VAR[[pr]])
# returns: list(rows = the control rows) or list(skip = "<reason>")
ps_select <- function(e, a, psg, n_pre, pr) {
  v <- PS_VAR[[pr]]
  W <- e$cvr
  w <- psg$fv[cvr == W]
  if (!ps_winner_ok(w, v)) return(list(skip = ps_variant(v)$w_skip))
  # Before the buyer exclusion, so it equals what the cascade's last rung counts (checked below).
  n_elig <- psg$elig[arm_pool == a & cvr != W, .N]
  b <- EV_BUY[.(e$ev), nomatch = 0L]                         # this event's buyers and their kommuner
  x <- psg$cand[[pr]][arm_pool == a & cvr != W & !(cvr %chin% b$buyer_cvr)]   # a subset, so a copy
  if (!nrow(x)) return(list(skip = "no scorable eligible candidate"))
  pscore_features(x, w, b$buyer_kommune, v)
  x[, control_pscore := pscore_index(x, PSM[[pr]]$coef, v)]
  sel <- x[ps_top_k(x$control_pscore, PS_K, v, PS_SEED + e$ev)]   # ties kept, or drawn at random (pscore_nofte)
  sel[, pscore_rank := as.integer(frank(-control_pscore, ties.method = "min"))]
  list(rows = data.table(
    ev = e$ev, scoring_protocol = pr, arm = a, cvr = sel$cvr,
    treatment = "control", control_protocol = MATCH_PROTOCOLS[[pr]]$label, rung_idx = NA_integer_,
    control_qscore = NA_real_, mean_fte_diff_sq = NA_real_, n_scored = NA_integer_,
    n_eligible = n_elig, n_pre = n_pre, n_tied = nrow(sel),
    control_pscore = sel$control_pscore, pscore_rank = sel$pscore_rank, n_ranked = nrow(x)))
}

# ---- the per-event matcher -------------------------------------------------------------------------------
# Returns one data.table of (ev, scoring_protocol, arm, cvr, ...) rows, or a `discard` record explaining why
# the event produced nothing. Discards are COUNTED and reported, never silently dropped.
# args:
#   e                 ONE row of `study` (the event): needs $cvr (the winner) and $ev (event id)
#   pre               pre-period rows for ALL firms in this event_qidx group (et in [-h,-1]).
#                     Shared across every event in the group -- never subset to this event.
#   win               the full +/-h window for all firms; needed for the post-coverage test,
#                     which `pre` cannot answer
#   keep_winner_cvrs  competitive winners eligible for the sometimes-winner arm, i.e. all of them
#                     MINUS any with an award inside this window. Precomputed per group.
#   psg               ps_group() for this group (NULL unless the pscore protocol is requested)
# returns: either list(rows = <match records>, diag = ...) or list(discard = "<reason>").
#          Never both. `rows` carries firm IDENTITIES and match diagnostics only -- no employment
#          series; that is joined on later. `ps_skip` lists (event, arm) pairs pscore could not score.
match_one_event <- function(e, pre, win, keep_winner_cvrs, psg = NULL) {
  W <- e$cvr
  tw <- pre[cvr == W] # Get treated firm pre-period
  if (!nrow(tw)) return(list(discard = "treated absent from the pre-window"))
  # Exactly one row per event time, or the scoring merge below silently doubles every control's rows
  # and corrupts every qscore in this event. That merge passes allow.cartesian = TRUE, which suppresses
  # the error data.table would otherwise raise, so this check is the only thing between a duplicate
  # firm-quarter upstream and a wrong answer that looks entirely healthy.
  if (anyDuplicated(tw$et)) return(list(discard = "treated has duplicate rows at an event time"))

  # Treated must clear the balanced +/-h test too, else the event is not estimable at this h.
  if (win[cvr == W & et >= 1L & et <= H, uniqueN(et)] < H)
    return(list(discard = "treated lacks h post-event quarters"))

  # The treated firm's usable pre-period defines what any control must cover, so test it ONCE up front
  # rather than inside the rung loop -- otherwise an event with no candidates gets blamed on the cascade.
  T_et  <- tw[!is.na(fte), unique(et)] # tw[] is the treated firm's preperiod data
  n_pre <- length(T_et)
  if (n_pre == 0L) return(list(discard = "treated has no valid pre-period FTE"))
  # Any missing pre-period quarter and the firm cannot survive stage 3 (non-missing is required in all
  # 2h+1 quarters, whatever MATCH_MIN_FTE is), so matching it is wasted work and inflates the match
  # report. It also keeps qscore comparable across events: the score normalises by the treated firm's
  # FTE but NOT by the number of quarters scored, so a short T_et would score artificially well.
  if (n_pre < H) return(list(discard = "treated has an incomplete pre-period"))

  ind_vals <- list(
    industry_code6    = unique(tw$industry_code6[!is.na(tw$industry_code6)]),
    industry_class    = unique(tw$industry_class[!is.na(tw$industry_class)]),
    industry_group    = unique(tw$industry_group[!is.na(tw$industry_group)]),
    industry_division = unique(tw$industry_division[!is.na(tw$industry_division)]))
  komm <- unique(tw[[KCOL]][!is.na(tw[[KCOL]])])   # every kommune the treated firm had in the pre-window

  # How many candidates at a rung clear EVERY eligibility test EXCEPT fte > 0. Called once per
  # (event, arm) -- at the rung that fired, or at the last rung tried if none did -- never inside the
  # loop, where it would double the eligibility work on every iteration.
  # It holds the rung FIXED, which is the honest limit of the number: a larger eligible set could have
  # cleared MIN_RUNG at an earlier, finer rung, so this is the cost at the observed rung and not the
  # counterfactual cascade. See ELIGIBILITY in the header for the other two limits.
  elig_nopos <- function(cv) {
    if (!length(cv)) return(0L)
    o <- pre[cvr %chin% cv & et %in% T_et & !is.na(fte), .N, by = cvr][N == n_pre, unique(cvr)]
    if (!length(o)) return(0L)
    length(win[cvr %chin% o & et >= 1L & et <= H, uniqueN(et), by = cvr][V1 >= H, unique(cvr)])
  }
  dg_row <- function(rung, n_cand, n_elig, n_nopos, fired, reason)
    list(rung = rung, n_cand = as.integer(n_cand), n_elig = as.integer(n_elig),
         n_elig_nopos = as.integer(n_nopos),
         n_drop_pos = as.integer(max(0L, n_nopos - n_elig)), fired = fired, reason = reason)

  out <- list(); diag <- list(); ps_skip <- list()

  # The pscore protocols: no cascade, the propensity index ranks the whole eligible arm (ps_select()).
  for (pr in PS_PROTOS) for (arm in ARMS) {
    s <- ps_select(e, arm, psg, n_pre, pr)
    if (!is.null(s$skip)) ps_skip[[paste(arm, pr)]] <- data.table(ev = e$ev, scoring_protocol = pr, arm = arm,
                                                                  reason = s$skip)
    else out[[paste(arm, pr)]] <- s$rows
  }

  # The cascade, for every other protocol.
  for (arm in if (length(QS_PROTOS)) ARMS else character()) {
    arm_cvrs <- if (arm == "never_winner") {
      pre[arm_pool == "never_winner", unique(cvr)] # Extract CVR numbers for each arm
      } else {
      intersect(pre[arm_pool == "winner", unique(cvr)], keep_winner_cvrs)
      }
    arm_cvrs <- setdiff(arm_cvrs, W) # Winning firm cannot be its own control
    if (!length(arm_cvrs)) { diag[[arm]] <- dg_row(NA_character_, 0L, 0L, 0L, FALSE, "empty arm pool"); next }

    # One rung: candidates sharing the treated firm's industry (+/- kommune) during the pre-window,
    # then reduced to those that are actually ELIGIBLE. Counting eligibles (not raw candidates) is what
    # makes MATCH_MIN_RUNG mean "a real choice set".
    rung_hit <- NULL
    last_cand <- character(0); last_rung <- NA_character_   # for the counter when no rung fires
    for (ri in seq_along(RUNGS)) {
      r <- RUNGS[[ri]] # Get out rung metadata
      vals <- if (is.null(r$col)) NULL else ind_vals[[r$col]] # Extract industry code
      if (!is.null(r$col) && !length(vals)) next          # treated has no value at this granularity
      
      # Define candidate set as those CVR numbers in the never-winner/winner arm
      # that have the industry as in the treated firm in the industry rung
      # and the same kommune (KCOL) in some pre-window quarter
      cand <- pre[cvr %chin% arm_cvrs &
                    (if (is.null(r$col)) TRUE else get(r$col) %chin% vals) &
                    (if (r$komm) get(KCOL) %chin% komm else TRUE), unique(cvr)]
      if (!length(cand)) next
      last_cand <- cand; last_rung <- r$label

      # Find eligible firms: those with positive, non-missing FTE every period over pre-period and post-period
      # Pre-period:
      ok <- pre[cvr %chin% cand & et %in% T_et & !is.na(fte) & fte > 0, .N, by = cvr][N == n_pre, unique(cvr)]
      if (!length(ok)) next
      # ... and >= h observed post-event quarters
      ok <- win[cvr %chin% ok & et >= 1L & et <= H, uniqueN(et), by = cvr][V1 >= H, unique(cvr)]
      if (!length(ok)) next

      floor_ok <- if (ri == length(RUNGS)) length(ok) >= 1L else length(ok) >= MIN_RUNG
      if (floor_ok) { rung_hit <- list(idx = ri, label = r$label, cvrs = ok, cand = cand); break }
    }
    if (is.null(rung_hit)) {
      # The consequential case for the counter: positivity may be WHY no rung cleared MIN_RUNG.
      diag[[arm]] <- dg_row(last_rung, length(last_cand), 0L, elig_nopos(last_cand), FALSE,
                            "no eligible candidate in this arm")
      next
    }
    diag[[arm]] <- dg_row(rung_hit$label, length(rung_hit$cand), length(rung_hit$cvrs),
                          elig_nopos(rung_hit$cand), TRUE, NA_character_)

    # score once per protocol over the SHARED eligible set
    for (pr in QS_PROTOS) {
      rank_off <- MATCH_PROTOCOLS[[pr]]$rank(H)
      rk <- intersect(T_et, rank_off)
      if (!length(rk)) next                                # nothing scorable under this protocol
      tv <- tw[et %in% rk, .(et, fte_t = fte)] # Simple treated firm event times and corresponding fte
      cv <- pre[cvr %chin% rung_hit$cvrs & et %in% rk, .(cvr, et, fte)] # Same for control firms
      # tv is one row per et (asserted on tw above), so this is many-to-one, not a cartesian blow-up.
      cv <- merge(cv, tv, by = "et", allow.cartesian = TRUE)
      sc <- cv[, .(mean_fte_diff_sq = mean((fte - fte_t)^2, na.rm = TRUE),
                   mean_fte_treated = mean(fte_t, na.rm = TRUE),
                   n_scored         = .N), by = cvr]
      # mean_fte_treated > 0 is a DENOMINATOR guard, not an eligibility screen: it is the divisor of
      # control_qscore on the next line. A treated firm averaging zero FTE across the ranked quarters has
      # no scale to normalise by, so the event is unscorable under this protocol and falls to the
      # "no control selected in any arm" discard.
      sc <- sc[is.finite(mean_fte_diff_sq) & is.finite(mean_fte_treated) & mean_fte_treated > 0]
      if (!nrow(sc)) next
      if (MATCH_PROTOCOLS[[pr]]$score == "log") {
        # Log score: RMS of the per-quarter log gap. Candidates already have fte > 0 on every T_et
        # quarter (eligibility, above); the treated firm need not, and a zero quarter has no log, so
        # the event is unscorable under this protocol -- the same fate as the denominator guard.
        if (any(tv$fte_t <= 0, na.rm = TRUE)) next
        lg <- cv[cvr %chin% sc$cvr, .(mean_log_diff_sq = mean((log(fte) - log(fte_t))^2)), by = cvr]
        sc <- merge(sc, lg, by = "cvr")
        sc <- sc[is.finite(mean_log_diff_sq)]
        if (!nrow(sc)) next
        sc[, control_qscore := sqrt(mean_log_diff_sq)]
        sc[, mean_log_diff_sq := NULL]
      } else {
        sc[, control_qscore := sqrt(mean_fte_diff_sq) / mean_fte_treated]
      }
      sel <- sc[control_qscore == min(control_qscore, na.rm = TRUE)]   # ALL ties at rank 1
      out[[paste(arm, pr)]] <- data.table(
        ev = e$ev, scoring_protocol = pr, arm = arm, cvr = sel$cvr,
        treatment = "control", control_protocol = rung_hit$label, rung_idx = rung_hit$idx,
        control_qscore = sel$control_qscore, mean_fte_diff_sq = sel$mean_fte_diff_sq,
        n_scored = sel$n_scored, n_eligible = length(rung_hit$cvrs), n_pre = n_pre,
        n_tied = nrow(sel),
        # pscore-only columns, typed NA: every protocol's rows must bind without fill
        control_pscore = NA_real_, pscore_rank = NA_integer_, n_ranked = NA_integer_)
    }
  }
  if (!length(out)) return(list(discard = "no control selected in any arm", diag = diag, ps_skip = ps_skip))

  ctrl <- rbindlist(out, use.names = TRUE)
  # The treated firm, once per (scoring window, pool) so every stack is self-contained.
  # It CARRIES THE STACK'S ATTRIBUTES rather than NAs. Every control in a stack sits at the same
  # minimum qscore, fired at the same industry level and shares the same counts, so these describe the
  # STACK, not the control -- and putting them on the treated row makes a stack-level filter a single
  # predicate. `d[control_qscore <= bound]` then keeps the whole stack; with NAs here it would
  # silently drop every treated firm and need a special case at every call site. (pscore stacks are
  # never cut, so their NA control_qscore is harmless -- see 4_run_regressions.R.) A firm has no
  # propensity against itself, so the treated row's control_pscore and pscore_rank are NA.
  trt <- unique(ctrl, by = c("ev", "scoring_protocol", "arm"))[
    , .(ev, scoring_protocol, arm, cvr = W, treatment = "treated",
        control_protocol, rung_idx, control_qscore, mean_fte_diff_sq,
        n_scored, n_eligible, n_pre, n_tied,
        control_pscore = NA_real_, pscore_rank = NA_integer_, n_ranked)]
  list(rows = rbindlist(list(trt, ctrl), use.names = TRUE), diag = diag, ps_skip = ps_skip)
}

# ---- run over event_qidx groups -----------------------------------------------------------------------
match_rule_banner("2. matching")
groups <- sort(unique(study$event_qidx))
cat(sprintf("  %d events across %d event_qidx groups | %d workers\n", nrow(study), length(groups), N_CORES))

# args:
#   Q  ONE event_qidx (a quarter index). Every event awarded in that quarter is handled together,
#      because they all need the identical +/-h slice of the panel -- ~100 slices instead of
#      ~33k. Purely a performance device: results are identical event-by-event.
# returns: list(rows, discards), the stacked returns of match_one_event() for every event in Q.
#          Returns NULL if the window is empty (see the note on that ambiguity in the report).
process_group <- function(Q) {
  win <- pool[.(seq.int(Q - H, Q + H)), nomatch = 0L] # Filter to periods around the event
  if (!nrow(win)) return(NULL)
  win[, et := qidx - Q] # Define event time relative to the treated firm's event_qidx, so the +/-h window is [-h, h]
  elig_off <- MATCH_PROTOCOLS[[PROTOS[1]]]$elig(H)   # all shipped protocols share [-h,-1]
  pre <- win[et %in% elig_off]
  # Firms disqualified from the sometimes-winner pool: a competitive award anywhere inside the BUFFER,
  # which is deliberately wider than the study window (see CONTROL POOLS in the header).
  in_win <- award_idx[event_qidx %between% c(Q - WBUF, Q + WBUF), unique(cvr)]
  keep_w <- setdiff(all_comp_w, in_win)
  # The pscore candidate set and its characteristics, once for every event in the group
  psg <- if (length(PS_PROTOS)) ps_group(Q, pre, win, keep_w) else NULL

  evs <- study[event_qidx == Q]
  res <- vector("list", nrow(evs)); dsc <- vector("list", nrow(evs)); dgs <- vector("list", nrow(evs))
  pss <- vector("list", nrow(evs))
  for (i in seq_len(nrow(evs))) {
    r <- tryCatch(match_one_event(evs[i], pre, win, keep_winner_cvrs = keep_w, psg = psg),
                  error = function(err) list(discard = paste("error:", conditionMessage(err))))
    if (!is.null(r$rows)) res[[i]] <- r$rows
    if (!is.null(r$discard)) dsc[[i]] <- data.table(ev = evs$ev[i], reason = r$discard)
    # diag was computed and thrown away before; it carries the eligibility funnel, so keep it
    if (length(r$diag)) dgs[[i]] <- rbindlist(lapply(names(r$diag), function(a)
      c(list(ev = evs$ev[i], arm = a), r$diag[[a]])), use.names = TRUE, fill = TRUE)
    if (length(r$ps_skip)) pss[[i]] <- rbindlist(r$ps_skip, use.names = TRUE)
  }
  list(rows = rbindlist(Filter(Negate(is.null), res), use.names = TRUE),
       discards = rbindlist(Filter(Negate(is.null), dsc), use.names = TRUE),
       diags = rbindlist(Filter(Negate(is.null), dgs), use.names = TRUE, fill = TRUE),
       ps_skips = rbindlist(Filter(Negate(is.null), pss), use.names = TRUE))
}

# What a checkpoint was matched under. Reusing one built under other settings would silently mix runs --
# a refitted pscore model, a different K, or `ev` renumbered by a stage-1 rerun -- so that stops instead.
CKPT_SETTINGS <- list(h = H, protocols = PROTOS, arms = ARMS, wbuf = WBUF, min_rung = MIN_RUNG, komm_col = KCOL,
                      ps_k     = if (length(PS_PROTOS)) PS_K else NA_integer_,
                      ps_seed  = if (length(PS_PROTOS)) PS_SEED else NA_integer_,
                      ps_model = if (length(PS_PROTOS)) vapply(PSM, function(m) format(m$run_at), character(1),
                                                               USE.NAMES = FALSE) else NA_character_,
                      events_mtime = format(file.mtime(P$events)))
if (file.exists(P$match_ckpt) && !match_env_lgl("MATCH_FORCE_REMATCH", FALSE)) {
  cat(sprintf("  reusing checkpoint (set MATCH_FORCE_REMATCH=1 to redo): %s\n", basename(P$match_ckpt)))
  parts <- readRDS(P$match_ckpt)
  if (!identical(attr(parts, "settings"), CKPT_SETTINGS))
    stop("the checkpoint was matched under different settings (window, protocols, arms, buffer, rung\n",
         "  floor, MATCH_PS_TOPK, the pscore model, or a rebuilt 01_events) -- or predates this check.\n",
         "  Re-run with MATCH_FORCE_REMATCH=1.", call. = FALSE)
} else {
  setDTthreads(1L)
  timed <- system.time({
    parts <- parallel::mclapply(groups, function(Q)
      tryCatch(process_group(Q), error = function(e) simpleError(conditionMessage(e))),
      mc.cores = N_CORES, mc.preschedule = FALSE)
  })
  setDTthreads(0L)
  cat(sprintf("  matched in %.1f min\n", timed[["elapsed"]] / 60))
  attr(parts, "settings") <- CKPT_SETTINGS
  saveRDS(parts, P$match_ckpt, compress = "gzip")
}

# An OS-killed fork leaves an error/NULL where a record should be; the per-event tryCatch cannot catch
# that. Distinguish it from a caught error so "out of memory" is not blamed for a logic bug.
n_err  <- sum(vapply(parts, function(x) inherits(x, "error"), logical(1)))
n_null <- sum(vapply(parts, is.null, logical(1)))
if (n_err + n_null > 0)
  cat(sprintf("  WARNING: %d groups failed (%d errored, %d NULL -- likely OOM-killed forks; lower MATCH_CORES)\n",
              n_err + n_null, n_err, n_null))
# Which quarters never produced a result, and how many study events were in them. mclapply preserves
# order, so parts[[i]] belongs to groups[i] -- guarded, because a checkpoint from a differently-sized
# run would break that correspondence.
failed_q <- if (length(parts) == length(groups))
  groups[vapply(parts, function(x) is.null(x) || inherits(x, "error"), logical(1))] else integer(0)
n_skipped <- study[event_qidx %in% failed_q, .N]

good <- Filter(function(x) !is.null(x) && !inherits(x, "error"), parts)
if (!length(good)) stop("every group failed -- run process_group(groups[1]) without tryCatch to see why",
                        call. = FALSE)

match_table <- rbindlist(lapply(good, `[[`, "rows"),     use.names = TRUE, fill = TRUE)
# A checkpoint from a run with fewer protocols would silently drop the missing ones downstream.
missing_pr <- setdiff(PROTOS, unique(match_table$scoring_protocol))
if (length(missing_pr))
  stop("no matches under protocol(s): ", paste(missing_pr, collapse = ", "),
       "\n  if the checkpoint predates them, re-run with MATCH_FORCE_REMATCH=1", call. = FALSE)
discards    <- rbindlist(lapply(good, `[[`, "discards"), use.names = TRUE, fill = TRUE)
# Absent from a checkpoint written before the funnel counters existed -- see the report block below.
eligfunnel  <- rbindlist(lapply(good, `[[`, "diags"),    use.names = TRUE, fill = TRUE)
ps_skips    <- rbindlist(lapply(good, `[[`, "ps_skips"), use.names = TRUE, fill = TRUE)

# ---- event provenance ---------------------------------------------------------------------------------
# Attached here, not inside match_one_event(): it is event-level, so carrying it through the per-event
# worker would copy five columns into every fork for nothing. Riding on match_table means it reaches the
# matched panel, the regression data and the estimation panel with no further join, and it lands on
# CONTROL rows too -- which is the point, since it says which procurement the whole stack is about.
# `study` is one row per ev, so this is many-to-one and the row count is invariant.
meta_have <- intersect(EVENT_META_COLS, names(study))
if (!length(meta_have)) {
  cat("  WARNING: 01_events.rds carries no event provenance -- re-run stage 1 to add it.\n")
} else {
  # THE FAILURE MODE THAT MATTERS: `ev` is positional (.I over events sorted by cvr, event_qidx), so it
  # is NOT stable across runs that built events differently -- the year->quarter grain change renumbered
  # every single one. A checkpoint reused without MATCH_FORCE_REMATCH carries the OLD numbering, and
  # merging it onto today's events would attach real-looking identifiers describing the wrong
  # procurement. all.x = TRUE would not complain. This is what complains.
  stopifnot(all(match_table$ev %in% study$ev))
  n_mt <- nrow(match_table)
  match_table <- merge(match_table, study[, c("ev", meta_have), with = FALSE], by = "ev", all.x = TRUE)
  stopifnot(nrow(match_table) == n_mt, !anyNA(match_table$event_data_source))
}

# ---- report ----------------------------------------------------------------------------------------------
match_rule_banner("3. report")
matched_ev  <- match_table[, uniqueN(ev)]
# Denominator is events ATTEMPTED, not the whole study set. Events in a failed group were never tried,
# so counting them here would report an infrastructure failure as a matching failure.
n_attempted <- nrow(study) - n_skipped
cat(sprintf("  events matched: %d of %d attempted (%.1f%%) | discarded: %d\n",
            matched_ev, n_attempted, 100 * matched_ev / max(1L, n_attempted), nrow(discards)))
if (n_skipped > 0L)
  cat(sprintf(paste0("  NOT ATTEMPTED: %d of %d study events (%.1f%%) sat in %d failed group(s) and were\n",
                     "                 never matched. They are absent from the rate above -- rerun those\n",
                     "                 groups (lower MATCH_CORES if they were OOM-killed) before using\n",
                     "                 this match, or the sample is silently short by that many events.\n"),
              n_skipped, nrow(study), 100 * n_skipped / nrow(study), length(failed_q)))
if (nrow(discards)) { cat("  discard reasons:\n"); print(discards[, .N, by = reason][order(-N)]) }


cat("\n  rung distribution (controls only):\n")
print(match_table[treatment == "control", .(events = uniqueN(ev), firms = .N),
                  by = .(arm, control_protocol)][order(arm, -events)])

cat("\n  eligible-set size per rung:\n")
print(match_table[treatment == "control", .(median_elig = as.numeric(median(n_eligible)),
                                            min_elig = min(n_eligible), max_elig = max(n_eligible)),
                  by = .(arm, control_protocol)][order(arm, -median_elig)])

cat("\n  ties at rank 1 (how many firms share the minimum qscore; cascade protocols):\n")
tie <- unique(match_table[treatment == "control" & scoring_protocol %chin% QS_PROTOS,
                          .(ev, scoring_protocol, arm, n_tied)])
if (nrow(tie))
  print(tie[, .(events = .N, mean_tied = round(mean(n_tied), 2), median_tied = as.numeric(median(n_tied)),
                p95 = as.numeric(quantile(n_tied, .95)), max_tied = max(n_tied)),
            by = .(scoring_protocol, arm)][order(scoring_protocol, arm)])
if (MAX_TIES > 0L && nrow(tie)) {
  over <- tie[n_tied > MAX_TIES]
  cat(sprintf("  events exceeding MATCH_MAX_TIES=%d: %d (flagged, NOT truncated)\n", MAX_TIES, nrow(over)))
}
# ---- what the fte > 0 eligibility screen costs the candidate pool -------------------------------------
# Read this as pool selectivity, NOT as sample loss and NOT as survival conditioning -- see ELIGIBILITY
# in the header for why it is neither. The survival-relevant number lives in stage 3, which sees the
# post-period; this one is pre-period by construction.
if (!nrow(eligfunnel)) {
  cat("\n  fte > 0 eligibility cost: unavailable -- no cascade protocol in this run, or the checkpoint\n")
  cat("    predates the counter (then re-run with MATCH_FORCE_REMATCH=1 to populate it).\n")
} else {
  cat("\n  fte > 0 eligibility cost (per event x arm, at the rung that fired):\n")
  ef <- eligfunnel[fired == TRUE]
  print(ef[, .(events      = .N,
               med_cand    = as.numeric(median(n_cand)),
               med_elig    = as.numeric(median(n_elig)),
               med_nopos   = as.numeric(median(n_elig_nopos)),
               excluded    = sum(n_drop_pos),
               pct_of_pool = round(100 * sum(n_drop_pos) / max(1L, sum(n_elig_nopos)), 1)),
           by = arm][order(arm)])
  nf <- eligfunnel[fired == FALSE & reason == "no eligible candidate in this arm"]
  if (nrow(nf)) {
    # The only channel where the screen costs a MATCH rather than merely candidates.
    # THE BAR IS 1, NOT MIN_RUNG. An arm fails only when the FINAL rung finds nothing, and that rung
    # ("any industry", no kommune) carries every candidate in the arm and asks for one eligible firm.
    # MIN_RUNG decides WHICH rung fires, never WHETHER one does, so it cannot be what lost the arm.
    arm_lost <- nf[n_elig_nopos >= 1L, .N]
    # An arm is not an event: each event runs the cascade once per control pool and survives on either.
    # So an event is lost only if EVERY arm it ran failed AND at least one of them would have found a
    # candidate without the screen.
    ev_state <- eligfunnel[, .(arms = .N, failed = sum(!fired)), by = ev]
    ev_lost  <- length(intersect(ev_state[arms == failed, ev], nf[n_elig_nopos >= 1L, ev]))
    cat(sprintf("    arms with no eligible candidate: %d | %d had >=1 without the screen\n",
                nrow(nf), arm_lost))
    cat(sprintf("      -> the screen cost those %d arm(s) their match, and %d event(s) every arm\n",
                arm_lost, ev_lost))
  }
}

if (length(QS_PROTOS)) {
  cat("\n  qscore distribution (0 = an exactly identical pre-period series):\n")
  print(match_table[treatment == "control" & scoring_protocol %chin% QS_PROTOS,
                    .(zero = sum(control_qscore == 0), median = round(as.numeric(median(control_qscore)), 4),
                      p90 = round(as.numeric(quantile(control_qscore, .9)), 4)),
                    by = .(scoring_protocol, arm)][order(scoring_protocol, arm)])
}

# The pscore protocols: how large the ranked pool is, and how often ties push a stack past K.
if (length(PS_PROTOS)) {
  cat(sprintf("\n  pscore protocols (top %d by propensity, whole arm, no cascade):\n", PS_K))
  ps_st <- unique(match_table[treatment == "control" & scoring_protocol %chin% PS_PROTOS,
                              .(ev, scoring_protocol, arm, n_eligible, n_ranked, n_tied)])
  print(ps_st[, .(stacks = .N, med_eligible = as.numeric(median(n_eligible)),
                  med_ranked = as.numeric(median(n_ranked)), stacks_over_k = sum(n_tied > PS_K),
                  max_kept = max(n_tied)), by = .(scoring_protocol, arm)][order(scoring_protocol, arm)])
  if (nrow(ps_skips)) {
    cat("  (event, arm) pairs a pscore protocol could not score:\n")
    print(ps_skips[, .N, by = .(scoring_protocol, arm, reason)][order(scoring_protocol, arm, -N)])
  }
}

# ---- checks ------------------------------------------------------------------------------------------------
match_rule_banner("4. checks")
stacks <- match_table[, .(has_treated = any(treatment == "treated"),
                          n_control   = sum(treatment == "control")), by = .(ev, scoring_protocol, arm)]
stopifnot(all(stacks$has_treated), all(stacks$n_control >= 1L))
cat("  OK  every (event, protocol, arm) stack has its treated firm and >=1 control\n")

ov <- match_table[treatment == "control", .(arms = uniqueN(arm)), by = .(ev, scoring_protocol, cvr)]
stopifnot(nrow(ov[arms > 1L]) == 0L)
cat("  OK  arms are disjoint (no firm controls in both arms for the same event)\n")

stopifnot(!any(is.na(match_table[treatment == "control", control_protocol])))
qc <- match_table[treatment == "control" & scoring_protocol %chin% QS_PROTOS]   # the cascade protocols
if (nrow(qc)) {
  stopifnot(!any(is.na(qc$control_qscore)))
  cat("  OK  every control carries a rung and a qscore (cascade protocols)\n")

  chk <- merge(qc[, .(ev, scoring_protocol, arm, cvr, control_qscore)],
               qc[, .(mn = min(control_qscore)), by = .(ev, scoring_protocol, arm)],
               by = c("ev", "scoring_protocol", "arm"))
  stopifnot(all(abs(chk$control_qscore - chk$mn) < 1e-12))
  cat("  OK  every kept control sits exactly at its stack's minimum qscore\n")
}

# The pscore protocol keeps the top K, not the minimum: every control has an index, and each stack holds
# exactly what it says it kept -- at least K, or every ranked candidate if there were fewer.
if (length(PS_PROTOS)) {
  pc <- match_table[treatment == "control" & scoring_protocol %chin% PS_PROTOS]
  stopifnot(nrow(pc) > 0L, !anyNA(pc$control_pscore))
  pst <- pc[, .(N = .N, n_tied = n_tied[1L], n_ranked = n_ranked[1L]), by = .(ev, scoring_protocol, arm)]
  stopifnot(pst[, all(N == n_tied & n_tied >= pmin(PS_K, n_ranked))])
  cat(sprintf("  OK  every pscore stack holds its top %d (ties kept, or drawn at random) and every control has an index\n", PS_K))
  # Where a cascade protocol fired at its LAST rung ("any industry"), its eligible set IS the whole arm,
  # so it must equal each pscore protocol's. Catches ps_group() drifting from the cascade's eligibility tests.
  if (nrow(qc)) {
    last <- unique(qc[rung_idx == length(RUNGS), .(ev, arm, n_eligible)])
    ex   <- merge(last, unique(pc[, .(ev, scoring_protocol, arm, n_ps = n_eligible)]), by = c("ev", "arm"))
    stopifnot(ex[, all(n_eligible == n_ps)])
    cat(sprintf("  OK  whole-arm eligibility reproduces the cascade's last rung (%d event x arm checked)\n",
                nrow(ex)))
  }
}

# Protocols sharing an eligibility window must share the ELIGIBLE set -- that shared set is the whole
# efficiency claim. Only the SELECTED set may differ between them. (Cascade protocols only: pscore's
# eligible set is the whole arm, checked against the last rung above.)
if (length(QS_PROTOS) > 1) {
  el <- unique(qc[, .(ev, arm, scoring_protocol, n_eligible)])
  wide <- dcast(el, ev + arm ~ scoring_protocol, value.var = "n_eligible")
  cols <- setdiff(names(wide), c("ev", "arm"))
  ref  <- wide[[cols[1]]]
  bad  <- 0L
  for (cc in cols[-1]) {
    other <- wide[[cc]]
    bad <- bad + sum(!is.na(ref) & !is.na(other) & ref != other)
  }
  stopifnot(bad == 0L)
  cat(sprintf("  OK  the eligible set is identical across the %d cascade protocols (only selection differs)\n",
              length(QS_PROTOS)))
}

# ---- matched panel ---------------------------------------------------------------------------------------
# The +/-h slice for every selected firm, tagged with event time. h=8 covers every window stage 3 builds
# so carrying the full series would only inflate the artefact.
match_rule_banner("5. matched panel")
panel <- read_tab(P$firm_panel)
keep  <- unique(match_table[, .(ev, cvr)])
evq   <- study[, .(ev, event_qidx)]
keep  <- merge(keep, evq, by = "ev")
MP_COLS <- c("cvr", "qidx", "year", "quarter", "fte", "employees", "frequency",
             "industry_code6", "industry_division", "hq_kommune_code", "firm_type")
MP_COLS <- unique(c(MP_COLS, KCOL))                                                  # the cascade's kommune
if ("legal_form_short" %in% names(panel)) MP_COLS <- c(MP_COLS, "legal_form_short")   # for the table below
mp    <- merge(panel[, ..MP_COLS], keep, by = "cvr", allow.cartesian = TRUE)
mp[, event_time := qidx - event_qidx]
mp <- mp[event_time %between% c(-H, H)]
# This merge is also what puts the event-provenance columns on every panel row -- they ride on
# match_table, so nothing here needs to name them.
mp <- merge(mp, match_table, by = c("ev", "cvr"), allow.cartesian = TRUE)
setorder(mp, ev, scoring_protocol, arm, treatment, cvr, qidx)
cat(sprintf("  matched panel: %d rows | %d events | %d firms\n", nrow(mp), uniqueN(mp$ev), uniqueN(mp$cvr)))
print(mp[, .(rows = .N, firms = uniqueN(cvr)), by = .(scoring_protocol, arm, treatment)][order(scoring_protocol, arm, -rows)])

# WHO EACH PROTOCOL PICKED -- the side-by-side comparison of the algorithms. At k = -2, how the controls
# of each (protocol, arm) sit relative to their own winner: size gap, same 2-digit industry, same kommune
# that quarter on the cascade's column (KCOL, what the cascade holds), the share that are A/S against the
# winners' share, and the age gap (what pscore_age matches on; |log(1 + age)|, age to the quarter, NA
# before stage 1 carried the date).
pk <- mp[event_time == -2L]
if (!"legal_form_short" %in% names(pk)) pk[, legal_form_short := NA_character_]
pk[, is_as := recode_legal_form(legal_form_short) == "A/S"]
pk[, age_y := NA_real_]
if ("registration_date" %in% names(panel)) {
  fd <- unique(panel[!is.na(registration_date), .(cvr, registration_date)])
  pk[fd, on = "cvr", age_y := (qidx - qidx_of(year(i.registration_date), quarter(i.registration_date))) / 4]
  pk[age_y < 0, age_y := NA_real_]
  rm(fd)
}
pw <- pk[treatment == "treated", .(ev, scoring_protocol, arm, w_fte = fte, w_div = industry_division,
                                   w_komm = get(KCOL), w_as = is_as, w_age = age_y)]
pk <- merge(pk[treatment == "control"], pw, by = c("ev", "scoring_protocol", "arm"))
picked <- pk[, .(controls      = .N,
                 firms         = uniqueN(cvr),
                 med_log_gap   = round(median(abs(pos_log(fte) - pos_log(w_fte)), na.rm = TRUE), 3),
                 med_age_gap   = round(median(abs(log1p(age_y) - log1p(w_age)), na.rm = TRUE), 3),
                 same_div      = round(mean(industry_division == w_div, na.rm = TRUE), 3),
                 same_komm     = round(mean(get(KCOL) == w_komm, na.rm = TRUE), 3),
                 as_controls   = round(mean(is_as), 3),
                 as_winners    = round(mean(w_as), 3)), by = .(scoring_protocol, arm)][order(arm, scoring_protocol)]
cat("\n  who each protocol picked (controls at k = -2, against their own winner):\n")
print(picked)

# ---- write ------------------------------------------------------------------------------------------------
match_rule_banner("6. write")
report <- list(run_at = Sys.time(), h = H, min_rung = MIN_RUNG, arms = ARMS, protocols = PROTOS,
               rule = RULES[1], komm_col = KCOL, n_study_events = nrow(study), n_matched_events = matched_ev,
               discards = discards, ties = tie, groups_failed = n_err + n_null,
               elig_funnel = eligfunnel,
               ps_topk = if (length(PS_PROTOS)) PS_K else NA_integer_, ps_seed = PS_SEED,
               ps_model_run_at = if (length(PS_PROTOS)) lapply(PSM, `[[`, "run_at") else NULL,   # by protocol
               ps_skips = ps_skips, picked = picked)
write_obj(match_table, P$match_table)
write_obj(report,      P$match_report)
write_tab(mp,          P$matched_panel)
cat(sprintf("  -> %s | %s | %s (%.0f MB)\n", basename(P$match_table), basename(P$match_report),
            basename(P$matched_panel), file.size(P$matched_panel) / 1e6))
cat("\nSTAGE 2 complete.\n")
