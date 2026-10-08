#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 1b -- fit the bidding-propensity model the pscore protocol matches on (2_match_controls.R with
# MATCH_PROTOCOLS including pscore; 5_cobidder_stacks.R with MATCH_SCORE=pscore). Not needed otherwise.
#
# THE QUESTION THE MODEL ANSWERS: given a winner, which other firms bid against it and lost? One stratum
# per (TED tender, winner). y = 1 for a firm that lost to that winner on a lot it won, y = 0 for a random
# sample of the firms that did not bid. Winners are the reference, never rows. Conditional logit with the
# stratum absorbed (survival::clogit), clustered by firm. The characteristics, all at t - 2 (t = the
# winner's award quarter), are defined ONCE in 0_matching_utils.R (PSCORE_VARIANTS, pscore_features()),
# so the matchers score candidates on exactly what was fitted here.
#
# ONE VARIANT PER RUN (MATCH_PS_VARIANT): pscore (default; the 0/1 characteristics -- industry, legal
# form, same kommune as a buyer and as the winner -- + the gap in average log FTE over t-8 .. t-1), or the
# opt-in pscore_fte (log FTE at t-2 and growth t-6 -> t-2 instead), pscore_age (the firm-age gap instead of
# size) or pscore_nofte (the 0/1 characteristics only). Each writes its own model
# file, so fitting one never touches the other.
# PS_TRAIN_H is a screen, not a characteristic: it applies to every variant.
#
# TRAINING MIRRORS MATCHING. Every firm in a stratum comes from the matching universe -- the never-winner
# pool and every competitive winner, stage 2's two arms -- and must clear a matching candidate's
# pre-period screen: positive FTE in every quarter t-PS_TRAIN_H .. t-1, and every characteristic known.
# Two deliberate differences from matching: NO winner buffer (a losing bidder that recently won
# elsewhere stays in -- the buffer protects the event study, it says nothing about who bids), and NO
# post-period test (losers are 2024+, so most have no post-period yet). Ones and zeros pass identical
# screens. A loser that is not in the matching universe cannot be a row; the funnel counts them.
#
# CASE-CONTROL SAMPLING. The zeros are PS_TRAIN_NB firms drawn at random per stratum from the universe,
# minus anyone named on the tender and the tender's buyers. Sampling zeros at random WITHIN a stratum
# leaves the clogit slopes consistent; it only costs precision.
#
# DATA LIMIT. TED names losing bidders only in eForms notices, 2024 onward
# (code/scraping/ted_2_extract_party_cvrs.R), so every stratum is 2024+, while the matchers apply the
# model to every event, 2006-2026, from all three sources.
#
# SECOND ENTRY POINT, DELIBERATELY (as stage 5): this reads the combined dataset for the TED
# winner/non-winner lots, a TED-only slice nothing else needs. Its other inputs are stage 1's.
#
# Reads:   <clean>/tender_data_2006_2026.*, 01_firm_panel.parquet, 01_eligible_controls.rds,
#          01_buyers.rds
# Writes:  01b_<variant>_model.rds    coefficients, robust vcov, fit statistics, the funnel, how the real
#                                     losers rank among ALL non-bidders, settings. Read by stages 2 and 5
#                                     (read_pscore_model()); 16_bidding_predictors_augmented.Rmd reads
#                                     01b_pscore_model.rds
#          01b_<variant>_train.parquet   the training rows, for audit
#
#   Rscript code/matching/1b_fit_pscore.R
#   MATCH_PS_VARIANT=pscore_fte Rscript code/matching/1b_fit_pscore.R      # FTE at t-2 + growth, opt-in
#   MATCH_PS_VARIANT=pscore_age Rscript code/matching/1b_fit_pscore.R      # the no-FTE age model, opt-in
#   MATCH_PS_VARIANT=pscore_nofte Rscript code/matching/1b_fit_pscore.R    # neither FTE nor age, opt-in
# Options (env):
#   MATCH_PS_VARIANT  the model to fit, a name in PSCORE_VARIANTS (default pscore)
#   PS_TRAIN_H     pre-period quarters a firm must have positive FTE in (default 8, stage 2's default h)
#   PS_TRAIN_NB    non-bidders sampled per stratum (default 500)
#   PS_SEED        seed for that sample (default 20261005)
#   MATCH_TENDER_FILE, MATCH_TEST_N (sets the artefact tag only), MATCH_OVERWRITE
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))
match_setup(extra_libs = "survival")

HT   <- match_env_int("PS_TRAIN_H", 8L)
NB   <- match_env_int("PS_TRAIN_NB", 500L)
SEED <- match_env_int("PS_SEED", 20261005L)
VARIANT <- match_env_chr("MATCH_PS_VARIANT", "pscore")
PV      <- ps_variant(VARIANT)               # its characteristics, spec, and what a scorable firm needs
NEED_AGE <- "a2" %in% PV$needs
NEED_AVG <- "lbar" %in% PV$needs           # pscore: average log FTE over t-8 .. t-1
P    <- match_paths()
cat(sprintf("STAGE 1b | variant %s (%s) | pre-period screen %d quarters | %d non-bidders per stratum | seed %d\n",
            VARIANT, paste(PV$features, collapse = ", "), HT, NB, SEED))

# ---- 1. TED lots and the strata ------------------------------------------------------------------------
# The lot filters are stage 5's (5_cobidder_stacks.R section 2); then 16's rule that every lot of the
# tender names a losing bidder -- on a lot without one the losers are unknown and could sit among the zeros.
match_rule_banner("1. TED lots with named losing bidders")
cmb <- as.data.table(read_clean(match_env_chr("MATCH_TENDER_FILE",
                                              file.path(dirs$clean_data, "tender_data_2006_2026"))))
wn <- cmb[data_source == "TED" & entity %chin% c("winner", "non-winner") &
            !is.na(cvr_final) & cvr_final != "" &
            !is.na(tender_id) & tender_id != "" & !is.na(lot_id) & lot_id != ""]
rm(cmb); invisible(gc())
wn[, cvr := as_cvr8(cvr_final)]
wn <- wn[!is.na(cvr)]
wn[, tender_id := as.character(tender_id)]
wn[, award_date := as.Date(award_date)]
wn <- wn[entity == "non-winner" | !is.na(award_date)]           # a winner needs a date to define t
# A winner also listed as a bidder on its own lot is not one of that lot's losers
wn[, is_dual := ("winner" %in% entity) & ("non-winner" %in% entity), by = .(tender_id, lot_id, cvr)]
wn <- wn[!(entity == "non-winner" & is_dual == TRUE)]
wn[, has_loser := any(entity == "non-winner"), by = .(tender_id, lot_id)]
wn[, every_lot := all(has_loser), by = tender_id]
n_tend_all <- wn[has_loser == TRUE, uniqueN(tender_id)]
wn <- wn[every_lot == TRUE]

# Strata: one per (tender, winner); its quarter tq is the winner's earliest award on the tender.
won    <- unique(wn[entity == "winner", .(tender_id, lot_id, w_cvr = cvr, award_date)])
strata <- won[, .(award_date = min(award_date)), by = .(tender_id, w_cvr)]
strata[, tq := qidx_of(year(award_date), quarter(award_date))]
setorder(strata, tq, tender_id, w_cvr)
strata[, stratum := .I]

# The ones: firms that lost on a lot this winner won. A firm that won ANY lot of the tender is a winner
# there, never a loser. Everyone named on the tender is excluded from the zeros.
lost    <- unique(wn[entity == "non-winner", .(tender_id, lot_id, cvr)])
lost_to <- unique(merge(lost, won[, .(tender_id, lot_id, w_cvr)], by = c("tender_id", "lot_id"),
                        allow.cartesian = TRUE)[, .(tender_id, w_cvr, cvr)])
lost_to <- lost_to[!unique(won[, .(tender_id, cvr = w_cvr)]), on = .(tender_id, cvr)]
named   <- unique(wn[, .(tender_id, cvr)])
setkey(lost_to, tender_id, w_cvr); setkey(named, tender_id)

# The tender's competition-notice quarter, where TED has one -- only to report how t - 2 sits against it.
notice_q <- if ("competition_publication_date" %in% names(wn))
  wn[!is.na(competition_publication_date),
     .(nq = min(qidx_of(year(as.Date(competition_publication_date)), quarter(as.Date(competition_publication_date))))),
     by = tender_id] else data.table(tender_id = character(), nq = integer())

cat(sprintf("  TED tenders with a named losing bidder: %d | every lot names one: %d\n",
            n_tend_all, uniqueN(wn$tender_id)))
cat(sprintf("  strata (tender x winner): %d | losing-bidder links: %d | award quarters: %d\n",
            nrow(strata), nrow(lost_to), uniqueN(strata$tq)))

# ---- 2. the matching universe ---------------------------------------------------------------------------
# Stage 2's two arms as one table (pool_side), with the columns the characteristics need.
match_rule_banner("2. the matching universe")
panel <- read_tab(P$firm_panel)
for (cc in c("legal_form_short", "kommune_code", if (NEED_AGE) "registration_date"))
  if (!cc %in% names(panel)) stop("the firm panel has no ", cc, " -- re-run stage 1", call. = FALSE)
# Firm-level founding dates, for the age at t-2 (pscore_age only)
FOUNDED <- if (NEED_AGE) unique(panel[!is.na(registration_date), .(cvr, registration_date)]) else NULL
eligible  <- as.data.table(read_obj(P$eligible))
BUY       <- read_obj(P$buyers)
elig_cvrs <- eligible[has_employment == TRUE, unique(cvr)]
pool <- panel[, .(cvr, qidx, fte, firm_type, industry_division, industry_class, legal_form_short, kommune_code)]
rm(panel); invisible(gc())
pool[, pool_side := fifelse(firm_type == "winner", "winner",
                     fifelse(cvr %chin% elig_cvrs, "never_winner", NA_character_))]
pool <- pool[!is.na(pool_side)]
setkey(pool, qidx)
cat(sprintf("  pool: %d rows | never_winner %d firms | winner %d firms\n", nrow(pool),
            pool[pool_side == "never_winner", uniqueN(cvr)], pool[pool_side == "winner", uniqueN(cvr)]))

TB <- BUY$tender_buyers[data_source == "TED"]
setkey(TB, tender_id)

# The universe of one award quarter tq, shared by every stratum in it.
# returns: list(fv = every pool firm's values at tq-2 (the winner side reads this),
#               U  = the firms that clear the screens: positive FTE in every quarter tq-HT .. tq-1 and
#                    every value the variant needs known)
universe_at <- function(tq) {
  pre <- pool[.(seq.int(tq - HT, tq - 1L)), nomatch = 0L]
  ok  <- pre[!is.na(fte) & fte > 0, .N, by = cvr][N == HT, cvr]
  f2  <- pool[.(tq - PSCORE_LAGS[1]), .(cvr, pool_side, fte2 = fte, industry_division, industry_class,
                                        legal_form_short, kommune_code), nomatch = 0L]
  f6  <- pool[.(tq - PSCORE_LAGS[2]), .(cvr, fte6 = fte), nomatch = 0L]
  fr  <- merge(f2, f6, by = "cvr", all.x = TRUE)
  if (NEED_AGE) pscore_add_age(fr, FOUNDED, tq - PSCORE_LAGS[1])
  if (NEED_AVG) pscore_add_avg(fr, pool, tq)
  fv  <- pscore_firm_vars(fr)
  list(fv = fv, U = fv[cvr %chin% ok & ps_scorable(fv, VARIANT)])
}

# One stratum's inputs: the winner row, its losers, and who may never be a zero.
stratum_inputs <- function(s, u) list(
  w    = u$fv[cvr == s$w_cvr],
  pos  = lost_to[.(s$tender_id, s$w_cvr), cvr, nomatch = 0L],
  excl = unique(c(named[.(s$tender_id), cvr, nomatch = 0L], TB[.(s$tender_id), buyer_cvr, nomatch = 0L])))

# ---- 3. the training rows -------------------------------------------------------------------------------
match_rule_banner("3. training rows")
set.seed(SEED)
rows <- list(); skips <- list(); fun <- list()
for (tq_i in sort(unique(strata$tq))) {
  u  <- universe_at(tq_i)
  st <- strata[tq == tq_i]
  bk <- buyer_kommunes(st[, .(key = stratum, data_source = "TED", tender_id, q = tq_i - PSCORE_LAGS[1])], BUY)
  for (i in seq_len(nrow(st))) {
    s  <- st[i]
    si <- stratum_inputs(s, u)
    why <- if (nrow(si$w) != 1L) "winner not in the matching universe at t-2" else
           if (!ps_winner_ok(si$w, VARIANT)) PV$w_skip else NA_character_
    if (!is.na(why)) { skips[[length(skips) + 1L]] <- data.table(stratum = s$stratum, reason = why); next }

    ones <- u$U[cvr %chin% si$pos]
    z    <- which(!(u$U$cvr %chin% si$excl))                    # positions of every possible zero
    fun[[length(fun) + 1L]] <- data.table(stratum = s$stratum, losers_listed = length(si$pos),
                                          losers_in_universe = sum(si$pos %chin% u$fv$cvr),
                                          losers_usable = nrow(ones), zeros_available = length(z))
    if (!nrow(ones) || !length(z)) {
      skips[[length(skips) + 1L]] <- data.table(stratum = s$stratum,
        reason = if (!nrow(ones)) "no losing bidder clears the screens" else "no non-bidder clears the screens")
      next
    }
    zeros <- u$U[z[sample.int(length(z), min(length(z), NB))]]
    x <- rbind(ones[, y := 1L], zeros[, y := 0L])
    pscore_features(x, si$w, bk[key == s$stratum, buyer_kommune], VARIANT)
    x[, `:=`(stratum = s$stratum, tender_id = s$tender_id, w_cvr = s$w_cvr, tq = tq_i)]
    rows[[length(rows) + 1L]] <- x
  }
  cat(sprintf("    quarter %d: %d strata\n", tq_i, nrow(st)))
}
tr    <- rbindlist(rows, use.names = TRUE)
skips <- rbindlist(skips, use.names = TRUE, fill = TRUE)
if (!nrow(skips)) skips <- data.table(stratum = integer(), reason = character())   # keep the columns
fun   <- rbindlist(fun, use.names = TRUE)
if (!nrow(tr)) stop("no stratum produced training rows -- see the skip reasons", call. = FALSE)
stopifnot(tr[, .(o = any(y == 1L), z = any(y == 0L)), by = stratum][, all(o & z)],
          !anyDuplicated(tr, by = c("stratum", "cvr")))
cat(sprintf("  training rows: %d | strata %d | tenders %d | losing bidders %d | non-bidders %d\n",
            nrow(tr), uniqueN(tr$stratum), uniqueN(tr$tender_id), tr[y == 1L, .N], tr[y == 0L, .N]))
if (nrow(skips)) { cat("  strata skipped:\n"); print(skips[, .N, by = reason][order(-N)]) }

# ---- 4. fit -------------------------------------------------------------------------------------------
match_rule_banner("4. fit")
fml <- reformulate(c(PV$features, "strata(stratum)"), response = "y")
m   <- clogit(fml, data = tr, cluster = cvr, method = "efron")
b   <- coef(m)
if (!identical(names(b), PV$features) || !all(is.finite(b)))
  stop("the fit did not return one finite coefficient per characteristic (a characteristic may be\n",
       "  constant in the training rows):\n  ", paste(names(b), signif(b, 3), collapse = " | "), call. = FALSE)
print(summary(m))

# ---- 5. how the real losers rank -----------------------------------------------------------------------
# The question the matchers will ask, asked of the training strata: score EVERY non-bidder in the universe
# (no sampling), and see where each real loser would have stood. In-sample, so it flatters the model.
#   rank_union  1 = scored above every non-bidder in the universe
#   rank_arm    the same among non-bidders in the loser's own pool (never_winner / winner), i.e. the arm
#               stage 2 would rank it in (stage 2 also applies the winner buffer; this does not)
match_rule_banner("5. how the real losers rank")
# zz sorted. 1 + the non-bidders scored above + half of those tied: the expected rank under a random
# tie-break, as pscore_nofte's matcher draws. Unchanged for a continuous index, where ties are rare; with
# only 0/1 characteristics a loser tied with thousands would otherwise count as rank 1.
rank_in <- function(v, zz) 1 + length(zz) - findInterval(v, zz) +
  (findInterval(v, zz) - findInterval(v, zz, left.open = TRUE)) / 2
rk <- list()
for (tq_i in sort(unique(tr$tq))) {
  u  <- universe_at(tq_i)
  st <- unique(tr[tq == tq_i, .(stratum, tender_id, w_cvr)])
  bk <- buyer_kommunes(st[, .(key = stratum, data_source = "TED", tender_id, q = tq_i - PSCORE_LAGS[1])], BUY)
  for (i in seq_len(nrow(st))) {
    s  <- st[i]
    si <- stratum_inputs(s, u)
    x  <- u$U[cvr %chin% si$pos | !(cvr %chin% si$excl)]       # the losers + every possible zero
    pscore_features(x, si$w, bk[key == s$stratum, buyer_kommune], VARIANT)
    x[, `:=`(idx = pscore_index(x, b, VARIANT), y = as.integer(cvr %chin% si$pos))]
    z_all  <- sort(x[y == 0L, idx])
    z_side <- lapply(split(x[y == 0L, idx], x[y == 0L, pool_side]), sort)
    r <- x[y == 1L, .(stratum = s$stratum, cvr, pool_side, idx, n_zero = length(z_all),
                      rank_union = rank_in(idx, z_all),
                      pct_union  = findInterval(idx, z_all) / length(z_all))]
    r[, `:=`(rank_arm = mapply(function(v, ps) rank_in(v, if (is.null(z_side[[ps]])) numeric() else z_side[[ps]]),
                               idx, pool_side),
             pct_arm  = mapply(function(v, ps) { zz <- z_side[[ps]]; if (length(zz)) findInterval(v, zz) / length(zz) else NA_real_ },
                               idx, pool_side))]
    rk[[length(rk) + 1L]] <- r
  }
}
ranks <- rbindlist(rk, use.names = TRUE)
rank_summary <- rbind(
  ranks[, .(among = "all non-bidders in the universe", losers = .N,
            median_rank = as.numeric(median(rank_union)), share_top_5 = round(mean(rank_union <= 5), 3),
            share_top_10 = round(mean(rank_union <= 10), 3), share_top_50 = round(mean(rank_union <= 50), 3),
            median_percentile = round(median(pct_union), 3))],
  ranks[, .(among = paste0("non-bidders in its own pool: ", pool_side[1L]), losers = .N,
            median_rank = as.numeric(median(rank_arm)), share_top_5 = round(mean(rank_arm <= 5), 3),
            share_top_10 = round(mean(rank_arm <= 10), 3), share_top_50 = round(mean(rank_arm <= 50), 3),
            median_percentile = round(median(pct_arm, na.rm = TRUE), 3)), by = pool_side][, pool_side := NULL])
print(rank_summary)

# ---- 6. write ------------------------------------------------------------------------------------------
match_rule_banner("6. write")
funnel <- data.table(
  step = c("TED tenders with a named losing bidder",
           "  ... every lot names one",
           "strata (tender x winner)",
           "  ... winner scorable at t-2 (skips: model$skips)",
           "  ... fitted (a usable losing bidder and non-bidders)",
           "losing-bidder links (stratum x loser)",
           "  ... in strata with a scorable winner",
           "  ... in the matching universe at t-2",
           "  ... clear the screens (pre-period FTE, all characteristics)",
           "non-bidder rows sampled"),
  n = c(n_tend_all, uniqueN(wn$tender_id), nrow(strata),
        nrow(strata) - skips[grepl("^winner", reason), .N], uniqueN(tr$stratum),
        nrow(lost_to), sum(fun$losers_listed), sum(fun$losers_in_universe), sum(fun$losers_usable),
        tr[y == 0L, .N]))
print(funnel)

timing <- merge(unique(tr[, .(stratum, tender_id, tq)]), notice_q, by = "tender_id", all.x = TRUE)
timing <- timing[, .(strata = .N), by = .(quarters_from_notice_to_award = tq - nq)][order(quarters_from_notice_to_award)]

model <- list(
  variant = VARIANT, features = PV$features, spec = PV$spec, lags = PV$lags,
  coef = b, vcov = vcov(m), vcov_naive = m$naive.var, coeftable = summary(m)$coefficients,
  n = m$n, nevent = m$nevent, n_strata = uniqueN(tr$stratum), n_tenders = uniqueN(tr$tender_id),
  concordance = unname(summary(m)$concordance[1]),
  funnel = funnel, skips = skips, ranks = ranks, rank_summary = rank_summary, timing = timing,
  settings = list(train_h = HT, nb_per_stratum = NB, seed = SEED,
                  winner_buffer = "none: training keeps losers that recently won elsewhere"),
  summary_text = capture.output(print(summary(m))),
  run_at = Sys.time())
write_obj(model, P$ps_model_for(VARIANT))
write_tab(tr[, c("stratum", "tender_id", "w_cvr", "tq", "cvr", "pool_side", "y", PV$features), with = FALSE],
          P$ps_train_for(VARIANT))
cat(sprintf("  -> %s | %s\n", basename(P$ps_model_for(VARIANT)), basename(P$ps_train_for(VARIANT))))
cat("\nSTAGE 1b complete.\n")
