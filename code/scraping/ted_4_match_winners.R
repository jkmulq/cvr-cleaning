# Match missing TED winner CVRs using the registered CVR names, and grade the
# winner-name quality against the registry - so the TED winner dataset is directly
# comparable to KFST/OpenTender on name-matching + quality scores.
#
# This mirrors code/processing/2_3_match_opentender.R: build the CVR-name key, then
# exact steps 1-4, fuzzy steps 5-6, assemble winner_cvr_final, and the PART-8 quality
# scoring + PART-9 invalid-CVR provenance flag. The OpenTender consortium-removal and
# name-partition splitting are OMITTED: TED parties are already one organisation per
# row (the XML splits consortia), so there are no combined names to separate.
#
# INPUT  data/intermediates/ted/ted_winner_data.rds  (from ted_3), + the CVR-name keys.
# OUTPUT data/intermediates/ted/ted_winner_data_name_matched.rds
#        data/intermediates/ted/manual_name_review_ted_winner.rds

rm(list = ls())
source("config.R")
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
}))
source(file.path(PROJECT_DIR, "code", "functions.R"))

clean_data_dir <- dirs$clean_data
ted_dir        <- file.path(dirs$intermediates, "ted")

# 1 Load the TED winners and the CVR-name keys (as in 2_3 section 1)
winner_data <- readRDS(file.path(ted_dir, "ted_winner_data.rds"))
name_key    <- readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds"))
biname_key  <- readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds"))
setDT(winner_data); setDT(name_key); setDT(biname_key)

setnames(name_key, "name", "registered_name")
setnames(biname_key, "binavn", "registered_name")
name_key[, name_source := "name"]
biname_key[, name_source := "biname"]
name_key[, cvr := sprintf("%08d", as.integer(cvr))]
biname_key[, cvr := sprintf("%08d", as.integer(cvr))]
name_key[, broad_first_letter := substr(name_broad, 1, 1)]
biname_key[, broad_first_letter := substr(name_broad, 1, 1)]
cvr_key <- rbindlist(list(name_key, biname_key), use.names = TRUE)
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]

# 1.5 Prepare the winner names + the matching flag (the prep 1_2 does for OT). The
#     XML-extracted clean CVR plays the role of winner_cvr_clean; the raw id is kept
#     as winner_cvr_candidate for the invalid-provenance flag.
winner_data[, winner_cvr_clean   := winner_cvr]
winner_data[, winner_cvr_candidate := winner_cvr_raw]
# The match-context helpers expect the KFST/OT key names; map them from the TED keys
# (notice_id is the tender; winner_number indexes the winning orgs within a lot).
winner_data[, tender_id     := notice_id]
winner_data[, winner_number := rowid(notice_id, lot)]
wp <- prepare_cvr_name(winner_data$winner_name)
winner_data[, `:=`(
  winner_name_basic     = wp$name_basic,
  winner_name_match     = wp$name_clean,
  winner_name_no_spaces = wp$name_no_spaces,
  winner_name_broad     = wp$name_broad,
  winner_firm_type      = wp$firm_type,
  first_letter          = substr(wp$name_clean, 1, 1),
  broad_first_letter    = substr(wp$name_broad, 1, 1))]
winner_data[, flag_matching_candidate :=
  !is.na(winner_name) & winner_name != "" & (is.na(winner_cvr_clean) | winner_cvr_clean == "")]

# 2 Filter (as in 2_3 section 2). Country mapping is deferred, so accept DK and the
#    eForms ISO3 form DNK. Match date = the TED contract-award date.
winner_data[, match_row_id := .I]
winner_data[, winner_name_in_data := winner_name]
remaining <- winner_data[
  flag_matching_candidate & grepl("DK|DNK", toupper(trimws(winner_country)))]
cat("Number observations to fuzzy match:", nrow(remaining), "\n")
remaining[, match_date := as.IDate(date_contract_award)]
remaining_original <- remaining

matched <- data.table(
  match_row_id            = integer(0),
  cvr_name_match          = character(0),
  registered_name_match   = character(0),
  name_match_source       = character(0),
  name_match_step         = integer(0),
  name_match_method       = character(0),
  name_match_score        = numeric(0),
  name_match_n_candidates = integer(0))

# 3 Exact matching (verbatim from 2_3 section 3: steps 1-4)
## 3.1 lightly prepared name + firm type
candidate_matches <- cvr_key[remaining,
  on = .(name_basic = winner_name_basic, firm_type = winner_firm_type),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 1L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 1 matches:", nrow(new_matches), "\n")

## 3.2 generalized name without spaces, retaining firm type
candidate_matches <- cvr_key[remaining,
  on = .(name_no_spaces = winner_name_no_spaces, firm_type = winner_firm_type),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 2L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 2 matches:", nrow(new_matches), "\n")

## 3.3 generalized name without spaces, ignoring firm type
candidate_matches <- cvr_key[remaining,
  on = .(name_no_spaces = winner_name_no_spaces),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 3L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 3 matches:", nrow(new_matches), "\n")

## 3.4 broad name, ignoring firm type
candidate_matches <- cvr_key[remaining,
  on = .(name_broad = winner_name_broad),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 4L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 4 matches:", nrow(new_matches), "\n")
rm(new_matches); gc()
cat("total exact matches:", nrow(matched), "\n")

# Preserve the CVR -> registered-name forms for the quality scores (as in 2_3),
# before cvr_key is dropped.
cvr_key_quality <- unique(cvr_key[, .(cvr, name_match, name_basic, name_no_spaces, name_broad)])
rm(cvr_key); gc()

# 4 Fuzzy matching (verbatim from 2_3 section 6, incl. the distinct-name speed-up)
fuzzy_match_cols <- c("winner_name_match", "winner_name_broad", "winner_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id]
remaining[, match_row_id := fuzzy_match_id]
cat("No. distinct observations to fuzzy match:", nrow(remaining), "\n")

fuzzy_candidates <- data.table()
matched_prefuzzy <- copy(matched)

## 4.1 main name key, prepared name (threshold 85)
step_candidates <- find_fuzzy_matches(remaining, name_key,
  entity_name_column = "winner_name_match", key_name_column = "name_match",
  first_letter_column = "first_letter", step = 5L, firm_type_column = "winner_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 5 main matches:", nrow(new_matches), "\n")

## 4.2 biname key, prepared name (threshold 85)
step_candidates <- find_fuzzy_matches(remaining, biname_key,
  entity_name_column = "winner_name_match", key_name_column = "name_match",
  first_letter_column = "first_letter", step = 5L, firm_type_column = "winner_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 5 biname matches:", nrow(new_matches), "\n")

## 4.3 main name key, broad name (threshold 86)
step_candidates <- find_fuzzy_matches(remaining, name_key,
  entity_name_column = "winner_name_broad", key_name_column = "name_broad",
  first_letter_column = "broad_first_letter", step = 6L, firm_type_column = "winner_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 86)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 6 main matches:", nrow(new_matches), "\n")

## 4.4 biname key, broad name (threshold 89)
step_candidates <- find_fuzzy_matches(remaining, biname_key,
  entity_name_column = "winner_name_broad", key_name_column = "name_broad",
  first_letter_column = "broad_first_letter", step = 6L, firm_type_column = "winner_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 89)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 6 biname matches:", nrow(new_matches), "\n")
rm(new_matches, step_candidates, name_key, biname_key); gc()

# Expand the distinct-name fuzzy results back onto every original row (as in 2_3).
if (nrow(fuzzy_candidates) > 0) {
  setnames(fuzzy_candidates, "match_row_id", "fuzzy_match_id")
  fuzzy_candidates <- fuzzy_row_lookup[fuzzy_candidates, on = "fuzzy_match_id", allow.cartesian = TRUE]
  fuzzy_candidates[, fuzzy_match_id := NULL]
}
fuzzy_matched <- matched[name_match_method == "fuzzy"]
fuzzy_matched <- fuzzy_row_lookup[fuzzy_matched,
  on = .(fuzzy_match_id = match_row_id), allow.cartesian = TRUE][, fuzzy_match_id := NULL]
matched <- rbindlist(list(matched_prefuzzy, fuzzy_matched), use.names = TRUE, fill = TRUE)

# 5 Join matches back to the full winner data (as in 2_3 section 7, minus partitions)
if (nrow(fuzzy_candidates) > 0) {
  fuzzy_candidates[, source_order := fifelse(fuzzy_candidate_source == "name", 1L, 2L)]
  setorder(fuzzy_candidates, match_row_id, -fuzzy_candidate_score,
           fuzzy_candidate_step, source_order, fuzzy_candidate_rank)
  fuzzy_candidates <- unique(fuzzy_candidates, by = c("match_row_id", "fuzzy_candidate_cvr"))
  fuzzy_candidates <- fuzzy_candidates[, head(.SD, 5), by = match_row_id]
  fuzzy_candidates[, fuzzy_candidate_rank := seq_len(.N), by = match_row_id]
  fuzzy_candidates_wide <- dcast(fuzzy_candidates, match_row_id ~ fuzzy_candidate_rank,
    value.var = c("fuzzy_candidate_cvr", "fuzzy_candidate_name", "fuzzy_candidate_score",
                  "fuzzy_candidate_source", "fuzzy_candidate_step"))
  winner_data <- merge(winner_data, fuzzy_candidates_wide, by = "match_row_id", all.x = TRUE, sort = FALSE)
}

winner_data[matched, on = "match_row_id",
  `:=`(winner_cvr_name_match = i.cvr_name_match,
       registered_name_match = i.registered_name_match,
       name_match_source     = i.name_match_source,
       name_match_step       = i.name_match_step,
       name_match_method     = i.name_match_method,
       name_match_score      = i.name_match_score,
       name_match_n_candidates = i.name_match_n_candidates)]

# Descriptive step code (exact 1-4 + fuzzy 5-6 + source), as in 2_3 minus partitions.
winner_data[, cvr_number_source := fcase(
  name_match_method == "exact" & name_match_step == 1L, "exact: basic name and firm type",
  name_match_method == "exact" & name_match_step == 2L, "exact: no spaces and firm type",
  name_match_method == "exact" & name_match_step == 3L, "exact: no spaces",
  name_match_method == "exact" & name_match_step == 4L, "exact: broad name",
  name_match_method == "fuzzy" & name_match_step == 5L & name_match_source == "name",   "fuzzy: prepared main name",
  name_match_method == "fuzzy" & name_match_step == 5L & name_match_source == "biname", "fuzzy: prepared biname",
  name_match_method == "fuzzy" & name_match_step == 6L & name_match_source == "name",   "fuzzy: broad main name",
  name_match_method == "fuzzy" & name_match_step == 6L & name_match_source == "biname", "fuzzy: broad biname",
  !is.na(winner_cvr_clean) & winner_cvr_clean != "", "source: CVR extracted from TED XML",
  flag_matching_candidate & grepl("DK|DNK", toupper(trimws(winner_country))), "matching candidate: no match found",
  flag_matching_candidate, "not a matching candidate: not marked as Danish",
  default = "not a matching candidate: no CVR name")]

# Reliability of a matching candidate's Danish gate (as in 2_1/2_3). "exact DK" = country is exactly
# DK/DNK (most reliable); "contains DK" = a mixed value merely containing DK; NA for non-candidates.
# TED country is a single ISO code, so in practice only "exact DK" occurs here.
winner_data[, matching_candidate_type := fcase(
  flag_matching_candidate & toupper(trimws(winner_country)) %chin% c("DK", "DNK"), "exact DK",
  flag_matching_candidate & grepl("DK|DNK", toupper(trimws(winner_country))),          "contains DK",
  default = NA_character_
)]

# Flags (as in 2_3)
winner_data[, flag_name_match_found := !is.na(winner_cvr_name_match)]
winner_data[, flag_name_match_ambiguous := (flag_name_match_found & name_match_n_candidates > 1)]
winner_data[, flag_review_name_match :=
  (flag_name_match_found & (name_match_method == "fuzzy" | flag_name_match_ambiguous))]
winner_data[, flag_manual_name_review :=
  (flag_matching_candidate & (!flag_name_match_found | flag_review_name_match))]

# Final CVR: the XML-extracted CVR, else the name-matched CVR for Danish candidates.
winner_data[, winner_cvr_final := as.character(winner_cvr_clean)]
winner_data[flag_matching_candidate & grepl("DK|DNK", toupper(trimws(winner_country))) &
              !is.na(winner_cvr_name_match),
            winner_cvr_final := winner_cvr_name_match]

winner_data[, name_match_status := fcase(
  !flag_matching_candidate, "not requested",
  flag_review_name_match,  "manual review - fuzzy or ambiguous match",
  flag_name_match_found,   "matched",
  is.na(winner_country) | !grepl("DK|DNK", toupper(trimws(winner_country))),
  "manual review - not marked as Danish",
  default = "manual review - no automatic match")]

# 6 Data quality (verbatim from 2_3 PART-8): winner name vs the final CVR's
#    registered names, for each prepared name form.
for (qf in list(c("winner_name_match",     "name_match",     "cvr_name_match_quality"),
                c("winner_name_basic",     "name_basic",     "cvr_name_match_quality_basic"),
                c("winner_name_no_spaces", "name_no_spaces", "cvr_name_match_quality_nospaces"),
                c("winner_name_broad",     "name_broad",     "cvr_name_match_quality_broad"))) {
  win_col <- qf[1]; key_col <- qf[2]; q_col <- qf[3]
  reg_lookup <- unique(data.table(cvr = as.character(cvr_key_quality$cvr),
                                  reg_name = cvr_key_quality[[key_col]]))[!is.na(reg_name) & reg_name != ""]
  qual <- data.table(match_row_id = winner_data$match_row_id,
                     cvr = as.character(winner_data$winner_cvr_final),
                     win_name = winner_data[[win_col]])
  qual <- qual[!is.na(cvr) & cvr != "" & !is.na(win_name) & win_name != ""]
  qual <- merge(qual, reg_lookup, by = "cvr", allow.cartesian = TRUE)
  qual[, score := levenshtein_ratio(win_name, reg_name, pairwise = TRUE)]
  setorder(qual, match_row_id, -score)
  best_qual <- qual[, .SD[1L], by = match_row_id]
  winner_data[, (q_col) := NA_real_]
  winner_data[best_qual, on = "match_row_id", (q_col) := i.score]
  if (q_col == "cvr_name_match_quality") {
    winner_data[, cvr_name_match_quality_name := NA_character_]
    winner_data[best_qual, on = "match_row_id", cvr_name_match_quality_name := i.reg_name]
  }
}
winner_data[, cvr_name_is_substring := NA]
winner_data[!is.na(cvr_name_match_quality_name) & !is.na(winner_name_match) & winner_name_match != "",
            cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(winner_name_match))]

# 7 Document CVRs recovered from an invalid raw id (verbatim from 2_3 PART-9)
reg_cvrs_prov <- unique(as.character(cvr_key_quality$cvr))
cand_prov <- ifelse(is.na(winner_data$winner_cvr_candidate), "",
                    gsub("\\s+", "", as.character(winner_data$winner_cvr_candidate)))
cand_has_reg_prov <- vapply(regmatches(cand_prov, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", cand_prov, perl = TRUE)),
                            function(v) any(v %chin% reg_cvrs_prov), logical(1))
winner_data[, flag_cvr_recovered_from_invalid :=
  cand_prov != "" & !cand_has_reg_prov & !is.na(winner_cvr_final) & as.character(winner_cvr_final) != ""]

# Registry membership of the FINAL CVR: TRUE iff winner_cvr_final is a CVR present in the registry
# name key -- independent of HOW it was obtained (field / backfill / name match). Distinct from
# valid_cvr (format check only) and from flag_cvr_recovered_from_invalid (about the original
# candidate, not the final). NA / blank final CVRs are FALSE.
winner_data[, flag_cvr_final_in_registry :=
  !is.na(winner_cvr_final) & as.character(winner_cvr_final) %chin% reg_cvrs_prov]

# ============================================================================
# 7b Shared-schema parity with the KFST/OpenTender matched winner datasets.
# Mirrors the derivations in 1_2 (cleaning) / 2_3 (matching) so the TED build
# carries the same columns. TED-specific source columns (amount_awarded,
# date_contract_award, cpv_main, ...) are kept AND aliased to the shared names.
# Lineage dates (planning_*/competition_*/award_*) and the annualised amounts
# depend on the notice-date panel + competition-notice duration and are added
# in ted_4b (part 7c below reserves the columns).
# ============================================================================

# --- Core field aliases (same data, shared names) ---
winner_data[, `:=`(
  lot_number             = as.character(lot),
  award_date             = as.Date(date_contract_award),
  submit_date            = as.Date(date_receipt_tenders),
  n_bids_received        = as.character(n_tenders_received),
  n_bidders              = suppressWarnings(as.numeric(n_tenders_received)),
  tender_amount          = as.numeric(amount_awarded),
  lot_amount             = as.numeric(lot_awarded_value),
  divided_tender         = fifelse(fcoalesce(n_lots > 1L, FALSE), "yes", "no"),
  joint_tender           = NA_character_,   # TED has no joint-procurement field
  consortium_winner      = NA_character_,   # TED splits consortia into rows; no source flag
  tender_cancelled       = FALSE,           # winner rows come from AWARDED_CONTRACT lots only
  flag_awarded           = TRUE,            # ditto -- every winner row is an awarded lot
  ted_notice_id          = as.character(notice_id),
  winner_cvr_original    = as.character(winner_cvr_raw),
  winner_name_original   = winner_name_in_data,
  winner_country_original = winner_country,
  winner_name_first_letter = first_letter
)]

# --- CPV groupings (clean_cpv_code handles CPV 2003/2008, as in 1_2) ---
cpv_prepared <- clean_cpv_code(winner_data$cpv_main)
winner_data[, `:=`(
  cpv_code          = cpv_main,
  cpv_code_first    = cpv_prepared$code_first,
  cpv_division      = cpv_prepared$division,
  cpv_division_name = cpv_prepared$division_name,
  cpv_sector        = cpv_prepared$sector,
  cpv_category      = cpv_prepared$category
)]

# Shared-schema `contract_type` = framework-vs-public (as in KFST/OT). TED's extracted contract_type is
# the CPV contract NATURE (services/supplies/works) -- keep that under `contract_nature` and rederive
# `contract_type` from is_framework so it agrees with KFST/OT and variable_descriptions.md.
setnames(winner_data, "contract_type", "contract_nature")
winner_data[, contract_type := fcase(
  is_framework %in% TRUE,  "Framework agreement",
  is_framework %in% FALSE, "Public contract",
  default = NA_character_)]

# --- Amount fill + equal-split across a notice's lots (mirrors 1_2) ---
# Fill a missing tender amount with the sum over its distinct lots when every lot
# amount is present; then, if ALL lot amounts are missing, split the tender amount
# equally across the notice's lots.
winner_data[, tender_amount := {
  if (all(is.na(tender_amount)) && all(!is.na(lot_amount)))
    sum(lot_amount[!duplicated(lot_id)]) else tender_amount
}, by = notice_id]
winner_data[, lot_amount_orig := lot_amount]
winner_data[, flag_all_orig_lot_amt_missing := all(is.na(lot_amount_orig)), by = notice_id]
winner_data[, lot_amount := fifelse(flag_all_orig_lot_amt_missing,
                                    tender_amount / uniqueN(lot_id), lot_amount_orig),
            by = notice_id]

# --- Currency counterparts. TED amounts are in the notice's original `currency`
# (multi-currency), so ONLY the exact EUR<->DKK peg is applied; other currencies
# are left NA (amounts are deprioritised -- see project notes). ---
dkk_per_eur <- 7.46038
winner_data[, .ccy := toupper(trimws(currency))]
winner_data[, `:=`(
  tender_amount_eur = fcase(.ccy == "EUR", tender_amount,
                            .ccy == "DKK", tender_amount / dkk_per_eur, default = NA_real_),
  tender_amount_dkk = fcase(.ccy == "DKK", tender_amount,
                            .ccy == "EUR", tender_amount * dkk_per_eur, default = NA_real_),
  lot_amount_eur    = fcase(.ccy == "EUR", lot_amount,
                            .ccy == "DKK", lot_amount / dkk_per_eur, default = NA_real_),
  lot_amount_dkk    = fcase(.ccy == "DKK", lot_amount,
                            .ccy == "EUR", lot_amount * dkk_per_eur, default = NA_real_)
)]
winner_data[, .ccy := NULL]

# --- valid_cvr: syntactic 8-digit validity of the cleaned CVR (as in cleaning) ---
winner_data[, valid_cvr :=
  !is.na(winner_cvr_clean) & grepl("^[0-9]{8}$", as.character(winner_cvr_clean)) &
  !(as.character(winner_cvr_clean) %chin% known_invalid_cvr_numbers())]

# --- CVR formatting-cleanup flags: what changed from the raw id (winner_cvr_raw,
# carried as winner_cvr_candidate) to the cleaned CVR (mirrors 1_2). ---
winner_data[, flag_cvr_ws       := !is.na(winner_cvr_candidate) & str_detect(winner_cvr_candidate, "\\s")]
winner_data[, flag_cvr_alphabet := !is.na(winner_cvr_candidate) & str_detect(winner_cvr_candidate, "[[:alpha:]]")]
winner_data[, flag_cvr_punct    := !is.na(winner_cvr_candidate) & str_detect(winner_cvr_candidate, "[[:punct:]]")]
winner_data[, flag_cvr_standardised := flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct]

# --- Same-name borrow columns: TED does not borrow CVRs across rows -> constant ---
winner_data[, flag_borrowed_cvr := FALSE]
winner_data[, winner_cvr_valid_from_same_name := NA_character_]

# --- Context flags ---
winner_data[, flag_single_bidder := fcoalesce(suppressWarnings(as.numeric(n_tenders_received)) == 1, FALSE)]
winner_data[, flag_multilot       := fcoalesce(n_lots > 1L, FALSE)]
winner_data[, flag_cancelled      := FALSE]

# --- Pre-match CVR review flags (on the cleaned CVR), mirroring 1_2 ---
winner_data[, flag_missing_winner_cvr     := is.na(winner_cvr_clean) | winner_cvr_clean == ""]
winner_data[, flag_missing_winner_name    := is.na(winner_name) | winner_name == ""]
winner_data[, flag_missing_winner_country := is.na(winner_country) | winner_country == ""]
winner_data[, flag_foreign_winner :=
  !is.na(winner_country) & !(grepl("DK|DNK", toupper(trimws(winner_country))))]
winner_data[, flag_missing_cvr_with_name := flag_missing_winner_cvr & !flag_missing_winner_name]
winner_data[, flag_review_cvr            := !flag_missing_winner_cvr & !valid_cvr]
winner_data[, flag_no_winner_info :=
  flag_missing_winner_cvr & flag_missing_winner_name & flag_missing_winner_country]
winner_data[, flag_verify_cvr_external := fcase(
  flag_missing_cvr_with_name, TRUE,
  flag_review_cvr,            TRUE,
  default = FALSE
)]

# --- Post-match ("_final") CVR review flags, on winner_cvr_final (mirrors 2_3) ---
winner_data[, flag_missing_winner_cvr_final := is.na(winner_cvr_final) | winner_cvr_final == ""]
winner_data[, flag_missing_cvr_with_name_final :=
  flag_missing_winner_cvr_final & !(is.na(winner_name) | winner_name == "")]
winner_data[, flag_review_cvr_final :=
  !flag_missing_winner_cvr_final & !flag_cvr_final_in_registry]
winner_data[, flag_no_winner_info_final :=
  flag_missing_winner_cvr_final & flag_missing_winner_name & flag_missing_winner_country]
winner_data[, flag_verify_cvr_external_final := fcase(
  flag_missing_cvr_with_name_final, TRUE,
  flag_review_cvr_final,            TRUE,
  default = FALSE
)]

# 7c Lineage dates + annualised amounts from the TED notice-date panel (built by the
#    ted_dates_* chain: award->competition->planning dates + the framework duration
#    from the competition notice, keyed by the award notice = TED notice_id). This
#    mirrors the OT/KFST lineage-date join; first-time replication builds the panel.
ted_panel_file <- file.path(ted_dir, "ted_notice_dates.rds")
if (!file.exists(ted_panel_file)) {
  source(file.path(PROJECT_DIR, "code", "scraping", "ted_dates_1_fetch.R"))
  source(file.path(PROJECT_DIR, "code", "scraping", "ted_dates_2_lineage.R"))
  source(file.path(PROJECT_DIR, "code", "scraping", "ted_dates_3_extract.R"))
  source(file.path(PROJECT_DIR, "code", "scraping", "ted_dates_5_panel.R"))
}
ted_dates_panel <- as.data.table(readRDS(ted_panel_file))
lineage_date_cols <- c(
  "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date",
  "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
  "award_dispatch_date", "award_publication_date", "award_tender_deadline_date", "award_contract_date")
winner_data <- merge(
  winner_data,
  ted_dates_panel[, c("notice_id", lineage_date_cols, "framework_duration_days"), with = FALSE],
  by = "notice_id", all.x = TRUE, sort = FALSE)

# Annualise framework amounts: total / duration_days * 365, for frameworks with a positive duration
# (the > 0 guard avoids divide-by-zero). Non-frameworks / missing duration stay NA (as in 1_2).
winner_data[, annualised_tender_amount := fifelse(
  is_framework %in% TRUE & !is.na(framework_duration_days) & framework_duration_days > 0,
  tender_amount / framework_duration_days * 365, NA_real_)]
winner_data[, annualised_lot_amount := fifelse(
  is_framework %in% TRUE & !is.na(framework_duration_days) & framework_duration_days > 0,
  lot_amount / framework_duration_days * 365, NA_real_)]
winner_data[, framework_duration_days := NULL]

# Rows for manual review
manual_name_review <- winner_data[flag_manual_name_review == TRUE,
  .(notice_id, lot_id, lot, winner_name_in_data, winner_name, winner_name_match, winner_firm_type,
    winner_country, date_contract_award, winner_cvr_name_match, registered_name_match,
    name_match_step, cvr_number_source, name_match_method, name_match_score,
    name_match_n_candidates, flag_name_match_found, flag_name_match_ambiguous,
    flag_review_name_match, flag_manual_name_review, name_match_status)]

winner_data[, match_row_id := NULL]

# Harmonise shared output column types with KFST / OpenTender (lot_id as character, not the raw integer).
winner_data[, lot_id := as.character(lot_id)]
winner_data[, winner_number := suppressWarnings(as.integer(winner_number))]

# 8 Save
saveRDS(winner_data, file.path(ted_dir, "ted_winner_data_name_matched.rds"))
fwrite(winner_data,  file.path(ted_dir, "ted_winner_data_name_matched.csv"))
saveRDS(manual_name_review, file.path(ted_dir, "manual_name_review_ted_winner.rds"))

# Diagnostics
cat(sprintf("\nTED winners: %d rows | winner_cvr_final on %.0f%% | name-matched %d | quality graded %d\n",
            nrow(winner_data), 100 * mean(!is.na(winner_data$winner_cvr_final)),
            winner_data[flag_name_match_found == TRUE, .N],
            winner_data[!is.na(cvr_name_match_quality), .N]))
