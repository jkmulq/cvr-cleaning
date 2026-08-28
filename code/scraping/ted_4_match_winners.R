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
winner_data[, flag_check_fuzzy_match :=
  !is.na(winner_name) & winner_name != "" & (is.na(winner_cvr_clean) | winner_cvr_clean == "")]

# 2 Filter (as in 2_3 section 2). Country mapping is deferred, so accept DK and the
#    eForms ISO3 form DNK. Match date = the TED contract-award date.
winner_data[, match_row_id := .I]
winner_data[, winner_name_in_data := winner_name]
remaining <- winner_data[
  flag_check_fuzzy_match & toupper(trimws(winner_country)) %in% c("DK", "DNK")]
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
  flag_check_fuzzy_match & toupper(trimws(winner_country)) %in% c("DK", "DNK"), "matching candidate: no match found",
  flag_check_fuzzy_match, "not a matching candidate: not marked as Danish",
  default = "not a matching candidate: no CVR name")]

# Reliability of a matching candidate's Danish gate (as in 2_1/2_3). "exact DK" = country is exactly
# DK/DNK (most reliable); "contains DK" = a mixed value merely containing DK; NA for non-candidates.
# TED country is a single ISO code, so in practice only "exact DK" occurs here.
winner_data[, matching_candidate_type := fcase(
  flag_check_fuzzy_match & toupper(trimws(winner_country)) %chin% c("DK", "DNK"), "exact DK",
  flag_check_fuzzy_match & grepl("DK", toupper(trimws(winner_country))),          "contains DK",
  default = NA_character_
)]

# Flags (as in 2_3)
winner_data[, flag_name_match_found := !is.na(winner_cvr_name_match)]
winner_data[, flag_name_match_ambiguous := (flag_name_match_found & name_match_n_candidates > 1)]
winner_data[, flag_review_name_match :=
  (flag_name_match_found & (name_match_method == "fuzzy" | flag_name_match_ambiguous))]
winner_data[, flag_manual_name_review :=
  (flag_check_fuzzy_match & (!flag_name_match_found | flag_review_name_match))]

# Final CVR: the XML-extracted CVR, else the name-matched CVR for Danish candidates.
winner_data[, winner_cvr_final := as.character(winner_cvr_clean)]
winner_data[flag_check_fuzzy_match & toupper(trimws(winner_country)) %in% c("DK", "DNK") &
              !is.na(winner_cvr_name_match),
            winner_cvr_final := winner_cvr_name_match]

winner_data[, name_match_status := fcase(
  !flag_check_fuzzy_match, "not requested",
  flag_review_name_match,  "manual review - fuzzy or ambiguous match",
  flag_name_match_found,   "matched",
  is.na(winner_country) | !toupper(trimws(winner_country)) %in% c("DK", "DNK"),
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

# Rows for manual review
manual_name_review <- winner_data[flag_manual_name_review == TRUE,
  .(notice_id, lot_id, lot, winner_name_in_data, winner_name, winner_name_match, winner_firm_type,
    winner_country, date_contract_award, winner_cvr_name_match, registered_name_match,
    name_match_step, cvr_number_source, name_match_method, name_match_score,
    name_match_n_candidates, flag_name_match_found, flag_name_match_ambiguous,
    flag_review_name_match, flag_manual_name_review, name_match_status)]

winner_data[, match_row_id := NULL]

# 8 Save
saveRDS(winner_data, file.path(ted_dir, "ted_winner_data_name_matched.rds"))
fwrite(winner_data,  file.path(ted_dir, "ted_winner_data_name_matched.csv"))
saveRDS(manual_name_review, file.path(ted_dir, "manual_name_review_ted_winner.rds"))

# Diagnostics
cat(sprintf("\nTED winners: %d rows | winner_cvr_final on %.0f%% | name-matched %d | quality graded %d\n",
            nrow(winner_data), 100 * mean(!is.na(winner_data$winner_cvr_final)),
            winner_data[flag_name_match_found == TRUE, .N],
            winner_data[!is.na(cvr_name_match_quality), .N]))
