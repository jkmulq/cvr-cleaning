# Match missing TED buyer CVRs using the registered CVR names, and grade the
# buyer-name quality against the registry - the buyer analog of ted_4_match_winners.R
# (which mirrors 2_3), so the TED buyer dataset is comparable to KFST/OpenTender on
# name-matching + quality scores. Mirrors 2_4_match_opentender_buyers.R minus the
# consortium-removal / name-partition splitting (TED buyers are one org per row).
#
# INPUT  data/intermediates/ted/ted_buyer_data.rds  (from ted_3), + the CVR-name keys.
# OUTPUT data/intermediates/ted/ted_buyer_data_name_matched.rds
#        data/intermediates/ted/manual_name_review_ted_buyer.rds

rm(list = ls())
source("config.R")
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
}))
source(file.path(PROJECT_DIR, "code", "functions.R"))

clean_data_dir <- dirs$clean_data
ted_dir        <- file.path(dirs$intermediates, "ted")

# 1 Load the TED buyers and the CVR-name keys
buyer_data <- readRDS(file.path(ted_dir, "ted_buyer_data.rds"))
name_key   <- readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds"))
biname_key <- readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds"))
setDT(buyer_data); setDT(name_key); setDT(biname_key)

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

# 1.5 Prepare the buyer names + the matching flag
buyer_data[, buyer_cvr_clean     := buyer_cvr]
buyer_data[, buyer_cvr_candidate := buyer_cvr_raw]
buyer_data[, tender_id    := notice_id]
buyer_data[, buyer_number := rowid(notice_id, lot)]
bp <- prepare_cvr_name(buyer_data$buyer_name)
buyer_data[, `:=`(
  buyer_name_basic     = bp$name_basic,
  buyer_name_match     = bp$name_clean,
  buyer_name_no_spaces = bp$name_no_spaces,
  buyer_name_broad     = bp$name_broad,
  buyer_firm_type      = bp$firm_type,
  first_letter         = substr(bp$name_clean, 1, 1),
  broad_first_letter   = substr(bp$name_broad, 1, 1))]
buyer_data[, flag_check_fuzzy_match :=
  !is.na(buyer_name) & buyer_name != "" & (is.na(buyer_cvr_clean) | buyer_cvr_clean == "")]

# 2 Filter (DK / DNK; match date = the TED contract-award date)
buyer_data[, match_row_id := .I]
buyer_data[, buyer_name_in_data := buyer_name]
remaining <- buyer_data[
  flag_check_fuzzy_match & toupper(trimws(buyer_country)) %in% c("DK", "DNK")]
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

# 3 Exact matching (steps 1-4)
candidate_matches <- cvr_key[remaining,
  on = .(name_basic = buyer_name_basic, firm_type = buyer_firm_type),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 1L)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 1 matches:", nrow(new_matches), "\n")

candidate_matches <- cvr_key[remaining,
  on = .(name_no_spaces = buyer_name_no_spaces, firm_type = buyer_firm_type),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 2L)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 2 matches:", nrow(new_matches), "\n")

candidate_matches <- cvr_key[remaining,
  on = .(name_no_spaces = buyer_name_no_spaces),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 3L)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 3 matches:", nrow(new_matches), "\n")

candidate_matches <- cvr_key[remaining,
  on = .(name_broad = buyer_name_broad),
  nomatch = 0, allow.cartesian = TRUE]
new_matches <- select_preferred_exact_match(candidate_matches, step = 4L)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 4 matches:", nrow(new_matches), "\n")
rm(new_matches); gc()
cat("total exact matches:", nrow(matched), "\n")

cvr_key_quality <- unique(cvr_key[, .(cvr, name_match, name_basic, name_no_spaces, name_broad)])
rm(cvr_key); gc()

# 4 Fuzzy matching (distinct-name speed-up + steps 5-6)
fuzzy_match_cols <- c("buyer_name_match", "buyer_name_broad", "buyer_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id]
remaining[, match_row_id := fuzzy_match_id]
cat("No. distinct observations to fuzzy match:", nrow(remaining), "\n")

fuzzy_candidates <- data.table()
matched_prefuzzy <- copy(matched)

step_candidates <- find_fuzzy_matches(remaining, name_key,
  entity_name_column = "buyer_name_match", key_name_column = "name_match",
  first_letter_column = "first_letter", step = 5L, firm_type_column = "buyer_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 5 main matches:", nrow(new_matches), "\n")

step_candidates <- find_fuzzy_matches(remaining, biname_key,
  entity_name_column = "buyer_name_match", key_name_column = "name_match",
  first_letter_column = "first_letter", step = 5L, firm_type_column = "buyer_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 5 biname matches:", nrow(new_matches), "\n")

step_candidates <- find_fuzzy_matches(remaining, name_key,
  entity_name_column = "buyer_name_broad", key_name_column = "name_broad",
  first_letter_column = "broad_first_letter", step = 6L, firm_type_column = "buyer_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 86)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 6 main matches:", nrow(new_matches), "\n")

step_candidates <- find_fuzzy_matches(remaining, biname_key,
  entity_name_column = "buyer_name_broad", key_name_column = "name_broad",
  first_letter_column = "broad_first_letter", step = 6L, firm_type_column = "buyer_firm_type")
fuzzy_candidates <- rbindlist(list(fuzzy_candidates, step_candidates), use.names = TRUE, fill = TRUE)
new_matches <- accept_fuzzy_match(step_candidates, threshold = 89)
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Fuzzy 6 biname matches:", nrow(new_matches), "\n")
rm(new_matches, step_candidates, name_key, biname_key); gc()

if (nrow(fuzzy_candidates) > 0) {
  setnames(fuzzy_candidates, "match_row_id", "fuzzy_match_id")
  fuzzy_candidates <- fuzzy_row_lookup[fuzzy_candidates, on = "fuzzy_match_id", allow.cartesian = TRUE]
  fuzzy_candidates[, fuzzy_match_id := NULL]
}
fuzzy_matched <- matched[name_match_method == "fuzzy"]
fuzzy_matched <- fuzzy_row_lookup[fuzzy_matched,
  on = .(fuzzy_match_id = match_row_id), allow.cartesian = TRUE][, fuzzy_match_id := NULL]
matched <- rbindlist(list(matched_prefuzzy, fuzzy_matched), use.names = TRUE, fill = TRUE)

# 5 Join matches back to the full buyer data
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
  buyer_data <- merge(buyer_data, fuzzy_candidates_wide, by = "match_row_id", all.x = TRUE, sort = FALSE)
}

buyer_data[matched, on = "match_row_id",
  `:=`(buyer_cvr_name_match  = i.cvr_name_match,
       registered_name_match = i.registered_name_match,
       name_match_source     = i.name_match_source,
       name_match_step       = i.name_match_step,
       name_match_method     = i.name_match_method,
       name_match_score      = i.name_match_score,
       name_match_n_candidates = i.name_match_n_candidates)]

buyer_data[, cvr_number_source := fcase(
  name_match_method == "exact" & name_match_step == 1L, "exact: basic name and firm type",
  name_match_method == "exact" & name_match_step == 2L, "exact: no spaces and firm type",
  name_match_method == "exact" & name_match_step == 3L, "exact: no spaces",
  name_match_method == "exact" & name_match_step == 4L, "exact: broad name",
  name_match_method == "fuzzy" & name_match_step == 5L & name_match_source == "name",   "fuzzy: prepared main name",
  name_match_method == "fuzzy" & name_match_step == 5L & name_match_source == "biname", "fuzzy: prepared biname",
  name_match_method == "fuzzy" & name_match_step == 6L & name_match_source == "name",   "fuzzy: broad main name",
  name_match_method == "fuzzy" & name_match_step == 6L & name_match_source == "biname", "fuzzy: broad biname",
  !is.na(buyer_cvr_clean) & buyer_cvr_clean != "", "source: CVR extracted from TED XML",
  flag_check_fuzzy_match & toupper(trimws(buyer_country)) %in% c("DK", "DNK"), "matching candidate: no match found",
  flag_check_fuzzy_match, "not a matching candidate: not marked as Danish",
  default = "not a matching candidate: no CVR name")]

# Reliability of a matching candidate's Danish gate (as in the winner matchers). "exact DK" = country
# is exactly DK/DNK; "contains DK" = a mixed value merely containing DK; NA for non-candidates. TED
# country is a single ISO code, so in practice only "exact DK" occurs here.
buyer_data[, matching_candidate_type := fcase(
  flag_check_fuzzy_match & toupper(trimws(buyer_country)) %chin% c("DK", "DNK"), "exact DK",
  flag_check_fuzzy_match & grepl("DK", toupper(trimws(buyer_country))),          "contains DK",
  default = NA_character_
)]

buyer_data[, flag_name_match_found := !is.na(buyer_cvr_name_match)]
buyer_data[, flag_name_match_ambiguous := (flag_name_match_found & name_match_n_candidates > 1)]
buyer_data[, flag_review_name_match :=
  (flag_name_match_found & (name_match_method == "fuzzy" | flag_name_match_ambiguous))]
buyer_data[, flag_manual_name_review :=
  (flag_check_fuzzy_match & (!flag_name_match_found | flag_review_name_match))]

buyer_data[, buyer_cvr_final := as.character(buyer_cvr_clean)]
buyer_data[flag_check_fuzzy_match & toupper(trimws(buyer_country)) %in% c("DK", "DNK") &
             !is.na(buyer_cvr_name_match),
           buyer_cvr_final := buyer_cvr_name_match]

buyer_data[, name_match_status := fcase(
  !flag_check_fuzzy_match, "not requested",
  flag_review_name_match,  "manual review - fuzzy or ambiguous match",
  flag_name_match_found,   "matched",
  is.na(buyer_country) | !toupper(trimws(buyer_country)) %in% c("DK", "DNK"),
  "manual review - not marked as Danish",
  default = "manual review - no automatic match")]

# 6 Data quality: buyer name vs the final CVR's registered names, per name form
for (qf in list(c("buyer_name_match",     "name_match",     "cvr_name_match_quality"),
                c("buyer_name_basic",     "name_basic",     "cvr_name_match_quality_basic"),
                c("buyer_name_no_spaces", "name_no_spaces", "cvr_name_match_quality_nospaces"),
                c("buyer_name_broad",     "name_broad",     "cvr_name_match_quality_broad"))) {
  buy_col <- qf[1]; key_col <- qf[2]; q_col <- qf[3]
  reg_lookup <- unique(data.table(cvr = as.character(cvr_key_quality$cvr),
                                  reg_name = cvr_key_quality[[key_col]]))[!is.na(reg_name) & reg_name != ""]
  qual <- data.table(match_row_id = buyer_data$match_row_id,
                     cvr = as.character(buyer_data$buyer_cvr_final),
                     buy_name = buyer_data[[buy_col]])
  qual <- qual[!is.na(cvr) & cvr != "" & !is.na(buy_name) & buy_name != ""]
  qual <- merge(qual, reg_lookup, by = "cvr", allow.cartesian = TRUE)
  qual[, score := levenshtein_ratio(buy_name, reg_name, pairwise = TRUE)]
  setorder(qual, match_row_id, -score)
  best_qual <- qual[, .SD[1L], by = match_row_id]
  buyer_data[, (q_col) := NA_real_]
  buyer_data[best_qual, on = "match_row_id", (q_col) := i.score]
  if (q_col == "cvr_name_match_quality") {
    buyer_data[, cvr_name_match_quality_name := NA_character_]
    buyer_data[best_qual, on = "match_row_id", cvr_name_match_quality_name := i.reg_name]
  }
}
buyer_data[, cvr_name_is_substring := NA]
buyer_data[!is.na(cvr_name_match_quality_name) & !is.na(buyer_name_match) & buyer_name_match != "",
           cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(buyer_name_match))]

# 7 Document CVRs recovered from an invalid raw id
reg_cvrs_prov <- unique(as.character(cvr_key_quality$cvr))
cand_prov <- ifelse(is.na(buyer_data$buyer_cvr_candidate), "",
                    gsub("\\s+", "", as.character(buyer_data$buyer_cvr_candidate)))
cand_has_reg_prov <- vapply(regmatches(cand_prov, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", cand_prov, perl = TRUE)),
                            function(v) any(v %chin% reg_cvrs_prov), logical(1))
buyer_data[, flag_cvr_recovered_from_invalid :=
  cand_prov != "" & !cand_has_reg_prov & !is.na(buyer_cvr_final) & as.character(buyer_cvr_final) != ""]

# Registry membership of the FINAL CVR: TRUE iff buyer_cvr_final is a CVR present in the registry
# name key -- independent of HOW it was obtained. Distinct from valid_cvr (format check only) and
# from flag_cvr_recovered_from_invalid (about the original candidate). NA / blank final CVRs are FALSE.
buyer_data[, flag_cvr_final_in_registry :=
  !is.na(buyer_cvr_final) & as.character(buyer_cvr_final) %chin% reg_cvrs_prov]

manual_name_review <- buyer_data[flag_manual_name_review == TRUE,
  .(notice_id, lot_id, lot, buyer_name_in_data, buyer_name, buyer_name_match, buyer_firm_type,
    buyer_country, date_contract_award, buyer_cvr_name_match, registered_name_match,
    name_match_step, cvr_number_source, name_match_method, name_match_score,
    name_match_n_candidates, flag_name_match_found, flag_name_match_ambiguous,
    flag_review_name_match, flag_manual_name_review, name_match_status)]

buyer_data[, match_row_id := NULL]

# 8 Save
saveRDS(buyer_data, file.path(ted_dir, "ted_buyer_data_name_matched.rds"))
fwrite(buyer_data,  file.path(ted_dir, "ted_buyer_data_name_matched.csv"))
saveRDS(manual_name_review, file.path(ted_dir, "manual_name_review_ted_buyer.rds"))

cat(sprintf("\nTED buyers: %d rows | buyer_cvr_final on %.0f%% | name-matched %d | quality graded %d\n",
            nrow(buyer_data), 100 * mean(!is.na(buyer_data$buyer_cvr_final)),
            buyer_data[flag_name_match_found == TRUE, .N],
            buyer_data[!is.na(cvr_name_match_quality), .N]))
