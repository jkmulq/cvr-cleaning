# =============================================================================
# code/processing/3_1_build_kfst_winner_datasets.R
# KFST winner robustness stack (mirror of the OpenTender stack in 3_2): base / extraction /
# name_only on the SHARED slim schema. NOT consumed by the pipeline (robustness/comparison
# only). Runs right after 2_1. Author: Jack Mulqueeney.
# =============================================================================
rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean_data_dir <- dirs$clean_data

base_raw <- as.data.table(readRDS(file.path(clean_data_dir, "clean_winner_data_kfst_name_matched.rds")))

# Slim CORE schema (shared with the OT stack). base expands beyond this (flags/consortium/provenance);
# extraction/name_only keep only the core and get NA elsewhere via rbindlist(fill = TRUE).
slim_cols <- c("dataset","tender_id","lot_id","winner_number","winner_name","winner_country",
  "winner_cvr_final","winner_cvr_clean","valid_cvr","name_match_method","name_match_step",
  "cvr_number_source","matching_candidate_type","name_match_score","flag_name_match_found","cvr_name_match_quality",
  "cvr_name_match_quality_basic","cvr_name_match_quality_nospaces","cvr_name_match_quality_broad",
  "cvr_name_match_quality_name","cvr_name_is_substring","award_date","flag_awarded",
  "tender_amount","tender_amount_eur","lot_amount","lot_amount_eur","annualised_tender_amount",
  "annualised_lot_amount","cpv_division","cpv_sector","cpv_category","buyer_name","tender_status",
  "n_bidders","award_url","winner_cvr_original","winner_name_original","winner_country_original")

# 1 Base
## Production consortium-matched KFST winners (from 2_1): slim core + all cleaning/matching flags +
## consortium/field-pairing + provenance cols. Full raw canonical stays in the *_name_matched.rds file.
base <- copy(base_raw)
base[, dataset := "base"]
extra_cols <- intersect(c(
  grep("^flag_", names(base), value = TRUE),
  "consortium_flag","semi_tier","is_consortium","consortium_number","consortium_name","consortium_cvr",
  "consortium_winner","type","registry_score","field_paired_cvr","field_paired_score",
  "field_cvr_1","field_cvr_2","field_cvr_3","field_cvr_score_1","field_cvr_score_2","field_cvr_score_3",
  "field_cvr_regname_1","field_cvr_regname_2","field_cvr_regname_3",
  "winner_cvr_candidate_original","winner_cvr_name_match","registered_name_match",
  "name_match_source","name_match_n_candidates","name_match_status"
), names(base))
expanded_cols <- unique(c(slim_cols, extra_cols))
base <- base[, ..expanded_cols]

# Lot-level info to attach to the CVR-only variants
ctx_cols <- c("tender_id","lot_id","award_date","flag_awarded","tender_amount","tender_amount_eur",
  "lot_amount","lot_amount_eur","annualised_tender_amount","annualised_lot_amount","cpv_division",
  "cpv_sector","cpv_category","buyer_name","tender_status","n_bidders","award_url")
lot_ctx <- unique(base[, ..ctx_cols], by = c("tender_id","lot_id"))

# 2 Valid CVRs only
## every standalone 8-digit CVR in the raw winner field, no matching or further cleaning
# lot_field_cvrs() = the shared functions.R helper (collapse_whitespace=FALSE, drop_invalid=TRUE).
extraction <- as.data.table(lot_field_cvrs(
  unique(base_raw[, .(tender_id, lot_id, winner_cvr = winner_cvr_original)])))
extraction[, winner_number := rowid(tender_id, lot_id)]
extraction[, `:=`(
  winner_cvr_final = cvr, winner_cvr_clean = cvr, valid_cvr = TRUE,
  winner_name = NA_character_, winner_country = NA_character_, name_match_method = NA_character_,
  name_match_step = NA_integer_,
  cvr_number_source = "CVR from the winner field: raw extraction (no matching)",
  name_match_score = NA_real_, flag_name_match_found = FALSE,
  cvr_name_match_quality = NA_real_, cvr_name_match_quality_basic = NA_real_,
  cvr_name_match_quality_nospaces = NA_real_, cvr_name_match_quality_broad = NA_real_,
  cvr_name_match_quality_name = NA_character_, cvr_name_is_substring = NA,
  winner_cvr_original = NA_character_, winner_name_original = NA_character_, winner_country_original = NA_character_)]
extraction <- merge(extraction, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)
extraction[, cvr := NULL][, dataset := "extraction"]
extraction <- extraction[, ..slim_cols]

# 3 Match names only
## Put every winner through the matching algorithm, ignore field CVRs
# winner_cvr_final = the name-matched CVR only (NA if no match). Mirrors the 2_1 CORE matcher
# (exact steps 1-4 + fuzzy 5-6, same functions.R primitives + thresholds 85/85/86/89).
no <- copy(base_raw)[, .(tender_id, lot_id, winner_number, winner_name, winner_country, pub_date)]
prep <- as.data.table(prepare_cvr_name(no$winner_name))
no[, `:=`(winner_name_basic = prep$name_basic, winner_name_no_spaces = prep$name_no_spaces,
          winner_name_broad = prep$name_broad, winner_name_match = prep$name_clean,
          winner_firm_type = prep$firm_type)]
no[, `:=`(match_row_id = .I, winner_name_in_data = winner_name, match_date = as.IDate(pub_date))]

# keys (identical construction to 2_1)
name_key   <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds")))
biname_key <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds")))
setnames(name_key, "name", "registered_name"); setnames(biname_key, "binavn", "registered_name")
name_key[, name_source := "name"]; biname_key[, name_source := "biname"]
name_key[, cvr := sprintf("%08d", as.integer(cvr))]; biname_key[, cvr := sprintf("%08d", as.integer(cvr))]
name_key[, broad_first_letter := substr(name_broad, 1, 1)]
biname_key[, broad_first_letter := substr(name_broad, 1, 1)]
cvr_key <- rbindlist(list(name_key, biname_key), use.names = TRUE)
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]

# candidates = every DK name 
remaining <- no[toupper(trimws(winner_country)) == "DK" & !is.na(winner_name_match) & winner_name_match != "",
  .(match_row_id, tender_id, lot_id, winner_number, winner_name_in_data, winner_name_basic,
    winner_name_no_spaces, winner_name_broad, winner_name_match, winner_firm_type, match_date)]
matched <- data.table(match_row_id = integer(0), cvr_name_match = character(0),
  registered_name_match = character(0), name_match_source = character(0), name_match_step = integer(0),
  name_match_method = character(0), name_match_score = numeric(0), name_match_n_candidates = integer(0))
remaining_original <- copy(remaining)   # add_winner_context_to_matches() reads this from the caller
cat("name_only: DK names to match:", nrow(remaining), "of", nrow(no), "\n")

# exact steps 1-4 (identical to 2_1)
run_step <- function(on_cols, step) {
  cand <- cvr_key[remaining, on = on_cols, nomatch = 0, allow.cartesian = TRUE]
  keep_step_matches(add_winner_context_to_matches(select_preferred_exact_match(cand, step = step)))
}
run_step(c(name_basic = "winner_name_basic", firm_type = "winner_firm_type"), 1L)
run_step(c(name_no_spaces = "winner_name_no_spaces", firm_type = "winner_firm_type"), 2L)
run_step(c(name_no_spaces = "winner_name_no_spaces"), 3L)
run_step(c(name_broad = "winner_name_broad"), 4L)

# collapse distinct name-problems, fuzzy steps 5-6 (identical to 2_1), then expand back
fuzzy_match_cols <- c("winner_name_match", "winner_name_broad", "winner_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id][, match_row_id := fuzzy_match_id]
remaining_original <- copy(remaining)   # refresh after the distinct-name collapse
matched_prefuzzy <- copy(matched)
run_fuzzy <- function(key, ecol, kcol, flcol, step, thr) {
  cand <- find_fuzzy_matches(remaining, key, entity_name_column = ecol, key_name_column = kcol,
                             first_letter_column = flcol, firm_type_column = "winner_firm_type", step = step)
  keep_step_matches(add_winner_context_to_matches(accept_fuzzy_match(cand, threshold = thr)))
}
run_fuzzy(name_key,   "winner_name_match", "name_match", "first_letter",       5L, 85)
run_fuzzy(biname_key, "winner_name_match", "name_match", "first_letter",       5L, 85)
run_fuzzy(name_key,   "winner_name_broad", "name_broad", "broad_first_letter", 6L, 86)
run_fuzzy(biname_key, "winner_name_broad", "name_broad", "broad_first_letter", 6L, 89)
fuzzy_matched <- matched[name_match_method == "fuzzy"]
if (nrow(fuzzy_matched) > 0)
  fuzzy_matched <- fuzzy_row_lookup[fuzzy_matched, on = .(fuzzy_match_id = match_row_id),
                                    allow.cartesian = TRUE][, fuzzy_match_id := NULL]
matched <- rbindlist(list(matched_prefuzzy, fuzzy_matched), use.names = TRUE, fill = TRUE)

km <- unique(matched[!is.na(cvr_name_match), .(match_row_id, winner_cvr_name_match = cvr_name_match,
  registered_name_match, name_match_source, name_match_step, name_match_method, name_match_score)])
no <- merge(no, km, by = "match_row_id", all.x = TRUE)
no[, winner_cvr_final := winner_cvr_name_match]     # PURE name match: field CVR ignored

# quality: best levenshtein of the winner name vs the final CVR's registered names, for EACH prepared
# name form (clean = strict default; basic/no-spaces/broad looser).
for (qf in list(c("winner_name_match",     "name_match",     "cvr_name_match_quality"),
                c("winner_name_basic",     "name_basic",     "cvr_name_match_quality_basic"),
                c("winner_name_no_spaces", "name_no_spaces", "cvr_name_match_quality_nospaces"),
                c("winner_name_broad",     "name_broad",     "cvr_name_match_quality_broad"))) {
  win_col <- qf[1]; key_col <- qf[2]; q_col <- qf[3]
  reg_lookup <- unique(data.table(cvr = as.character(cvr_key$cvr),
                                  reg_name = cvr_key[[key_col]]))[!is.na(reg_name) & reg_name != ""]
  qual <- data.table(match_row_id = no$match_row_id, cvr = as.character(no$winner_cvr_final),
                     win_name = no[[win_col]])
  qual <- qual[!is.na(cvr) & cvr != "" & !is.na(win_name) & win_name != ""]
  qual <- merge(qual, reg_lookup, by = "cvr", allow.cartesian = TRUE)
  qual[, score := levenshtein_ratio(win_name, reg_name, pairwise = TRUE)]
  setorder(qual, match_row_id, -score)
  best_qual <- qual[, .SD[1L], by = match_row_id]
  no[, (q_col) := NA_real_]
  no[best_qual, on = "match_row_id", (q_col) := i.score]
  if (q_col == "cvr_name_match_quality") {
    no[, cvr_name_match_quality_name := NA_character_]
    no[best_qual, on = "match_row_id", cvr_name_match_quality_name := i.reg_name]
  }
}
no[, cvr_name_is_substring := NA]
no[!is.na(cvr_name_match_quality_name) & !is.na(winner_name_match) & winner_name_match != "",
   cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(winner_name_match))]

name_only <- no[, .(tender_id, lot_id, winner_number, winner_name, winner_country,
  winner_cvr_final, winner_cvr_clean = NA_character_, valid_cvr = !is.na(winner_cvr_final),
  name_match_method, name_match_step,
  cvr_number_source = fifelse(!is.na(winner_cvr_final), "CVR from name matching only (field CVR ignored)", NA_character_),
  name_match_score, flag_name_match_found = !is.na(winner_cvr_final),
  cvr_name_match_quality, cvr_name_match_quality_basic, cvr_name_match_quality_nospaces,
  cvr_name_match_quality_broad, cvr_name_match_quality_name, cvr_name_is_substring,
  winner_cvr_original = NA_character_, winner_name_original = NA_character_, winner_country_original = NA_character_)]
name_only <- merge(name_only, lot_ctx, by = c("tender_id", "lot_id"), all.x = TRUE)
name_only[, dataset := "name_only"]
name_only <- name_only[, ..slim_cols]

# 4 stack all and save
stacked <- rbindlist(list(base, extraction, name_only), use.names = TRUE, fill = TRUE)
stacked[, dataset := factor(dataset, levels = c("base","extraction","name_only"))]
out_path <- Sys.getenv("KFST_STACK_OUT", unset = file.path(clean_data_dir, "kfst_winner_datasets_stacked.rds"))
saveRDS(stacked, out_path)
cat("kfst_winner_datasets_stacked.rds:", nrow(stacked), "rows\n")
print(stacked[, .N, by = dataset])
