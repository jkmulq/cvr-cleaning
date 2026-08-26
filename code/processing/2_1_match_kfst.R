# Match missing KFST winner CVRs using registered CVR names
# Author: Jack Mulqueeney
# Date: 30 June 2026

rm(list = ls())

# Load config
source("config.R")

# Packages
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
}))

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Directories
clean_data_dir <- dirs$clean_data

# 1 Load the KFST winners and the CVR-name keys

winner_data <- readRDS(
  file.path(clean_data_dir, "clean_winner_data_kfst.rds")
)
name_key <- readRDS(
  file.path(clean_data_dir, "clean_cvr_name_key.rds")
)
biname_key <- readRDS(
  file.path(clean_data_dir, "clean_cvr_biname_key.rds")
)

setDT(winner_data)
setDT(name_key)
setDT(biname_key)

## 1.1 Concord column names across keys
setnames(name_key, "name", "registered_name")
setnames(biname_key, "binavn", "registered_name")

## 1.2 Improve keys before matching
# Key source identifiers
name_key[, name_source := "name"]
biname_key[, name_source := "biname"]

# Ensure CVRs are eight-character strings.
name_key[, cvr := sprintf("%08d", as.integer(cvr))]
biname_key[, cvr := sprintf("%08d", as.integer(cvr))]

# Extract first letter of the broadly generalized name.
name_key[, broad_first_letter := substr(name_broad, 1, 1)]
biname_key[, broad_first_letter := substr(name_broad, 1, 1)]

# Combine keys for exact matching. 
cvr_key <- rbindlist(
  list(name_key, biname_key),
  use.names = TRUE
)

# If the same match is available as both a main name and a biname, 
# prioritise main name
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]


# 2 Filter KFST data
## Only attempt to fuzzy match Danish firms AND 
## row is missing cvr number and row has winning firm name (flag_check_fuzzy_match)

## 2.1 Row id for later joining
winner_data[, match_row_id := .I]
winner_data[, winner_name_in_data := winner_name]

# --- 2.2 Registry-validity: a name-bearing row whose cleaned CVR is NOT a registered CVR is
# erroneous (typo/extra digit/malformed), so clear it and send it to the name matcher. The
# original stays in winner_cvr_candidate_original; winner_cvr_final can fall back to the match.
registered_cvrs <- unique(cvr_key$cvr)
winner_data[!is.na(winner_name) & winner_name != "" &
              !is.na(winner_cvr_clean) & winner_cvr_clean != "" &
              !(winner_cvr_clean %chin% registered_cvrs),
            `:=`(winner_cvr_clean = NA_character_, valid_cvr = FALSE, flag_check_fuzzy_match = TRUE)]

# Force every tier-3b name through the open matcher (even if borrow-filled) so each 3b row stores
# BOTH a field score (graft 2.3) and an open-match score -> the field-vs-open cut is re-tunable later.
winner_data[semi_tier == "3b_pending" & !is.na(winner_name) & winner_name != "",
            flag_check_fuzzy_match := TRUE]

# --- 2.3 Tier-3b field pairing (resolve semi_tier == "3b_pending") ---------------------------
# For lots with more names than field CVRs, prefer the lot's OWN listed field CVRs. Each 3b name
# is scored against the lot's field CVRs (from winner_cvr_original) via the registry; a name keeps
# its best field CVR when that match clears the fuzzy bar (field_prefer_threshold). If two names
# claim the same field CVR, the stronger match keeps it and the other name falls to the open
# matcher below (so no field CVR is attached to two winners). The top-3 candidates are recorded.
field_prefer_threshold <- 85

# Create the field-pairing columns up front (stable schema even when there are no 3b rows).
winner_data[, field_paired_cvr    := NA_character_]
winner_data[, field_paired_score  := NA_real_]
winner_data[, field_cvr_1         := NA_character_]
winner_data[, field_cvr_2         := NA_character_]
winner_data[, field_cvr_3         := NA_character_]
winner_data[, field_cvr_score_1   := NA_real_]
winner_data[, field_cvr_score_2   := NA_real_]
winner_data[, field_cvr_score_3   := NA_real_]
winner_data[, field_cvr_regname_1 := NA_character_]
winner_data[, field_cvr_regname_2 := NA_character_]
winner_data[, field_cvr_regname_3 := NA_character_]

t3b_names <- winner_data[
  semi_tier == "3b_pending" & !is.na(winner_name_match) & winner_name_match != "",
  .(match_row_id, tender_id, lot_id, winner_name_match)
]

if (nrow(t3b_names) > 0L) {
  # The lot's valid field CVRs (raw field). lot_field_cvrs() is the shared functions.R helper.
  t3b_cvrs <- as.data.table(lot_field_cvrs(
    unique(winner_data[semi_tier == "3b_pending",
                       .(tender_id, lot_id, winner_cvr = winner_cvr_original)])
  ))
  # Registry: cvr -> registered names (main + bi-names), restricted to those field CVRs.
  registry_lookup <- unique(cvr_key[
    !is.na(name_match) & name_match != "" & cvr %chin% t3b_cvrs$cvr,
    .(cvr, reg_name = name_match)
  ])

  # Score every (name, field CVR, registered name); keep each (name, CVR)'s best registered name.
  pairs <- merge(t3b_names, t3b_cvrs, by = c("tender_id", "lot_id"), allow.cartesian = TRUE)
  pairs <- merge(pairs, registry_lookup, by = "cvr", allow.cartesian = TRUE)
  pairs[, score := levenshtein_ratio(winner_name_match, reg_name, pairwise = TRUE)]
  setorder(pairs, match_row_id, cvr, -score)
  best <- pairs[, .SD[1L], by = .(match_row_id, cvr)]

  # Rank each name's field CVRs best-first and record the top 3 (one explicit join per rank).
  setorder(best, match_row_id, -score)
  best[, field_rank := rowid(match_row_id)]
  winner_data[best[field_rank == 1L], on = "match_row_id",
              `:=`(field_cvr_1 = i.cvr, field_cvr_score_1 = i.score, field_cvr_regname_1 = i.reg_name)]
  winner_data[best[field_rank == 2L], on = "match_row_id",
              `:=`(field_cvr_2 = i.cvr, field_cvr_score_2 = i.score, field_cvr_regname_2 = i.reg_name)]
  winner_data[best[field_rank == 3L], on = "match_row_id",
              `:=`(field_cvr_3 = i.cvr, field_cvr_score_3 = i.score, field_cvr_regname_3 = i.reg_name)]

  # Assign each name its best field CVR when it clears the bar ...
  assigned <- best[field_rank == 1L & score >= field_prefer_threshold]
  # ... then if two names in a lot claim the same field CVR, keep only the stronger match.
  setorder(assigned, tender_id, lot_id, cvr, -score)
  assigned <- assigned[, .SD[1L], by = .(tender_id, lot_id, cvr)]
  winner_data[assigned, on = "match_row_id",
              `:=`(field_paired_cvr = i.cvr, field_paired_score = i.score)]
}

# Relabel 3b_pending -> 3_field_paired / 3_open_match / 3_blank; carry the paired score.
winner_data[semi_tier == "3b_pending", semi_tier := fcase(
  !is.na(field_paired_cvr),                 "3_field_paired",
  !is.na(winner_name) & winner_name != "",  "3_open_match",
  default =                                 "3_blank"
)]
winner_data[!is.na(field_paired_cvr), registry_score := field_paired_score]

# The CVR key contains Danish firms, so only rows marked DK are automatically
remaining <- winner_data[
  flag_check_fuzzy_match &
    toupper(trimws(winner_country)) == "DK"
]

cat("No. observations to fuzzy match:", nrow(remaining), "\n")

# The CVR key records when a name was valid. 
# We will use tender publication dates to filter potential matches. 
remaining[, match_date := as.IDate(pub_date)]
remaining_original <- remaining

# Table to append matches at each step.
# Matched rows are removed from remaining (just as in matching.ipynb).
# Initialised with the full output schema (not a bare data.table()) so the
# update-join onto winner_data below still works when every step finds zero
# matches: the *_name_match* columns are then created as typed NA.
matched <- data.table(
  match_row_id            = integer(0),
  cvr_name_match          = character(0),
  registered_name_match   = character(0),
  name_match_source       = character(0),
  name_match_step         = integer(0),
  name_match_method       = character(0),
  name_match_score        = numeric(0),
  name_match_n_candidates = integer(0)
)

# 3 Exact matching
## 3.1 Match on lightly prepared name and firm type
candidate_matches <- cvr_key[
  remaining,
  on = .(
    name_basic = winner_name_basic,
    firm_type = winner_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
# The above matches to both the main name and all the potential business names
# and also ignores whether the firm's registration date is compatible with 
# with the tender date. 

# select_preferred_exact_match() prioritises matches from the main firm name, 
# and removes invalid matches based on registration/tender dates.
new_matches <- select_preferred_exact_match(candidate_matches, step = 1L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches) # Remove successful matches from remaining
cat("Step 1 matches:", nrow(new_matches), "\n")

## 3.2 match on generalized name without spaces, retaining firm type.
candidate_matches <- cvr_key[
  remaining,
  on = .(
    name_no_spaces = winner_name_no_spaces,
    firm_type = winner_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_matches <- select_preferred_exact_match(candidate_matches, step = 2L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 2 matches:", nrow(new_matches), "\n")

## 3.3 the same generalized name, now ignoring firm type
candidate_matches <- cvr_key[
  remaining,
  on = .(name_no_spaces = winner_name_no_spaces),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_matches <- select_preferred_exact_match(candidate_matches, step = 3L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 3 matches:", nrow(new_matches), "\n")

## 3.4 remove common words, ignore word order, and ignore firm type.
candidate_matches <- cvr_key[
  remaining,
  on = .(name_broad = winner_name_broad),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_matches <- select_preferred_exact_match(candidate_matches, step = 4L)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 4 matches:", nrow(new_matches), "\n")

# Preserve the CVR -> registered-name forms for the PART-6 quality scores below,
# before cvr_key (large) is freed ahead of the fuzzy pass.
cvr_key_quality <- unique(cvr_key[, .(cvr, name_match, name_basic, name_no_spaces, name_broad)])
rm(new_matches, cvr_key)
gc()

# Speed-up: fuzzy match each distinct winner-name problem once, then expand the
# results back onto every original row. Keep match_date because CVR names are
# filtered by tender publication date inside the fuzzy step, so two rows with the
# same name but different dates are legitimately different problems.
fuzzy_match_cols <- c(
  "winner_name_match",
  "winner_name_broad",
  "winner_firm_type",
  "match_date"
)
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id]
remaining[, match_row_id := fuzzy_match_id]
remaining_original <- remaining
cat("No. distinct observations to fuzzy match:", nrow(remaining), "\n")


# 4 Fuzzy matching
# Create storage table
fuzzy_candidates <- data.table()

# All matches so far (exact) are keyed by the real match_row_id. Snapshot them so
# the fuzzy join-back below is a plain rbind onto this table, instead of filtering
# the fuzzy rows back out of a mixed table.
matched_prefuzzy <- copy(matched)

## 4.1 Main name key, full winner name
# Find top 5 fuzzy matches
step_candidates <- find_fuzzy_matches(
  remaining,
  name_key,
  entity_name_column = "winner_name_match",
  key_name_column = "name_match",
  first_letter_column = "first_letter",
  step = 5L,
  firm_type_column = "winner_firm_type"
)

# Append new match candidates the fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept only if match score exceeds 85
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to larger matched dataset
cat("Number of new fuzzy matches:", nrow(new_matches), "\n")

## 4.2 Biname key, full winner name
step_candidates <- find_fuzzy_matches(
  remaining,
  biname_key,
  entity_name_column = "winner_name_match",
  key_name_column = "name_match",
  first_letter_column = "first_letter",
  step = 5L,
  firm_type_column = "winner_firm_type"
)

# Append new match candidates the fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept only if match score exceeds 85
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to larger matched dataset
cat("Number of new fuzzy matches:", nrow(new_matches), "\n")

## 4.3 main name key, but using the broader name
# The documented thresholds are 86 for main names and 89 for binames.
step_candidates <- find_fuzzy_matches(
  remaining,
  name_key,
  entity_name_column = "winner_name_broad",
  key_name_column = "name_broad",
  first_letter_column = "broad_first_letter",
  step = 6L,
  firm_type_column = "winner_firm_type"
)

# Append new match candidates the fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept only if match score exceeds 86
new_matches <- accept_fuzzy_match(step_candidates, threshold = 86)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to new matched dataset
cat("Number of fuzzy matches:", nrow(new_matches), "\n")

## 4.4 biname name key, but using the broader name
step_candidates <- find_fuzzy_matches(
  remaining,
  biname_key,
  entity_name_column = "winner_name_broad",
  key_name_column = "name_broad",
  first_letter_column = "broad_first_letter",
  step = 6L,
  firm_type_column = "winner_firm_type"
)

# Append new match candidates to fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept if threshold exceeds 89
new_matches <- accept_fuzzy_match(step_candidates, threshold = 89)
new_matches <- add_winner_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to matched dataset
cat("Number of fuzzy matches", nrow(new_matches), "\n")

rm(new_matches, step_candidates, name_key, biname_key)
gc()


# Expand distinct-name fuzzy results back to every original winner row before the
# rank/dcast and the update-join, so both are keyed by the real match_row_id.
if (nrow(fuzzy_candidates) > 0) {
  setnames(fuzzy_candidates, "match_row_id", "fuzzy_match_id")
  fuzzy_candidates <- fuzzy_row_lookup[
    fuzzy_candidates,
    on = "fuzzy_match_id",
    allow.cartesian = TRUE
  ]
  fuzzy_candidates[, fuzzy_match_id := NULL]
}

# Pull the fuzzy matches (keyed by group id) into their own table, expand each
# group's match onto every original winner row, then recombine with the pre-fuzzy
# (exact) matches, which are already keyed by real ids.
fuzzy_matched <- matched[name_match_method == "fuzzy"]
fuzzy_matched <- fuzzy_row_lookup[
  fuzzy_matched,
  on = .(fuzzy_match_id = match_row_id),
  allow.cartesian = TRUE
][, fuzzy_match_id := NULL]

matched <- rbindlist(
  list(matched_prefuzzy, fuzzy_matched),
  use.names = TRUE,
  fill = TRUE
)


# 5 Join matches back to the full KFST winner data
# A winner can have candidates from more than one fuzzy step or key. Rank them
# together, remove repeated CVRs, and retain the best five in wide columns.
if (nrow(fuzzy_candidates) > 0) {
  fuzzy_candidates[, source_order := fifelse(
    fuzzy_candidate_source == "name",
    1L,
    2L
  )]
  setorder(
    fuzzy_candidates,
    match_row_id,
    -fuzzy_candidate_score,
    fuzzy_candidate_step,
    source_order,
    fuzzy_candidate_rank
  )
  fuzzy_candidates <- unique(
    fuzzy_candidates,
    by = c("match_row_id", "fuzzy_candidate_cvr")
  )
  fuzzy_candidates <- fuzzy_candidates[
    ,
    head(.SD, 5),
    by = match_row_id
  ]
  fuzzy_candidates[
    ,
    fuzzy_candidate_rank := seq_len(.N),
    by = match_row_id
  ]
  
  fuzzy_candidates_wide <- dcast(
    fuzzy_candidates,
    match_row_id ~ fuzzy_candidate_rank,
    value.var = c(
      "fuzzy_candidate_cvr",
      "fuzzy_candidate_name",
      "fuzzy_candidate_score",
      "fuzzy_candidate_source",
      "fuzzy_candidate_step"
    )
  )
  
  winner_data <- merge(
    winner_data,
    fuzzy_candidates_wide,
    by = "match_row_id",
    all.x = TRUE,
    sort = FALSE
  )
}

winner_data[
  matched,
  on = "match_row_id",
  `:=`(
    winner_cvr_name_match = i.cvr_name_match,
    registered_name_match = i.registered_name_match,
    name_match_source = i.name_match_source,
    name_match_step = i.name_match_step,
    name_match_method = i.name_match_method,
    name_match_score = i.name_match_score,
    name_match_n_candidates = i.name_match_n_candidates
  )
]

# Give each numeric matching step a stable, descriptive code. The numeric step
# is retained so the original matching order remains easy to inspect.
winner_data[, name_match_step_code := fcase(
  !is.na(field_paired_cvr),
  "CVR from tier-3b field pairing: winner name matched a CVR listed on this lot (lots with more names than field CVRs)",
  name_match_method == "exact" & name_match_step == 1L,
  "exact matching: basic name and firm type",
  name_match_method == "exact" & name_match_step == 2L,
  "exact matching: no spaces and firm type",
  name_match_method == "exact" & name_match_step == 3L,
  "exact matching: no spaces",
  name_match_method == "exact" & name_match_step == 4L,
  "exact matching: broad name",
  name_match_method == "fuzzy" & name_match_step == 5L &
    name_match_source == "name",
  "fuzzy matching: prepared main name",
  name_match_method == "fuzzy" & name_match_step == 5L &
    name_match_source == "biname",
  "fuzzy matching: prepared biname",
  name_match_method == "fuzzy" & name_match_step == 6L &
    name_match_source == "name",
  "fuzzy matching: broad main name",
  name_match_method == "fuzzy" & name_match_step == 6L &
    name_match_source == "biname",
  "fuzzy matching: broad biname",
  !is.na(winner_cvr_clean) & winner_cvr_clean != "" &
    type == "simple split on ;",
  "CVR from the original winner field: extracted after separting by semi-colon",
  !is.na(winner_cvr_clean) & winner_cvr_clean != "" &
    type %chin% c("simple consort split on ,", "only split on name, cvr, ignore country"),
  "CVR from the original winner field: extracted after separating consortium members by comma",
  !is.na(winner_cvr_clean) & winner_cvr_clean != "",
  "CVR from the original winner field: existing CVR, other misc split",
  flag_check_fuzzy_match &
    toupper(trimws(winner_country)) == "DK",
  "matching candidate: no match found",
  flag_check_fuzzy_match,
  "not a matching candidate: not marked as Danish",
  default = "not a matching candidate: no CVR name"
)]

# Fuzzy matches and matches tied across several CVRs are retained but flagged.
winner_data[, flag_name_match_found := !is.na(winner_cvr_name_match)]
winner_data[, flag_name_match_ambiguous := (
  flag_name_match_found & name_match_n_candidates > 1
)]
winner_data[, flag_review_name_match := (
  flag_name_match_found &
    (name_match_method == "fuzzy" | flag_name_match_ambiguous)
)]

# Step 7 in the documentation is manual review. This flag includes:
#   - rows that did not receive a match;
#   - fuzzy matches;
#   - matches where several CVRs were possible.
winner_data[, flag_manual_name_review := (
  flag_check_fuzzy_match &
    is.na(field_paired_cvr) &   # a field-paired row is already resolved -- don't route it to manual review
    (
      !flag_name_match_found |
        flag_review_name_match
    )
)]

# winner_cvr_final precedence: a preferred tier-3b field CVR wins first (a CVR listed for THIS lot
# beats a whole-registry name match), then the directly-clean/borrowed CVR, then the open name match.
winner_data[, winner_cvr_final := fcase(
  !is.na(field_paired_cvr),                            field_paired_cvr,
  !is.na(winner_cvr_clean) & winner_cvr_clean != "",   as.character(winner_cvr_clean),
  default =                                            winner_cvr_name_match
)]

# Provenance: winner_cvr_final was recovered (matched / field-paired / borrowed) BECAUSE the field
# candidate held no valid, REGISTERED CVR -- so the final differs from the original candidate due to its
# invalidity. Rows with no candidate (e.g. name-only tier-3b members) are NOT flagged -- see semi_tier.
reg_cvrs_prov <- unique(as.character(cvr_key_quality$cvr))
cand_prov <- ifelse(is.na(winner_data$winner_cvr_candidate_original), "",
                    gsub("\\s+", "", as.character(winner_data$winner_cvr_candidate_original)))  # de-space first, like the CVR cleaner
cand_has_reg_prov <- vapply(regmatches(cand_prov, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", cand_prov, perl = TRUE)),
                            function(v) any(v %chin% reg_cvrs_prov), logical(1))
winner_data[, flag_cvr_recovered_from_invalid :=
  cand_prov != "" & !cand_has_reg_prov & !is.na(winner_cvr_final) & as.character(winner_cvr_final) != ""]

winner_data[, name_match_status := fcase(
  !flag_check_fuzzy_match,
  "not requested",
  flag_review_name_match,
  "manual review - fuzzy or ambiguous match",
  flag_name_match_found,
  "matched",
  is.na(winner_country) | toupper(trimws(winner_country)) != "DK",
  "manual review - not marked as Danish",
  default = "manual review - no automatic match"
)]

# --- 6 Data quality (NOT in the old pipeline): how well the winner's name agrees with the REGISTERED
# name of its final CVR -- the best levenshtein ratio over that CVR's registered names (main + bi-names).
# Computed for EACH prepared name form: the clean form is the strict default (cvr_name_match_quality);
# basic/no-spaces/broad are looser variants (kept so you can pick your own quality bar). Independent of
# the matcher, for every row with a final CVR. cvr_key_quality (all forms) was preserved above.
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
  if (q_col == "cvr_name_match_quality") {   # keep the matched registry name for the strict (clean) form
    winner_data[, cvr_name_match_quality_name := NA_character_]
    winner_data[best_qual, on = "match_row_id", cvr_name_match_quality_name := i.reg_name]
  }
}
# is the cleaned winner name a verbatim substring of that best registered name? (rescues terse names)
winner_data[, cvr_name_is_substring := NA]
winner_data[!is.na(cvr_name_match_quality_name) & !is.na(winner_name_match) & winner_name_match != "",
            cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(winner_name_match))]

## Dedup phantom convergent duplicates (post-match). When a firm's raw CVR field holds two tokens -- one
## valid, one a typo -- the "."->"," step in the consortium split makes two member rows that then converge to
## the SAME winner_cvr_final after matching. Example: lot 9855-1 lists "NCC Danmark A/S" with CVR
## "69894011.698940098", so one NCC row keeps 69894011 while the other's invalid 9-digit token 698940098 is
## cleared and name-matched right back to 69894011 -- the same firm+CVR in the same lot slot. Keep one,
## preferring the row that kept a valid field CVR. (The 1_1 winner distinct can't catch this: it keys on
## winner_cvr_candidate_original, which differs here, 69894011 vs 698940098. The cancelled/non-cancelled
## dedup is at the tender-lot grain, not winner rows, so it is unrelated.)
setorder(winner_data, tender_id, lot_id, winner_number, consortium_number, winner_name, -valid_cvr)
winner_data <- unique(winner_data,
  by = c("tender_id", "lot_id", "winner_number", "consortium_number", "winner_name", "winner_cvr_final"))

# Save a compact table containing only rows that need a person to inspect.
manual_name_review <- winner_data[
  flag_manual_name_review == TRUE,] %>% 
  select(
    tender_id,
    lot_id,
    winner_number,
    winner_name_in_data,
    winner_name,
    winner_name_match,
    winner_firm_type,
    winner_country,
    pub_date,
    winner_cvr_name_match,
    winner_cvr_final,
    registered_name_match,
    starts_with("fuzzy_candidate_cvr"),
    starts_with("fuzzy_candidate_name"),
    starts_with("fuzzy_candidate_score"),
    name_match_step,
    name_match_step_code,
    name_match_method,
    name_match_score,
    name_match_n_candidates,
    flag_name_match_found,
    flag_name_match_ambiguous,
    flag_review_name_match,
    flag_manual_name_review,
    name_match_status
  )

# Delete match_row_id
winner_data[, match_row_id := NULL]

# 7 Save
saveRDS(winner_data,
        file.path(clean_data_dir, "clean_winner_data_kfst_name_matched.rds"))
saveRDS(manual_name_review, 
        file.path(clean_data_dir, "manual_name_review_kfst.rds"))
