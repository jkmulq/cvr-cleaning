# Match missing KFST winner CVRs using registered CVR names
# Author: Jack Mulqueeney
# Date: 30 June 2026

rm(list = ls())

# Load config
library(here)
source(here::here("config.R"))

# Packages
library(data.table)

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Directories
clean_data_dir <- dirs$clean_data

# 1 Load the KFST winners and the CVR-name keys
buyer_data <- readRDS(
  file.path(clean_data_dir, "clean_buyer_data_kfst.rds")
)
name_key <- readRDS(
  file.path(clean_data_dir, "clean_cvr_name_key.rds")
)
biname_key <- readRDS(
  file.path(clean_data_dir, "clean_cvr_biname_key.rds")
)

setDT(buyer_data)
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
## 2.1 Row id for later joining
buyer_data[, match_row_id := .I]
buyer_data[, buyer_name_in_data := buyer_name]
remaining <- buyer_data[flag_check_fuzzy_match == TRUE, ]
cat("No. observations to match:", nrow(remaining))

# The CVR key records when a name was valid. 
# We will use tender publication dates to filter potential matches. 
remaining[, match_date := as.IDate(pub_date)]
remaining_original <- remaining

# Table to append matches at each step.
# Matched rows are removed from remaining (just as in matching.ipynb).
matched <- data.table()

# 3 Exact matching
## 3.1 Match on lightly prepared name and firm type
candidate_matches <- cvr_key[
  remaining,
  on = .(
    name_basic = buyer_name_basic,
    firm_type = buyer_firm_type
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
keep_step_matches(new_matches) # Remove successful matches from remaining
cat("Step 1 matches:", nrow(new_matches), "\n")

## 3.2 match on generalized name without spaces, retaining firm type.
candidate_matches <- cvr_key[
  remaining,
  on = .(
    name_no_spaces = buyer_name_no_spaces,
    firm_type = buyer_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_matches <- select_preferred_exact_match(candidate_matches, step = 2L)
keep_step_matches(new_matches)
cat("Step 2 matches:", nrow(new_matches), "\n")

## 3.3 the same generalized name, now ignoring firm type
candidate_matches <- cvr_key[
  remaining,
  on = .(name_no_spaces = buyer_name_no_spaces),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_matches <- select_preferred_exact_match(candidate_matches, step = 3L)
keep_step_matches(new_matches)
cat("Step 3 matches:", nrow(new_matches), "\n")

## 3.4 remove common words, ignore word order, and ignore firm type.
candidate_matches <- cvr_key[
  remaining,
  on = .(name_broad = buyer_name_broad),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_matches <- select_preferred_exact_match(candidate_matches, step = 4L)
keep_step_matches(new_matches)
cat("Step 4 matches:", nrow(new_matches), "\n")

rm(new_matches, cvr_key)
gc()


# 4 Fuzzy matching
# Create storage table
fuzzy_candidates <- data.table()

## 4.1 Main name key, full winner name
# Find top 5 fuzzy matches
step_candidates <- find_fuzzy_matches(
  remaining,
  name_key,
  entity_name_column = "buyer_name_match",
  key_name_column = "name_match",
  first_letter_column = "first_letter",
  step = 5L,
  firm_type_column = "buyer_firm_type"
)

# Append new match candidates the fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept only if match score exceeds 85
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
keep_step_matches(new_matches) # Append to larger matched dataset
cat("Number of new fuzzy matches:", nrow(new_matches))

## 4.2 Biname key, full winner name
step_candidates <- find_fuzzy_matches(
  remaining,
  biname_key,
  entity_name_column = "buyer_name_match",
  key_name_column = "name_match",
  first_letter_column = "first_letter",
  step = 5L,
  firm_type_column = "buyer_firm_type"
)

# Append new match candidates the fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept only if match score exceeds 85
new_matches <- accept_fuzzy_match(step_candidates, threshold = 85)
keep_step_matches(new_matches) # Append to larger matched dataset
cat("Number of new fuzzy matches:", nrow(new_matches))

## 4.3 main name key, but using the broader name
# The documented thresholds are 86 for main names and 89 for binames.
step_candidates <- find_fuzzy_matches(
  remaining,
  name_key,
  entity_name_column = "buyer_name_broad",
  key_name_column = "name_broad",
  first_letter_column = "broad_first_letter",
  step = 6L,
  firm_type_column = "buyer_firm_type"
)

# Append new match candidates the fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept only if match score exceeds 86
new_matches <- accept_fuzzy_match(step_candidates, threshold = 86)
keep_step_matches(new_matches) # Append to new matched dataset
cat("Number of fuzzy matches:", nrow(new_matches))

## 4.4 biname name key, but using the broader name
step_candidates <- find_fuzzy_matches(
  remaining,
  biname_key,
  entity_name_column = "buyer_name_broad",
  key_name_column = "name_broad",
  first_letter_column = "broad_first_letter",
  step = 6L,
  firm_type_column = "buyer_firm_type"
)

# Append new match candidates to fuzzy_candidates
fuzzy_candidates <- rbindlist(
  list(fuzzy_candidates, step_candidates),
  use.names = TRUE,
  fill = TRUE
)

# Accept if threshold exceeds 89
new_matches <- accept_fuzzy_match(step_candidates, threshold = 89)
keep_step_matches(new_matches) # Append to matched dataset
cat("Number of fuzzy matches", nrow(new_matches))

rm(new_matches, step_candidates, name_key, biname_key)
gc()


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
  
  buyer_data <- merge(
    buyer_data,
    fuzzy_candidates_wide,
    by = "match_row_id",
    all.x = TRUE,
    sort = FALSE
  )
}

buyer_data[
  matched,
  on = "match_row_id",
  `:=`(
    buyer_cvr_name_match = i.cvr_name_match,
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
buyer_data[, name_match_step_code := fcase(
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
  default = "match failure"
)]

# Fuzzy matches and matches tied across several CVRs are retained but flagged.
buyer_data[, flag_name_match_found := !is.na(buyer_cvr_name_match)]
buyer_data[, flag_name_match_ambiguous := (
  flag_name_match_found &
    name_match_n_candidates > 1
)]
buyer_data[, flag_review_name_match := (
  flag_name_match_found &
    (
      name_match_method == "fuzzy" |
        flag_name_match_ambiguous
    )
)]

# Step 7 in the documentation is manual review. This flag includes:
#   - rows that did not receive a match;
#   - fuzzy matches;
#   - matches where several CVRs were possible.
buyer_data[, flag_manual_name_review := (flag_check_fuzzy_match & (!flag_name_match_found | flag_review_name_match))]

# Create final CVR for buyers
buyer_data[, buyer_cvr_final :=  buyer_cvr_name_match]

buyer_data[, name_match_status := fcase(
  !flag_check_fuzzy_match,
  "not requested",
  flag_review_name_match,
  "manual review - fuzzy or ambiguous match",
  flag_name_match_found,
  "matched",
  default = "manual review - no automatic match"
)]

# Save a compact table containing only rows that need a person to inspect.
manual_name_review <- buyer_data[flag_manual_name_review == TRUE, ] %>% 
  select(
    tender_id, lot_id, buyer_name, buyer_name_match, buyer_firm_type,
    pub_date,
    buyer_cvr_name_match, registered_name_match,
    starts_with("fuzzy_candidate_cvr"), starts_with("fuzzy_candidate_name"), starts_with("fuzzy_candidate_score"),
    name_match_step, name_match_step_code, name_match_method,
    name_match_score,name_match_n_candidates, flag_name_match_ambiguous, name_match_status
  )

# Delete match_row_id
buyer_data[, match_row_id := NULL]

# 7 Save
saveRDS(buyer_data, file.path(clean_data_dir, "clean_buyer_data_kfst_name_matched.rds"))
saveRDS(manual_name_review,  file.path(clean_data_dir, "manual_buyer_name_review_kfst.rds"))
