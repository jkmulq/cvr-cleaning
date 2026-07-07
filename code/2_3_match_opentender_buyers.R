# Match missing OpenTender buyer CVRs using registered CVR names
# Includes a multiple firm check on buyer_names
# Author: Jack Mulqueeney
# Date: 30 June 2026

rm(list = ls())

# Load config
suppressWarnings(suppressPackageStartupMessages(library(here)))
source(here::here("config.R"))

# Packages
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Directories
clean_data_dir <- dirs$clean_data

# 1 Load the OpenTender buyers and the CVR-name keys
buyer_data <- readRDS(file.path(clean_data_dir, "clean_buyer_data_ot.rds"))
name_key <- readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds"))
biname_key <- readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds"))

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


# 2 Filter OT data
## Only attempt to fuzzy match Danish firms AND 
## row is missing cvr number and row has buyer name (flag_check_fuzzy_match)

## 2.1 Row id for later joining
buyer_data[, match_row_id := .I]
buyer_data[, buyer_name_in_data := buyer_name]

# The CVR key contains Danish firms, so only rows marked DK are automatically
remaining <- buyer_data[
  flag_check_fuzzy_match & toupper(trimws(buyer_country)) == "DK"
]

cat("Number observations to fuzzy match:", nrow(remaining))

# The CVR key records when a name was valid. 
# We will use tender publication dates to filter potential matches. 
remaining[, match_date := as.IDate(tender_publications_firstdContractAwardDate)]
remaining_original <- remaining

# Table to append matches at each step.
# Matched rows are removed from remaining (just as in matching.ipynb).
matched <- data.table()

# Some buyer-side matching steps can return no matches. In that case the
# temporary match table has no match_row_id column, so there is nothing to remove.
drop_matched_rows <- function(rows, matches) {
  if (nrow(matches) == 0 || !"match_row_id" %in% names(matches)) {
    return(rows)
  }
  
  rows[!matches, on = "match_row_id"]
}

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
new_matches <- add_buyer_context_to_matches(new_matches)
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
new_matches <- add_buyer_context_to_matches(new_matches)
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
new_matches <- add_buyer_context_to_matches(new_matches)
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
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches)
cat("Step 4 matches:", nrow(new_matches), "\n")

# Clean up 
rm(new_matches)
gc()

cat("total exact matches:", nrow(matched))
cat("share:", round(nrow(matched) / nrow(remaining_original), 3))


# 4 Exact matching after removing consortium language
## The previous four exact steps above use the untouched prepared buyer name.
## This stage only considers remaining names with a potential collaboration label.
## It removes consortium/JV labels from a temporary whole-name string and asks
## whether that complete string represents one registered firm before trying
## to split potential multiple firm entries into separate entries.

# Regex for consortium labels. Covers the Danish and English consortium/JV labels.
# Examples include: "Konsortiet", "konsortium", "konsortie", "ARC-Konsortiet",
# "Consortium", "Building Consortium", "joint venture", "joint-venture", "jointventure"
# "JV", "J.V.", "J V", "J. V.", among others.
consortium_language_pattern <- stringr::regex(
  paste(
    "konsorti(?:et|um|e)?",
    "consorti(?:um)?",
    "joint[ -]?venture",
    "(?<![[:alnum:]])j[.]?\\s*v[.]?(?![[:alnum:]])",
    "sammenslutningen(?:\\s+af)?",
    "i\\s+samarbejde\\s+med",
    "sammen\\s+med",
    sep = "|"
  ),
  ignore_case = TRUE
)
consortium_name_rows <- copy(remaining)

# Flag the relevant rows, then remove the labels from a temporary name.
consortium_name_rows[, flag_collaboration_text := stringr::str_detect(buyer_name, consortium_language_pattern)]
consortium_name_rows <- consortium_name_rows[flag_collaboration_text == TRUE, ]

# Small cleanup prior to matching processing
consortium_name_rows[
  ,
  buyer_name_without_consortium_language :=
    buyer_name |>
    stringr::str_remove_all(consortium_language_pattern) |>
    stringr::str_replace_all("[;:()]+", " ") |>
    stringr::str_squish()
]

# Prepare names
consortium_name_prepared <- prepare_cvr_name(consortium_name_rows$buyer_name_without_consortium_language)
consortium_name_rows[
  ,
  `:=`(
    buyer_name_basic = consortium_name_prepared$name_basic,
    buyer_name_match = consortium_name_prepared$name_clean,
    buyer_name_no_spaces = consortium_name_prepared$name_no_spaces,
    buyer_name_broad = consortium_name_prepared$name_broad,
    buyer_firm_type = consortium_name_prepared$firm_type
  )
]
consortium_remaining <- copy(consortium_name_rows)

## 4.1 Lightly prepared whole name and firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(
    name_basic = buyer_name_basic,
    firm_type = buyer_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(
  consortium_candidates,
  step = 5L
)
new_consortium_matches <- add_buyer_context_to_matches(new_consortium_matches)
keep_step_matches(new_consortium_matches)
consortium_remaining <- drop_matched_rows(
  consortium_remaining,
  new_consortium_matches
)

## 4.2 Generalized whole name without spaces, retaining firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(
    name_no_spaces = buyer_name_no_spaces,
    firm_type = buyer_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 6L)
new_consortium_matches <- add_buyer_context_to_matches(new_consortium_matches)
keep_step_matches(new_consortium_matches)
consortium_remaining <- drop_matched_rows(
  consortium_remaining,
  new_consortium_matches
)

## 4.3 Whole name without spaces, ignoring firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(name_no_spaces = buyer_name_no_spaces),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 7L)
new_consortium_matches <- add_buyer_context_to_matches(new_consortium_matches)
keep_step_matches(new_consortium_matches)
consortium_remaining <- drop_matched_rows(
  consortium_remaining,
  new_consortium_matches
)

## 4.4 Broad whole name, ignoring firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(name_broad = buyer_name_broad),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 8L)
new_consortium_matches <- add_buyer_context_to_matches(new_consortium_matches)
keep_step_matches(new_consortium_matches)


# 5 Deduplication
## OpenTender data isn't clear whether multiple firm names appear in the data
## and how they're separated. 
## This step performs substring exact matching detection to see whether we can 
## unambiguously parse out multiple firm names. 
# Generate every possible split/keep combination for names with no more than
# five potential delimiter positions. 
# Original row unchanged.

partition_summaries <- vector("list", nrow(remaining)) # Store summary of each partition test
partition_tables <- vector("list", nrow(remaining)) # Store the proposed segments for each partition test

for (row_number in seq_len(nrow(remaining))) {
  buyer <- remaining[row_number]
  result <- make_name_partitions(
    buyer$buyer_name,
    max_boundaries = 5L
  )

  # Only test partitions when there is positive evidence of several firms:
  # collaboration vocabulary or at least two legal forms. This protects
  # geography, divisions, generic fragments, and single legal-form names from
  # being interpreted as separate firms. 
  # The rule is recorded in TODO.md so it can be removed cleanly if the test is too restrictive.
  partition_eligible <- (result$flag_collaboration_text | result$n_legal_forms >= 2L)
  eligibility_reason <- fcase(
    result$flag_collaboration_text & result$n_legal_forms >= 2L,
    "collaboration vocabulary and multiple legal forms",
    result$flag_collaboration_text,
    "collaboration vocabulary",
    result$n_legal_forms >= 2L,
    "multiple legal forms",
    default = "excluded - insufficient multiple-firm evidence"
  )

  # Create data.table of paritions
  partition_summaries[[row_number]] <- data.table(
    match_row_id = buyer$match_row_id,
    name_partition_n_boundaries = result$n_boundaries,
    name_partition_n_legal_forms = result$n_legal_forms,
    flag_name_partition_eligible = partition_eligible,
    name_partition_eligibility_reason = eligibility_reason,
    flag_joint_venture_text = result$flag_joint_venture_text,
    flag_consortium_text = result$flag_consortium_text,
    flag_collaboration_text = result$flag_collaboration_text,
    too_many_delimiters = result$too_many_delimiters
  )

  if (partition_eligible && nrow(result$partitions) > 0) {
    partition_tables[[row_number]] <- copy(result$partitions)[
      ,
      `:=`(
        match_row_id = buyer$match_row_id,
        tender_id = buyer$tender_id,
        lot_id = buyer$lot_id,
        buyer_number = buyer$buyer_number,
        buyer_name_original = result$original_name,
        buyer_name_working = result$working_name,
        name_partition_n_boundaries = result$n_boundaries,
        name_partition_n_legal_forms = result$n_legal_forms,
        name_partition_eligibility_reason = eligibility_reason,
        flag_joint_venture_text = result$flag_joint_venture_text,
        flag_consortium_text = result$flag_consortium_text,
        flag_collaboration_text = result$flag_collaboration_text,
        match_date = buyer$match_date
      )
    ]
  }
}

# Bind into data.tables
name_partition_summary <- rbindlist(
  partition_summaries,
  use.names = TRUE,
  fill = TRUE
)

name_partition_segments <- rbindlist(
  partition_tables,
  use.names = TRUE,
  fill = TRUE
)

rm(partition_summaries, partition_tables, result, buyer)
gc()

# Print diagnostics
cat("Number of row eligible for segmentation:", 
    nrow(name_partition_summary[flag_name_partition_eligible == TRUE]), "\n")
cat("Number of segments extracted:", nrow(name_partition_segments), "\n")

## 5.1 Prepare each proposed firm-name segment
segment_names_prepared <- prepare_cvr_name(name_partition_segments$segment_text)

# Add prepared segment names to the segment table for matching.
name_partition_segments[
  ,
  `:=`(
    segment_match_id = .I,
    buyer_name_basic = segment_names_prepared$name_basic,
    buyer_name_match = segment_names_prepared$name_clean,
    buyer_name_no_spaces = segment_names_prepared$name_no_spaces,
    buyer_name_broad = segment_names_prepared$name_broad,
    buyer_firm_type = segment_names_prepared$firm_type
  )
]

# Setup matching infrastructure for segmented names. 
# Each segment is treated as a separate row for matching.
segment_remaining <- name_partition_segments[,
  .(
    match_row_id = segment_match_id,
    buyer_name_basic,
    buyer_name_match,
    buyer_name_no_spaces,
    buyer_name_broad,
    buyer_firm_type,
    match_date
  )
]
segment_matches <- data.table()

## 5.2 Exact segment match: prepared name and firm type
segment_candidates <- cvr_key[
  segment_remaining,
  on = .(
    name_basic = buyer_name_basic,
    firm_type = buyer_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_segment_matches <- select_preferred_exact_match(segment_candidates, step = 1L)
segment_matches <- rbindlist(
  list(segment_matches, new_segment_matches),
  use.names = TRUE,
  fill = TRUE
)
segment_remaining <- drop_matched_rows(
  segment_remaining,
  new_segment_matches
)

## 5.3 Exact segment match: name without spaces and firm type
segment_candidates <- cvr_key[
  segment_remaining,
  on = .(
    name_no_spaces = buyer_name_no_spaces,
    firm_type = buyer_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_segment_matches <- select_preferred_exact_match(segment_candidates, step = 2L)
segment_matches <- rbindlist(
  list(segment_matches, new_segment_matches),
  use.names = TRUE,
  fill = TRUE
)
segment_remaining <- drop_matched_rows(
  segment_remaining,
  new_segment_matches
)

## 5.4 Exact segment match: name without spaces, ignoring firm type
segment_candidates <- cvr_key[
  segment_remaining,
  on = .(name_no_spaces = buyer_name_no_spaces),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_segment_matches <- select_preferred_exact_match(
  segment_candidates,
  step = 3L
)
segment_matches <- rbindlist(
  list(segment_matches, new_segment_matches),
  use.names = TRUE,
  fill = TRUE
)
segment_remaining <- drop_matched_rows(
  segment_remaining,
  new_segment_matches
)

## 5.5 Exact segment match: broad name, ignoring firm type
segment_candidates <- cvr_key[
  segment_remaining,
  on = .(name_broad = buyer_name_broad),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_segment_matches <- select_preferred_exact_match(
  segment_candidates,
  step = 4L
)
segment_matches <- rbindlist(
  list(segment_matches, new_segment_matches),
  use.names = TRUE,
  fill = TRUE
)

# Join the segment matches back to the segment table
setnames(segment_matches, "match_row_id", "segment_match_id")
name_partition_segments[
  segment_matches,
  on = "segment_match_id",
  `:=`(
    segment_cvr_match = i.cvr_name_match,
    segment_registered_name_match = i.registered_name_match,
    segment_match_source = i.name_match_source,
    segment_match_step = i.name_match_step,
    segment_match_n_candidates = i.name_match_n_candidates
  )
]

## 5.6 Evaluate segment matches within each partition
partition_evaluation <- name_partition_segments[,
  .(name_partition_n_firms = .N,
    
    # Are all CVRs in a segment filled?
    all_segments_matched = all(!is.na(segment_cvr_match)), 
    
    # Did each matched CVR match only one CVR in the key?
    all_segments_unique = all(!is.na(segment_match_n_candidates) 
                              & segment_match_n_candidates == 1L),
    
    # Number of distinct CVRs matched in the partition.
    n_distinct_segment_cvrs = uniqueN(segment_cvr_match, na.rm = TRUE)
  ),
  by = .(match_row_id, partition_id, partition_text)]

# Flag 'complete' partitions:
# - all segments matched
# - all segments unique
# - number of distinct CVRs equals the number of segments in the partition
partition_evaluation[,
  partition_complete := (
    name_partition_n_firms >= 2L &
      all_segments_matched &
      all_segments_unique &
      n_distinct_segment_cvrs == name_partition_n_firms
  )
]

# Join partition evaluations back onto segments
name_partition_segments[
  partition_evaluation,
  on = .(match_row_id, partition_id, partition_text),
  `:=`(
    name_partition_n_firms = i.name_partition_n_firms,
    all_segments_matched = i.all_segments_matched,
    all_segments_unique = i.all_segments_unique,
    n_distinct_segment_cvrs = i.n_distinct_segment_cvrs,
    partition_complete = i.partition_complete
  )
]

# Separate out complete partitions
complete_partitions <- partition_evaluation[partition_complete == TRUE, ]

# Flag rows with multiple accepted partitions
complete_partitions[, partition_count := .N, by = match_row_id]
complete_partitions[, flag_multiple_complete_partitions := partition_count > 1L]

# Collapse to single rows
complete_summary <- complete_partitions[, .(
    name_partition_n_complete = .N,
    proposed_name_partition = fifelse(.N == 1L, first(partition_text), NA_character_),
    proposed_name_partition_n_firms = fifelse(.N == 1L, first(name_partition_n_firms), NA_integer_)
  ), by = match_row_id]

rm(
  segment_names_prepared,
  segment_remaining,
  segment_candidates,
  new_segment_matches,
  segment_matches,
  partition_evaluation,
  complete_partitions
)

## 5.7 Summarise the result for each original buyer row
# Create dummy variables in main partition summary object
name_partition_summary[,
  `:=`(
    name_partition_n_complete = 0L,
    proposed_name_partition = NA_character_,
    name_partition_n_firms = NA_integer_
  )
]

# Join complete partitions onto main partition object
name_partition_summary[
  complete_summary,
  on = "match_row_id",
  `:=`(
    name_partition_n_complete = i.name_partition_n_complete,
    proposed_name_partition = i.proposed_name_partition,
    name_partition_n_firms = i.proposed_name_partition_n_firms
  )
]

# Flag partition status
name_partition_summary[,
  name_partition_status := fcase(
    !flag_name_partition_eligible,
    "not tested: insufficient evidence",
    too_many_delimiters,
    "not tested: too many delimiters",
    name_partition_n_complete > 1L,
    "not accepted: multiple complete partitions",
    name_partition_n_complete == 1L,
    "accepted: unique complete partition",
    default = "no complete partition"
  )
]
name_partition_summary[, flag_potential_multiple_names := name_partition_n_complete > 0L]

# Separate accepted partitions
unique_partition_ids <- name_partition_summary[
  name_partition_status == "accepted: unique complete partition",
  .(match_row_id)
]
separated_name_segments <- name_partition_segments[
  partition_complete == TRUE &
    match_row_id %in% unique_partition_ids$match_row_id,
  .(
    match_row_id,
    name_partition_segment_number = segment_number,
    separated_buyer_name = segment_text,
    separated_buyer_name_basic = buyer_name_basic,
    separated_buyer_name_match = buyer_name_match,
    separated_buyer_name_no_spaces = buyer_name_no_spaces,
    separated_buyer_name_broad = buyer_name_broad,
    separated_buyer_firm_type = buyer_firm_type,
    separated_buyer_cvr = segment_cvr_match,
    separated_registered_name = segment_registered_name_match,
    separated_name_source = segment_match_source,
    separated_match_step = segment_match_step,
    separated_n_candidates = segment_match_n_candidates
  )
]

# Remove successfully separated rows from remaining object
remaining <- remaining[
  !unique_partition_ids,
  on = "match_row_id"
]

# Rows with several complete partitions are ambiguous. 
# Keep their original rows and partition diagnostics for manual review
# do not fuzzy match the combined buyer name.
potential_multiple_ids <- name_partition_summary[
  name_partition_status == "not accepted: multiple complete partitions",
  .(match_row_id)]
remaining <- remaining[!potential_multiple_ids, on = "match_row_id"]

rm(complete_summary, cvr_key)
gc()

# 6 Fuzzy matching
# Create storage table
fuzzy_candidates <- data.table()

## 6.1 Main name key, full buyer name
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
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to larger matched dataset
cat("Number of new fuzzy matches:", nrow(new_matches))

## 6.2 Biname key, full buyer name
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
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to larger matched dataset
cat("Number of new fuzzy matches:", nrow(new_matches))

## 6.3 main name key, but using the broader name
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
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to new matched dataset
cat("Number of fuzzy matches:", nrow(new_matches))

## 6.4 biname name key, but using the broader name
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
new_matches <- add_buyer_context_to_matches(new_matches)
keep_step_matches(new_matches) # Append to matched dataset
cat("Number of fuzzy matches", nrow(new_matches))

rm(new_matches, step_candidates, name_key, biname_key)
gc()


# 7 Join matches back to the full OpenTender buyer data
# A buyer can have candidates from more than one fuzzy step or key. Rank them
# together, remove repeated CVRs, and retain the best five in wide columns.
if (nrow(fuzzy_candidates) > 0) {
  fuzzy_candidates[, source_order := fifelse(fuzzy_candidate_source == "name", 1L, 2L)]

  # Arrange to take top candidate scores
  setorder(
    fuzzy_candidates,
    match_row_id,
    -fuzzy_candidate_score,
    fuzzy_candidate_step,
    source_order,
    fuzzy_candidate_rank
  )

  # Make distinct and take top five candidates for each row
  fuzzy_candidates <- unique(fuzzy_candidates, by = c("match_row_id", "fuzzy_candidate_cvr"))
  fuzzy_candidates <- fuzzy_candidates[, head(.SD, 5), by = match_row_id]
  fuzzy_candidates[, fuzzy_candidate_rank := seq_len(.N), by = match_row_id]
  
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

# Add the multiple-firm diagnostics to the original buyer rows.
# Note, rows that received a whole-name exact match were not assessed for delimiters.
buyer_data[name_partition_summary, on = "match_row_id",
  `:=`(
    name_partition_status = i.name_partition_status,
    name_partition_n_boundaries = i.name_partition_n_boundaries,
    name_partition_n_legal_forms = i.name_partition_n_legal_forms,
    flag_name_partition_eligible = i.flag_name_partition_eligible,
    name_partition_eligibility_reason = i.name_partition_eligibility_reason,
    name_partition_n_complete = i.name_partition_n_complete,
    proposed_name_partition = i.proposed_name_partition,
    name_partition_n_firms = i.name_partition_n_firms,
    flag_potential_multiple_names = i.flag_potential_multiple_names,
    flag_joint_venture_text = i.flag_joint_venture_text,
    flag_consortium_text = i.flag_consortium_text,
    flag_collaboration_text = i.flag_collaboration_text)]

# Coalesce flags to FALSE for rows that were not assessed for multiple buyers
buyer_data[, `:=`(
    flag_potential_multiple_names = fcoalesce(flag_potential_multiple_names, FALSE),
    flag_joint_venture_text = fcoalesce(flag_joint_venture_text, FALSE),
    flag_consortium_text = fcoalesce(flag_consortium_text, FALSE),
    flag_collaboration_text = fcoalesce(flag_collaboration_text, FALSE))]

# Join match data onto dataset
buyer_data[matched, on = "match_row_id",
  `:=`(
    buyer_cvr_name_match = i.cvr_name_match,
    registered_name_match = i.registered_name_match,
    name_match_source = i.name_match_source,
    name_match_step = i.name_match_step,
    name_match_method = i.name_match_method,
    name_match_score = i.name_match_score,
    name_match_n_candidates = i.name_match_n_candidates)]

# Replace each original combined-name row with one row per uniquely matched
# segment. The original row_id and buyer_name_original remain on every new row
# so the expansion can always be traced back to the OpenTender source.
buyer_data[, flag_name_partition_expanded := FALSE]
buyer_data[, flag_separated_name := fifelse(match_row_id %in% unique_partition_ids$match_row_id, 
                                            TRUE, FALSE)]

# Join the accepted partition segments onto their original buyer rows.
separated_buyer_data <- buyer_data[unique_partition_ids,
                                     on = "match_row_id",
                                     nomatch = 0]

# Expand the buyer data with the joined segments
# Means every separated buyer gets the data from the source row.
separated_buyer_data <- separated_buyer_data[separated_name_segments, 
                                             on = "match_row_id", nomatch = 0]

# Rename
separated_buyer_data[,
  `:=`(
    buyer_name = separated_buyer_name,
    buyer_name_basic = separated_buyer_name_basic,
    buyer_name_match = separated_buyer_name_match,
    buyer_name_no_spaces = separated_buyer_name_no_spaces,
    buyer_name_broad = separated_buyer_name_broad,
    buyer_firm_type = separated_buyer_firm_type,
    buyer_name_first_letter = substr(separated_buyer_name_match, 1, 1),
    buyer_cvr_name_match = separated_buyer_cvr,
    registered_name_match = separated_registered_name,
    name_match_source = separated_name_source,
    name_match_step = separated_match_step,
    name_match_method = "exact",
    name_match_score = 100,
    name_match_n_candidates = separated_n_candidates,
    flag_name_partition_expanded = TRUE)]

# Remove old separated columns to avoid confusion
separated_columns <- grep(
  "^separated_",
  names(separated_buyer_data),
  value = TRUE
)
separated_buyer_data[, (separated_columns) := NULL]
separated_buyer_data[, flag_separated_name := TRUE]

# Remove original combined-name rows then appending separated rows
buyer_data <- buyer_data[flag_separated_name == FALSE, ]
buyer_data[, name_partition_segment_number := NA_integer_]
buyer_data <- rbindlist(
  list(buyer_data, separated_buyer_data),
  use.names = TRUE,
  fill = TRUE
)

# Give each numeric matching step a stable, descriptive code. 
# Keep numeric step so the matching route remains easy to inspect.
buyer_data[, name_match_step_code := fcase(
  flag_name_partition_expanded & name_match_step == 1L,
  "exact partition: basic name and firm type",
  flag_name_partition_expanded & name_match_step == 2L,
  "exact partition: no spaces and firm type",
  flag_name_partition_expanded & name_match_step == 3L,
  "exact partition: no spaces",
  flag_name_partition_expanded & name_match_step == 4L,
  "exact partition: broad name",
  name_match_method == "exact" & name_match_step == 1L,
  "exact: basic name and firm type",
  name_match_method == "exact" & name_match_step == 2L,
  "exact: no spaces and firm type",
  name_match_method == "exact" & name_match_step == 3L,
  "exact: no spaces",
  name_match_method == "exact" & name_match_step == 4L,
  "exact: broad name",
  name_match_method == "exact" & name_match_step == 5L,
  "exact consortium removed: basic name and firm type",
  name_match_method == "exact" & name_match_step == 6L,
  "exact consortium removed: no spaces and firm type",
  name_match_method == "exact" & name_match_step == 7L,
  "exact consortium removed: no spaces",
  name_match_method == "exact" & name_match_step == 8L,
  "exact consortium removed: broad name",
  name_match_method == "fuzzy" & name_match_step == 5L &
    name_match_source == "name",
  "fuzzy: prepared main name",
  name_match_method == "fuzzy" & name_match_step == 5L &
    name_match_source == "biname",
  "fuzzy: prepared biname",
  name_match_method == "fuzzy" & name_match_step == 6L &
    name_match_source == "name",
  "fuzzy: broad main name",
  name_match_method == "fuzzy" & name_match_step == 6L &
    name_match_source == "biname",
  "fuzzy: broad biname",
  flag_fill_missing_cvr,
  "source: same-name CVR fill",
  !is.na(buyer_cvr_clean) & buyer_cvr_clean != "" &
    source == "single buyer",
  "source: single CVR cleaning",
  !is.na(buyer_cvr_clean) & buyer_cvr_clean != "" &
    source == "multiple CVRs",
  "source: multiple CVR separation",
  !is.na(buyer_cvr_clean) & buyer_cvr_clean != "",
  "source: existing CVR, unclassified",
  flag_check_fuzzy_match &
    toupper(trimws(buyer_country)) == "DK",
  "matching candidate: no match found",
  flag_check_fuzzy_match,
  "not a matching candidate: not marked as Danish",
  default = "not a matching candidate: no CVR name"
)]

# Fuzzy matches and matches tied across several CVRs are retained but flagged.
buyer_data[, flag_name_match_found := !is.na(buyer_cvr_name_match)]
buyer_data[, flag_name_match_ambiguous := (flag_name_match_found & name_match_n_candidates > 1)]
buyer_data[, flag_review_name_match := (
  (flag_potential_multiple_names & !flag_name_partition_expanded) |
    (flag_name_match_found & (name_match_method == "fuzzy" | flag_name_match_ambiguous))
)]

# Step 7 in the documentation is manual review. This flag includes:
#   - rows that did not receive a match in the CVR-name key
#   - fuzzy matches
#   - matches where several CVRs were possible
#   - partition candidates that did not have a unique complete partition
buyer_data[, flag_manual_name_review := (
  flag_check_fuzzy_match & # Fuzzy matches
    (!flag_name_match_found | flag_review_name_match)
)]

# Create final cvr number
# Start with the CVR obtained from the original cleaning process.
buyer_data[, buyer_cvr_final := as.character(buyer_cvr_clean)]

# Fill missing CVRs from name matching
buyer_data[flag_check_fuzzy_match & # Candidates for matching
              toupper(trimws(buyer_country)) == "DK" & # Danish firm 
              !flag_potential_multiple_names & # Not a potential multiple-name row
              !is.na(buyer_cvr_name_match), # Has a matched CVR number
  buyer_cvr_final := buyer_cvr_name_match]

# Successfully separated multiple firms use their segment-level CVR matches.
buyer_data[flag_name_partition_expanded == TRUE, buyer_cvr_final := buyer_cvr_name_match]

# Readable name-match status
buyer_data[, name_match_status := fcase(
  flag_name_partition_expanded,
  "matched - separated buyer name",
  !flag_check_fuzzy_match,
  "not requested",
  flag_potential_multiple_names,
  "manual review - potential multiple buyers",
  flag_review_name_match,
  "manual review - fuzzy or ambiguous match",
  name_match_method == "exact" & name_match_step %in% 5:8,
  "matched - consortium language removed",
  flag_name_match_found,
  "matched",
  is.na(buyer_country) | toupper(trimws(buyer_country)) != "DK",
  "manual review - not marked as Danish",
  default = "manual review - no automatic match"
)]

# Save a compact table containing only rows that need a person to inspect.
manual_buyer_name_review <- buyer_data[
  flag_manual_name_review == TRUE,
  .(
    row_id,
    tender_id,
    lot_id,
    buyer_number,
    buyer_name_in_data,
    buyer_name,
    buyer_name_match,
    buyer_firm_type,
    buyer_country,
    tender_publications_firstdContractAwardDate,
    buyer_cvr_name_match,
    registered_name_match,
    name_partition_status,
    name_partition_n_legal_forms,
    flag_name_partition_eligible,
    name_partition_eligibility_reason,
    proposed_name_partition,
    name_partition_n_firms,
    name_partition_n_complete,
    flag_potential_multiple_names,
    flag_joint_venture_text,
    flag_consortium_text,
    flag_collaboration_text,
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
]

# Add the five retained fuzzy names and scores when those columns exist.
fuzzy_review_columns <- grep(
  "^fuzzy_candidate_(cvr|name|score)_",
  names(buyer_data),
  value = TRUE
)
if (length(fuzzy_review_columns) > 0) {
  manual_buyer_name_review <- cbind(
    manual_buyer_name_review,
    buyer_data[
      flag_manual_name_review == TRUE,
      ..fuzzy_review_columns
    ]
  )
}

# Delete the temporary joining identifier.
buyer_data[, match_row_id := NULL]

# 8 Save
saveRDS(buyer_data, file.path(clean_data_dir, "clean_buyer_data_ot_name_matched.rds"))
saveRDS(manual_buyer_name_review, file.path(clean_data_dir, "manual_buyer_name_review_ot.rds"))
saveRDS(name_partition_segments, file.path(clean_data_dir, "buyer_name_partition_diagnostics_ot.rds"))
