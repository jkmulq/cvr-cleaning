# Match missing OpenTender winner CVRs using registered CVR names
# Includes a multiple firm check on winner_names
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

# 1 Load the OpenTender winners and the CVR-name keys
winner_data <- readRDS(file.path(clean_data_dir, "clean_winner_data_ot.rds"))
name_key <- readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds"))
biname_key <- readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds"))

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


# 2 Filter OT data
## Only attempt to fuzzy match Danish firms AND 
## row is missing cvr number and row has winning firm name (flag_check_fuzzy_match)

## 2.1 Row id for later joining
winner_data[, match_row_id := .I]

# The CVR key contains Danish firms, so only rows marked DK are automatically
remaining <- winner_data[
  flag_check_fuzzy_match &
    toupper(trimws(winner_country)) == "DK"
]

cat("Number observations to fuzzy match:", nrow(remaining))

# The CVR key records when a name was valid. 
# We will use tender publication dates to filter potential matches. 
remaining[, match_date := as.IDate(tender_publications_firstdContractAwardDate)]
remaining_original <- remaining

# Table to append matches at each step.
# Matched rows are removed from remaining (just as in matching.ipynb).
matched <- data.table()


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
keep_step_matches(new_matches)
cat("Step 4 matches:", nrow(new_matches), "\n")

# Clean up 
rm(new_matches)
gc()

cat("total exact matches:", nrow(matched))
cat("share:", round(nrow(matched) / nrow(remaining_original), 3))


# 4 Exact matching after removing consortium language
## The previous four exact steps above use the untouched prepared winner name.
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
consortium_name_rows[, flag_collaboration_text := stringr::str_detect(winner_name, consortium_language_pattern)]
consortium_name_rows <- consortium_name_rows[flag_collaboration_text == TRUE, ]
consortium_name_rows[, winner_name_without_consortium_language := str_remove_all(winner_name, consortium_language_pattern)]

# Small cleanup prior to matching processing
consortium_name_rows[
  ,
  winner_name_without_consortium_language :=
    winner_name %>% 
    stringr::str_remove_all(consortium_language_pattern)  %>% 
    stringr::str_replace_all("[;:()]+", " ")  %>% 
    stringr::str_squish()
]

# Prepare names
consortium_name_prepared <- prepare_cvr_name(consortium_name_rows$winner_name_without_consortium_language)
consortium_name_rows[
  ,
  `:=`(
    winner_name_basic = consortium_name_prepared$name_basic,
    winner_name_match = consortium_name_prepared$name_clean,
    winner_name_no_spaces = consortium_name_prepared$name_no_spaces,
    winner_name_broad = consortium_name_prepared$name_broad,
    winner_firm_type = consortium_name_prepared$firm_type
  )
]
consortium_remaining <- copy(consortium_name_rows)
consortium_name_results <- data.table()

## 4.1 Lightly prepared whole name and firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(
    name_basic = winner_name_basic,
    firm_type = winner_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(
  consortium_candidates,
  step = 5L
)
consortium_name_results <- rbindlist(
  list(consortium_name_results, new_consortium_matches),
  use.names = TRUE,
  fill = TRUE
)
keep_step_matches(new_consortium_matches)
consortium_remaining <- consortium_remaining[
  !new_consortium_matches,
  on = "match_row_id"
]

## 4.2 Generalized whole name without spaces, retaining firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(
    name_no_spaces = winner_name_no_spaces,
    firm_type = winner_firm_type
  ),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 6L)
consortium_name_results <- rbindlist(
  list(consortium_name_results, new_consortium_matches),
  use.names = TRUE,
  fill = TRUE
)
keep_step_matches(new_consortium_matches)
consortium_remaining <- consortium_remaining[
  !new_consortium_matches,
  on = "match_row_id"
]

## 4.3 Whole name without spaces, ignoring firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(name_no_spaces = winner_name_no_spaces),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 7L)
consortium_name_results <- rbindlist(
  list(consortium_name_results, new_consortium_matches),
  use.names = TRUE,
  fill = TRUE
)
keep_step_matches(new_consortium_matches)
consortium_remaining <- consortium_remaining[!new_consortium_matches, on = "match_row_id"]

## 4.4 Broad whole name, ignoring firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(name_broad = winner_name_broad),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 8L)
keep_step_matches(new_consortium_matches)
consortium_name_results <- rbindlist(
  list(consortium_name_results, new_consortium_matches),
  use.names = TRUE,
  fill = TRUE
)


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
  winner <- remaining[row_number]
  result <- make_winner_name_partitions(
    winner$winner_name,
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
    match_row_id = winner$match_row_id,
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
        match_row_id = winner$match_row_id,
        tender_id = winner$tender_id,
        lot_id = winner$lot_id,
        winner_number = winner$winner_number,
        winner_name_original = result$original_name,
        winner_name_working = result$working_name,
        name_partition_n_boundaries = result$n_boundaries,
        name_partition_n_legal_forms = result$n_legal_forms,
        name_partition_eligibility_reason = eligibility_reason,
        flag_joint_venture_text = result$flag_joint_venture_text,
        flag_consortium_text = result$flag_consortium_text,
        flag_collaboration_text = result$flag_collaboration_text,
        match_date = winner$match_date
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

rm(partition_summaries, partition_tables, result, winner)
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
    winner_name_basic = segment_names_prepared$name_basic,
    winner_name_match = segment_names_prepared$name_clean,
    winner_name_no_spaces = segment_names_prepared$name_no_spaces,
    winner_name_broad = segment_names_prepared$name_broad,
    winner_firm_type = segment_names_prepared$firm_type
  )
]

# Setup matching infrastructure for segmented names. 
# Each segment is treated as a separate row for matching.
segment_remaining <- name_partition_segments[,
  .(
    match_row_id = segment_match_id,
    winner_name_basic,
    winner_name_match,
    winner_name_no_spaces,
    winner_name_broad,
    winner_firm_type,
    match_date
  )
]
segment_matches <- data.table()
