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
keep_step_matches(auto_consortium_matches)
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
keep_step_matches(auto_consortium_matches)
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
consortium_remaining <- consortium_remaining[!new_consortium_matches, on = "match_row_id"]

## 4.4 Broad whole name, ignoring firm type
consortium_candidates <- cvr_key[
  consortium_remaining,
  on = .(name_broad = winner_name_broad),
  nomatch = 0,
  allow.cartesian = TRUE
]
new_consortium_matches <- select_preferred_exact_match(consortium_candidates, step = 8L)
consortium_name_results <- rbindlist(
  list(consortium_name_results, new_consortium_matches),
  use.names = TRUE,
  fill = TRUE
)

rm(
  consortium_name_prepared,
  consortium_remaining,
  consortium_candidates,
  new_consortium_matches,
  auto_consortium_matches
)

