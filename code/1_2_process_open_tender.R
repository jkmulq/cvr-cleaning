# Cleans OpenTender provided tender data
# Author: Jack Mulqueeney
# Date: 18 June 2026

# Clean environment
rm(list = ls())

# Config: edit config.R at the project root to set your own PROJECT_DIR and Stata path
library(here)
source(here::here("config.R"))

# Packages
library(haven)
library(tidyverse)
library(readxl)
library(data.table)

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Paths
raw_data_dir <- dirs$raw_data
raw_data_names <- list.files(file.path(raw_data_dir, "OpenTender"))
raw_data_paths <- file.path(raw_data_dir, "OpenTender", raw_data_names)


# 1 Data
## 1.1 Check column-name concordance across yearly files
### 1.1.1 Extract columns names into a list and append to schema
# Note, data is semi-colon separated
data_col_names <- map(raw_data_paths, read.csv,
                      sep = ";", nrows = 0, check.names = FALSE) %>% 
  setNames(raw_data_names)
data_col_names <- map(data_col_names, ~as_tibble(names(.x))) # Extract the column names
data_col_names <- bind_rows(data_col_names, .id = "dataset")

### 1.1.2 Check column name equality of all combinations
data_name_combos <- combn(raw_data_names, m = 2)

# Apply over columns of combinations the difference in column names
col_name_diffs <- apply(data_name_combos, MARGIN = 2, FUN = function(col) {
  
  # Data names corresponding to col element 1
  df1 <- data_col_names %>% 
    filter(dataset == col[1])
  
  # Data names corresponding to col element 2
  df2 <- data_col_names %>% 
    filter(dataset == col[2])
  
  # Check whether the two datasets have exactly the same column names
  out <- setequal(df1$value, df2$value)
  return(out)

})

if (all(col_name_diffs)) {
  print("all column names concord across datasets")
} else {
  stop("some column names do not concord across datasets")
}

## 1.2 Load data
# Note, data is semi colon separated.
data_ls <- map(raw_data_paths, data.table::fread, sep = ";", colClasses = "character") %>% 
  setNames(raw_data_names)

# Bind into one dataframe
# Ignoring attributes because read.csv() is bad at guessing column types
data <- rbindlist(
  data_ls,
  use.names = TRUE,
  fill = FALSE,
  idcol = "dataset",
  ignore.attr = TRUE
)

data <- as_tibble(data)

# Keep a stable reference to the original OpenTender row.
# This lets expanded winner rows point back to the raw bid row.
data <- data %>%
  mutate(row_id = row_number()) %>% 
  select(row_id, everything())

## 1.3 Original tender data
## Keep all OpenTender source fields, but rename the variables that clearly
## correspond to the KFST naming convention.
original_tender_data <- data %>%
  rename(
    lot_id = lot_lotId,
    lot_number = lot_lotNumber,
    n_bids_received = lot_bidsCount,
    n_lots = tender_lots_count,
    submit_date = tender_bidDeadline,
    award_date = tender_awardDecisionDate,
    cpv_code = tender_cpvs,
    divided_tender = tender_hasLots,
    joint_tender = tender_isJointProcurement,
    consortium_winner = bid_isConsortium,
    winner_cvr_original = bidder_bodyIds,
    winner_name_original = bidder_name,
    winner_country_original = bidder_country,
    buyer_cvr_original = buyer_bodyIds,
    buyer_name_original = buyer_name,
    buyer_country_original = buyer_country
  ) %>%
  mutate(
    tender_cancelled = coalesce(
      (!is.na(tender_cancellationDate) & tender_cancellationDate != "") |
        (!is.na(lot_cancellationDate) & lot_cancellationDate != ""),
      FALSE
    )
  )

## 1.4 Separate winners/buyers/original data
winner_data_original <- data %>% 
  select(row_id, tender_id, bidder_bodyIds, bidder_name, bidder_country) %>% 
  rename(winner_cvr = bidder_bodyIds, winner_name = bidder_name, winner_country = bidder_country)
buyer_data_original <- data %>% 
  select(row_id, tender_id, buyer_bodyIds, buyer_name, buyer_country) %>% 
  rename(buyer_cvr = buyer_bodyIds)


# 2 Winner data
# Duplicate data so I can bind back later
winner_data <- winner_data_original

# Guard against whitespace acting as an unhandled delimiter between two CVRs.
# The helper removes whitespace to repair spaced CVRs, so these rows would become
# one 16-digit string and incorrectly bypass the multiple-CVR cleaning workflow.
whitespace_separated_cvr_rows <- winner_data %>%
  filter(str_detect(
    winner_cvr,
    "(?<![0-9])[0-9]{8}[[:space:]]+[0-9]{8}(?![0-9])"
  )) %>%
  select(row_id, tender_id, winner_cvr)

if (nrow(whitespace_separated_cvr_rows) > 0) {
  print(whitespace_separated_cvr_rows)
  stop("Some winner rows contain two CVRs separated only by whitespace.")
}

## 2.1 Count distinct CVR numbers by row
winner_data <- winner_data %>% 
  mutate(
    # First pass: count CVRs that already appear as distinct eight-digit runs.
    n_valid_cvr_raw = compute_distinct_valid_cvr(winner_cvr),
    
    # Second pass: only for Danish rows where the first pass found nothing,
    # recover one CVR if punctuation or prefixes were the only problem.
    winner_cvr_recovered_from_formatting = recover_formatted_danish_cvr(
      cvr_candidate = winner_cvr,
      country = winner_country,
      n_valid_cvr_raw = n_valid_cvr_raw
    ),
    flag_cvr_recovered_from_formatting = coalesce(
      !is.na(winner_cvr_recovered_from_formatting),
      FALSE
    ),
    
    # Third pass: count again using the recovered CVR where one was found. This
    # is the count used to decide whether the row should be split.
    winner_cvr_for_count = ifelse(
      flag_cvr_recovered_from_formatting,
      winner_cvr_recovered_from_formatting,
      winner_cvr
    ),
    n_valid_cvr = compute_distinct_valid_cvr(winner_cvr_for_count),
    flag_row_multiple_valid_cvr = (n_valid_cvr > 1)
  ) %>%
  select(
    -winner_cvr_for_count
  )

n_formatted_winner_cvrs_recovered <- sum(
  winner_data$flag_cvr_recovered_from_formatting
)
cat("Number of formatted winner CVRs recovered:",
    n_formatted_winner_cvrs_recovered, "\n")

## 2.2 Standardise CVR number delimiters
winner_data <- winner_data %>%
  mutate(
    winner_cvr = ifelse(
      flag_row_multiple_valid_cvr,
      str_replace_all(
        winner_cvr,
        regex(
          "\\s*(,|;|\\||/|&|\\bog\\b|(?<=[\\d\\)])og(?=[[:alnum:]]))\\s*",
          ignore_case = TRUE
        ),
        ";"
      ),
      winner_cvr
    ),
    winner_cvr = str_replace_all(winner_cvr, ";+", ";")
  )


# Print number of true multi CVR numbers.
n_true_multi_cvrs <- sum(winner_data$flag_row_multiple_valid_cvr, na.rm = TRUE)
cat("Number of true multi CVR numbers detected:", n_true_multi_cvrs, "\n")

## 2.3 Separate into single and multiple CVRs
# Multiple distinct CVR numbers
multi_winner_data <- winner_data %>% 
  filter(flag_row_multiple_valid_cvr)

# Only one valid CVR
single_winner_data <- winner_data %>% 
  filter(!flag_row_multiple_valid_cvr)

# Check these datasets cover complete data dataset
if (nrow(multi_winner_data) + 
    nrow(single_winner_data) - 
    nrow(winner_data) != 0) {
  stop("subsetted datasets do not have the same number of rows as the full winner dataset")
}

## 2.4 Multi-winner data with confirmed multiple firms
### 2.4.1 Split by CVR number
# These are separated by a standardised delimiter ';', so easy to separate out.
multi_winner_data_long <- multi_winner_data %>% 
  separate_longer_delim(cols = winner_cvr, delim = ";")

# Rename and create copy
multi_winner_data_long <- multi_winner_data_long %>% 
  rename(winner_cvr_candidate = winner_cvr)

### 2.4.2 Extract and clean CVR
multi_winner_data_long$winner_cvr_clean <- map_chr(multi_winner_data_long$winner_cvr_candidate,
                                                   extract_valid_cvr_candidates)

# Flag the cleaning steps
multi_winner_data_long <- multi_winner_data_long %>% 
  mutate(
    flag_cvr_placeholder = coalesce(
      winner_cvr_clean %in% known_invalid_cvr_numbers(),
      FALSE
    ),
    winner_cvr_clean = ifelse(
      flag_cvr_placeholder,
      NA_character_,
      winner_cvr_clean
    ),
    
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),

    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),

    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_candidate, "[[:punct:]]"), FALSE),

    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct,
      FALSE
    )
  )

### 2.4.3 Make distinct (sometimes CVR numbers are repeated within a row)
multi_winner_data_long <- multi_winner_data_long %>% 
  distinct(row_id, tender_id, winner_cvr_clean, .keep_all = TRUE)

### 2.4.4 Add metadata
multi_winner_data_long <- multi_winner_data_long %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_clean),
         winner_number = row_number(),
         source = "multiple confirmed winners",
         .by = c(row_id, tender_id))


## 2.5 Clean single CVR data
# Rename and copy
single_winner_data <- single_winner_data %>% 
  rename(winner_cvr_candidate = winner_cvr)

### 2.5.1 Extract and clean CVR
single_winner_data$winner_cvr_clean <- map_chr(
  single_winner_data$winner_cvr_candidate,
  ~unique(extract_valid_cvr_candidates(.x))
  )

### 2.5.2 Adding the CVR standardisation flags
single_winner_data <- single_winner_data %>%
  mutate(
         winner_cvr_clean = ifelse(
           flag_cvr_recovered_from_formatting,
           winner_cvr_recovered_from_formatting,
           winner_cvr_clean
         ),
         
         flag_cvr_placeholder = coalesce(
           winner_cvr_clean %in% known_invalid_cvr_numbers(),
           FALSE
         ),
         winner_cvr_clean = ifelse(
           flag_cvr_placeholder,
           NA_character_,
           winner_cvr_clean
         ),
         
         # Remove white space
         flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),

         # Remove alphabetical letters
         flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),

         # Remove all punctuation
         flag_cvr_punct = coalesce(str_detect(winner_cvr_candidate, "[[:punct:]]"), FALSE),

         # Flag if any standardisation performed
         flag_cvr_standardised = coalesce(
           flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct, FALSE
         ))

# Create metadata
single_winner_data <- single_winner_data %>% 
  mutate(winner_number = 1,
         source = "single winner")

## 2.6 Bind winner data
## Goal: Create one OpenTender winner table with cleaned CVR candidates and
## keep the OpenTender-specific review flags created above.
clean_winner_data <- bind_rows(single_winner_data, multi_winner_data_long) %>%
  arrange(row_id, winner_number) 

## 2.7 Join original tender data
## This keeps the full OpenTender row attached to the cleaned winner rows, so
## replication checks can always go back to the source fields.
clean_winner_data <- left_join(clean_winner_data, original_tender_data,
                               by = c("row_id", "tender_id"))

# Rearrange columns 
clean_winner_data <- clean_winner_data %>%
  select(row_id, tender_id, winner_number, winner_name, 
         winner_cvr_clean, winner_cvr_candidate, winner_cvr_original,
         winner_country,
         source, everything())

# Create valid CVR flag
clean_winner_data <- clean_winner_data %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

## 2.8 Fill missing CVRs when present elsewhere in data
### 2.8.1 Create key of CVR to firm names present in the data
valid_invalid_cvr_winner_key <- clean_winner_data %>%
  distinct(winner_name, winner_cvr_clean, valid_cvr) %>%
  mutate(n_valid_cvr = sum(valid_cvr), 
         n_total_cvr = n(), # Counts missings
         .by = winner_name) 

### 2.8.2 Identify a reproducible source row for each valid firm-name/CVR pairing
valid_cvr_sources <- clean_winner_data %>%
  filter(valid_cvr, !is.na(winner_name), winner_name != "") %>%
  summarise(
    row_id_borrowed_from = paste(sort(unique(row_id)), collapse = ";"),
    .by = c(winner_name, winner_cvr_clean)
  )

### 2.8.3 Create subset of firms with 1 valid CVR, but more than 1 CVR entry (including missings)
single_valid_cvr_key <- valid_invalid_cvr_winner_key %>% 
  filter(n_valid_cvr == 1, n_total_cvr > 1, valid_cvr) %>% 
  rename(winner_cvr_valid_from_same_name = winner_cvr_clean) %>%
  select(-valid_cvr, -n_valid_cvr, -n_total_cvr) %>% 
  distinct()

# Join sources
single_valid_cvr_key <- single_valid_cvr_key %>%
  left_join(valid_cvr_sources, 
            by = c("winner_name", "winner_cvr_valid_from_same_name" = "winner_cvr_clean"))

# Join key
clean_winner_data <- left_join(clean_winner_data, single_valid_cvr_key, 
                               by = "winner_name",
                               na_matches = "never")

### 2.8.4 Overwrite missing CVR when valid alternative available 
clean_winner_data <- clean_winner_data %>% 
  mutate(flag_fill_missing_cvr = coalesce((winner_cvr_original == "" | is.na(winner_cvr_original)) &
                                            !is.na(winner_cvr_valid_from_same_name) &
                                            winner_cvr_valid_from_same_name != "", 
                                          FALSE))
clean_winner_data <- clean_winner_data %>% 
  mutate(winner_cvr_clean = ifelse(flag_fill_missing_cvr, winner_cvr_valid_from_same_name, winner_cvr_clean))

# Update valid CVR flag
clean_winner_data <- clean_winner_data %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

### 2.8.5 Standardise winner_name (prepare for fuzzy match)

winner_name_prepared <- prepare_cvr_name(clean_winner_data$winner_name)

clean_winner_data <- clean_winner_data %>%
  mutate(
    winner_name_basic = winner_name_prepared$name_basic,
    winner_name_match = winner_name_prepared$name_clean,
    winner_name_no_spaces = winner_name_prepared$name_no_spaces,
    winner_name_broad = winner_name_prepared$name_broad,
    winner_firm_type = winner_name_prepared$firm_type,
    winner_name_first_letter = winner_name_prepared$first_letter
  )

## 2.9 Check carried CVR standardisation flags
## The actual CVR standardisation happens inside each winner dataframe before
## binding. This section only makes the carried flags complete after bind_rows().
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_cvr_ws = coalesce(flag_cvr_ws, FALSE),
    flag_cvr_alphabet = coalesce(flag_cvr_alphabet, FALSE),
    flag_cvr_punct = coalesce(flag_cvr_punct, FALSE),
    flag_cvr_standardised = coalesce(
      flag_cvr_ws |
        flag_cvr_alphabet |
        flag_cvr_punct,
      FALSE
    )
  )

## 2.10 Other winner quality flags
## Quality flags treat NAs as FALSE: missing values are captured by explicit
## missingness flags, not by propagating NA through boolean indicators.
# Flag valid CVR numbers (exactly 8 digits, no letters or special characters)
# missing/invalid = FALSE, valid = TRUE
clean_winner_data <- clean_winner_data %>%
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

# Flag missing CVR number
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_cvr =
           coalesce(
             is.na(winner_cvr_clean) | winner_cvr_clean == "",
             FALSE
           )
  )

# Flag missing winner name
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_name =
           coalesce(
             is.na(winner_name) | winner_name == "",
             FALSE
           )
  )

# Foreign winner
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_foreign_winner = coalesce(
      !is.na(winner_country) & winner_country != "" & winner_country != "DK",
      FALSE
    )
  )

# Missing country
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_country = coalesce(is.na(winner_country) | winner_country == "", FALSE))

# Single bidder
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_single_bidder = coalesce(parse_number(n_bids_received) == 1, FALSE)
  )

# Multi-lot tender
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_multilot = coalesce(parse_number(n_lots) > 1, FALSE)
  )

# Cancelled procurement
clean_winner_data <- clean_winner_data %>%
  mutate(flag_cancelled = coalesce(tender_cancelled, FALSE))

# Observation review
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_missing_cvr_with_name = coalesce(
      flag_missing_winner_cvr & !flag_missing_winner_name,
      FALSE
    ),
    flag_review_cvr = coalesce(!flag_missing_winner_cvr & !valid_cvr, FALSE),
    flag_no_winner_info = coalesce(
      flag_missing_winner_cvr &
        flag_missing_winner_name &
        flag_missing_winner_country,
      FALSE
    ),
    flag_verify_cvr_external = coalesce(
      case_when(
        flag_missing_cvr_with_name ~ TRUE,
        flag_review_cvr ~ TRUE,
        flag_no_winner_info ~ FALSE, # Cannot verify without information.
        valid_cvr ~ FALSE,
        TRUE ~ FALSE
      ),
      FALSE
    )
  )

# Flag if observation will need CVR fuzzy match 
clean_winner_data <- clean_winner_data %>% 
  mutate(flag_check_fuzzy_match = coalesce(winner_name != "" & is.na(winner_cvr_clean), FALSE))

## 2.11 Reorder columns
## Keep the cleaned CVR, original source CVR, winner name, winner country, and
## quality flags near each other so we can inspect the cleaning decisions.
clean_winner_data <- clean_winner_data %>%
  select(
    any_of(c(
      "row_id", "dataset", "tender_id", "lot_id", "lot_number", "winner_number", "source",
      "winner_cvr_clean", "winner_cvr_candidate", "winner_cvr_original",
      "winner_cvr_recovered_from_formatting",
      "winner_cvr_valid_from_same_name", "row_id_borrowed_from",
      "flag_fill_missing_cvr",
      "winner_name", "winner_name_original", "winner_name_basic",
      "winner_name_match", "winner_name_no_spaces", "winner_name_broad",
      "winner_firm_type", "winner_name_first_letter",
      "winner_country", "winner_country_original",
      "valid_cvr", "n_valid_cvr_raw", "n_valid_cvr", "flag_row_multiple_valid_cvr",
      "flag_check_fuzzy_match",
      "flag_cvr_recovered_from_formatting", "flag_cvr_placeholder",
      "flag_cvr_standardised", "flag_cvr_ws",
      "flag_cvr_alphabet", "flag_cvr_punct",
      "flag_missing_winner_cvr", "flag_missing_winner_name",
      "flag_missing_winner_country", "flag_foreign_winner",
      "flag_review_cvr", "flag_missing_cvr_with_name",
      "flag_no_winner_info", "flag_verify_cvr_external",
      "n_bids_received", "flag_single_bidder",
      "n_lots", "flag_multilot", "tender_cancelled", "flag_cancelled",
      "buyer_name", "buyer_cvr_original"
    )),
    everything()
  )

# 3 Clean up buyer data
## Buyers do not have CVR numbers, but they have names.
buyer_data <- buyer_data_original

# Guard against whitespace acting as an unhandled delimiter between two CVRs.
# The helper removes whitespace to repair spaced CVRs, so these rows would become
# one 16-digit string and incorrectly bypass the multiple-CVR cleaning workflow.
whitespace_separated_cvr_rows <- buyer_data %>%
  filter(str_detect(
    buyer_cvr,
    "(?<![0-9])[0-9]{8}[[:space:]]+[0-9]{8}(?![0-9])"
  )) %>%
  select(row_id, tender_id, buyer_cvr)

if (nrow(whitespace_separated_cvr_rows) > 0) {
  print(whitespace_separated_cvr_rows)
  stop("Some buyer rows contain two CVRs separated only by whitespace.")
}

## 3.1 Count distinct CVR numbers by row
buyer_data <- buyer_data %>% 
  mutate(
    # First pass: count CVRs that already appear as distinct eight-digit runs.
    n_valid_cvr_raw = compute_distinct_valid_cvr(buyer_cvr),
    
    # Second pass: only for Danish rows where the first pass found nothing,
    # recover one CVR if punctuation or prefixes were the only problem.
    buyer_cvr_recovered_from_formatting = recover_formatted_danish_cvr(
      cvr_candidate = buyer_cvr,
      country = buyer_country,
      n_valid_cvr_raw = n_valid_cvr_raw
    ),
    flag_cvr_recovered_from_formatting = coalesce(
      !is.na(buyer_cvr_recovered_from_formatting),
      FALSE
    ),
    
    # Third pass: count again using the recovered CVR where one was found. This
    # is the count used to decide whether the row should be split.
    buyer_cvr_for_count = ifelse(
      flag_cvr_recovered_from_formatting,
      buyer_cvr_recovered_from_formatting,
      buyer_cvr
    ),
    n_valid_cvr = compute_distinct_valid_cvr(buyer_cvr_for_count),
    flag_row_multiple_valid_cvr = (n_valid_cvr > 1)
  ) %>%
  select(
    -buyer_cvr_for_count
  )

n_formatted_buyer_cvrs_recovered <- sum(
  buyer_data$flag_cvr_recovered_from_formatting
)
cat("Number of formatted buyer CVRs recovered:",
    n_formatted_buyer_cvrs_recovered, "\n")


## 3.2 Standardise CVR number delimiters
buyer_data <- buyer_data %>%
  mutate(
    buyer_cvr = ifelse(
      flag_row_multiple_valid_cvr,
      str_replace_all(
        buyer_cvr,
        regex("\\s*(,|;|\\||/|&|\\bog\\b|\\bsamt\\b|\\band\\b|(?<=[\\d\\)])og(?=[[:alnum:]])|(?<=\\d)samt(?=\\d)|(?<=\\d)and(?=\\d))\\s*", ignore_case = TRUE),
        ";"
      ),
      buyer_cvr
    ),
    buyer_cvr = str_replace_all(buyer_cvr, ";+", ";")
  )

# Print number of true multi CVR numbers.
n_true_multi_cvrs <- sum(buyer_data$flag_row_multiple_valid_cvr, na.rm = TRUE)
cat("Number of true multi CVR numbers detected:", n_true_multi_cvrs, "\n")

## 3.3 Separate into single and multiple CVRs
# Multiple distinct CVR numbers
multi_buyer_data <- buyer_data %>% 
  filter(flag_row_multiple_valid_cvr)

# Only one valid CVR
single_buyer_data <- buyer_data %>% 
  filter(!flag_row_multiple_valid_cvr)

# Check these datasets cover complete data dataset
if (nrow(multi_buyer_data) + 
    nrow(single_buyer_data) - 
    nrow(buyer_data) != 0) {
  stop("subsetted datasets do not have the same number of rows as the full winner dataset")
}


## 3.4 Rows with multiple valid buyer CVRs
### 3.4.1 Split by CVR number
# These are separated by a standardised delimiter ';', so easy to separate out.
multi_buyer_data_long <- multi_buyer_data %>% 
  separate_longer_delim(cols = buyer_cvr, delim = ";")

# Rename and create copy
multi_buyer_data_long <- multi_buyer_data_long %>% 
  rename(buyer_cvr_candidate = buyer_cvr)

multi_buyer_data_long$buyer_cvr_clean <- map_chr(multi_buyer_data_long$buyer_cvr_candidate,
                                                   extract_valid_cvr_candidates)

# Flag the cleaning steps
multi_buyer_data_long <- multi_buyer_data_long %>% 
  mutate(
    flag_cvr_placeholder = coalesce(
      buyer_cvr_clean %in% known_invalid_cvr_numbers(),
      FALSE
    ),
    buyer_cvr_clean = ifelse(
      flag_cvr_placeholder,
      NA_character_,
      buyer_cvr_clean
    ),
    
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(buyer_cvr_candidate, "\\s"), FALSE),
    
    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(buyer_cvr_candidate, "[[:alpha:]]"), FALSE),
    
    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(buyer_cvr_candidate, "[[:punct:]]"), FALSE),
    
    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct,
      FALSE
    )
  )

### 3.4.3 Make distinct (sometimes CVR numbers are repeated within a row)
multi_buyer_data_long <- multi_buyer_data_long %>% 
  distinct(row_id, tender_id, buyer_cvr_clean, .keep_all = TRUE)

### 3.4.4 Add metadata
multi_buyer_data_long <- multi_buyer_data_long %>%
  mutate(buyer_cvr_clean = as.character(buyer_cvr_clean),
         buyer_number = row_number(),
         source = "multiple CVRs",
         .by = c(row_id, tender_id))


## 3.5 Clean single CVR data
# Rename and copy
single_buyer_data <- single_buyer_data %>% 
  rename(buyer_cvr_candidate = buyer_cvr)

### 3.5.1 Extract and clean CVR
single_buyer_data$buyer_cvr_clean <- map_chr(
  single_buyer_data$buyer_cvr_candidate,
  ~unique(extract_valid_cvr_candidates(.x))
)

### 3.5.2 Adding the CVR standardisation flags
single_buyer_data <- single_buyer_data %>%
  mutate(
    buyer_cvr_clean = ifelse(
      flag_cvr_recovered_from_formatting,
      buyer_cvr_recovered_from_formatting,
      buyer_cvr_clean
    ),
    
    flag_cvr_placeholder = coalesce(
      buyer_cvr_clean %in% known_invalid_cvr_numbers(),
      FALSE
    ),
    buyer_cvr_clean = ifelse(
      flag_cvr_placeholder,
      NA_character_,
      buyer_cvr_clean
    ),
    
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(buyer_cvr_candidate, "\\s"), FALSE),
    
    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(buyer_cvr_candidate, "[[:alpha:]]"), FALSE),
    
    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(buyer_cvr_candidate, "[[:punct:]]"), FALSE),
    
    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct, FALSE
    ))

# Create metadata
single_buyer_data <- single_buyer_data %>% 
  mutate(buyer_number = 1,
         source = "single buyer")

## 3.6 Bind buyer data
## Goal: Create one OpenTender buyer table with cleaned CVR candidates and
## keep the OpenTender-specific review flags created above.
clean_buyer_data <- bind_rows(single_buyer_data, multi_buyer_data_long) %>%
  arrange(row_id, buyer_number) 

## 3.7 Join original tender data
## This keeps the full OpenTender row attached to the cleaned buyer rows, so
## replication checks can always go back to the source fields.
clean_buyer_data <- left_join(clean_buyer_data, original_tender_data,
                               by = c("row_id", "tender_id"))

# Rearrange columns 
clean_buyer_data <- clean_buyer_data %>%
  select(row_id, tender_id, buyer_number, buyer_name, 
         buyer_cvr_clean, buyer_cvr_candidate, buyer_cvr_original,
         buyer_country,
         source, everything())

## 3.8 Remove non-CVR tokens from multi-CVR buyer rows
## A multi-CVR source row can also contain foreign or alternative identifiers.
## If that source row already supplied valid Danish CVRs, do not turn its
## remaining invalid tokens into additional missing buyers for name matching.
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    buyer_cvr_is_valid = coalesce(
      str_detect(buyer_cvr_clean, "^[0-9]{8}$"),
      FALSE
    ),
    row_has_valid_buyer_cvr = any(buyer_cvr_is_valid),
    .by = c(row_id, tender_id)
  )

invalid_multi_buyer_cvr_tokens <- clean_buyer_data %>%
  filter(
    source == "multiple CVRs",
    row_has_valid_buyer_cvr,
    !buyer_cvr_is_valid
  )

cat("Number of non-CVR tokens removed from multi-CVR rows:",
    nrow(invalid_multi_buyer_cvr_tokens), "\n")

clean_buyer_data <- clean_buyer_data %>%
  filter(
    !(
      source == "multiple CVRs" &
        row_has_valid_buyer_cvr &
        !buyer_cvr_is_valid
    )
  ) %>%
  mutate(
    buyer_number = row_number(),
    .by = c(row_id, tender_id)
  ) %>%
  mutate(
    flag_non_cvr_identifier = coalesce(
      !is.na(buyer_cvr_candidate) &
        str_trim(buyer_cvr_candidate) != "" &
        is.na(buyer_cvr_clean),
      FALSE
    )
  ) %>%
  select(
    -buyer_cvr_is_valid,
    -row_has_valid_buyer_cvr
  )

# Create valid CVR flag
clean_buyer_data <- clean_buyer_data %>% 
  mutate(valid_cvr = coalesce(str_detect(buyer_cvr_clean, "^\\d{8}$"), FALSE))

## 3.9 Fill missing CVRs when present elsewhere in data
### 3.9.1 Create key of CVR to firm names present in the data
valid_invalid_cvr_buyer_key <- clean_buyer_data %>%
  distinct(buyer_name, buyer_cvr_clean, valid_cvr) %>%
  mutate(n_valid_cvr = sum(valid_cvr), 
         n_total_cvr = n(), # Counts missings
         .by = buyer_name) 

### 3.9.2 Identify a reproducible source row for each valid firm-name/CVR pairing
valid_cvr_sources <- clean_buyer_data %>%
  filter(valid_cvr, !is.na(buyer_name), buyer_name != "") %>%
  summarise(
    # One source row is enough to trace where the borrowed CVR came from.
    row_id_borrowed_from = min(row_id),
    .by = c(buyer_name, buyer_cvr_clean)
  )

### 3.9.3 Create subset of firms with 1 valid CVR, but more than 1 CVR entry (including missings)
single_valid_cvr_key <- valid_invalid_cvr_buyer_key %>% 
  filter(n_valid_cvr == 1, n_total_cvr > 1, valid_cvr) %>% 
  rename(buyer_cvr_valid_from_same_name = buyer_cvr_clean) %>%
  select(-valid_cvr, -n_valid_cvr, -n_total_cvr) %>% 
  distinct()

# Join sources
single_valid_cvr_key <- single_valid_cvr_key %>%
  left_join(valid_cvr_sources, by = c("buyer_name", "buyer_cvr_valid_from_same_name" = "buyer_cvr_clean"))

# Join key
clean_buyer_data <- left_join(clean_buyer_data, single_valid_cvr_key, 
                               by = "buyer_name",
                               na_matches = "never")

### 3.9.4 Overwrite missing CVR when valid alternative available 
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_fill_missing_cvr = coalesce((buyer_cvr_original == "" | is.na(buyer_cvr_original)) &
                                            !is.na(buyer_cvr_valid_from_same_name) &
                                            buyer_cvr_valid_from_same_name != "", 
                                          FALSE))
clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    buyer_cvr_clean = ifelse(
      flag_fill_missing_cvr,
      buyer_cvr_valid_from_same_name,
      buyer_cvr_clean
    ),
    # Keep source provenance only on rows whose CVR was actually filled.
    row_id_borrowed_from = ifelse(
      flag_fill_missing_cvr,
      row_id_borrowed_from,
      NA_integer_
    )
  )

# Update valid CVR flag
clean_buyer_data <- clean_buyer_data %>% 
  mutate(valid_cvr = coalesce(str_detect(buyer_cvr_clean, "^\\d{8}$"), FALSE))

### 3.9.5 Standardise buyer_name (prepare for fuzzy match)
buyer_name_prepared <- prepare_cvr_name(clean_buyer_data$buyer_name)

clean_buyer_data <- clean_buyer_data %>%
  mutate(
    buyer_name_basic = buyer_name_prepared$name_basic,
    buyer_name_match = buyer_name_prepared$name_clean,
    buyer_name_no_spaces = buyer_name_prepared$name_no_spaces,
    buyer_name_broad = buyer_name_prepared$name_broad,
    buyer_firm_type = buyer_name_prepared$firm_type,
    buyer_name_first_letter = buyer_name_prepared$first_letter
  )

## 3.10 Check carried CVR standardisation flags
## The actual CVR standardisation happens inside each buyer dataframe before
## binding. This section only makes the carried flags complete after bind_rows().
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_cvr_ws = coalesce(flag_cvr_ws, FALSE),
    flag_cvr_alphabet = coalesce(flag_cvr_alphabet, FALSE),
    flag_cvr_punct = coalesce(flag_cvr_punct, FALSE),
    flag_cvr_standardised = coalesce(
      flag_cvr_ws |
        flag_cvr_alphabet |
        flag_cvr_punct,
      FALSE
    )
  )

## 3.11 Other buyer quality flags
## Quality flags treat NAs as FALSE: missing values are captured by explicit
## missingness flags, not by propagating NA through boolean indicators.
# Flag valid CVR numbers (exactly 8 digits, no letters or special characters)
# missing/invalid = FALSE, valid = TRUE
clean_buyer_data <- clean_buyer_data %>%
  mutate(valid_cvr = coalesce(str_detect(buyer_cvr_clean, "^\\d{8}$"), FALSE))

# Flag missing CVR number
clean_buyer_data <- clean_buyer_data %>%
  mutate(flag_missing_buyer_cvr =
           coalesce(
             is.na(buyer_cvr_clean) | buyer_cvr_clean == "",
             FALSE
           )
  )

# Flag missing buyer name
clean_buyer_data <- clean_buyer_data %>%
  mutate(flag_missing_buyer_name =
           coalesce(
             is.na(buyer_name) | buyer_name == "",
             FALSE
           )
  )

# Foreign buyer
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_foreign_buyer = coalesce(
      !is.na(buyer_country) & buyer_country != "" & buyer_country != "DK",
      FALSE
    )
  )

# Missing country
clean_buyer_data <- clean_buyer_data %>%
  mutate(flag_missing_buyer_country = coalesce(is.na(buyer_country) | buyer_country == "", FALSE))

# Multi-lot tender
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_multilot = coalesce(parse_number(n_lots) > 1, FALSE)
  )

# Cancelled procurement
clean_buyer_data <- clean_buyer_data %>%
  mutate(flag_cancelled = coalesce(tender_cancelled, FALSE))

# Observation review
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_missing_cvr_with_name = coalesce(
      flag_missing_buyer_cvr & !flag_missing_buyer_name,
      FALSE
    ),
    flag_review_cvr = coalesce(!flag_missing_buyer_cvr & !valid_cvr, FALSE),
    flag_no_buyer_info = coalesce(
      flag_missing_buyer_cvr &
        flag_missing_buyer_name &
        flag_missing_buyer_country,
      FALSE
    ),
    flag_verify_cvr_external = coalesce(
      case_when(
        flag_missing_cvr_with_name ~ TRUE,
        flag_review_cvr ~ TRUE,
        flag_no_buyer_info ~ FALSE, # Cannot verify without information.
        valid_cvr ~ FALSE,
        TRUE ~ FALSE
      ),
      FALSE
    )
  )

# Flag if observation will need CVR fuzzy match 
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_check_fuzzy_match = coalesce(buyer_name != "" & is.na(buyer_cvr_clean), FALSE))


# 4 Save 
saveRDS(clean_winner_data, file.path(dirs$clean_data, "clean_winner_data_ot.rds"))
haven::write_dta(clean_winner_data, file.path(dirs$clean_data, "clean_winner_data_ot.dta"))
