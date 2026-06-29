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
    buyer_cvr_original = buyer_bodyIds
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

## 2.1 Count distinct CVR numbers by row
winner_data <- winner_data %>% 
  mutate(
    n_valid_cvr = compute_distinct_valid_cvr(winner_cvr),
    flag_row_multiple_valid_cvr = (n_valid_cvr > 1)
  )

## 2.2 Standardise CVR number delimiters
winner_data <- winner_data %>%
  mutate(
    winner_cvr = ifelse(
      flag_row_multiple_valid_cvr,
      str_replace_all(
        winner_cvr,
        regex("\\s*(,|;|\\||/|&|\\bog\\b)\\s*", ignore_case = TRUE),
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
# Multiple distinct CVRs, identified multiple firm names
multi_winner_names_data <- winner_data %>% 
  filter(flag_multiple_distinct_valid_cvrs, flag_multiple_distinct_winner_names)

# Multiple CVRs (non distinct or distinct), single firm name
multi_cvr_nondistinct_names_data <- winner_data %>% 
  filter((flag_multiple_distinct_valid_cvrs & !flag_multiple_distinct_winner_names) | 
          (flag_multi_winner & !flag_multiple_distinct_valid_cvrs & !flag_multiple_distinct_winner_names))

# Either no CVR delimiter detected, or nondistinct CVR numbers if delimited detected
single_winner_data <- winner_data %>% 
  filter(!flag_multi_winner)

# Check these datasets cover complete data dataset
if (nrow(multi_winner_names_data) + 
    nrow(multi_cvr_nondistinct_names_data) + 
    nrow(single_winner_data) - 
    nrow(winner_data) != 0) {
  stop("subsetted datasets do not have the same number of rows as the full winner dataset")
}

## 2.4 Multi-winner data with confirmed multiple firms
### 2.4.1 Clean up name delimiters for easier splitting
# Since there are few of these cases, I just do them manually for transparency
# Where original firm names include 'consortium', I have put the firm name corresponding to the CVR obtained from https://virk.dk

multi_winner_names_data <- multi_winner_names_data %>% 
  mutate(winner_name = case_when(
    winner_name == "Konsortiet Dansk Flygtningehjælp og Als Research ApS" ~ "Dansk Flygtningehjælp;Als Research ApS",
    winner_name ==  "MT Højgaard A/S og Moe A/S" ~ "MT Højgaard A/S;Moe A/S",
    winner_name == "C. C. Brun Entreprise A/S og Rørbæk & Møller ApS" ~ "C. C. Brun Entreprise A/S;Rørbæk & Møller ApS",
    winner_name == "konsortiet IESenergy A/S / Victor DST A/S" ~ "IESenergy A/S;Victor DST A/S",
    winner_name == "IESEnergy A/S/VictorDST A/S" ~ "IESEnergy A/S;VictorDST A/S",
    winner_name == "LM Byg Konsortiet v/LM Byg A/S og M.J. Eriksson A/S" ~ "LM Byg A/S;M.J. Eriksson A/S",
    winner_name == "Crone & Co | Impact Group" ~ "Crone & Co;Impact Group",
    winner_name == "Nøhr & Sigsgaard A/S og Jakon A/S" ~ "Nøhr & Sigsgaard A/S;Jakon A/S",
    winner_name == "Arkitema K/S / Tegnestuen Vandkunsten A/S / JJW Arkitekter A/S" ~ "Arkitema K/S;Tegnestuen Vandkunsten A/S;JJW Arkitekter A/S",
    winner_name == "LINK Arkitektur A/S & 5E Byg A/S" ~ "LINK Arkitektur A/S;5E Byg A/S",
    winner_name == "PÅLSSON ARKITEKTER A/S / ERIK Arkitekter A/S / AI A/S" ~ "PÅLSSON ARKITEKTER A/S;ERIK Arkitekter A/S;AI A/S",
    winner_name == "Konsortium bestående af DitoBus Excursions A/S, Jørns Busrejser A/S og Nilles Busser A/S" ~ "DitoBus Excursions A/S;Jørns Busrejser A/S;Nilles Busser A/S",
    winner_name == "New Stories - MacMann Berg P/S" ~ "New Stories;MacMann Berg P/S",
    winner_name == "Konsortiet KomPublic ApS og syv.ai ApS" ~ "KomPublic ApS;syv.ai ApS",
    winner_name == "Team bestående af Einar Kornerup A/S og Wissenberg A/S" ~ "Einar Kornerup A/S;Wissenberg A/S"
  )) 

# Deliberately default all others to NA so we get an easy check of completeness
if (any(is.na(multi_winner_names_data$winner_name))) {
  stop("you haven't cleaned all the multiple distinct cvr winner names. check `multi_winner_names_data` and try again.")
}

### 2.4.2 Pivot to longer
# Method below assumes ordering of CVRs matches ordering of winner name
multi_winner_names_data_long <- multi_winner_names_data %>% 
  separate_longer_delim(cols = c("winner_cvr", "winner_name"), delim = ";")

# Rename and create copy
multi_winner_names_data_long <- multi_winner_names_data_long %>% 
  rename(winner_cvr_candidate = winner_cvr) %>% 
  mutate(winner_cvr_clean = winner_cvr_candidate)

# Deal with tricky edge case separately
multi_winner_names_data_long <- multi_winner_names_data_long %>%
  mutate(winner_cvr_clean = ifelse(
    winner_cvr_candidate == "CVR5EByg:30811097",
    "30811097",
    winner_cvr_candidate
  ))

## Clean up/standardise CVR numbers
## Keep the separated raw CVR candidate before cleaning. Cleaning flags treat
## NAs as FALSE: a missing source value is not counted as evidence that a
## cleaning operation was performed.
multi_winner_names_data_long <- multi_winner_names_data_long %>% 
  mutate(
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),

    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),

    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
    winner_cvr_clean = as.character(parse_number(winner_cvr_clean)),

    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct,
      FALSE
    )
  )

## Add metadata
multi_winner_names_data_long <- multi_winner_names_data_long %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_clean),
         winner_number = row_number(),
         source = "multiple confirmed winner names",
         .by = c(row_id, tender_id))

## 2.5 Multiple distinct CVRs
### 2.5.1 Pivot to long
# This section focuses on multiple distinct CVRs with only one identifiable firm name
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data %>% 
  separate_longer_delim(cols = "winner_cvr", delim = ";")

# Rename and copy variable for cleaning
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  rename(winner_cvr_candidate = winner_cvr) %>% 
  mutate(winner_cvr_clean = winner_cvr_candidate)

## Clean up/standardise CVR numbers
## Keep the separated raw CVR candidate before cleaning. Cleaning flags treat
## NAs as FALSE: a missing source value is not counted as evidence that a
## cleaning operation was performed.
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  mutate(

    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),

    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),

    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_candidate, "[[:punct:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
    winner_cvr_clean = as.character(parse_number(winner_cvr_clean)),

    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct,
      FALSE
    )
  )

# Flag valid CVR string post cleaning/standardisation (8 numerical digits)
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

# Make distinct by (row_id, tender_id, winner_cvr_clean, winner_name)
## Since a lot of these rows come from records with multiple instances of the same CVR number
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  distinct(tender_id, row_id, winner_name, winner_cvr_clean, valid_cvr, 
           .keep_all = TRUE)

# Determine whether the original OpenTender row has one or more distinct valid CVR.
# If it does, drop the invalid expanded rows for that source row.
# If it does not, keep them.
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  mutate(n_valid_cvr_in_row = sum(valid_cvr, na.rm = TRUE), 
         n_total_expanded_rows = n(),
         .by = c(row_id, tender_id, winner_name))

multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(flag_row_has_valid_cvr = coalesce(n_valid_cvr_in_row >= 1, FALSE))

# I keep expanded rows if:
#   - there's at least one valid CVR in the source row and the expanded row is a valid CVR
#   - there's no valid CVR's in the source row.
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  filter((flag_row_has_valid_cvr & valid_cvr) | (!flag_row_has_valid_cvr))

# Other firms have many valid CVRs and many invalid CVRs. 
# Flag them, but I keep all their rows.
multi_valid_cvr_firms <- multi_cvr_nondistinct_names_data_long %>% 
  filter(n_valid_cvr_in_row > 1) %>% 
  distinct(winner_name, n_valid_cvr_in_row, n_total_expanded_rows) 

cat("Number of winning firms with several valid CVRs:", nrow(multi_valid_cvr_firms), "\n")
cat("Ave. number of valid CVRs for these firms:", mean(multi_valid_cvr_firms$n_valid_cvr_in_row), "\n")
cat("Ave. number of expanded rows (valid + invalid CVRs) for these firms:", mean(multi_valid_cvr_firms$n_total_expanded_rows), "\n")

multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(flag_winner_has_multi_valid_cvr = coalesce(if_else(winner_name %in% multi_valid_cvr_firms$winner_name,
                                                            TRUE, FALSE), FALSE))

## Add metadata
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_clean),
         source = "multiple CVR candidates for one winner name",
         winner_number = row_number(),
         .by = c(row_id, tender_id))

## 2.6 Clean single CVR data
# Rename and copy
single_winner_data <- single_winner_data %>% 
  rename(winner_cvr_candidate = winner_cvr) %>% 
  mutate(winner_cvr_clean = winner_cvr_candidate)

## Adding the CVR standardisation flags
single_winner_data <- single_winner_data %>%
  mutate(# Remove white space
         flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),
         
         # Remove alphabetical letters
         flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),
         
         # Remove all punctuation
         flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
         winner_cvr_clean = as.character(parse_number(winner_cvr_clean)),
         
         # Flag if any standardisation performed
         flag_cvr_standardised = coalesce(
           flag_cvr_ws | flag_cvr_alphabet | flag_cvr_punct, FALSE
         ))

# Create metadata
single_winner_data <- single_winner_data %>% 
  mutate(winner_number = 1,
         source = "single winner")

## 2.7 Bind winner data
## Goal: Create one OpenTender winner table with cleaned CVR candidates and
## keep the OpenTender-specific review flags created above.

clean_winner_data <- bind_rows(
  single_winner_data %>% select(-any_of("winner_cvr")),
  multi_winner_names_data_long %>% select(-any_of("winner_cvr")),
  multi_cvr_nondistinct_names_data_long %>% select(-any_of("winner_cvr"))
) %>%
  arrange(row_id, winner_number) 

## 2.8 Join original tender data
## This keeps the full OpenTender row attached to the cleaned winner rows, so
## replication checks can always go back to the source fields.
clean_winner_data <- left_join(clean_winner_data, original_tender_data,
                               by = c("row_id", "tender_id"))

clean_winner_data <- clean_winner_data %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_clean))

# Rearrange columns 
clean_winner_data <- clean_winner_data %>%
  select(row_id, tender_id, winner_number, winner_name, 
         winner_cvr_clean, winner_cvr_candidate, winner_cvr_original,
         winner_country,
         source, everything())


## The row-level CVR evidence, borrowed-CVR, and multi-valid-CVR flags
## only exist in the multiple CVR subdatasets.
## Treat these flags values as FALSE in the final cleaned table for completeness
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_row_has_single_valid_cvr = coalesce(flag_row_has_single_valid_cvr, FALSE),
    flag_winner_has_multi_valid_cvr = coalesce(flag_winner_has_multi_valid_cvr, FALSE)
  )

## 2.9 Check carried CVR standardisation flags
## The actual CVR standardisation happens inside each winner dataframe before
## binding. This section only makes the carried flags complete after bind_rows().
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_cvr_ws = coalesce(flag_cvr_ws, FALSE),
    flag_cvr_hyphen = coalesce(flag_cvr_hyphen, FALSE),
    flag_cvr_alphabet = coalesce(flag_cvr_alphabet, FALSE),
    flag_cvr_punct = coalesce(flag_cvr_punct, FALSE),
    flag_cvr_standardised = coalesce(
      flag_cvr_ws |
        flag_cvr_hyphen |
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

# Flag transformed winner CVR number (not equal to original winner CVR number)
clean_winner_data <- clean_winner_data %>%
  mutate(flag_winner_cvr_changed =
           coalesce(
             winner_cvr_clean != winner_cvr_candidate_original,
             FALSE
           )
  )

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

# Count extracted winners from each original OpenTender row
clean_winner_data <- clean_winner_data %>%
  mutate(n_winners_extracted = n(), .by = c(row_id, tender_id))

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

## 2.12 Reorder columns
## Keep the cleaned CVR, original source CVR, winner name, winner country, and
## quality flags near each other so we can inspect the cleaning decisions.
clean_winner_data <- clean_winner_data %>%
  select(
    any_of(c(
      "row_id", "dataset", "tender_id", "lot_id", "lot_number", "winner_number", "source",
      "winner_cvr_clean", "winner_cvr_clean_real",
      "winner_cvr_candidate_original", "winner_cvr_original",
      "winner_name", "winner_name_original",
      "winner_country", "winner_country_original",
      "valid_cvr", "flag_winner_cvr_changed",
      "flag_cvr_standardised", "flag_cvr_ws", "flag_cvr_hyphen",
      "flag_cvr_alphabet", "flag_cvr_punct",
      "n_valid_cvr_in_row", "flag_row_has_single_valid_cvr",
      "flag_cvr_borrowed_from_winner_name",
      "flag_missing_winner_cvr", "flag_missing_winner_name",
      "flag_missing_winner_country", "flag_foreign_winner",
      "flag_review_cvr", "flag_missing_cvr_with_name",
      "flag_no_winner_info", "flag_verify_cvr_external",
      "flag_manual_review", "manual_review_reason",
      "flag_multi_winner", "flag_multiple_distinct_valid_cvrs",
      "flag_multiple_distinct_winner_names", "flag_winner_has_multi_valid_cvr",
      "n_winners_extracted", "n_bids_received", "flag_single_bidder",
      "n_lots", "flag_multilot", "tender_cancelled", "flag_cancelled",
      "buyer_name", "buyer_cvr_original"
    )),
    everything()
  )
