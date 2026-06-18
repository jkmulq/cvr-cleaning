# Cleans KFST provided tender data
# Author: Jack Mulqueeney
# Date: 16 June 2026

# Clean environment
rm(list = ls())

# Config: edit config.R at the project root to set your own PROJECT_DIR and Stata path
library(here)
source(here::here("config.R"))

# Packages
library(haven)
library(tidyverse)
library(readxl)

# Paths
data_dir           <- dirs$raw_data
raw_data_name      <- "udbudsdata_kfst.xlsx"

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# 1 Load data
data <- read_excel(file.path(data_dir, "kfst", raw_data_name), sheet = "2.0 Udbudsdata")

# Rename
data <- data %>% 
  rename(winner_cvr = `Vinders CVR`,
         winner_name = `Vinders navn`,
         winner_country = `Vinders land`,
         buyer_name = `Navn på ordregiver`,
         pub_date = `Publikationsdato for bekendtgørelse om indgået kontrakt`,
         award_date = `Dato for tildeling af kontrakten`,
         submit_date = `Frist for aflevering af tilbud`,
         tender_id = `Løbenummer`,
         lot_id = `Nummerplade`,
         divided_tender = `Opdelt udbud`,
         joint_tender = `Fælles-/enkeltudbud`,
         consortium_winner = `Konsortium/Sammenslutning`,
         cpv_code = `CPV-koder`,
         tender_cancelled = `Annulleret udbud`,
         tender_status = `Helt/delvist gennemført/annulleret`,
         lot_number = `Delkontraktnr.`,
         n_lots = `Antal delkontrakter kortlagt`,
         n_lots_contracted = `Antal delkontrakter i udbudsbekendtgørelsen`,
         n_lot_winners = `Antal vindere på delkontrakten`,
         n_bids_received = `Antal modtagne bud`)

# Order columns nicely
data <- data %>% 
  select(tender_id, lot_id, lot_number,
         n_lots, n_lots_contracted, n_lot_winners,
         pub_date, award_date,
         buyer_name,
         winner_name, winner_cvr, winner_country,
         everything())

# Arrange
data <- data %>% 
  group_by(tender_id) %>% 
  arrange(lot_number, .by_group = TRUE) %>% 
  ungroup()

# Check whether lot_id is unique
data <- data %>% 
  mutate(n_lot_id = n(), .by = lot_id)

dup_lots <- data %>%
  filter(n_lot_id > 1) %>%
  distinct(lot_id, n_lot_id, tender_cancelled, tender_status) %>%
  arrange(lot_id, tender_cancelled)

cancelled_duplicate_lots <- data %>%
  slice(0)

# Print results of duplication check
if (nrow(dup_lots) == 0) {
  cat("All lot_id values are unique.\n")
} else {
  cat("Duplicate lot_id values:\n")
  print(dup_lots)

  # Check whether duplicates have one cancelled and one not cancelled row per lot_id
  dup_lot_cancelled_pattern <- data %>%
    filter(n_lot_id > 1) %>%
    summarise(
      n_rows = n(),
      n_cancelled = sum(tender_cancelled == "Ja", na.rm = TRUE),
      n_not_cancelled = sum(tender_cancelled == "Nej", na.rm = TRUE),
      .by = lot_id
    )

  # If any duplicate lot_id values do not follow the expected pattern of one 
  # cancelled and one not cancelled row, print these and stop the script to review before cleaning.
  unexpected_dup_lots <- dup_lot_cancelled_pattern %>%
    filter(!(n_rows == 2 & n_cancelled == 1 & n_not_cancelled == 1))
  if (nrow(unexpected_dup_lots) > 0) {
    print(unexpected_dup_lots)
    stop("Unexpected duplicate lot_id pattern. Review unexpected_dup_lots before cleaning.")
  }
  
  # Save object of what rows are cancelled
  cancelled_duplicate_lots <- data %>%
    filter(n_lot_id > 1, tender_cancelled == "Ja")

  # Filter out cancelled duplicate rows, keeping the non-cancelled row for each duplicated lot_id
  data <- data %>%
    filter(!(n_lot_id > 1 & tender_cancelled == "Ja"))
}

# Check whether duplicates remain after dropping duplicate cancelled/non-cancelled pairs
data <- data %>%
  select(-n_lot_id) %>%
  mutate(n_lot_id = n(), .by = lot_id)

remaining_dup_lots <- data %>%
  filter(n_lot_id > 1) %>%
  distinct(lot_id, n_lot_id)

if (nrow(remaining_dup_lots) > 0) {
  print(remaining_dup_lots)
  stop("Duplicate lot_id values remain after filtering cancelled duplicate rows.")
}

# Tender/lot-level data to join onto cleaned entity tables at the end.
tender_lot_data <- data %>%
  select(any_of(c(
    "tender_id", "lot_id", "lot_number", "buyer_name",
    "n_lots", "n_lots_contracted", "n_lot_winners", "n_bids_received",
    "pub_date", "award_date", "submit_date",
    "divided_tender", "joint_tender", "consortium_winner",
    "cpv_code", "tender_cancelled", "tender_status",
    "n_lot_id"
  ))) %>%
  arrange(tender_id, lot_id, lot_number)


# 2 Winners
winner_data <- data %>% 
  select(tender_id, lot_id, winner_cvr, winner_name, winner_country, n_lot_winners)
original_winner_data <- winner_data # Store original for later joining

## 2.1 Separate winners
## Goal: Separate winners into single winners and multiple winners.

## Number of winners using number of winner names
## Winner names are separate by a comma or semicolon
winner_data <- winner_data %>% 
  mutate(n_winner_name = str_count(winner_name, ",|;") + 1,
         n_winner_cvr = str_count(winner_cvr, ",|;|[.]") + 1,
         n_winner_country = str_count(winner_country, ",|;"))

## Missing winner CVR column
winner_data <- winner_data %>% 
  mutate(missing_winner_cvr = is.na(winner_cvr))

## Find reliably single CVRs
# CVRs without any commas, semi-colons, periods, etc. 
# and with exactly 8 digits are likely to be single CVRs; flag these
winner_data <- winner_data %>% 
  mutate(single_cvr = ifelse(grepl("^\\d{8}$", winner_cvr) & 
                               !str_detect(winner_cvr, regex("[.,; ]")), 
                             TRUE, NA))

# Print result
cat("Share of easily identifiable single CVRs:", 
    sum(winner_data$single_cvr, na.rm = TRUE) / nrow(winner_data), "\n")

# CVRs with spaces but whose characters are all numbers and 
# with exactly 8 characters are likely to be single CVRs; flag these
winner_data <- winner_data %>% 
  mutate(single_cvr = ifelse(grepl("^\\d{8}$", gsub(" ", "", winner_cvr)) & # removes white space first before checking digits
                               str_detect(winner_cvr, regex(" ")) & 
                               !str_detect(winner_cvr, regex("[.,;]")), 
                             TRUE, single_cvr))

# Print result
cat("Share of identifiable single CVRs (including separated spaces):", 
    sum(winner_data$single_cvr, na.rm = TRUE) / nrow(winner_data), "\n")

# Keep object
single_winner_data <- winner_data %>% 
  filter(single_cvr) %>% 
  mutate(winner_number = 1, 
         source = "single winners")

multi_winner_data <- winner_data %>% 
  filter(is.na(single_cvr))

## 2.2 Deal with multiple winners
## Goal: Create long dataframe with one row per winner (identified by tender_id/lot_id)
# Map function over rows and bind.
cvr_cols_to_sep <- c("winner_cvr", "winner_name", "winner_country")
multi_winner_data_sep <- map(1:nrow(multi_winner_data), 
                             .f = extract_multiple_cvr,
                             data = multi_winner_data,
                             cvr_cols = cvr_cols_to_sep) %>% 
  bind_rows()

# Order columns
column_pattern <- paste0(cvr_cols_to_sep, collapse = "|")
column_pattern <- paste0("(", column_pattern, ")_(\\d+)$")
multi_winner_data_sep <- multi_winner_data_sep %>% 
  select(tender_id, lot_id, max_detected, matches(column_pattern))

# Pivot longer
multi_winner_long <- multi_winner_data_sep %>%
  
  # Convert each variable type separately into long form
  pivot_longer(
    cols = matches("^winner_cvr_\\d+|^winner_name_\\d+|^winner_country_\\d+"),
    names_to = c("variable", "winner_number"),
    names_pattern = "(winner_cvr|winner_name|winner_country)_(\\d+)",
    values_to = "value"
  ) %>%
  
  # Spread variable types back into columns
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  
  # Clean winner_number type
  mutate(winner_number = as.integer(winner_number),
         source = "multiple winners") %>%
  arrange(tender_id, lot_id, winner_number)

# Filter out 'fake' winners (bind_rows() takes global max, not within lot_id max)
multi_winner_long <- multi_winner_long %>% 
  filter(winner_number <= max_detected)

## 2.3 Bind single and multi winner together
clean_winner_data <- bind_rows(
  single_winner_data %>% 
    select(tender_id, lot_id, winner_number, winner_cvr, winner_name, winner_country, source),
  multi_winner_long %>%
    select(tender_id, lot_id, winner_number, winner_cvr, winner_name, winner_country, source)
) %>% 
  arrange(tender_id)

## 2.4 Join original tender data and original winner data
clean_winner_data <- left_join(clean_winner_data, tender_lot_data, 
                               by = c("tender_id", "lot_id"))
clean_winner_data <- left_join(clean_winner_data, original_winner_data, 
                               by = c("tender_id", "lot_id"),
                               suffix = c("", "_original"))

### Only keep lots that had winners (defined by original data)
clean_winner_data <- clean_winner_data %>% 
  filter(n_lot_winners_original > 0)

## 2.5 Clean up/standardise CVR numbers
## Treats NAs as FALSE. Contains flags everytime an operation executes
clean_winner_data <- clean_winner_data %>%
  mutate(
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr, "\\s"), FALSE),
    winner_cvr = str_remove_all(winner_cvr, "\\s+"),
    
    # Remove hyphens
    flag_cvr_hyphen = coalesce(str_detect(winner_cvr, "-"), FALSE),
    winner_cvr = str_remove_all(winner_cvr, "-"),
    
    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr, "[[:alpha:]]"), FALSE),
    winner_cvr = str_remove_all(winner_cvr, "[[:alpha:]]"),
    
    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr, "[[:punct:]]"), FALSE),
    winner_cvr = str_remove_all(winner_cvr, "[[:punct:]]+"),
    
    # Flag if any standardisation performed
    flag_cvr_standardised = flag_cvr_ws | 
      flag_cvr_hyphen | 
      flag_cvr_alphabet | 
      flag_cvr_punct
    )

## 2.6 Other winner quality flags
# Flag valid CVR numbers (exactly 8 digits, no letters or special characters)
# missing/invalid = FALSE, valid = TRUE
clean_winner_data <- clean_winner_data %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr, "^\\d{8}$"), FALSE))

# Flag transformed winner CVR number (not equal to original winner CVR number)
# Don't do this for multiple winners because the original winner CVR number 
# is not necessarily wrong in this case (it may just be the first of multiple 
# CVRs listed in the original data, which we have now separated into multiple rows).
clean_winner_data <- clean_winner_data %>% 
  mutate(flag_winner_cvr_changed = 
           coalesce(winner_cvr != winner_cvr_original, FALSE) & 
           (source == "single winners")
         )

# Flag missing CVR number
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_cvr =
      is.na(winner_cvr) | 
        winner_cvr == ""
  )

# Flag missing winner name
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_name = 
           is.na(winner_name) | 
           winner_name == ""
  )

# Foreign winner
clean_winner_data <- clean_winner_data %>%
  mutate(flag_foreign_winner = winner_country != "DK")

# Flag when n winners extracted agrees with original data
clean_winner_data <- clean_winner_data %>% 
  mutate(n_winners_extracted = n(), .by = c("tender_id", "lot_id"))
clean_winner_data <- clean_winner_data %>%
  mutate(flag_winner_count_agree = 
           coalesce(n_winners_extracted == n_lot_winners_original, FALSE))

# Single bidder
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_single_bidder = n_bids_received == 1
  )

# Multi-lot tender
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_multilot = n_lots > 1
  )

# Cancelled procurement
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_cancelled = tender_cancelled == 1
  )

# Observation review
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_invalid_winner_cvr =
      !flag_missing_winner_cvr & !valid_cvr,
    flag_missing_cvr_with_name =
      flag_missing_winner_cvr & !flag_missing_winner_name,
    flag_manual_review_winner =
      flag_invalid_winner_cvr |
      !flag_winner_count_agree |
      flag_missing_cvr_with_name
  )

# 3 Buyers
## Buyers do not have CVR numbers, but they have names.
buyer_data <- data %>%
  select(tender_id, lot_id, buyer_name, joint_tender) %>%
  mutate(
    joint_tender = case_when(
      joint_tender == "Enkelt" ~ "single",
      joint_tender == "Fælles" ~ "joint",
      TRUE ~ NA_character_
    )
  )
original_buyer_data <- buyer_data # Store original for later joining

## 3.1 Separate into single and multiple buyer tenders
## According to the documentation (page 27, variable 19: 'Navn på ordregiver')
## multiple contracting authorities are separated by a semicolon.
## Buyer name is always populated (no NAs); so this split completely covers the data
single_buyer_data <- buyer_data %>% 
  filter(!str_detect(buyer_name, ";"))
multi_buyer_data <- buyer_data %>%
  filter(str_detect(buyer_name, ";"))

## 3.2 Split multiple buyers into one row per buyer.
## Note, I don't need the extract_multiple_cvr() function because 
## the documentation is clear about how multiple buyers are separated
multiple_buyer_long <- multi_buyer_data %>%
  separate_rows(buyer_name, sep = ";")

## 3.3 Clean up/add buyer_numbers
multiple_buyer_long <- multiple_buyer_long %>%
  mutate(
    buyer_name = str_squish(buyer_name),
    buyer_number = row_number(),
    source = "multiple listed buyers",
    .by = c(tender_id, lot_id)
  )

## 3.4 Clean single buyer data
## If only one buyer is listed, keep one row. Joint tenders with unlisted buyers
## remain one row because the unlisted buyers cannot be separated from this field.
single_buyer_data <- single_buyer_data %>%
  mutate(
    buyer_number = 1,
    source = "single buyer or joint tender with unlisted buyers"
  )

## 3.5 Bind single and multiple buyers
clean_buyer_data <- bind_rows(single_buyer_data, multiple_buyer_long) %>%
  arrange(tender_id, buyer_number) %>%
  select(tender_id, lot_id, buyer_number, buyer_name, source)

## 3.6 Join original tender data and original buyer data
clean_buyer_data <- left_join(clean_buyer_data, tender_lot_data %>% select(-buyer_name), # Don't need to add buyer_name here. 
                               by = c("tender_id", "lot_id"))
clean_buyer_data <- left_join(clean_buyer_data, original_buyer_data, 
                               by = c("tender_id", "lot_id"),
                               suffix = c("", "_original"))


## 3.7 Quality/processing flags

## Flag joint tenders with unlisted buyers 
## (i.e. joint tenders that do not have multiple buyers 
## listed in the buyer_name field)
clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    flag_joint_unlisted_buyers =
      coalesce(joint_tender == "joint", FALSE) &
      source == "single buyer or joint tender with unlisted buyers"
  )

# Flag single buyer name changes and missingness
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_single_buyer_name_changed = 
      coalesce(buyer_name != buyer_name_original, FALSE) &
      (source == "single buyer or joint tender with unlisted buyers")
  ) 

# Flag missing buyer names
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_missing_buyer_name = is.na(buyer_name) | buyer_name == "")

# Flag extracted n_buyers with implied number from original buyer name
clean_buyer_data <- clean_buyer_data %>% 
  mutate(n_buyers_extracted = n(), .by = c(tender_id, lot_id))

clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    n_buyers_listed_original = str_count(buyer_name_original, ";") + 1,
    flag_buyer_count_agree = coalesce(n_buyers_extracted == n_buyers_listed_original, FALSE)
  )
