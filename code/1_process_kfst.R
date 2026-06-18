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
    "tender_id", "lot_id", "lot_number",
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

## 2.5 Winners
# Flag valid CVR numbers (exactly 8 digits, no letters or special characters)
# missing = NA, valid = FALSE, invalid = TRUE
clean_winner_data <- clean_winner_data %>% 
  mutate(valid_cvr = grepl("^\\d{8}$", winner_cvr))

# Flag transformed winner CVR number (not equal to original winner CVR number)
# Don't do this for multiple winners because the original winner CVR number 
# is not necessarily wrong in this case (it may just be the first of multiple 
# CVRs listed in the original data, which we have now separated into multiple rows).
clean_winner_data <- clean_winner_data %>% 
  mutate(flag_winner_cvr_changed = 
           (winner_cvr != winner_cvr_original) & 
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
           n_winners_extracted == n_lot_winners_original)

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


# Print diagnostics
cat("Share of obs. with winner count mismatch:", 
    mean(multi_long$flag_winner_count_mismatch, na.rm = TRUE), "\n")
cat("Share of lots with winner count mismatch:", 
    mean(multi_long %>% 
           distinct(lot_id, flag_winner_count_mismatch) %>% 
           .$flag_winner_count_mismatch, na.rm = TRUE), "\n")


# 4 Bind
clean_winner_data <- bind_rows(
  single_data %>% 
    select(tender_id, lot_id, n_lot_winners, n_bids_received, tender_cancelled,
           winner_cvr, winner_name, winner_country),
  multi_long %>%
    select(tender_id, lot_id, n_lot_winners, n_bids_received, tender_cancelled,
           winner_cvr, winner_name, winner_country)
) %>% 
  arrange(tender_id)

## 4.1 Data munging
clean_winner_data <- clean_winner_data %>% 
  mutate(n_winners_extracted = n(), .by = lot_id)


# 5 Buyers
## Buyers do not have CVR numbers, but they have names.
buyer_data <- data %>%
  select(tender_id, lot_id, buyer_name, joint_tender) %>%
  mutate(
    buyer_name_original = buyer_name,
    joint_tender = case_when(
      joint_tender == "Enkelt" ~ "single",
      joint_tender == "Fælles" ~ "joint",
      TRUE ~ NA_character_
    )
  )

## According to the documentation (page 27, variable 19: 'Navn på ordregiver')
## multiple contracting authorities are separated by a semicolon.
buyer_data <- buyer_data %>%
  mutate(
    flag_multiple_buyers_listed = str_detect(buyer_name_original, ";"),
    flag_joint_unlisted_buyers =
      joint_tender == "joint" & !flag_multiple_buyers_listed,
    n_buyers_listed_original = if_else(
      flag_multiple_buyers_listed,
      str_count(buyer_name_original, ";") + 1L,
      1L
    )
  )

## If multiple buyers are explicitly listed, split them into one row per buyer.
multiple_buyer_long <- buyer_data %>%
  filter(flag_multiple_buyers_listed, !flag_joint_unlisted_buyers) %>%
  separate_rows(buyer_name, sep = ";") %>%
  mutate(
    buyer_name = str_squish(buyer_name), # Clean up white space
    buyer_number = row_number(),
    source = "multiple listed buyers",
    .by = lot_id
  )

## If only one buyer is listed, keep one row. Joint tenders with unlisted buyers
## remain one row because the unlisted buyers cannot be separated from this field.
single_buyer <- buyer_data %>%
  filter(!flag_multiple_buyers_listed | flag_joint_unlisted_buyers) %>%
  mutate(
    buyer_name = str_squish(buyer_name),
    buyer_number = 1L,
    source = "single buyer or joint tender with unlisted buyers"
  )

## Bind single and multiple listed buyers, then add row-level quality checks.
buyer_data_clean <- bind_rows(single_buyer, multiple_buyer_long) %>%
  arrange(tender_id, lot_id, buyer_number) %>%
  mutate(
    n_buyers_extracted = n(),
    flag_buyer_name_missing = is.na(buyer_name) | buyer_name == "",
    flag_buyer_name_changed = buyer_name != buyer_name_original,
    flag_buyer_count_agree =
      n_buyers_extracted == n_buyers_listed_original,
    .by = lot_id
  )

## Check number of buyers extracted equals estimate from original wide data.
n_buyer_extracted_check <- buyer_data_clean %>%
  filter(flag_multiple_buyers_listed, !flag_joint_unlisted_buyers) %>%
  distinct(lot_id, flag_buyer_count_agree) %>%
  pull(flag_buyer_count_agree) %>%
  all(na.rm = TRUE)

if (!n_buyer_extracted_check) {
  stop("Number of buyers extracted didn't match original estimate from wide data.frame.")
} else {
  cat("Number of buyers extracted matches original estimate from wide data.frame.\n")
}


# 6 Cleaning flags

# Missing winner information
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_missing_winner_name = is.na(winner_name) | winner_name == ""
  )

# Missing CVR number
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_missing_winner_cvr =
      is.na(winner_cvr) | winner_cvr == ""
  )

# Invalid CVR number
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_invalid_cvr =
      !is.na(winner_cvr) &
      !grepl("^\\d{8}$", winner_cvr)
  )

# Multiple winners (only flag when raw data and extracted data agree)
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_multiple_winners = case_when(
      n_winners_extracted == n_lot_winners & n_winners_extracted > 1 ~ TRUE,
      n_winners_extracted == n_lot_winners & n_winners_extracted == 1 ~ FALSE,
      n_winners_extracted != n_lot_winners ~ NA
    )
  )

# Extracted N winners and raw N winners disagreement
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_winner_count_agree = n_winners_extracted == n_lot_winners)

# Foreign winner
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_foreign_winner =
      winner_country != "DK"
  )

# Single bidder
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_single_bidder =
      n_bids_received == 1
  )

# Multi-lot tender
clean_winner_data <- clean_winner_data %>% 
  mutate(n_lots = n(), .by = tender_id)
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_multilot =
      n_lots > 1
  )

# Cancelled procurement
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_cancelled =
      tender_cancelled == 1
  )
