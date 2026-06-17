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
  distinct(lot_id, n_lot_id) %>%
  arrange(desc(n_lot_id))

# Print results of duplication check
if (nrow(dup_lots) == 0) {
  cat("All lot_id values are unique.\n")
} else {
  cat("Duplicate lot_id values:\n")
  print(dup_lots)
  
  cat("Assuming unique and creating flag")
  data <- data %>% 
    mutate(dup_lot_id_flag = ifelse(n_lot_id > 1, 1, 0)) 
  data <- data %>% 
    mutate(lot_id = ifelse(n_lot_id > 1, 
                           paste0(tender_id, "-", 1:n()), lot_id), 
           .by = lot_id) 
}


# 2 Separate winners
## Goal: Separate winners into single winners and multiple winners.

## Number of winners using number of winner names
## Winner names are separate by a comma or semicolon
data <- data %>% 
  mutate(n_winner_name = str_count(winner_name, ",|;") + 1,
         n_winner_cvr = str_count(winner_cvr, ",|;|[.]") + 1,
         n_winner_country = str_count(winner_country, ",|;"))

## Missing column
data <- data %>% 
  mutate(missing_winner_cvr = as.integer(is.na(winner_cvr)))

## Find reliably single CVRs
# CVRs without any commas, semi-colons, periods, etc. 
# and with exactly 8 characters are likely to be single CVRs; flag these
data <- data %>% 
  mutate(single_cvr = ifelse(nchar(winner_cvr) == 8 & 
                               !str_detect(winner_cvr, regex("[.,; ]")), 
                             1, NA))

# Print result
cat("Number of easily identifiable single CVRs:", 
    sum(data$single_cvr, na.rm = TRUE), "\n")

# CVRs with spaces but whose characters are all numbers and 
# with exactly 8 characters are likely to be single CVRs; flag these
data <- data %>% 
  mutate(single_cvr = ifelse(nchar(gsub(" ", "", winner_cvr)) == 8 & # removes white space
                               str_detect(winner_cvr, regex(" ")) & 
                               !str_detect(winner_cvr, regex("[.,;]")), 
                             1, single_cvr))

# Print result
cat("Number of identifiable single CVRs with separated spaces:", 
    sum(data$single_cvr, na.rm = TRUE), "\n")

# Keep object
single_data <- data %>% 
  filter(single_cvr == 1)
multi_data <- data %>% 
  filter(is.na(single_cvr))

# 3 Multiple winners
## Goal: create long dataframe with one row per winner (identified by tender_id/lot_id)
extract_multiple_cvr <- function(
    data,
    row_id,
    cvr_cols = c("winner_cvr", "winner_name", "winner_country")
) {
  
  # Extract row
  data_row <- data[row_id, ]
  
  # Keep identifiers + originals only once
  out <- data_row %>%
    select(tender_id, lot_id, dplyr::all_of(cvr_cols))
  
  # Container for expanded values (DO NOT include IDs here)
  out_list <- list()
  
  # Loop over each column separately
  for (col in cvr_cols) {
    
    x <- data_row[[col]]
    
    if (is.na(x)) next
    
    # Standardise separators
    x <- gsub("\\s*,\\s*", ";", x)
    x <- gsub("\\s*;\\s*", ";", x)
    
    # CVR-specific dot handling
    if (col == "winner_cvr") {
      x <- gsub("\\.\\s*(?=[A-Z0-9])", ";", x, perl = TRUE)
    }
    
    # Split
    # Note, I treat an empty space as a firm, not a mistake. 
    # Usually these are firms that don't have a CVR number. 
    parts <- strsplit(x, ";", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts[parts == ""] <- NA_character_ # Treat empty strings as NA (better missing label for firms without CVR)
    
    # Store
    for (i in seq_along(parts)) {
      out_list[[paste0(col, "_", i)]] <- parts[i]
    }
  }
  
  # Return base row if nothing expanded
  if (length(out_list) == 0) {
    return(out)
  }
  
  bind_cols(out, tibble::as_tibble(out_list))
}

# Map function over rows and bind.
cvr_cols_to_sep <- c("winner_cvr", "winner_name", "winner_country")
multi_data_sep <- map(1:nrow(multi_data), extract_multiple_cvr, 
    data = multi_data,
    cvr_cols = cvr_cols_to_sep) %>% 
  bind_rows()

# Order columns
column_pattern <- paste0(cvr_cols_to_sep, collapse = "|")
column_pattern <- paste0("(", column_pattern, ")_(\\d+)$")
multi_data_sep <- multi_data_sep %>% 
  select(tender_id, lot_id, matches(column_pattern))

# Pivot longer
multi_long <- multi_data_sep %>%
  
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
  mutate(winner_number = as.integer(winner_number)) %>%
  arrange(tender_id, lot_id, winner_number)

# Only keep rows where at least one of the winner identifiers are filled.
multi_long <- multi_long %>% 
  filter(!is.na(winner_cvr) | !is.na(winner_name) | !is.na(winner_country))

# Create flags for invalid CVR numbers
multi_long <- multi_long %>% 
  mutate(invalid_cvr = ifelse(nchar(winner_cvr) != 8, 1, 0))

# Print diagnostics
cat("Number of rows:", nrow(multi_long), "\n")
cat("Number of unique CVR numbers in multi-winner data:", 
    n_distinct(multi_long$winner_cvr), "\n")
cat("Number of unique CVR names in multi-winner data:", 
    n_distinct(multi_long$winner_name), "\n")
cat("Number of rows with missing CVR numbers:", 
    sum(is.na(multi_long$invalid_cvr)), "\n")
cat("Number of rows with invalid CVR numbers (nonmissing, but not exactly 8 digits):", 
    sum(multi_long$invalid_cvr, na.rm = TRUE), "\n")
cat("Share of rows with invalid CVR numbers (excluding missing):", 
    mean(multi_long$invalid_cvr, na.rm = TRUE), "\n")


# Join original CVR and winner names
original_multi_data <- multi_data %>% 
  select(tender_id, lot_id, n_lot_winners, n_bids_received, tender_cancelled,
         winner_cvr, winner_name, winner_country)
multi_long <- multi_long %>% 
  left_join(original_multi_data, 
            by = c("tender_id", "lot_id"),
            suffix = c("", "_original"))

# Reorder nicely
multi_long <- multi_long %>% 
  select(tender_id, lot_id, winner_number, n_lot_winners,
         n_bids_received, tender_cancelled,
         winner_cvr, winner_cvr_original,
         winner_name, winner_name_original,
         winner_country, winner_country_original)

# Flag when number of winners from extraction process doesn't match
# the n_lot_winners provided in the original data.
multi_long <- multi_long %>% 
  mutate(n_extracted_winners = n(), .by = lot_id)
multi_long <- multi_long %>%
  mutate(flag_winner_count_mismatch = ifelse(n_extracted_winners != n_lot_winners, 
                                        1, 0))

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
  select(tender_id, lot_id, buyer_name, joint_tender)

## Clean joint_tender variable
buyer_data <- buyer_data %>% 
  mutate(joint_tender = case_when(
    joint_tender == "Enkelt" ~ "single",
    joint_tender == "Fælles" ~ "joint",
    TRUE ~ NA_character_
  ))

## According to the documentation (page 27, variable 19: 'Navn på ordregiver')
## Multiple contracting authorities are separated by a semicolon. Flag these.
buyer_data <- buyer_data %>% 
  mutate(flag_multiple_buyers_listed = str_detect(buyer_name, ";"))

## joint_tender might be "joint" and flag_multiple_buyers_listed might be FALSE 
## if the listed buyer is an authority performing the tender on behalf of several authorities.
## Flag these.
buyer_data <- buyer_data %>% 
  mutate(flag_joint_unlisted_buyers = (joint_tender == "joint" & !flag_multiple_buyers_listed))

## Single versus multiple buyers
single_buyer <- buyer_data %>% 
  filter(!flag_multiple_buyers_listed)
multiple_buyer <- buyer_data %>% 
  filter(flag_multiple_buyers_listed, !flag_joint_unlisted_buyers)

## For multiple buyers, record the number of buyers
multiple_buyer <- multiple_buyer %>% 
  mutate(n_buyers_extracted = str_count(buyer_name, ";") + 1)

## Split multiple buyers and pivot longer
multiple_buyer_sep <- multiple_buyer %>% 
  separate_wider_delim(buyer_name, delim = ";", cols_remove = FALSE, 
                       names_sep = "_", too_few = "align_start") %>% 
  select(tender_id, lot_id, starts_with("buyer_name_"), -buyer_name_buyer_name)

multiple_buyer_long <- multiple_buyer_sep %>% 
  pivot_longer(., cols = -c("tender_id", "lot_id"), names_to = "buyer_number", values_to = "buyer_name")

multiple_buyer_long <- multiple_buyer_long %>%
  mutate(buyer_number = as.integer(str_remove(buyer_number, "buyer_name_"))) %>% 
  arrange(tender_id, lot_id, buyer_number) %>% 
  filter(!is.na(buyer_name))

## Check number of buyers extracted equals estimate from original wide data
n_buyer_extracted_check <- left_join(multiple_buyer %>% 
            distinct(lot_id, n_buyers_extracted), 
          multiple_buyer_long %>% 
            summarise(buyer_number = max(buyer_number), .by = lot_id), 
          by = c("lot_id")) %>% 
  mutate(check = n_buyers_extracted == buyer_number) %>% 
  .$check %>% 
  all

if (!n_buyer_extracted_check) {
  stop("Number of buyers extracted didn't match original estimate from wide data.frame.")
} else {
  cat("Number of buyers extracted matches original estimate from wide data.frame.\n")
}

## Bind single buyers with multiple buyers
single_buyer <- single_buyer %>% 
  select(tender_id, lot_id, buyer_name) %>% 
  mutate(buyer_number = 1)

buyer_data_clean <- bind_rows(single_buyer, multiple_buyer_long) %>% 
  arrange(tender_id, lot_id, buyer_number)

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
