# Cleans KFST provided tender data
# Author: Jack Mulqueeney
# Date: 16 June 2026

# Clean environment
rm(list = ls())

# Config: run from the project root or use run_replication.sh.
source("config.R")

# Packages
suppressWarnings(suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(readxl)
}))

# Paths
raw_data_dir <- dirs$raw_data
raw_data_name <- "udbudsdata_kfst.xlsx"

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# 1 Load data
data <- read_excel(file.path(raw_data_dir, "kfst", raw_data_name), sheet = "2.0 Udbudsdata")

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
         contract_type = `Rammeaftale`,
         divided_tender = `Opdelt udbud`,
         joint_tender = `Fælles-/enkeltudbud`,
         consortium_winner = `Konsortium/Sammenslutning`,
         cpv_code = `CPV-koder`,
         tender_cancelled = `Annulleret udbud`,
         tender_status = `Helt/delvist gennemført/annulleret`,
         estimated_tender_amount = `Estimat af samlet kontraktværdi - angivet i udbud`,
         estimated_lot_amount = `Estimat af delkontrakts kontraktværdi`,
         final_tender_amount = `Endelig kontraktværdi`,
         final_lot_amount = `Endelig værdi af delaftaler`,
         lot_number = `Delkontraktnr.`,
         n_lots = `Antal delkontrakter kortlagt`,
         n_lots_contracted = `Antal delkontrakter i udbudsbekendtgørelsen`,
         n_lot_winners = `Antal vindere på delkontrakten`,
         n_bids_received = `Antal modtagne bud`,
         contract_duration_months_min = `Varighed af kontrakten i måneder (min)`,
         contract_duration_months_max = `Varighed af kontrakten i måneder (max)`)

# Standardise tender-level fields before they are joined onto buyer/winner rows.
## Tender/lot amount
data <- data %>%
  mutate(
    tender_amount = coalesce(
      as.numeric(final_tender_amount),
      as.numeric(estimated_tender_amount)
    ),
    lot_amount = coalesce(
      as.numeric(final_lot_amount),
      as.numeric(estimated_lot_amount)
    )
  )

## Number of bidders
data <- data %>%
  mutate(n_bidders = as.numeric(n_bids_received))

## Award date
data <- data %>%
  mutate(
    award_date = coalesce(
      as.Date(
        if_else(
          str_detect(as.character(award_date), "^[0-9]+$"),
          suppressWarnings(as.numeric(award_date)),
          NA_real_
        ),
        origin = "1899-12-30"
      ),
      lubridate::ymd(as.character(award_date), quiet = TRUE)
    )
  )

## Framework agreement
data <- data %>%
  mutate(contract_type = case_when(
    contract_type == "Offentlig kontrakt" ~ "Public contract",
    contract_type == "Rammeaftale" ~ "Framework agreement"))

## Tender awarded
# KFST rows are at the lot (delkontrakt) level, and `tender_cancelled`
# (variable 43, "Annulleret udbud") records annulment at that level: "Nej" = the
# lot was carried out (awarded), "Ja" = annulled. This is the right granularity
# for "keep awarded lots only" — it drops the annulled lots inside partially-
# completed tenders that the tender-level status (variable 44, `tender_status`)
# would keep. Mirrors the OpenTender flag_awarded; default FALSE if status is
# missing.
data <- data %>%
  mutate(flag_awarded = coalesce(tender_cancelled == "Nej", FALSE))

## Award end date
# KFST has no contract end date, only the award (start) date and the contract
# duration in months. Per the documentation, "min" (variable 46) is the base
# contract length excluding options and is the more reliable/complete field,
# while "max" (variable 47) includes extension options; prefer min, fall back to
# max. Months are approximated as 30 days. Populated for awarded lots only.
data <- data %>%
  mutate(award_end_date = if_else(
    flag_awarded,
    award_date + coalesce(
      as.numeric(contract_duration_months_min),
      as.numeric(contract_duration_months_max)
    ) * 30,
    as.Date(NA)
  ))

## Annualised framework amounts
# A framework agreement's amount covers its whole (multi-year) duration, so the
# headline total is not comparable to a single-year contract. Annualise it:
# amount per month (amount / duration in months) scaled to 12 months. KFST
# records duration in months, so annualising by month avoids any day
# approximation. Uses the base ("min") duration, falling back to "max", matching
# award_end_date. Framework agreements only, and only where the amount and a
# positive duration are both present (the > 0 guard avoids divide-by-zero).
data <- data %>%
  mutate(
    contract_duration_months = coalesce(
      as.numeric(contract_duration_months_min),
      as.numeric(contract_duration_months_max)
    ),
    annualised_tender_amount = if_else(
      contract_type == "Framework agreement" &
        !is.na(contract_duration_months) & contract_duration_months > 0,
      tender_amount / contract_duration_months * 12,
      NA_real_
    ),
    annualised_lot_amount = if_else(
      contract_type == "Framework agreement" &
        !is.na(contract_duration_months) & contract_duration_months > 0,
      lot_amount / contract_duration_months * 12,
      NA_real_
    )
  )

## CPV code
## Tenders can list several CPV codes; as a first pass keep the first listed
## code and map it to its EU CPV division (the broadest interpretable grouping).
cpv_prepared <- clean_cpv_code(data$cpv_code)
data <- data %>%
  mutate(
    cpv_code_first = cpv_prepared$code_first,
    cpv_division = cpv_prepared$division,
    cpv_division_name = cpv_prepared$division_name,
    # Coarser groupings for treatment-effect heterogeneity (large enough cells).
    cpv_sector = cpv_prepared$sector,
    cpv_category = cpv_prepared$category
  )

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
    "tender_id", "lot_id", "contract_type", "lot_number", "buyer_name",
    "n_lots", "n_lots_contracted", "n_lot_winners", "n_bids_received",
    "tender_amount", "lot_amount", "n_bidders",
    "pub_date", "award_date", "submit_date",
    "divided_tender", "joint_tender", "consortium_winner",
    "cpv_code", "cpv_code_first", "cpv_division", "cpv_division_name",
    "cpv_sector", "cpv_category",
    "tender_cancelled", "tender_status", "flag_awarded",
    "contract_duration_months_min", "contract_duration_months_max", "award_end_date",
    "annualised_tender_amount", "annualised_lot_amount",
    "n_lot_id"
  ))) %>%
  arrange(tender_id, lot_id, lot_number)


# 2 Winners
winner_data <- data %>% 
  select(tender_id, lot_id, winner_cvr, winner_name, winner_country)

# Store original winner fields separately. The cleaned table keeps both the raw
# source field and the standardized CVR field.
original_winner_data <- winner_data %>%
  rename(
    winner_cvr_original = winner_cvr,
    winner_name_original = winner_name,
    winner_country_original = winner_country
  )

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
                             entity_cols = cvr_cols_to_sep,
                             cvr_column = "winner_cvr") %>%
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

clean_winner_data <- clean_winner_data %>%
  rename(winner_cvr_candidate_original = winner_cvr)

clean_winner_data <- clean_winner_data %>%
  mutate(winner_cvr_clean = winner_cvr_candidate_original)

## 2.4 Join original tender data and original winner data
clean_winner_data <- left_join(clean_winner_data, tender_lot_data, 
                               by = c("tender_id", "lot_id"))
clean_winner_data <- left_join(clean_winner_data, original_winner_data, 
                               by = c("tender_id", "lot_id"),
                               suffix = c("", "_original"))

### Only keep lots that had winners (defined by original data)
clean_winner_data <- clean_winner_data %>% 
  filter(n_lot_winners > 0)

## 2.5 Clean up/standardise CVR numbers
## Cleaning flags treat NAs as FALSE: a missing source value is not counted as
## evidence that a cleaning operation was performed.
clean_winner_data <- clean_winner_data %>%
  mutate(
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_clean, "\\s"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),
    
    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_clean, "[[:alpha:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),
    
    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
    
    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | 
        flag_cvr_alphabet | 
        flag_cvr_punct,
      FALSE
    )
    )

## 2.6 Standardise winner names for matching
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

## 2.7 Initial winner CVR quality flags
## Quality flags treat NAs as FALSE: missing values are captured by explicit
## missingness flags, not by propagating NA through boolean indicators.
# Flag valid CVR numbers (exactly 8 digits, no letters or special characters)
# missing/invalid = FALSE, valid = TRUE
clean_winner_data <- clean_winner_data %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

# Flag transformed winner CVR number (not equal to original winner CVR number)
# Don't do this for multiple winners because the original winner CVR number 
# is not necessarily wrong in this case (it may just be the first of multiple 
# CVRs listed in the original data, which we have now separated into multiple rows).
clean_winner_data <- clean_winner_data %>% 
  mutate(
    flag_winner_cvr_changed = coalesce(
      winner_cvr_clean != winner_cvr_candidate_original,
      FALSE
    )
  )

## 2.8 Fill missing CVRs when the same winner name has one valid CVR elsewhere
### 2.8.1 Count the distinct valid CVRs observed for each exact winner name
# This uses the original winner name rather than a standardised name. Exact-name
# matching is more conservative because it does not combine similar-looking firms.
valid_invalid_cvr_winner_key <- clean_winner_data %>%
  filter(!is.na(winner_name), winner_name != "") %>%
  distinct(winner_name, winner_cvr_clean, valid_cvr) %>%
  mutate(
    n_valid_cvr = sum(valid_cvr),
    n_total_cvr = n(), # Includes missing and invalid cleaned CVR values
    .by = winner_name
  )

### 2.8.2 Record which KFST lots supplied each valid winner-name/CVR pair
# lot_id uniquely identifies the original KFST lot. Keeping every source lot
# makes each borrowed CVR traceable back to the rows that supplied it.
valid_cvr_sources <- clean_winner_data %>%
  filter(valid_cvr, !is.na(winner_name), winner_name != "") %>%
  summarise(
    lot_id_borrowed_from = paste(sort(unique(lot_id)), collapse = ";"),
    .by = c(winner_name, winner_cvr_clean)
  )

### 2.8.3 Keep only names linked to exactly one distinct valid CVR
# Names linked to several valid CVRs are ambiguous and are not filled.
single_valid_cvr_key <- valid_invalid_cvr_winner_key %>%
  filter(n_valid_cvr == 1, n_total_cvr > 1, valid_cvr) %>%
  rename(winner_cvr_valid_from_same_name = winner_cvr_clean) %>%
  select(-valid_cvr, -n_valid_cvr, -n_total_cvr) %>%
  distinct()

single_valid_cvr_key <- single_valid_cvr_key %>%
  left_join(
    valid_cvr_sources,
    by = c(
      "winner_name",
      "winner_cvr_valid_from_same_name" = "winner_cvr_clean"
    )
  )

### 2.8.4 Join the same-name CVR onto the full winner data
# Missing winner names cannot match each other. This prevents unrelated rows
# with missing names from borrowing a CVR from one another.
n_winner_rows_before_cvr_fill <- nrow(clean_winner_data)

clean_winner_data <- left_join(
  clean_winner_data,
  single_valid_cvr_key,
  by = "winner_name",
  na_matches = "never"
)

if (nrow(clean_winner_data) != n_winner_rows_before_cvr_fill) {
  stop("Joining same-name CVRs changed the number of KFST winner rows.")
}

### 2.8.5 Fill only rows whose cleaned CVR is missing
# Use winner_cvr_clean here rather than winner_cvr_original. A multiple-winner
# source row can contain CVRs overall while one separated winner still has none.
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_fill_missing_cvr = coalesce(
      (is.na(winner_cvr_clean) | winner_cvr_clean == "") &
        !is.na(winner_cvr_valid_from_same_name) &
        winner_cvr_valid_from_same_name != "",
      FALSE
    )
  )

clean_winner_data <- clean_winner_data %>%
  mutate(
    winner_cvr_clean = ifelse(
      flag_fill_missing_cvr,
      winner_cvr_valid_from_same_name,
      winner_cvr_clean
    ),
    # Source lots are relevant only when a CVR was actually borrowed.
    lot_id_borrowed_from = ifelse(
      flag_fill_missing_cvr,
      lot_id_borrowed_from,
      NA_character_
    )
  )

# Recalculate validity after filling the missing CVRs.
clean_winner_data <- clean_winner_data %>%
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

n_filled_winner_cvrs <- sum(clean_winner_data$flag_fill_missing_cvr)
cat("Number of missing winner CVRs filled from the same exact winner name:",
    n_filled_winner_cvrs, "\n")

if (any(clean_winner_data$flag_fill_missing_cvr & !clean_winner_data$valid_cvr)) {
  stop("At least one borrowed KFST winner CVR is not a valid eight-digit CVR.")
}

if (any(clean_winner_data$flag_fill_missing_cvr &
        (is.na(clean_winner_data$lot_id_borrowed_from) |
           clean_winner_data$lot_id_borrowed_from == ""))) {
  stop("At least one borrowed KFST winner CVR is missing its source lot.")
}

## 2.9 Other winner quality flags
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
  mutate(flag_foreign_winner = coalesce(winner_country != "DK", FALSE))

# Missing country
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_country = coalesce(is.na(winner_country), FALSE))

# Flag when n winners extracted agrees with original data
clean_winner_data <- clean_winner_data %>% 
  mutate(n_winners_extracted = n(), .by = c("tender_id", "lot_id"))
clean_winner_data <- clean_winner_data %>%
  mutate(flag_mismatch_winner_count = 
           coalesce(n_winners_extracted != n_lot_winners, FALSE))

# Single bidder
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_single_bidder = coalesce(n_bids_received == 1, FALSE)
  )

# Multi-lot tender
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_multilot = coalesce(n_lots > 1, FALSE)
  )

# Cancelled procurement
clean_winner_data <- clean_winner_data %>%
  mutate(flag_cancelled = coalesce(tender_cancelled != "Nej", FALSE))

# Observation review
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_missing_cvr_with_name = coalesce(
      flag_missing_winner_cvr & !flag_missing_winner_name,
      FALSE
    ),
    flag_check_fuzzy_match = coalesce(
      flag_missing_winner_cvr & !flag_missing_winner_name,
      FALSE
    ),
    flag_review_cvr = coalesce(!flag_missing_winner_cvr & !valid_cvr, FALSE),
    flag_review_n_winners = coalesce(flag_mismatch_winner_count, FALSE), 
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
                               by = c("tender_id", "lot_id"),
                              suffix = c("", "_original"))
clean_buyer_data <- left_join(clean_buyer_data, original_buyer_data, 
                               by = c("tender_id", "lot_id"),
                               suffix = c("", "_original"))


## 3.6 Standardise buyer names for matching
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

## 3.7 Quality/processing flags
## Flag joint tenders with unlisted buyers 
## (i.e. joint tenders that do not have multiple buyers 
## listed in the buyer_name field)
clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    flag_joint_unlisted_buyers = coalesce(
      joint_tender == "joint" &
        source == "single buyer or joint tender with unlisted buyers",
      FALSE
    )
  )

# Flag single buyer name changes and missingness
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_single_buyer_name_changed = coalesce(
      buyer_name != buyer_name_original &
        source == "single buyer or joint tender with unlisted buyers",
      FALSE
    )
  )

# Flag missing buyer names
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_missing_buyer_name = coalesce(is.na(buyer_name) | buyer_name == "", FALSE))

# Flag extracted n_buyers with implied number from original buyer name
clean_buyer_data <- clean_buyer_data %>% 
  mutate(n_buyers_extracted = n(), .by = c(tender_id, lot_id))

clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    n_buyers_listed_original = str_count(buyer_name_original, ";") + 1,
    flag_buyer_count_agree = coalesce(n_buyers_extracted == n_buyers_listed_original, FALSE)
  )

# Flag fuzzy match check (only requires non-missing buyer name; no CVR numbers available)
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_check_fuzzy_match = coalesce(!flag_missing_buyer_name, FALSE))

# Check that amount fields are present in both saved KFST outputs.
required_amount_cols <- c("tender_amount", "lot_amount")
stopifnot(all(required_amount_cols %in% names(clean_winner_data)))
stopifnot(all(required_amount_cols %in% names(clean_buyer_data)))

# 4 Save 
saveRDS(clean_winner_data, file.path(dirs$clean_data, "clean_winner_data_kfst.rds"))
saveRDS(clean_buyer_data, file.path(dirs$clean_data, "clean_buyer_data_kfst.rds"))
