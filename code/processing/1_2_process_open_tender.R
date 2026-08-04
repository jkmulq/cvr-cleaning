# Cleans OpenTender provided tender data
# Author: Jack Mulqueeney
# Date: 18 June 2026

# Clean environment
rm(list = ls())

# Config: run from the project root or use run_replication.sh.
source("config.R")

# Packages
suppressWarnings(suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(readxl)
  library(data.table)
}))

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

## 1.3 Basic munging
## Keep all OpenTender source fields, but rename the variables that clearly
## correspond to the KFST naming convention.
data <- data %>%
  rename(
    lot_id = lot_lotId,
    lot_number = lot_lotNumber,
    n_bids_received = lot_bidsCount,
    n_lots = tender_lots_count,
    submit_date = tender_bidDeadline,
    award_date = tender_awardDecisionDate,
    cpv_code = tender_cpvs,
    divided_tender = tender_hasLots,
    contract_type = tender_isFrameworkAgreement,
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

# Keep only the awarded sample: real awards (AWARDED) plus framework/DPS
# qualification panels (PREAWARDED). Drops ANNOUNCED/PREPARED (pre-award) and
# CANCELLED lots, none of which have a winner. Also drop cancelled tenders.
data <- data %>%
  filter(lot_status %in% c("AWARDED", "PREAWARDED"),
         !tender_cancelled)

# Rearrange so easier to keep good data entries and dedup
# Also keep a stable reference to the original OpenTender row.
# This lets expanded winner rows point back to the raw bid row.
data <- data %>% 
  arrange(tender_id, lot_id, 
          buyer_name_original, desc(buyer_cvr_original),
          winner_name_original, desc(winner_cvr_original), 
          desc(bid_isWinning == "yes")) %>%
  mutate(row_id = row_number()) %>% 
  select(row_id, everything())

# Only keep valid tender-lot-winner-buyer matches
data <- data %>%
  distinct(tender_id, lot_id, 
           winner_cvr_original, winner_name_original, 
           buyer_cvr_original, buyer_name_original, .keep_all = TRUE)

## Tender/lot amount. 
data <- data %>%
  mutate(
    tender_amount = coalesce(
      parse_number(tender_finalPrice_EUR),
      parse_number(tender_estimatedPrice_EUR)
    ),
    lot_amount = coalesce(
      parse_number(bid_price_EUR),
      parse_number(lot_estimatedPrice_EUR)
    ),
    bid_amount = parse_number(bid_price_EUR)
  )

## Number of bidders
data <- data %>%
  mutate(n_bidders = parse_number(n_bids_received))

## Award date
data <- data %>%
  mutate(award_date = as.Date(award_date))

data <- data %>%
  mutate(award_date = dplyr::if_else(is.na(award_date),
                                     lubridate::ymd(tender_publications_firstdContractAwardDate),
                                     award_date))


## Framework agreement
data <- data %>%
  mutate(contract_type = case_when(
    contract_type == "yes" ~ "Framework agreement",
    contract_type == "no" ~ "Public contract"
  ))

## Framework agreement end date
# OpenTender has no dedicated framework-duration field, so derive an end date:
# use the reported completion date when present, otherwise add the reported
# duration to a start anchor (planned start date, else award date). The duration
# falls back from the most to least granular unit (days, then months, then
# years). Populated for framework agreements only.
data <- data %>%
  mutate(
    framework_start_anchor = coalesce(
      award_date,
      lubridate::ymd(tender_estimatedStartDate)
    ),
    framework_duration_days = coalesce(
      parse_number(tender_estimatedDurationInDays),
      parse_number(tender_estimatedDurationInMonths) * 30,
      parse_number(tender_estimatedDurationInYears) * 365
    ),
    framework_end_date = coalesce(
      lubridate::ymd(tender_estimatedCompletionDate),
      framework_start_anchor + framework_duration_days
    ),
    framework_end_date = if_else(
      contract_type == "Framework agreement",
      framework_end_date,
      as.Date(NA)
    )
  ) 

## Fill missing amounts
# Check whether tenders with multiple rows disagree across tender amounts
tender_variance <- data %>% 
  summarise(tvals = n_distinct(tender_amount), 
            n = n(), 
            .by = tender_id) %>% 
  filter(n > 1) %>% 
  arrange(-tvals)

check <- (tender_variance$tvals == 1)
if (!all(check)) { 
  stop("some tender IDs have more than one row with different listed amounts.")
}

# Lot amount variance check
# The following row is not present in the TED notice:
# (tender_id, lot_id, winner_name, lot_amount) = 
# (b96cecee-2793-471d-896e-1c88e1feca56, group_EU_tender_7b094b56ffad217bea213493b5bf7460f52036da_3,
# ISS Facility Services A/S, 2769258)
# The real row has a lot amount = 1846172 Euros. 
# See the TED notice here: http://ted.europa.eu/udl?uri=TED:NOTICE:166156-2017:TEXT:EN:HTML
# Section V, second lot lists the lot amount as 13,744,732 DKK (roughly 1846172 Euros)
# I remove it before the check below, but if the check below fails again must investigate other failures.
data <- data %>%
  filter(!(tender_id == "b96cecee-2793-471d-896e-1c88e1feca56" &
             lot_id == "group_EU_tender_7b094b56ffad217bea213493b5bf7460f52036da_3" &
             winner_name_original == "ISS Facility Services A/S" &
             round(lot_amount, 0) == 2769258))

# Create check for lot amount
lot_variance <- data %>% 
  summarise(lvals = n_distinct(lot_amount), 
            n = n(), 
            rows = paste(row_id, collapse = ", "),
            # Note: Adds winner_name_original because many lots are genuinely multi-winner lots
            .by = c(tender_id, lot_id, winner_name_original)) %>% 
  filter(n > 1) %>% 
  arrange(-lvals)

check <- (lot_variance$lvals == 1)
if (!all(check)) { 
  check_failures <- lot_variance %>% filter(lvals > 1)
  print(check_failures)
  stop("some lot-winner pairs have multiple rows with different listed amounts. they are printed above.")
}

# Fill a missing tender amount with the sum over its distinct lot IDs, but only
# when every lot amount is present. Summing over distinct lot_ids (not distinct
# amounts, and not raw rows) counts each lot exactly once despite the OT explosion
# (a lot repeats across buyer/bid rows) and despite two lots sharing an amount.
data <- data %>%
  mutate(tender_amount_orig = tender_amount,
         tender_amount = if (all(is.na(tender_amount)) & all(!is.na(lot_amount))) {
           sum(lot_amount[!duplicated(lot_id)])
         } else {
           tender_amount
         },
         .by = tender_id)

# Equally split tender over all lots, if all lot amounts are unavailable
data <- data %>%
  mutate(lot_amount_orig = lot_amount,
         flag_all_orig_lot_amt_missing = all(is.na(lot_amount_orig)),
         # Genuine cross-lot split: a multi-lot tender with a tender amount to
         # divide. (Single-lot tenders trivially get lot_amount = tender_amount;
         # tenders with no tender amount stay NA -- neither is a synthetic split.)
         flag_lot_amt_equal_split = flag_all_orig_lot_amt_missing &
           n_distinct(lot_id) > 1 & !is.na(tender_amount),
         lot_amount = ifelse(flag_all_orig_lot_amt_missing,
                             tender_amount / n_distinct(lot_id),
                             lot_amount_orig),
         .by = tender_id)

# Framework/DPS pre-awards (PREAWARDED) are a single synthetic lot shared by all
# qualified suppliers, so the equal-split above gave each row the FULL framework
# ceiling. Re-split it equally across the qualified suppliers as an expected
# per-firm slice, and flag it (an estimated ceiling split, not a realised award).
data <- data %>%
  mutate(flag_framework_prequalified = (lot_status == "PREAWARDED"),
         lot_amount = ifelse(flag_framework_prequalified & !is.na(tender_amount),
                             tender_amount / n_distinct(winner_name_original),
                             lot_amount),
         .by = c(tender_id, lot_id))

## Add the DKK counterpart at Denmark's fixed ERM II central rate (7.46038 DKK
## per EUR, +/-2.25% band). Derived after the amount fill so imputed values are
## included, and on the tender/lot-level data so the EUR/DKK columns propagate to
## the winner and buyer tables through the joins below.
# OT amounts are in Euros.
dkk_per_eur <- 7.46038
data <- data %>%
  mutate(
    tender_amount_eur = tender_amount,
    lot_amount_eur    = lot_amount,
    tender_amount_dkk = tender_amount * dkk_per_eur,
    lot_amount_dkk    = lot_amount    * dkk_per_eur
  )

## Annualised framework amounts
# A framework agreement's amount covers its whole (multi-year) duration, so the
# headline total is not comparable to a single-year contract. Annualise it:
# amount per day (amount / framework_duration_days) scaled to a 365-day year.
# framework_duration_days is the same duration used for framework_end_date above.
# Framework agreements only, and only where the amount and a positive duration
# are both present (the > 0 guard avoids divide-by-zero).
data <- data %>%
  mutate(
    annualised_tender_amount = if_else(
      contract_type == "Framework agreement" &
        !is.na(framework_duration_days) & framework_duration_days > 0,
      tender_amount / framework_duration_days * 365,
      NA_real_
    ),
    annualised_lot_amount = if_else(
      contract_type == "Framework agreement" &
        !is.na(framework_duration_days) & framework_duration_days > 0,
      lot_amount / framework_duration_days * 365,
      NA_real_
    )
  )

## CPV code
## Tenders can list several CPV codes (comma-separated here); as a first pass
## keep the first listed code and map it to its EU CPV division (the broadest
## interpretable grouping). OpenTender spans many years, so codes mix the CPV
## 2003 and CPV 2008 vocabularies; clean_cpv_code() handles both.
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

## Tender awarded
# Make a flag at the lot level for whether the lot is awarded.
# I am strict here: only say TRUE if "yes", otherwise false
data <- data %>% 
  mutate(flag_awarded = ifelse(tender_isAwarded == "yes", TRUE, FALSE))

## 1.4 Separate winners/buyers/original data
winner_data_original <- data %>%
  mutate(winner_cvr = winner_cvr_original,
         winner_name = winner_name_original,
         winner_country = winner_country_original)
buyer_data_original <- data %>%
  mutate(buyer_cvr = buyer_cvr_original,
         buyer_name = buyer_name_original,
         buyer_country = buyer_country_original)


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
    # Standardise the delimiters between multiple CVRs to ';'. Same enumerated
    # list as the buyer path, extended with hyphen and period. It must cover
    # every separator that can join two CVRs; a separator missing here leaves two
    # CVRs glued in one token and crashes map_chr() with "Result must be length
    # 1, not 2". We deliberately do NOT split on arbitrary digit boundaries,
    # because firm names can contain digits (e.g. "5E Byg A/S") and would be
    # fragmented into spurious tokens.
    winner_cvr = ifelse(
      flag_row_multiple_valid_cvr,
      str_replace_all(
        winner_cvr,
        regex("\\s*(,|;|\\||/|&|-|\\.|\\bog\\b|\\bsamt\\b|\\band\\b|(?<=[\\d\\)])og(?=[[:alnum:]])|(?<=\\d)samt(?=\\d)|(?<=\\d)and(?=\\d))\\s*", ignore_case = TRUE),
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
    
    # Candidate had whitespace
    flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),

    # Candidate had alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),

    # Candidate had punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_candidate, "[[:punct:]]"), FALSE),

    # Candidate had any of the above. NB: these flags only record what the raw
    # candidate contained - the removal itself happens at the extraction step
    # (extract_valid_cvr_candidates), not explicitly here. A TRUE flag whose
    # winner/buyer_cvr_clean is a clean 8-digit CVR means that character was removed.
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
         
         # Candidate had whitespace
         flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate, "\\s"), FALSE),

         # Candidate had alphabetical letters
         flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate, "[[:alpha:]]"), FALSE),

         # Candidate had punctuation
         flag_cvr_punct = coalesce(str_detect(winner_cvr_candidate, "[[:punct:]]"), FALSE),

         # Candidate had any of the above. NB: these flags only record what the raw
         # candidate contained - the removal itself happens at the extraction step
         # (extract_valid_cvr_candidates), not explicitly here. A TRUE flag whose
         # winner/buyer_cvr_clean is a clean 8-digit CVR means that character was removed.
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

## 2.7 No tender-data join needed: the winner table is seeded from the full `data`,
## so the tender/lot/amount fields already ride along with each cleaned row.

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
  mutate(
    winner_cvr_clean = ifelse(flag_fill_missing_cvr, winner_cvr_valid_from_same_name, winner_cvr_clean),
    # Keep source provenance only on rows whose CVR was actually filled (mirrors
    # the buyer path). Winner's row_id_borrowed_from is a ';'-collapsed string,
    # so reset to NA_character_.
    row_id_borrowed_from = ifelse(flag_fill_missing_cvr, row_id_borrowed_from, NA_character_)
  )

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
      !is.na(winner_country) & trimws(winner_country) != "" &
        toupper(trimws(winner_country)) != "DK",
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
      "tender_amount", "lot_amount", "bid_amount",
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
    # Standardise the delimiters between multiple CVRs to ';' (same list as the
    # winner path, extended with hyphen and period). See the winner note: we do
    # NOT split on arbitrary digit boundaries, to avoid fragmenting firm names
    # that contain digits.
    buyer_cvr = ifelse(
      flag_row_multiple_valid_cvr,
      str_replace_all(
        buyer_cvr,
        regex("\\s*(,|;|\\||/|&|-|\\.|\\bog\\b|\\bsamt\\b|\\band\\b|(?<=[\\d\\)])og(?=[[:alnum:]])|(?<=\\d)samt(?=\\d)|(?<=\\d)and(?=\\d))\\s*", ignore_case = TRUE),
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
    
    # Candidate had whitespace
    flag_cvr_ws = coalesce(str_detect(buyer_cvr_candidate, "\\s"), FALSE),
    
    # Candidate had alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(buyer_cvr_candidate, "[[:alpha:]]"), FALSE),
    
    # Candidate had punctuation
    flag_cvr_punct = coalesce(str_detect(buyer_cvr_candidate, "[[:punct:]]"), FALSE),
    
    # Candidate had any of the above. NB: these flags only record what the raw
    # candidate contained - the removal itself happens at the extraction step
    # (extract_valid_cvr_candidates), not explicitly here. A TRUE flag whose
    # winner/buyer_cvr_clean is a clean 8-digit CVR means that character was removed.
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
    
    # Candidate had whitespace
    flag_cvr_ws = coalesce(str_detect(buyer_cvr_candidate, "\\s"), FALSE),
    
    # Candidate had alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(buyer_cvr_candidate, "[[:alpha:]]"), FALSE),
    
    # Candidate had punctuation
    flag_cvr_punct = coalesce(str_detect(buyer_cvr_candidate, "[[:punct:]]"), FALSE),
    
    # Candidate had any of the above. NB: these flags only record what the raw
    # candidate contained - the removal itself happens at the extraction step
    # (extract_valid_cvr_candidates), not explicitly here. A TRUE flag whose
    # winner/buyer_cvr_clean is a clean 8-digit CVR means that character was removed.
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

## 3.7 No tender-data join needed: the buyer table is seeded from the full `data`,
## so the tender/lot/amount fields already ride along with each cleaned row.

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
      !is.na(buyer_country) & trimws(buyer_country) != "" &
        toupper(trimws(buyer_country)) != "DK",
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

# Ensure natural row grains for each of winner/buyer side
# OpenTender expands every row across the buyer dimension, where
# a single award is repeated once per buyer
# winner = tender-lot-winner
# buyer = tender-lot-buyer
clean_winner_data <- clean_winner_data %>%
  distinct(tender_id, lot_id, winner_number, winner_cvr_clean, .keep_all = TRUE)
clean_buyer_data <- clean_buyer_data %>%
  distinct(tender_id, lot_id, buyer_number, buyer_cvr_clean, .keep_all = TRUE)

# Check that amount fields (incl. the EUR/DKK versions carried through the joins)
# are present in both saved OpenTender outputs.
required_amount_cols <- c("tender_amount", "lot_amount", "bid_amount",
                          "tender_amount_eur", "tender_amount_dkk",
                          "lot_amount_eur", "lot_amount_dkk")
stopifnot(all(required_amount_cols %in% names(clean_winner_data)))
stopifnot(all(required_amount_cols %in% names(clean_buyer_data)))

# 4 Save
saveRDS(clean_winner_data, file.path(dirs$clean_data, "clean_winner_data_ot.rds"))
saveRDS(clean_buyer_data, file.path(dirs$clean_data, "clean_buyer_data_ot.rds"))
