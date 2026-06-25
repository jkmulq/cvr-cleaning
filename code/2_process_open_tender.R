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

## 2.1 Investigate bidder ID delimiter for multiple CVR numbers/winning firms
### 2.1.1 Find delimiter types
winner_data <- winner_data %>%
  mutate(
    delim_flag_missing = is.na(winner_cvr) | winner_cvr == "",
    delim_flag_comma = coalesce(str_detect(winner_cvr, ","), FALSE),
    delim_flag_semicolon = coalesce(str_detect(winner_cvr, ";"), FALSE), 
    delim_flag_period = coalesce(str_detect(winner_cvr, fixed(".")), FALSE), 
    delim_flag_pipe = coalesce(str_detect(winner_cvr, fixed("|")), FALSE), 
    delim_flag_slash = coalesce(str_detect(winner_cvr, "/"), FALSE), 
    delim_flag_space = coalesce(str_detect(winner_cvr, "\\s"), FALSE),
    delim_flag_hyphen = coalesce(str_detect(winner_cvr, "-"), FALSE),
    delim_flag_no_punct = !str_detect(winner_cvr, "[[:punct:]]") & !delim_flag_missing,
    delim_flag_no_punct = coalesce(delim_flag_no_punct, FALSE),
    delim_flag_ampersand = coalesce(str_detect(winner_cvr, "&"), FALSE),
    delim_flag_colon = coalesce(str_detect(winner_cvr, ":"), FALSE),
    delim_flag_og = coalesce(str_detect(winner_cvr, "og"), FALSE)
  )

# Print summaries
winner_data %>% 
  summarise(across(.cols = starts_with("delim_flag_"), ~sum(.x, na.rm = TRUE)), 
            n = n()) %>% 
  mutate(row_sum = rowSums(.) - n) %>% 
  t() 

# Most of these potential delimiters don't separate CVR numbers
# 'period' is not valid
# Hyphens aren't (most come from Swedish bidder numbers)
# Space is not (usually separates a single CVR by 2 digits)

# The 1 '|' row represents a genuine delimiter, as well as all the commas.
# Flag these. 
winner_data <- winner_data %>%
  mutate(delim_flag_valid_comma = coalesce(delim_flag_comma, FALSE),
         delim_flag_valid_pipe = coalesce(delim_flag_pipe, FALSE))


# Sometimes '/' is for a name inside the bidder ID column, 
# other times it separates multiple bidders. 
# Flag likely valid ones for conversion to semi-colon
valid_slash_rows <- c(73374, 140635, 141894, 146029, 157184)
winner_data <- winner_data %>% 
  mutate(delim_flag_valid_slash = row_id %in% valid_slash_rows,
         flag_review_slash = coalesce(delim_flag_slash, FALSE) & !delim_flag_valid_slash) 

# Ampersand also represents valid delimiter sometimes too.
# Flag likely valid ones for conversion to semi-colon
valid_ampersand_rows <- c(62215, 65494, 148062)
winner_data <- winner_data %>% 
  mutate(delim_flag_valid_ampersand = row_id %in% valid_ampersand_rows,
         flag_review_ampersand = coalesce(delim_flag_ampersand, FALSE) & !delim_flag_valid_ampersand)

# 'og' (meaning 'and') sometime have multiple CVR numbers too
# Flag likely valid ones for conversion to semi-colon
valid_og_rows <- c(59588, 78505, 105116, 144636, 146512, 156134)
winner_data <- winner_data %>% 
  mutate(delim_flag_valid_og = row_id %in% valid_og_rows,
         flag_review_og = coalesce(delim_flag_og, FALSE) & !delim_flag_valid_og)

# Check manually accepted row_id's still present in the data
missing_valid_delim_rows <- setdiff(
  c(valid_slash_rows, valid_ampersand_rows, valid_og_rows),
  winner_data$row_id
)

if (length(missing_valid_delim_rows) > 0) {
  print(missing_valid_delim_rows)
  stop("Some manually reviewed delimiter row IDs are not present in winner_data.")
}

# Check accepted row_id's have expected delimiter
invalid_valid_delim_rows <- winner_data %>%
  filter(
    (delim_flag_valid_slash & !delim_flag_slash) |
      (delim_flag_valid_ampersand & !delim_flag_ampersand) |
      (delim_flag_valid_og & !delim_flag_og)
  ) %>%
  select(row_id, winner_cvr)

if (nrow(invalid_valid_delim_rows) > 0) {
  print(invalid_valid_delim_rows)
  stop("Some manually reviewed delimiter row IDs no longer have the expected delimiter.")
}

# Flag all failures for manual review 
winner_data <- winner_data %>% 
  mutate(flag_manual_review = flag_review_slash | flag_review_ampersand  | flag_review_og,
         manual_review_reason = ifelse(flag_review_slash | flag_review_ampersand | flag_review_og, 
                                       "check whether bidder ID contains multiple winning firms",
                                       NA))

# Convert valid delims to semi-colon
winner_data <- winner_data %>%
  mutate(
    winner_cvr = if_else(delim_flag_valid_comma, str_replace_all(winner_cvr, fixed(","), ";"), winner_cvr),
    winner_cvr = if_else(delim_flag_valid_pipe, str_replace_all(winner_cvr, fixed("|"), ";"), winner_cvr),
    winner_cvr = if_else(delim_flag_valid_slash, str_replace_all(winner_cvr, fixed("/"), ";"), winner_cvr),
    winner_cvr = if_else(delim_flag_valid_ampersand, str_replace_all(winner_cvr, fixed("&"), ";"), winner_cvr),
    winner_cvr = if_else(delim_flag_valid_og, str_replace_all(winner_cvr, "og", ";"), winner_cvr)
  )

# Flag likely multi-value winner ID strings.
# `flag_multi_winner` means the source string has an accepted
# multi-value delimiter. It does not yet mean the row has multiple distinct
# cleaned CVRs, because some delimited values repeat the same identifier.
# Likewise, `single_cvr` means non-empty and not accepted-multi at this stage,
# not yet a valid eight-digit Danish CVR.
winner_data <- winner_data %>% 
  mutate(flag_multi_winner = coalesce(delim_flag_valid_comma | delim_flag_valid_pipe | 
                                        delim_flag_valid_slash | delim_flag_valid_ampersand |
                                        delim_flag_valid_og, FALSE),
         single_cvr = coalesce(!is.na(winner_cvr) & winner_cvr != "" & !flag_multi_winner, FALSE))

# Flag rows with several distinct valid CVRs (can use ; as delimiter since we converted them above)
winner_data <- winner_data %>%
  mutate(
    flag_multiple_distinct_valid_cvrs = has_multiple_distinct_valid_cvrs(winner_cvr, delim = ";")
  )

# Print number of true multi CVR numbers.
n_true_multi_cvrs <- sum(winner_data$flag_multiple_distinct_valid_cvrs, na.rm = TRUE)
cat("Number of true multi CVR numbers detected: ", n_true_multi_cvrs, "\n")

# Since the number is small, manually reviewed row IDs confirm cases where the
# multiple distinct valid CVR flag corresponds to multiple firms in winner_name.
multiple_distinct_winner_names_row_ids <- c(
  157184, 156134, 148062, 146512, 146029, 144636, 141894, 140635,
  138833, 105116, 78505, 73374, 65494, 62215, 59588
)

winner_data <- mutate(
  winner_data,
  flag_multiple_distinct_winner_names = coalesce(row_id %in% multiple_distinct_winner_names_row_ids, FALSE)
)

# Check manually reviewed row IDs still exist and still have multiple distinct
# valid CVRs. If either check fails, the manual coding needs to be revisited.
missing_multiple_distinct_name_rows <- setdiff(
  multiple_distinct_winner_names_row_ids,
  winner_data$row_id
)

if (length(missing_multiple_distinct_name_rows) > 0) {
  print(missing_multiple_distinct_name_rows)
  stop("Some manually reviewed multiple-winner row IDs are not present in winner_data.")
}

invalid_multiple_distinct_name_rows <- winner_data %>%
  filter(flag_multiple_distinct_winner_names & !flag_multiple_distinct_valid_cvrs) %>%
  select(row_id, winner_cvr, winner_name)

if (nrow(invalid_multiple_distinct_name_rows) > 0) {
  print(invalid_multiple_distinct_name_rows)
  stop("Some manually reviewed multiple-winner rows are no longer flagged as multiple distinct valid CVRs.")
}

## 2.2 Check reviewed multiple-winner names
# The automatic CVR flag can find multiple distinct valid CVRs even when the
# winner name is a single firm. Keep the final multiple-winner-name flag tied to
# the manually reviewed row IDs above, then audit the inputs needed for that
# decision.
reviewed_multiple_winner_names <- winner_data %>%
  filter(flag_multiple_distinct_winner_names) %>%
  select(row_id, winner_cvr, winner_name, winner_country)

missing_reviewed_winner_names <- reviewed_multiple_winner_names %>%
  filter(is.na(winner_name) | winner_name == "")

if (nrow(missing_reviewed_winner_names) > 0) {
  print(missing_reviewed_winner_names)
  stop("Some manually reviewed multiple-winner rows have missing winner names.")
}

unconfirmed_multiple_cvr_rows <- winner_data %>%
  filter(flag_multiple_distinct_valid_cvrs & !flag_multiple_distinct_winner_names) %>%
  select(row_id, winner_cvr, winner_name, winner_country)

cat("Number of manually confirmed multiple-winner name rows: ",
    nrow(reviewed_multiple_winner_names), "\n")
cat("Number of multiple-distinct-CVR rows not confirmed as multiple winners: ",
    nrow(unconfirmed_multiple_cvr_rows), "\n")

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

## Clean up/standardise CVR numbers
## Keep the separated raw CVR candidate before cleaning. Cleaning flags treat
## NAs as FALSE: a missing source value is not counted as evidence that a
## cleaning operation was performed.
multi_winner_names_data_long <- multi_winner_names_data_long %>%
  mutate(
    winner_cvr_candidate_original = winner_cvr,
    winner_cvr_clean = ifelse(
      winner_cvr_candidate_original == "CVR5EByg:30811097",
      "30811097",
      winner_cvr_candidate_original
    ),

    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate_original, "\\s"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),

    # Remove hyphens
    flag_cvr_hyphen = coalesce(str_detect(winner_cvr_candidate_original, "-"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "-"),

    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate_original, "[[:alpha:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),

    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
    winner_cvr_clean = as.character(parse_number(winner_cvr_clean)),

    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws |
        flag_cvr_hyphen |
        flag_cvr_alphabet |
        flag_cvr_punct,
      FALSE
    )
  )

## Add metadata
multi_winner_names_data_long <- multi_winner_names_data_long %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_clean),
         winner_number = row_number(),
         source = "multiple confirmed winner names",
         .by = c(row_id, tender_id))

## 2.5 Pivot multiple distinct CVRs to long
# This section focuses on multiple distinct CVRs with only one identifiable firm name
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data %>% 
  separate_longer_delim(cols = "winner_cvr", delim = ";")

## Clean up/standardise CVR numbers
## Keep the separated raw CVR candidate before cleaning. Cleaning flags treat
## NAs as FALSE: a missing source value is not counted as evidence that a
## cleaning operation was performed.
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  mutate(
    winner_cvr_candidate_original = winner_cvr,
    winner_cvr_clean = winner_cvr_candidate_original,

    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate_original, "\\s"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),

    # Remove hyphens
    flag_cvr_hyphen = coalesce(str_detect(winner_cvr_candidate_original, "-"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "-"),

    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate_original, "[[:alpha:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),

    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
    winner_cvr_clean = as.character(parse_number(winner_cvr_clean)),

    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws |
        flag_cvr_hyphen |
        flag_cvr_alphabet |
        flag_cvr_punct,
      FALSE
    )
  )

# Flag valid CVR string post cleaning (8 numerical digits)
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

# Make distinct by (row_id, tender_id, winner_cvr_clean, winner_name)
## Since a lot of these rows come from records with multiple instances of the same CVR number
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  distinct(tender_id, row_id, winner_name, winner_cvr_clean, valid_cvr, 
           .keep_all = TRUE)

### 2.4.2 Fix erroneous CVR cites across firm
# Many bidder names have multiple CVR numbers, some are not valid
# Make a key and join each instance of a firm with the valid CVR
# I only focus on firms with ONE valid CVR but more than one entry in the CVR
valid_invalid_cvr_winner_key <- multi_cvr_nondistinct_names_data_long %>%
  distinct(winner_name, winner_cvr_clean, valid_cvr) %>%
  mutate(n_valid_cvr = sum(valid_cvr), 
         n_total_cvr = n(),
         .by = winner_name) 

single_valid_cvr_key <- valid_invalid_cvr_winner_key %>% 
  filter(n_valid_cvr == 1, n_total_cvr > 1, valid_cvr) %>% 
  rename(winner_cvr_real = winner_cvr) %>% 
  select(-valid_cvr, -n_valid_cvr, n_total_cvr) %>% 
  distinct()

# Join key
multi_cvr_nondistinct_names_data_long <- left_join(multi_cvr_nondistinct_names_data_long, 
                              single_valid_cvr_key, 
                              by = c("winner_name"))

# Assume the single valid CVR is the true CVR for this firm name.
# This does not create separate rows for invalid or non-CVR tokens; the raw
# OpenTender source string joined later remains the audit trail.
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(
    flag_assumed_single_valid_cvr = coalesce(
      !is.na(winner_cvr_clean_real) & (is.na(winner_cvr_clean) | winner_cvr_clean_real != winner_cvr_clean),
      FALSE
    ),
    winner_cvr_clean = ifelse(flag_assumed_single_valid_cvr, winner_cvr_clean_real, winner_cvr_clean)
  )

# Update valid CVR flag
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

# Collapse rows that become duplicates after the single-valid-CVR assumption.
# The full source string is preserved in `winner_cvr_original` after joining the
# original OpenTender row below.
# Note, use summarise() instead of distinct() to control how flags are treated
# after removing the non-distinct rows. 
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  mutate(
    flag_assumed_single_valid_cvr = coalesce(flag_assumed_single_valid_cvr, FALSE),
    valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE)
  ) %>%
  distinct(
    tender_id, row_id, winner_name, winner_cvr_clean, valid_cvr, .keep_all = TRUE
  )

# Other firms have many valid CVRs and many invalid CVRs. 
# Flag them, but I keep all their rows.
multi_valid_cvr_firms <- valid_invalid_cvr_winner_key %>% 
  filter(n_valid_cvr > 1) %>% 
  distinct(winner_name, n_valid_cvr, n_total_cvr) 

cat("Number of winning firms with several valid CVRs:", nrow(multi_valid_cvr_firms), "\n")
cat("Ave. number of valid CVRs for these firms:", mean(multi_valid_cvr_firms$n_valid_cvr), "\n")
cat("Ave. number of total CVRs (valid + invalid) for these firms:", mean(multi_valid_cvr_firms$n_total_cvr), "\n")

multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>% 
  mutate(flag_multi_valid_cvr = coalesce(if_else(winner_name %in% multi_valid_cvr_firms$winner_name, 
                                                 TRUE, FALSE), FALSE))

## Add metadata
multi_cvr_nondistinct_names_data_long <- multi_cvr_nondistinct_names_data_long %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_clean),
         source = "multiple CVR candidates for one winner name",
         winner_number = row_number(),
         .by = c(row_id, tender_id))

## 2.6 Clean single CVR data
## Adding the CVR standardisation flags
single_winner_data <- single_winner_data %>%
  mutate(winner_cvr = as.character(winner_cvr),
         winner_cvr_candidate_original = winner_cvr,
         winner_cvr_clean = winner_cvr_candidate_original,
         
         # Remove white space
         flag_cvr_ws = coalesce(str_detect(winner_cvr_candidate_original, "\\s"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),
         
         # Remove hyphens
         flag_cvr_hyphen = coalesce(str_detect(winner_cvr_candidate_original, "-"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "-"),
         
         # Remove alphabetical letters
         flag_cvr_alphabet = coalesce(str_detect(winner_cvr_candidate_original, "[[:alpha:]]"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),
         
         # Remove all punctuation
         flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
         winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
         winner_cvr_clean = as.character(parse_number(winner_cvr_clean)),
         
         # Flag if any standardisation performed
         flag_cvr_standardised = coalesce(
           flag_cvr_ws |
             flag_cvr_hyphen |
             flag_cvr_alphabet |
             flag_cvr_punct,
           FALSE
         ),
         winner_number = 1,
         source = "single winner")

## 2.7 Bind winner data
## Goal: Create one OpenTender winner table with cleaned CVR candidates and
## keep the OpenTender-specific review flags created above.

clean_winner_data <- bind_rows(
  single_winner_data,
  multi_winner_names_data_long,
  multi_cvr_nondistinct_names_data_long
) %>%
  arrange(row_id, winner_number) %>%
  select(row_id, tender_id, winner_number, winner_cvr, winner_name, winner_country, source, everything())

clean_winner_data <- clean_winner_data %>%
  rename(winner_cvr_candidate_original = winner_cvr)

## 2.7 Join original tender data
## This keeps the full OpenTender row attached to the cleaned winner rows, so
## replication checks can always go back to the source fields.
clean_winner_data <- left_join(clean_winner_data, original_tender_data,
                               by = c("row_id", "tender_id"))

clean_winner_data <- clean_winner_data %>%
  mutate(winner_cvr_clean = as.character(winner_cvr_candidate_original))

## The single valid CVR overwriting flags and the multi valid CVR flags
## only exist in the multiple CVR subdatasets. 
## Treat these flags values as FALSE in the final cleaned table for completeness
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_assumed_single_valid_cvr = coalesce(flag_assumed_single_valid_cvr, FALSE),
    flag_multi_valid_cvr = coalesce(flag_multi_valid_cvr, FALSE)
  )
