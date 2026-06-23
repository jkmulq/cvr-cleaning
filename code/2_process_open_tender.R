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

## 1.3 Separate winners/buyers/original data
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

## 2.2 Separate rows
winner_data_long <- separate_longer_delim(winner_data, cols = "winner_cvr", delim = ";")

## 2.3 Clean up CVRs
### 2.3.1 Basic cleaning
# Remove spaces
winner_data_long <- winner_data_long %>% 
  mutate(winner_cvr = gsub(" ", "", winner_cvr))

# Remove bidder_country prefix if present
# e.g. bidder_country = "DK" and bidder_bodyIds = "DK12345678"
# or bidder_country = "SE" and bidder_bodyIds = "SE123456789"
winner_data_long <- winner_data_long %>% 
  mutate(winner_cvr = if_else(str_sub(winner_cvr, start = 1, end = 2) == winner_country,
                              substring(winner_cvr, first = 3),
                              winner_cvr))

# Remove starting string:
# 'CVR-nr:', 'CVR-nr::', 'CVR-nr.:', 'CVR-nummer', 'CVR-nr.'
# 'CVRnr.', 	'CVR.nr.', 'CVR.:', 'CVR'
winner_data_long <- winner_data_long %>% 
  mutate(winner_cvr = gsub("CVR-nr:", "", winner_cvr),
         winner_cvr = gsub("CVR-nr::", "", winner_cvr),
         winner_cvr = gsub("CVR-nr.:", "", winner_cvr),
         winner_cvr = gsub("CVR-nummer", "", winner_cvr),
         winner_cvr = gsub("CVR-nr.", "", winner_cvr),
         winner_cvr = gsub("CVRnr.", "", winner_cvr),
         winner_cvr = gsub("CVR.nr.", "", winner_cvr),
         winner_cvr = gsub("CVR.:", "", winner_cvr),
         winner_cvr = gsub("CVR", "", winner_cvr),
         winner_cvr = gsub("CVR(VATno.)", "", winner_cvr),
         winner_cvr = gsub("Cvr.nr.", "", winner_cvr),
         winner_cvr = gsub("Cvr-nr.", "", winner_cvr),
         winner_cvr = gsub("Cvr:", "", winner_cvr)) 

# Check all 'CVR' or any variation on the capitalisation strings no longer present
cvr_string_check <- winner_data_long %>% 
  filter(str_detect(winner_cvr, regex("cvr", ignore_case = TRUE))) %>% 
  nrow()

if (cvr_string_check > 0) {
  stop("some cvr numbers still contain some variation of the letters 'cvr' in the string.")
}

# Flag valid CVR string post cleaning (8 numerical digits)
winner_data_long <- winner_data_long %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr, "^\\d{8}$"), FALSE))

### 2.3.2 Fix erroneous CVR cites across firm
# Many bidder names have multiple CVR numbers, some are not valid
# Make a key and join each instance of a firm with the valid CVR
# I only focus on firms with ONE valid CVR but more than one entry in the CVR
valid_invalid_cvr_winner_key <- winner_data_long %>% 
  distinct(winner_name, winner_cvr, valid_cvr) %>% 
  mutate(n_valid_cvr = sum(valid_cvr), 
         n_total_cvr = n(),
         .by = winner_name) 

single_valid_cvr_key <- valid_invalid_cvr_winner_key %>% 
  filter(n_valid_cvr == 1, n_total_cvr > 1, valid_cvr) %>% 
  rename(winner_cvr_real = winner_cvr) %>% 
  select(-valid_cvr, -n_valid_cvr, n_total_cvr)

# Join key
winner_data_long <- left_join(winner_data_long, 
                              single_valid_cvr_key, 
                              by = c("winner_name"))

# Overwrite erroneous CVRs
winner_data_long <- winner_data_long %>% 
  mutate(winner_cvr_original = winner_cvr) %>% 
  mutate(flag_cvr_overwrite = coalesce(!is.na(winner_cvr_real) & winner_cvr_real != winner_cvr, FALSE),
         winner_cvr = ifelse(!is.na(winner_cvr_real) & winner_cvr_real != winner_cvr,
                             winner_cvr_real, winner_cvr)) 

