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
data_dir           <- dirs$raw
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
         tender_id = `Løbenummer`,
         lot_id = `Nummerplade`,
         lot_number = `Delkontraktnr.`,
         n_lots = `Antal delkontrakter kortlagt`,
         n_lots_contracted = `Antal delkontrakter i udbudsbekendtgørelsen`,)

# Order columns nicely
data <- data %>% 
  select(tender_id, lot_id, lot_number, n_lots, n_lots_contracted,
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
extract_multiple_cvr <- function(data, row_id, cvr_cols = c("winner_cvr")) {
  
  # Extract row
  data_row <- data[row_id, ]
  
  # Base output (keep originals)
  out <- data_row %>%
    select(tender_id, lot_id, all_of(cvr_cols))
  
  # Prepare to split cvr_cols
  out_list <- list()
  for (col in cvr_cols) {
    
    # Select column, and skip if empty
    x <- data_row[[col]]
    if (is.na(x)) next
    
    # Split by delimiters
    parts <- strsplit(x, "[.,;]")[[1]]
    for (i in seq_along(parts)) {
      out_list[[paste0(col, "_", i)]] <- parts[i]
    }
  }
  
  if (length(out_list) == 0) {
    return(out)
  } else {
    bind_cols(out, as_tibble(out_list))
  }
  
}

# Map function over rows and bind.
cvr_cols_to_sep <- c("winner_cvr", "winner_name", "winner_country")
multi_data_sep_new <- map(1:nrow(multi_data), extract_multiple_cvr, 
    data = multi_data,
    cvr_cols = cvr_cols_to_sep) %>% 
  bind_rows()

# Order columns
column_pattern <- paste0(cvr_cols_to_sep, collapse = "|")
column_pattern <- paste0("(", column_pattern, ")_(\\d+)$")
multi_data_sep <- multi_data_sep %>% 
  select(tender_id, lot_id, matches(column_pattern))

# Pivot longer
long_data <- multi_data_sep %>%
  pivot_longer(
    cols = matches("^winner_(cvr|name|country)_\\d+$"),
    names_to = c(".value", "winner_id"),
    names_pattern = "winner_(cvr|name|country)_(\\d+)"
  )