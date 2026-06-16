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





# 2 Winners
# Function to check for extract multiple CVRs into one column
extract_multiple_cvr <- function(data, row_id, cvr_cols = "winner_cvr") {
  
  # For semi-colon
  data %>% 
    select(all_of(cvr_cols)) %>% 
    separate_wider_delim(col = cvr_cols, 
                         names_sep = "_", 
                         delim = regex("[.,;]"),
                         cols_remove = FALSE)
}

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



