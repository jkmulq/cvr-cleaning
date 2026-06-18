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

# Paths
raw_data_dir <- dirs$raw_data
raw_data_names <- list.files(file.path(raw_data_dir, "OpenTender"))
raw_data_paths <- file.path(raw_data_dir, "OpenTender", raw_data_names)

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# 1 Data
## 1.1 Check column-name concordance across yearly files
opentender_schema <- tibble(
  dataset = raw_data_names,
  path = raw_data_paths,
  year = str_extract(raw_data_names, "\\d{4}")
) %>%
  mutate(
    column_names = map(path, ~ names(read.csv(.x, sep = ";", nrows = 0, check.names = FALSE))),
    n_columns = map_int(column_names, length)
  )

column_presence <- opentender_schema %>%
  select(dataset, year, column_names) %>%
  unnest_longer(column_names, values_to = "column_name") %>%
  distinct(dataset, year, column_name) %>%
  mutate(present = TRUE)

column_concordance <- crossing(
  column_name = sort(unique(column_presence$column_name)),
  dataset = raw_data_names
) %>%
  left_join(column_presence, by = c("column_name", "dataset")) %>%
  mutate(present = replace_na(present, FALSE)) %>%
  select(column_name, dataset, present) %>%
  pivot_wider(names_from = dataset, values_from = present) %>%
  rowwise() %>%
  mutate(
    n_datasets = sum(c_across(all_of(raw_data_names))),
    present_in_all = n_datasets == length(raw_data_names),
    missing_from = paste(raw_data_names[!c_across(all_of(raw_data_names))], collapse = "; ")
  ) %>%
  ungroup() %>%
  arrange(present_in_all, desc(n_datasets), column_name)

column_concordance_summary <- column_concordance %>%
  summarise(
    n_datasets = length(raw_data_names),
    n_unique_columns = n(),
    n_columns_in_all_datasets = sum(present_in_all),
    n_columns_not_in_all_datasets = sum(!present_in_all)
  )

column_discordance <- column_concordance %>%
  filter(!present_in_all)

if (nrow(column_discordance) > 0) {
  stop("some yearly datasets contain different columns, inspect and try again.")
}

## 1.2 Load data
## Note, data is semi colon separated.
data_ls <- map(raw_data_paths, read.csv, sep = ";") %>% 
  setNames(raw_data_names)

data <- rbindlist(
  data_ls,
  use.names = TRUE,
  fill = FALSE,
  idcol = "file_name",
  ignore.attr = TRUE
)

data <- as_tibble(data)

# 2 Split into buyers/winners/tender
buyer_data <- data %>% 
  select(tender_id, buyer_bodyIds)
winner_data <- data %>% 
  select(tender_id, bidder_bodyIds, bidder_name)
tender_data <- data %>% 
  select(-bidder_bodyIds, -bidder_name, -buyer_bodyIds, contains("tender"), contains("[Dd]ate"))


