# Match missing KFST winner CVRs using registered CVR names
# Author: Jack Mulqueeney
# Date: 30 June 2026

rm(list = ls())

# Load config
library(here)
source(here::here("config.R"))

# Packages
library(data.table)

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Directories
clean_data_dir <- dirs$clean_data

# 1. Load the KFST winners and the CVR-name keys

winner_data <- readRDS(
  file.path(clean_data_dir, "clean_winner_data_kfst.rds")
)
name_key <- readRDS(
  file.path(clean_data_dir, "clean_cvr_name_key.rds")
)
biname_key <- readRDS(
  file.path(clean_data_dir, "clean_cvr_biname_key.rds")
)

setDT(winner_data)
setDT(name_key)
setDT(biname_key)

## 1.1 Concord column names across keys
setnames(name_key, "name", "registered_name")
setnames(biname_key, "binavn", "registered_name")
