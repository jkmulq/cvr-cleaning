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

# Paths
raw_data_dir <- dirs$raw_data
raw_data_names <- list.files(file.path(raw_data_dir, "OpenTender"))

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))
