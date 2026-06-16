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
