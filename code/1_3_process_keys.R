# Process CVR to name keys 
# Uses data.table since data is larger
# Author: Jack Mulqueeney
# Date: 30 June 2026

# Clean environment
rm(list = ls())

# Config: run from the project root or use run_replication.sh.
source("config.R")

# Packages
suppressWarnings(suppressPackageStartupMessages({
  library(haven)
  library(readxl)
  library(data.table)
}))

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Paths
cvr_key_dir <- dirs$cvr_key
clean_data_dir <- dirs$clean_data

# 1 Process main CVR names
key_data <- data.table::fread(file.path(cvr_key_dir, "cvr_names_full.csv"),
                              encoding = "Latin-1")

key_names_prepared <- prepare_cvr_name(key_data$name)
setDT(key_names_prepared)

# Keep one lookup row per original name so repeated names in the CVR key 
# do not create a many-to-many join.
key_names_prepared <- unique(key_names_prepared, by = "name_original")

# Update join
n_key_rows <- nrow(key_data)
key_data[
  key_names_prepared,
  on = .(name = name_original),
  `:=`(
    name_basic = i.name_basic,
    name_match = i.name_clean,
    name_no_spaces = i.name_no_spaces,
    name_broad = i.name_broad,
    firm_type = i.firm_type,
    first_letter = i.first_letter
  )
]

# Check equivalent row dimension
stopifnot(nrow(key_data) == n_key_rows)

saveRDS(
  key_data,
  file.path(clean_data_dir, "clean_cvr_name_key.rds")
)

# Clear memory
rm(key_names_prepared, key_data)
gc()

# 2 Process alternative CVR names
alt_name_data <- data.table::fread(
  file.path(cvr_key_dir, "cvr_binavne_full.csv"),
  encoding = "Latin-1"
)

# Prepare names and make distinct (like above)
binavn_prepared <- prepare_cvr_name(alt_name_data$binavn)
setDT(binavn_prepared)
binavn_prepared <- unique(binavn_prepared, by = "name_original")

# Update join 
n_alt_name_rows <- nrow(alt_name_data)
alt_name_data[
  binavn_prepared,
  on = .(binavn = name_original),
  `:=`(
    name_basic = i.name_basic,
    name_match = i.name_clean,
    name_no_spaces = i.name_no_spaces,
    name_broad = i.name_broad,
    firm_type = i.firm_type,
    first_letter = i.first_letter
  )
]

# Check equivalent row dimension
stopifnot(nrow(alt_name_data) == n_alt_name_rows)

saveRDS(
  alt_name_data,
  file.path(clean_data_dir, "clean_cvr_biname_key.rds")
)
