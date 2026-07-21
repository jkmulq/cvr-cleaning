# Build CVR-name lookup files from the Virk system-to-system API.
# This script is optional: the standard replication workflow can use supplied
# lookup CSVs instead.

rm(list = ls())

source("config.R")

suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
}))

source(file.path(PROJECT_DIR, "code", "functions.R"))

sample_size <- Sys.getenv("CVR_LOOKUP_SAMPLE_SIZE")
batch_size <- as.integer(Sys.getenv("CVR_LOOKUP_BATCH_SIZE", "1000"))
overwrite <- tolower(Sys.getenv("CVR_LOOKUP_OVERWRITE", "false")) == "true"
output_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

names_file <- Sys.getenv("CVR_LOOKUP_NAMES_FILE")
binavne_file <- Sys.getenv("CVR_LOOKUP_BINAVNE_FILE")

if (!nzchar(names_file)) {
  names_file <- if (overwrite) {
    "cvr_names_full.csv"
  } else {
    paste0("cvr_names_virk_", output_stamp, ".csv")
  }
}

if (!nzchar(binavne_file)) {
  binavne_file <- if (overwrite) {
    "cvr_binavne_full.csv"
  } else {
    paste0("cvr_binavne_virk_", output_stamp, ".csv")
  }
}

if (is.na(batch_size) || batch_size < 1L) {
  stop("CVR_LOOKUP_BATCH_SIZE must be a positive integer.", call. = FALSE)
}

if (nzchar(sample_size)) {
  sample_size <- as.integer(sample_size)

  if (is.na(sample_size) || sample_size < 1L) {
    stop("CVR_LOOKUP_SAMPLE_SIZE must be a positive integer.", call. = FALSE)
  }

  cat("Testing Virk CVR lookup extraction on", sample_size, "firms\n")
  sample_result <- test_cvr_lookup_sample(
    n = sample_size,
    out_dir = dirs$cvr_key
  )
  print(sample_result)
  quit(save = "no")
}

cat("Building full CVR lookup files from Virk API\n")
cat("Output directory:", dirs$cvr_key, "\n")
cat("Batch size:", batch_size, "\n")
cat("Overwrite canonical lookup files:", overwrite, "\n")
cat("Names file:", names_file, "\n")
cat("Alternative names file:", binavne_file, "\n")

result <- generate_cvr_lookup_from_virk(
  out_dir = dirs$cvr_key,
  batch_size = batch_size,
  names_file = names_file,
  binavne_file = binavne_file,
  overwrite = overwrite
)

print(result)
