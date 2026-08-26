#!/usr/bin/env Rscript
# Quick viewer for the pipeline's .rds outputs.
#   Rscript code/view_data.R                 # list every .rds in data/clean with dims
#   Rscript code/view_data.R kfst_name       # glimpse + export CSV for files matching "kfst_name"
#   Rscript code/view_data.R ot_name 500     # ... but cap the CSV at the first 500 rows
# CSVs land in data/clean/csv_view/ -- open them in the IDE.
# (In an interactive R session you can also just: View(readRDS("data/clean/<file>.rds")))

suppressWarnings(suppressPackageStartupMessages(library(data.table)))
source("config.R")

clean_dir <- dirs$clean_data
args      <- commandArgs(trailingOnly = TRUE)
rds_files <- list.files(clean_dir, pattern = "\\.rds$")

# No argument -> list what's available (by size; reading every file just for dims is slow).
if (length(args) == 0) {
  cat("Data files in", clean_dir, "\n\n")
  fi <- file.info(file.path(clean_dir, rds_files))
  info <- data.table(file = rds_files, size_mb = round(fi$size / 1e6, 1))
  print(info[order(-size_mb)])
  cat("\nView one:  Rscript code/view_data.R <part-of-filename> [max_rows]\n")
  quit(save = "no")
}

pattern <- args[1]
n_max   <- if (length(args) >= 2) suppressWarnings(as.integer(args[2])) else NA_integer_
matches <- rds_files[grepl(pattern, rds_files, ignore.case = TRUE)]

if (length(matches) == 0) {
  cat("Nothing matches", shQuote(pattern), "in", clean_dir, "\nAvailable:\n")
  cat(paste(" -", rds_files), sep = "\n"); cat("\n")
  quit(save = "no")
}

csv_dir <- file.path(clean_dir, "csv_view")
dir.create(csv_dir, showWarnings = FALSE)

for (f in matches) {
  d <- as.data.table(readRDS(file.path(clean_dir, f)))
  cat("\n================================================================\n")
  cat(f, "--", nrow(d), "rows x", ncol(d), "cols\n")
  cat("================================================================\n")
  print(data.table(col = names(d), type = vapply(d, function(x) class(x)[1], "")))
  cat("\nFirst rows:\n"); print(head(d, 10))

  # Flatten any list-columns so fwrite won't choke.
  lc <- names(d)[vapply(d, is.list, logical(1))]
  if (length(lc)) d[, (lc) := lapply(.SD, function(x)
    vapply(x, function(v) paste(unlist(v), collapse = ";"), character(1))), .SDcols = lc]

  out <- file.path(csv_dir, sub("\\.rds$", ".csv", f))
  fwrite(if (is.na(n_max)) d else head(d, n_max), out)
  cat("\n-> CSV:", out, if (!is.na(n_max)) sprintf("(first %d rows)", n_max) else "(all rows)", "\n")
}
