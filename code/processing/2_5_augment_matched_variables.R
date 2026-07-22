# Augment the matched datasets with new tender/lot-level variables WITHOUT
# re-running the (~10 hour) name-matching scripts.
#
# The tender/lot-level variables (awarded flag, award/framework end dates,
# contract duration, annualised amounts, ...) are produced by the fast
# processing scripts (1_1, 1_2); the matching step never changes them. This
# script joins them from the freshly-regenerated clean data onto the existing
# *_name_matched.rds files by a stable row key, in place.
#
# Workflow:
#   1. Regenerate the clean data with the new columns (minutes) — run
#      code/processing/1_1_process_kfst.R and 1_2_process_open_tender.R, or
#      `RUN_MATCHING=false ./run_replication.sh` (which runs only those two).
#   2. Run this script to refresh the matched files.
#
# It is add-only and idempotent (re-running refreshes the columns) and never
# touches the matching results. VALID ONLY while the processing changes are
# additive (new columns; same keys and existing values). If cleaning logic that
# feeds matching changes (names, CVRs, row expansion), re-run the matching.
#
# Deliberately NOT part of run_replication.sh: that script is for end-to-end
# replication; this is a maintenance utility run on demand.

rm(list = ls())

source("config.R")

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
}))

# ── What to sync ─────────────────────────────────────────────────────────────
# Tender/lot-level variables to (re)attach to the matched data. Add future
# tender-level variables here — each source only takes the ones it actually has.
vars <- c(
  "flag_awarded",
  "award_end_date",                 # KFST
  "framework_end_date",             # OpenTender
  "framework_start_anchor",         # OpenTender
  "framework_duration_days",        # OpenTender
  "contract_duration_months_min",   # KFST
  "contract_duration_months_max",   # KFST
  "annualised_tender_amount",
  "annualised_lot_amount"
)

# Each matched file, its clean-data source, and the row key that links them.
specs <- list(
  list(clean = "clean_winner_data_kfst.rds",
       matched = "clean_winner_data_kfst_name_matched.rds",
       key = c("tender_id", "lot_id", "winner_number")),
  list(clean = "clean_buyer_data_kfst.rds",
       matched = "clean_buyer_data_kfst_name_matched.rds",
       key = c("tender_id", "lot_id", "buyer_number")),
  list(clean = "clean_winner_data_ot.rds",
       matched = "clean_winner_data_ot_name_matched.rds",
       key = c("row_id", "winner_number")),
  list(clean = "clean_buyer_data_ot.rds",
       matched = "clean_buyer_data_ot_name_matched.rds",
       key = c("row_id", "buyer_number"))
)

# ── Augment one matched file in place ────────────────────────────────────────
augment_matched <- function(spec, vars) {
  clean_path   <- file.path(dirs$clean_data, spec$clean)
  matched_path <- file.path(dirs$clean_data, spec$matched)

  clean   <- readRDS(clean_path)
  matched <- readRDS(matched_path)

  add <- intersect(vars, names(clean))
  if (length(add) == 0) {
    message(sprintf("%s: none of the target variables are in %s - skipped. Re-run 1_1/1_2 first?",
                    spec$matched, spec$clean))
    return(invisible(NULL))
  }

  # The key must exist on both sides and must uniquely map the new variables.
  stopifnot(all(spec$key %in% names(clean)), all(spec$key %in% names(matched)))
  lookup <- clean %>%
    select(all_of(c(spec$key, add))) %>%
    distinct()
  if (anyDuplicated(lookup[spec$key]) > 0) {
    stop(sprintf("%s: key {%s} does not uniquely determine %s.",
                 spec$clean, paste(spec$key, collapse = ", "),
                 paste(add, collapse = ", ")))
  }

  # Add-only + idempotent: drop any stale copies, then attach the fresh values.
  n_before <- nrow(matched)
  matched <- matched %>%
    select(-any_of(add)) %>%
    left_join(lookup, by = spec$key)
  stopifnot(nrow(matched) == n_before)   # a keyed lookup must not multiply rows

  saveRDS(matched, matched_path)
  message(sprintf("%s: attached [%s] to %d rows.",
                  spec$matched, paste(add, collapse = ", "), nrow(matched)))
}

invisible(lapply(specs, augment_matched, vars = vars))
message("Done. Matched datasets refreshed with tender/lot-level variables.")
