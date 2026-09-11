# =============================================================================
# code/processing/98_final_data_checks.R
# Post-combine sanity checks on clean_all_samples_combined:
#   (1) its columns match the variable-key "Final key (preview)" sheet exactly;
#   (2) missingness of every variable, per (data_source, entity) pair;
#   (3) tender/lot-level columns agree across entities within each (data_source, tender_id, lot_id);
#   (4) the sample-selection rules (build_prod / build_extr) reproduce the PRE-DEDUP production /
#       extraction datasets (vs the references saved by 3_1/3_2/3_3) -- i.e. the dedup is lossless.
# Standalone check -- not part of the production pipeline.
# =============================================================================
rm(list = ls())
source("config.R")
source(file.path(PROJECT_DIR, "code", "functions.R"))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

# Load data + the variable-key sheet.
# read_clean() prefers the parquet, so column types (and leading-zero CVRs) are preserved and the
# result is a data.table -- unlike read.csv(), which returns a data.frame and would break the
# data.table syntax below and coerce CVRs to numbers.
final_data <- read_clean(file.path(dirs$clean_data, "clean_all_samples_combined"))
var_key <- readxl::read_excel(file.path(dirs$data, "variable_key_expanded.xlsx"),
                              sheet = "Final key (preview)")

# Run all four checks, collecting any failures, then stop at the end if there were any -- so one run
# reports every check rather than aborting at the first failure.
failures <- character(0)

# 1 Check the columns in the variable key equal the columns in the data.
var_list_key  <- var_key$`Variable name`
var_list_data <- names(final_data)
if (setequal(var_list_key, var_list_data)) {
  message("Passed: variable list in combined dataset and variable key are equal!")
} else {
  message("  in key, not in data: ", paste(setdiff(var_list_key,  var_list_data), collapse = ", "))
  message("  in data, not in key: ", paste(setdiff(var_list_data, var_list_key),  collapse = ", "))
  message("Failed (1): variable list in combined dataset and variable key are NOT equal.")
  failures <- c(failures, "1: variable list mismatch")
}

# 2 Missingness per (data_source, entity) pair, for every variable.
#   Missing = NA, or (for character columns) blank after trimming.
n_miss <- function(x) if (is.character(x)) sum(is.na(x) | trimws(x) == "") else sum(is.na(x))
value_cols <- setdiff(var_list_data, c("data_source", "entity"))   # the by-cols are trivially complete

grp_n        <- final_data[, .(n_rows = .N), by = .(data_source, entity)]
missingness  <- final_data[, lapply(.SD, n_miss), by = .(data_source, entity), .SDcols = value_cols]
missingness  <- melt(missingness, id.vars = c("data_source", "entity"),
                     variable.name = "variable", value.name = "n_missing", variable.factor = FALSE)
missingness  <- grp_n[missingness, on = c("data_source", "entity")]   # attach each pair's row count
missingness[, pct_missing := round(100 * n_missing / n_rows, 1)]
setorder(missingness, data_source, entity, variable)

out_csv <- file.path(dirs$clean_data, "clean_all_samples_combined_missingness_by_source_entity.csv")
fwrite(missingness, out_csv)
cat(sprintf("Missingness: %d rows (%d variables x %d source-entity pairs) -> %s\n",
            nrow(missingness), uniqueN(missingness$variable), nrow(grp_n), out_csv))
cat("Rows per (data_source, entity):\n"); print(grp_n[order(data_source, entity)])

# 3 Tender/lot-level agreement across entities.
#   The winner, buyer (and TED non-winner) rows of one lot all inherit the tender/lot context, so every
#   tender-level column must hold ONE value per (data_source, tender_id, lot_id) -- e.g. tender_amount
#   must be identical on a lot's winner and buyer rows. Flag any that carry >1 distinct non-NA value.
#   NOTE: the lot_amount family is deliberately EXCLUDED -- lot_amount is a per-firm value (the
#   OpenTender demand shock; and per-award for TED), so it legitimately varies within a lot. Only the
#   tender-level amount (tender_amount + its EUR/DKK/annualised twins) is expected to be lot-constant.
tender_level_cols <- c(
  "tender_amount", "tender_amount_eur", "tender_amount_dkk", "tender_amount_orig",
  "annualised_tender_amount", "annualised_tender_amount_eur", "annualised_tender_amount_dkk",
  "contract_type", "contract_nature", "is_framework", "is_dps", "eu_funded",
  "procedure_type", "procedure_group", "procedure_group_h", "award_criteria", "award_criteria_h",
  "n_award_criteria", "price_weight",
  "cpv_code", "cpv_code_first", "cpv_division", "cpv_division_name", "cpv_sector", "cpv_category", "cpv_main",
  "n_lots", "n_lots_announced", "n_lots_awarded", "n_lot_winners", "n_lot_id", "divided_tender", "joint_tender",
  "tender_cancelled", "tender_status", "flag_awarded",
  "pub_date", "award_date", "submit_date", "award_end_date", "award_contract_date",
  "contract_duration_months", "contract_duration_months_min", "contract_duration_months_max", "contract_duration_days",
  "ted_notice_id", "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date",
  "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
  "award_dispatch_date", "award_publication_date", "award_tender_deadline_date")
key3 <- c("data_source", "tender_id", "lot_id")
tl_present <- intersect(tender_level_cols, names(final_data))
disagree <- rbindlist(lapply(tl_present, function(col) {
  g <- final_data[!is.na(get(col)), .(nd = uniqueN(get(col))), by = key3][nd > 1L]
  if (nrow(g)) data.table(column = col, n_lots_disagree = nrow(g)) else NULL
}), fill = TRUE)

if (nrow(disagree) == 0L) {
  message(sprintf("Passed: all %d tender/lot-level columns agree within every (data_source, tender_id, lot_id).",
                  length(tl_present)))
} else {
  setorder(disagree, -n_lots_disagree)
  message("Failed: tender/lot-level columns that DISAGREE within a lot (distinct non-NA values > 1):")
  print(disagree)
  bad_col <- disagree$column[1]
  first_bad <- final_data[!is.na(get(bad_col)), .(nd = uniqueN(get(bad_col))), by = key3][nd > 1L][1]
  ex <- merge(final_data, first_bad[, ..key3], by = key3)[, c(key3, "entity", bad_col), with = FALSE]
  cat(sprintf("Example disagreeing lot for `%s`:\n", bad_col)); print(ex)
  failures <- c(failures, sprintf("3: %d tender-level column(s) disagree within a lot", nrow(disagree)))
}

# 4 The sample-selection rules (build_prod / build_extr) reproduce the PRE-DEDUP production / extraction
#   datasets. Load the pre-dedup references saved by 3_1/3_2/3_3 (per-lot CVR lists captured BEFORE the
#   cross-method dedup) and rebuild the equivalent from the delivered combined's build flags. If the
#   dedup (or the combine/save) dropped or altered any CVR, the rebuilt lists will not match -- so this
#   confirms the dedup is lossless and the README's selection rules recover the datasets they claim.
#   Same lot key + cvr_list rule as the references (tender_id + lot_id; for TED tender_id is the notice
#   id; CVRs sorted, not deduped -> exact per-lot CVR multiset).
ref_files <- c("predup_cvr_lists_kfst_winner.rds", "predup_cvr_lists_ot_winner.rds", "predup_cvr_lists_ot_buyer.rds")
ref <- rbindlist(lapply(file.path(dirs$clean_data, "checks", ref_files), readRDS))

# Rebuild from the combined, restricted to the deduped source-entities the references cover
# (KFST winner, OT winner, OT buyer). Filter on the "source | entity" pair so we keep exactly those
# combos -- filtering data_source and entity separately would also let (KFST, buyer) through, which has
# no reference. ref_pairs is derived from the references so it stays in sync if the deduped set changes.
ref_pairs <- ref[, unique(paste(data_source, entity, sep = " | "))]
fd <- final_data[paste(data_source, entity, sep = " | ") %in% ref_pairs]
prod_rows <- fd[build_prod == TRUE, 
                .(data_source, entity, tender_id, lot_id, cvr_final, 
                  method = "production")]
extr_rows <- fd[build_extr == TRUE, 
                .(data_source, entity, tender_id, lot_id, cvr_final, 
                  method = "extraction")]
rebuilt <- rbind(prod_rows, extr_rows)[
  , .(cvr_list = paste(sort(cvr_final), collapse = ";")),
  by = .(data_source, entity, method, tender_id, lot_id)]

kcols <- c("data_source", "entity", "method", "tender_id", "lot_id")
setorderv(ref, kcols)
setorderv(rebuilt, kcols)
# identical() on the sorted tables (as data.frames -- data.tables carry an internal pointer that makes
# a direct identical() always FALSE). TRUE only if every (source, entity, method, tender, lot) row and
# its cvr_list match exactly, i.e. the build flags reproduce the pre-dedup datasets perfectly.
if (identical(as.data.frame(ref), as.data.frame(rebuilt))) {
  message(sprintf("Passed: build_prod/build_extr reproduce the pre-dedup production + extraction datasets (all %d lot-method rows).",
                  nrow(ref)))
} else {
  cmp <- merge(ref, rebuilt, by = kcols, all = TRUE, suffixes = c("_ref", "_rebuilt"))
  bad_rebuild <- cmp[is.na(cvr_list_ref) | is.na(cvr_list_rebuilt) | cvr_list_ref != cvr_list_rebuilt]
  message(sprintf("Failed: reference (%d rows) and rebuilt (%d rows) not identical; %d differing lot-method rows:",
                  nrow(ref), nrow(rebuilt), nrow(bad_rebuild)))
  print(head(bad_rebuild, 5))
  failures <- c(failures, "4: build flags do not reproduce the pre-dedup datasets")
}

# ---- Summary: stop if any check failed, so 98_ can gate the pipeline ----
if (length(failures)) {
  stop("Final data checks FAILED:\n  - ", paste(failures, collapse = "\n  - "), call. = FALSE)
} else {
  message("ALL CHECKS PASSED.")
}
