# =============================================================================
# code/processing/98_final_data_checks.R
# Post-combine sanity checks on clean_all_samples_combined:
#   (1) its columns match the variable-key "Final key (preview)" sheet exactly;
#   (2) missingness of every variable, per (data_source, entity) pair;
#   (3) tender/lot-level columns agree across entities within each (data_source, tender_id, lot_id);
#   (4) the sample-selection rule (cvr_method) reproduces the PRE-DEDUP production / extraction
#       datasets (vs the references saved by 3_1/3_2/3_3) -- i.e. the CVR-level dedup is lossless;
#   (5) the delivered combined is unique on (data_source, entity, tender_id, lot_id, cvr_final);
#   (6) provenance: every KFST winner extraction CVR traces to the raw 'Vinders CVR' source field.
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

# Run all checks, collecting any failures, then stop at the end if there were any -- so one run
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

# 4 The sample-selection rule (cvr_method) reproduces the PRE-DEDUP production / extraction datasets.
#   Load the pre-dedup references saved by 3_1/3_2/3_3 (per-lot CVR lists captured BEFORE the
#   cross-method dedup) and rebuild the equivalent from the delivered combined's cvr_method
#   (production = grepl("production", cvr_method); extraction = grepl("extraction", cvr_method)). If the
#   dedup (or the combine/save) dropped or altered any CVR, the rebuilt lists will not match -- so this
#   confirms the dedup is lossless and the README's selection rules recover the datasets they claim.
#   Same lot key + cvr_list rule as the references (tender_id + lot_id; for TED tender_id is the notice
#   id; CVRs sorted, not deduped -> exact per-lot CVR multiset).
ref_files <- c("predup_cvr_lists_kfst_winner.rds", "predup_cvr_lists_ot_winner.rds", "predup_cvr_lists_ot_buyer.rds",
               "predup_cvr_lists_ted_winner.rds", "predup_cvr_lists_ted_buyer.rds")
ref <- rbindlist(lapply(file.path(dirs$clean_data, "checks", ref_files), readRDS))

# Rebuild from the combined, restricted to the deduped source-entities the references cover
# (KFST winner, OT winner, OT buyer). Filter on the "source | entity" pair so we keep exactly those
# combos -- filtering data_source and entity separately would also let (KFST, buyer) through, which has
# no reference. ref_pairs is derived from the references so it stays in sync if the deduped set changes.
ref_pairs <- ref[, unique(paste(data_source, entity, sep = " | "))]
fd <- final_data[paste(data_source, entity, sep = " | ") %in% ref_pairs]
prod_rows <- fd[grepl("production", cvr_method),
                .(data_source, entity, tender_id, lot_id, cvr_final,
                  method = "production")]
extr_rows <- fd[grepl("extraction", cvr_method),
                .(data_source, entity, tender_id, lot_id, cvr_final,
                  method = "extraction")]
nm_rows   <- fd[grepl("name_match", cvr_method),
                .(data_source, entity, tender_id, lot_id, cvr_final,
                  method = "name_match")]
rebuilt <- rbind(prod_rows, extr_rows, nm_rows)[
  , .(cvr_list = paste(sort(cvr_final), collapse = ";")),
  by = .(data_source, entity, method, tender_id, lot_id)]

kcols <- c("data_source", "entity", "method", "tender_id", "lot_id")
# 4_combine standardises blank strings to NA, but the pre-dedup references (saved by 3_* BEFORE that step)
# keep an empty tender_id/lot_id as "". TED notice-level rows legitimately have an empty lot_id, so the same
# (tender, CVR) would otherwise bucket under lot_id="" in the reference vs lot_id=NA in the rebuilt and
# spuriously mismatch. Treat "" and NA identically on the key columns before comparing.
for (dt in list(ref, rebuilt)) for (kc in c("tender_id", "lot_id")) {
  dt[!is.na(get(kc)) & trimws(get(kc)) == "", (kc) := NA_character_]
}
setorderv(ref, kcols)
setorderv(rebuilt, kcols)
# identical() on the sorted tables (as data.frames -- data.tables carry an internal pointer that makes
# a direct identical() always FALSE). TRUE only if every (source, entity, method, tender, lot) row and
# its cvr_list match exactly, i.e. cvr_method reproduces the pre-dedup datasets perfectly.
if (identical(as.data.frame(ref), as.data.frame(rebuilt))) {
  message(sprintf("Passed: cvr_method reproduces the pre-dedup production + extraction datasets (all %d lot-method rows).",
                  nrow(ref)))
} else {
  cmp <- merge(ref, rebuilt, by = kcols, all = TRUE, suffixes = c("_ref", "_rebuilt"))
  bad_rebuild <- cmp[is.na(cvr_list_ref) | is.na(cvr_list_rebuilt) | cvr_list_ref != cvr_list_rebuilt]
  message(sprintf("Failed: reference (%d rows) and rebuilt (%d rows) not identical; %d differing lot-method rows:",
                  nrow(ref), nrow(rebuilt), nrow(bad_rebuild)))
  print(head(bad_rebuild, 5))
  failures <- c(failures, "4: cvr_method does not reproduce the pre-dedup datasets")
}

# 5 The delivered combined is UNIQUE on (data_source, entity, tender_id, lot_id, cvr_final): one row per
#   CVR. The CVR-level dedup in the 3_* builders collapses a CVR produced by both methods into a single
#   row (cvr_method = "both"), so no (source, entity, tender, lot, CVR) key may repeat.
ukey <- c("data_source", "entity", "tender_id", "lot_id", "cvr_final")
n_dup_rows <- nrow(final_data) - nrow(unique(final_data, by = ukey))
if (n_dup_rows == 0L) {
  message(sprintf("Passed: combined is unique on (%s) -- no duplicate CVR rows.", paste(ukey, collapse = ", ")))
} else {
  dup_keys <- final_data[, .N, by = ukey][N > 1L][order(-N)]
  message(sprintf("Failed: %d duplicate rows across %d repeated keys on (%s):",
                  n_dup_rows, nrow(dup_keys), paste(ukey, collapse = ", ")))
  print(head(dup_keys, 5))
  failures <- c(failures, sprintf("5: %d duplicate (source,entity,tender,lot,cvr) rows", n_dup_rows))
}

# 6 Provenance (KFST winner extraction): every extraction-sample CVR must be a standalone 8-digit run in
#   the raw KFST 'Vinders CVR' field for that lot. Moved here from the retired
#   tests/test_kfst_winner_datasets.R and rewritten against the combined. (The test's other assertions are
#   already covered: structure/no-invalid-CVR/uniqueness by checks 1 & 5; production reproduction by check 4.)
#   Reads BOTH sheets -- 2.0 Udbudsdata + 2.1 Profylaksebekendtgørelser -- namespacing the profylakse ids
#   with "P" exactly as 1_1 does, so the (tender_id, lot_id) keys line up with the combined.
is8 <- function(x) !is.na(x) & grepl("^[0-9]{8}$", x)
kfst_xlsx <- file.path(dirs$raw_data, "kfst", "udbudsdata_kfst.xlsx")
raw_field <- rbindlist(lapply(
  list(c("^2\\.0 Udbudsdata", ""), c("Profylakse", "P")),
  function(sp) {
    sh <- grep(sp[1], readxl::excel_sheets(kfst_xlsx), value = TRUE)[1]
    r  <- as.data.table(readxl::read_excel(kfst_xlsx, sheet = sh, col_types = "text"))
    r[, .(tender_id = paste0(sp[2], `Løbenummer`), lot_id = paste0(sp[2], `Nummerplade`), f = `Vinders CVR`)]
  }))
raw_field <- raw_field[!is.na(f), .(cvr = unlist(regmatches(f, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", f, perl = TRUE)))),
                       by = .(tender_id, lot_id)]
ex <- unique(final_data[data_source == "KFST" & entity == "winner" & grepl("extraction", cvr_method) & is8(cvr_final),
                        .(tender_id = as.character(tender_id), lot_id = as.character(lot_id), cvr = as.character(cvr_final))])
n_untraced <- nrow(ex[!raw_field, on = .(tender_id, lot_id, cvr)])
if (n_untraced == 0L) {
  message(sprintf("Passed: all %d KFST winner extraction CVRs trace to the raw 'Vinders CVR' field (both sheets).", nrow(ex)))
} else {
  message(sprintf("Failed: %d of %d KFST winner extraction CVR(s) do NOT trace to the raw field:", n_untraced, nrow(ex)))
  print(head(ex[!raw_field, on = .(tender_id, lot_id, cvr)], 5))
  failures <- c(failures, sprintf("6: %d untraceable KFST extraction CVRs", n_untraced))
}

# 7 Informational data-quality metrics -- NON-GATING. Surfaces distributional/coverage signals from the
#   raw-reconciliation review (CVR validity, contract_nature domain, direct-award namespacing, currency peg,
#   is_winner coverage, annualisation consistency, duration plausibility). Wrapped so a metric erroring can
#   never stop the pipeline; these WARN only and are excluded from `failures`.
message("\n--- 7. Informational data-quality metrics (non-gating) ---")
tryCatch({
  fd <- final_data; pk <- c("data_source", "entity")

  # 7a cvr_final validity: ~100% clean 8-digit expected (NA already dropped upstream).
  print(fd[, .(rows = .N, pct_cvr_8digit = round(100 * mean(grepl("^[0-9]{8}$", cvr_final)), 3)), by = pk][order(data_source, entity)])

  # 7b contract_nature: out-of-domain count (want 0), coverage + cpv-mismatch rate by source.
  if ("contract_nature" %in% names(fd)) {
    ood <- fd[!is.na(contract_nature) & !(tolower(contract_nature) %chin% c("works", "services", "supplies")), .N]
    message(sprintf("7b contract_nature out-of-domain values: %d (want 0)", ood))
    print(fd[, .(contract_nature_cov = round(100 * mean(!is.na(contract_nature)), 1),
                 cpv_mismatch_pct = if ("flag_nature_cpv_mismatch" %in% names(fd)) round(100 * mean(flag_nature_cpv_mismatch, na.rm = TRUE), 2) else NA_real_),
             by = data_source][order(data_source)])
  }

  # 7c KFST direct_award <-> "P" namespacing (want the 2nd and 3rd counts = 0).
  kf <- fd[data_source == "KFST"]
  message(sprintf("7c KFST direct_award vs 'P'-id: P&TRUE=%d | P&not-TRUE=%d | non-P&TRUE=%d",
                  kf[grepl("^P", tender_id) & direct_award %in% TRUE, .N],
                  kf[grepl("^P", tender_id) & !(direct_award %in% TRUE), .N],
                  kf[!grepl("^P", tender_id) & direct_award %in% TRUE, .N]))

  # 7d currency peg (dkk/eur ~ 7.46038) + currency-label gaps.
  for (a in c("tender", "lot")) {
    ec <- paste0(a, "_amount_eur"); dc <- paste0(a, "_amount_dkk")
    if (all(c(ec, dc) %in% names(fd))) {
      ok <- !is.na(fd[[ec]]) & !is.na(fd[[dc]]) & fd[[ec]] != 0
      if (any(ok)) { r <- fd[[dc]][ok] / fd[[ec]][ok]
        message(sprintf("7d %s_amount dkk/eur: min=%.5f median=%.5f max=%.5f | off-peg(>0.1%%)=%d",
                        a, min(r), median(r), max(r), sum(abs(r - 7.46038) / 7.46038 > 0.001))) }
    }
  }
  if ("currency" %in% names(fd))
    message(sprintf("7d currency label blank but _eur/_dkk populated: %d rows (amounts fine; label only)",
                    fd[(is.na(currency) | trimws(currency) == "") & (!is.na(tender_amount_dkk) | !is.na(tender_amount_eur)), .N]))

  # 7e is_winner coverage by source/entity (OT is expected all-NA -> use is_awarded_winner there).
  if ("is_winner" %in% names(fd))
    print(fd[, .(is_winner_cov = round(100 * mean(!is.na(is_winner)), 1)), by = pk][order(data_source, entity)])

  # 7f annualisation recompute: annualised_tender_amount == tender_amount / contract_duration_months * 12.
  if (all(c("annualised_tender_amount", "tender_amount", "contract_duration_months") %in% names(fd))) {
    dm <- suppressWarnings(as.numeric(fd$contract_duration_months))
    okd <- !is.na(dm) & dm > 0 & !is.na(fd$tender_amount) & !is.na(fd$annualised_tender_amount)
    rec <- fd$tender_amount[okd] / dm[okd] * 12
    message(sprintf("7f annualised_tender_amount recompute mismatches: %d of %d checkable rows (want 0)",
                    sum(abs(rec - fd$annualised_tender_amount[okd]) > 1e-6 * pmax(1, abs(fd$annualised_tender_amount[okd]))), sum(okd)))
  }

  # 7g duration plausibility: implausibly long contract_duration_months (source data-entry errors).
  if ("contract_duration_months" %in% names(fd))
    message(sprintf("7g contract_duration_months > 600 months (~>50yr, implausible): %d rows",
                    fd[!is.na(contract_duration_months) & contract_duration_months > 600, .N]))
}, error = function(e) message("7: informational metrics errored (non-fatal): ", conditionMessage(e)))

# ---- Summary: stop if any check failed, so 98_ can gate the pipeline ----
if (length(failures)) {
  stop("Final data checks FAILED:\n  - ", paste(failures, collapse = "\n  - "), call. = FALSE)
} else {
  message("ALL CHECKS PASSED.")
}
