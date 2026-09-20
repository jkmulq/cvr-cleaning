# =============================================================================
# code/processing/98_final_data_checks.R
# Post-combine sanity checks on tender_data_2006_2026:
#   (1) its columns match the variable_key.xlsx "Variable key" sheet exactly;
#   (2) missingness of every variable, per (data_source, entity) pair;
#   (3) tender/lot-level columns agree across entities within each (data_source, tender_id, lot_id);
#   (4) the sample-selection rule (cvr_method) reproduces the PRE-DEDUP production / extraction
#       datasets (vs the references saved by 3_1/3_2/3_3) -- i.e. the CVR-level dedup is lossless;
#   (5) the delivered combined is unique on (data_source, entity, tender_id, lot_id, cvr_final);
#   (6) provenance: every KFST winner extraction CVR traces to the raw 'Vinders CVR' source field;
#   (6b) cross-source join key: ted_notice_id is byte-identical across data sources (no zero-padding
#        mismatch), so it still joins after de-identification/hashing.
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
final_data <- read_clean(file.path(dirs$clean_data, "tender_data_2006_2026"))
var_key <- readxl::read_excel(file.path(dirs$data, "variable_key.xlsx"),
                              sheet = "Variable key")

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

out_csv <- file.path(dirs$clean_data, "tender_data_2006_2026_missingness_by_source_entity.csv")
fwrite(missingness, out_csv)
cat(sprintf("Missingness: %d rows (%d variables x %d source-entity pairs) -> %s\n",
            nrow(missingness), uniqueN(missingness$variable), nrow(grp_n), out_csv))
cat("Rows per (data_source, entity):\n"); print(grp_n[order(data_source, entity)])

# 2b Availability sheets in the variable key (programmatic) -- ONE WIDE sheet PER METHOD (production /
#    extraction / name_match), so coverage is comparable both across (data_source, entity) combos and across
#    methods (e.g. to see name_match thinness). WIDE: variables as rows, one availability% column per
#    (data_source, entity); availability% = 100 - missing% (1 d.p.); first row __n_rows__ = each combo's
#    denominator. Regenerated every run; non-gating (tryCatch) so a write failure can't fail the data checks.
avail_wide_for <- function(dt) {
  vc <- setdiff(names(dt), c("data_source", "entity"))
  gn <- dt[, .(n_rows = .N), by = .(data_source, entity)]
  m  <- melt(dt[, lapply(.SD, n_miss), by = .(data_source, entity), .SDcols = vc],
             id.vars = c("data_source", "entity"), variable.name = "variable",
             value.name = "n_missing", variable.factor = FALSE)
  m  <- gn[m, on = c("data_source", "entity")]
  m[, pct_available := round(100 - 100 * n_missing / n_rows, 1)]
  aw <- dcast(m, variable ~ data_source + entity, value.var = "pct_available")
  nw <- dcast(gn[, .(variable = "__n_rows__", data_source, entity, v = n_rows)],
              variable ~ data_source + entity, value.var = "v")
  rbindlist(list(nw, aw), use.names = TRUE, fill = TRUE)
}
vk_path <- file.path(dirs$data, "variable_key.xlsx")
tryCatch({
  wb <- openxlsx::loadWorkbook(vk_path)
  if ("Availability" %in% openxlsx::sheets(wb)) openxlsx::removeWorksheet(wb, "Availability")  # drop old combined sheet
  for (mth in c("production", "extraction", "name_match")) {
    sub   <- final_data[grepl(mth, cvr_method)]
    sheet <- paste0("Availability_", mth)
    if (sheet %in% openxlsx::sheets(wb)) openxlsx::removeWorksheet(wb, sheet)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, if (nrow(sub)) avail_wide_for(sub) else data.table(note = "no rows for this method"))
    cat(sprintf("  %-24s %d rows\n", sheet, sub[, .N]))
  }
  openxlsx::saveWorkbook(wb, vk_path, overwrite = TRUE)
  cat("Availability_{production,extraction,name_match} sheets written to", basename(vk_path), "\n")
}, error = function(e) message("WARNING: could not write Availability sheets: ", conditionMessage(e)))

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
  "n_lots", "n_lots_announced", "n_lots_awarded", "n_lot_winners", "divided_tender", "joint_tender",
  "tender_cancelled", "tender_status", "flag_awarded",
  "pub_date", "award_date", "submit_date", "award_end_date", "award_contract_date",
  "contract_duration_months", "contract_duration_months_min", "contract_duration_months_max", "contract_duration_days",
  "ted_notice_id", "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date",
  "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
  "award_dispatch_date", "award_publication_date", "award_tender_deadline_date")
key3 <- c("data_source", "tender_id", "lot_id")
tl_present <- intersect(tender_level_cols, names(final_data))
# award_date / award_end_date are assigned PER WINNER for KFST multi-winner framework lots (the positional
# per-winner award dates from 1_1_process_kfst.R), so they legitimately vary WITHIN a KFST lot and are not
# lot-constant there. They remain lot-level for OT/TED, and TED winner/non-winner date alignment is checked
# separately in 3c -- so exempt ONLY KFST for ONLY these two columns; the strict check stands everywhere else.
per_winner_date_cols <- c("award_date", "award_end_date")
disagree <- rbindlist(lapply(tl_present, function(col) {
  dt <- if (col %in% per_winner_date_cols) final_data[data_source != "KFST"] else final_data
  g <- dt[!is.na(get(col)), .(nd = uniqueN(get(col))), by = key3][nd > 1L]
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

# 3b Method agreement (NA-STRICT): within (data_source, entity, tender_id, lot_id), every tender/lot-level
#    column must be IDENTICAL across all rows -- i.e. across cvr_method (production/extraction/name_match), so
#    e.g. KFST-winner extraction rows agree with KFST-winner production rows on their shared tender-lots.
#    uniqueN() counts NA as a level, so a method that is THIN (NA where another has a value) is flagged --
#    this catches context that failed to attach to one method (e.g. the name_match lot_id mismatch). Stricter
#    and more granular than check 3 (NA-lenient, cross-entity). INFORMATIONAL for now (reported, not gating);
#    promote to gating once confirmed clean post-fix.
key3b <- c("data_source", "entity", "tender_id", "lot_id")
disagree_m <- rbindlist(lapply(tl_present, function(col) {
  g <- final_data[, .(nd = uniqueN(get(col))), by = key3b][nd > 1L]   # NA counts as a level -> NA-strict
  if (nrow(g)) data.table(column = col, n_groups_disagree = nrow(g)) else NULL
}), fill = TRUE)
if (nrow(disagree_m) == 0L) {
  message(sprintf("Passed (3b): tender/lot columns agree across methods within every (data_source, entity, tender_id, lot_id) [%d cols].",
                  length(tl_present)))
} else {
  setorder(disagree_m, -n_groups_disagree)
  message("INFO (3b): tender/lot columns differing across methods within a (source, entity, tender, lot) [NA-strict]:")
  print(disagree_m)
  bad <- disagree_m$column[1]
  fb  <- final_data[, .(nd = uniqueN(get(bad))), by = key3b][nd > 1L][1]
  ex  <- merge(final_data, fb[, ..key3b], by = key3b)[, c(key3b, "cvr_method", bad), with = FALSE]
  cat(sprintf("Example for `%s`:\n", bad)); print(head(ex, 8))
}

# 3c TED winner/non-winner DATE alignment. A losing bidder and the winner of the same TED lot share the
#    notice's lineage dates (attached by notice_id), so on any (tender_id, lot_id) that carries BOTH a winner
#    and a non-winner row, the notice-level date columns must be IDENTICAL across them (NA-strict). Flags a
#    non-winner that didn't inherit the same dates as its winner. INFORMATIONAL.
ted_date_cols <- intersect(c(
  "award_dispatch_date", "award_publication_date", "award_tender_deadline_date", "award_contract_date",
  "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
  "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date"), names(final_data))
tedwn   <- final_data[data_source == "TED" & entity %chin% c("winner", "non-winner")]
lots_wn <- tedwn[, .(hw = any(entity == "winner"), hn = any(entity == "non-winner")), by = .(tender_id, lot_id)][hw & hn, .(tender_id, lot_id)]
if (nrow(lots_wn) == 0L) {
  message("Check 3c skipped: no TED lots carry both a winner and a non-winner row.")
} else {
  tw <- merge(tedwn, lots_wn, by = c("tender_id", "lot_id"))
  disagree_wn <- rbindlist(lapply(ted_date_cols, function(col) {
    g <- tw[, .(nd = uniqueN(get(col))), by = .(tender_id, lot_id)][nd > 1L]   # NA-strict across winner + non-winner
    if (nrow(g)) data.table(column = col, n_lots_misaligned = nrow(g)) else NULL
  }), fill = TRUE)
  if (nrow(disagree_wn) == 0L) {
    message(sprintf("Passed (3c): TED winner/non-winner dates align on all %d lots with both [%d date cols].",
                    nrow(lots_wn), length(ted_date_cols)))
  } else {
    setorder(disagree_wn, -n_lots_misaligned)
    message(sprintf("INFO (3c): TED lots (of %d with winner+non-winner) where winner/non-winner dates DIFFER [NA-strict]:", nrow(lots_wn)))
    print(disagree_wn)
    bad <- disagree_wn$column[1]
    fb  <- tw[, .(nd = uniqueN(get(bad))), by = .(tender_id, lot_id)][nd > 1L][1]
    ex  <- merge(tw, fb[, .(tender_id, lot_id)], by = c("tender_id", "lot_id"))[, .(tender_id, lot_id, entity, cvr_method, val = get(bad))]
    cat(sprintf("Example for `%s`:\n", bad)); print(head(ex, 8))
  }
}

# 4 Every (data_source, entity, method) pre-dedup dataset can be extracted EXACTLY from the deduped combined
#   using only data_source + entity + cvr_method. For each of the 18 combinations we run TWO comparisons:
#     row-by-row : take the deduped rows whose cvr_method contains the method as ONE ROW PER CVR
#                  (tender_id, lot_id, cvr) and fsetequal(..., all = TRUE) [MULTISET/bag] them against the
#                  pre-dedup reference un-collapsed to one row per CVR -- row COUNT and every row must match.
#     cvr_list   : collapse the same deduped rows to one sorted ";"-joined CVR list per lot and fsetequal()
#                  that against the reference's stored cvr_list -- the per-lot CVR set must match.
#   Both TRUE = the CVR-level dedup dropped/altered/duplicated nothing and the README's grepl rule recovers
#   that dataset. Inline (no helper) so each combination is visible. (KFST buyer is name-only -> no reference.)
ref_files <- c("predup_cvr_lists_kfst_winner.rds", "predup_cvr_lists_ot_winner.rds", "predup_cvr_lists_ot_buyer.rds",
               "predup_cvr_lists_ted_winner.rds", "predup_cvr_lists_ted_buyer.rds")
ref <- rbindlist(lapply(file.path(dirs$clean_data, "checks", ref_files), readRDS))
# 4_combine standardised blank tender_id/lot_id to NA; the references (saved by 3_* BEFORE that) keep "".
# TED notice-level rows legitimately have an empty lot_id, so align "" -> NA on the references' keys once,
# otherwise the same lot would bucket under lot_id="" (ref) vs lot_id=NA (deduped) and mismatch spuriously.
ref[!is.na(tender_id) & trimws(tender_id) == "", tender_id := NA_character_]
ref[!is.na(lot_id)    & trimws(lot_id)    == "", lot_id    := NA_character_]
# ref (as loaded) is one row per (source, entity, method, tender_id, lot_id) with the collapsed cvr_list -> used
# by the cvr_list check. ref_rows un-collapses it to one row per CVR -> used by the row-by-row check.
ref_rows <- ref[, .(cvr = unlist(strsplit(cvr_list, ";", fixed = TRUE))), by = .(data_source, entity, method, tender_id, lot_id)]

# Drift-guard: the 18 explicit checks below MUST cover exactly the source-entities the references contain.
stopifnot(setequal(ref[, unique(paste(data_source, entity, sep = " | "))],
                   c("KFST | winner", "OpenTender | winner", "OpenTender | buyer",
                     "TED | winner", "TED | non-winner", "TED | buyer")))

message("\nCheck 4: extract each (source, entity, method) from the DEDUPED combined via cvr_method and confirm")
message("         it matches its pre-dedup reference -- both ROW-BY-ROW and at the collapsed CVR-LIST level.")

r_kfst_w_prod <- fsetequal(final_data[data_source == "KFST" & entity == "winner" & grepl("production", cvr_method), 
                                      .(tender_id, lot_id, cvr = cvr_final)], 
                           ref_rows[data_source == "KFST" & entity == "winner" & method == "production", 
                                    .(tender_id, lot_id, cvr)], all = TRUE); 
l_kfst_w_prod <- fsetequal(final_data[data_source == "KFST" & entity == "winner" & grepl("production", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "KFST" & entity == "winner" & method == "production", .(tender_id, lot_id, cvr_list)]); message("  KFST       | winner     | production : row-by-row=", r_kfst_w_prod, "  cvr_list=", l_kfst_w_prod)
r_kfst_w_extr <- fsetequal(final_data[data_source == "KFST" & entity == "winner" & grepl("extraction", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "KFST" & entity == "winner" & method == "extraction", .(tender_id, lot_id, cvr)], all = TRUE); l_kfst_w_extr <- fsetequal(final_data[data_source == "KFST" & entity == "winner" & grepl("extraction", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "KFST" & entity == "winner" & method == "extraction", .(tender_id, lot_id, cvr_list)]); message("  KFST       | winner     | extraction : row-by-row=", r_kfst_w_extr, "  cvr_list=", l_kfst_w_extr)
r_kfst_w_name <- fsetequal(final_data[data_source == "KFST" & entity == "winner" & grepl("name_match", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "KFST" & entity == "winner" & method == "name_match", .(tender_id, lot_id, cvr)], all = TRUE); l_kfst_w_name <- fsetequal(final_data[data_source == "KFST" & entity == "winner" & grepl("name_match", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "KFST" & entity == "winner" & method == "name_match", .(tender_id, lot_id, cvr_list)]); message("  KFST       | winner     | name_match : row-by-row=", r_kfst_w_name, "  cvr_list=", l_kfst_w_name)

r_ot_w_prod <- fsetequal(final_data[data_source == "OpenTender" & entity == "winner" & grepl("production", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "OpenTender" & entity == "winner" & method == "production", .(tender_id, lot_id, cvr)], all = TRUE); l_ot_w_prod <- fsetequal(final_data[data_source == "OpenTender" & entity == "winner" & grepl("production", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "OpenTender" & entity == "winner" & method == "production", .(tender_id, lot_id, cvr_list)]); message("  OpenTender | winner     | production : row-by-row=", r_ot_w_prod, "  cvr_list=", l_ot_w_prod)
r_ot_w_extr <- fsetequal(final_data[data_source == "OpenTender" & entity == "winner" & grepl("extraction", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "OpenTender" & entity == "winner" & method == "extraction", .(tender_id, lot_id, cvr)], all = TRUE); l_ot_w_extr <- fsetequal(final_data[data_source == "OpenTender" & entity == "winner" & grepl("extraction", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "OpenTender" & entity == "winner" & method == "extraction", .(tender_id, lot_id, cvr_list)]); message("  OpenTender | winner     | extraction : row-by-row=", r_ot_w_extr, "  cvr_list=", l_ot_w_extr)
r_ot_w_name <- fsetequal(final_data[data_source == "OpenTender" & entity == "winner" & grepl("name_match", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "OpenTender" & entity == "winner" & method == "name_match", .(tender_id, lot_id, cvr)], all = TRUE); l_ot_w_name <- fsetequal(final_data[data_source == "OpenTender" & entity == "winner" & grepl("name_match", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "OpenTender" & entity == "winner" & method == "name_match", .(tender_id, lot_id, cvr_list)]); message("  OpenTender | winner     | name_match : row-by-row=", r_ot_w_name, "  cvr_list=", l_ot_w_name)

r_ot_b_prod <- fsetequal(final_data[data_source == "OpenTender" & entity == "buyer" & grepl("production", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "OpenTender" & entity == "buyer" & method == "production", .(tender_id, lot_id, cvr)], all = TRUE); l_ot_b_prod <- fsetequal(final_data[data_source == "OpenTender" & entity == "buyer" & grepl("production", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "OpenTender" & entity == "buyer" & method == "production", .(tender_id, lot_id, cvr_list)]); message("  OpenTender | buyer      | production : row-by-row=", r_ot_b_prod, "  cvr_list=", l_ot_b_prod)
r_ot_b_extr <- fsetequal(final_data[data_source == "OpenTender" & entity == "buyer" & grepl("extraction", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "OpenTender" & entity == "buyer" & method == "extraction", .(tender_id, lot_id, cvr)], all = TRUE); l_ot_b_extr <- fsetequal(final_data[data_source == "OpenTender" & entity == "buyer" & grepl("extraction", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "OpenTender" & entity == "buyer" & method == "extraction", .(tender_id, lot_id, cvr_list)]); message("  OpenTender | buyer      | extraction : row-by-row=", r_ot_b_extr, "  cvr_list=", l_ot_b_extr)
r_ot_b_name <- fsetequal(final_data[data_source == "OpenTender" & entity == "buyer" & grepl("name_match", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "OpenTender" & entity == "buyer" & method == "name_match", .(tender_id, lot_id, cvr)], all = TRUE); l_ot_b_name <- fsetequal(final_data[data_source == "OpenTender" & entity == "buyer" & grepl("name_match", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "OpenTender" & entity == "buyer" & method == "name_match", .(tender_id, lot_id, cvr_list)]); message("  OpenTender | buyer      | name_match : row-by-row=", r_ot_b_name, "  cvr_list=", l_ot_b_name)

r_ted_w_prod <- fsetequal(final_data[data_source == "TED" & entity == "winner" & grepl("production", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "winner" & method == "production", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_w_prod <- fsetequal(final_data[data_source == "TED" & entity == "winner" & grepl("production", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "winner" & method == "production", .(tender_id, lot_id, cvr_list)]); message("  TED        | winner     | production : row-by-row=", r_ted_w_prod, "  cvr_list=", l_ted_w_prod)
r_ted_w_extr <- fsetequal(final_data[data_source == "TED" & entity == "winner" & grepl("extraction", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "winner" & method == "extraction", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_w_extr <- fsetequal(final_data[data_source == "TED" & entity == "winner" & grepl("extraction", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "winner" & method == "extraction", .(tender_id, lot_id, cvr_list)]); message("  TED        | winner     | extraction : row-by-row=", r_ted_w_extr, "  cvr_list=", l_ted_w_extr)
r_ted_w_name <- fsetequal(final_data[data_source == "TED" & entity == "winner" & grepl("name_match", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "winner" & method == "name_match", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_w_name <- fsetequal(final_data[data_source == "TED" & entity == "winner" & grepl("name_match", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "winner" & method == "name_match", .(tender_id, lot_id, cvr_list)]); message("  TED        | winner     | name_match : row-by-row=", r_ted_w_name, "  cvr_list=", l_ted_w_name)

r_ted_n_prod <- fsetequal(final_data[data_source == "TED" & entity == "non-winner" & grepl("production", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "non-winner" & method == "production", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_n_prod <- fsetequal(final_data[data_source == "TED" & entity == "non-winner" & grepl("production", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "non-winner" & method == "production", .(tender_id, lot_id, cvr_list)]); message("  TED        | non-winner | production : row-by-row=", r_ted_n_prod, "  cvr_list=", l_ted_n_prod)
r_ted_n_extr <- fsetequal(final_data[data_source == "TED" & entity == "non-winner" & grepl("extraction", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "non-winner" & method == "extraction", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_n_extr <- fsetequal(final_data[data_source == "TED" & entity == "non-winner" & grepl("extraction", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "non-winner" & method == "extraction", .(tender_id, lot_id, cvr_list)]); message("  TED        | non-winner | extraction : row-by-row=", r_ted_n_extr, "  cvr_list=", l_ted_n_extr)
r_ted_n_name <- fsetequal(final_data[data_source == "TED" & entity == "non-winner" & grepl("name_match", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "non-winner" & method == "name_match", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_n_name <- fsetequal(final_data[data_source == "TED" & entity == "non-winner" & grepl("name_match", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "non-winner" & method == "name_match", .(tender_id, lot_id, cvr_list)]); message("  TED        | non-winner | name_match : row-by-row=", r_ted_n_name, "  cvr_list=", l_ted_n_name)

r_ted_b_prod <- fsetequal(final_data[data_source == "TED" & entity == "buyer" & grepl("production", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "buyer" & method == "production", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_b_prod <- fsetequal(final_data[data_source == "TED" & entity == "buyer" & grepl("production", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "buyer" & method == "production", .(tender_id, lot_id, cvr_list)]); message("  TED        | buyer      | production : row-by-row=", r_ted_b_prod, "  cvr_list=", l_ted_b_prod)
r_ted_b_extr <- fsetequal(final_data[data_source == "TED" & entity == "buyer" & grepl("extraction", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "buyer" & method == "extraction", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_b_extr <- fsetequal(final_data[data_source == "TED" & entity == "buyer" & grepl("extraction", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "buyer" & method == "extraction", .(tender_id, lot_id, cvr_list)]); message("  TED        | buyer      | extraction : row-by-row=", r_ted_b_extr, "  cvr_list=", l_ted_b_extr)
r_ted_b_name <- fsetequal(final_data[data_source == "TED" & entity == "buyer" & grepl("name_match", cvr_method), .(tender_id, lot_id, cvr = cvr_final)], ref_rows[data_source == "TED" & entity == "buyer" & method == "name_match", .(tender_id, lot_id, cvr)], all = TRUE); l_ted_b_name <- fsetequal(final_data[data_source == "TED" & entity == "buyer" & grepl("name_match", cvr_method), .(cvr_list = paste(sort(cvr_final), collapse = ";")), by = .(tender_id, lot_id)], ref[data_source == "TED" & entity == "buyer" & method == "name_match", .(tender_id, lot_id, cvr_list)]); message("  TED        | buyer      | name_match : row-by-row=", r_ted_b_name, "  cvr_list=", l_ted_b_name)

if (all(r_kfst_w_prod, r_kfst_w_extr, r_kfst_w_name, r_ot_w_prod, r_ot_w_extr, r_ot_w_name,
        r_ot_b_prod, r_ot_b_extr, r_ot_b_name, r_ted_w_prod, r_ted_w_extr, r_ted_w_name,
        r_ted_n_prod, r_ted_n_extr, r_ted_n_name, r_ted_b_prod, r_ted_b_extr, r_ted_b_name,
        l_kfst_w_prod, l_kfst_w_extr, l_kfst_w_name, l_ot_w_prod, l_ot_w_extr, l_ot_w_name,
        l_ot_b_prod, l_ot_b_extr, l_ot_b_name, l_ted_w_prod, l_ted_w_extr, l_ted_w_name,
        l_ted_n_prod, l_ted_n_extr, l_ted_n_name, l_ted_b_prod, l_ted_b_extr, l_ted_b_name)) {
  message("Passed (4): all 18 (source, entity, method) datasets extract identically -- both row-by-row and cvr_list -- so the dedup is lossless.")
} else {
  failures <- c(failures, "4: a (source, entity, method) dataset does NOT extract identically from the deduped combined (see FALSE lines above)")
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

# 6b Cross-source join key. `ted_notice_id` is the ONLY identifier shared across data sources
#    (tender_id/lot_id are source-local -- KFST integers vs OT UUIDs). It is de-identified (hashed)
#    before delivery, so the SAME notice must carry a BYTE-IDENTICAL string in every source or the
#    hashes won't match and server-side cross-source dedup silently breaks. This GATES on the one thing
#    that would break the join: a notice represented with different zero-padding across sources (e.g.
#    "43653-2021" vs "043653-2021"). We detect it by comparing each source-pair's raw exact overlap to
#    the overlap after stripping leading zeros -- if stripping recovers extra matches, padding differs.
#    (The 6-digit legacy / 8-digit eForms split is a real TED numbering change over time, NOT a mismatch.)
#    Non-blocking remedy if this ever fails: standardise ted_notice_id (zero-pad) before de-identification.
nid <- function(src) {
  v <- final_data[data_source == src & !is.na(ted_notice_id) & trimws(ted_notice_id) != "", ted_notice_id]
  unique(as.character(v))
}
strip0 <- function(x) paste0(sub("^0+", "", sub("-.*$", "", x)), "-", sub("^.*-", "", x))
srcs   <- intersect(c("KFST", "OpenTender", "TED"), unique(final_data$data_source))
ids    <- setNames(lapply(srcs, nid), srcs)
message("Cross-source join key (ted_notice_id) -- exact-match overlap by source pair:")
pad_loss_total <- 0L
if (length(srcs) >= 2) {
  for (i in 1:(length(srcs) - 1)) for (j in (i + 1):length(srcs)) {
    a <- ids[[srcs[i]]]; b <- ids[[srcs[j]]]
    raw_ov   <- length(intersect(a, b))
    strip_ov <- length(intersect(strip0(a), strip0(b)))
    pad_loss <- strip_ov - raw_ov
    pad_loss_total <- pad_loss_total + pad_loss
    message(sprintf("  %-10s <-> %-10s : raw=%6d  zero-stripped=%6d  padding-loss=%d%s",
                    srcs[i], srcs[j], raw_ov, strip_ov, pad_loss,
                    if (pad_loss > 0L) "  <-- MISMATCH" else ""))
  }
}
# format conformance + coverage (informational, printed for the record)
fmt_ok <- final_data[!is.na(ted_notice_id) & trimws(ted_notice_id) != "",
                     .(pct_conform = round(100 * mean(grepl("^[0-9]+-[0-9]{4}$", ted_notice_id)), 2)), by = data_source]
cov    <- final_data[, .(pct_with_join_key = round(100 * mean(!is.na(ted_notice_id) & trimws(ted_notice_id) != ""), 1)), by = data_source]
message("  format conformance (^digits-YYYY$):"); print(fmt_ok[order(data_source)])
message("  coverage (rows with a join key):");    print(cov[order(data_source)])
if (pad_loss_total == 0L) {
  message("Passed: ted_notice_id is byte-identical across sources -- joins survive de-identification.")
} else {
  message(sprintf("Failed: %d notice(s) differ only by zero-padding across sources -- would fail to join after hashing.", pad_loss_total))
  failures <- c(failures, sprintf("6b: %d cross-source ted_notice_id padding mismatch(es)", pad_loss_total))
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

# ---- 8 Analysis-readiness: build each planned analysis sample and require it is non-empty ----
# Gates the pipeline so the delivered clean data can actually feed the server-side analyses. Each block
# constructs the sample an analysis needs (resolved CVR + the required date + entity) and FAILS if it is
# empty. Employment series are pulled server-side, so these check the clean-data INPUTS only:
#   1 winner event study            -> winner CVR + award_date
#   4 non-winner event study        -> non-winner CVR + award_date (filled from award_contract_date in 4_combine)
#   3 winner vs non-winner control  -> (tender_id, lot_id) lots holding BOTH a dated winner and a dated non-winner
tryCatch({
  fd <- final_data
  if (!("award_date" %in% names(fd))) stop("award_date column missing from combined")
  areq <- function(name, dt) {
    n <- nrow(dt); message(sprintf("  analysis-readiness [%s]: %d rows", name, n))
    if (n == 0) failures <<- c(failures, sprintf("analysis sample '%s' is EMPTY", name))
    invisible(n)
  }
  dated <- function(ent) fd[entity == ent & !is.na(cvr_final) & cvr_final != "" & !is.na(award_date)]
  areq("1 winner event study (winner CVR + award_date)",      dated("winner"))
  areq("4 non-winner event study (non-winner CVR + award_date)", dated("non-winner"))
  kw  <- unique(fd[entity == "winner"     & !is.na(award_date), .(tender_id, lot_id)])
  knw <- unique(fd[entity == "non-winner" & !is.na(award_date), .(tender_id, lot_id)])
  shared <- merge(kw, knw, by = c("tender_id", "lot_id"))
  areq("3 winner-vs-non-winner control (lots with both, dated)", shared)
  # Do winner and non-winner carry the SAME award date on those shared lots? (the within-notice tie) -- report only.
  if (nrow(shared)) {
    both <- merge(fd[entity %in% c("winner","non-winner") & !is.na(award_date), .(tender_id, lot_id, award_date)],
                  shared, by = c("tender_id", "lot_id"))
    disagree <- both[, .(nd = uniqueN(award_date)), by = .(tender_id, lot_id)][nd > 1, .N]
    message(sprintf("  analysis-readiness [within-lot date tie]: %d of %d shared lots have winner/non-winner award_date disagreement (framework lots can differ)", disagree, nrow(shared)))
  }
  # Can we size the tender for the dated-winner universe? (amount present) -- report only.
  au <- fd[entity == "winner" & !is.na(award_date)]
  amt_ok <- au[!is.na(lot_amount) | !is.na(tender_amount), .N]
  message(sprintf("  analysis-readiness [winner tender size]: %d of %d dated-winner rows have a lot/tender amount (%.1f%%)",
                  amt_ok, nrow(au), 100 * amt_ok / max(1, nrow(au))))
}, error = function(e) failures <<- c(failures, paste("analysis-readiness errored:", conditionMessage(e))))

# 9 Viewable diagnostic objects (not pass/fail): built here so a reader can inspect them directly, and
#   saved to CSV alongside the missingness output. (a) the cross-source notice_id overlap matrix -- how
#   joinable notice ids are across data sources; (b) the winner/non-winner lot table -- every lot that
#   carries a non-winner, with its winner + non-winner rows side by side.

# 9a Cross-source notice_id overlap. Drop rows with no notice id first (otherwise every keyless row folds
#    into a single phantom id = NA), and sort the sources in the label so a combo is not split by row order.
notice_src <- unique(final_data[!is.na(ted_notice_id) & ted_notice_id != "", .(data_source, id = ted_notice_id)])
notice_src <- notice_src[, .(in_source = paste(sort(unique(data_source)), collapse = "; "),
                             n_sources = uniqueN(data_source)), by = id]
notice_source_overlap <- notice_src[, .(n_notices = .N), by = .(n_sources, in_source)][order(-n_sources, -n_notices)]
# coverage: the OTHER half of joinability -- how many rows even carry a notice id, per source.
notice_id_coverage <- final_data[, .(rows = .N,
                                      pct_with_notice_id = round(100 * mean(!is.na(ted_notice_id) & ted_notice_id != ""), 1),
                                      distinct_notice_ids = uniqueN(ted_notice_id[!is.na(ted_notice_id) & ted_notice_id != ""])),
                                  by = data_source][order(data_source)]
message("\n9a notice_id coverage per source (can a row even join?):")
print(notice_id_coverage)
message(sprintf("9a cross-source notice_id overlap (%d of %d id-bearing notices are in >=2 sources, %.1f%%):",
                notice_src[n_sources >= 2, .N], nrow(notice_src),
                100 * notice_src[n_sources >= 2, .N] / max(1, nrow(notice_src))))
print(notice_source_overlap)
fwrite(notice_source_overlap, file.path(dirs$clean_data, "tender_data_2006_2026_notice_source_overlap.csv"))
fwrite(notice_id_coverage,    file.path(dirs$clean_data, "tender_data_2006_2026_notice_id_coverage.csv"))

# 9b Winner vs non-winner lots: every (tender_id, lot_id) that carries a non-winner, with its winner and
#    non-winner rows, ordered winner-first within each lot. NOTE: this keeps dual-role firms in place -- a
#    firm listed as both winner and non-winner on the same lot appears in both arms (excluded only at
#    analysis time, e.g. estudy_winner_vs_nonwinner_matched.R).
final_data[, non_winner_lot := any(entity == "non-winner"), by = .(tender_id, lot_id)]
winner_nonwinner_lots <- final_data[non_winner_lot == TRUE & entity %chin% c("non-winner", "winner"),
                                    .(tender_id, lot_id, entity, ted_notice_id, cvr_final,
                                      tender_amount, lot_amount, award_date, award_publication_date)]
setorder(winner_nonwinner_lots, tender_id, lot_id, -entity)
final_data[, non_winner_lot := NULL]   # drop the scratch flag from the delivered object
message(sprintf("\n9b winner/non-winner lot table: %d rows across %d lots (%d winner, %d non-winner rows); first 6:",
                nrow(winner_nonwinner_lots), uniqueN(winner_nonwinner_lots[, .(tender_id, lot_id)]),
                winner_nonwinner_lots[entity == "winner", .N], winner_nonwinner_lots[entity == "non-winner", .N]))
print(head(winner_nonwinner_lots, 6))
fwrite(winner_nonwinner_lots, file.path(dirs$clean_data, "tender_data_2006_2026_winner_nonwinner_lots.csv"))

# ---- Summary: stop if any check failed, so 98_ can gate the pipeline ----
if (length(failures)) {
  stop("Final data checks FAILED:\n  - ", paste(failures, collapse = "\n  - "), call. = FALSE)
} else {
  message("ALL CHECKS PASSED.")
}
