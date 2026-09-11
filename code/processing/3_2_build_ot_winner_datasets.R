# OpenTender winner concatenate-and-dedup, mirroring the KFST builder in 3_1: the production
# (name-matched winners from 2_3) and extraction (raw 8-digit CVRs from the bidder field, no matching)
# methods reduced to one row per distinct (tender_id, lot_id, CVR). Author: Jack Mulqueeney. Date: 8 Sep 2026.
#
# Grain: one row per distinct (tender_id, lot_id, winner_cvr_final). Rows with no resolved CVR are
# dropped here (they remain in clean_winner_data_ot_name_matched.rds); rows that share a CVR collapse to one.
#
# OUTPUT (data/clean/ot_winner_datasets_stacked.rds):
#   dataset     : "production" | "extraction" -- the method that produced the surviving row.
#   build_prod / build_extr : construction flags -- rebuild each sample EXACTLY by a simple filter:
#     production sample = build_prod == TRUE ;  extraction sample = build_extr == TRUE

rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean_data_dir <- dirs$clean_data

base_raw <- as.data.table(readRDS(file.path(clean_data_dir, "clean_winner_data_ot_name_matched.rds")))

# 1 Production: the name-matched OpenTender winners from 2_3, dropped to rows with a resolved CVR and
#   collapsed to distinct (tender_id, lot_id, winner_cvr_final). Preserve OpenTender's native `dataset`
#   (the annual source CSV) as `ot_source_file` before repurposing `dataset` as the method flag.
production <- copy(base_raw)
if ("dataset" %in% names(production)) setnames(production, "dataset", "ot_source_file")
production <- unique(production[!is.na(winner_cvr_final) & winner_cvr_final != ""],
                     by = c("tender_id", "lot_id", "winner_cvr_final"))
production[, dataset := "production"]

# Lot-level context (shared-schema columns constant within a tender-lot) to attach to the extraction
# rows. Built from base_raw so every lot has context, even lots whose production winners were all dropped.
ctx_cols <- intersect(c(
  "tender_id","lot_id","contract_type","lot_number","n_lots","n_bids_received","n_bidders",
  "award_date","submit_date","divided_tender","joint_tender","consortium_winner","tender_cancelled",
  "flag_awarded","tender_amount","tender_amount_eur","tender_amount_dkk","lot_amount","lot_amount_eur",
  "lot_amount_dkk","lot_amount_orig","flag_all_orig_lot_amt_missing","annualised_tender_amount",
  "annualised_lot_amount","cpv_code","cpv_code_first","cpv_division","cpv_division_name","cpv_sector",
  "cpv_category","ted_notice_id","planning_dispatch_date","planning_publication_date",
  "planning_tender_deadline_date","competition_dispatch_date","competition_publication_date",
  "competition_tender_deadline_date","award_dispatch_date","award_publication_date",
  "award_tender_deadline_date","award_contract_date",
  "procedure_type","procedure_group","procedure_group_h","award_criteria","award_criteria_h",
  "contract_duration_months","contract_duration_months_min","contract_duration_months_max",
  "is_framework","is_dps","eu_funded","subcontracted","n_award_criteria","price_weight"),
  names(base_raw))
lot_ctx <- unique(base_raw[, ..ctx_cols], by = c("tender_id","lot_id"))

# 2 Extraction: every standalone 8-digit CVR in the raw bidder field, no matching. lot_field_cvrs() is
#   the shared functions.R helper (returns distinct tender-lot-CVR triples).
extraction <- as.data.table(lot_field_cvrs(
  unique(base_raw[, .(tender_id, lot_id, winner_cvr = winner_cvr_original)])))
extraction[, winner_number := rowid(tender_id, lot_id)]
extraction[, `:=`(winner_cvr_final = cvr, winner_cvr_clean = cvr, valid_cvr = TRUE,
                  cvr_number_source = "CVR from the bidder field: raw extraction (no matching)",
                  flag_name_match_found = FALSE)]
extraction[, cvr := NULL][, dataset := "extraction"]
extraction <- merge(extraction, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)

# 2b Pre-dedup reference for 98_ (does the cross-method dedup destroy data?): per-lot sorted CVR lists
#    of the production + extraction samples as built here, BEFORE the stack/dedup below. 98 compares
#    these to the final combined's build_prod/build_extr reconstruction. No-CVR rows dropped to match
#    the combine; sorted, not deduped within a lot -- a perfect reproduction of each sample's CVRs.
ref_prod <- production[!is.na(winner_cvr_final) & winner_cvr_final != "",
                       .(data_source = "OpenTender", entity = "winner", method = "production", tender_id, lot_id, cvr_final = winner_cvr_final)]
ref_extr <- extraction[!is.na(winner_cvr_final) & winner_cvr_final != "",
                       .(data_source = "OpenTender", entity = "winner", method = "extraction", tender_id, lot_id, cvr_final = winner_cvr_final)]
predup_ref <- rbind(ref_prod, ref_extr)[, .(cvr_list = paste(sort(cvr_final), collapse = ";")),
                                        by = .(data_source, entity, method, tender_id, lot_id)]
chk_dir <- file.path(clean_data_dir, "checks"); dir.create(chk_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(predup_ref, file.path(chk_dir, "predup_cvr_lists_ot_winner.rds"))

# 3 Stack the two methods; extraction rows get NA for the production-only columns via fill = TRUE.
stacked <- rbindlist(list(production, extraction), use.names = TRUE, fill = TRUE)
stacked[, dataset := factor(dataset, levels = c("production","extraction"))]
stacked[, is_awarded_winner := awarded_winner(stacked)]   # flag_awarded (stacks carry no is_winner)

# 4 Lot-level dedup: where production and extraction agree on the lot's whole CVR set, keep one copy
#   (production); where they disagree, keep both methods' rows. build_prod / build_extr then rebuild each
#   sample exactly by a simple filter (build_prod for production, build_extr for extraction).
stacked[, cvr_list_prod := paste(sort(unique(winner_cvr_final[dataset == "production"])), collapse = ";"), by = .(tender_id, lot_id)]
stacked[, cvr_list_extr := paste(sort(unique(winner_cvr_final[dataset == "extraction"])),  collapse = ";"), by = .(tender_id, lot_id)]
stacked[, cvr_list_equal := cvr_list_prod == cvr_list_extr]
stacked_deduped <- stacked[(dataset == "production" & cvr_list_equal) | cvr_list_equal == FALSE]
stacked_deduped[, build_prod := (dataset == "production" & cvr_list_equal == FALSE) | cvr_list_equal == TRUE]
stacked_deduped[, build_extr := (dataset == "extraction"  & cvr_list_equal == FALSE) | cvr_list_equal == TRUE]

# 5 Carry every analytical/provenance column through to 4_combine, which applies the single final
#    column selection (keep_cols). Only the internal dedup scratch + the raw source-dump columns
#    (raw_dump_cols(): a negative pattern drop of known junk) are removed here.
drop_now <- unique(c("cvr_list_prod", "cvr_list_extr", "cvr_list_equal",
                     raw_dump_cols(names(stacked_deduped))))
stacked_deduped[, (drop_now) := NULL]
lead <- intersect(c("dataset","build_prod","build_extr","tender_id","lot_id","winner_number",
                    "winner_name","winner_cvr_final","is_awarded_winner"), names(stacked_deduped))
setcolorder(stacked_deduped, c(lead, setdiff(names(stacked_deduped), lead)))

# 6 Self-check: build_prod / build_extr must reproduce the production and extraction samples exactly.
#   Per tender-lot, compare the sorted CVR list of each original sample against its rebuilt version; halt if off.
.prod_orig  <- production[,     .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_orig  <- extraction[,     .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.prod_built <- stacked_deduped[build_prod == TRUE, .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_built <- stacked_deduped[build_extr == TRUE, .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.pm <- merge(.prod_orig, .prod_built, by = c("tender_id","lot_id"), all = TRUE, suffixes = c("_orig","_built"))
.em <- merge(.extr_orig, .extr_built, by = c("tender_id","lot_id"), all = TRUE, suffixes = c("_orig","_built"))
if (anyNA(.pm$l_orig) || anyNA(.pm$l_built) || !all(.pm$l_orig == .pm$l_built))
  stop("build_prod does not reproduce the production sample (tender-lot CVR-list mismatch).", call. = FALSE)
if (anyNA(.em$l_orig) || anyNA(.em$l_built) || !all(.em$l_orig == .em$l_built))
  stop("build_extr does not reproduce the extraction sample (tender-lot CVR-list mismatch).", call. = FALSE)
cat("  self-check passed: build_prod rebuilds production and build_extr rebuilds extraction exactly.\n")

out_path <- Sys.getenv("OT_STACK_OUT", unset = file.path(clean_data_dir, "ot_winner_datasets_stacked.rds"))
save_dataset(stacked_deduped, out_path)   # .rds (canonical) + .csv + .parquet
cat(sprintf("ot_winner_datasets_stacked.rds: %d rows, %d cols\n", nrow(stacked_deduped), ncol(stacked_deduped)))
print(stacked_deduped[, .N, by = dataset][order(dataset)])
cat(sprintf("  rebuild: production (build_prod)=%d | extraction (build_extr)=%d\n",
            sum(stacked_deduped$build_prod), sum(stacked_deduped$build_extr)))
