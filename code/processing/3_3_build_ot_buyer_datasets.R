# OpenTender BUYER concatenate-and-dedup, the buyer analogue of the winner builders (3_1/3_2): the
# production (name-matched buyers from 2_4) and extraction (raw 8-digit CVRs from the buyer field, no
# matching) methods reduced to one row per distinct (tender_id, lot_id, CVR). 
# Author: Jack Mulqueeney. 
# Date: 8 Sep 2026.

# OpenTender ONLY: KFST buyer data carries no source CVR field (buyer CVRs there come purely from name
# matching), so there is nothing to extract and no KFST buyer stack.
#
# Grain: one row per distinct (tender_id, lot_id, buyer_cvr_final). Rows with no resolved CVR are dropped
# here (they remain in clean_buyer_data_ot_name_matched.rds); rows that share a CVR collapse to one.
#
# OUTPUT (data/clean/ot_buyer_datasets_stacked.rds):
#   dataset     : "production" | "extraction" | "name_match" -- the method that produced the surviving row.
#   cvr_method  : method(s) that produced the CVR, ";"-joined (e.g. "production; extraction; name_match").
#     Rebuild each sample EXACTLY via grepl(<m>, cvr_method). name_match = re-run name-match-only CVRs
#     (field CVR ignored); generated inline in 2c (CVR only, no quality diagnostics).

rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean_data_dir <- dirs$clean_data

base_raw <- as.data.table(readRDS(file.path(clean_data_dir, "clean_buyer_data_ot_name_matched.rds")))

# 1 Production: the name-matched OpenTender buyers from 2_4, dropped to rows with a resolved CVR and
#   collapsed to distinct (tender_id, lot_id, buyer_cvr_final). Preserve OpenTender's native `dataset`
#   (the annual source CSV) as `ot_source_file` before repurposing `dataset` as the method flag.
production <- copy(base_raw)
if ("dataset" %in% names(production)) setnames(production, "dataset", "ot_source_file")
production <- unique(production[!is.na(buyer_cvr_final) & buyer_cvr_final != ""],
                     by = c("tender_id", "lot_id", "buyer_cvr_final"))
production[, dataset := "production"]

# Lot-level context (shared-schema columns constant within a tender-lot) to attach to the extraction
# rows. Built from base_raw so every lot has context, even lots whose production buyers were all dropped.
ctx_cols <- intersect(c(
  "tender_id","lot_id","contract_type","contract_nature","lot_number","n_lots","n_bids_received","n_bidders",
  "award_date","submit_date","divided_tender","joint_tender","consortium_winner","tender_cancelled",
  "flag_awarded","tender_amount","tender_amount_eur","tender_amount_dkk","lot_amount","lot_amount_eur",
  "lot_amount_dkk","lot_amount_orig","flag_all_orig_lot_amt_missing","annualised_tender_amount",
  "annualised_lot_amount","cpv_code","cpv_code_first","cpv_division","cpv_division_name","cpv_sector",
  "cpv_category","ted_notice_id","planning_dispatch_date","planning_publication_date",
  "planning_tender_deadline_date","competition_dispatch_date","competition_publication_date",
  "competition_tender_deadline_date","award_dispatch_date","award_publication_date",
  "award_tender_deadline_date","award_contract_date",
  "procedure_type","procedure_group","procedure_group_h","direct_award","award_criteria","award_criteria_h",
  "contract_duration_months","contract_duration_months_min","contract_duration_months_max",
  "is_framework","is_dps","eu_funded","subcontracted","n_award_criteria","price_weight"),
  names(base_raw))
lot_ctx <- unique(base_raw[, ..ctx_cols], by = c("tender_id","lot_id"))

# 2 Extraction: every standalone 8-digit CVR in the raw buyer field, no matching. lot_field_cvrs() keys
#   on a column literally named winner_cvr, so the buyer field is aliased to it; it returns distinct
#   tender-lot-CVR triples. The buyer shared schema has no valid_cvr / buyer_cvr_clean (KFST buyers lack
#   them), so extraction rows set only buyer_cvr_final + provenance.
extraction <- as.data.table(lot_field_cvrs(
  unique(base_raw[, .(tender_id, lot_id, winner_cvr = buyer_cvr_original)])))
extraction[, buyer_number := rowid(tender_id, lot_id)]
extraction[, `:=`(buyer_cvr_final = cvr,
                  cvr_number_source = "CVR from the buyer field: raw extraction (no matching)",
                  flag_name_match_found = FALSE)]
extraction[, cvr := NULL][, dataset := "extraction"]
extraction <- merge(extraction, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)

# 2c Name-match-only: re-run the exact+fuzzy matcher on EVERY DK buyer name, IGNORING the field CVR, so
#   buyer_cvr_final = the name-matched CVR alone. Same primitives + thresholds as 2_4; script scope (<<-).
#   Buyer schema carries no buyer_cvr_clean/valid_cvr (as extraction here); surfaced ONLY via buyer_cvr_final.
nmo <- copy(base_raw)[, .(tender_id, lot_id, buyer_number, buyer_name, buyer_country,
                          tender_publications_firstdContractAwardDate)]
.prep <- as.data.table(prepare_cvr_name(nmo$buyer_name))
nmo[, `:=`(buyer_name_basic = .prep$name_basic, buyer_name_no_spaces = .prep$name_no_spaces,
           buyer_name_broad = .prep$name_broad, buyer_name_match = .prep$name_clean,
           buyer_firm_type = .prep$firm_type)]
nmo[, `:=`(match_row_id = .I, buyer_name_in_data = buyer_name,
           match_date = as.IDate(tender_publications_firstdContractAwardDate))]
name_key   <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds")))
biname_key <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds")))
setnames(name_key, "name", "registered_name"); setnames(biname_key, "binavn", "registered_name")
name_key[, name_source := "name"]; biname_key[, name_source := "biname"]
name_key[, cvr := sprintf("%08d", as.integer(cvr))]; biname_key[, cvr := sprintf("%08d", as.integer(cvr))]
name_key[, broad_first_letter := substr(name_broad, 1, 1)]; biname_key[, broad_first_letter := substr(name_broad, 1, 1)]
cvr_key <- rbindlist(list(name_key, biname_key), use.names = TRUE)
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]
remaining <- nmo[toupper(trimws(buyer_country)) == "DK" & !is.na(buyer_name_match) & buyer_name_match != "",
  .(match_row_id, tender_id, lot_id, buyer_number, buyer_name_in_data, buyer_name_basic,
    buyer_name_no_spaces, buyer_name_broad, buyer_name_match, buyer_firm_type, match_date)]
matched <- data.table(match_row_id = integer(0), cvr_name_match = character(0),
  registered_name_match = character(0), name_match_source = character(0), name_match_step = integer(0),
  name_match_method = character(0), name_match_score = numeric(0), name_match_n_candidates = integer(0))
remaining_original <- copy(remaining)   # add_buyer_context_to_matches() reads this from the caller
.run_step <- function(on_cols, step) keep_step_matches(add_buyer_context_to_matches(
  select_preferred_exact_match(cvr_key[remaining, on = on_cols, nomatch = 0, allow.cartesian = TRUE], step = step)))
.run_step(c(name_basic = "buyer_name_basic", firm_type = "buyer_firm_type"), 1L)
.run_step(c(name_no_spaces = "buyer_name_no_spaces", firm_type = "buyer_firm_type"), 2L)
.run_step(c(name_no_spaces = "buyer_name_no_spaces"), 3L)
.run_step(c(name_broad = "buyer_name_broad"), 4L)
fuzzy_match_cols <- c("buyer_name_match", "buyer_name_broad", "buyer_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id][, match_row_id := fuzzy_match_id]
remaining_original <- copy(remaining)
matched_prefuzzy <- copy(matched)
.run_fuzzy <- function(key, ecol, kcol, flcol, step, thr) keep_step_matches(add_buyer_context_to_matches(
  accept_fuzzy_match(find_fuzzy_matches(remaining, key, entity_name_column = ecol, key_name_column = kcol,
    first_letter_column = flcol, firm_type_column = "buyer_firm_type", step = step), threshold = thr)))
.run_fuzzy(name_key,   "buyer_name_match", "name_match", "first_letter",       5L, 85)
.run_fuzzy(biname_key, "buyer_name_match", "name_match", "first_letter",       5L, 85)
.run_fuzzy(name_key,   "buyer_name_broad", "name_broad", "broad_first_letter", 6L, 86)
.run_fuzzy(biname_key, "buyer_name_broad", "name_broad", "broad_first_letter", 6L, 89)
fuzzy_matched <- matched[name_match_method == "fuzzy"]
if (nrow(fuzzy_matched) > 0)
  fuzzy_matched <- fuzzy_row_lookup[fuzzy_matched, on = .(fuzzy_match_id = match_row_id),
                                    allow.cartesian = TRUE][, fuzzy_match_id := NULL]
matched <- rbindlist(list(matched_prefuzzy, fuzzy_matched), use.names = TRUE, fill = TRUE)
km <- unique(matched[!is.na(cvr_name_match), .(match_row_id, buyer_cvr_final = cvr_name_match)])
nmo <- merge(nmo, km, by = "match_row_id", all.x = TRUE)   # PURE name match: field CVR ignored
name_match <- nmo[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
  .(tender_id, lot_id, buyer_number, buyer_cvr_final,
    cvr_number_source = "CVR from name matching only (field CVR ignored)", flag_name_match_found = TRUE)]
name_match[, dataset := "name_match"]
name_match <- unique(name_match, by = c("tender_id","lot_id","buyer_cvr_final"))
name_match <- merge(name_match, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)
rm(nmo, cvr_key, name_key, biname_key, remaining, matched); invisible(gc())

# 2b Pre-dedup reference for 98_ (does the cross-method dedup destroy data?): per-lot sorted CVR lists
#    of the production + extraction samples as built here, BEFORE the stack/dedup below. 98 compares
#    these to the final combined's cvr_method reconstruction. No-CVR rows dropped to match
#    the combine; sorted, not deduped within a lot -- a perfect reproduction of each sample's CVRs.
ref_prod <- production[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
                       .(data_source = "OpenTender", entity = "buyer", method = "production", tender_id, lot_id, cvr_final = buyer_cvr_final)]
ref_extr <- extraction[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
                       .(data_source = "OpenTender", entity = "buyer", method = "extraction", tender_id, lot_id, cvr_final = buyer_cvr_final)]
ref_nm   <- name_match[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
                       .(data_source = "OpenTender", entity = "buyer", method = "name_match", tender_id, lot_id, cvr_final = buyer_cvr_final)]
predup_ref <- rbind(ref_prod, ref_extr, ref_nm)[, .(cvr_list = paste(sort(cvr_final), collapse = ";")),
                                        by = .(data_source, entity, method, tender_id, lot_id)]
chk_dir <- file.path(clean_data_dir, "checks"); dir.create(chk_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(predup_ref, file.path(chk_dir, "predup_cvr_lists_ot_buyer.rds"))

# 3 Stack the two methods; extraction rows get NA for the production-only columns via fill = TRUE.
stacked <- rbindlist(list(production, extraction, name_match), use.names = TRUE, fill = TRUE)
stacked[, dataset := factor(dataset, levels = c("production","extraction","name_match"))]

# 4 CVR-level dedup -> ONE row per distinct (tender_id, lot_id, buyer_cvr_final). A CVR is in the
#   production sample if it appears among the production rows and in the extraction sample if it appears
#   among the extraction rows. `cvr_method` lists the method(s) that produced this CVR, ";"-joined in
#   production-before-extraction order: "production", "extraction", or "production; extraction". Rebuild each
#   sample by a grepl filter -- production = grepl("production", cvr_method); extraction = grepl("extraction",
#   cvr_method). Where both methods produced the same CVR the two rows collapse to ONE (unique on (tender,
#   lot, CVR)); the production copy is kept (richer metadata; extraction rows are thin) by ordering production
#   ahead of extraction before unique().
setorder(stacked, tender_id, lot_id, buyer_cvr_final, dataset)
stacked[, cvr_method := paste(unique(dataset), collapse = "; "), by = .(tender_id, lot_id, buyer_cvr_final)]
stacked_deduped <- unique(stacked, by = c("tender_id", "lot_id", "buyer_cvr_final"))

# 5 Carry every analytical/provenance column through to 4_combine, which applies the single final
#    column selection (keep_cols). Removed here: the internal dedup scratch, the raw source-dump
#    columns (raw_dump_cols(): negative pattern drop of known junk), and OpenTender's winner_*
#    source artifact (buyer rows must not carry winner identity).
drop_now <- unique(c(grep("^winner_", names(stacked_deduped), value = TRUE),
                     raw_dump_cols(names(stacked_deduped))))
if (length(drop_now)) stacked_deduped[, (drop_now) := NULL]
lead <- intersect(c("dataset","cvr_method","tender_id","lot_id","buyer_number",
                    "buyer_name","buyer_cvr_final"), names(stacked_deduped))
setcolorder(stacked_deduped, c(lead, setdiff(names(stacked_deduped), lead)))

# 6 Self-check: cvr_method must reproduce the production and extraction samples exactly.
#   Per tender-lot, compare the sorted CVR list of each original sample against its rebuilt version; halt if off.
.prod_orig  <- production[,     .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_orig  <- extraction[,     .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.nm_orig    <- name_match[,      .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.prod_built <- stacked_deduped[grepl("production", cvr_method), .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_built <- stacked_deduped[grepl("extraction", cvr_method), .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.nm_built   <- stacked_deduped[grepl("name_match", cvr_method), .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.pm <- merge(.prod_orig, .prod_built, by = c("tender_id","lot_id"), all = TRUE, suffixes = c("_orig","_built"))
.em <- merge(.extr_orig, .extr_built, by = c("tender_id","lot_id"), all = TRUE, suffixes = c("_orig","_built"))
.nmm <- merge(.nm_orig, .nm_built, by = c("tender_id","lot_id"), all = TRUE, suffixes = c("_orig","_built"))
if (anyNA(.pm$l_orig) || anyNA(.pm$l_built) || !all(.pm$l_orig == .pm$l_built))
  stop("cvr_method does not reproduce the production sample (tender-lot CVR-list mismatch).", call. = FALSE)
if (anyNA(.em$l_orig) || anyNA(.em$l_built) || !all(.em$l_orig == .em$l_built))
  stop("cvr_method does not reproduce the extraction sample (tender-lot CVR-list mismatch).", call. = FALSE)
if (anyNA(.nmm$l_orig) || anyNA(.nmm$l_built) || !all(.nmm$l_orig == .nmm$l_built))
  stop("cvr_method does not reproduce the name_match sample (tender-lot CVR-list mismatch).", call. = FALSE)
cat("  self-check passed: cvr_method rebuilds production, extraction and name_match exactly.\n")

out_path <- Sys.getenv("OT_BUYER_STACK_OUT", unset = file.path(clean_data_dir, "ot_buyer_datasets_stacked.rds"))
save_dataset(stacked_deduped, out_path)   # .rds (canonical) + .csv + .parquet
cat(sprintf("ot_buyer_datasets_stacked.rds: %d rows, %d cols\n", nrow(stacked_deduped), ncol(stacked_deduped)))
print(stacked_deduped[, .N, by = dataset][order(dataset)])
cat(sprintf("  rebuild: production=%d | extraction=%d | name_match=%d\n",
            sum(grepl("production", stacked_deduped$cvr_method)),
            sum(grepl("extraction", stacked_deduped$cvr_method)),
            sum(grepl("name_match", stacked_deduped$cvr_method))))
