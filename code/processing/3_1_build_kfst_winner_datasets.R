# Concatenate the KFST winner data from the two CVR-resolution methods -- 'production' (the
# consortium/name-matched winners from 2_1) and 'extraction' (raw 8-digit CVRs pulled straight from the
# winner field, no matching) -- reduced to one row per distinct (tender_id, lot_id, CVR).
# Author: Jack Mulqueeney. Date: 8 Sep 2026.
#
# Grain: one row per distinct (tender_id, lot_id, winner_cvr_final). Rows with no resolved CVR are
# dropped here (they remain in clean_winner_data_kfst_name_matched.rds); consortium members that share a
# CVR collapse to one row (branch names are not needed downstream -- only the tender-lot-CVR triplet is).
#
# OUTPUT (data/clean/kfst_winner_datasets_stacked.rds):
#   dataset     : "production" | "extraction" | "name_match" -- the method that produced the surviving row.
#   cvr_method  : method(s) that produced the CVR, ";"-joined (e.g. "production", "production; extraction",
#                 "production; name_match", "production; extraction; name_match").
#     Rebuild each sample EXACTLY via grepl: production/extraction/name_match = grepl(<m>, cvr_method).
#     name_match = re-run name-match-only CVRs (field CVR ignored); generated inline (section 2c).

# 0 Prelims
rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean_data_dir <- dirs$clean_data
base_raw <- as.data.table(readRDS(file.path(clean_data_dir, "clean_winner_data_kfst_name_matched.rds")))

# ── Whole-result stack cache: skip the build when the matched input + keys are unchanged ──
# (see match_cache_read/write in functions.R). Empty refresh -> exact skip on identical input, full
# rebuild on any change. Version ties to the CVR keys' mtime. Disable with MATCH_CACHE=false. Correctness
# gated by the archive diff (a cache-off run must reproduce the cached output bit-for-bit).
match_cache_ver  <- paste0("p2-", key_sig(clean_data_dir))  # p2: name_match slice now carries fuzzy_candidate_cvr_2/_score_2
match_cache_file <- file.path(dirs$intermediates, "match_cache", "stack__kfst_winner.rds")
match_input      <- copy(base_raw)
.hit <- match_cache_read(match_input, character(0), character(0), match_cache_file, match_cache_ver)
if (!is.null(.hit)) {
  cat("Stack cache HIT -> skipping KFST winner stack build\n")
  .chk <- file.path(clean_data_dir, "checks"); dir.create(.chk, showWarnings = FALSE, recursive = TRUE)
  saveRDS(.hit$extra, file.path(.chk, "predup_cvr_lists_kfst_winner.rds"))
  save_dataset(.hit$output, Sys.getenv("KFST_STACK_OUT",
    unset = file.path(clean_data_dir, "kfst_winner_datasets_stacked.rds")))
  quit(save = "no")
}
cat("Stack cache MISS -> building KFST winner stack\n")

# 1 Production: the consortium/name-matched KFST winners from 2_1, dropped to rows with a resolved CVR
#   and collapsed to distinct (tender_id, lot_id, winner_cvr_final) -- keeps one representative member row.
production <- unique(copy(base_raw)[!is.na(winner_cvr_final) & winner_cvr_final != ""],
                     by = c("tender_id", "lot_id", "winner_cvr_final"))
production[, dataset := "production"]

# Lot-level context (shared-schema columns constant within a tender-lot) to attach to the extraction
# rows. Built from base_raw so every lot has context, even lots whose production winners were all dropped.
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

# 2 Extraction: every standalone 8-digit CVR in the raw winner field, no matching. lot_field_cvrs() is
#   the shared functions.R helper (returns distinct tender-lot-CVR triples).
extraction <- as.data.table(lot_field_cvrs(
  unique(base_raw[, .(tender_id, lot_id, winner_cvr = winner_cvr_original)])))
extraction[, winner_number := rowid(tender_id, lot_id)]
extraction[, `:=`(winner_cvr_final = cvr, winner_cvr_clean = cvr, valid_cvr = TRUE,
                  cvr_number_source = "CVR from the winner field: raw extraction (no matching)",
                  flag_name_match_found = FALSE)]
extraction[, cvr := NULL][, dataset := "extraction"]
extraction <- merge(extraction, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)

# 2c Name-match-only: re-run the exact+fuzzy matcher on EVERY DK winner name, IGNORING the field CVR, so
#   winner_cvr_final = the name-matched CVR alone (the mirror image of extraction). Same primitives +
#   thresholds (85/85/86/89) as 2_1; runs at script scope because keep_step_matches() uses <<-. The
#   name-match CVR is surfaced ONLY via winner_cvr_final (winner_cvr_clean := NA) so no new CVR column is
#   introduced. DK-gated; match_date = pub_date (same publication window 2_1 uses).
nmo <- copy(base_raw)[, .(tender_id, lot_id, winner_number, winner_name, winner_country, pub_date)]
.prep <- as.data.table(prepare_cvr_name(nmo$winner_name))
nmo[, `:=`(winner_name_basic = .prep$name_basic, winner_name_no_spaces = .prep$name_no_spaces,
           winner_name_broad = .prep$name_broad, winner_name_match = .prep$name_clean,
           winner_firm_type = .prep$firm_type)]
# Matching date = pub_date: KFST's most-available date (93% vs award_date 84%). Used ONLY for the
# +/-2y CVR registry-validity window in matching, so pub-vs-award (~3wk median gap) is immaterial to matches.
nmo[, `:=`(match_row_id = .I, winner_name_in_data = winner_name, match_date = as.IDate(pub_date))]
name_key   <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds")))
biname_key <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds")))
setnames(name_key, "name", "registered_name"); setnames(biname_key, "binavn", "registered_name")
name_key[, name_source := "name"]; biname_key[, name_source := "biname"]
name_key[, cvr := sprintf("%08d", as.integer(cvr))]; biname_key[, cvr := sprintf("%08d", as.integer(cvr))]
name_key[, broad_first_letter := substr(name_broad, 1, 1)]; biname_key[, broad_first_letter := substr(name_broad, 1, 1)]
cvr_key <- rbindlist(list(name_key, biname_key), use.names = TRUE)
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]
remaining <- nmo[toupper(trimws(winner_country)) == "DK" & !is.na(winner_name_match) & winner_name_match != "",
  .(match_row_id, tender_id, lot_id, winner_number, winner_name_in_data, winner_name_basic,
    winner_name_no_spaces, winner_name_broad, winner_name_match, winner_firm_type, match_date)]
matched <- data.table(match_row_id = integer(0), cvr_name_match = character(0),
  registered_name_match = character(0), name_match_source = character(0), name_match_step = integer(0),
  name_match_method = character(0), name_match_score = numeric(0), name_match_n_candidates = integer(0))
remaining_original <- copy(remaining)   # add_winner_context_to_matches() reads this from the caller
.run_step <- function(on_cols, step) keep_step_matches(add_winner_context_to_matches(
  select_preferred_exact_match(cvr_key[remaining, on = on_cols, nomatch = 0, allow.cartesian = TRUE], step = step)))
.run_step(c(name_basic = "winner_name_basic", firm_type = "winner_firm_type"), 1L)
.run_step(c(name_no_spaces = "winner_name_no_spaces", firm_type = "winner_firm_type"), 2L)
.run_step(c(name_no_spaces = "winner_name_no_spaces"), 3L)
.run_step(c(name_broad = "winner_name_broad"), 4L)
fuzzy_match_cols <- c("winner_name_match", "winner_name_broad", "winner_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id][, match_row_id := fuzzy_match_id]
remaining_original <- copy(remaining)
matched_prefuzzy <- copy(matched)
# Fuzzy match cache (see find_fuzzy_matches): caches the name-match-only fuzzy pass, shared with the 2_*
# matchers where the (name, firm_type, match_date) problem is identical. Disable with MATCH_CACHE=false.
if (tolower(Sys.getenv("MATCH_CACHE", "true")) != "false")
  options(cvr.fuzzy_cache_dir = file.path(dirs$intermediates, "match_cache"),
          cvr.fuzzy_cache_version = paste0("p1-", key_sig(clean_data_dir)))
# Collect fuzzy candidate long-tables (not just the accepted match) so the name_match slice can carry the
# runner-up (rank-2) -- lets name_match-only fuzzy rows deliver fuzzy_candidate_cvr_2 / _score_2.
fuzzy_cands <- list()
.run_fuzzy <- function(key, ecol, kcol, flcol, step, thr) {
  fc <- find_fuzzy_matches(remaining, key, entity_name_column = ecol, key_name_column = kcol,
    first_letter_column = flcol, firm_type_column = "winner_firm_type", step = step,
    key_id = if (key$name_source[1] == "name") "name_key" else "biname_key")
  fuzzy_cands[[length(fuzzy_cands) + 1L]] <<- fc
  keep_step_matches(add_winner_context_to_matches(accept_fuzzy_match(fc, threshold = thr)))
}
.run_fuzzy(name_key,   "winner_name_match", "name_match", "first_letter",       5L, 85)
.run_fuzzy(biname_key, "winner_name_match", "name_match", "first_letter",       5L, 85)
.run_fuzzy(name_key,   "winner_name_broad", "name_broad", "broad_first_letter", 6L, 86)
.run_fuzzy(biname_key, "winner_name_broad", "name_broad", "broad_first_letter", 6L, 89)
fuzzy_matched <- matched[name_match_method == "fuzzy"]
if (nrow(fuzzy_matched) > 0)
  fuzzy_matched <- fuzzy_row_lookup[fuzzy_matched, on = .(fuzzy_match_id = match_row_id),
                                    allow.cartesian = TRUE][, fuzzy_match_id := NULL]
matched <- rbindlist(list(matched_prefuzzy, fuzzy_matched), use.names = TRUE, fill = TRUE)

# Runner-up (rank-2) wide candidate columns for the name_match slice (mirrors the production matcher):
# map fuzzy candidates back to real rows, keep top-5 distinct CVRs by score, attach cvr_2 / score_2.
fuzzy_candidates <- rbindlist(fuzzy_cands, use.names = TRUE, fill = TRUE)
if (nrow(fuzzy_candidates) > 0) {
  setnames(fuzzy_candidates, "match_row_id", "fuzzy_match_id")
  fuzzy_candidates <- fuzzy_row_lookup[fuzzy_candidates, on = "fuzzy_match_id",
                                       allow.cartesian = TRUE][, fuzzy_match_id := NULL]
  fuzzy_candidates[, source_order := fifelse(fuzzy_candidate_source == "name", 1L, 2L)]
  setorder(fuzzy_candidates, match_row_id, -fuzzy_candidate_score, fuzzy_candidate_step,
           source_order, fuzzy_candidate_rank)
  fuzzy_candidates <- unique(fuzzy_candidates, by = c("match_row_id", "fuzzy_candidate_cvr"))
  fuzzy_candidates <- fuzzy_candidates[, head(.SD, 5), by = match_row_id]
  fuzzy_candidates[, fuzzy_candidate_rank := seq_len(.N), by = match_row_id]
  fcw <- dcast(fuzzy_candidates, match_row_id ~ fuzzy_candidate_rank,
               value.var = c("fuzzy_candidate_cvr", "fuzzy_candidate_score"))
  keep2 <- intersect(c("match_row_id", "fuzzy_candidate_cvr_2", "fuzzy_candidate_score_2"), names(fcw))
  nmo <- merge(nmo, fcw[, ..keep2], by = "match_row_id", all.x = TRUE, sort = FALSE)
}
for (cc in c("fuzzy_candidate_cvr_2", "fuzzy_candidate_score_2"))
  if (!cc %in% names(nmo)) nmo[, (cc) := if (cc == "fuzzy_candidate_score_2") NA_real_ else NA_character_]

km <- unique(matched[!is.na(cvr_name_match), .(match_row_id, winner_cvr_final = cvr_name_match,
  name_match_source, name_match_step, name_match_method, name_match_score)])
nmo <- merge(nmo, km, by = "match_row_id", all.x = TRUE)   # PURE name match: field CVR ignored
for (qf in list(c("winner_name_match","name_match","cvr_name_match_quality"),
                c("winner_name_basic","name_basic","cvr_name_match_quality_basic"),
                c("winner_name_no_spaces","name_no_spaces","cvr_name_match_quality_nospaces"),
                c("winner_name_broad","name_broad","cvr_name_match_quality_broad"))) {
  .wc <- qf[1]; .kc <- qf[2]; .qc <- qf[3]
  .rl <- unique(data.table(cvr = as.character(cvr_key$cvr), reg_name = cvr_key[[.kc]]))[!is.na(reg_name) & reg_name != ""]
  .q  <- data.table(match_row_id = nmo$match_row_id, cvr = as.character(nmo$winner_cvr_final), win_name = nmo[[.wc]])
  .q  <- merge(.q[!is.na(cvr) & cvr != "" & !is.na(win_name) & win_name != ""], .rl, by = "cvr", allow.cartesian = TRUE)
  .q[, score := levenshtein_ratio(win_name, reg_name, pairwise = TRUE)]
  setorder(.q, match_row_id, -score); .bq <- .q[, .SD[1L], by = match_row_id]
  nmo[, (.qc) := NA_real_]; nmo[.bq, on = "match_row_id", (.qc) := i.score]
  if (.qc == "cvr_name_match_quality") { nmo[, cvr_name_match_quality_name := NA_character_]
    nmo[.bq, on = "match_row_id", cvr_name_match_quality_name := i.reg_name] }
}
nmo[, cvr_name_is_substring := NA]
nmo[!is.na(cvr_name_match_quality_name) & !is.na(winner_name_match) & winner_name_match != "",
    cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(winner_name_match))]
name_match <- nmo[!is.na(winner_cvr_final) & winner_cvr_final != "",
  .(tender_id, lot_id, winner_number, winner_name, winner_country, winner_cvr_final,
    winner_cvr_clean = NA_character_, valid_cvr = TRUE, name_match_method, name_match_step,
    cvr_number_source = "CVR from name matching only (field CVR ignored)", matching_candidate_type = NA_character_,
    name_match_score, flag_name_match_found = TRUE, cvr_name_match_quality, cvr_name_match_quality_basic,
    cvr_name_match_quality_nospaces, cvr_name_match_quality_broad, cvr_name_match_quality_name, cvr_name_is_substring,
    fuzzy_candidate_cvr_2, fuzzy_candidate_score_2)]
name_match[, dataset := "name_match"]
name_match <- unique(name_match, by = c("tender_id","lot_id","winner_cvr_final"))
name_match <- merge(name_match, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)
rm(nmo, cvr_key, name_key, biname_key, remaining, matched); invisible(gc())

# 2b Pre-dedup reference for 98_ (does the cross-method dedup destroy data?): per-lot sorted CVR lists
#    of the production + extraction samples as built here, BEFORE the stack/dedup below. 98 compares
#    these to the final combined's cvr_method reconstruction. No-CVR rows dropped to match
#    the combine; sorted, not deduped within a lot -- a perfect reproduction of each sample's CVRs.
ref_prod <- production[!is.na(winner_cvr_final) & winner_cvr_final != "",
                       .(data_source = "KFST", entity = "winner", method = "production", tender_id, lot_id, cvr_final = winner_cvr_final)]
ref_extr <- extraction[!is.na(winner_cvr_final) & winner_cvr_final != "",
                       .(data_source = "KFST", entity = "winner", method = "extraction", tender_id, lot_id, cvr_final = winner_cvr_final)]
ref_nm   <- name_match[!is.na(winner_cvr_final) & winner_cvr_final != "",
                       .(data_source = "KFST", entity = "winner", method = "name_match", tender_id, lot_id, cvr_final = winner_cvr_final)]
predup_ref <- rbind(ref_prod, ref_extr, ref_nm)[, .(cvr_list = paste(sort(cvr_final), collapse = ";")),
                                        by = .(data_source, entity, method, tender_id, lot_id)]
chk_dir <- file.path(clean_data_dir, "checks"); dir.create(chk_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(predup_ref, file.path(chk_dir, "predup_cvr_lists_kfst_winner.rds"))

# 3 Stack the two methods; extraction rows get NA for the production-only columns via fill = TRUE.
stacked <- rbindlist(list(production, extraction, name_match), use.names = TRUE, fill = TRUE)
stacked[, dataset := factor(dataset, levels = c("production","extraction","name_match"))]
stacked[, is_awarded_winner := awarded_winner(stacked)]   # flag_awarded (stacks carry no is_winner)

# 4 CVR-level dedup -> ONE row per distinct (tender_id, lot_id, winner_cvr_final). `cvr_method` lists the
#   method(s) that produced this CVR, ";"-joined in production-before-extraction order (dataset is a factor
#   with production < extraction, so setorder puts it first): "production", "extraction", or
#   "production; extraction". Rebuild each sample by a grepl filter -- production = grepl("production",
#   cvr_method); extraction = grepl("extraction", cvr_method). Where both methods produced the same CVR the
#   two rows collapse to ONE (unique on (tender, lot, CVR), no duplicates); the production copy is kept
#   (richer metadata; extraction rows are thin) by ordering production ahead of extraction before unique().
setorder(stacked, tender_id, lot_id, winner_cvr_final, dataset)
stacked[, cvr_method := paste(unique(dataset), collapse = "; "), by = .(tender_id, lot_id, winner_cvr_final)]
stacked_deduped <- unique(stacked, by = c("tender_id", "lot_id", "winner_cvr_final"))

# 5 Carry every analytical/provenance column through to 4_combine, which applies the single final
#    column selection (keep_cols). Only the internal dedup scratch + the raw source-dump columns
#    (raw_dump_cols(): a negative pattern drop of known junk) are removed here.
drop_now <- unique(raw_dump_cols(names(stacked_deduped)))
if (length(drop_now)) stacked_deduped[, (drop_now) := NULL]
lead <- intersect(c("dataset","cvr_method","tender_id","lot_id","winner_number",
                    "winner_name","winner_cvr_final","is_awarded_winner"), names(stacked_deduped))
setcolorder(stacked_deduped, c(lead, setdiff(names(stacked_deduped), lead)))

# 6 Self-check: cvr_method must reproduce the production and extraction samples exactly.
#   Per tender-lot, compare the sorted CVR list of each original sample against its rebuilt version; halt if off.
.prod_orig  <- production[,     .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_orig  <- extraction[,     .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.nm_orig    <- name_match[,      .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.prod_built <- stacked_deduped[grepl("production", cvr_method), .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_built <- stacked_deduped[grepl("extraction", cvr_method), .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.nm_built   <- stacked_deduped[grepl("name_match", cvr_method), .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
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

out_path <- Sys.getenv("KFST_STACK_OUT", unset = file.path(clean_data_dir, "kfst_winner_datasets_stacked.rds"))
save_dataset(stacked_deduped, out_path)   # .rds (canonical) + .csv + .parquet
cat(sprintf("kfst_winner_datasets_stacked.rds: %d rows, %d cols\n", nrow(stacked_deduped), ncol(stacked_deduped)))

# Persist the whole stack so an unchanged-input rerun can skip this build entirely.
match_cache_write(match_input, stacked_deduped, character(0), match_cache_file, match_cache_ver,
                  extra = predup_ref)
print(stacked_deduped[, .N, by = dataset][order(dataset)])
cat(sprintf("  rebuild: production=%d | extraction=%d | name_match=%d\n",
            sum(grepl("production", stacked_deduped$cvr_method)),
            sum(grepl("extraction", stacked_deduped$cvr_method)),
            sum(grepl("name_match", stacked_deduped$cvr_method))))
