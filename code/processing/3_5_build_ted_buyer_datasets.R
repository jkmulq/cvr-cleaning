# TED BUYER concatenate-and-dedup -- the TED buyer analogue of 3_3, with all THREE CVR-resolution methods:
#   production  : the XML-resolved buyer CVR from ted_5 (buyer_cvr_final).
#   extraction  : raw 8-digit CVRs pulled straight from the buyer field (buyer_cvr_original), no matching.
#   name_match  : re-run name-match-only CVRs (XML/field CVR IGNORED); generated inline (section 2c).
# Reduced to one row per distinct (tender_id, lot_id, buyer_cvr_final). Author: Jack Mulqueeney.
#
# OUTPUT (data/clean/ted_buyer_datasets_stacked.rds):
#   dataset    : "production" | "extraction" | "name_match" -- the method that produced the surviving row.
#   cvr_method : method(s) that produced the CVR, ";"-joined per (tender,lot,CVR). Rebuild each sample
#                EXACTLY via grepl(<m>, cvr_method). name_match ignores the field/XML CVR.

rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean_data_dir <- dirs$clean_data

base_raw <- as.data.table(readRDS(file.path(clean_data_dir, "clean_buyer_data_ted_name_matched.rds")))

# ── Whole-result stack cache: skip the build when inputs are unchanged (see functions.R). This builder also
# reads the raw ted_buyer_data (for the name-match pass), so the version folds its CONTENT hash alongside
# the CVR keys' mtime. Empty refresh -> exact skip on identical input, full rebuild on any change.
.nmo_f <- file.path(dirs$intermediates, "ted", "ted_buyer_data.rds")
.nmo_h <- if (file.exists(.nmo_f)) substr(rlang::hash(readRDS(.nmo_f)), 1, 12) else "nonmo"
match_cache_ver  <- paste0("p1-", key_sig(clean_data_dir), "-", .nmo_h)
match_cache_file <- file.path(dirs$intermediates, "match_cache", "stack__ted_buyer.rds")
match_input      <- copy(base_raw)
.hit <- match_cache_read(match_input, character(0), character(0), match_cache_file, match_cache_ver)
if (!is.null(.hit)) {
  cat("Stack cache HIT -> skipping TED buyer stack build\n")
  .chk <- file.path(clean_data_dir, "checks"); dir.create(.chk, showWarnings = FALSE, recursive = TRUE)
  saveRDS(.hit$extra, file.path(.chk, "predup_cvr_lists_ted_buyer.rds"))
  save_dataset(.hit$output, Sys.getenv("TED_BUYER_STACK_OUT",
    unset = file.path(clean_data_dir, "ted_buyer_datasets_stacked.rds")))
  quit(save = "no")
}
cat("Stack cache MISS -> building TED buyer stack\n")

# 1 Production: the XML-resolved TED buyers from ted_5, dropped to rows with a resolved CVR and collapsed
#   to distinct (tender_id, lot_id, buyer_cvr_final).
production <- unique(base_raw[!is.na(buyer_cvr_final) & buyer_cvr_final != ""],
                     by = c("tender_id", "lot_id", "buyer_cvr_final"))
production[, dataset := "production"]

# Lot-level context (constant within a tender-lot) to attach to the extraction / name_match rows.
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

# 2 Extraction: raw 8-digit CVRs from the buyer field, no matching. lot_field_cvrs() keys on a column
#   literally named winner_cvr, so the buyer field is aliased to it. Buyer schema has no valid_cvr /
#   buyer_cvr_clean, so extraction sets only buyer_cvr_final + provenance.
extraction <- as.data.table(lot_field_cvrs(
  unique(base_raw[, .(tender_id, lot_id, winner_cvr = buyer_cvr_original)])))
extraction[, buyer_number := rowid(tender_id, lot_id)]
extraction[, `:=`(buyer_cvr_final = cvr,
                  cvr_number_source = "CVR from the buyer field: raw extraction (no matching)",
                  flag_name_match_found = FALSE)]
extraction[, cvr := NULL][, dataset := "extraction"]
extraction <- merge(extraction, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)

# 2c Name-match-only: re-run the exact+fuzzy matcher on EVERY DK buyer name from ted_3, IGNORING the XML CVR
#   (buyer_cvr_final = the name-matched CVR alone). Same primitives + thresholds as ted_5; script scope (<<-).
#   Reads ted_buyer_data.rds (ted_3); lot context comes from lot_ctx. Surfaced ONLY via buyer_cvr_final.
ted_dir <- file.path(dirs$intermediates, "ted")
nmo <- as.data.table(readRDS(file.path(ted_dir, "ted_buyer_data.rds")))
# lot_id must be the CANONICAL lot id (1..N, as in ted_3/ted_4/base_raw), NOT the raw TED lot code `lot` --
# else the lot_ctx merge (dates/amounts/cpv/procedure) misses and name_match buyer rows come out context-less.
nmo[, `:=`(tender_id = as.character(notice_id), lot_id = as.character(lot_id),
           buyer_number = rowid(notice_id, lot))]
bp <- prepare_cvr_name(nmo$buyer_name)
nmo[, `:=`(buyer_name_basic = bp$name_basic, buyer_name_match = bp$name_clean,
           buyer_name_no_spaces = bp$name_no_spaces, buyer_name_broad = bp$name_broad,
           buyer_firm_type = bp$firm_type, first_letter = substr(bp$name_clean, 1, 1),
           broad_first_letter = substr(bp$name_broad, 1, 1))]
nmo[, `:=`(flag_matching_candidate = !is.na(buyer_name) & buyer_name != "",
           match_row_id = .I, buyer_name_in_data = buyer_name)]
name_key   <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds")))
biname_key <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds")))
setnames(name_key, "name", "registered_name"); setnames(biname_key, "binavn", "registered_name")
name_key[, name_source := "name"]; biname_key[, name_source := "biname"]
name_key[, cvr := sprintf("%08d", as.integer(cvr))]; biname_key[, cvr := sprintf("%08d", as.integer(cvr))]
name_key[, broad_first_letter := substr(name_broad, 1, 1)]; biname_key[, broad_first_letter := substr(name_broad, 1, 1)]
cvr_key <- rbindlist(list(name_key, biname_key), use.names = TRUE)
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]
remaining <- nmo[flag_matching_candidate & grepl("DK|DNK", toupper(trimws(buyer_country)))]
# Matching date = contract-award date: TED's most-available award date. Used ONLY for the +/-2y CVR
# registry-validity window in matching (see KFST note in 2_1 on the pub-vs-award gap).
remaining[, match_date := as.IDate(date_contract_award)]
remaining_original <- remaining
matched <- data.table(match_row_id = integer(0), cvr_name_match = character(0),
  registered_name_match = character(0), name_match_source = character(0), name_match_step = integer(0),
  name_match_method = character(0), name_match_score = numeric(0), name_match_n_candidates = integer(0))
.run_step <- function(on_cols, step) keep_step_matches(add_buyer_context_to_matches(
  select_preferred_exact_match(cvr_key[remaining, on = on_cols, nomatch = 0, allow.cartesian = TRUE], step = step)))
.run_step(c(name_basic = "buyer_name_basic", firm_type = "buyer_firm_type"), 1L)
.run_step(c(name_no_spaces = "buyer_name_no_spaces", firm_type = "buyer_firm_type"), 2L)
.run_step(c(name_no_spaces = "buyer_name_no_spaces"), 3L)
.run_step(c(name_broad = "buyer_name_broad"), 4L)
cvr_key_quality <- unique(cvr_key[, .(cvr, name_match, name_basic, name_no_spaces, name_broad)])
fuzzy_match_cols <- c("buyer_name_match", "buyer_name_broad", "buyer_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id][, match_row_id := fuzzy_match_id]
matched_prefuzzy <- copy(matched)
# Fuzzy match cache (see find_fuzzy_matches): caches the name-match-only fuzzy pass, shared with the 2_*
# matchers where the (name, firm_type, match_date) problem is identical. Disable with MATCH_CACHE=false.
if (tolower(Sys.getenv("MATCH_CACHE", "true")) != "false")
  options(cvr.fuzzy_cache_dir = file.path(dirs$intermediates, "match_cache"),
          cvr.fuzzy_cache_version = paste0("p1-", key_sig(clean_data_dir)))
.run_fuzzy <- function(key, ecol, kcol, flcol, step, thr) keep_step_matches(add_buyer_context_to_matches(
  accept_fuzzy_match(find_fuzzy_matches(remaining, key, entity_name_column = ecol, key_name_column = kcol,
    first_letter_column = flcol, firm_type_column = "buyer_firm_type", step = step,
    key_id = if (key$name_source[1] == "name") "name_key" else "biname_key"), threshold = thr)))
.run_fuzzy(name_key,   "buyer_name_match", "name_match", "first_letter",       5L, 85)
.run_fuzzy(biname_key, "buyer_name_match", "name_match", "first_letter",       5L, 85)
.run_fuzzy(name_key,   "buyer_name_broad", "name_broad", "broad_first_letter", 6L, 86)
.run_fuzzy(biname_key, "buyer_name_broad", "name_broad", "broad_first_letter", 6L, 89)
fuzzy_matched <- matched[name_match_method == "fuzzy"]
if (nrow(fuzzy_matched) > 0)
  fuzzy_matched <- fuzzy_row_lookup[fuzzy_matched, on = .(fuzzy_match_id = match_row_id),
                                    allow.cartesian = TRUE][, fuzzy_match_id := NULL]
matched <- rbindlist(list(matched_prefuzzy, fuzzy_matched), use.names = TRUE, fill = TRUE)
nmo[matched, on = "match_row_id", `:=`(buyer_cvr_final = i.cvr_name_match,
  name_match_source = i.name_match_source, name_match_step = i.name_match_step,
  name_match_method = i.name_match_method, name_match_score = i.name_match_score,
  name_match_n_candidates = i.name_match_n_candidates)]
nmo[, matching_candidate_type := fcase(
  flag_matching_candidate & toupper(trimws(buyer_country)) %chin% c("DK","DNK"), "exact DK",
  flag_matching_candidate & grepl("DK|DNK", toupper(trimws(buyer_country))), "contains DK",
  default = NA_character_)]
nmo[, flag_name_match_found := !is.na(buyer_cvr_final)]
nmo[, flag_name_match_ambiguous := flag_name_match_found & name_match_n_candidates > 1]
nmo[, flag_review_name_match := flag_name_match_found & (name_match_method == "fuzzy" | flag_name_match_ambiguous)]
nmo[, name_match_status := fcase(
  !flag_matching_candidate, "not requested",
  flag_review_name_match, "manual review - fuzzy or ambiguous match",
  flag_name_match_found, "matched",
  is.na(buyer_country) | !grepl("DK|DNK", toupper(trimws(buyer_country))), "manual review - not marked as Danish",
  default = "manual review - no automatic match")]
for (qf in list(c("buyer_name_match","name_match","cvr_name_match_quality"),
                c("buyer_name_basic","name_basic","cvr_name_match_quality_basic"),
                c("buyer_name_no_spaces","name_no_spaces","cvr_name_match_quality_nospaces"),
                c("buyer_name_broad","name_broad","cvr_name_match_quality_broad"))) {
  .bc <- qf[1]; .kc <- qf[2]; .qc <- qf[3]
  .rl <- unique(data.table(cvr = as.character(cvr_key_quality$cvr), reg_name = cvr_key_quality[[.kc]]))[!is.na(reg_name) & reg_name != ""]
  .q  <- data.table(match_row_id = nmo$match_row_id, cvr = as.character(nmo$buyer_cvr_final), buy_name = nmo[[.bc]])
  .q  <- merge(.q[!is.na(cvr) & cvr != "" & !is.na(buy_name) & buy_name != ""], .rl, by = "cvr", allow.cartesian = TRUE)
  .q[, score := levenshtein_ratio(buy_name, reg_name, pairwise = TRUE)]
  setorder(.q, match_row_id, -score); .bq <- .q[, .SD[1L], by = match_row_id]
  nmo[, (.qc) := NA_real_]; nmo[.bq, on = "match_row_id", (.qc) := i.score]
  if (.qc == "cvr_name_match_quality") { nmo[, cvr_name_match_quality_name := NA_character_]
    nmo[.bq, on = "match_row_id", cvr_name_match_quality_name := i.reg_name] }
}
nmo[, cvr_name_is_substring := NA]
nmo[!is.na(cvr_name_match_quality_name) & !is.na(buyer_name_match) & buyer_name_match != "",
    cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(buyer_name_match))]
name_match <- nmo[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
  .(tender_id, lot_id, buyer_number, buyer_name, buyer_country, buyer_cvr_final,
    name_match_method, name_match_step, name_match_source, name_match_score, name_match_n_candidates,
    matching_candidate_type, flag_name_match_found,
    cvr_number_source = "CVR from name matching only (field CVR ignored)", cvr_name_match_quality,
    cvr_name_match_quality_basic, cvr_name_match_quality_nospaces, cvr_name_match_quality_broad,
    cvr_name_match_quality_name, cvr_name_is_substring, name_match_status)]
name_match[, dataset := "name_match"]
name_match <- unique(name_match, by = c("tender_id","lot_id","buyer_cvr_final"))
name_match <- merge(name_match, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)
rm(nmo, cvr_key, cvr_key_quality, name_key, biname_key, remaining, matched); invisible(gc())

# 2b Pre-dedup reference for 98_.
ref_prod <- production[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
  .(data_source = "TED", entity = "buyer", method = "production", tender_id, lot_id, cvr_final = buyer_cvr_final)]
ref_extr <- extraction[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
  .(data_source = "TED", entity = "buyer", method = "extraction", tender_id, lot_id, cvr_final = buyer_cvr_final)]
ref_nm   <- name_match[!is.na(buyer_cvr_final) & buyer_cvr_final != "",
  .(data_source = "TED", entity = "buyer", method = "name_match", tender_id, lot_id, cvr_final = buyer_cvr_final)]
predup_ref <- rbind(ref_prod, ref_extr, ref_nm)[, .(cvr_list = paste(sort(cvr_final), collapse = ";")),
                                        by = .(data_source, entity, method, tender_id, lot_id)]
chk_dir <- file.path(clean_data_dir, "checks"); dir.create(chk_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(predup_ref, file.path(chk_dir, "predup_cvr_lists_ted_buyer.rds"))

# 3 Stack the three methods; missing columns get NA via fill = TRUE.
stacked <- rbindlist(list(production, extraction, name_match), use.names = TRUE, fill = TRUE)
stacked[, dataset := factor(dataset, levels = c("production","extraction","name_match"))]

# 4 CVR-level dedup -> ONE row per distinct (tender_id, lot_id, buyer_cvr_final).
setorder(stacked, tender_id, lot_id, buyer_cvr_final, dataset)
stacked[, cvr_method := paste(unique(dataset), collapse = "; "), by = .(tender_id, lot_id, buyer_cvr_final)]
stacked_deduped <- unique(stacked, by = c("tender_id", "lot_id", "buyer_cvr_final"))

# 5 Carry every analytical/provenance column through to 4_combine; drop dedup scratch, raw junk, and any
#   winner_* artifact (buyer rows must not carry winner identity).
drop_now <- unique(c(grep("^winner_", names(stacked_deduped), value = TRUE),
                     raw_dump_cols(names(stacked_deduped))))
if (length(drop_now)) stacked_deduped[, (drop_now) := NULL]
lead <- intersect(c("dataset","cvr_method","tender_id","lot_id","buyer_number",
                    "buyer_name","buyer_cvr_final"), names(stacked_deduped))
setcolorder(stacked_deduped, c(lead, setdiff(names(stacked_deduped), lead)))

# 6 Self-check: cvr_method must reproduce the production/extraction/name_match samples exactly.
.prod_orig  <- production[,  .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_orig  <- extraction[,  .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.nm_orig    <- name_match[,   .(l = paste(sort(buyer_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
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

out_path <- Sys.getenv("TED_BUYER_STACK_OUT", unset = file.path(clean_data_dir, "ted_buyer_datasets_stacked.rds"))
save_dataset(stacked_deduped, out_path)   # .rds (canonical) + .csv + .parquet
cat(sprintf("ted_buyer_datasets_stacked.rds: %d rows, %d cols\n", nrow(stacked_deduped), ncol(stacked_deduped)))

# Persist the whole stack so an unchanged-input rerun can skip this build entirely.
match_cache_write(match_input, stacked_deduped, character(0), match_cache_file, match_cache_ver,
                  extra = predup_ref)
print(stacked_deduped[, .N, by = dataset][order(dataset)])
cat(sprintf("  rebuild: production=%d | extraction=%d | name_match=%d\n",
            sum(grepl("production", stacked_deduped$cvr_method)),
            sum(grepl("extraction", stacked_deduped$cvr_method)),
            sum(grepl("name_match", stacked_deduped$cvr_method))))
