# TED WINNER concatenate-and-dedup -- the TED analogue of 3_1/3_2, with all THREE CVR-resolution methods:
#   production  : the XML-resolved winner CVR from ted_4 (winner_cvr_final).
#   extraction  : raw 8-digit CVRs pulled straight from the winner field (winner_cvr_original), no matching.
#   name_match  : re-run name-match-only CVRs (XML/field CVR IGNORED); generated inline (section 2c).
# Reduced to one row per distinct (tender_id, lot_id, winner_cvr_final, is_winner). Author: Jack Mulqueeney.
#
# is_winner is part of the grain: TED keeps losing bidders (is_winner == FALSE), 4_combine relabels entity
# from it (winner vs non-winner), and a CVR can be BOTH a winner and a bidder in the same lot -- so the
# dedup key includes is_winner to keep those as separate rows.
#
# OUTPUT (data/clean/ted_winner_datasets_stacked.rds):
#   dataset    : "production" | "extraction" | "name_match" -- the method that produced the surviving row.
#   cvr_method : method(s) that produced the CVR, ";"-joined per (tender,lot,CVR,is_winner). Rebuild each
#                sample EXACTLY via grepl(<m>, cvr_method). name_match ignores the field/XML CVR.

rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean_data_dir <- dirs$clean_data

base_raw <- as.data.table(readRDS(file.path(clean_data_dir, "clean_winner_data_ted_name_matched.rds")))

# ── Whole-result stack cache: skip the build when inputs are unchanged (see functions.R). This builder also
# reads the raw ted_winner_data (for the name-match pass), so the version folds its CONTENT hash alongside
# the CVR keys' mtime. Empty refresh -> exact skip on identical input, full rebuild on any change.
.nmo_f <- file.path(dirs$intermediates, "ted", "ted_winner_data.rds")
.nmo_h <- if (file.exists(.nmo_f)) substr(rlang::hash(readRDS(.nmo_f)), 1, 12) else "nonmo"
match_cache_ver  <- paste0("p3-", key_sig(clean_data_dir), "-", .nmo_h)  # p3: extraction/name_match slices now carry full lot/notice metadata (schema/cpv_main/currency/buyer_type)
match_cache_file <- file.path(dirs$intermediates, "match_cache", "stack__ted_winner.rds")
match_input      <- copy(base_raw)
.hit <- match_cache_read(match_input, character(0), character(0), match_cache_file, match_cache_ver)
if (!is.null(.hit)) {
  cat("Stack cache HIT -> skipping TED winner stack build\n")
  .chk <- file.path(clean_data_dir, "checks"); dir.create(.chk, showWarnings = FALSE, recursive = TRUE)
  saveRDS(.hit$extra, file.path(.chk, "predup_cvr_lists_ted_winner.rds"))
  save_dataset(.hit$output, Sys.getenv("TED_WINNER_STACK_OUT",
    unset = file.path(clean_data_dir, "ted_winner_datasets_stacked.rds")))
  quit(save = "no")
}
cat("Stack cache MISS -> building TED winner stack\n")

# 1 Production: the XML-resolved TED winners from ted_4. Keep rows with a resolved CVR, unique per
#   (tender, lot, CVR, is_winner) so a firm that both wins and (in the same lot) loses stays two rows.
production <- unique(base_raw[!is.na(winner_cvr_final) & winner_cvr_final != ""],
                     by = c("tender_id", "lot_id", "winner_cvr_final", "is_winner"))
production[, dataset := "production"]

# Lot/notice-level metadata attached to the extraction / name_match slices (production carries it
# natively). build_lot_ctx() keeps every column that is constant within a tender-lot and drops
# entity-identity / per-CVR / per-winner columns -- see its definition in functions.R.
lot_ctx <- build_lot_ctx(base_raw, entity = "winner")

# 2 Extraction: raw 8-digit CVRs from the winner field, no matching. Built PER is_winner so the
#   winner/non-winner tag survives (lot_field_cvrs() collapses per lot and would otherwise drop it).
ex_win <- as.data.table(lot_field_cvrs(unique(base_raw[is_winner == TRUE,
                          .(tender_id, lot_id, winner_cvr = winner_cvr_original)])))[, is_winner := TRUE]
ex_los <- as.data.table(lot_field_cvrs(unique(base_raw[is_winner == FALSE,
                          .(tender_id, lot_id, winner_cvr = winner_cvr_original)])))[, is_winner := FALSE]
extraction <- rbind(ex_win, ex_los)
extraction[, winner_number := rowid(tender_id, lot_id)]
extraction[, `:=`(winner_cvr_final = cvr, winner_cvr_clean = cvr, valid_cvr = TRUE,
                  cvr_number_source = "CVR from the winner field: raw extraction (no matching)",
                  flag_name_match_found = FALSE)]
extraction[, cvr := NULL][, dataset := "extraction"]
extraction <- merge(extraction, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)

# 2c Name-match-only: re-run the exact+fuzzy matcher on EVERY DK winner name from ted_3, IGNORING the XML
#   CVR (winner_cvr_final = the name-matched CVR alone). Same primitives + thresholds as ted_4; script scope
#   (keep_step_matches() uses <<-). Reads ted_winner_data.rds (ted_3) for the names + is_winner; lot context
#   comes from lot_ctx. Surfaced ONLY via winner_cvr_final; dedup to distinct (tender,lot,CVR,is_winner).
ted_dir <- file.path(dirs$intermediates, "ted")
nmo <- as.data.table(readRDS(file.path(ted_dir, "ted_winner_data.rds")))
# lot_id must be the CANONICAL lot id (1..N, as in ted_3/ted_4/base_raw), NOT the raw TED lot code `lot`:
# lot_ctx + production + extraction all key on the canonical lot_id, so using `lot` here put name_match rows on
# a different lot-id namespace -> the lot_ctx merge (dates/amounts/cpv/procedure) missed and name_match rows
# came out context-less. Coerce the existing canonical lot_id to character to match.
nmo[, `:=`(tender_id = as.character(notice_id), lot_id = as.character(lot_id),
           winner_number = rowid(notice_id, lot))]
bp <- prepare_cvr_name(nmo$winner_name)
nmo[, `:=`(winner_name_basic = bp$name_basic, winner_name_match = bp$name_clean,
           winner_name_no_spaces = bp$name_no_spaces, winner_name_broad = bp$name_broad,
           winner_firm_type = bp$firm_type, first_letter = substr(bp$name_clean, 1, 1),
           broad_first_letter = substr(bp$name_broad, 1, 1))]
nmo[, `:=`(flag_matching_candidate = !is.na(winner_name) & winner_name != "",
           match_row_id = .I, winner_name_in_data = winner_name)]
name_key   <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds")))
biname_key <- as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds")))
setnames(name_key, "name", "registered_name"); setnames(biname_key, "binavn", "registered_name")
name_key[, name_source := "name"]; biname_key[, name_source := "biname"]
name_key[, cvr := sprintf("%08d", as.integer(cvr))]; biname_key[, cvr := sprintf("%08d", as.integer(cvr))]
name_key[, broad_first_letter := substr(name_broad, 1, 1)]; biname_key[, broad_first_letter := substr(name_broad, 1, 1)]
cvr_key <- rbindlist(list(name_key, biname_key), use.names = TRUE)
cvr_key[, source_order := fifelse(name_source == "name", 1L, 2L)]
remaining <- nmo[flag_matching_candidate & grepl("DK|DNK", toupper(trimws(winner_country)))]
# Matching date = contract-award date: TED's most-available award date. Used ONLY for the +/-2y CVR
# registry-validity window in matching (see KFST note in 2_1 on the pub-vs-award gap).
remaining[, match_date := as.IDate(date_contract_award)]
remaining_original <- remaining
matched <- data.table(match_row_id = integer(0), cvr_name_match = character(0),
  registered_name_match = character(0), name_match_source = character(0), name_match_step = integer(0),
  name_match_method = character(0), name_match_score = numeric(0), name_match_n_candidates = integer(0))
.run_step <- function(on_cols, step) keep_step_matches(add_winner_context_to_matches(
  select_preferred_exact_match(cvr_key[remaining, on = on_cols, nomatch = 0, allow.cartesian = TRUE], step = step)))
.run_step(c(name_basic = "winner_name_basic", firm_type = "winner_firm_type"), 1L)
.run_step(c(name_no_spaces = "winner_name_no_spaces", firm_type = "winner_firm_type"), 2L)
.run_step(c(name_no_spaces = "winner_name_no_spaces"), 3L)
.run_step(c(name_broad = "winner_name_broad"), 4L)
cvr_key_quality <- unique(cvr_key[, .(cvr, name_match, name_basic, name_no_spaces, name_broad)])
fuzzy_match_cols <- c("winner_name_match", "winner_name_broad", "winner_firm_type", "match_date")
remaining[, fuzzy_match_id := .GRP, by = fuzzy_match_cols]
fuzzy_row_lookup <- remaining[, .(match_row_id, fuzzy_match_id)]
remaining <- remaining[, .SD[1], by = fuzzy_match_id][, match_row_id := fuzzy_match_id]
matched_prefuzzy <- copy(matched)
# Fuzzy match cache (see find_fuzzy_matches): caches the name-match-only fuzzy pass, shared with the 2_*
# matchers where the (name, firm_type, match_date) problem is identical. Disable with MATCH_CACHE=false.
if (tolower(Sys.getenv("MATCH_CACHE", "true")) != "false")
  options(cvr.fuzzy_cache_dir = file.path(dirs$intermediates, "match_cache"),
          cvr.fuzzy_cache_version = paste0("p1-", key_sig(clean_data_dir)))
# Collect the fuzzy candidate long-tables (not just the accepted match) so the name_match slice can carry
# the runner-up (rank-2), exactly as the production matchers do -- this is what lets name_match-only
# survivors deliver a fuzzy_candidate_cvr_2 / _score_2 (the "second best" for a name-only fuzzy match).
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

# Runner-up (rank-2) wide candidate columns for the name_match slice, mirroring the production matcher
# (ted_5): map the fuzzy candidates back to the real rows, keep the top 5 distinct CVRs by score, and
# attach fuzzy_candidate_cvr_2 / _score_2 so a name-only fuzzy match delivers its second-best option.
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

nmo[matched, on = "match_row_id", `:=`(winner_cvr_final = i.cvr_name_match,
  name_match_source = i.name_match_source, name_match_step = i.name_match_step,
  name_match_method = i.name_match_method, name_match_score = i.name_match_score,
  name_match_n_candidates = i.name_match_n_candidates)]
nmo[, matching_candidate_type := fcase(
  flag_matching_candidate & toupper(trimws(winner_country)) %chin% c("DK","DNK"), "exact DK",
  flag_matching_candidate & grepl("DK|DNK", toupper(trimws(winner_country))), "contains DK",
  default = NA_character_)]
nmo[, flag_name_match_found := !is.na(winner_cvr_final)]
nmo[, flag_name_match_ambiguous := flag_name_match_found & name_match_n_candidates > 1]
nmo[, flag_review_name_match := flag_name_match_found & (name_match_method == "fuzzy" | flag_name_match_ambiguous)]
nmo[, name_match_status := fcase(
  !flag_matching_candidate, "not requested",
  flag_review_name_match, "manual review - fuzzy or ambiguous match",
  flag_name_match_found, "matched",
  is.na(winner_country) | !grepl("DK|DNK", toupper(trimws(winner_country))), "manual review - not marked as Danish",
  default = "manual review - no automatic match")]
for (qf in list(c("winner_name_match","name_match","cvr_name_match_quality"),
                c("winner_name_basic","name_basic","cvr_name_match_quality_basic"),
                c("winner_name_no_spaces","name_no_spaces","cvr_name_match_quality_nospaces"),
                c("winner_name_broad","name_broad","cvr_name_match_quality_broad"))) {
  .wc <- qf[1]; .kc <- qf[2]; .qc <- qf[3]
  .rl <- unique(data.table(cvr = as.character(cvr_key_quality$cvr), reg_name = cvr_key_quality[[.kc]]))[!is.na(reg_name) & reg_name != ""]
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
  .(tender_id, lot_id, is_winner, winner_number, winner_name, winner_country, winner_cvr_final,
    winner_cvr_clean = NA_character_, valid_cvr = TRUE, name_match_method, name_match_step, name_match_source,
    name_match_score, name_match_n_candidates, matching_candidate_type, flag_name_match_found,
    cvr_number_source = "CVR from name matching only (field CVR ignored)", cvr_name_match_quality,
    cvr_name_match_quality_basic, cvr_name_match_quality_nospaces, cvr_name_match_quality_broad,
    cvr_name_match_quality_name, cvr_name_is_substring, name_match_status,
    fuzzy_candidate_cvr_2, fuzzy_candidate_score_2)]
name_match[, dataset := "name_match"]
name_match <- unique(name_match, by = c("tender_id","lot_id","winner_cvr_final","is_winner"))
name_match <- merge(name_match, lot_ctx, by = c("tender_id","lot_id"), all.x = TRUE)
rm(nmo, cvr_key, cvr_key_quality, name_key, biname_key, remaining, matched); invisible(gc())

# 2b Pre-dedup reference for 98_ (per-lot sorted CVR lists BEFORE the cross-method dedup). TED entity is
#    winner/non-winner by is_winner (matches 4_combine's relabel), so the reference keys on it.
.ent <- function(iw) fifelse(iw %in% FALSE, "non-winner", "winner")
ref_prod <- production[!is.na(winner_cvr_final) & winner_cvr_final != "",
  .(data_source = "TED", entity = .ent(is_winner), method = "production", tender_id, lot_id, cvr_final = winner_cvr_final)]
ref_extr <- extraction[!is.na(winner_cvr_final) & winner_cvr_final != "",
  .(data_source = "TED", entity = .ent(is_winner), method = "extraction", tender_id, lot_id, cvr_final = winner_cvr_final)]
ref_nm   <- name_match[!is.na(winner_cvr_final) & winner_cvr_final != "",
  .(data_source = "TED", entity = .ent(is_winner), method = "name_match", tender_id, lot_id, cvr_final = winner_cvr_final)]
predup_ref <- rbind(ref_prod, ref_extr, ref_nm)[, .(cvr_list = paste(sort(cvr_final), collapse = ";")),
                                        by = .(data_source, entity, method, tender_id, lot_id)]
chk_dir <- file.path(clean_data_dir, "checks"); dir.create(chk_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(predup_ref, file.path(chk_dir, "predup_cvr_lists_ted_winner.rds"))

# 3 Stack the three methods; missing columns get NA via fill = TRUE.
stacked <- rbindlist(list(production, extraction, name_match), use.names = TRUE, fill = TRUE)
stacked[, dataset := factor(dataset, levels = c("production","extraction","name_match"))]
stacked[, is_awarded_winner := awarded_winner(stacked)]   # flag_awarded & is_winner

# 4 CVR-level dedup -> ONE row per distinct (tender_id, lot_id, winner_cvr_final, is_winner). cvr_method
#   lists the method(s) that produced this CVR, ";"-joined in production<extraction<name_match order; the
#   production copy is kept where methods agree (richest metadata) via the factor ordering before unique().
setorder(stacked, tender_id, lot_id, winner_cvr_final, is_winner, dataset)
stacked[, cvr_method := paste(unique(dataset), collapse = "; "),
        by = .(tender_id, lot_id, winner_cvr_final, is_winner)]
stacked_deduped <- unique(stacked, by = c("tender_id", "lot_id", "winner_cvr_final", "is_winner"))

# 5 Carry every analytical/provenance column through to 4_combine; drop only dedup scratch + raw junk.
drop_now <- unique(raw_dump_cols(names(stacked_deduped)))
if (length(drop_now)) stacked_deduped[, (drop_now) := NULL]
lead <- intersect(c("dataset","cvr_method","tender_id","lot_id","is_winner","winner_number",
                    "winner_name","winner_cvr_final","is_awarded_winner"), names(stacked_deduped))
setcolorder(stacked_deduped, c(lead, setdiff(names(stacked_deduped), lead)))

# 6 Self-check: cvr_method must reproduce the production/extraction/name_match samples exactly (per-lot
#   sorted CVR multiset -- a CVR that is both winner and bidder in a lot appears twice on both sides).
.prod_orig  <- production[,  .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.extr_orig  <- extraction[,  .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
.nm_orig    <- name_match[,   .(l = paste(sort(winner_cvr_final), collapse = ";")), by = .(tender_id, lot_id)]
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

out_path <- Sys.getenv("TED_WINNER_STACK_OUT", unset = file.path(clean_data_dir, "ted_winner_datasets_stacked.rds"))
save_dataset(stacked_deduped, out_path)   # .rds (canonical) + .csv + .parquet
cat(sprintf("ted_winner_datasets_stacked.rds: %d rows, %d cols\n", nrow(stacked_deduped), ncol(stacked_deduped)))

# Persist the whole stack so an unchanged-input rerun can skip this build entirely.
match_cache_write(match_input, stacked_deduped, character(0), match_cache_file, match_cache_ver,
                  extra = predup_ref)
print(stacked_deduped[, .N, by = dataset][order(dataset)])
cat(sprintf("  rebuild: production=%d | extraction=%d | name_match=%d\n",
            sum(grepl("production", stacked_deduped$cvr_method)),
            sum(grepl("extraction", stacked_deduped$cvr_method)),
            sum(grepl("name_match", stacked_deduped$cvr_method))))
