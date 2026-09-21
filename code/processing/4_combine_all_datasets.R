# Combine ALL six matched samples into one long dataset for the secure-server delivery:
#   KFST winners, KFST buyers, OpenTender winners, OpenTender buyers, TED winners, TED buyers.
# Author: Jack Mulqueeney. Date: 9 Sep 2026.
#
# Inputs. Winners/buyers that have a concat-and-dedup stack (3_1/3_2/3_3) come from the stack (so their
# production/extraction sub-samples are selectable); the rest come from their matched file:
#   KFST winner  -> kfst_winner_datasets_stacked.rds        (stack: production + extraction)
#   OT winner    -> ot_winner_datasets_stacked.rds          (stack)
#   OT buyer     -> ot_buyer_datasets_stacked.rds            (stack)
#   KFST buyer   -> clean_buyer_data_kfst_name_matched.rds   (matched; no source CVR field to extract)
#   TED winner   -> clean_winner_data_ted_name_matched.rds   (matched)
#   TED buyer    -> clean_buyer_data_ted_name_matched.rds    (matched)
#
# Sample-selection columns (kept SEPARATE because samples overlap -- an agree-lot row belongs to both the
# production and extraction samples):
#   data_source : "KFST" | "OpenTender" | "TED"   (named data_source, not source: OpenTender already has a
#                 native `source` column -- its CVR-cleaning provenance -- which is left untouched)
#   entity      : "winner" | "buyer"
#   cvr_method  : the method(s) that extracted this (tender, lot, CVR), ";"-joined -- "production",
#                 "extraction", or "production; extraction" (both). Matched-only sources (TED, KFST buyer)
#                 are "production". The delivered table is UNIQUE on
#                 (data_source, entity, tender_id, lot_id, cvr_final) -- one row per CVR.
# Any one sample: production = grepl("production", cvr_method); extraction = grepl("extraction", cvr_method).
#
# CVR columns are standardised: the entity's `winner_cvr_*` / `buyer_cvr_*` become `cvr_*` (so winner and
# buyer CVRs share one set of columns). The buyer-context `buyer_cvr_original` carried on winner rows is
# left as-is (it is not the winner's CVR). Winner/buyer country is harmonised to ISO alpha-2 via
# standardise_country() (TED XML uses alpha-3, e.g. DNK -> DK; a no-op for the already-alpha-2 sources).
#
# Rows with no resolved CVR (cvr_final NA/empty) are dropped -- not useful in the server, and it trims size.
#
# OUTPUT (data/clean/): tender_data_2006_2026.{rds,csv}  -- union schema, source-specific columns NA-filled.

rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean <- dirs$clean_data
stack_dir <- Sys.getenv("STACK_DIR", unset = clean)   # stacks live with the matched files; override for testing

# file, directory, source, entity, is_stack
spec <- list(
  list("kfst_winner_datasets_stacked.rds",      stack_dir, "KFST",       "winner", TRUE),
  list("ot_winner_datasets_stacked.rds",         stack_dir, "OpenTender", "winner", TRUE),
  list("ot_buyer_datasets_stacked.rds",          stack_dir, "OpenTender", "buyer",  TRUE),
  list("clean_buyer_data_kfst_name_matched.rds", clean,     "KFST",       "buyer",  FALSE),
  list("ted_winner_datasets_stacked.rds",        stack_dir, "TED",        "winner", TRUE),
  list("ted_buyer_datasets_stacked.rds",         stack_dir, "TED",        "buyer",  TRUE)
)

parts <- lapply(spec, function(s) {
  f <- s[[1]]; dir <- s[[2]]; src <- s[[3]]; ent <- s[[4]]; is_stack <- s[[5]]
  p <- file.path(dir, f)
  if (!file.exists(p)) stop(sprintf("4_combine_all_datasets: missing input %s", p), call. = FALSE)
  d <- as.data.table(readRDS(p))

  # standardise the entity's own CVR columns -> cvr_* (winner_cvr_* on winner rows, buyer_cvr_* on buyer)
  old <- grep(paste0("^", ent, "_cvr"), names(d), value = TRUE)
  if (length(old)) setnames(d, old, sub(paste0("^", ent, "_cvr"), "cvr", old))

  # sample-selection columns (data_source, not source: OpenTender has its own native `source` column)
  d[, `:=`(data_source = src, entity = ent)]
  if ("dataset" %in% names(d)) d[, dataset := as.character(dataset)] else d[, dataset := "production"]
  # cvr_method is built per (tender, lot, CVR) in the 3_* stack builders; the matched-only sources
  # (TED, KFST buyer) have no raw-extraction method, so they are "production".
  if (!is_stack) d[, cvr_method := "production"]
  d[]
})

combined <- rbindlist(parts, use.names = TRUE, fill = TRUE)

# Drop rows with no resolved CVR -- they carry no linkable firm and are not useful in the secure server.
# (The stacks already dropped theirs; this removes the unmatched rows the matched TED / KFST-buyer carry.)
combined <- combined[!is.na(cvr_final) & cvr_final != ""]

# Harmonise country to ISO alpha-2 (TED XML uses alpha-3: DNK -> DK, SWE -> SE, ...); no-op for alpha-2.
for (cc in intersect(c("winner_country", "buyer_country"), names(combined)))
  combined[, (cc) := standardise_country(get(cc))]

# Null negative "not disclosed" sentinels in count columns. TED legacy XML uses -1 (and -2 when summed over
# lots) for undisclosed tender counts; these are MISSINGS, not real counts, so map <0 -> NA (all missings
# are NA regardless of type -- character counts like n_bids_received are coerced to check the sign).
for (cc in intersect(c("n_bidders", "n_tenders_sme", "n_tenders_received", "n_bids_received"), names(combined))) {
  x <- combined[[cc]]
  if (is.numeric(x)) {
    combined[which(x < 0), (cc) := NA]
  } else {
    xi <- suppressWarnings(as.integer(x))
    combined[which(!is.na(xi) & xi < 0), (cc) := NA_character_]
  }
}

combined[, dataset := factor(dataset, levels = c("production", "extraction", "name_match"))]

# lead with the sample-selection + key identity columns, then everything else
lead <- intersect(c("data_source","entity","dataset","cvr_method","tender_id","lot_id",
                    "cvr_final","winner_name","buyer_name","is_awarded_winner","cvr_number_source"),
                  names(combined))
setcolorder(combined, c(lead, setdiff(names(combined), lead)))

# --- Self-check: every (data_source, entity[, build flag]) slice reproduces its source dataset ----------
# Re-read each input and confirm the combined slice recovers exactly its resolved-CVR (tender, lot, CVR)
# set; for the stacks, also that build_prod / build_extr recover the production / extraction sub-samples.
for (s in spec) {
  f <- s[[1]]; dir <- s[[2]]; src <- s[[3]]; ent <- s[[4]]; is_stack <- s[[5]]
  cvrcol <- paste0(ent, "_cvr_final")
  o  <- as.data.table(readRDS(file.path(dir, f)))
  o  <- o[!is.na(get(cvrcol)) & get(cvrcol) != ""]
  ok <- unique(o[, .(tender_id, lot_id, cvr = get(cvrcol))])
  ck <- unique(combined[data_source == src & entity == ent, .(tender_id, lot_id, cvr = cvr_final)])
  if (nrow(fsetdiff(ok, ck)) || nrow(fsetdiff(ck, ok)))
    stop(sprintf("selection check: (%s, %s) slice does not reproduce %s", src, ent, f), call. = FALSE)
  if (is_stack) {
    for (m in c("production", "extraction", "name_match")) {
      ok2 <- unique(o[grepl(m, cvr_method), .(tender_id, lot_id, cvr = get(cvrcol))])
      ck2 <- unique(combined[data_source == src & entity == ent & grepl(m, cvr_method),
                             .(tender_id, lot_id, cvr = cvr_final)])
      if (nrow(fsetdiff(ok2, ck2)) || nrow(fsetdiff(ck2, ok2)))
        stop(sprintf("selection check: (%s, %s) %s does not reproduce the stack's %s sample", src, ent, m, m), call. = FALSE)
    }
  } else {
    sl <- combined[data_source == src & entity == ent]
    if (!all(sl$cvr_method == "production"))
      stop(sprintf("selection check: matched source (%s, %s) should be cvr_method == 'production'", src, ent), call. = FALSE)
  }
}
cat("selection-column self-check passed: every (data_source, entity[, build flag]) slice reproduces its source dataset.\n")

# ---- Entity: surface TED non-winning bidders as their own value ----
# TED is the only source that records losing bidders (is_winner == FALSE); relabel them so `entity` is a
# clean 3-way {winner, non-winner, buyer}. Done AFTER the self-check, which validates the winner/buyer
# reconstruction against the source files (where these rows are still "winner"). is_winner is NA for the
# other sources and for buyers, so only the TED bidders are affected.
combined[!is.na(is_winner) & is_winner == FALSE, entity := "non-winner"]
cat(sprintf("entity relabel: %d TED non-winning bidder rows -> entity = 'non-winner'\n",
            combined[entity == "non-winner", .N]))

# ---- Drop dual-role rows: a firm that WON a lot cannot also be its own non-winning bidder ----
# TED eForms builds the winner dataset from roles ("winner","bidder"); the winning firm is often ALSO listed
# in the tenderer/bidder set (ted_3_build_winner_buyer_datasets.R:98), so it can appear both as entity=="winner"
# and entity=="non-winner" on the same (tender_id, lot_id, cvr_final). A firm that won is not a "non-winning
# bidder", so remove ONLY that spurious non-winner row (the winner row is kept). Row-level: genuine non-winners
# on the same lot are untouched. Applied here (post-combine) rather than in ted_3 so it needs no re-matching.
# Dual-role firms (TED): a firm can appear as BOTH a winner and a non-winner on the same (tender_id, lot_id)
# -- confirmed genuine against the source PDFs (a firm that submitted multiple tenders / is listed in both the
# winner and tenderer sets). We deliberately KEEP both rows: the dual role is fully recoverable from the data
# with no extra column, since a dual non-winner is exactly a non-winner row whose (data_source, tender_id,
# lot_id, cvr_final) also has a winner row. Downstream/server-side can flag or exclude these controls via that
# self-join; we do not drop them here, so the delivered data stays faithful to the source notices.

# ---- Harmonise TED award_date: fill gaps from award_contract_date (the same contract-award event) ----
# TED only (KFST/OT left as-is, by design). For TED, `award_date` and `award_contract_date` are the SAME
# event -- the contract-conclusion date -- but `award_date` comes from the LEGACY uppercase-tag parser, which
# does not read eForms (2024+) notices, so it is NA on every eForms lot (all non-winners + the winners of
# competitive eForms lots). `award_contract_date` is the eForms-aware BT-145 date. Coalescing fills those
# gaps without overwriting any present award_date, so: non-winners get a date (was 0%), competitive-lot
# winners get a date, and a winner and its non-winners in the same (tender_id, lot_id) carry the SAME date
# (the within-notice tie; group a matched analysis on tender_id == ted_notice_id). eForms 0% -> ~83%.
.ted_filled <- combined[data_source == "TED" & is.na(award_date) & !is.na(award_contract_date), .N]
combined[data_source == "TED" & is.na(award_date) & !is.na(award_contract_date),
         award_date := award_contract_date]
cat(sprintf("TED award_date fill: %d rows dated from award_contract_date (winners + non-winners)\n", .ted_filled))

# ---- Lot-level award_date / award_end_date recovery ----
# award_date is genuinely per-winner in ~1.3% of KFST lots, so build_lot_ctx excludes it from stamping
# entirely (all-or-nothing per column). That leaves name_match-only / extraction-only rows (where production
# resolved no CVR for that tender-lot-CVR) with NA award_date, even though the lot's date is known from its
# production/extraction rows. Fill any remaining NA within (data_source, tender_id, lot_id) from the lot's
# first non-NA value; present dates are NEVER overwritten (so real per-winner dates survive). Exact for
# single-date lots (98.7% of fills); same-lot-approximate for the ~58 rows on genuinely per-winner lots
# (0.014% of all rows). award_end_date (award_date + duration) carries the identical gap, so fill both.
.fill_lot <- function(x) { i <- which(!is.na(x)); if (!length(i)) return(x); x[is.na(x)] <- x[i[1L]]; x }
.dcols <- intersect(c("award_date", "award_end_date"), names(combined))
.na_before <- vapply(.dcols, function(c) sum(is.na(combined[[c]])), integer(1))
combined[, (.dcols) := lapply(.SD, .fill_lot), by = .(data_source, tender_id, lot_id), .SDcols = .dcols]
for (.c in .dcols) cat(sprintf("%s lot-fill: %d NA -> %d remaining (recovered from same-lot value)\n",
                               .c, .na_before[[.c]], sum(is.na(combined[[.c]]))))

# ---- Harmonise divided_tender to a clean logical across sources ----
# KFST/OpenTender arrive as the strings "TRUE"/"FALSE" while TED arrives as "yes"/"no", so the raw column
# mixes all four. Recode to a proper logical (TRUE = split into lots) so the variable is usable cross-source;
# anything unrecognised (incl. blanks) becomes NA.
if ("divided_tender" %in% names(combined)) {
  .dt <- tolower(trimws(as.character(combined$divided_tender)))
  combined[, divided_tender := fcase(.dt %in% c("true", "yes"), TRUE,
                                     .dt %in% c("false", "no"), FALSE,
                                     default = NA)]
  cat(sprintf("divided_tender harmonised to logical: %d TRUE / %d FALSE / %d NA\n",
              sum(combined$divided_tender %in% TRUE), sum(combined$divided_tender %in% FALSE),
              sum(is.na(combined$divided_tender))))
}

# ---- Coerce contract_duration_months_max to numeric ----
# KFST's raw "Varighed ... (max)" field reaches here as character (its sibling _min is already numeric);
# the only non-numeric value is "Ubegraenset" (Danish for "unlimited"), which correctly becomes NA.
if ("contract_duration_months_max" %in% names(combined) && !is.numeric(combined$contract_duration_months_max)) {
  .n_unlimited <- combined[!is.na(contract_duration_months_max) &
                           is.na(suppressWarnings(as.numeric(contract_duration_months_max))), .N]
  combined[, contract_duration_months_max := suppressWarnings(as.numeric(contract_duration_months_max))]
  cat(sprintf("contract_duration_months_max coerced to numeric (%d non-numeric -> NA)\n", .n_unlimited))
}

# Rename for clarity: n_lots_contracted (KFST-only) actually holds the count of lots in the tender
# NOTICE -- `Antal delkontrakter i udbudsbekendtgoerelsen`, i.e. *announced*, not "contracted".
if ("n_lots_contracted" %in% names(combined)) setnames(combined, "n_lots_contracted", "n_lots_announced")

# ---- Final standardisation: normalise every date/datetime column to a clean whole-day Date ----
# Dates reach here in mixed forms -- midnight POSIXct (e.g. pub_date), fractional-day Dates from upstream
# arithmetic (e.g. award_end_date), and the TED-notice-XML lineage dates. Collapse each to a plain Date
# (whole days) so the value is unambiguous in every output format: CSV otherwise renders POSIXct as
# "...T00:00:00Z" and reads it back as a string, and fractional days differ across formats. Detected by
# class, so it also covers any date column added later.
date_cols <- names(combined)[vapply(combined, function(x) inherits(x, c("Date", "POSIXct", "IDate")), logical(1))]
for (col in date_cols) {
  x <- combined[[col]]
  if (inherits(x, "POSIXct")) {                       # POSIXct unclass is seconds -> convert to a date first
    tz <- attr(x, "tzone"); if (is.null(tz) || !nzchar(tz)) tz <- "UTC"
    x <- as.Date(x, tz = tz)
  }
  set(combined, j = col, value = as.Date(floor(as.numeric(x)), origin = "1970-01-01"))  # floor fractional days
}
cat(sprintf("normalised %d date column(s) to whole-day Date: %s\n", length(date_cols), paste(date_cols, collapse = ", ")))

out_dir <- Sys.getenv("COMBINE_OUT_DIR", unset = clean)

# ---- Final standardisation: recode tender_status (KFST) to English ----
# Verified against the KFST variabelbeskrivelse (variabel 44 "Helt/delvist gennemført/annulleret").
# Non-KFST rows are NA and stay NA. Prefix matching avoids any encoding fragility on the Danish text.
if ("tender_status" %in% names(combined)) {
  ts  <- combined$tender_status
  new <- rep(NA_character_, length(ts))
  new[grepl("^Helt gennemf", ts)] <- "fully_completed"                  # all lots completed
  new[grepl("^Delvist",      ts)] <- "partially_completed_or_cancelled" # some completed, some cancelled
  new[grepl("^Helt annull",  ts)] <- "fully_cancelled"                  # all lots cancelled
  unmapped <- unique(ts[!is.na(ts) & is.na(new)])
  if (length(unmapped)) warning("tender_status unmapped value(s): ", paste(unmapped, collapse = " | "))
  set(combined, j = "tender_status", value = new)
}

# ---- Final harmonisation and variable consolidation: renames + derived EUR/DKK annualised amounts ----
# Renames applied to the combined table only (the per-source files keep the old names).
ren <- c(flag_cvr_final_in_registry = "flag_valid_cvr_in_registry",
         type = "kfst_consortium_split_method", valid_cvr = "valid_cvr_before_match")
for (old in names(ren)) if (old %in% names(combined)) setnames(combined, old, ren[[old]])

# cvr_method (";"-joined production / extraction / name_match) lists the method(s) that produced each
# (tender, lot, CVR). It is built in the 3_* stack builders (KFST/OT winner, OT buyer, TED winner/buyer)
# and set to "production" for the sole remaining matched-only source (KFST buyer) above, so it is already
# present on every row. KFST buyers have NO source-CVR field -- their production CVR IS the name-matched CVR
# -- so tag them name_match too (they are inherently the name-only sample -- no field CVR to differ).
# build_prod / build_extr / build_name_match are convenience booleans derived from cvr_method (a row is in
# a sample if cvr_method mentions that method) -- kept alongside cvr_method for filtering convenience.
combined[data_source == "KFST" & entity == "buyer", cvr_method := "production; name_match"]
combined[, build_prod       := grepl("production", cvr_method)]
combined[, build_extr       := grepl("extraction", cvr_method)]
combined[, build_name_match := grepl("name_match", cvr_method)]

# notice_source: TED-only provenance flag -- did the notice enter via an OpenTender/KFST award URL ("url") or
# was it discovered only by the TED API sweep (ted_dates_0_api_universe.R) and folded into the TED universe
# ("api")? Non-TED rows are NA. TED tender_id IS the notice id, so we match it against the API-only manifest
# (leading-zero-insensitive). No manifest / no API rows -> every TED row is "url".
combined[, notice_source := NA_character_]
.api_manifest <- file.path(dirs$intermediates, "ted", "api_only_award_ids.rds")
.api_only_ids <- if (file.exists(.api_manifest)) unique(sub("^0+", "", readRDS(.api_manifest)$publication_number)) else character(0)
combined[data_source == "TED",
         notice_source := fifelse(sub("^0+", "", tender_id) %chin% .api_only_ids, "api", "url")]

# Currency label: TED carries its native (multi-)currency; KFST amounts are always DKK and OpenTender
# always EUR, but those two sources leave the `currency` string blank. Fill it so the column is complete
# cross-source -- the *_eur/*_dkk amounts were already converted per-source, this only labels them, and
# TED's native values are left untouched.
if ("currency" %in% names(combined)) {
  combined[data_source == "KFST"       & (is.na(currency) | trimws(currency) == ""), currency := "DKK"]
  combined[data_source == "OpenTender" & (is.na(currency) | trimws(currency) == ""), currency := "EUR"]
}

# QC cross-check: the buyer-declared contract nature (contract_nature, from KFST Kontrakttype / OT
# tender_supplyType / TED contract_nature) vs the CPV-derived cpv_category. They agree ~97%; TRUE flags the
# ~3% where they differ (declared is generally authoritative; cpv_category is inferred from the first CPV).
combined[, flag_nature_cpv_mismatch := !is.na(contract_nature) & !is.na(cpv_category) &
           tolower(contract_nature) != tolower(cpv_category)]

# ---- Safety net: enforce one row per (data_source, entity, tender_id, lot_id, cvr_final) ----
# Every source now delivers this grain upstream: the three stacks are CVR-level unique (3_1/3_2/3_3), and
# TED collapses one row per (notice, lot, CVR) in ted_4/ted_5 -- summing per-contract winner_amount there
# (amount aggregation lives in the source scripts, not here). This is a backstop for any residual duplicate
# (e.g. a KFST buyer listed twice on a lot): collapse to one row per key, keeping the most complete record
# (fewest NAs; ties -> first). It does NOT sum. cvr_method is unaffected.
ukey <- c("data_source", "entity", "tender_id", "lot_id", "cvr_final")
n_before_unique <- nrow(combined)
combined[, .row_na := rowSums(is.na(.SD)), .SDcols = setdiff(names(combined), ukey)]
setorderv(combined, c(ukey, ".row_na"))
combined <- unique(combined, by = ukey)
combined[, .row_na := NULL]
cat(sprintf("uniqueness collapse: %d -> %d rows (removed %d duplicate (source,entity,tender,lot,CVR) rows)\n",
            n_before_unique, nrow(combined), n_before_unique - nrow(combined)))
# Annualise amounts for ALL contract types (not just frameworks), standardised on the harmonised
# contract_duration_months (= KFST mean(min,max) / OT native months / TED days/30.44): per-year value =
# amount / contract_duration_months * 12, for any row with a positive duration. This OVERRIDES the old
# per-source framework-only, day-vs-month annualisation (single source of truth here). Annualised from the
# native amount only (the _eur/_dkk twins are not carried to the final); rows with no positive duration -> NA.
if ("contract_duration_months" %in% names(combined)) {
  dm <- suppressWarnings(as.numeric(combined[["contract_duration_months"]]))
  ok <- !is.na(dm) & dm > 0
  for (a in c("tender", "lot")) for (suff in c("")) {
    src <- paste0(a, "_amount", suff); tgt <- paste0("annualised_", a, "_amount", suff)
    if (src %in% names(combined)) {
      v <- rep(NA_real_, nrow(combined))
      v[ok] <- suppressWarnings(as.numeric(combined[[src]][ok])) / dm[ok] * 12
      set(combined, j = tgt, value = v)
    }
  }
  cat(sprintf("annualised amounts: %d rows with positive duration (all contract types) via contract_duration_months\n", sum(ok)))
}

# ---- Column selection for the secure server ----
# Keep an explicit allowlist of server-permissible columns and drop everything else: firm identifiers
# (names, name fragments, registered names), redundant CVR-provenance columns (only cvr_final +
# cvr_number_source are kept), most fuzzy/field matching diagnostics (but the fuzzy_candidate_cvr_1..5
# alternative-CVR candidates AND their fuzzy_candidate_score_1..5 match scores ARE kept -- the CVRs are
# the authorised key and the scores make them interpretable; the name_* siblings are firm names and
# stay dropped), derived CPV (cpv_code / cpv_main kept),
# NUTS regions, and consortium-split internals. Using a KEEP-list rather than a drop-list means any
# future/unreviewed column is dropped by default and logged -- a new column can never silently ship.
# cvr_final stays: it is the authorised key that links to the register on the server.
keep_cols <- c(
  "data_source", "entity", "cvr_method", "build_prod", "build_extr", "build_name_match", "tender_id", "lot_id", "cvr_final",
  "is_awarded_winner", "cvr_number_source", "ot_source_file", "consortium_flag",
  "semi_tier", "registry_score", "type",
  "contract_type", "n_lots", "n_lots_announced",
  "n_lot_winners", "tender_amount", "lot_amount",
  "tender_amount_estimated", "tender_amount_final", "lot_amount_estimated", "lot_amount_final",
  "flag_all_orig_lot_amt_missing", "n_bidders", "pub_date", "award_date",
  "submit_date", "divided_tender", "joint_tender",
  "cpv_code", "cpv_code_first", "cpv_division", "cpv_division_name", "cpv_sector", "cpv_category",
  "tender_cancelled", "tender_status", "flag_awarded", "flag_nature_cpv_mismatch",
  "contract_duration_months_min", "contract_duration_months_max", "award_end_date", "annualised_tender_amount",
  "annualised_lot_amount", "ted_notice_id", "notice_source", "planning_dispatch_date",
  "planning_publication_date", "planning_tender_deadline_date", "competition_dispatch_date", "competition_publication_date",
  "competition_tender_deadline_date", "award_dispatch_date", "award_publication_date", "award_tender_deadline_date",
  "award_contract_date", "flag_cvr_ws", "flag_cvr_alphabet", "flag_cvr_punct",
  "flag_cvr_standardised", "valid_cvr", "flag_winner_cvr_changed", "lot_id_borrowed_from",
  "flag_borrowed_cvr", "flag_missing_winner_cvr", "flag_missing_winner_name", "flag_foreign_winner",
  "flag_missing_winner_country", "n_winners_extracted", "flag_mismatch_winner_count", "flag_single_bidder",
  "flag_multilot", "flag_missing_cvr_with_name", "flag_matching_candidate",
  "flag_review_cvr", "flag_review_n_winners", "flag_verify_cvr_external",
  "flag_consortium", "cvr_name_match", "name_match_source", "name_match_step",
  "name_match_method", "name_match_score", "name_match_n_candidates", "matching_candidate_type",
  "flag_name_match_found", "flag_name_match_ambiguous", "flag_review_name_match",
  "flag_cvr_recovered_from_invalid", "flag_cvr_final_in_registry", "flag_missing_winner_cvr_final", "flag_missing_cvr_with_name_final",
  "flag_review_cvr_final", "flag_verify_cvr_external_final", "name_match_status",
  "cvr_name_match_quality", "cvr_name_match_quality_basic", "cvr_name_match_quality_nospaces", "cvr_name_match_quality_broad",
  "cvr_name_is_substring", "row_id", "source", "cvr_recovered_from_formatting",
  "n_valid_cvr_raw", "flag_row_multiple_valid_cvr", "flag_cvr_recovered_from_formatting", "flag_cvr_placeholder",
  "flag_lot_amt_equal_split", "flag_framework_prequalified", "name_partition_status",
  "name_partition_n_boundaries", "name_partition_n_legal_forms", "flag_name_partition_eligible", "name_partition_eligibility_reason",
  "name_partition_n_complete", "name_partition_n_firms", "flag_potential_multiple_names", "flag_joint_venture_text",
  "flag_consortium_text", "flag_collaboration_text", "flag_name_partition_expanded", "flag_separated_name",
  "name_partition_segment_number", "flag_non_cvr_identifier", "flag_missing_buyer_cvr", "flag_missing_buyer_name",
  "flag_foreign_buyer", "flag_missing_buyer_country", "flag_missing_buyer_cvr_final",
  "joint_tender_original", "flag_joint_unlisted_buyers", "flag_single_buyer_name_changed",
  "n_buyers_extracted", "n_buyers_listed_original", "flag_buyer_count_agree",
  "year", "schema", "is_winner",
  "winner_amount", "currency", "cpv_main", "contract_nature",
  "lot_estimated_value", "lot_awarded_value", "n_tenders_received", "amount_awarded",
  "amount_estimated", "procedure_type", "procedure_group", "is_framework",
  "is_dps", "direct_award", "date_receipt_tenders", "date_contract_award",
  "date_award_dispatch", "date_award_publication", "buyer_type", "buyer_activity",
  "eu_funded", "award_criteria", "price_weight", "n_tenders_sme",
  "subcontracted", "contract_duration_days", "winner_is_sme", "buyer_amount"
)
# ---- Final harmonisation and variable consolidation: renames, added columns, and drops on the allowlist ----
keep_cols[keep_cols == "flag_cvr_final_in_registry"] <- "flag_valid_cvr_in_registry"
keep_cols[keep_cols == "type"]                       <- "kfst_consortium_split_method"
keep_cols[keep_cols == "valid_cvr"]                  <- "valid_cvr_before_match"
# Harmonised procedure / award-criteria / duration columns derived in the source scripts
# (1_1, 1_2, ted_3): the cross-source group + criteria categories, the months duration, and
# OpenTender's award-criteria count. The source-specific originals stay alongside them.
keep_cols <- c(keep_cols, "procedure_group_h", "award_criteria_h",
               "contract_duration_months", "n_award_criteria")
# Alternative-CVR candidates from fuzzy name-matching (top 5) plus their 0-100 match scores. The CVRs
# are kept because they are CVRs (the authorised key); the scores make each candidate interpretable.
# The parallel fuzzy_candidate_name_* (firm names) stay dropped for de-identification.
# Keep only the NEXT-BEST alternative candidate (rank 2) + its score. Rank 1 is ~99% identical to the chosen
# match (already delivered as cvr_name_match / cvr_final), so it is redundant; ranks 3-5 are rarely useful.
keep_cols <- c(keep_cols, "fuzzy_candidate_cvr_2", "fuzzy_candidate_score_2")
round2_drop <- c(
  "n_tenders_received", "n_winners_extracted", "n_buyers_extracted", "n_buyers_listed_original",
  "buyer_amount", "amount_awarded", "amount_estimated", "lot_estimated_value", "lot_awarded_value",
  "date_receipt_tenders", "date_contract_award", "date_award_dispatch", "date_award_publication",
  "joint_tender_original", "consortium_flag", "year", "buyer_activity", "registry_score",
  "n_valid_cvr_raw", "flag_cvr_placeholder", "lot_id_borrowed_from", "row_id",
  "name_partition_n_boundaries", "name_partition_n_legal_forms", "name_partition_segment_number",
  "name_partition_n_complete", "flag_missing_buyer_cvr_final", "flag_single_buyer_name_changed",
  "flag_mismatch_winner_count", "source", "flag_review_n_winners")
keep_cols <- setdiff(keep_cols, round2_drop)

missing_keep <- setdiff(keep_cols, names(combined))
if (length(missing_keep)) warning("keep-list references columns not present: ", paste(missing_keep, collapse = ", "))
keep_present <- intersect(keep_cols, names(combined))               # original column order, only those present
dropped_cols <- setdiff(names(combined), keep_present)
writeLines(sort(dropped_cols), file.path(out_dir, "tender_data_2006_2026_dropped_columns.txt"))
cat(sprintf("column selection: kept %d of %d columns; dropped %d (list -> tender_data_2006_2026_dropped_columns.txt)\n",
            length(keep_present), ncol(combined), length(dropped_cols)))
cat("dropped columns:\n"); print(dropped_cols)
combined <- combined[, ..keep_present]

# ---- Column order: group intuitively for the reader ----
# core ids -> CVR numbers (final first, then progressively less-final) -> CVR provenance ->
# sample-selection -> tender/lot information -> match-quality measures -> flags -> everything else.
# Rule-based: `ord_flags` sweeps up every remaining flag_* and `ord_rest` everything still unplaced,
# and a setequal() guard guarantees no column is dropped or duplicated by the reorder.
ord_core   <- c("data_source", "tender_id", "lot_id", "ted_notice_id", "notice_source", "entity")
ord_cvr    <- c("cvr_final", "cvr_name_match", "cvr_recovered_from_formatting")   # final -> less final
ord_prov   <- c("cvr_number_source")
ord_select <- c("cvr_method", "build_prod", "build_extr", "build_name_match", "is_winner", "is_awarded_winner")
ord_tender <- c(
  "contract_type", "contract_nature", "n_lots", "n_lots_announced", "n_lot_winners",
  "n_bidders", "n_tenders_received", "n_tenders_sme", "n_winners_extracted", "n_buyers_extracted", "n_buyers_listed_original",
  "tender_amount", "lot_amount",
  "tender_amount_estimated", "tender_amount_final", "lot_amount_estimated", "lot_amount_final",
  "lot_amount_orig", "tender_amount_orig",
  "annualised_tender_amount", "annualised_lot_amount",
  "winner_amount", "buyer_amount", "amount_awarded", "amount_estimated", "lot_estimated_value", "lot_awarded_value", "currency",
  "pub_date", "award_date", "submit_date", "award_end_date", "award_contract_date",
  "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date",
  "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
  "award_dispatch_date", "award_publication_date", "award_tender_deadline_date",
  "date_receipt_tenders", "date_contract_award", "date_award_dispatch", "date_award_publication",
  "contract_duration_months", "contract_duration_months_min", "contract_duration_months_max", "contract_duration_days",
  "cpv_code", "cpv_code_first", "cpv_division", "cpv_division_name", "cpv_sector", "cpv_category", "cpv_main",
  "procedure_type", "procedure_group", "procedure_group_h", "is_framework", "is_dps", "direct_award",
  "divided_tender", "joint_tender", "joint_tender_original", "tender_cancelled", "tender_status",
  "eu_funded", "award_criteria", "award_criteria_h", "n_award_criteria", "price_weight", "subcontracted", "winner_is_sme",
  "buyer_type", "buyer_activity", "is_consortium", "consortium_flag", "consortium_winner", "semi_tier",
  "year", "schema", "ot_source_file")
ord_quality <- c(
  "cvr_name_match_quality", "cvr_name_match_quality_basic", "cvr_name_match_quality_nospaces",
  "cvr_name_match_quality_broad", "cvr_name_is_substring", "valid_cvr_before_match",
  "name_match_status", "name_match_method", "name_match_step", "name_match_source",
  "name_match_score", "name_match_n_candidates", "matching_candidate_type",
  "fuzzy_candidate_cvr_2", "fuzzy_candidate_score_2")
ord_named <- c(ord_core, ord_cvr, ord_prov, ord_select, ord_tender, ord_quality)
ord_flags <- grep("^flag_", setdiff(names(combined), ord_named), value = TRUE)                 # all remaining flags
ord_rest  <- setdiff(names(combined), c(ord_named, ord_flags))                                 # everything else
new_order <- intersect(unique(c(ord_named, ord_flags, ord_rest)), names(combined))
stopifnot(setequal(new_order, names(combined)))                                                # no column lost/duplicated
setcolorder(combined, new_order)
cat(sprintf("reordered %d columns (core/CVR/source/selection/tender/quality/flags/other); trailing 'other': %s\n",
            length(new_order), paste(ord_rest, collapse = ", ")))

# ---- Standardise missings to NA -----------------------------------------------------------------
# Represent "missing" uniformly as NA across every column before shipping: empty / whitespace-only
# strings -> NA_character_ (character cols), and NaN -> NA_real_ (numeric cols). Without this, a blank
# string reads as present in the .rds/.parquet even though it carries no value; NA is the single,
# type-correct missing marker. (In the .csv, fwrite already writes NA as "", so this mainly aligns the
# .rds/.parquet with that convention.) Factors/logicals/Dates already use NA and are left untouched.
char_cols <- names(combined)[vapply(combined, is.character, logical(1))]
for (col in char_cols) {
  idx <- which(!is.na(combined[[col]]) & trimws(combined[[col]]) == "")
  if (length(idx)) set(combined, i = idx, j = col, value = NA_character_)
}
num_cols <- names(combined)[vapply(combined, is.numeric, logical(1))]
for (col in num_cols) {
  idx <- which(is.nan(combined[[col]]))
  if (length(idx)) set(combined, i = idx, j = col, value = NA_real_)
}
cat(sprintf("standardised missings to NA across %d character + %d numeric columns\n",
            length(char_cols), length(num_cols)))

# Emit .rds (canonical, read back by the pipeline), .csv, and .parquet so the
# server-delivery format can be chosen at ship time. See save_dataset() in functions.R.
save_dataset(combined, file.path(out_dir, "tender_data_2006_2026"))

message(sprintf("tender_data_2006_2026: %d rows, %d cols", nrow(combined), ncol(combined)))
print(combined[, .(rows = .N,
                   production = sum(grepl("production", cvr_method)),
                   extraction = sum(grepl("extraction", cvr_method))),
               by = .(data_source, entity)][order(data_source, entity)])
message("Written to ", out_dir)
