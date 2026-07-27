# TODO

This file tracks development work against the current codebase and the project brief in `documents/HowFirmsGrow - documentation_2026_06.pdf`.

## KFST Cleaning

- [x] Preserve original identifiers and names in final outputs.
  - The brief says original identifier and firm-name variables should always be retained, with cleaned identifiers and cleaning indicators stored in separate variables.
  - Current gap: the KFST workflow renames and parses `winner_cvr`, then builds `clean_winner_data` from a narrow winner-column selection.
  - Target: keep explicit original columns such as `winner_cvr_original` and cleaned columns such as `winner_cvr_clean`.
- [x] Separate tender-level fields from cleaned winner and buyer tables.
  - Current state: tender-level fields live in `tender_lot_data`; narrow entity tables are joined to those fields at the end in `winner_analysis_data` and `buyer_analysis_data`.
- [x] Standardize all non-missing CVR fields into exactly eight digits.
  - The brief expects prefixes, spaces, hyphens, punctuation, and other non-numeric characters to be cleaned where unambiguous.
  - Current gap: the script identifies some single CVRs and splits some multi-CVR fields, but it does not yet apply a general CVR standardization function.
  - **Conclusion**: Most of the punctuation actually relates to winners without CVR numbers or names. I think the current process works better, and I don't think standardisation is needed in most cases.
- [x] Add explicit manual-review flags.
  - Values that cannot be cleaned or separated unambiguously should be retained and flagged.
  - **Conclusion**: flags cover missing and invalid CVRs, and also have standardisation and cleaning/processing flags.
- [x] Add a stable original-observation identifier before expanding records.
  - The brief asks for expanded winner or bidder rows to link back to the original procurement record.
  - **Conclusion**: the workflow uses `tender_id` and `lot_id` as the explicit flag. These two variables uniquely index the tender information in the original data. 
- [x] Add reproducible KFST outputs.
  - Target: write final cleaned data and diagnostics to `data/clean/`, including a compact summary of cleaning outcomes.
  - **Conclusion**: Will put separate tables for `clean_winnner_data` and `clean_buyer_data`. `clean_winner_data` will contain buyer-winner matches, but it won't have multiple buyers separated out row-by-row.

## Buyer And Name Matching

- [x] Finish buyer cleaning.
  - Current gap: KFST buyer names are split and flagged for multiple buyers, but buyer entities are not yet linked to CVR numbers.
- [x] CVR name to CVR matching from virk.dk
  - Add a reproducible matching workflow using virk.dk or another documented reference.
  - Conclusion: Uses the replication material provided keys; will update with new keys once access is approved. 
- [x] Add missing-CVR name matching.
  - The brief expects observations with missing CVR numbers but non-missing firm names to be assessed for unambiguous Danish firm matches.
  - Conclusion: exact matching and fuzzy matching process copies the process provided in the replication materials. Most of the matches come from exact matches; counts/proportions documented in diagnostic note. 
- [x] Add ambiguity flags for name matching.
  - Target: flag cases where a name could map to several CVR numbers or where the match confidence is too low.
- [ ] Validate existing cleaned CVR-name pairs against the provided CVR keys.
  - Check winner rows that already had a valid cleaned CVR and therefore did not need missing-CVR name matching.
  - Start with the KFST multiple-winner rows. For each expanded row, compare the cleaned CVR and winner name with the main-name and biname records for that CVR.
  - Use the same prepared name fields and tender-date rules as the name-matching process.
  - Report counts and proportions for exact agreement, fuzzy agreement, a CVR found with a different name, and a CVR not found in the provided keys.
  - Report results separately by dataset and cleaning source, and keep `tender_id`, `lot_id`, `winner_number`, the original values, and the cleaned values for review.
  - Add the results and representative disagreements to the quality analysis report.
- [ ] Speed up matching by matching distinct CVR names, then joining back.
  - The costly fuzzy step runs per row, but most rows repeat the same winner/buyer name. Reduce to the set of distinct prepared names, fuzzy-match each once, then expand the matches back onto every original row.
  - Draft implementations in `code/drafts/` (`2_1_match_kfst_distinct_winners_draft.R`, `2_2_match_kfst_buyers_distinct_names_draft.R`), each with a `*_normal_benchmark_draft.R` counterpart to confirm the distinct-name outputs match the row-level results.
  - Target: once outputs are confirmed identical to the benchmark, fold the distinct-name approach into the production matching scripts (`2_1`-`2_4`) for both KFST and OpenTender.

## OpenTender Cleaning

- [x] Implement conservative multiple-name partition rule.
  - Candidate for multi-firm separation if original name contains consortium/joint venture langauge AND/OR at least two legal types. 
  - Splits each candidate by all possible segments defined by specific delimiters. 
  - Partition accepted if all segments exact match to a unique identifiable name in the CVR name key. 
  - Partition Treat exact steps 1-4 equally.
  - Purpose: conservative test to minimise potential false positives
- [ ] Revisit ambiguous OpenTender exact-name matches.
  - Exact step 4 currently has 415 ambiguous matches out of 1,105 matches (37.6%).
  - These matches use the broadest prepared name and should not be treated as final without checking the competing CVRs.
- [ ] Reconsider the OpenTender fuzzy-matching thresholds.
  - Test raising the current thresholds by a few points to reduce false-positive CVR matches.
  - Compare match coverage and manually reviewed false-positive rates at each proposed threshold before choosing new cutoffs.
- [ ] Evaluate fuzzy-match confidence using the gap between the top two candidates.
  - Calculate the first-ranked score minus the second-ranked score for each winner name.
  - Treat small score gaps as less convincing because two CVRs fit the winner name almost equally well; large gaps provide stronger evidence for the top candidate.
  - Test whether requiring both a minimum top score and a minimum score gap reduces false positives without discarding too many useful matches.
- [x] Inspect raw OpenTender schemas across years.
  - Raw files are present for 2006-2026.
  - The brief focuses on the full available period up to 2026, while noting analysis had previously covered 2009-2023.
  - Conclusion: Script automatically detects whether column names agree across the source data files. Ignores loaded variable type (e.g. logical/integer/character) since whatever R detects can be fragile.  
- [x] Clean OpenTender bidder CVRs.
  - Target: clean `bidder bodyid id` into valid eight-digit CVR numbers where possible.
- [x] Investigate row-level single-valid-CVR overwrite edge cases.
  - Audit finding before fix: 36 rows had `flag_row_has_single_valid_cvr == TRUE` but retained invalid final CVR rows.
  - These occur when the row itself has exactly one valid CVR, but the same `winner_name` has multiple valid CVRs elsewhere, so `winner_cvr_clean_real` is missing.
  - Conclusion: invalid sibling tokens now collapse to the row's own single valid CVR; only `flag_cvr_borrowed_from_winner_name` distinguishes cross-row borrowing.
  - Key examples include `Nykredit A/S`, `Hoffmann A/S`, and `Deloitte Statsautoriseret Revisionsaktieselskab`.
- [x] Clean OpenTender buyer CVRs.
  - Target: clean `buyer bodyid id` into valid eight-digit CVR numbers where possible.
  - Conclusion: OpenTender buyer CVRs are cleaned in the processing workflow and unmatched missing buyer CVRs are sent to the buyer name-matching workflow.
- [x] Expand OpenTender multi-identifier fields.
  - Target: ensure cleaned data contain at most one bidder or buyer CVR per row when the source field contains multiple identifiers.
  - Conclusion: this is done in the matching script
- [x] De-duplicate the buyer-dimension explosion (tender-lot-entity grain).
  - Finding: OpenTender stores a fully exploded (tender x buyer x lot x bid x bidder) table, so joint-procurement / multi-buyer notices repeat every award once per buyer with identical amounts. ~28% of OpenTender winner rows were buyer-duplicates (up to 8x within multi-buyer tenders). Confirmed against TED notice 12607-2020: one real contracting authority but two "buyers" (Aura Energi + covered entity Dinel A/S), giving 30 rows for 15 real lot-winner records.
  - Fix (source; `1_1` and `1_2`): collapse before saving to one row per winner and per buyer within a tender-lot - winners on `(tender_id, lot_id, winner_number, winner_cvr_clean)`, buyers on `(tender_id, lot_id, buyer_number[, buyer_cvr_clean])`. `distinct(.keep_all = TRUE)`, no schema change. Applied symmetrically to KFST (a no-op there - already at grain).
  - Effect: OpenTender winner 161,510 -> 115,876; OpenTender buyer 163,103 -> 121,441; KFST unchanged. Event studies were already safe (they dedup by firm + award_date), but row-level award counts and amount sums were inflated ~28%. Propagated to the `*_name_matched.rds` outputs by the 2026-07-23 full run.
- [ ] Clean OpenTender tender- and lot-level variables.
  - Target: review and standardize dates, counts, amounts, indicators, and other tender fields while retaining the original source variables.
- [ ] Add reproducible OpenTender outputs and diagnostics.
  - RDS is the preferred output format.
  - Fix optional `.dta` export for OpenTender outputs. The current blocker is
    that some tender-level column names inherited from OpenTender are too long
    for Stata variable-name limits.
  - Target: add a readable, documented short-name mapping for tender-level
    columns before calling `haven::write_dta()`, while keeping the full column
    names in the `.rds` outputs.

## Overall cleaning
- [ ] Fix typos in the final cleaned and matched dataset. I'll define typo as a CVR number only different from another CVR attached to the same firm name, where one appears in the CVR key and the other doesn't.
- [x] Convert tender/lot amounts to a common currency (EUR and DKK). KFST amounts are DKK; OpenTender amounts are already EUR (built from the `_EUR` columns), so pooling them (e.g. `data_tender` in the employment report) previously mixed currencies.
  - Done: added `tender_amount_eur`/`tender_amount_dkk` and `lot_amount_eur`/`lot_amount_dkk` to both the winner and buyer tables in `1_1` (KFST) and `1_2` (OpenTender), using Denmark's fixed ERM II central rate (7.46038 DKK per EUR, +/-2.25% band; https://economy-finance.ec.europa.eu/euro/eu-countries-and-euro/denmark-and-euro_en). Propagated to the matched outputs by the 2026-07-23 full run (all four columns present in `*_name_matched.rds`).
  - Possible future refinement (not needed given the peg): contract-date daily EUR/DKK rates joined on `award_date` differ <1% from the fixed peg. Not yet converted: `bid_amount` and the annualised amounts (they inherit their source currency). And OpenTender's EUR was itself converted from native by OpenTender, so EUR->DKK double-converts DKK-native OT tenders - re-deriving from OT native `tender_finalPrice` + currency is the faithful fix if ever needed.

## Code robustness (from July 2026 systematic review)
None of these fire on the current data/API (the 2026-07-23 full run completed cleanly), but the High items are latent crashes that abort under other inputs/environments (a subset run, different data vintage, an API format change) - so they matter for replication-readiness.

### High (latent crashes)
- [ ] Guard the empty-`matched` update-join in every matching script (`2_1:315/369`, `2_2:305/346`, `2_3:802`, `2_4:814`). If zero rows match across all steps (realistic on a small/sample input), `matched` stays column-less and `winner_data[matched, on="match_row_id", :=…]` errors. Fix: `if (nrow(matched) > 0)` + pre-init the `*_name_match*` columns to typed NA when empty.
- [ ] Guard the OpenTender segment/partition stage against a 0-column `rbindlist` (`2_3:377`, `2_4:389`). When no eligible row yields a partition, `segment_remaining <- name_partition_segments[, .(…, match_date)]` throws `object 'match_date' not found`. Fix: short-circuit when `nrow(name_partition_segments)==0`, or seed a typed empty prototype.
- [ ] Fix the OpenTender multi-CVR split crash (`1_2:342` winners, `:755` buyers). Two CVRs joined by hyphen/period/`samt`/`and` aren't standardised to `;`, so `map_chr(extract_valid_cvr_candidates)` gets length 2 and aborts ("Result must be length 1, not 2"). Fix: split on any non-digit boundary (matching the multi-CVR counter), or extend the delimiter regex and `map()`+unnest.
- [ ] Harden the employment-pull production-unit scroll (`scraping/1_build_cvr_employment_history.R:1362`). `as.integer(hits.total)` crashes on an ES7 object total (`{value,relation}` → length-2 → "condition has length > 1") and silently under-collects if total is capped at 10k. Fix: `if (is.list(tot)) tot$value else tot`, and scroll until an empty page rather than trusting `total`.

### Medium
- [ ] Make the employment-pull resume atomic (`scraping/1_build_cvr_employment_history.R:1502/1558`). Data rows are appended before the status row; a crash between them re-pulls & duplicates rows on resume (a second duplicate source beyond the fixed cross-batch leakage). Fix: write status first (or temp+rename), or dedupe on read by `(cvr, frequency, year, quarter, month)`.
- [ ] Fix the isolated-annual event-study window (`6_firm_employment_quality.Rmd:708`). `window_years <- -2:2` but the prose/tab/labels say ±1 and isolation only requires gap `>1`, so ±2-edge awards contaminate "isolated" windows. Fix: set `-1:1`, or commit to ±2 and update isolation (`>2`) + all labels.
- [ ] `flag_joint_unlisted_buyers` is always FALSE (`1_1:711`) - tests `joint_tender=="joint"` but the recoded value lives in `joint_tender_original` (join suffix). Fix: test `joint_tender_original`.
- [ ] Normalise country in `flag_foreign_winner/buyer` (`1_1:572`, `1_2:564/1037`) with `toupper(trimws())`, matching every other script (raw `!= "DK"` mis-flags `"dk"`/`" DK"`).
- [ ] Reset OpenTender `row_id_borrowed_from` to NA on non-filled rows (`1_2:487`), as KFST and the OT buyer path already do (currently falsely implies a borrow).
- [ ] Concordance join can cartesian-explode (`5_cvr_key_concordance.Rmd:162`) - join on `.(cvr, name)` like the alt-names chunk, or add `allow.cartesian=TRUE`.
- [ ] Return the full empty schema from `find_fuzzy_matches`/`accept_fuzzy_match` (`functions.R:813/918`) - same latent bare-`data.table()` pattern as the already-fixed exact helper (guarded by callers today).
- [ ] Coerce CVR-key dates to IDate explicitly in `1_3_process_keys.R:57/94` (`as.IDate(substr(...,1,10))`) - currently relies on `fread` auto-typing; a full ISO timestamp would shrink the ±2-year match window to ±730 seconds.

### Low (bundle - full line refs in the review report)
- [ ] Sweep the low-severity items: dead code (`6_firm:79-80/311-326`, `7_tender_amounts:86`, KFST dead flags), stale comment (`1_2:161` framework anchor), missing `na.rm` (`3_quality:508`), `flag_awarded` NA-not-FALSE (`1_2:224`), `parse_summary_date` serial branch (`4_summary:83`), leading-zero `%in%` (`6_firm:99`), `combn` single-file guard (`1_2:39`), TED retry sleep-on-last-attempt + dead `n_missing`, `run_replication.sh:28` tee-flush. One-liners; none change results materially.

## Replication readiness
- [ ] Document data provenance for the raw inputs (KFST `udbudsdata_kfst.xlsx`, OpenTender CSVs, Virk CVR keys). Ask PI's coauthors where each source comes from and how a replicator obtains access, then add a "Data availability" section to the README.