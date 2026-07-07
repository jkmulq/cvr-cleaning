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
