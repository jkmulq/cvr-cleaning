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
- [ ] CVR name to CVR matching from virk.dk
  - Add a reproducible matching workflow using virk.dk or another documented reference.
- [ ] Add missing-CVR name matching.
  - The brief expects observations with missing CVR numbers but non-missing firm names to be assessed for unambiguous Danish firm matches.
  - Current gap: this is not yet implemented for winners, bidders, or buyers.
- [ ] Add ambiguity flags for name matching.
  - Target: flag cases where a name could map to several CVR numbers or where the match confidence is too low.

## OpenTender Cleaning

- [ ] Inspect raw OpenTender schemas across years.
  - Raw files are present for 2006-2026.
  - The brief focuses on the full available period up to 2026, while noting analysis had previously covered 2009-2023.
- [ ] Clean OpenTender bidder CVRs.
  - Target: clean `bidder bodyid id` into valid eight-digit CVR numbers where possible.
- [ ] Clean OpenTender buyer CVRs.
  - Target: clean `buyer bodyid id` into valid eight-digit CVR numbers where possible.
- [ ] Expand OpenTender multi-identifier fields.
  - Target: ensure cleaned data contain at most one bidder or buyer CVR per row when the source field contains multiple identifiers.
- [ ] Add reproducible OpenTender outputs and diagnostics.

## Scope Decisions

- [ ] Decide whether job ad CVR cleaning belongs in this repository.
  - The brief mentions job ad data from 2007-2022, including firm CVRs and workplace identifiers.
  - Current state: no job ad data or scripts are present in this repository.
