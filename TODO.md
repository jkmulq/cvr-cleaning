# TODO

This file tracks development work against the current codebase and the project brief in `documents/HowFirmsGrow - documentation_2026_06.pdf`.

## KFST Cleaning

- [ ] Preserve original identifiers and names in final outputs.
  - The brief says original identifier and firm-name variables should always be retained, with cleaned identifiers and cleaning indicators stored in separate variables.
  - Current gap: the KFST workflow renames and parses `winner_cvr`, then builds `clean_data` from a narrow winner-column selection.
  - Target: keep explicit original columns such as `winner_cvr_original` and cleaned columns such as `winner_cvr_clean`.
- [ ] Carry tender-level fields into `clean_data` before creating tender-level flags.
  - Current gap: flags reference fields such as `n_bids_received` and `tender_cancelled`, but those variables are not selected into `clean_data` when single- and multi-winner records are bound.
- [ ] Standardize all non-missing CVR fields into exactly eight digits.
  - The brief expects prefixes, spaces, hyphens, punctuation, and other non-numeric characters to be cleaned where unambiguous.
  - Current gap: the script identifies some single CVRs and splits some multi-CVR fields, but it does not yet apply a general CVR standardization function.
- [ ] Add explicit manual-review flags.
  - Values that cannot be cleaned or separated unambiguously should be retained and flagged.
  - Current gap: flags cover missing and invalid CVRs, but do not yet distinguish successful standardization, multiple-CVR extraction, ambiguous values, and manual-review cases.
- [ ] Add a stable original-observation identifier before expanding records.
  - The brief asks for expanded winner or bidder rows to link back to the original procurement record.
  - Current gap: the workflow uses `tender_id` and `lot_id`, but an explicit original row or record id would make expansion safer and easier to audit.
- [ ] Add reproducible KFST outputs.
  - Target: write final cleaned data and diagnostics to `data/clean/`, including a compact summary of cleaning outcomes.

## Buyer And Name Matching

- [ ] Finish buyer cleaning.
  - Current gap: KFST buyer names are split and flagged for multiple buyers, but buyer entities are not yet linked to CVR numbers.
  - Target: add a reproducible matching workflow using virk.dk or another documented reference.
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
