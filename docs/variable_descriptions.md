# Variable Descriptions — shared schema of the matched winner datasets

This dictionary documents **every variable in the shared schema** of the two full matched winner
datasets:

- `data/clean/clean_winner_data_kfst_name_matched.rds` (162 columns)
- `data/clean/clean_winner_data_ot_name_matched.rds` (320 columns)

Only the **122 columns common to both** are listed here — the analytical schema that lets KFST and
OpenTender be pooled. Each source additionally carries its own native columns (OpenTender: the many
`tender_publications_*` source fields; KFST: `semi_tier`, `consortium_*`, tier-3b `field_*` pairing
columns); those source-specific columns are out of scope for this file. The matched **buyer** datasets
carry an analogous schema (`buyer_*` in place of `winner_*`). Flag semantics are also documented, from a
review angle, in [cleaning_flags.md](cleaning_flags.md).

**Grain:** one row per tender–lot–winner member (`tender_id` × `lot_id` × `winner_number`; consortium
members share a `winner_number` but are separate rows).

**"Origin" legend**
- **raw** — taken directly from the source data (KFST `udbudsdata_kfst.xlsx` / OpenTender CSVs), only
  renamed/typed.
- **raw→clean** — a raw value transformed in place (cleaning, recoding, currency, parsing).
- **derived** — computed by our code from other columns (not present in the source).

**"Created in" legend** — `1_1` = `code/processing/1_1_process_kfst.R` (KFST), `1_2` =
`code/processing/1_2_process_open_tender.R` (OpenTender); both build the corresponding clean winner
table. `2_1` = `code/processing/2_1_match_kfst.R`, `2_3` = `code/processing/2_3_match_opentender.R` add
the matching layer. `ted_dates_*` = the TED XML date-lineage chain (`code/scraping/ted_dates_*`), joined
onto the tender-lot data in `1_1`/`1_2`.

---

## 1. Tender / lot identifiers and structure

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `tender_id` | raw | 1_1 / 1_2 | — | Source tender identifier. KFST `Løbenummer` (e.g. `2`); OpenTender tender UUID (e.g. `00003a63-32cd-…`). |
| `lot_id` | raw | 1_1 / 1_2 | — | Source lot identifier within a tender. KFST `Nummerplade` (e.g. `2-1`); OpenTender `lot_lotId`. |
| `lot_number` | raw | 1_1 / 1_2 | — | Ordinal lot number (KFST `Delkontraktnr.`), e.g. `1`; often blank in OpenTender. |
| `winner_number` | derived | 1_1 / 1_2 | winner-field split | Winner index within the lot after splitting the winner field (`;` winners, `,` consortium members). Consortium members of one winner share the number. e.g. `1`. |
| `n_lots` | raw | 1_1 / 1_2 | — | Number of lots mapped for the tender. e.g. `1`. |
| `n_bids_received` | raw | 1_1 / 1_2 | — | Bids received on the lot, source value. e.g. `5`. |
| `n_bidders` | raw→clean | 1_1 / 1_2 | `n_bids_received` | Numeric bidder count (coerced). e.g. `5`. |

## 2. Tender attributes

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `contract_type` | raw→clean | 1_1 / 1_2 | source contract-type field | Recoded to English: `Public contract` / `Framework agreement`. |
| `divided_tender` | raw→clean | 1_1 / 1_2 | source divided field | Whether the tender is split into lots (KFST `Opdelt udbud`). Standardised to English `yes` / `no` in both sources (KFST `Ja`→`yes`, `Nej`→`no`). |
| `joint_tender` | raw→clean | 1_1 / 1_2 | source joint/single field | Recoded `single` / `joint`. |
| `consortium_winner` | raw | 1_1 / 1_2 | — | **Raw source flag**, not a cleaned indicator — it is the source's own "winner is a consortium" field (KFST `Konsortium/Sammenslutning`, `Ja`/`Nej`; OT its own value), carried through verbatim and *not* reconciled against the actual extracted consortium splits. Treat it as noisy provenance, not ground truth; the reliable consortium signal is the KFST split machinery (`is_consortium`, `consortium_number`). |
| `tender_cancelled` | raw→clean | 1_1 / 1_2 | source annulment field | Whether the lot/tender was annulled. Standardised to **logical** `TRUE`/`FALSE` in both sources (KFST `Ja`→`TRUE`, `Nej`→`FALSE`; OT already logical). |
| `flag_awarded` | derived | 1_1 / 1_2 | `tender_cancelled` (KFST) / `tender_isAwarded` (OT) | `TRUE` if the lot was actually awarded. **Difference from `tender_cancelled`:** `tender_cancelled` is the raw annulment field (was the procedure formally cancelled?), while `flag_awarded` is the derived usable-for-analysis signal — for KFST it is simply `!tender_cancelled`, but for OT it comes from the separate `tender_isAwarded` field, so a lot can be non-cancelled yet still not awarded. Many OT lots are not awarded because OT publishes contract-notice / in-progress rows that never reach an award (no winner recorded), not because they were cancelled. Drives the "keep awarded lots" filters. |

## 3. Amounts

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `tender_amount` | derived | 1_1 / 1_2 | final/estimated tender-value fields | Tender contract value in source currency (final, else estimated). e.g. `1.36e8` (DKK, KFST). |
| `lot_amount` | derived | 1_1 / 1_2 | final/estimated lot-value fields; `tender_amount`, `n_lots` | Per-lot value; if all lot values are missing, the tender value split equally across lots. |
| `lot_amount_orig` | derived | 1_1 / 1_2 | source lot value | The lot value **before** the equal-split fill. e.g. `3600000`. |
| `flag_all_orig_lot_amt_missing` | derived | 1_1 / 1_2 | `lot_amount_orig` | `TRUE` if every lot value in the tender was missing (so `lot_amount` was imputed by split). |
| `tender_amount_dkk` / `lot_amount_dkk` | derived | 1_1 / 1_2 | `tender_amount`/`lot_amount` | Value in DKK. KFST is already DKK; OT converted from EUR at the fixed rate. |
| `tender_amount_eur` / `lot_amount_eur` | derived | 1_1 / 1_2 | `tender_amount`/`lot_amount` | Value in EUR at Denmark's fixed ERM-II rate (7.46038 DKK/EUR). |
| `annualised_tender_amount` / `annualised_lot_amount` | derived | 1_1 / 1_2 | amount + `contract_duration_months` | For framework agreements only: amount per month × 12 (annualised); else `NA`. |

## 4. CPV (procurement category)

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `cpv_code` | raw | 1_1 / 1_2 | — | Raw CPV code(s) as listed. e.g. `85311300`. |
| `cpv_code_first` | derived | 1_1 / 1_2 | `cpv_code` | First listed CPV code (`clean_cpv_code()`), e.g. `85311300`. |
| `cpv_division` | derived | 1_1 / 1_2 | `cpv_code_first` | 2-digit CPV division, e.g. `85`. |
| `cpv_division_name` | derived | 1_1 / 1_2 | `cpv_division` | Division label, e.g. `Health and social work services`. |
| `cpv_sector` | derived | 1_1 / 1_2 | `cpv_code_first` | Coarser sector grouping, e.g. `Health, medical & pharma`. |
| `cpv_category` | derived | 1_1 / 1_2 | `cpv_code_first` | Works / Supplies / Services, e.g. `Services`. |

## 5. Dates and TED notice lineage

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `award_date` | raw→clean | 1_1 / 1_2 | source award-date field | Contract award date, parsed to `Date`, e.g. `2017-05-17`. |
| `submit_date` | raw | 1_1 | — | Tender submission deadline (KFST `Frist for aflevering af tilbud`); blank for OT. |
| `ted_notice_id` | derived | 1_1 / 1_2 | `award_url` (KFST) / `…lastContractAwardUrl` (OT) | TED notice id parsed from the award-notice URL (`derive_ted_notice_id()`), e.g. `304771-2017`. Links a lot to its TED XML. |
| `planning_dispatch_date` | derived | ted_dates_* → 1_1/1_2 | `ted_notice_id` lineage | Dispatch date of the **planning** (prior-information) notice. |
| `planning_publication_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Publication date of the planning notice. |
| `planning_tender_deadline_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Tender-receipt deadline on the planning notice (sparse). |
| `competition_dispatch_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Dispatch date of the **competition** (contract) notice. |
| `competition_publication_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Publication date of the competition notice. |
| `competition_tender_deadline_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Tender-submission deadline on the competition notice (the meaningful deadline; ~96% filled for KFST). |
| `award_dispatch_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Dispatch date of the **award** notice. |
| `award_publication_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Publication date of the award notice. |
| `award_tender_deadline_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Award-level tender deadline; rare, OpenTender-only (all-`NA` for KFST). Kept for schema parity. |
| `award_contract_date` | derived | ted_dates_* → 1_1/1_2 | lineage | Contract-award date from the award XML; agrees with KFST `award_date` ~91% same-day. |

## 6. Winner identity (raw + prepared names)

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `winner_name` | raw→clean | 1_1 / 1_2 | source winner-name field | Winner firm name for this member row (post winner-field split), e.g. `Personalegruppen A/S`. |
| `winner_country` | raw→clean | 1_1 / 1_2 | source country field | Winner country; all-Danish tokens normalised to `DK`; may be `DK,IE`-style for mixed consortia. |
| `winner_name_original` | raw | 1_1 / 1_2 | — | The whole, unsplit winner-name field as delivered (audit snapshot). |
| `winner_country_original` | raw | 1_1 / 1_2 | — | The whole, unsplit winner-country field as delivered. |
| `winner_name_in_data` | derived | 2_1 / 2_3 | `winner_name` | The winner name carried into matching (as seen in the data). |
| `winner_name_basic` | derived | 1_1 / 1_2 | `winner_name` | Lowercased, punctuation-stripped name (`prepare_cvr_name()`), e.g. `personalegruppen`. |
| `winner_name_match` | derived | 1_1 / 1_2 | `winner_name` | Primary matching form (firm-type-aware), e.g. `personalegruppen`. |
| `winner_name_no_spaces` | derived | 1_1 / 1_2 | `winner_name` | Name with spaces removed, for a looser exact match. |
| `winner_name_broad` | derived | 1_1 / 1_2 | `winner_name` | Broadest normalised form (sorted tokens) for fuzzy matching. |
| `winner_firm_type` | derived | 1_1 / 1_2 | `winner_name` | Detected legal form, e.g. `a/s`, `aps`. |
| `winner_name_first_letter` | derived | 1_1 / 1_2 | `winner_name_match` | First letter, a fuzzy-matching blocking key, e.g. `p`. |

## 7. Winner CVR cleaning and validity

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `winner_cvr_original` | raw | 1_1 / 1_2 | — | The whole, unsplit winner-CVR field as delivered (audit snapshot). |
| `winner_cvr_clean` | raw→clean | 1_1 / 1_2 | `winner_cvr_original` (KFST: field split) | Cleaned per-member CVR: whitespace/letters/punctuation stripped; may be filled by same-name borrow. e.g. `28706650`. |
| `valid_cvr` | derived | 1_1 / 1_2 | `winner_cvr_clean` | `TRUE` iff `winner_cvr_clean` is a well-formed 8-digit CVR (format only, not registry). |
| `flag_cvr_ws` | derived | 1_1 / 1_2 | `winner_cvr_original` | The CVR candidate contained whitespace before cleaning. |
| `flag_cvr_alphabet` | derived | 1_1 / 1_2 | `winner_cvr_original` | It contained letters (e.g. a `DK` prefix). |
| `flag_cvr_punct` | derived | 1_1 / 1_2 | `winner_cvr_original` | It contained punctuation. |
| `flag_cvr_standardised` | derived | 1_1 / 1_2 | the three flags above | Any CVR formatting cleanup fired. |
| `winner_cvr_valid_from_same_name` | derived | 1_1 / 1_2 | `winner_name`, `winner_cvr_clean` | The CVR borrowable from a same-name row, under a strict **one-to-one** rule (name↔CVR). |
| `flag_borrowed_cvr` | derived | 1_1 / 1_2 | `winner_cvr_clean`, `winner_cvr_valid_from_same_name` | `TRUE` when a missing CVR was filled by that one-to-one same-name borrow. |

## 8. Winner missingness / review flags

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `flag_missing_winner_cvr` | derived | 1_1 / 1_2 | `winner_cvr_clean` | Cleaned CVR missing/blank. |
| `flag_missing_winner_name` | derived | 1_1 / 1_2 | `winner_name` | Cleaned name missing/blank. |
| `flag_missing_winner_country` | derived | 1_1 / 1_2 | `winner_country` | Country missing. |
| `flag_foreign_winner` | derived | 1_1 / 1_2 | `winner_country` | Winner marked non-Danish. |
| `flag_single_bidder` | derived | 1_1 / 1_2 | `n_bidders` | The lot received one bid. |
| `flag_multilot` | derived | 1_1 / 1_2 | `n_lots` | The procurement has multiple lots. |
| `flag_cancelled` | derived | 1_1 / 1_2 | `tender_cancelled` | Source marks the tender/lot cancelled. |
| `flag_missing_cvr_with_name` | derived | 1_1 / 1_2 | `flag_missing_winner_cvr`, `flag_missing_winner_name` | CVR missing but name present — a name-match candidate. **Redundant with `flag_matching_candidate`** (identical condition; see note below). Kept only for backward compatibility; prefer `flag_matching_candidate`. |
| `flag_matching_candidate` | derived | 1_1 / 1_2 | `flag_missing_winner_cvr`, `flag_missing_winner_name` (name present & CVR missing) | Row is eligible for the name-matching workflow (formerly `flag_check_fuzzy_match`). Canonical flag for "name present, CVR missing". |
| `flag_review_cvr` | derived | 1_1 / 1_2 | `valid_cvr`, `flag_missing_winner_cvr` | Non-missing CVR that is not syntactically valid. |
| `flag_no_winner_info` | derived | 1_1 / 1_2 | the missingness flags | CVR, name, and country all missing. |
| `flag_verify_cvr_external` | derived | 1_1 / 1_2 | `flag_missing_cvr_with_name`, `flag_review_cvr` | Row worth checking against an external CVR register. |
| `flag_missing_winner_cvr_final` | derived | 2_1 / 2_3 | `winner_cvr_final` | **Post-match** version of `flag_missing_winner_cvr`: CVR missing/blank *after* matching/borrowing/field-pairing filled it. See the [post-match flags note](#post-match-_final-review-flags). |
| `flag_missing_cvr_with_name_final` | derived | 2_1 / 2_3 | `flag_missing_winner_cvr_final`, `winner_name` | Post-match version of `flag_missing_cvr_with_name`. |
| `flag_review_cvr_final` | derived | 2_1 / 2_3 | `flag_missing_winner_cvr_final`, `flag_cvr_final_in_registry` | Post-match version of `flag_review_cvr`: has a final CVR that is **not** in the registry. |
| `flag_no_winner_info_final` | derived | 2_1 / 2_3 | `flag_missing_winner_cvr_final`, `winner_name`, `winner_country` | Post-match version of `flag_no_winner_info`. |
| `flag_verify_cvr_external_final` | derived | 2_1 / 2_3 | `flag_missing_cvr_with_name_final`, `flag_review_cvr_final` | Post-match version of `flag_verify_cvr_external`. |

## 9. Name matching — candidates and outcome

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `winner_cvr_name_match` | derived | 2_1 / 2_3 | `winner_name_*`, CVR name key | CVR found by matching the name to the registry, e.g. `37311065`. |
| `registered_name_match` | derived | 2_1 / 2_3 | `winner_cvr_name_match` | Registered name of that matched CVR, e.g. `Playtype Foundry ApS`. |
| `name_match_source` | derived | 2_1 / 2_3 | matcher | Which key matched: `name` or `biname`. |
| `name_match_step` | derived | 2_1 / 2_3 | matcher | Matching step (1–4 exact tiers, 5–6 fuzzy). |
| `name_match_method` | derived | 2_1 / 2_3 | matcher | `exact` or `fuzzy`. |
| `name_match_score` | derived | 2_1 / 2_3 | matcher | Similarity score of the accepted match (populated only for matched rows), e.g. `100`. |
| `name_match_n_candidates` | derived | 2_1 / 2_3 | matcher | Number of tied CVR candidates for the match. |
| `fuzzy_candidate_cvr_1…5` | derived | 2_1 / 2_3 | fuzzy matcher | Top-5 fuzzy candidate CVRs considered. |
| `fuzzy_candidate_name_1…5` | derived | 2_1 / 2_3 | fuzzy matcher | Registered names of those candidates. |
| `fuzzy_candidate_score_1…5` | derived | 2_1 / 2_3 | fuzzy matcher | Their similarity scores. |
| `fuzzy_candidate_source_1…5` | derived | 2_1 / 2_3 | fuzzy matcher | `name`/`biname` per candidate. |
| `fuzzy_candidate_step_1…5` | derived | 2_1 / 2_3 | fuzzy matcher | Matching step per candidate. |
| `flag_name_match_found` | derived | 2_1 / 2_3 | `winner_cvr_name_match` | A candidate CVR was found by matching. |
| `flag_name_match_ambiguous` | derived | 2_1 / 2_3 | `name_match_n_candidates` | Match found but >1 candidate CVR. |
| `flag_review_name_match` | derived | 2_1 / 2_3 | match method/ambiguity | Found match still needs review (fuzzy/ambiguous). |
| `flag_manual_name_review` | derived | 2_1 / 2_3 | review flags | Row is in the compact manual-review output. |
| `name_match_status` | derived | 2_1 / 2_3 | flags above | Readable status: `matched`, `not requested`, `manual review - …`. |

## 10. Final CVR and provenance

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `winner_cvr_final` | derived | 2_1 / 2_3 | `field_paired_cvr`, `winner_cvr_clean`, `winner_cvr_name_match` | The resolved winner CVR. Precedence differs by source (KFST: field/backfill beats name match; OT: name match overrides). e.g. `28706650`. |
| `cvr_number_source` | derived | 2_1 / 2_3 | resolution precedence + `flag_borrowed_cvr`, `type`, `name_match_*` | Plain-English provenance of `winner_cvr_final` (raw field split / tier-3b field pairing / exact-fuzzy match / backfilled from another lot / not a candidate). **Full value-by-value dictionary in the [Notes](#cvr_number_source--value-dictionary) below.** |
| `matching_candidate_type` | derived | 2_1 / 2_3 | `flag_matching_candidate`, `winner_country` | Why admitted to matching: `exact DK` / `contains DK` / `NA`. |
| `flag_cvr_recovered_from_invalid` | derived | 2_1 / 2_3 | `winner_cvr_candidate_original`, `winner_cvr_final`, registry | Original field CVR wasn't a registered CVR but a valid final was recovered (typo/extra-digit/foreign/placeholder). |
| `flag_cvr_final_in_registry` | derived | 2_1 / 2_3 | `winner_cvr_final`, registry | `TRUE` iff the final CVR exists in the CVR registry (stricter than `valid_cvr`). |

## 11. CVR ↔ name quality (independent QA)

| Variable | Origin | Created in | Depends on | Description & example |
|---|---|---|---|---|
| `cvr_name_match_quality` | derived | 2_1 / 2_3 | `winner_cvr_final`, `winner_name_match`, registry | Levenshtein ratio of the winner name vs the registered name of the final CVR (default form), e.g. `100`. |
| `cvr_name_match_quality_basic` | derived | 2_1 / 2_3 | `winner_name_basic`, registry | Same quality score under the basic name form. |
| `cvr_name_match_quality_nospaces` | derived | 2_1 / 2_3 | `winner_name_no_spaces`, registry | Under the no-spaces form. |
| `cvr_name_match_quality_broad` | derived | 2_1 / 2_3 | `winner_name_broad`, registry | Under the broad form. |
| `cvr_name_match_quality_name` | derived | 2_1 / 2_3 | `winner_cvr_final`, registry | The registered name the quality was scored against, e.g. `personalegruppen`. |
| `cvr_name_is_substring` | derived | 2_1 / 2_3 | `winner_name_match`, `cvr_name_match_quality_name` | `TRUE` if the winner name is a verbatim substring of the registered name. |

---

## Notes

### Post-match `*_final` review flags

The CVR missingness/review flags come in two vintages:

- **Un-suffixed** (`flag_missing_winner_cvr`, `flag_missing_cvr_with_name`, `flag_review_cvr`,
  `flag_no_winner_info`, `flag_verify_cvr_external`) are computed in the **cleaning** scripts (1_1 / 1_2)
  on `winner_cvr_clean` — i.e. **before matching**. They describe the raw-data state and drive the matcher
  (e.g. `flag_matching_candidate` decides who gets searched), so they intentionally stay pre-match.
- **`*_final`** versions are computed in the **matchers** (2_1 / 2_3) on `winner_cvr_final` — i.e.
  **after** name matching, same-name borrowing, and (KFST) tier-3b field pairing have resolved a CVR. A row
  that had no CVR in the raw field but was matched by name is `flag_missing_winner_cvr == TRUE` yet
  `flag_missing_winner_cvr_final == FALSE`. `flag_review_cvr_final` uses `flag_cvr_final_in_registry`
  (registry membership) as its validity signal rather than the syntactic `valid_cvr`.

For "what is still missing/suspect in the delivered data", read the `*_final` flags. For "what the raw
source provided", read the un-suffixed ones. The **buyer** datasets carry the same `*_final` family
(`flag_missing_buyer_cvr_final`, etc.) for parity — and for KFST buyers, which have no source CVR at all,
the `*_final` flags are the only meaningful CVR missingness signal (buyer CVRs are assigned entirely by
matching). KFST buyers have no country field, so `flag_no_buyer_info_final` omits the country term.

### `cvr_number_source` — value dictionary

`cvr_number_source` is the single plain-English provenance label for `winner_cvr_final`. It is assigned in
2_1 (KFST) / 2_3 (OT) by a first-match-wins `fcase`, so the **order below is the precedence order**: a row
gets the first label whose condition it satisfies. The two sources share the same *structure* but use
slightly different wording and have source-specific cases (KFST field-pairing; OT name-partition and
consortium-removed steps). Read it as four families:

**1. CVR came straight from the raw award field** (no name matching needed — the source already gave a CVR):
- KFST: `CVR from the original winner field: extracted after separating by semi-colon` / `... separating consortium members by comma` / `... existing CVR, other misc split`.
- OT: `source: single CVR cleaning` / `source: multiple CVR separation` / `source: existing CVR, unclassified`.

**2. CVR recovered without a registry match**, in precedence order:
- `CVR from tier-3b field pairing: ...` (KFST only) — the winner name was paired to a CVR already listed on the same lot (lots with more names than field CVRs).
- `CVR backfilled from another lot: ...` (both) — this row had no valid CVR, so the CVR of a **same-exact-name** winner elsewhere in the dataset was borrowed (the one-to-one borrow; `flag_borrowed_cvr = TRUE`).

**3. CVR came from a name→registry match.** Each value names the winning step so the match quality is legible:
- KFST exact: `exact matching: basic name and firm type` → `... no spaces and firm type` → `... no spaces` → `... broad name` (steps 1–4).
- KFST fuzzy: `fuzzy matching: prepared main name` / `... prepared biname` (step 5) → `... broad main name` / `... broad biname` (step 6).
- OT adds `exact partition: ...` (steps 1–4 on partitioned multi-winner names), `exact: ...` (steps 1–4), `exact consortium removed: ...` (steps 5–8, matched after stripping consortium wording), and `fuzzy: ...` (steps 5–6, main name / biname). "partition" and "consortium removed" are OT-specific because OT winner names arrive concatenated.

**4. No CVR resolved** — why the row has none:
- `matching candidate: no match found` — it was eligible and searched (name present, country contains DK) but nothing matched.
- `not a matching candidate: not marked as Danish` — has a name but country is not DK, so it was never searched (the registry is Danish-only).
- `not a matching candidate: no CVR name` — no usable name to match on.

### `fuzzy_candidate_*_1` need not equal the accepted match

`fuzzy_candidate_cvr_1` is the CVR of the **single best-scoring** fuzzy candidate across *all* fuzzy steps
(the global argmax of `fuzzy_candidate_score`). The **accepted** match (`winner_cvr_final` when
`name_match_method == "fuzzy"`) is chosen by a *different* rule — `accept_fuzzy_match()` takes the
**earliest step** that clears the threshold **and** has a *unique* top score in that step
(`fuzzy_candidate_rank == 1 & score > threshold & n_top_score_candidates == 1`). So the two can disagree
when the globally-highest-scoring candidate sits in a step where the top score is **tied** (rejected as
ambiguous) while an earlier/later step offers a lower but *unambiguous* score that is accepted. This is
expected and rare (≈1 KFST row, ≈14 OT rows). Example (KFST): a step-5 candidate scores 87.5 but ties
another candidate at 87.5 → rejected; step-6 offers 86.7 uniquely → accepted. `fuzzy_candidate_cvr_1`
reports the 87.5 CVR, `winner_cvr_final` carries the 86.7 CVR. When you need the CVR actually used, read
`winner_cvr_final` / `name_match_*`, not `fuzzy_candidate_*_1`.

---

*Scope note:* this covers the 122 shared columns. Source-specific columns (OpenTender
`tender_publications_*`, name-partition flags; KFST `semi_tier`, `is_consortium`, `consortium_number`,
`field_paired_*`, `field_cvr_*`, `flag_winner_cvr_changed`, `flag_mismatch_winner_count`) are documented
where relevant in [cleaning_flags.md](cleaning_flags.md).

*TED/XML build parity:* the TED winner dataset (`ted_winner_data_name_matched.rds`, built by
`ted_4_match_winners.R`) carries this **same shared schema** — the matching/quality family, the review
flags (incl. the `*_final` versions), the aliases (`award_date`, `submit_date`, `tender_amount`,
`lot_amount`, `cpv_*`, `winner_*_original`, …), and the lineage dates. It keeps its TED-specific source
columns too (`amount_awarded`, `date_contract_award`, `cpv_main`, `procedure_type`, …). A few columns
differ in how they are produced, because TED is a different source:
- **Currency** — TED amounts are in the notice's original `currency` (multi-currency), so `*_eur`/`*_dkk`
  are filled only for DKK and EUR rows via the exact EUR↔DKK peg; other currencies are `NA`.
- **Annualised amounts** — `framework_duration_days` is extracted from the linked **competition** notice
  (award notices omit it); `annualised_*` are filled for frameworks with a positive duration, else `NA`.
- **Same-name borrow** — TED does not borrow CVRs across rows, so `flag_borrowed_cvr` is always `FALSE`
  and `winner_cvr_valid_from_same_name` is always `NA`.
- **`consortium_winner` / `joint_tender`** — no TED source field, so `NA` (TED already splits consortia
  into one row per organisation).
