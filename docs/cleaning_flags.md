# Cleaning Flag Data Dictionary

This file documents the cleaning, review, delimiter, and quality flags created
by `code/1_process_kfst.R` and `code/2_process_open_tender.R`.

The flags are intended for audit and review, not for mutually exclusive
classification. A row can have several flags at once. Boolean flags are stored
as `TRUE` or `FALSE`, with explicit missingness flags used where missing source
values matter. Original source values are kept beside cleaned values so a human
reviewer can inspect how each cleaned value was produced.

## KFST Winner Data

These variables are created while building `clean_winner_data` in
`code/1_process_kfst.R`. The final KFST winner files are written to
`data/clean/clean_winner_data_kfst.rds` and
`data/clean/clean_winner_data_kfst.dta`.

| Name | Dataset/object | Level | Values | Derivation and example |
|---|---|---|---|---|
| `missing_winner_cvr` | `winner_data` | KFST tender-lot source row | `TRUE` when the raw `winner_cvr` is missing; otherwise `FALSE` or `NA` before later filtering | Early processing indicator used before winner expansion. Example: a tender-lot row with no listed winner CVR is marked before the script decides whether the row can be treated as a single-CVR row. |
| `single_cvr` | `winner_data` | KFST tender-lot source row | `TRUE` for a reliably single raw CVR; `NA` otherwise | Set when the raw CVR is exactly eight digits with no delimiter, or exactly eight digits after removing spaces. Example: `12 34 56 78` is treated as a single CVR candidate. |
| `flag_cvr_ws` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if whitespace was detected in `winner_cvr_clean` before whitespace removal; otherwise `FALSE` | Flags a syntactic standardisation step. Example: `12 34 56 78` becomes `12345678`. |
| `flag_cvr_hyphen` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if a hyphen was detected before hyphen removal; otherwise `FALSE` | Flags CVRs such as `12-34-56-78` before standardisation. |
| `flag_cvr_alphabet` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if letters were detected before letter removal; otherwise `FALSE` | Flags source strings such as `DK12345678`, where the cleaned candidate keeps the numeric part. |
| `flag_cvr_punct` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if punctuation was detected before punctuation removal; otherwise `FALSE` | Flags source strings with punctuation other than the earlier hyphen handling. |
| `flag_cvr_standardised` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if any CVR standardisation flag is `TRUE`; otherwise `FALSE` | Summarises whether whitespace, hyphen, letters, or punctuation were removed from the CVR candidate. |
| `valid_cvr` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if `winner_cvr_clean` is exactly eight digits; otherwise `FALSE` | Validity is syntactic only. Example: `12345678` is valid; `123456789` and missing values are not. |
| `flag_winner_cvr_changed` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if `winner_cvr_clean` differs from `winner_cvr_candidate_original`; otherwise `FALSE` | Shows where the displayed CVR candidate changed during cleaning. Example: `DK12345678` is changed to `12345678`. |
| `flag_missing_winner_cvr` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if the cleaned CVR is missing or blank; otherwise `FALSE` | Explicit missingness flag for CVR values after cleaning. |
| `flag_missing_winner_name` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if the cleaned winner name is missing or blank; otherwise `FALSE` | Explicit missingness flag for winner names after expansion. |
| `flag_foreign_winner` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if `winner_country` is not `DK`; otherwise `FALSE` | Flags non-Danish winners because a Danish CVR may not be expected. |
| `flag_missing_winner_country` | `clean_winner_data` | Expanded KFST winner row | `TRUE` if `winner_country` is missing; otherwise `FALSE` | Explicit missingness flag for winner country. |
| `n_winners_extracted` | `clean_winner_data` | KFST tender-lot group | Integer count | Counts expanded winner rows within each `tender_id` and `lot_id`. Example: a lot split into three winner rows has `n_winners_extracted = 3` on each of those rows. |
| `flag_mismatch_winner_count` | `clean_winner_data` | KFST tender-lot group | `TRUE` if extracted winners differ from the original listed count; otherwise `FALSE` | Compares `n_winners_extracted` with `n_lot_winners_original`. |
| `flag_single_bidder` | `clean_winner_data` | Expanded KFST winner row with tender-lot context | `TRUE` when `n_bids_received == 1`; otherwise `FALSE` | Context flag for awards where only one bid was received. |
| `flag_multilot` | `clean_winner_data` | Expanded KFST winner row with tender-lot context | `TRUE` when the procurement has more than one lot; otherwise `FALSE` | Context flag based on `n_lots`. |
| `flag_cancelled` | `clean_winner_data` | Expanded KFST winner row with tender-lot context | `TRUE` when `tender_cancelled` is not `Nej`; otherwise `FALSE` | Context flag for cancelled procurement records retained in the source. |
| `flag_missing_cvr_with_name` | `clean_winner_data` | Expanded KFST winner row | `TRUE` when CVR is missing but winner name is present; otherwise `FALSE` | Review cue for cases where an external register lookup may recover the CVR. |
| `flag_review_cvr` | `clean_winner_data` | Expanded KFST winner row | `TRUE` when a non-missing cleaned CVR is not syntactically valid; otherwise `FALSE` | Review cue for malformed CVR candidates. |
| `flag_review_n_winners` | `clean_winner_data` | KFST tender-lot group | `TRUE` when `flag_mismatch_winner_count` is `TRUE`; otherwise `FALSE` | Review cue for lots where the extraction count disagrees with the original winner count. |
| `flag_no_winner_info` | `clean_winner_data` | Expanded KFST winner row | `TRUE` when cleaned winner CVR, name, and country are all missing; otherwise `FALSE` | Marks rows where there is no winner information to verify. |
| `flag_verify_cvr_external` | `clean_winner_data` | Expanded KFST winner row | `TRUE` for rows that should be checked against an external CVR register; otherwise `FALSE` | Set for missing-CVR-with-name and invalid-CVR cases. Not set for rows with no winner information or already valid CVRs. |

## KFST Buyer Data

These variables are created while building `clean_buyer_data` in
`code/1_process_kfst.R`. KFST buyer data do not include buyer CVR numbers in
the current cleaning script; these flags concern buyer names and buyer counts.

| Name | Dataset/object | Level | Values | Derivation and example |
|---|---|---|---|---|
| `flag_joint_unlisted_buyers` | `clean_buyer_data` | KFST buyer row | `TRUE` for joint tenders kept as one buyer row because additional buyers are not listed in `buyer_name`; otherwise `FALSE` | Flags joint tenders where the source says the procurement is joint but the buyer-name field does not list multiple separable buyers. |
| `flag_single_buyer_name_changed` | `clean_buyer_data` | KFST buyer row | `TRUE` if a single-buyer row's cleaned `buyer_name` differs from `buyer_name_original`; otherwise `FALSE` | Review cue for single-buyer name transformations. |
| `flag_missing_buyer_name` | `clean_buyer_data` | KFST buyer row | `TRUE` if `buyer_name` is missing or blank; otherwise `FALSE` | Explicit missingness flag for buyer names. |
| `n_buyers_extracted` | `clean_buyer_data` | KFST tender-lot group | Integer count | Counts buyer rows extracted for each `tender_id` and `lot_id`. |
| `n_buyers_listed_original` | `clean_buyer_data` | KFST tender-lot source row | Integer count | Counts semicolon-separated buyer names in `buyer_name_original`. Example: `A;B` gives `2`. |
| `flag_buyer_count_agree` | `clean_buyer_data` | KFST tender-lot group | `TRUE` if extracted and original buyer counts agree; otherwise `FALSE` | Compares `n_buyers_extracted` with `n_buyers_listed_original`. |

## OpenTender Delimiter And Source-Row Flags

These variables are created on `winner_data` in `code/2_process_open_tender.R`
before OpenTender winner rows are split or cleaned. They are source-row flags:
the level is the original OpenTender bidder row identified by `row_id` and
`tender_id`.

| Name | Dataset/object | Level | Values | Derivation and example |
|---|---|---|---|---|
| `delim_flag_missing` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` is missing or blank; otherwise `FALSE` | Identifies rows with no bidder body identifier before delimiter handling. |
| `delim_flag_comma` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `,`; otherwise `FALSE` | Detects comma-separated candidate strings. Commas are treated as valid delimiters. |
| `delim_flag_semicolon` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `;`; otherwise `FALSE` | Records semicolon presence in the raw source string. |
| `delim_flag_period` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `.`; otherwise `FALSE` | Audit flag only; periods are not accepted as winner delimiters in the current script. |
| `delim_flag_pipe` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `|`; otherwise `FALSE` | Detects pipe-separated candidate strings. Pipes are treated as valid delimiters. |
| `delim_flag_slash` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `/`; otherwise `FALSE` | Slash can mean either a delimiter or part of a name-like identifier, so accepted slash rows are manually listed. |
| `delim_flag_space` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains whitespace; otherwise `FALSE` | Audit flag for spaced identifiers such as `DK21 47 96 83`. |
| `delim_flag_hyphen` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `-`; otherwise `FALSE` | Audit flag; many hyphen cases are non-Danish body identifiers rather than multi-winner delimiters. |
| `delim_flag_no_punct` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` is non-missing and has no punctuation; otherwise `FALSE` | Helps describe the delimiter landscape before manual review. |
| `delim_flag_ampersand` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `&`; otherwise `FALSE` | Ampersand can mark multiple winners in a few manually accepted cases. |
| `delim_flag_colon` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `:`; otherwise `FALSE` | Audit flag for colon-containing identifiers. |
| `delim_flag_og` | `winner_data` | OpenTender source row | `TRUE` if raw `winner_cvr` contains `og`; otherwise `FALSE` | Detects Danish "and" strings that may separate multiple winners. Example: manually accepted row_id `59588` is one reviewed multi-winner-name case. |
| `delim_flag_valid_comma` | `winner_data` | OpenTender source row | `TRUE` when comma is accepted as a delimiter; otherwise `FALSE` | Currently mirrors `delim_flag_comma`; accepted commas are converted to semicolons before splitting. |
| `delim_flag_valid_pipe` | `winner_data` | OpenTender source row | `TRUE` when pipe is accepted as a delimiter; otherwise `FALSE` | Currently mirrors `delim_flag_pipe`; accepted pipes are converted to semicolons before splitting. |
| `delim_flag_valid_slash` | `winner_data` | OpenTender source row | `TRUE` for manually accepted slash-delimiter `row_id`s; otherwise `FALSE` | Example accepted rows include `73374`, `140635`, `141894`, `146029`, and `157184`. |
| `delim_flag_valid_ampersand` | `winner_data` | OpenTender source row | `TRUE` for manually accepted ampersand-delimiter `row_id`s; otherwise `FALSE` | Example accepted rows include `62215`, `65494`, and `148062`. |
| `delim_flag_valid_og` | `winner_data` | OpenTender source row | `TRUE` for manually accepted `og` delimiter `row_id`s; otherwise `FALSE` | Example accepted rows include `59588`, `78505`, `105116`, `144636`, `146512`, and `156134`. |
| `flag_review_slash` | `winner_data` | OpenTender source row | `TRUE` when slash is present but the row is not manually accepted as a delimiter case; otherwise `FALSE` | Manual-review cue for ambiguous slash usage. |
| `flag_review_ampersand` | `winner_data` | OpenTender source row | `TRUE` when ampersand is present but the row is not manually accepted as a delimiter case; otherwise `FALSE` | Manual-review cue for ambiguous ampersand usage. |
| `flag_review_og` | `winner_data` | OpenTender source row | `TRUE` when `og` is present but the row is not manually accepted as a delimiter case; otherwise `FALSE` | Manual-review cue for ambiguous `og` usage. |
| `flag_manual_review` | `winner_data`, later `clean_winner_data` | OpenTender source row carried to expanded winner rows | `TRUE` when any delimiter-review flag is `TRUE`; otherwise `FALSE` | Example: a row with an unaccepted slash in `winner_cvr` is marked for manual review before any CVR standardisation. |
| `manual_review_reason` | `winner_data`, later `clean_winner_data` | OpenTender source row carried to expanded winner rows | Text reason or `NA` | Currently records `check whether bidder ID contains multiple winning firms` when delimiter usage needs manual review. |
| `flag_multi_winner` | `winner_data`, later `clean_winner_data` | OpenTender source row carried to expanded winner rows | `TRUE` when the source string has an accepted multi-value delimiter; otherwise `FALSE` | This means the row has an accepted delimiter, not necessarily several distinct valid CVRs. |
| `single_cvr` | `winner_data` | OpenTender source row | `TRUE` when raw `winner_cvr` is non-missing and not an accepted multi-winner string; otherwise `FALSE` | Processing indicator for rows routed to the single-winner branch. It does not itself mean the CVR is syntactically valid. |
| `flag_multiple_distinct_valid_cvrs` | `winner_data`, later `clean_winner_data` | OpenTender source row carried to expanded winner rows | `TRUE` when the accepted-delimited source string contains more than one distinct eight-digit CVR; otherwise `FALSE` | Example: a source row with two different valid Danish CVRs is flagged even before deciding whether the winner name represents multiple firms. |
| `flag_multiple_distinct_winner_names` | `winner_data`, later `clean_winner_data` | OpenTender source row carried to expanded winner rows | `TRUE` for manually confirmed rows where multiple distinct valid CVRs correspond to multiple firm names; otherwise `FALSE` | Example: row_id `59588` is in the manually reviewed list and is split into separate winner-name rows. |

## OpenTender Winner/Bidder CVR And Quality Flags

These variables are created in `code/2_process_open_tender.R` while building
the in-memory OpenTender `clean_winner_data` object. The table preserves the
cleaned candidate beside the original OpenTender values joined from
`original_tender_data`.

| Name | Dataset/object | Level | Values | Derivation and example |
|---|---|---|---|---|
| `valid_cvr` | `multi_cvr_nondistinct_names_data_long`, `clean_winner_data` | OpenTender firm-name candidate row, then expanded winner row | `TRUE` if the cleaned CVR candidate is exactly eight digits; otherwise `FALSE` | In the final table this is recalculated from `winner_cvr_clean`. Example: `21479683` is valid; `111562071` is not. |
| `n_valid_cvr` | `valid_invalid_cvr_winner_key` | OpenTender firm-name candidate group | Integer count | Counts distinct valid CVR candidates observed for a given `winner_name` in the multi-CVR, non-distinct-name branch. |
| `n_total_cvr` | `valid_invalid_cvr_winner_key`, joined to multi-CVR branch rows | OpenTender firm-name candidate group | Integer count | Counts all distinct CVR candidates observed for a given `winner_name` in the multi-CVR, non-distinct-name branch. |
| `n_valid_cvr_in_row` | `multi_cvr_nondistinct_names_data_long`, later `clean_winner_data` | OpenTender source row and winner-name group | Integer count | Counts distinct valid CVR candidates in the original `(row_id, tender_id, winner_name)` after token cleaning. Example: row_id `79` has one valid cleaned CVR for `Dako Norden A/S` even though the source string also contains invalid or repeated identifiers. |
| `winner_cvr_clean_row` | `multi_cvr_nondistinct_names_data_long`, later `clean_winner_data` | OpenTender source row and winner-name group | Eight-digit CVR or `NA` | Stores the row's own single valid cleaned CVR when `(row_id, tender_id, winner_name)` has exactly one valid CVR. It is used to collapse invalid sibling tokens from the same source row before any cross-row borrowing. |
| `winner_cvr_clean_real` | `single_valid_cvr_key`, joined to multi-CVR branch rows | OpenTender winner-name candidate group | Eight-digit CVR or `NA` | Stores the single valid CVR observed across the same `winner_name` when that name has exactly one valid candidate and more than one total candidate in the multi-CVR branch. It is the possible cross-row borrowing reference. |
| `winner_cvr_clean_reference` | `multi_cvr_nondistinct_names_data_long`, later `clean_winner_data` | OpenTender expanded winner row | Eight-digit CVR or `NA` | Shows the reference CVR used to overwrite invalid sibling tokens. It prefers `winner_cvr_clean_row` and falls back to `winner_cvr_clean_real` only when the row itself has no valid CVR. |
| `flag_row_has_single_valid_cvr` | `multi_cvr_nondistinct_names_data_long`, later `clean_winner_data` | OpenTender source row and winner-name group | `TRUE` when `(row_id, tender_id, winner_name)` has exactly one distinct valid cleaned CVR; otherwise `FALSE` | Separates row-level evidence from cross-row inference. Example: row_id `79` is `TRUE` because the original row already contains a detectable valid CVR for `Dako Norden A/S`. |
| `flag_cvr_borrowed_from_winner_name` | `multi_cvr_nondistinct_names_data_long`, later `clean_winner_data` | OpenTender source row and winner-name group carried to expanded winner rows | `TRUE` when the original row has no valid CVR and the script fills the cleaned CVR from the single valid CVR observed elsewhere for the same `winner_name`; otherwise `FALSE` | This is narrower than simply finding one valid CVR for the firm name. Example: a row with only an invalid identifier for a winner name can borrow the valid CVR if another row with that exact winner name supplies exactly one valid CVR. Row_id `79` is not borrowed because it already contains a valid CVR. |
| `flag_winner_has_multi_valid_cvr` | `multi_cvr_nondistinct_names_data_long`, later `clean_winner_data` | OpenTender firm-name candidate group carried to expanded winner rows | `TRUE` when the same `winner_name` has more than one valid CVR candidate in the multi-CVR branch; otherwise `FALSE` | Review cue for names that may contain typos, reused names, or genuine ambiguity. Example: a future analytical report can compare candidate CVR digit distance to distinguish likely one-digit typos from more substantive conflicts. |
| `flag_cvr_ws` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if whitespace was detected in the separated raw CVR candidate before whitespace removal; otherwise `FALSE` | Flags syntactic standardisation even when the branch cleaned CVRs before binding. Example: `DK21 47 96 83` has whitespace before cleaning. |
| `flag_cvr_hyphen` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if a hyphen was detected in the separated raw CVR candidate; otherwise `FALSE` | Flags hyphenated candidate strings before any branch-level cleanup. |
| `flag_cvr_alphabet` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if letters were detected in the separated raw CVR candidate; otherwise `FALSE` | Flags country prefixes and other alphabetic characters. Example: `DK21479683` becomes `21479683`. |
| `flag_cvr_punct` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if punctuation remains after whitespace, hyphen, and letter removal; otherwise `FALSE` | Flags candidate strings where punctuation was removed during standardisation. Delimiters introduced only to display collapsed candidate sets are not counted as cleaning evidence. |
| `flag_cvr_standardised` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if any CVR standardisation flag is `TRUE`; otherwise `FALSE` | Summarises whether the separated raw candidate token behind the final row required syntactic cleanup. |
| `flag_winner_cvr_changed` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if `winner_cvr_clean` differs from `winner_cvr_candidate_original`; otherwise `FALSE` | Example: `DK21479683` changes to `21479683`. The full OpenTender source string remains available in `winner_cvr_original` after the original tender row is joined back on. |
| `flag_missing_winner_cvr` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if `winner_cvr_clean` is missing or blank; otherwise `FALSE` | Explicit missingness flag after OpenTender CVR cleaning. |
| `flag_missing_winner_name` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if `winner_name` is missing or blank; otherwise `FALSE` | Explicit missingness flag for winner names after expansion. |
| `flag_foreign_winner` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` when `winner_country` is present and not `DK`; otherwise `FALSE` | Flags non-Danish winners because a Danish CVR may not be expected. Missing country is handled separately. |
| `flag_missing_winner_country` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` if `winner_country` is missing or blank; otherwise `FALSE` | Explicit missingness flag for country. |
| `n_winners_extracted` | `clean_winner_data` | OpenTender source-row group | Integer count | Counts expanded winner rows within each `row_id` and `tender_id`. Example: a manually confirmed multi-winner row split into two firms has `n_winners_extracted = 2` on both rows. |
| `flag_single_bidder` | `clean_winner_data` | OpenTender expanded winner row with tender context | `TRUE` when parsed `n_bids_received` equals `1`; otherwise `FALSE` | Context flag based on the original OpenTender tender fields. |
| `flag_multilot` | `clean_winner_data` | OpenTender expanded winner row with tender context | `TRUE` when parsed `n_lots` is greater than `1`; otherwise `FALSE` | Context flag for multi-lot OpenTender records. |
| `flag_cancelled` | `clean_winner_data` | OpenTender expanded winner row with tender context | `TRUE` when the original tender or lot has a cancellation date; otherwise `FALSE` | Derived in `original_tender_data` from `tender_cancellationDate` or `lot_cancellationDate`. |
| `flag_missing_cvr_with_name` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` when CVR is missing but winner name is present; otherwise `FALSE` | Review cue for possible external CVR lookup. |
| `flag_review_cvr` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` when a non-missing cleaned CVR is not syntactically valid; otherwise `FALSE` | Review cue for malformed CVR candidates after standardisation. |
| `flag_no_winner_info` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` when cleaned winner CVR, name, and country are all missing; otherwise `FALSE` | Marks rows where there is no useful winner information to verify. |
| `flag_verify_cvr_external` | `clean_winner_data` | OpenTender expanded winner row | `TRUE` for rows that should be checked against an external CVR register; otherwise `FALSE` | Set for missing-CVR-with-name and invalid-CVR cases. Not set for rows with no winner information or already valid CVRs. |

## Notes For Later Analytical Reporting

This dictionary can be used as the backbone for a markdown or HTML data-quality
report. The most important OpenTender follow-up is likely
`flag_winner_has_multi_valid_cvr`: for those rows, compare the candidate CVR
numbers within each firm-name group. If two valid CVRs differ by only one digit,
that pattern may suggest a typo; if they differ in several positions, it may
suggest a genuinely ambiguous firm name, a reused name, or another source-data
issue.

When producing that report, keep the analysis tied to the original OpenTender
`row_id`, `winner_name_original`, `winner_cvr_original`, cleaned
`winner_cvr_clean`, and the source branch recorded in `source`, so reviewers can
trace every claim back to the raw row.
