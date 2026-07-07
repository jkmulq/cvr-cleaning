# Cleaning Flag Data Dictionary

This file documents the flags in the final objects we normally inspect:

- `clean_winner_data_kfst.rds` and `clean_winner_data_ot.rds`
- `clean_buyer_data_kfst.rds` and `clean_buyer_data_ot.rds`
- `clean_winner_data_kfst_name_matched.rds` and
  `clean_winner_data_ot_name_matched.rds`
- `clean_buyer_data_kfst_name_matched.rds` and
  `clean_buyer_data_ot_name_matched.rds`

The focus is on final flags, not temporary processing variables. The structure
is:

1. clean data flags;
2. matching flags;
3. other flags, including source-specific and OpenTender partitioning flags.

The flags are intended for audit and review, not for mutually exclusive
classification. A row can have several flags at once.

## Clean Data Flags

These are the main flags created in the processing scripts and saved in the
final clean winner and buyer objects.

### Clean winner data

These flags appear in both final winner cleaning outputs:

- `data/clean/clean_winner_data_kfst.rds`
- `data/clean/clean_winner_data_ot.rds`

| Flag | Meaning | How to read it |
|---|---|---|
| `flag_cvr_ws` | The winner CVR candidate contained whitespace before cleaning. | Useful for checking simple formatting cleanup, for example `12 34 56 78`. |
| `flag_cvr_alphabet` | The winner CVR candidate contained letters before cleaning. | Often catches country prefixes such as `DK12345678`. |
| `flag_cvr_punct` | The winner CVR candidate contained punctuation before cleaning. | Useful for checking whether punctuation removal affected the CVR. |
| `flag_cvr_standardised` | At least one CVR formatting cleanup flag is `TRUE`. | Summary flag for rows where the CVR candidate was changed syntactically. |
| `flag_fill_missing_cvr` | A missing winner CVR was filled from another row with the same winner name. | This is same-name borrowing. It should not overwrite an existing CVR. |
| `flag_missing_winner_cvr` | The cleaned winner CVR is missing or blank. | Main flag for rows that still need CVR recovery or matching. |
| `flag_missing_winner_name` | The cleaned winner name is missing or blank. | Useful for separating rows that cannot be name matched. |
| `flag_foreign_winner` | The winner is marked as non-Danish. | A Danish CVR may not be expected. |
| `flag_missing_winner_country` | Winner country is missing. | Useful when deciding whether missing CVR is actually suspicious. |
| `flag_single_bidder` | The tender/lot received one bid. | Context flag, not a CVR quality problem by itself. |
| `flag_multilot` | The procurement has more than one lot. | Context flag for later analysis. |
| `flag_cancelled` | The source indicates the tender or lot was cancelled. | Context flag for interpreting missing or unusual award data. |
| `flag_missing_cvr_with_name` | Winner CVR is missing, but winner name is present. | These are natural candidates for name matching or external CVR lookup. |
| `flag_check_fuzzy_match` | The row should be sent to the name-matching workflow. | For winners, this means winner name is present and winner CVR is missing. |
| `flag_review_cvr` | A non-missing cleaned winner CVR is not syntactically valid. | Review cue for malformed CVRs. |
| `flag_no_winner_info` | Winner CVR, name, and country are all missing. | Nothing useful is available for CVR verification. |
| `flag_verify_cvr_external` | The row should be checked against an external CVR register. | Set for missing-CVR-with-name and invalid-CVR rows, but not for rows with no winner information. |

### Clean buyer data

KFST buyer data and OpenTender buyer data are less symmetric than the winner
data. KFST buyer data do not contain buyer CVR numbers in the processing script,
while OpenTender buyer data do. The shared buyer flags are therefore mostly
about buyer names and matching eligibility.

These flags appear in both final buyer cleaning outputs:

- `data/clean/clean_buyer_data_kfst.rds`
- `data/clean/clean_buyer_data_ot.rds`

| Flag | Meaning | How to read it |
|---|---|---|
| `flag_missing_buyer_name` | The cleaned buyer name is missing or blank. | Rows with missing buyer names cannot be name matched. |
| `flag_check_fuzzy_match` | The row should be sent to the name-matching workflow. | In KFST this means buyer name is present. In OpenTender this means buyer name is present and buyer CVR is missing. |

## Matching Flags

These flags appear in all four final matched datasets:

- `data/clean/clean_winner_data_kfst_name_matched.rds`
- `data/clean/clean_winner_data_ot_name_matched.rds`
- `data/clean/clean_buyer_data_kfst_name_matched.rds`
- `data/clean/clean_buyer_data_ot_name_matched.rds`

| Flag | Meaning | How to read it |
|---|---|---|
| `flag_name_match_found` | The matching workflow found a proposed CVR from the CVR name key. | Check the entity-specific match column: `winner_cvr_name_match` or `buyer_cvr_name_match`. |
| `flag_name_match_ambiguous` | A match was found, but more than one CVR candidate was possible. | These rows should not be treated as clean automatic matches without review. |
| `flag_review_name_match` | A found match should still be reviewed. | Usually means the match was fuzzy or ambiguous. In OpenTender it can also mean an unresolved potential multiple-name row. |
| `flag_manual_name_review` | The row is included in the compact manual-review output. | Includes no-match rows, fuzzy matches, ambiguous matches, and unresolved OpenTender name partitions. |

The matched datasets also contain non-flag matching metadata such as
`name_match_step`, `name_match_step_code`, `name_match_method`,
`name_match_score`, `name_match_n_candidates`, and `name_match_status`. Those
columns explain how the match was produced, but they are not themselves flags.

## Other Flags

These flags are final-object flags, but they are not shared across both KFST and
OpenTender in the same way as the clean data and matching flags above. They are
included because they explain important source-specific choices.

### Winner flags unique to one source

| Flag | Final object | Meaning |
|---|---|---|
| `flag_winner_cvr_changed` | KFST winner data | The displayed winner CVR changed during cleaning. |
| `flag_mismatch_winner_count` | KFST winner data | The number of extracted winners differs from the original listed winner count. |
| `flag_review_n_winners` | KFST winner data | Review cue for the KFST winner-count mismatch. |
| `flag_cvr_recovered_from_formatting` | OpenTender winner data | A Danish CVR was recovered after cautious formatting cleanup. |
| `flag_cvr_placeholder` | OpenTender winner data | The cleaned CVR was a known placeholder or dummy value and was set to missing. |
| `flag_row_multiple_valid_cvr` | OpenTender winner data | The original OpenTender source row contained more than one distinct valid Danish CVR. |

### Buyer flags unique to one source

| Flag | Final object | Meaning |
|---|---|---|
| `flag_joint_unlisted_buyers` | KFST buyer data | The source marks the tender as joint, but the buyer-name field does not list separable buyer names. |
| `flag_single_buyer_name_changed` | KFST buyer data | A single-buyer name changed during cleaning. |
| `flag_buyer_count_agree` | KFST buyer data | The extracted buyer count agrees with the original listed buyer count. |
| `flag_cvr_recovered_from_formatting` | OpenTender buyer data | A Danish CVR was recovered after cautious formatting cleanup. |
| `flag_row_multiple_valid_cvr` | OpenTender buyer data | The original OpenTender source row contained more than one distinct valid Danish CVR. |
| `flag_fill_missing_cvr` | OpenTender buyer data | A missing buyer CVR was filled from another row with the same buyer name. |
| `flag_cvr_placeholder` | OpenTender buyer data | The cleaned buyer CVR was a known placeholder or dummy value and was set to missing. |
| `flag_non_cvr_identifier` | OpenTender buyer data | A multi-CVR buyer row also contained an invalid non-CVR token, which was removed rather than sent to name matching. |
| `flag_cvr_ws` | OpenTender buyer data | The buyer CVR candidate contained whitespace before cleaning. |
| `flag_cvr_alphabet` | OpenTender buyer data | The buyer CVR candidate contained letters before cleaning. |
| `flag_cvr_punct` | OpenTender buyer data | The buyer CVR candidate contained punctuation before cleaning. |
| `flag_cvr_standardised` | OpenTender buyer data | At least one buyer CVR formatting cleanup flag is `TRUE`. |
| `flag_missing_buyer_cvr` | OpenTender buyer data | The cleaned buyer CVR is missing or blank. |
| `flag_foreign_buyer` | OpenTender buyer data | The buyer is marked as non-Danish. |
| `flag_missing_buyer_country` | OpenTender buyer data | Buyer country is missing. |
| `flag_multilot` | OpenTender buyer data | The procurement has more than one lot. |
| `flag_cancelled` | OpenTender buyer data | The source indicates the tender or lot was cancelled. |
| `flag_missing_cvr_with_name` | OpenTender buyer data | Buyer CVR is missing, but buyer name is present. |
| `flag_review_cvr` | OpenTender buyer data | A non-missing cleaned buyer CVR is not syntactically valid. |
| `flag_no_buyer_info` | OpenTender buyer data | Buyer CVR, name, and country are all missing. |
| `flag_verify_cvr_external` | OpenTender buyer data | The row should be checked against an external CVR register. |

### OpenTender partitioning flags

These flags are only created in the OpenTender matched datasets. They explain
when a single OpenTender winner or buyer name may actually contain multiple
firms.

| Flag | Final object | Meaning |
|---|---|---|
| `flag_name_partition_eligible` | OpenTender matched winner and buyer data | The original name looked separable enough to test for multiple firms. |
| `flag_joint_venture_text` | OpenTender matched winner and buyer data | Joint-venture language was detected in the name. |
| `flag_consortium_text` | OpenTender matched winner and buyer data | Consortium language was detected in the name. |
| `flag_collaboration_text` | OpenTender matched winner and buyer data | Any collaboration-style language was detected in the name. |
| `flag_potential_multiple_names` | OpenTender matched winner and buyer data | The row may contain multiple firm names and should not receive a simple one-name-to-one-CVR fill without review or successful partitioning. |
| `flag_name_partition_expanded` | OpenTender matched winner and buyer data | The original row was successfully expanded into separated firm rows. |
| `flag_separated_name` | OpenTender matched winner and buyer data | The final row was created from a successful name partition. |

## Notes For Later Analytical Reporting

For routine checks, start from the final clean and matched objects above rather
than temporary intermediate tables. If the goal is to diagnose OpenTender rows
where the same name may map to several CVRs, keep the analysis tied to
`row_id`, the original name and CVR columns, the cleaned CVR, and `source`, so
reviewers can trace every claim back to the raw row.
