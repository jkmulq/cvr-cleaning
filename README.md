# CVR Cleaning

This repository cleans Danish public procurement data and prepares CVR-number
matches for winners and buyers in the KFST and OpenTender data sources.

The README is a navigation and replication guide. For substantive data-quality
details, see:

- Download the [quality report HTML](https://github.com/jkmulq/cvr-cleaning/releases/download/quality-report/3_quality_analysis.html) (rendered from [this](code/analysis/3_quality_analysis.Rmd) Rmarkdown) 
- [Cleaning flag dictionary](docs/cleaning_flags.md)
- [Function manual](docs/functions_manual.md)

If you want to skip to replication, navigate to the [Configuration](#configuration)
and then [Replication](#replication) sections. Otherwise, there's a table of
contents below.

## Contents

- [Repository structure](#repository-structure)
- [What each script does](#what-each-script-does)
- [Required local inputs](#required-local-inputs)
- [Configuration](#configuration)
- [Replication](#replication)
- [Main outputs](#main-outputs)
- [Match quality and cleaning flags](#match-quality-and-cleaning-flags)

## Repository structure

```text
cvr-cleaning/
├── README.md
├── TODO.md
├── config.R
├── run_replication.sh
├── code/
│   ├── functions.R
│   ├── processing/                      # data cleaning + CVR matching pipeline
│   │   ├── 0_build_cvr_lookup.R
│   │   ├── 1_1_process_kfst.R
│   │   ├── 1_2_process_open_tender.R
│   │   ├── 1_2b_recover_ted_winners.R
│   │   ├── 1_3_process_keys.R
│   │   ├── 2_1_match_kfst.R
│   │   ├── 2_2_match_kfst_buyers.R
│   │   ├── 2_3_match_opentender.R
│   │   ├── 2_4_match_opentender_buyers.R
│   │   ├── 3_1_build_kfst_winner_datasets.R
│   │   ├── 3_2_build_ot_winner_datasets.R
│   │   └── 4_augment_matched_variables.R
│   ├── analysis/                        # R Markdown quality/analysis reports
│   │   ├── 3_quality_analysis.Rmd
│   │   ├── 4_summary_stats.Rmd
│   │   ├── 5_cvr_key_concordance.Rmd
│   │   ├── 6_firm_employment_quality.Rmd
│   │   ├── 6a_estudy_control.Rmd
│   │   ├── 7_tender_amounts_eu.Rmd
│   │   ├── 8_notice_date_gaps.Rmd
│   │   ├── 9_reconcile_processed_data.Rmd
│   │   ├── 10_twfe_estudy_cvr_method.Rmd
│   │   └── find_control_firms.R
│   └── scraping/                        # optional web/API pulls (run after matching)
│       ├── 1_build_cvr_employment_history.R
│       ├── 1_2a_fetch_notices.R
│       ├── 1_2b_build_notice_lineage.R
│       ├── 1_2c_extract_notice_dates.R
│       ├── 1_2d_build_field_dictionary.R
│       ├── 1_2e_build_date_panel.R
│       ├── 2_extract_ted_notices.R
│       └── notice_lineage_utils.R
├── docs/
│   ├── cleaning_flags.md
│   ├── functions_manual.md
│   └── 3_quality_analysis.html
├── tests/
│   └── test_kfst_winner_datasets.R
├── data/
│   ├── raw/
│   ├── cvr_matching_data/
│   └── clean/
├── output/
│   └── docs/
├── renv/
└── renv.lock
```

The `data/` and `output/` folders are local working folders. They are expected
to contain inputs and generated outputs, and should not be treated as complete
repository source code.

## What each script does

The workflow is staged. Scripts beginning with `1_` clean inputs and prepare
lookup keys; scripts beginning with `2_` perform name matching and build the
winner datasets. KFST winners are consortium-expanded (one row per member), and
both sources ship a "robustness stack" of winner-CVR variants (see
[Main outputs](#main-outputs)). Analysis notebooks and the quality report are
generated separately (listed below the pipeline table).

| Script | Purpose | Main outputs |
|---|---|---|
| [code/functions.R](code/functions.R) | Shared helper functions for CVR extraction, CVR formatting, name preparation, name partitioning, and matching support. | No direct output. |
| [code/processing/0_build_cvr_lookup.R](code/processing/0_build_cvr_lookup.R) | Optional script for users with Virk system-to-system API access. Builds CVR official-name and alternative-name lookup CSVs, or runs a small API timing sample. | Timestamped `cvr_names_virk_*.csv` and `cvr_binavne_virk_*.csv`, or sample CSVs. |
| [code/processing/1_1_process_kfst.R](code/processing/1_1_process_kfst.R) | Cleans KFST winner and buyer data with a **consortium-aware tiered split**: `;` separates winners and `,` separates consortium members, so each member becomes its own row tagged with `semi_tier`, `is_consortium`, and `consortium_number`. Standardises winner CVRs (all-Danish country tokens are normalised to `"DK"`), creates matching-ready name fields, and saves clean KFST objects. | `clean_winner_data_kfst.rds`, `clean_buyer_data_kfst.rds`. |
| [code/processing/1_2_process_open_tender.R](code/processing/1_2_process_open_tender.R) | Reads all annual OpenTender CSVs present in `data/raw/OpenTender/`, checks column-name concordance before binding, keeps source-file and source-row provenance, derives tender/lot amount and framework-duration variables, cleans winner and buyer CVR fields, removes non-CVR tokens from multi-CVR buyer rows, fills some missing CVRs when the same firm name appears elsewhere with one valid CVR, prepares matching-ready names, and saves clean OpenTender objects with the original tender fields attached. | `clean_winner_data_ot.rds`, `clean_buyer_data_ot.rds`. |
| [code/processing/1_2b_recover_ted_winners.R](code/processing/1_2b_recover_ted_winners.R) | Optional/manual. Folds TED-recovered winners (from the notice-lineage pull under `code/scraping/`) back into the OpenTender winner table. Run only when the TED notice data has been built. | updated OpenTender winner data. |
| [code/processing/1_3_process_keys.R](code/processing/1_3_process_keys.R) | Cleans the CVR register name keys used for later matching. It prepares both official names and alternative names. | `clean_cvr_name_key.rds`, `clean_cvr_biname_key.rds`. |
| [code/processing/2_1_match_kfst.R](code/processing/2_1_match_kfst.R) | Matches KFST winner names to CVRs against the prepared CVR-name keys (tier-3b consortium members are paired to the lot's own listed field CVRs first). Writes the **consortium-expanded** canonical winner table with the CVR-name **quality columns** and the provenance flag `flag_cvr_recovered_from_invalid`. | `clean_winner_data_kfst_name_matched.rds`, `manual_name_review_kfst.rds`. |
| [code/processing/2_2_match_kfst_buyers.R](code/processing/2_2_match_kfst_buyers.R) | Matches KFST buyer names to CVRs, since KFST buyer CVRs are not supplied in the raw source. | `clean_buyer_data_kfst_name_matched.rds`, `manual_buyer_name_review_kfst.rds`. |
| [code/processing/2_3_match_opentender.R](code/processing/2_3_match_opentender.R) | Matches missing OpenTender winner CVRs, records ambiguous/fuzzy cases for review, and writes winner-name partition diagnostics. Also **scores CVR-name quality inline** — the same quality columns + `flag_cvr_recovered_from_invalid` as KFST `2_1` (this scoring used to be a separate step). | `clean_winner_data_ot_name_matched.rds`, `manual_name_review_ot.rds`, `winner_name_partition_diagnostics_ot.rds`. |
| [code/processing/2_4_match_opentender_buyers.R](code/processing/2_4_match_opentender_buyers.R) | Matches missing OpenTender buyer CVRs and records ambiguous or fuzzy cases for review. Also writes buyer-name partition diagnostics. | `clean_buyer_data_ot_name_matched.rds`, `manual_buyer_name_review_ot.rds`, `buyer_name_partition_diagnostics_ot.rds`. |
| [code/processing/3_1_build_kfst_winner_datasets.R](code/processing/3_1_build_kfst_winner_datasets.R) | Builds the KFST winner **robustness stack** — `base` / `extraction` / `name_only` variants in one table (a `dataset` factor). Not consumed downstream; for robustness comparison. | `kfst_winner_datasets_stacked.rds`. |
| [code/processing/3_2_build_ot_winner_datasets.R](code/processing/3_2_build_ot_winner_datasets.R) | Builds the OpenTender winner **robustness stack** — `base` / `extraction` / `name_only` variants (a `dataset` factor), mirroring KFST `3_1`. Not consumed downstream. | `ot_winner_datasets_stacked.rds`. |
| [code/processing/4_augment_matched_variables.R](code/processing/4_augment_matched_variables.R) | Maintenance utility for additive tender/lot-level updates. Re-attaches newly created variables from the clean datasets onto the existing `*_name_matched.rds` files without re-running the slow name-matching scripts. Use only when the cleaning changes are add-only and do not alter names, CVRs, or row expansion. | refreshed `*_name_matched.rds` files in place. |

The [code/analysis/](code/analysis) notebooks and helpers run **manually, after matching**:

| Notebook / script | Purpose |
|---|---|
| [3_quality_analysis.Rmd](code/analysis/3_quality_analysis.Rmd) | Match-quality and data-quality report → `docs/3_quality_analysis.html`. |
| [4_summary_stats.Rmd](code/analysis/4_summary_stats.Rmd) | Summary statistics for the matched winner/buyer datasets. |
| [5_cvr_key_concordance.Rmd](code/analysis/5_cvr_key_concordance.Rmd) | Concordance checks on the CVR-name keys. |
| [6_firm_employment_quality.Rmd](code/analysis/6_firm_employment_quality.Rmd) | Quality of the pulled Virk firm-employment data. |
| [6a_estudy_control.Rmd](code/analysis/6a_estudy_control.Rmd) | Control-matched firm-employment event studies. |
| [7_tender_amounts_eu.Rmd](code/analysis/7_tender_amounts_eu.Rmd) | Tender/lot-amount and EUR-conversion checks. |
| [8_notice_date_gaps.Rmd](code/analysis/8_notice_date_gaps.Rmd) | Coverage and gaps in the TED notice dates. |
| [9_reconcile_processed_data.Rmd](code/analysis/9_reconcile_processed_data.Rmd) | Reconcile the KFST vs OpenTender processed data. |
| [10_twfe_estudy_cvr_method.Rmd](code/analysis/10_twfe_estudy_cvr_method.Rmd) | TWFE event study testing whether the CVR-resolution method (matched / extraction / old) changes the firm-employment estimates. |
| [find_control_firms.R](code/analysis/find_control_firms.R) | Builds the matched control group (one control per winner-event) for the event study. |

The [code/scraping/](code/scraping) folder holds optional web/API data pulls
that run **after** matching, because they consume the matched datasets. They
need network access and are off by default:

| Script | Purpose | Main outputs |
|---|---|---|
| [code/scraping/1_build_cvr_employment_history.R](code/scraping/1_build_cvr_employment_history.R) | Pulls annual/quarterly/monthly employment history from the Virk CVR API for the winner/buyer CVRs across all winner variants (matched + extraction/name_only + prior-production sets). Resumable and tolerant of missing optional inputs (requires Virk credentials). Feeds `6_firm_employment_quality.Rmd`. | `data/clean/cvr_employment_history_virk.csv` (+ `_status.csv`). |
| [code/scraping/1_2a_fetch_notices.R](code/scraping/1_2a_fetch_notices.R) | TED notice lineage, stage 1: fetches award/competition/planning notice XML in dependency order (requires internet; cached). | cached TED notice XML. |
| [code/scraping/1_2b_build_notice_lineage.R](code/scraping/1_2b_build_notice_lineage.R) | TED notice lineage, stage 2: assembles the notice-links lineage from the cached XML (parsing only, no network). | `notice_links` lineage table. |
| [code/scraping/1_2c_extract_notice_dates.R](code/scraping/1_2c_extract_notice_dates.R) | Extracts every date from every notice in the lineage, tied back to tender/lot. | extracted notice dates. |
| [code/scraping/1_2d_build_field_dictionary.R](code/scraping/1_2d_build_field_dictionary.R) | Builds a TED field dictionary (XML element → plain-English) from the EU schema label files and annotates the extracted dates. | annotated dates + field dictionary. |
| [code/scraping/1_2e_build_date_panel.R](code/scraping/1_2e_build_date_panel.R) | Builds the OpenTender notice-date panel from the extracted/annotated dates. | OpenTender date panel. |
| [code/scraping/notice_lineage_utils.R](code/scraping/notice_lineage_utils.R) | Shared helpers for the TED notice-lineage pair (`1_2a`/`1_2b`). | No direct output. |
| [code/scraping/2_extract_ted_notices.R](code/scraping/2_extract_ted_notices.R) | Fetches TED notice XML for OpenTender award notices and flags whether non-winning tenderers are listed (requires internet). | `data/intermediates/ted/` (cached XML + per-notice indicators). |

Enable them in a run with `BUILD_EMPLOYMENT_HISTORY=true` and/or
`EXTRACT_TED_NOTICES=true` (see [Replication](#replication)). Both are resumable
and can also be run on their own with `Rscript` once the matched datasets exist.

## Required local inputs

The repository expects the following local input folders:

```text
data/raw/kfst/
data/raw/OpenTender/
data/cvr_matching_data/
```

Expected source files:

- `data/raw/kfst/udbudsdata_kfst.xlsx`
- annual OpenTender CSV files under `data/raw/OpenTender/`
- `data/cvr_matching_data/cvr_names_virk_*.csv`
- `data/cvr_matching_data/cvr_binavne_virk_*.csv`

The OpenTender script reads all semicolon-delimited CSV files present in
`data/raw/OpenTender/`, checks that their column names concord, and then binds
them into one cleaning dataset with a source-file identifier (`dataset`) and a
stable source-row identifier (`row_id`). The replication sample is therefore
determined by the files placed in that folder.

## Configuration

All scripts source [config.R](config.R). The main setting is `PROJECT_DIR`, the
root of this repository.

For the standard replication workflow, no edits should be needed. `config.R`
locates the repository root automatically by searching upward from the working
directory for the `cvr-cleaning.Rproj` marker, so scripts and reports resolve
paths correctly whether they are run from the root or from `code/processing/`,
`code/analysis/`, etc. [run_replication.sh](run_replication.sh) still runs from
the repository root.

If you run an individual R script or knit a report manually, keep the
`cvr-cleaning.Rproj` marker at the repository root (open the project in RStudio,
or clone the repo intact). `config.R` falls back to the working directory, with a
warning, only if the marker cannot be found.

The derived paths in `config.R` are:

```text
dirs$raw_data      -> data/raw/
dirs$cvr_key       -> data/cvr_matching_data/
dirs$clean_data    -> data/clean/
dirs$intermediates -> data/intermediates/
dirs$code          -> code/
```

`config.R` creates the expected local data output directories (`data/raw/`,
`data/clean/`) if they are missing, but it does not download or create the raw
input files.

## Replication

### 1. Add local input data

Place the KFST, OpenTender, and CVR-name-key files in the folders listed above
before restoring the R environment or running the workflow. The raw data are
local inputs and are not committed to this repository.

Required for the cleaning scripts:

```text
data/raw/kfst/udbudsdata_kfst.xlsx
data/raw/OpenTender/*.csv
```

Required for the CVR-name-key and matching scripts:

```text
data/cvr_matching_data/cvr_names_virk_*.csv
data/cvr_matching_data/cvr_binavne_virk_*.csv
```

The key-processing script uses the newest timestamped Virk files in this folder.
The original `cvr_names_full.csv` and `cvr_binavne_full.csv` files can remain in
the same directory for old/new key comparisons.

[run_replication.sh](run_replication.sh) checks for these local inputs before it
runs `renv::restore()` or any processing scripts. If `RUN_MATCHING=false`, the
script only requires the KFST and OpenTender raw inputs.

### 2. Restore the R environment

The project uses `renv`. On a new machine, restore the package environment once:

```bash
Rscript --vanilla -e 'renv::restore(prompt = FALSE)'
```

Alternatively, let the replication script do this:

```bash
RESTORE_RENV=true ./run_replication.sh
```

When using the `RESTORE_RENV=true` option, the script still checks that the
local input data are present before restoring packages.

### 3. Optional: rebuild the CVR lookup from Virk

The matching workflow uses CVR lookup files built from the Virk
system-to-system API. Store your credentials locally, not in the repository.

Copy [.Renviron.example](.Renviron.example) to `.Renviron` in either your home
folder or this project folder, or set the variables in another local environment
setup:

```text
VIRK_CVR_USER="your-virk-username"
VIRK_CVR_PASSWORD="your-virk-password"
```

The API helper reads these `.Renviron` files explicitly, so this also works when
running scripts with `Rscript --vanilla`. To test API speed and output shape on
a small sample without overwriting the full lookup files:

```bash
CVR_LOOKUP_SAMPLE_SIZE=100 Rscript --vanilla code/processing/0_build_cvr_lookup.R
```

This writes sample files under `data/cvr_matching_data/`. To build a new full
lookup from Virk:

```bash
BUILD_CVR_LOOKUP=true ./run_replication.sh
```

By default, the API build writes timestamped comparison files such as
`cvr_names_virk_YYYYMMDD_HHMMSS.csv` and
`cvr_binavne_virk_YYYYMMDD_HHMMSS.csv`. The matching workflow uses the newest
timestamped Virk files. These generated files remain local inputs and are not
committed to the repository.

### 4. Run the full workflow

From the repository root:

```bash
./run_replication.sh
```

The script runs:

```text
code/processing/1_1_process_kfst.R
code/processing/1_2_process_open_tender.R
code/processing/0_build_cvr_lookup.R  # only if BUILD_CVR_LOOKUP=true
code/processing/1_3_process_keys.R
code/processing/2_1_match_kfst.R
code/processing/2_2_match_kfst_buyers.R
code/processing/2_3_match_opentender.R
code/processing/2_4_match_opentender_buyers.R
code/processing/3_1_build_kfst_winner_datasets.R
code/processing/3_2_build_ot_winner_datasets.R
code/scraping/1_build_cvr_employment_history.R  # only if BUILD_EMPLOYMENT_HISTORY=true
code/scraping/2_extract_ted_notices.R           # only if EXTRACT_TED_NOTICES=true
```

The default replication script does **not** run
`code/processing/4_augment_matched_variables.R`. That script is a targeted
maintenance utility for cases where `1_1_process_kfst.R` or
`1_2_process_open_tender.R` gained new tender/lot-level variables and you want
to refresh existing matched datasets without re-running the buyer-matching
steps. It is only valid when those processing changes are additive.

Outputs are written to `data/clean/` (and, for the optional TED pull,
`data/intermediates/`).

Expected run time depends on the machine. The figures below are from a full
reference run (2026-08-25, **~2h 15m** total); they scale with the hardware but the
shape holds — name matching dominates, and buyer matching most of all. (The
matching scripts fuzzy-match distinct names once and join the results back, which
roughly halved the pipeline from an earlier ~3h 32m.)

Reference machine: Apple M2 (8-core), 24 GB RAM, macOS 15.7.4 (Sequoia, arm64),
R 4.5.1.

| Stage | Scripts | Approximate run time |
|---|---|---|
| Input checks | built into `run_replication.sh` | seconds |
| Environment restore, if `RESTORE_RENV=true` | `renv::restore()` | depends on whether packages are already installed |
| Cleaning only | `code/processing/1_1_process_kfst.R`, `code/processing/1_2_process_open_tender.R` | ~1.5 minutes |
| CVR-name-key preparation | `code/processing/1_3_process_keys.R` | ~4 minutes |
| Winner matching | `code/processing/2_1_match_kfst.R`, `code/processing/2_3_match_opentender.R` | ~22 minutes (KFST ~5m; OpenTender ~16m, now incl. inline quality scoring) |
| Winner robustness stacks | `code/processing/3_1_build_kfst_winner_datasets.R`, `code/processing/3_2_build_ot_winner_datasets.R` | ~45 minutes (the OpenTender name-only pass in `3_2` dominates) |
| Buyer matching | `code/processing/2_2_match_kfst_buyers.R`, `code/processing/2_4_match_opentender_buyers.R` | ~57 minutes (KFST ~24m, OpenTender ~33m) |

For a quick check that the cleaning scripts still run, use `RUN_MATCHING=false`.
For a full matched dataset, plan for roughly **~2h 15m**, split mainly between
the OpenTender name-only robustness pass in `3_2` and the buyer-matching scripts.

### 5. Run cleaning only

To stop after the KFST and OpenTender cleaning scripts, without building the CVR
name keys or running name matching:

```bash
RUN_MATCHING=false ./run_replication.sh
```

### 6. Optional: employment history and TED notices

These post-matching pulls (in [code/scraping/](code/scraping)) consume the
matched datasets, so they only run when matching runs (`RUN_MATCHING=true`, the
default). Both are resumable — an interrupted pull continues on the next run.

Build firm employment history from the Virk CVR API (needs Virk credentials, as
in step 3):

```bash
BUILD_EMPLOYMENT_HISTORY=true ./run_replication.sh
```

Extract TED notice data for OpenTender award notices (needs internet access):

```bash
EXTRACT_TED_NOTICES=true ./run_replication.sh
```

The flags combine with each other and with the other options. Because their
inputs are the matched `*_name_matched.rds` files, you can also run either script
on its own once those files exist, without re-running the pipeline:

```bash
Rscript --vanilla code/scraping/2_extract_ted_notices.R
```

## Main outputs

After a full replication run, the most important files are in `data/clean/`.

Clean data:

```text
clean_winner_data_kfst.rds
clean_buyer_data_kfst.rds
clean_winner_data_ot.rds
clean_buyer_data_ot.rds
```

The OpenTender clean files now keep the bound source-file identifier
(`dataset`), a stable source-row identifier (`row_id`), the original tender
fields joined back onto the cleaned winner and buyer rows, and derived
tender-level variables including `tender_amount`, `lot_amount`, `bid_amount`,
their EUR/DKK counterparts (`tender_amount_eur`, `tender_amount_dkk`,
`lot_amount_eur`, `lot_amount_dkk`), `flag_awarded`,
`framework_start_anchor`, `framework_duration_days`,
`framework_end_date`, `annualised_tender_amount`, and
`annualised_lot_amount`. They also record when a missing OpenTender CVR was
recovered from formatting alone (`*_cvr_recovered_from_formatting` and
`flag_cvr_recovered_from_formatting`) or filled from another row with the same
firm name (`row_id_borrowed_from` and `flag_fill_missing_cvr`); on the buyer
side, the cleaning output also flags invalid multi-CVR tokens that were dropped
before name matching (`flag_non_cvr_identifier`).

If you add tender/lot-level variables like these after matched files already
exist, either re-run the full matching workflow or run
`code/processing/4_augment_matched_variables.R` after regenerating the clean
files. That utility updates the matched `*_name_matched.rds` files in place by
joining on stable row keys; it is not a substitute for re-running matching when
the cleaning logic changes names, CVRs, or row expansion.

Matched data:

```text
clean_winner_data_kfst_name_matched.rds
clean_buyer_data_kfst_name_matched.rds
clean_winner_data_ot_name_matched.rds
clean_buyer_data_ot_name_matched.rds
```

The KFST winner table (`clean_winner_data_kfst_name_matched.rds`) is now
**consortium-expanded** — one row per consortium member, tagged `semi_tier`,
`is_consortium`, and `consortium_number`. Every matched winner row (KFST and
OpenTender) also carries CVR-name **quality columns** — `cvr_name_match_quality`
(plus `_basic` / `_nospaces` / `_broad` variants), `cvr_name_match_quality_name`
(the registered name that scored best), and `cvr_name_is_substring` — plus a
provenance flag `flag_cvr_recovered_from_invalid` (the final CVR was recovered
because the listed candidate was not a valid registered CVR). See
[docs/cleaning_flags.md](docs/cleaning_flags.md) for full definitions.

Robustness stacks (one row-stacked file per source, **not** consumed downstream;
they exist for robustness comparison):

```text
kfst_winner_datasets_stacked.rds
ot_winner_datasets_stacked.rds
```

Each carries a `dataset` factor with three winner-CVR variants: `base` (the
canonical matched table), `extraction` (every standalone 8-digit CVR in the raw
field, no matching), and `name_only` (winner names matched with the field CVR
ignored).

Manual-review files:

```text
manual_name_review_kfst.rds
manual_buyer_name_review_kfst.rds
manual_name_review_ot.rds
manual_buyer_name_review_ot.rds
```

OpenTender name-partition diagnostics:

```text
winner_name_partition_diagnostics_ot.rds
buyer_name_partition_diagnostics_ot.rds
```

## Match quality and cleaning flags

The matched files include exact matches, fuzzy matches, ambiguous matches, and
rows that still require manual review. Do not treat every populated final CVR as
equally verified without checking the matching flags.

For match-quality statistics by source, entity, match type, and match step, see:

- [code/analysis/3_quality_analysis.Rmd](code/analysis/3_quality_analysis.Rmd)
- generated output:  `docs/3_quality_analysis.html`

For definitions of cleaning and matching flags, see:

- [docs/cleaning_flags.md](docs/cleaning_flags.md)

Those two documents are the best starting points for reviewers who want to know
which rows were cleaned directly, which rows were matched by name, and which
rows should be manually inspected before analysis.
