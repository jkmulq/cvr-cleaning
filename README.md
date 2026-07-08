# CVR Cleaning

This repository cleans Danish public procurement data and prepares CVR-number
matches for winners and buyers in the KFST and OpenTender data sources.

The README is a navigation and replication guide. For substantive data-quality
details, see:

- [Download the quality report HTML](https://github.com/jkmulq/cvr-cleaning/releases/download/quality-report/3_quality_analysis.html) (rendered from [this](code/3_quality_analysis.Rmd) Rmarkdown) 
- [Cleaning flag dictionary](docs/cleaning_flags.md)

## Repository structure

```text
cvr-cleaning/
├── README.md
├── TODO.md
├── config.R
├── run_replication.sh
├── code/
│   ├── functions.R
│   ├── 1_1_process_kfst.R
│   ├── 1_2_process_open_tender.R
│   ├── 1_3_process_keys.R
│   ├── 2_1_match_kfst.R
│   ├── 2_2_match_kfst_buyers.R
│   ├── 2_2_match_opentender.R
│   ├── 2_3_match_opentender_buyers.R
│   ├── 3_quality_analysis.Rmd
│   └── drafts/
├── docs/
│   ├── cleaning_flags.md
│   └── 3_quality_analysis.html
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
lookup keys. Scripts beginning with `2_` perform name matching. The quality
report is generated separately.

| Script | Purpose | Main outputs |
|---|---|---|
| [code/functions.R](code/functions.R) | Shared helper functions for CVR extraction, CVR formatting, name preparation, name partitioning, and matching support. | No direct output. |
| [code/1_1_process_kfst.R](code/1_1_process_kfst.R) | Cleans KFST winner and buyer data. Splits multi-winner and multi-buyer rows where possible, standardises winner CVRs, creates matching-ready name fields, and saves clean KFST objects. | `clean_winner_data_kfst.rds`, `clean_buyer_data_kfst.rds`, plus `.dta` versions. |
| [code/1_2_process_open_tender.R](code/1_2_process_open_tender.R) | Reads annual OpenTender files, checks schema consistency, cleans winner and buyer CVR fields, handles multi-CVR rows, creates audit identifiers, prepares matching-ready names, and saves clean OpenTender objects. | `clean_winner_data_ot.rds`, `clean_buyer_data_ot.rds`. |
| [code/1_3_process_keys.R](code/1_3_process_keys.R) | Cleans the CVR register name keys used for later matching. It prepares both official names and alternative names. | `clean_cvr_name_key.rds`, `clean_cvr_biname_key.rds`. |
| [code/2_1_match_kfst.R](code/2_1_match_kfst.R) | Matches missing KFST winner CVRs against the prepared CVR-name keys. | `clean_winner_data_kfst_name_matched.rds`, `manual_name_review_kfst.rds`. |
| [code/2_2_match_kfst_buyers.R](code/2_2_match_kfst_buyers.R) | Matches KFST buyer names to CVRs, since KFST buyer CVRs are not supplied in the raw source. | `clean_buyer_data_kfst_name_matched.rds`, `manual_buyer_name_review_kfst.rds`. |
| [code/2_2_match_opentender.R](code/2_2_match_opentender.R) | Matches missing OpenTender winner CVRs and records ambiguous or fuzzy cases for review. Also writes winner-name partition diagnostics. | `clean_winner_data_ot_name_matched.rds`, `manual_name_review_ot.rds`, `winner_name_partition_diagnostics_ot.rds`. |
| [code/2_3_match_opentender_buyers.R](code/2_3_match_opentender_buyers.R) | Matches missing OpenTender buyer CVRs and records ambiguous or fuzzy cases for review. Also writes buyer-name partition diagnostics. | `clean_buyer_data_ot_name_matched.rds`, `manual_buyer_name_review_ot.rds`, `buyer_name_partition_diagnostics_ot.rds`. |
| [code/3_quality_analysis.Rmd](code/3_quality_analysis.Rmd) | Builds the match-quality and data-quality report from the cleaned and matched outputs. | `docs/3_quality_analysis.html`. |

The [code/drafts/](code/drafts) folder contains experimental or benchmark
scripts. These are useful for development, but they are not part of the default
replication workflow.

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
- `data/cvr_matching_data/cvr_names_full.csv`
- `data/cvr_matching_data/cvr_binavne_full.csv`

The OpenTender script reads all files present in `data/raw/OpenTender/`. The
replication sample is therefore determined by the files placed in that folder.

## Configuration

All scripts source [config.R](config.R). The main setting is `PROJECT_DIR`, the
root of this repository.

If you run scripts from the repository root, no changes should be needed. If you
run from another location or machine, either edit `config.R` or pass
`CVR_CLEANING_PROJECT_DIR` as an environment variable:

```bash
CVR_CLEANING_PROJECT_DIR="/path/to/cvr-cleaning" ./run_replication.sh
```

`config.R` also creates the expected local directories if they are missing.

## Replication

### 1. Restore the R environment

The project uses `renv`. On a new machine, restore the package environment once:

```bash
Rscript --vanilla -e 'renv::restore(prompt = FALSE)'
```

Alternatively, let the replication script do this:

```bash
RESTORE_RENV=true ./run_replication.sh
```

### 2. Add local input data

Place the KFST, OpenTender, and CVR-name-key files in the folders listed above.
The raw data are local inputs and are not committed to this repository.

### 3. Run the full workflow

From the repository root:

```bash
./run_replication.sh
```

The script runs:

```text
code/1_1_process_kfst.R
code/1_2_process_open_tender.R
code/1_3_process_keys.R
code/2_1_match_kfst.R
code/2_2_match_kfst_buyers.R
code/2_2_match_opentender.R
code/2_3_match_opentender_buyers.R
```

Outputs are written to `data/clean/`.

### 4. Run cleaning only

To stop after the KFST and OpenTender cleaning scripts, without building the CVR
name keys or running name matching:

```bash
RUN_MATCHING=false ./run_replication.sh
```

### 5. Use a different R executable

If needed, set `RSCRIPT`:

```bash
RSCRIPT="/path/to/Rscript" ./run_replication.sh
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

Matched data:

```text
clean_winner_data_kfst_name_matched.rds
clean_buyer_data_kfst_name_matched.rds
clean_winner_data_ot_name_matched.rds
clean_buyer_data_ot_name_matched.rds
```

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

- [code/3_quality_analysis.Rmd](code/3_quality_analysis.Rmd)
- generated output:  `docs/3_quality_analysis.html`

For definitions of cleaning and matching flags, see:

- [docs/cleaning_flags.md](docs/cleaning_flags.md)

Those two documents are the best starting points for reviewers who want to know
which rows were cleaned directly, which rows were matched by name, and which
rows should be manually inspected before analysis.
