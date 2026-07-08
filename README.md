# CVR Cleaning

This repository cleans Danish public procurement data and prepares CVR-number
matches for winners and buyers in the KFST and OpenTender data sources.

The README is a navigation and replication guide. For substantive data-quality
details, see:

- Download the [quality report HTML](https://github.com/jkmulq/cvr-cleaning/releases/download/quality-report/3_quality_analysis.html) (rendered from [this](code/3_quality_analysis.Rmd) Rmarkdown) 
- [Cleaning flag dictionary](docs/cleaning_flags.md)
- [Function manual](docs/functions_manual.md)

If you want to skip to replication, navigate to the [Configuration](#configuration)
and then [Replication](#replication) sections. Otherwise, there's a table of
contents below.

## Contents

- [Replication](#replication)
- [Required local inputs](#required-local-inputs)
- [Configuration](#configuration)
- [Repository structure](#repository-structure)
- [What each script does](#what-each-script-does)
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

For the standard replication workflow, no edits should be needed:
[run_replication.sh](run_replication.sh) moves to the repository root before it
runs any R scripts, and `config.R` sets `PROJECT_DIR` from that working
directory.

If you run an individual R script manually, first open `cvr-cleaning.Rproj` or
set your R working directory to the repository root. If you need to run from a
different working directory on another machine, edit the `PROJECT_DIR` line in
`config.R` to your local repository path.

The derived paths in `config.R` are:

```text
dirs$raw_data    -> data/raw/
dirs$cvr_key     -> data/cvr_matching_data/
dirs$clean_data  -> data/clean/
dirs$code        -> code/
```

`config.R` creates the expected local output directories if they are missing,
but it does not download or create the raw input files.

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
data/cvr_matching_data/cvr_names_full.csv
data/cvr_matching_data/cvr_binavne_full.csv
```

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

Expected run time depends on the machine, but the main distinction is between
cleaning and name matching:

| Stage | Scripts | Approximate run time |
|---|---|---|
| Input checks | built into `run_replication.sh` | seconds |
| Environment restore, if `RESTORE_RENV=true` | `renv::restore()` | depends on whether packages are already installed |
| Cleaning only | `code/1_1_process_kfst.R`, `code/1_2_process_open_tender.R` | minutes |
| CVR-name-key preparation | `code/1_3_process_keys.R` | minutes |
| Winner matching | `code/2_1_match_kfst.R`, `code/2_2_match_opentender.R` | slower than cleaning, but not usually the main bottleneck |
| Buyer matching | `code/2_2_match_kfst_buyers.R`, `code/2_3_match_opentender_buyers.R` | the main bottleneck; this is where most of the few-hour full-run time is spent |

For a quick check that the cleaning scripts still run, use `RUN_MATCHING=false`.
For a full matched dataset, plan for a few hours and expect most of that time to
come from the buyer-matching scripts.

### 4. Run cleaning only

To stop after the KFST and OpenTender cleaning scripts, without building the CVR
name keys or running name matching:

```bash
RUN_MATCHING=false ./run_replication.sh
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
