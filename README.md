# CVR Cleaning

This project cleans Danish public procurement tender data with a focus on CVR numbers for winning firms. The current workflow reads KFST tender data, standardizes selected variable names, separates single- and multi-winner records, expands multi-winner lots into one row per winner, and creates diagnostic flags for common data-quality issues.

## Repository Contents

- `code/1_process_kfst.R` - Main R script for processing KFST tender data.
- `config.R` - Project-level configuration for paths and Stata settings.
- `renv.lock` and `renv/` - Reproducible R environment files.
- `data/raw/kfst/udbudsdata_kfst.xlsx` - Raw KFST workbook expected by the processing script.
- `data/raw/kfst/variabelbeskrivelse-for-kfsts-udbudsdata-a.pdf` - KFST variable description and data documentation.
- `data/raw/OpenTender/` - Raw OpenTender annual CSV files.

## Data Sources

The main script currently uses the KFST workbook:

```text
data/raw/kfst/udbudsdata_kfst.xlsx
```

It reads sheet `2.0 Udbudsdata`. The accompanying KFST documentation describes the tender dataset, including identifiers, buyer fields, contract dates and values, bid counts, winner names, winner CVR numbers, winner country, and the number of winners per lot.

## Setup

Open `cvr-cleaning.Rproj` in RStudio. The project uses `renv`, so restore the recorded package environment before running the processing script:

```r
renv::restore()
```

The lockfile records R `4.5.1` and includes the packages used by the current script, including `here`, `haven`, `tidyverse`, and `readxl`.

Review `config.R` before running the workflow. The file defines the project root, Stata executable path, and derived project directories. If you run scripts outside RStudio, set `PROJECT_DIR` manually in `config.R`.

## Running The KFST Cleaning Script

From the project root, run:

```r
source("code/1_process_kfst.R")
```

The script currently:

1. Loads the KFST workbook from the raw data directory.
2. Renames selected Danish variable names to shorter English names.
3. Orders and arranges tender and lot identifiers.
4. Checks whether `lot_id` values are duplicated.
5. Separates likely single-winner records from records requiring multi-winner parsing.
6. Splits multi-winner CVR, name, and country fields into long format.
7. Compares extracted winner counts against the original `n_lot_winners` field.
8. Builds `clean_data` with one row per winner-lot combination.
9. Creates buyer helper tables and data-quality flags.

## Main Output Objects

The script creates objects in the R session rather than writing final files to disk. Important objects include:

- `data` - Renamed and arranged raw KFST data.
- `single_data` - Lots with a likely single valid CVR.
- `multi_data` - Lots needing multi-winner parsing.
- `multi_long` - Parsed multi-winner records in long format.
- `clean_data` - Combined winner-lot data with derived flags.
- `buyer_data`, `single_buyer`, `multiple_buyer`, `multiple_buyer_sep` - Buyer-focused helper tables. `multiple_buyer_sep` keeps buyer names in wide columns after splitting semicolon-separated buyers.

## Development Notes

This README reflects the current project state. Recent updates fixed the direct name mismatches for the raw data directory, bid-count field, and cancelled-tender field. Before treating the script as a fully reproducible end-to-end pipeline, check the following remaining items:

- `clean_data` is built from a narrow set of winner columns. Any later flags that use original tender-level fields, such as `n_bids_received` and `tender_cancelled`, need those fields joined or selected into `clean_data` before the flags are created.
- `clean_data` is built in memory only; add an explicit write step if a committed or shared cleaned dataset is needed.

See `TODO.md` for the active development roadmap and progress tracker.

## Suggested Project Workflow

1. Restore the R environment with `renv::restore()`.
2. Confirm raw data files are present under `data/raw/`.
3. Review and update `config.R` for the local machine.
4. Run `code/1_process_kfst.R`.
5. Inspect printed diagnostics and the final `clean_data` object.
6. Add an export step once the cleaned output format is finalized.
