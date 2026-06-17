# CVR Cleaning

This repository contains R code for cleaning Danish public procurement tender data, with a current focus on KFST winner and buyer fields. The active workflow reads the KFST workbook, standardizes selected variable names, separates single- and multi-winner records, expands multi-winner lots to one row per winner, creates buyer helper data, and adds diagnostic flags for common data-quality issues.

## Repository Structure

- `code/1_process_kfst.R` - Main KFST processing script.
- `config.R` - Project configuration for paths, Stata settings, and derived directories.
- `README.md` - Current setup and workflow documentation.
- `TODO.md` - Development roadmap and known remaining work.
- `cvr-cleaning.Rproj` - RStudio project file.
- `renv.lock` and `renv/` - Reproducible R environment metadata.
- `data/raw/kfst/` - Local raw KFST inputs expected by the script.
- `data/raw/OpenTender/` - Local raw OpenTender annual CSV files for future cleaning work.
- `data/clean/` - Local output directory created by `config.R`; no cleaned outputs are currently written there.
- `documents/` - Local project brief and supporting documentation.

The `.gitignore` ignores `data/*` and `documents/*`, so the raw data files, project brief, and future cleaned outputs are local working files rather than tracked repository contents.

## Data Inputs

The current script uses the KFST workbook:

```text
data/raw/kfst/udbudsdata_kfst.xlsx
```

It reads sheet `2.0 Udbudsdata`. The local KFST variable documentation is expected at:

```text
data/raw/kfst/variabelbeskrivelse-for-kfsts-udbudsdata-a.pdf
```

The project also currently has local OpenTender CSV files under `data/raw/OpenTender/` for 2006-2026 plus `data-dk-year-unavailable.csv`, but there is not yet a script that processes those files.

The local project brief is:

```text
documents/HowFirmsGrow - documentation_2026_06.pdf
```

## Setup

Open `cvr-cleaning.Rproj` in RStudio. The project uses `renv`; restore the recorded package environment before running the processing script:

```r
renv::restore()
```

The lockfile records R `4.5.1` and includes the packages used by the current script, including `here`, `haven`, `tidyverse`, and `readxl`.

Review `config.R` before running the workflow. It defines:

- `PROJECT_DIR`, resolved with `here::here()` when the RStudio project is open.
- `STATA_PATH` and `STATA_VERSION`, currently configured for StataNow/StataMP on macOS.
- `dirs$raw_data`, `dirs$clean_data`, and `dirs$code`.

If running outside RStudio, edit `PROJECT_DIR` in `config.R` so paths resolve to this repository.

## Running The KFST Workflow

From the project root, run:

```r
source("code/1_process_kfst.R")
```

The script currently:

1. Clears the R session and sources `config.R`.
2. Loads `data/raw/kfst/udbudsdata_kfst.xlsx`, sheet `2.0 Udbudsdata`.
3. Renames selected Danish fields to shorter English names.
4. Orders and arranges tender and lot identifiers.
5. Checks duplicate `lot_id` values and creates a duplicate flag if needed.
6. Separates likely single-winner records from records requiring multi-winner parsing.
7. Splits multi-winner CVR, name, and country fields into long format.
8. Compares extracted winner counts against the original `n_lot_winners` field.
9. Builds `clean_winner_data` with one row per winner-lot combination.
10. Builds `buyer_data_clean` with one row per listed buyer where possible.
11. Adds winner and buyer diagnostic flags.

The script prints diagnostics to the console. It currently creates R objects in memory only and does not write final files to `data/clean/`.

## Main Session Objects

- `data` - Renamed and arranged KFST source data with helper counts.
- `dup_lots` - Duplicate lot-id diagnostics.
- `single_data` - Lots with a likely single valid CVR.
- `multi_data` - Lots needing multi-winner parsing.
- `multi_data_sep` - Wide intermediate object after splitting multi-winner fields.
- `multi_long` - Parsed multi-winner records in long format.
- `clean_winner_data` - Combined winner-lot data with derived flags.
- `buyer_data` - Buyer source fields with original names and joint-tender flags.
- `single_buyer` - One-row buyer records for single-buyer lots or joint tenders with unlisted buyers.
- `multiple_buyer_long` - Long buyer records for lots with multiple listed buyers.
- `buyer_data_clean` - Combined buyer-level data with buyer-count and name-quality flags.

## Outputs

No final artifacts are currently written to disk. `config.R` creates `data/clean/` if it is missing, but `code/1_process_kfst.R` does not yet call `write_*()`, `saveRDS()`, or another export function.

Before sharing or reusing cleaned data outside the current R session, add an explicit export step for `clean_winner_data`, `buyer_data_clean`, and any diagnostics that should be preserved.

## Known Issues And Gaps

See `TODO.md` for the active roadmap. The main current gaps are:

- Original identifiers and names are not fully preserved in final winner outputs as separate original and cleaned columns.
- CVR values are identified and split in several cases, but there is not yet a general CVR standardization function that converts all unambiguous non-missing values to exactly eight digits.
- Manual-review flags do not yet distinguish every expected cleaning outcome, such as successful standardization, ambiguous values, and unresolved cases.
- Expanded winner and buyer rows do not yet have a stable original-observation identifier beyond tender and lot fields.
- `data/clean/` has no reproducible output files yet.
- Buyer names are split and flagged, but buyer entities are not linked to CVR numbers.
- Missing winner/bidder/buyer CVR values are not yet resolved through name matching.
- OpenTender raw files are present locally, but their schemas and CVR fields have not yet been processed.
- Job ad CVR cleaning is mentioned in the project brief, but no job ad data or scripts are present in this repository.

## Suggested Workflow

1. Restore the R environment with `renv::restore()`.
2. Confirm the local raw KFST and OpenTender files are present under `data/raw/`.
3. Review `config.R` for local path and Stata settings.
4. Run `source("code/1_process_kfst.R")`.
5. Inspect console diagnostics, `clean_winner_data`, and `buyer_data_clean`.
6. Address relevant TODO items before treating the cleaned data as final.
7. Add explicit exports to `data/clean/` once the cleaned output format is finalized.
