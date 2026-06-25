# CVR Cleaning

This repository cleans Danish public procurement data for use in firm-level
research. The main goal is to identify, where possible, valid Danish CVR numbers
for winning firms, bidders, and buyers while keeping the original source fields
available for audit and review.

There are two source-specific cleaning scripts:

- `code/1_process_kfst.R` cleans the KFST tender workbook.
- `code/2_process_open_tender.R` cleans annual OpenTender CSV exports.

The two sources have different structures and therefore require different
cleaning choices. KFST has a more documented tender/lot structure and buyer
names but no buyer CVRs. OpenTender has bidder and buyer identifier fields, but
the bidder identifier field contains a mix of valid CVRs, prefixes, spacing,
alternative IDs, and hand-reviewed delimiter cases.

## Repository Contents

- `README.md` - Project overview, setup, data inputs, cleaning logic, and current status.
- `TODO.md` - Development roadmap and remaining cleaning/matching tasks.
- `config.R` - Project root, Stata path, and derived directory settings.
- `code/functions.R` - Shared helpers for expanding multi-value fields and detecting valid CVR candidates.
- `code/1_process_kfst.R` - KFST cleaning workflow.
- `code/2_process_open_tender.R` - OpenTender schema check and bidder CVR cleanup workflow.
- `renv.lock` and `renv/` - Reproducible R environment files.
- `cvr-cleaning.Rproj` - RStudio project file.
- `data/raw/kfst/` - Local raw KFST input files.
- `data/raw/OpenTender/` - Local raw OpenTender annual CSV files.
- `data/clean/` - Local output directory created by `config.R`.

The `data/` tree is local project data and is not committed to the repository.

## Setup

Open `cvr-cleaning.Rproj` in RStudio. The project uses `renv`, so restore the
recorded package environment before running the processing scripts:

```r
renv::restore()
```

Review `config.R` before running the workflow. It defines `PROJECT_DIR`,
`STATA_PATH`, `STATA_VERSION`, and the derived `dirs` list:

- `dirs$raw_data` -> `data/raw`
- `dirs$clean_data` -> `data/clean`
- `dirs$code` -> `code`

If scripts are run outside RStudio, update `PROJECT_DIR` in `config.R` so paths
resolve to this repository.

## Running Scripts

From the project root, run:

```r
source("code/1_process_kfst.R")
source("code/2_process_open_tender.R")
```

Each script starts by clearing the R environment, sourcing `config.R`, and
loading required packages. The KFST script also writes cleaned output files. The
OpenTender script currently creates working objects in memory and is still being
developed before final export steps are added.

## KFST Cleaning

### Input

`code/1_process_kfst.R` reads:

```text
data/raw/kfst/udbudsdata_kfst.xlsx
```

It uses sheet `2.0 Udbudsdata`. The source includes tender identifiers, lot
identifiers, buyer names, winner names, winner CVR strings, winner country,
dates, bid counts, and the reported number of winners per lot.

### Main Logic

The KFST script currently:

1. Loads the workbook and renames selected Danish variables to consistent English names.
2. Orders the data by tender and lot identifiers.
3. Checks whether `lot_id` values are unique.
4. Handles duplicate `lot_id` values only when they follow the expected pattern of one cancelled row and one non-cancelled row.
5. Builds `tender_lot_data`, a tender/lot-level table used later for flags and joins.
6. Creates `winner_data` with winner CVR, winner name, winner country, and reported winner count.
7. Separates obvious single-CVR winner rows from rows that require multi-winner parsing.
8. Uses `extract_multiple_cvr()` to split multi-winner CVR/name/country fields into one row per winner.
9. Builds `clean_winner_data`.
10. Keeps original winner fields, including `winner_cvr_original`, next to cleaned fields such as `winner_cvr_clean`.
11. Standardizes winner CVRs by removing whitespace, hyphens, alphabetic characters, and punctuation.
12. Adds quality flags for valid CVRs, changed CVRs, missing winner fields, foreign winners, winner-count mismatches, single-bidder lots, multi-lot tenders, cancelled procurements, and rows that should be checked externally.
13. Splits KFST buyer names on semicolons where multiple buyers are listed.
14. Keeps joint tenders with unlisted buyers as one row, because the missing buyers cannot be recovered from the buyer-name field.
15. Builds `clean_buyer_data` with buyer-number and buyer-quality flags.

### KFST Particularities

- KFST buyer data contain buyer names but not buyer CVR numbers.
- Multiple KFST buyers are documented as semicolon-separated in the buyer-name field.
- Duplicate lot IDs are treated conservatively: unexpected duplicate patterns stop the script.
- KFST winner CVR cleaning now keeps both the original winner CVR string and the cleaned CVR candidate.

### KFST Outputs

The KFST script writes:

```text
data/clean/clean_winner_data_kfst.rds
data/clean/clean_buyer_data_kfst.rds
data/clean/clean_winner_data_kfst.dta
data/clean/clean_buyer_data_kfst.dta
```

Important in-session objects include:

- `data` - Renamed and arranged KFST data after duplicate-lot handling.
- `cancelled_duplicate_lots` - Cancelled rows dropped from duplicate lot pairs.
- `tender_lot_data` - Tender/lot context used for joins and flags.
- `single_winner_data`, `multi_winner_data`, and `multi_winner_long` - Winner parsing helper tables.
- `clean_winner_data` - Final KFST winner table.
- `single_buyer_data`, `multi_buyer_data`, and `multiple_buyer_long` - Buyer parsing helper tables.
- `clean_buyer_data` - Final KFST buyer table.

## OpenTender Cleaning

### Input

`code/2_process_open_tender.R` reads semicolon-separated annual CSV files from:

```text
data/raw/OpenTender/
```

The script currently reads all files present in this folder. This means the
replication sample is determined by the local OpenTender files supplied under
`data/raw/OpenTender/`, not by an internal year filter in the script.

### Main Logic

The OpenTender script currently:

1. Lists raw OpenTender CSV files.
2. Reads column names from every listed file.
3. Checks all pairwise file combinations with `setequal()` and stops if column schemas differ.
4. Reads the CSV files with `data.table::fread(..., colClasses = "character")` so identifier-like fields are not altered by automatic numeric type guessing.
5. Row-binds the annual files into one table and records the source file in `dataset`.
6. Adds `row_id`, a stable row reference within the combined OpenTender data.
7. Creates `winner_data_original` from bidder identifier, bidder name, and bidder country fields.
8. Creates `buyer_data_original` from buyer identifier, buyer name, and buyer country fields.
9. Investigates delimiter patterns in `bidder_bodyIds`.
10. Flags delimiter types such as commas, semicolons, periods, pipes, slashes, spaces, hyphens, ampersands, colons, and the Danish word `og`.
11. Treats commas and the one pipe case as valid delimiters.
12. Uses manually reviewed `row_id` lists for valid slash, ampersand, and `og` delimiters.
13. Flags non-reviewed slash, ampersand, and `og` cases for manual review.
14. Stops if manually reviewed row IDs are missing or no longer contain the expected delimiter, which helps catch row-order drift if the file list changes.
15. Converts accepted delimiters to semicolons.
16. Flags likely multi-winner rows based on accepted delimiters.
17. Separately flags rows with more than one distinct valid eight-digit CVR.
18. Uses a manually reviewed row-ID list to confirm the small subset where multiple distinct CVRs correspond to multiple winning firms rather than repeated or erroneous identifiers for one firm.
19. Splits confirmed multi-firm rows into one row per winner after hand-coding the corresponding winner-name delimiters.
20. Splits the remaining multi-CVR or delimited rows separately, cleans CVR strings by removing spaces, country prefixes, punctuation, and letters, and flags valid eight-digit CVRs.
21. For firm names that appear with one valid CVR and one or more invalid CVR entries, assumes the single valid CVR is the true CVR, collapses duplicate rows created by that assumption, and records this with `flag_assumed_single_valid_cvr`.
22. Flags firm names that appear with multiple valid CVRs using `flag_multi_valid_cvr`, because those cases need additional review rather than automatic overwrite.

### OpenTender Particularities

- OpenTender bidder IDs often contain several identifier formats for the same bidder, such as `DK` prefixes, spaced CVRs, repeated CVRs, and non-CVR IDs.
- Some delimiters are genuine separators between multiple winning firms, while others are part of names or identifier text.
- The script intentionally separates "valid delimiter" flags from "manual review" flags so hand-reviewed cases remain auditable.
- The script also separates "multiple distinct valid CVRs" from "multiple winning firms." This matters because some OpenTender rows list several CVR-like values for a single bidder name, while only a small manually confirmed set represents true multi-firm winning groups.
- For confirmed multi-firm rows, the script manually standardizes a small number of winner-name strings so CVRs and names can be expanded together. Consortium labels are replaced with the firm names corresponding to the CVRs, with the script noting virk.dk as the reference source for those cases.
- For repeated or partly invalid CVR strings attached to one firm name, the script uses a conservative overwrite rule: only firms with exactly one valid CVR and more than one CVR entry get their invalid entries replaced, and the change is flagged.
- The current OpenTender workflow is focused on bidder/winner CVR cleanup. Buyer identifier cleaning has not yet been implemented beyond creating `buyer_data_original`.
- Because manually reviewed cases are stored as `row_id` lists, the script includes drift checks to prevent those row IDs from silently pointing to the wrong records.

### OpenTender Outputs

The OpenTender script currently creates objects in the R session but does not yet
write final files to `data/clean/`.

Important in-session objects include:

- `data_col_names` - Column names by OpenTender source file.
- `col_name_diffs` - Pairwise schema concordance checks.
- `data` - Combined OpenTender data for all listed files in `data/raw/OpenTender/`.
- `winner_data_original` - Original bidder fields used for winner CVR cleaning.
- `buyer_data_original` - Original buyer fields retained for later buyer CVR cleaning.
- `winner_data` - Winner/bidder working table with delimiter and manual-review flags.
- `multi_winner_names_data_long` - Manually confirmed multi-firm winner rows split to one row per winner name and CVR.
- `multi_cvr_nondistinct_names_data_long` - Delimited or multi-CVR rows that do not represent manually confirmed multi-firm winner-name cases, with CVR cleanup, overwrite flags, and multi-valid-CVR flags.

## Cleaning And Review Flags

Across both sources, the scripts keep original source values beside cleaned
values. This is important because rows sent for review should still show the raw
string that produced the cleaned value.

The flagging convention is deliberately audit-oriented:

- boolean flags use `TRUE` / `FALSE`, with explicit missingness flags where
  missing source values matter;
- flags are not mutually exclusive, so a row can be valid, foreign, multi-lot,
  or review-relevant in overlapping ways;
- delimiter and manual-review flags document how source strings were split;
- CVR quality flags document syntactic standardisation, missingness, validity,
  and cases that may need external verification.

The detailed flag dictionary is in
[`docs/cleaning_flags.md`](docs/cleaning_flags.md). The scripts clean CVR
strings syntactically and flag rows for review. They do not yet match missing
CVRs or ambiguous names against virk.dk or another external CVR register.

## Current Status And Remaining Work

- KFST winner and buyer cleaning writes `.rds` and `.dta` files to `data/clean/`.
- OpenTender bidder/winner delimiter handling, true multi-winner review, and CVR cleanup are in progress.
- OpenTender buyer CVR cleaning is not yet implemented.
- OpenTender final cleaned outputs are not yet written to disk.
- External CVR/name matching is not yet implemented.
- Missing-CVR name matching and ambiguity flags remain open development tasks.
- The repository does not currently include job-ad CVR cleaning scripts.

See `TODO.md` for the active development roadmap and progress tracker.

## Suggested Review Workflow

1. Restore the R environment with `renv::restore()`.
2. Confirm raw files are present under `data/raw/kfst/` and `data/raw/OpenTender/`.
3. Review and update `config.R` for the local machine.
4. Run `code/1_process_kfst.R`.
5. Inspect the KFST diagnostics and saved files in `data/clean/`.
6. Run `code/2_process_open_tender.R`.
7. Inspect OpenTender delimiter summaries, manual-review flags, confirmed multi-winner rows, overwrite flags, and multi-valid-CVR flags.
8. Add OpenTender buyer cleaning and export steps once the cleaned output format is finalized.
