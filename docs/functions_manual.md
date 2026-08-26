# CVR Cleaning Function Manual

This note documents the helper functions in `code/functions.R`. It is meant to
be lighter than package documentation: each entry explains what the function is
for, how it works, what it returns, and where it appears in the pipeline.

Most functions are used by the staged scripts:

- `code/processing/0_build_cvr_lookup.R`
- `code/processing/1_1_process_kfst.R`
- `code/processing/1_2_process_open_tender.R`
- `code/processing/1_2b_recover_ted_winners.R`
- `code/processing/1_3_process_keys.R`
- `code/processing/2_1_match_kfst.R`
- `code/processing/2_2_match_kfst_buyers.R`
- `code/processing/2_3_match_opentender.R`
- `code/processing/2_4_match_opentender_buyers.R`

A few are also called by the scraping script
`code/scraping/employment_1_winners.R` and the analysis notebooks
`code/analysis/3_quality_analysis.Rmd` and `code/analysis/5_cvr_key_concordance.Rmd`.

Each function entry lists the scripts it is **Used in** (direct call sites).
"Internal" means the function is only invoked by another function in
`code/functions.R`, not called directly by a pipeline script. Script names are
under `code/processing/` unless another path is given. A consolidated
[Scripts By Function](#function-reference-scripts-by-function) table at the end
maps every function in `code/functions.R` to its call sites.

The examples assume the same packages used by the pipeline scripts are already
loaded, especially `tidyverse` and `data.table`, and that `code/functions.R`
has been sourced.

## CVR Extraction And Cleaning

### `extract_multiple_cvr()`

**Purpose:** Split a row that may contain multiple entity values into numbered
wide columns, so it can later be pivoted into one row per entity.

**Used for:** KFST winner rows where CVRs, names, or countries may contain more
than one winner.

**Used in:** `1_1_process_kfst.R`.

**How it works:**

- Takes one source row at a time.
- Standardizes commas and semicolons to semicolon delimiters.
- For the CVR column, treats a dot followed by a letter or number as a
  delimiter.
- Splits each requested entity column into numbered fields such as
  `winner_cvr_1`, `winner_cvr_2`, `winner_name_1`, and `winner_name_2`.
- Returns `max_detected`, the largest number of entities detected across the
  requested columns.
- Treats an empty split segment as `NA`, because a listed firm can be missing a
  CVR.

**Example:**

```r
example_row <- tibble::tibble(
  tender_id = "T1",
  lot_id = "L1",
  winner_cvr = "23456789; 87654321",
  winner_name = "Firm A; Firm B",
  winner_country = "DK; DK"
)

extract_multiple_cvr(
  data = example_row,
  row_id = 1,
  entity_cols = c("winner_cvr", "winner_name", "winner_country"),
  cvr_column = "winner_cvr"
)
```

**Example output:**

```text
  tender_id lot_id max_detected winner_cvr_1 winner_cvr_2 winner_name_1
1        T1     L1            2     23456789     87654321        Firm A
  winner_name_2 winner_country_1 winner_country_2
1        Firm B               DK               DK
```

The full function output also keeps the original unsplit entity fields. The
example above shows the main columns used by the later pivoting step.

### `clean_cvr_candidate()`

**Purpose:** Prepare a raw CVR string for more sophisticated pattern matching/distinct CVR extraction.

**Used for:** Low-level CVR extraction inside `extract_valid_cvr_candidates()`.

**Used in:** Internal — called only by `extract_valid_cvr_candidates()`; no
direct pipeline-script call sites.

**How it works:**

- Converts the input to character.
- Trims leading and trailing whitespace.
- Removes all internal whitespace.
- Converts empty strings to `NA`.

**Example:**

```r
clean_cvr_candidate(c(" DK 23 45 67 89 ", ""))
```

**Example output:**

```r
c("DK23456789", NA)
```

**Important assumption:** This only removes whitespace. It does not remove
letters or punctuation. That means labels such as `CVR:` remain visible to the
regular expression used later.

### `extract_valid_cvr_candidates()`

**Purpose:** Extract valid-looking CVRs from a candidate string.

**Used for:** OpenTender winner and buyer CVR cleaning, and helper functions
that count CVRs.

**Used in:** `1_2_process_open_tender.R`, `1_2b_recover_ted_winners.R`; also
called internally by `compute_distinct_valid_cvr()` (and, in `1_2b`, by the
recovered-TED CVR cleaner `ted_cvr_to_clean()`).

**How it works:**

- First calls `clean_cvr_candidate()` to strip whitespace.
- Uses the regex `(?<!\d)\d{8}(?!\d)` with `str_extract_all()` to pull out every
  run of exactly eight digits that is not touching another digit, returning all
  matches (one candidate string can legitimately contain more than one CVR).
- Returns `NA` if no eight-digit candidate is found.

**Character handling — what is removed vs. kept when extracting the CVR:**

- **Whitespace is removed.** `clean_cvr_candidate()` strips leading, trailing,
  *and internal* whitespace (`\s+`) before matching, so a spaced CVR such as
  `"DK55 77 52 14"` collapses to `"DK55775214"` and is read as one CVR.
- **Letters are NOT removed.** They stay in the string but act as non-digit
  boundaries, so prefixes/labels like `"DK"` or `"CVR:"` don't block the match —
  `"DK23456789"` and `"CVR: 23456789"` both yield `23456789`.
- **Punctuation is NOT removed.** Symbols such as `:` `-` `/` `.` are likewise
  treated as boundaries rather than stripped, so `"12345678-87654321"` yields
  *two* CVRs (`12345678` and `87654321`).
- **Digit-run length is enforced.** The lookarounds require exactly eight digits
  with a non-digit (or the string edge) on each side, so a seven- or nine-digit
  number such as `"111151609"` is ignored, and a 16-digit run is never split into
  two eight-digit CVRs.

In short: only whitespace is deleted; letters and punctuation are left in place
and simply serve as delimiters around the eight-digit CVR(s).

**Example:**

```r
extract_valid_cvr_candidates("CVR: 23456789")
```

**Example output:**

```r
"23456789"
```

```r

extract_valid_cvr_candidates("111151609")
```

**Example output:**

```r
NA_character_
```

**Important limitation:** Whitespace is removed before matching, so
`"12 34 56 78"` can be read as one CVR. In the OpenTender scripts, a separate
guard stops rows where two eight-digit CVRs are separated only by whitespace,
because those would be unsafe to collapse.

### `compute_distinct_valid_cvr()`

**Purpose:** Count how many distinct valid-looking CVRs appear in each input
string.

**Used for:** Deciding whether an OpenTender winner or buyer row should be
treated as a single-CVR row or a multiple-CVR row.

**Used in:** `1_2_process_open_tender.R`.

**How it works:**

- Calls `extract_valid_cvr_candidates()` on each value.
- Removes missing values.
- Counts unique eight-digit CVRs.

**Example:**

```r
compute_distinct_valid_cvr(c(
  "23456789",
  "23456789; 87654321",
  "CVR missing"
))
```

**Example output:**

```r
c(1, 2, 0)
```

### `known_invalid_cvr_numbers()`

**Purpose:** Function to call technically valid but likely a placeholder CVR value. 

**Used for:** Winner and buyer CVR cleaning in OpenTender. Called in multiple functions to make them cleaner.

**Used in:** `1_2_process_open_tender.R`, `1_2b_recover_ted_winners.R`; also
referenced internally (e.g., the `recover_formatted_danish_cvr()` default
argument).

**Current placeholders:**

```r
known_invalid_cvr_numbers()
```

**Example output:**

```r
c("00000000", "11111111", "12345678", "99999999")
```

**Why this matters:** These values match the eight-digit format but are not
trusted as real firm identifiers.

### `recover_formatted_danish_cvr()`

**Purpose:** Recover one clearly formatted Danish CVR when punctuation, spaces,
or prefixes prevented the conservative extractor from finding it.

**Used for:** OpenTender winner and buyer CVR cleaning.

**Used in:** `1_2_process_open_tender.R` (applied to both winner and buyer CVRs).

**How it works:**

- Removes all non-digits from the raw candidate.
- Only recovers a CVR when all of these are true:
  - the original candidate is not blank;
  - the country is `DK`;
  - the first-pass extractor found zero valid CVRs;
  - the digits collapse to exactly eight digits;
  - the result is not a known placeholder.

**Example:**

```r
recover_formatted_danish_cvr(
  cvr_candidate = "DK-23 45 67 89",
  country = "DK",
  n_valid_cvr_raw = 0
)
```

**Example output:**

```r
"23456789"
```


## Match Context Helpers

### `add_entity_context_to_matches()`

**Purpose:** Add source-row context to a table of name matches.

**Used for:** Exact and fuzzy match tables before matches are joined back to the
main winner or buyer data.

**Used in:** Internal — called only via its wrappers
`add_winner_context_to_matches()` and `add_buyer_context_to_matches()`; no direct
pipeline-script call sites.

**How it works:**

- Requires a temporary `match_row_id`.
- Pulls contextual fields such as tender ID, lot ID, entity number, entity name,
  prepared name, and firm type from the source rows.
- Adds those fields to the match table so manual review files can be inspected
  without rebuilding joins by hand.

**Example:**

```r
source_rows <- data.table::data.table(
  match_row_id = 1L,
  row_id = 10L,
  tender_id = "T1",
  lot_id = "L1",
  winner_number = 1L,
  winner_name_in_data = "Firm A",
  winner_name_basic = "firm a",
  winner_firm_type = "a/s"
)

matches <- data.table::data.table(
  match_row_id = 1L,
  cvr_name_match = "23456789"
)

matches_with_context <- add_entity_context_to_matches(
  matches = matches,
  source_rows = source_rows,
  entity = "winner"
)
```

**Example output:**

```text
  row_id match_row_id tender_id lot_id winner_number winner_name_in_data
1     10            1        T1     L1             1              Firm A
  winner_name_basic_in_data winner_firm_type_in_data cvr_name_match
1                    firm a                      a/s       23456789
```

### `add_winner_context_to_matches()`

**Purpose:** Winner-specific wrapper around `add_entity_context_to_matches()`.

**Used for:** KFST and OpenTender winner matching scripts.

**Used in:** `2_1_match_kfst.R`, `2_3_match_opentender.R`.

**Example:**

```r
new_matches <- add_winner_context_to_matches(new_matches)
```

By default, this looks for `remaining_original` in the parent environment.

### `add_buyer_context_to_matches()`

**Purpose:** Buyer-specific wrapper around `add_entity_context_to_matches()`.

**Used for:** KFST and OpenTender buyer matching scripts.

**Used in:** `2_2_match_kfst_buyers.R`, `2_4_match_opentender_buyers.R`.

**Example:**

```r
new_matches <- add_buyer_context_to_matches(new_matches)
```

By default, this looks for `remaining_original` in the parent environment.

## Name Preparation

### `cvr_firm_type_patterns()`

**Purpose:** Define legal-form spellings and map them to standardized firm-type
labels.

**Used for:** Name preparation and multiple-name partition detection.

**Used in:** Internal — called by `prepare_cvr_name()` and
`make_name_partitions()`; no direct pipeline-script call sites.

**How it works:**

- Stores regular expression patterns for legal forms such as `A/S`, `ApS`,
  `I/S`, `K/S`, `IVS`, and related spellings.
- Maps each pattern to a common firm-type value.

**Example:**

```r
cvr_firm_type_patterns()[c("aktieselskab", "aps", "i/s")]
```

**Example output:**

```r
c(aktieselskab = "a/s", aps = "aps", `i/s` = "i/s")
```

### `prepare_cvr_name()`

**Purpose:** Convert firm names into several standardized forms used for exact
and fuzzy matching.

**Used for:** Procurement winner and buyer names, main CVR registered names,
and alternative CVR names.

**Used in:** `1_1_process_kfst.R`, `1_2_process_open_tender.R`,
`1_2b_recover_ted_winners.R`, `1_3_process_keys.R`, `2_3_match_opentender.R`,
`2_4_match_opentender_buyers.R`, and `code/analysis/5_cvr_key_concordance.Rmd`.

**How it works:**

- Converts names to lowercase and trims whitespace.
- Standardizes Danish and accented letters.
- Detects legal form as a separate `firm_type` value, then removes it from the
  match name.
- Creates several name variants:
  - `name_basic`: lightly cleaned name used in the strictest exact match;
  - `name_clean`: normalized name used for fuzzy matching;
  - `name_no_spaces`: normalized name with spaces removed;
  - `name_broad`: common words removed and remaining words sorted;
  - `firm_type`: detected legal form;
  - `first_letter`: first letter of the cleaned name.

**Example:**

```r
prepare_cvr_name("Moller & Son A/S")
```

**Example output:**

```text
     name_original   name_basic    name_clean name_no_spaces name_broad
1 Moller & Son A/S moller & son moller og son    mollerogson  mollerson
  firm_type first_letter
1       a/s            m
```

**Why there are multiple versions:** The matching scripts use a ladder. They
try stricter prepared names first and only move to broader names if no earlier
match is found.

## Multiple-Firm Name Detection

### `make_name_partitions()`

**Purpose:** Generate possible ways to split a potentially multi-firm name into
separate firm names.

**Used for:** OpenTender winner and buyer matching before fuzzy matching.

**Used in:** `2_3_match_opentender.R`, `2_4_match_opentender_buyers.R`, and
`code/analysis/3_quality_analysis.Rmd`.

**How it works:**

- Flags collaboration language such as consortium, joint venture, and related
  Danish/English phrases.
- Removes collaboration labels from a temporary working name while preserving
  the original name for auditing.
- Masks legal-form text, so the slash in `A/S` is not mistaken for a separator
  between firms.
- Looks for plausible delimiters such as semicolons, slashes, plus signs,
  ampersands, and words like `og`, `and`, or `samt`.
- Generates all split/keep combinations across the detected delimiters.
- Returns a table of candidate partitions, where each partition contains one or
  more candidate firm-name segments.

**Example:**

```r
p <- make_name_partitions(
  "Firm A A/S og Firm B ApS",
  max_boundaries = 5L
)
```

**Example output:**

```text
$original_name
[1] "Firm A A/S og Firm B ApS"

$working_name
[1] "Firm A A/S og Firm B ApS"

$n_boundaries
[1] 1

$n_legal_forms
[1] 2

$flag_collaboration_text
[1] FALSE

  partition_id         partition_text segment_number segment_text
1            1 Firm A A/S; Firm B ApS              1   Firm A A/S
2            1 Firm A A/S; Firm B ApS              2   Firm B ApS
```

**Output structure:** A list containing:

- `original_name`
- `working_name`
- `n_boundaries`
- `n_legal_forms`
- `too_many_delimiters`
- collaboration flags
- `partitions`, a data table with `partition_id`, `partition_text`,
  `segment_number`, and `segment_text`

**Important assumption:** The matching scripts do not accept every generated
partition. OpenTender only accepts a partition when all segments exact-match to
unique CVRs. Rows with multiple complete partitions go to manual review.

## Matching Helpers

### `keep_valid_dates()`

**Purpose:** Filter CVR-name candidates to those whose registration dates are
compatible with the tender date.

**Used for:** Exact and fuzzy matching.

**Used in:** `1_3_process_keys.R` directly; also used indirectly by all four
matching scripts (`2_1_match_kfst.R`, `2_2_match_kfst_buyers.R`,
`2_3_match_opentender.R`, `2_4_match_opentender_buyers.R`) via
`select_preferred_exact_match()` and `find_fuzzy_matches()`.

**How it works:**

- Keeps candidates if `match_date` is missing.
- Otherwise, allows candidates where the tender date is within the CVR name's
  validity period, with a two-year buffer before `gyldigfra` and after
  `gyldigtil`.
- Treats missing start or end dates as open-ended.

**Example:**

```r
candidates <- data.table::data.table(
  match_row_id = c(1L, 2L, 3L),
  match_date = as.IDate(c("2020-01-01", "2020-01-01", "2020-01-01")),
  gyldigfra = as.IDate(c("2019-01-01", "2025-01-01", NA)),
  gyldigtil = as.IDate(c(NA, NA, "2019-01-01")),
  cvr = c("23456789", "34567891", "45678912")
)

keep_valid_dates(candidates)
```

**Example output:**

```text
  match_row_id match_date  gyldigfra  gyldigtil      cvr
1            1 2020-01-01 2019-01-01       <NA> 23456789
2            3 2020-01-01       <NA> 2019-01-01 45678912
```

### `select_preferred_exact_match()`

**Purpose:** Choose one preferred exact match when an exact join returns one or
more possible CVRs.

**Used for:** Exact matching steps in all winner and buyer matching scripts.

**Used in:** `2_1_match_kfst.R`, `2_2_match_kfst_buyers.R`,
`2_3_match_opentender.R`, `2_4_match_opentender_buyers.R`.

**How it works:**

- Calls `keep_valid_dates()` to remove date-incompatible candidates.
- Counts how many distinct CVRs were available for the source row.
- Orders candidates by:
  1. main registered name before alternative name;
  2. active on the tender date before not active;
  3. oldest registration date;
  4. CVR as a final deterministic tie-breaker.
- Keeps the first candidate for each source row.
- Returns the selected CVR plus match metadata.

**Example:**

```r
exact_candidates <- data.table::data.table(
  match_row_id = c(1L, 1L),
  match_date = as.IDate(c("2020-01-01", "2020-01-01")),
  gyldigfra = as.IDate(c("2018-01-01", "2018-01-01")),
  gyldigtil = as.IDate(c(NA, NA)),
  cvr = c("23456789", "87654321"),
  registered_name = c("Firm A A/S", "Firm A Trading A/S"),
  name_source = c("name", "biname"),
  source_order = c(1L, 2L)
)

select_preferred_exact_match(exact_candidates, step = 1L)
```

**Example output:**

```text
  match_row_id cvr_name_match registered_name_match name_match_source
1            1       23456789            Firm A A/S              name
  name_match_step name_match_method name_match_score name_match_n_candidates
1               1             exact              100                       2
```

**Manual-review implication:** If several CVRs were possible, the chosen match
is retained but `name_match_n_candidates` records that ambiguity.

### `levenshtein_ratio()`

**Purpose:** Calculate a similarity score from 0 to 100 between two strings.

**Used for:** Fuzzy matching.

**Used in:** `code/analysis/5_cvr_key_concordance.Rmd`; also called internally by
`find_fuzzy_matches()`, so it is used indirectly by all four matching scripts.

**How it works:**

- Uses base R's `adist()` to calculate Levenshtein distance.
- Converts distance into a percentage-like score:
  `100 * (total_length - distance) / total_length`.

**Example:**

```r
levenshtein_ratio("moller og son", c("moller og son", "different name"))
```

**Example output:**

```r
c(100, 59.3)
```

### `find_fuzzy_matches()`

**Purpose:** Find the top fuzzy CVR-name candidates for each remaining unmatched
entity.

**Used for:** Fuzzy matching in all winner and buyer matching scripts.

**Used in:** `2_1_match_kfst.R`, `2_2_match_kfst_buyers.R`,
`2_3_match_opentender.R`, `2_4_match_opentender_buyers.R`.

**How it works:**

- Requires `match_row_id`, `match_date`, a prepared entity-name column, and a
  firm-type column.
- Blocks candidate CVR names by firm type and first letter.
- Filters candidates by registration-date compatibility.
- Calculates Levenshtein similarity scores.
- Orders candidates by score and then by date/tie-break rules.
- Keeps one row per CVR.
- Returns the top five candidates per source row.
- Records how many CVRs share the highest score.

**Example:**

```r
rows <- data.table::data.table(
  match_row_id = 1L,
  match_date = as.IDate("2020-01-01"),
  winner_name_match = "firm a",
  winner_firm_type = "a/s"
)

name_key <- data.table::data.table(
  cvr = c("23456789", "87654321", "34567891"),
  registered_name = c("Firm A A/S", "Firm Alpha A/S", "Other ApS"),
  name_source = c("name", "name", "name"),
  name_match = c("firm a", "firm alpha", "other"),
  firm_type = c("a/s", "a/s", "aps"),
  first_letter = c("f", "f", "o"),
  gyldigfra = as.IDate(c("2018-01-01", "2018-01-01", "2018-01-01")),
  gyldigtil = as.IDate(c(NA, NA, NA))
)

step_candidates <- find_fuzzy_matches(
  rows = rows,
  key = name_key,
  entity_name_column = "winner_name_match",
  key_name_column = "name_match",
  first_letter_column = "first_letter",
  firm_type_column = "winner_firm_type",
  step = 5L
)
```

**Example output:**

```text
  match_row_id fuzzy_candidate_cvr fuzzy_candidate_name fuzzy_candidate_source
1            1            23456789           Firm A A/S                  name
2            1            87654321       Firm Alpha A/S                  name
  fuzzy_candidate_step fuzzy_candidate_score fuzzy_candidate_rank
1                    5                   100                    1
2                    5                    75                    2
  n_top_score_candidates
1                      1
2                      1
```

**Why it returns candidates rather than final matches:** The candidate table is
used both to accept strong matches and to populate manual-review files with the
next-best alternatives.

### `accept_fuzzy_match()`

**Purpose:** Decide which fuzzy candidates are strong enough to accept
automatically.

**Used for:** Fuzzy matching after each call to `find_fuzzy_matches()`.

**Used in:** `2_1_match_kfst.R`, `2_2_match_kfst_buyers.R`,
`2_3_match_opentender.R`, `2_4_match_opentender_buyers.R`.

**How it works:**

- Keeps only the first-ranked candidate.
- Requires the fuzzy score to exceed the relevant threshold.
- Requires the top score to be unique across CVRs.
- Returns accepted matches in the same compact format as exact matches.

**Example:**

```r
new_matches <- accept_fuzzy_match(
  candidates = step_candidates,
  threshold = 85
)
```

**Example output:**

```text
  match_row_id cvr_name_match registered_name_match name_match_source
1            1       23456789            Firm A A/S              name
  name_match_step name_match_method name_match_score name_match_n_candidates
1               5             fuzzy              100                       1
```


### `keep_step_matches()`

**Purpose:** Add newly accepted matches to the cumulative match table and remove
them from the remaining unmatched rows.

**Used for:** Exact and fuzzy matching scripts.

**Used in:** `2_1_match_kfst.R`, `2_2_match_kfst_buyers.R`,
`2_3_match_opentender.R`, `2_4_match_opentender_buyers.R`.

**How it works:**

- Appends `new_matches` to the global `matched` object.
- Removes those `match_row_id` values from the global `remaining` object.
- Returns invisibly.

**Example:**

```r
matched <- data.table::data.table()
remaining <- data.table::data.table(
  match_row_id = c(1L, 2L),
  label = c("matched row", "still remaining")
)
new_matches <- data.table::data.table(
  match_row_id = 1L,
  cvr_name_match = "23456789"
)

keep_step_matches(new_matches)
```

**Example output:**

After the function runs, `matched` contains the accepted match:

```text
  match_row_id cvr_name_match
1            1       23456789
```

and `remaining` no longer contains `match_row_id == 1`:

```text
  match_row_id           label
1            2 still remaining
```

**Important implementation detail:** This function uses `<<-`, so it modifies
`matched` and `remaining` in the calling script's environment. That mirrors the
original notebook workflow but means the matching scripts need those object
names to exist before calling it.

## CPV Classification

Map procurement CPV (Common Procurement Vocabulary) codes to coarse category and
sector labels for the cleaned tender data.

### `clean_cpv_code()`

**Purpose:** Normalize a raw CPV code and derive coarse category + sector labels.

**Used in:** `1_1_process_kfst.R`, `1_2_process_open_tender.R`.

**How it works:** Extracts the division (leading digits) from a raw CPV code,
then calls `cpv_division_to_sector()` and `cpv_division_to_category()`. Returns a
list of parallel vectors (mirrors `prepare_cvr_name()`) so the caller can bind
several derived columns at once.

### `cpv_division_to_category()`

**Purpose:** Map a CPV division to a coarse category — works, services, or
supplies (goods). Kept deliberately broad so each group is large enough for
regressions; returns `NA` for a missing division.

**Used in:** Internal — called only by `clean_cpv_code()`; no direct
pipeline-script call sites.

### `cpv_division_to_sector()`

**Purpose:** Map a CPV division to a sector label (aligned to CPV 2008
successors); divisions not listed fall through to manufactured goods/supplies.

**Used in:** Internal — called only by `clean_cpv_code()`; no direct
pipeline-script call sites.

## Virk CVR-Lookup Builder

Helpers that build (and spot-check) the CVR name/lifecycle lookup by querying the
Danish CVR register (Virk / Erhvervsstyrelsen distribution API). Documented more
lightly than the functions above — purpose and call sites only.

**Entry points (called from scripts):**

- `generate_cvr_lookup_from_virk()` — **Used in:** `0_build_cvr_lookup.R`. Pages
  through the Virk API to build the CVR → registered-name / binavne (alternative
  names) / lifecycle lookup that the matching stage joins against.
- `test_cvr_lookup_sample()` — **Used in:** `0_build_cvr_lookup.R`. Runs the
  lookup on a small sample of CVRs as a quick sanity check.

**Virk API helpers** — **Used in:** `code/scraping/employment_1_winners.R`
and internally by `generate_cvr_lookup_from_virk()`: `get_virk_credentials()`
(reads API credentials), `virk_post_json()` (POSTs a query, returns parsed JSON),
`virk_scalar()` (safely pulls a scalar field), `format_virk_cvr()` (normalizes a
CVR for the query).

**Internal Virk helpers** — no direct pipeline-script call sites; invoked within
`generate_cvr_lookup_from_virk()` / the Virk pipeline:
`load_virk_renviron_files()`, `extract_virk_period()`, `empty_virk_name_table()`,
`bind_virk_name_tables()`, `extract_virk_lifecycle_summary()`,
`extract_virk_main_names()`, `extract_virk_binavne()`, `append_virk_lookup_chunk()`,
`virk_lookup_source_fields()`, `virk_lookup_query_body()`.

## Function Groups By Pipeline Stage

| Pipeline stage | Functions |
|---|---|
| CVR extraction and standardization | `extract_multiple_cvr()`, `clean_cvr_candidate()`, `extract_valid_cvr_candidates()`, `compute_distinct_valid_cvr()`, `known_invalid_cvr_numbers()`, `recover_formatted_danish_cvr()` |
| Name preparation | `cvr_firm_type_patterns()`, `prepare_cvr_name()` |
| Multiple-firm detection | `make_name_partitions()` |
| Exact matching | `keep_valid_dates()`, `select_preferred_exact_match()`, `keep_step_matches()` |
| Fuzzy matching | `levenshtein_ratio()`, `find_fuzzy_matches()`, `accept_fuzzy_match()`, `keep_step_matches()` |
| Review context | `add_entity_context_to_matches()`, `add_winner_context_to_matches()`, `add_buyer_context_to_matches()` |
| CPV classification | `clean_cpv_code()`, `cpv_division_to_category()`, `cpv_division_to_sector()` |
| Virk CVR-lookup build | `generate_cvr_lookup_from_virk()`, `test_cvr_lookup_sample()`, `get_virk_credentials()`, `virk_post_json()`, `virk_scalar()`, `format_virk_cvr()`, + 10 internal Virk helpers |

## Function Reference: Scripts By Function

Direct call sites for every function in `code/functions.R`. "internal" means the
function is only invoked by another function in `code/functions.R`. Script codes:
`0` = `processing/0_build_cvr_lookup.R`; `1_1` = `1_1_process_kfst.R`;
`1_2` = `1_2_process_open_tender.R`; `1_2b` = `1_2b_recover_ted_winners.R`;
`1_3` = `1_3_process_keys.R`; `2_1`–`2_4` = the four matching scripts;
`scrape/1` = `scraping/employment_1_winners.R`;
`an/3` = `analysis/3_quality_analysis.Rmd`;
`an/5` = `analysis/5_cvr_key_concordance.Rmd`.

| Function | Used in (direct call sites) |
|---|---|
| `extract_multiple_cvr()` | `1_1` |
| `clean_cvr_candidate()` | internal (`extract_valid_cvr_candidates()`) |
| `extract_valid_cvr_candidates()` | `1_2`, `1_2b` (+ internal: `compute_distinct_valid_cvr()`) |
| `compute_distinct_valid_cvr()` | `1_2` |
| `known_invalid_cvr_numbers()` | `1_2`, `1_2b` (+ internal) |
| `recover_formatted_danish_cvr()` | `1_2` |
| `add_entity_context_to_matches()` | internal (winner/buyer wrappers) |
| `add_winner_context_to_matches()` | `2_1`, `2_3` |
| `add_buyer_context_to_matches()` | `2_2`, `2_4` |
| `cvr_firm_type_patterns()` | internal (`prepare_cvr_name()`, `make_name_partitions()`) |
| `prepare_cvr_name()` | `1_1`, `1_2`, `1_2b`, `1_3`, `2_3`, `2_4`, `an/5` |
| `make_name_partitions()` | `2_3`, `2_4`, `an/3` |
| `keep_valid_dates()` | `1_3` (+ indirect `2_1`–`2_4`) |
| `select_preferred_exact_match()` | `2_1`, `2_2`, `2_3`, `2_4` |
| `levenshtein_ratio()` | `an/5` (+ internal: `find_fuzzy_matches()`) |
| `find_fuzzy_matches()` | `2_1`, `2_2`, `2_3`, `2_4` |
| `accept_fuzzy_match()` | `2_1`, `2_2`, `2_3`, `2_4` |
| `keep_step_matches()` | `2_1`, `2_2`, `2_3`, `2_4` |
| `clean_cpv_code()` | `1_1`, `1_2` |
| `cpv_division_to_category()` | internal (`clean_cpv_code()`) |
| `cpv_division_to_sector()` | internal (`clean_cpv_code()`) |
| `generate_cvr_lookup_from_virk()` | `0` |
| `test_cvr_lookup_sample()` | `0` |
| `get_virk_credentials()` | `scrape/1` (+ internal) |
| `virk_post_json()` | `scrape/1` (+ internal) |
| `virk_scalar()` | `scrape/1` (+ internal) |
| `format_virk_cvr()` | `scrape/1` (+ internal) |
| `load_virk_renviron_files()` | internal |
| `extract_virk_period()` | internal |
| `empty_virk_name_table()` | internal |
| `bind_virk_name_tables()` | internal |
| `extract_virk_lifecycle_summary()` | internal |
| `extract_virk_main_names()` | internal |
| `extract_virk_binavne()` | internal |
| `append_virk_lookup_chunk()` | internal |
| `virk_lookup_source_fields()` | internal |
| `virk_lookup_query_body()` | internal |

## Design Principles

- Preserve original source values wherever possible.
- Repair formatting when the intended CVR is unambiguous.
- Do not automatically correct digit-level CVR typos.
- Prefer exact matches over fuzzy matches.
- Prefer main registered names over alternative names.
- Use date compatibility to avoid linking to names outside a plausible validity
  window.
- Keep ambiguous, fuzzy, or tied matches visible for manual review.
- Use shared winner/buyer-neutral helpers where the same logic applies to both
  entity types.
