# Fold TED-recovered winners into the master OpenTender winner table.
# Author: Jack Mulqueeney

# Clean environment
rm(list = ls())

# Config: run from the project root or use run_replication.sh.
source("config.R")

suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
}))

# prepare_cvr_name(), extract_valid_cvr_candidates(), known_invalid_cvr_numbers()
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Reuse the TED recovery process WITHOUT running its standalone artifact write.
# Transitively sources 2_extract_ted_notices.R (SKIP_TED_RUN) for the cache-aware
# fetcher. Exposes recover_ted_missing_winners(winner_data).
SKIP_RECOVER_RUN <- TRUE
source(file.path(PROJECT_DIR, "code", "scraping", "3_recover_ted_missing_winners.R"))

winner_file <- file.path(dirs$clean_data, "clean_winner_data_ot.rds")
w <- readRDS(winner_file)

# Idempotency guard: a fresh 1_2 rebuilds the table without the recovery flag; if it
# is already present we have nothing to fold (run_replication runs 1_2 then 1_2b, so
# this only short-circuits a stray second 1_2b run).
if ("flag_winner_recovered_from_ted" %in% names(w)) {
  message("clean_winner_data_ot.rds already carries recovered TED winners - 1_2b is a no-op.")
  quit(save = "no", status = 0)
}

# NOTE - award verification. The TED parsers in code/scraping/3_ only emit a
# winner from a lot the notice CONFIRMS was awarded:
#   * legacy: an AWARD_CONTRACT block with a real contractor and no
#     NO_AWARDED_CONTRACT / PROCUREMENT_DISCONTINUED /
#     PROCUREMENT_UNSUCCESSFUL marker;
#   * eForms: LotResult winner-selection-status == "selec-w".
# Notices TED reports as not-awarded, or with no award section, contribute no
# winner and stay blank extraction failures. (Most no-winner PREAWARDED
# extraction failures are TED-confirmed non-awards - phantom frameworks.)

# 1. Recover from TED (cache-first); keep only the precise-tier winners.
recovery <- as_tibble(recover_ted_missing_winners(w))
rec <- recovery %>%
  filter(flag_winner_found_in_ted,
         lot_match_confidence %in% c("single_lot_notice", "lot_no_match")) %>%
  transmute(tender_id, lot_id, ted_winner_names, ted_winner_cvrs)

message(sprintf("Precise-tier lots with a TED winner: %d", nrow(rec)))

# ── Helpers ────────────────────────────────────────────────────────────────────

# Split a ';'-joined slot string, preserving trailing empties (strsplit drops them).
split_slots <- function(s) {
  s <- ifelse(is.na(s), "", s)
  n <- str_count(s, ";") + 1L
  parts <- strsplit(s, ";", fixed = TRUE)[[1]]
  length(parts) <- n
  parts[is.na(parts)] <- ""
  parts
}
# Pair name and CVR slots by index (equal length by construction; pad defensively).
pair_slots <- function(names_s, cvr_s) {
  a <- split_slots(names_s); b <- split_slots(cvr_s)
  n <- max(length(a), length(b))
  length(a) <- n; a[is.na(a)] <- ""
  length(b) <- n; b[is.na(b)] <- ""
  tibble(ted_name = a, ted_cvr = b)
}

# The lot's blank extraction-failure row (matches the scraper's target definition);
# exactly one such row per lot in the cleaned table.
is_extraction_failure <- function(df) {
  df$lot_status %in% c("AWARDED", "PREAWARDED") &
    (is.na(df$winner_name) | df$winner_name == "") &
    (is.na(df$winner_cvr_clean) | df$winner_cvr_clean == "" | !df$valid_cvr)
}

# Clean a TED CVR the same way the pipeline cleans OpenTender CVRs; reject placeholders.
ted_cvr_to_clean <- function(x) {
  cands <- extract_valid_cvr_candidates(x)
  cands <- cands[!is.na(cands)]
  if (!length(cands)) return(NA_character_)
  v <- cands[[1]]
  if (v %in% known_invalid_cvr_numbers()) return(NA_character_)
  v
}

# ── 2. Expand to one row per recovered winner (positionally aligned) ───────────
rec_long <- rec %>%
  mutate(pairs = map2(ted_winner_names, ted_winner_cvrs, pair_slots)) %>%
  select(tender_id, lot_id, pairs) %>%
  unnest(pairs) %>%
  filter(!(ted_name == "" & ted_cvr == ""))   # drop padding-only slots

# ── 3. Build schema-compatible recovered rows from the blank templates ─────────
recovered_lots <- rec_long %>% distinct(tender_id, lot_id)

templates <- w %>%
  semi_join(recovered_lots, by = c("tender_id", "lot_id")) %>%
  filter(is_extraction_failure(.)) %>%
  group_by(tender_id, lot_id) %>% slice(1) %>% ungroup()

recovered_rows <- rec_long %>%
  left_join(templates, by = c("tender_id", "lot_id")) %>%
  mutate(
    winner_name          = ted_name,
    winner_name_original = "",
    winner_cvr_original  = "",
    winner_cvr_candidate = ted_cvr,
    winner_cvr_clean     = map_chr(ted_cvr, ted_cvr_to_clean),
    valid_cvr            = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE),
    source               = "recovered from TED",
    flag_winner_recovered_from_ted = TRUE,
    # CVR came straight from the XML, not standardised out of an OpenTender value:
    # reset the CVR-standardisation provenance so it is not misattributed.
    winner_cvr_recovered_from_formatting = NA_character_,
    flag_cvr_recovered_from_formatting   = FALSE,
    flag_cvr_placeholder = FALSE,
    flag_cvr_ws = FALSE, flag_cvr_alphabet = FALSE, flag_cvr_punct = FALSE,
    flag_cvr_standardised = FALSE,
    winner_cvr_valid_from_same_name = NA_character_,
    row_id_borrowed_from = NA_character_,
    flag_fill_missing_cvr = FALSE
  )

# Prepared-name columns for matching (identical call to 1_2).
winner_name_prepared <- prepare_cvr_name(recovered_rows$winner_name)
recovered_rows <- recovered_rows %>%
  mutate(
    winner_name_basic        = winner_name_prepared$name_basic,
    winner_name_match        = winner_name_prepared$name_clean,
    winner_name_no_spaces    = winner_name_prepared$name_no_spaces,
    winner_name_broad        = winner_name_prepared$name_broad,
    winner_firm_type         = winner_name_prepared$firm_type,
    winner_name_first_letter = winner_name_prepared$first_letter
  )

# Recompute the winner missingness / review flags with 1_2's logic.
recovered_rows <- recovered_rows %>%
  mutate(
    flag_missing_winner_cvr  = coalesce(is.na(winner_cvr_clean) | winner_cvr_clean == "", FALSE),
    flag_missing_winner_name = coalesce(is.na(winner_name) | winner_name == "", FALSE),
    flag_missing_winner_country = coalesce(is.na(winner_country) | winner_country == "", FALSE),
    flag_foreign_winner = coalesce(
      !is.na(winner_country) & trimws(winner_country) != "" &
        toupper(trimws(winner_country)) != "DK", FALSE),
    flag_missing_cvr_with_name = coalesce(flag_missing_winner_cvr & !flag_missing_winner_name, FALSE),
    flag_review_cvr = coalesce(!flag_missing_winner_cvr & !valid_cvr, FALSE),
    flag_no_winner_info = coalesce(
      flag_missing_winner_cvr & flag_missing_winner_name & flag_missing_winner_country, FALSE),
    flag_verify_cvr_external = coalesce(case_when(
      flag_missing_cvr_with_name ~ TRUE,
      flag_review_cvr ~ TRUE,
      flag_no_winner_info ~ FALSE,
      valid_cvr ~ FALSE,
      TRUE ~ FALSE), FALSE),
    # CVR rows bypass the matcher; name-only rows enter the fuzzy match.
    flag_check_fuzzy_match = coalesce(winner_name != "" & is.na(winner_cvr_clean), FALSE)
  ) %>%
  # Re-number winners within the lot so the saved key stays unique after expansion.
  group_by(tender_id, lot_id) %>%
  mutate(winner_number = row_number()) %>%
  ungroup()

# ── 4. Replace the blank rows with the recovered rows ──────────────────────────
recovered_keys <- recovered_lots %>% transmute(k = paste(tender_id, lot_id)) %>% pull(k)

w_kept <- w %>%
  # Drop the single blank extraction-failure row for each recovered lot; keep
  # everything else (blank lots with no precise TED winner - incl. the dropped
  # notice-level tier - stay as unrecovered extraction failures).
  filter(!(paste(tender_id, lot_id) %in% recovered_keys & is_extraction_failure(.))) %>%
  mutate(flag_winner_recovered_from_ted = FALSE)

# Align recovered rows to the kept-table schema (drops the ted_name/ted_cvr helpers).
recovered_rows <- recovered_rows %>% select(any_of(names(w_kept)))

clean_winner_data <- bind_rows(w_kept, recovered_rows) %>%
  # Preserve 1_2's uniqueness guarantee on the saved key.
  distinct(tender_id, lot_id, winner_number, winner_cvr_clean, .keep_all = TRUE)

# ── 5. Overwrite the winner table + summary ────────────────────────────────────
saveRDS(clean_winner_data, winner_file)

n_cvr  <- sum(recovered_rows$valid_cvr)
n_name <- sum(!recovered_rows$valid_cvr & recovered_rows$winner_name != "")
message("\n── TED winner recovery folded in (precise tier only) ──")
message(sprintf("Blank lots replaced:         %d", nrow(recovered_lots)))
message(sprintf("Recovered winner rows added: %d  (CVR-resolved %d, name-only %d)",
                nrow(recovered_rows), n_cvr, n_name))
message(sprintf("Winner table: %d rows x %d cols -> %s",
                nrow(clean_winner_data), ncol(clean_winner_data), winner_file))
