# Cleans KFST provided tender data
# Author: Jack Mulqueeney
# Date: 16 June 2026

# Clean environment
rm(list = ls())

# Config: run from the project root or use run_replication.sh.
source("config.R")

# Packages
suppressWarnings(suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(readxl)
}))

# Paths
raw_data_dir <- dirs$raw_data
raw_data_name <- "udbudsdata_kfst.xlsx"

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# 1 Load data
data <- read_excel(file.path(raw_data_dir, "kfst", raw_data_name), sheet = "2.0 Udbudsdata")

# Rename
data <- data %>% 
  rename(winner_cvr = `Vinders CVR`,
         winner_name = `Vinders navn`,
         winner_country = `Vinders land`,
         buyer_name = `Navn på ordregiver`,
         pub_date = `Publikationsdato for bekendtgørelse om indgået kontrakt`,
         award_date = `Dato for tildeling af kontrakten`,
         award_url = `Link til bekendtgørelse om indgået kontrakt`,
         submit_date = `Frist for aflevering af tilbud`,
         tender_id = `Løbenummer`,
         lot_id = `Nummerplade`,
         contract_type = `Rammeaftale`,
         divided_tender = `Opdelt udbud`,
         joint_tender = `Fælles-/enkeltudbud`,
         consortium_winner = `Konsortium/Sammenslutning`,
         cpv_code = `CPV-koder`,
         tender_cancelled = `Annulleret udbud`,
         tender_status = `Helt/delvist gennemført/annulleret`,
         estimated_tender_amount = `Estimat af samlet kontraktværdi - angivet i udbud`,
         estimated_lot_amount = `Estimat af delkontrakts kontraktværdi`,
         final_tender_amount = `Endelig kontraktværdi`,
         final_lot_amount = `Endelig værdi af delaftaler`,
         lot_number = `Delkontraktnr.`,
         n_lots = `Antal delkontrakter kortlagt`,
         n_lots_contracted = `Antal delkontrakter i udbudsbekendtgørelsen`,
         n_lot_winners = `Antal vindere på delkontrakten`,
         n_bids_received = `Antal modtagne bud`,
         contract_duration_months_min = `Varighed af kontrakten i måneder (min)`,
         contract_duration_months_max = `Varighed af kontrakten i måneder (max)`)

# Recode the joint/single-tender indicator to English on the source data up
# front, so the value is consistent everywhere it propagates (tender/lot data,
# and the winner and buyer tables through the joins below).
data <- data %>%
  mutate(
    joint_tender = case_when(
      joint_tender == "Enkelt" ~ "single",
      joint_tender == "Fælles" ~ "joint",
      TRUE ~ NA_character_
    )
  )

# Standardise tender-level fields before they are joined onto buyer/winner rows.
## Tender/lot amount. 
data <- data %>%
  mutate(
    tender_amount = coalesce(
      as.numeric(final_tender_amount),
      as.numeric(estimated_tender_amount)
    ),
    lot_amount = coalesce(
      as.numeric(final_lot_amount),
      as.numeric(estimated_lot_amount)
    )
  )

## Fill missing amounts
# Equally split tender over all lots, if all lot amounts are unavailable
data <- data %>%
  mutate(lot_amount_orig = lot_amount,
         flag_all_orig_lot_amt_missing = all(is.na(lot_amount_orig)),
         lot_amount = ifelse(flag_all_orig_lot_amt_missing,
                             tender_amount / n_distinct(lot_id),
                             lot_amount_orig),
         .by = tender_id)

## Add the EUR counterpart at Denmark's fixed ERM II central rate (7.46038 DKK
## per EUR, +/-2.25% band). Derived after the amount fill so imputed values are
## included, and on the tender/lot-level data so the EUR/DKK columns propagate to
## the winner and buyer tables through the joins below.
# KFST amounts are in DKK.
dkk_per_eur <- 7.46038
data <- data %>%
  mutate(
    tender_amount_dkk = tender_amount,
    lot_amount_dkk    = lot_amount,
    tender_amount_eur = tender_amount / dkk_per_eur,
    lot_amount_eur    = lot_amount    / dkk_per_eur
  )

## Number of bidders
data <- data %>%
  mutate(n_bidders = as.numeric(n_bids_received))

## Award date
data <- data %>%
  mutate(
    award_date = coalesce(
      as.Date(
        if_else(
          str_detect(as.character(award_date), "^[0-9]+$"),
          suppressWarnings(as.numeric(award_date)),
          NA_real_
        ),
        origin = "1899-12-30"
      ),
      lubridate::ymd(as.character(award_date), quiet = TRUE)
    )
  )

## Framework agreement
data <- data %>%
  mutate(contract_type = case_when(
    contract_type == "Offentlig kontrakt" ~ "Public contract",
    contract_type == "Rammeaftale" ~ "Framework agreement"))

## Tender awarded
# KFST rows are at the lot (delkontrakt) level, and `tender_cancelled`
# (variable 43, "Annulleret udbud") records annulment at that level: "Nej" = the
# lot was carried out (awarded), "Ja" = annulled. This is the right granularity
# for "keep awarded lots only" — it drops the annulled lots inside partially-
# completed tenders that the tender-level status (variable 44, `tender_status`)
# would keep. Mirrors the OpenTender flag_awarded; default FALSE if status is
# missing.
data <- data %>%
  mutate(flag_awarded = coalesce(tender_cancelled == "Nej", FALSE))

## Award end date
# KFST has no contract end date, only the award (start) date and the contract
# duration in months. Per the documentation, "min" (variable 46) is the base
# contract length excluding options and is the more reliable/complete field,
# while "max" (variable 47) includes extension options; prefer min, fall back to
# max. Months are approximated as 30 days. Populated for awarded lots only.
data <- data %>%
  mutate(award_end_date = if_else(
    flag_awarded,
    award_date + coalesce(
      as.numeric(contract_duration_months_min),
      as.numeric(contract_duration_months_max)
    ) * 30,
    as.Date(NA)
  ))

## Annualised framework amounts
# A framework agreement's amount covers its whole (multi-year) duration, so the
# headline total is not comparable to a single-year contract. Annualise it:
# amount per month (amount / duration in months) scaled to 12 months. KFST
# records duration in months, so annualising by month avoids any day
# approximation. Uses the base ("min") duration, falling back to "max", matching
# award_end_date. Framework agreements only, and only where the amount and a
# positive duration are both present (the > 0 guard avoids divide-by-zero).
data <- data %>%
  mutate(
    contract_duration_months = coalesce(
      as.numeric(contract_duration_months_min),
      as.numeric(contract_duration_months_max)
    ),
    annualised_tender_amount = if_else(
      contract_type == "Framework agreement" &
        !is.na(contract_duration_months) & contract_duration_months > 0,
      tender_amount / contract_duration_months * 12,
      NA_real_
    ),
    annualised_lot_amount = if_else(
      contract_type == "Framework agreement" &
        !is.na(contract_duration_months) & contract_duration_months > 0,
      lot_amount / contract_duration_months * 12,
      NA_real_
    )
  )

## CPV code
## Tenders can list several CPV codes; as a first pass keep the first listed
## code and map it to its EU CPV division (the broadest interpretable grouping).
cpv_prepared <- clean_cpv_code(data$cpv_code)
data <- data %>%
  mutate(
    cpv_code_first = cpv_prepared$code_first,
    cpv_division = cpv_prepared$division,
    cpv_division_name = cpv_prepared$division_name,
    # Coarser groupings for treatment-effect heterogeneity (large enough cells).
    cpv_sector = cpv_prepared$sector,
    cpv_category = cpv_prepared$category
  )

# Order columns nicely
data <- data %>% 
  select(tender_id, lot_id, lot_number,
         n_lots, n_lots_contracted, n_lot_winners,
         pub_date, award_date,
         buyer_name,
         winner_name, winner_cvr, winner_country,
         everything())

# Arrange
data <- data %>% 
  group_by(tender_id) %>% 
  arrange(lot_number, .by_group = TRUE) %>% 
  ungroup()

# Check whether lot_id is unique
data <- data %>% 
  mutate(n_lot_id = n(), .by = lot_id)

dup_lots <- data %>%
  filter(n_lot_id > 1) %>%
  distinct(lot_id, n_lot_id, tender_cancelled, tender_status) %>%
  arrange(lot_id, tender_cancelled)

cancelled_duplicate_lots <- data %>%
  slice(0)

# Print results of duplication check
if (nrow(dup_lots) == 0) {
  cat("All lot_id values are unique.\n")
} else {
  cat("Duplicate lot_id values:\n")
  print(dup_lots)

  # Check whether duplicates have one cancelled and one not cancelled row per lot_id
  dup_lot_cancelled_pattern <- data %>%
    filter(n_lot_id > 1) %>%
    summarise(
      n_rows = n(),
      n_cancelled = sum(tender_cancelled == "Ja", na.rm = TRUE),
      n_not_cancelled = sum(tender_cancelled == "Nej", na.rm = TRUE),
      .by = lot_id
    )

  # If any duplicate lot_id values do not follow the expected pattern of one 
  # cancelled and one not cancelled row, print these and stop the script to review before cleaning.
  unexpected_dup_lots <- dup_lot_cancelled_pattern %>%
    filter(!(n_rows == 2 & n_cancelled == 1 & n_not_cancelled == 1))
  if (nrow(unexpected_dup_lots) > 0) {
    print(unexpected_dup_lots)
    stop("Unexpected duplicate lot_id pattern. Review unexpected_dup_lots before cleaning.")
  }
  
  # Save object of what rows are cancelled
  cancelled_duplicate_lots <- data %>%
    filter(n_lot_id > 1, tender_cancelled == "Ja")

  # Filter out cancelled duplicate rows, keeping the non-cancelled row for each duplicated lot_id
  data <- data %>%
    filter(!(n_lot_id > 1 & tender_cancelled == "Ja"))
}

# Check whether duplicates remain after dropping duplicate cancelled/non-cancelled pairs
data <- data %>%
  select(-n_lot_id) %>%
  mutate(n_lot_id = n(), .by = lot_id)

remaining_dup_lots <- data %>%
  filter(n_lot_id > 1) %>%
  distinct(lot_id, n_lot_id)

if (nrow(remaining_dup_lots) > 0) {
  print(remaining_dup_lots)
  stop("Duplicate lot_id values remain after filtering cancelled duplicate rows.")
}

# Tender/lot-level data to join onto cleaned entity tables at the end.
tender_lot_data <- data %>%
  select(any_of(c(
    "tender_id", "lot_id", "contract_type", "lot_number", "buyer_name",
    "n_lots", "n_lots_contracted", "n_lot_winners", "n_bids_received",
    "tender_amount", "lot_amount",
    "tender_amount_eur", "tender_amount_dkk", "lot_amount_eur", "lot_amount_dkk",
    "lot_amount_orig", "flag_all_orig_lot_amt_missing",
    "n_bidders",
    "pub_date", "award_date", "award_url", "submit_date",
    "divided_tender", "joint_tender", "consortium_winner",
    "cpv_code", "cpv_code_first", "cpv_division", "cpv_division_name",
    "cpv_sector", "cpv_category",
    "tender_cancelled", "tender_status", "flag_awarded",
    "contract_duration_months_min", "contract_duration_months_max", "award_end_date",
    "annualised_tender_amount", "annualised_lot_amount",
    "n_lot_id"
  ))) %>%
  arrange(tender_id, lot_id, lot_number) %>%
  distinct(tender_id, lot_id, lot_number, .keep_all = TRUE) %>%
  # Shared TED notice id (tender-level) so KFST links to OpenTender + the TED extract.
  # See derive_ted_notice_id() in functions.R. Flows to winner + buyer via the join.
  mutate(ted_notice_id = derive_ted_notice_id(award_url))


# 2 Winners 
## (CONSORTIUM extraction: ';' = winners via tiers, ',' = consortium members)
original_winner_data <- data %>%
  transmute(tender_id, lot_id, winner_cvr_original = winner_cvr,
            winner_name_original = winner_name, winner_country_original = winner_country) %>%
  distinct(tender_id, lot_id, .keep_all = TRUE)

# Winner fields as the consortium extraction consumes them (blank -> "", one row/lot).
# Named `raw` so the ';'-split below (and downstream consumers) are unchanged.
raw <- data %>%
  transmute(tender_id, lot_id,
            winner_cvr      = replace_na(as.character(winner_cvr), ""),
            winner_name     = replace_na(as.character(winner_name), ""),
            winner_country  = replace_na(as.character(winner_country), ""),
            consortium_flag = replace_na(as.character(consortium_winner), ""))


## 2.1 CVR extraction
### 2.1.1 Make different cases for separation
w_semi <- raw %>%
  mutate(nc = n_pieces(winner_cvr), nn = n_pieces(winner_name),
         nk = n_pieces(winner_country), ncons = n_pieces(consortium_flag),
         nvc = compute_distinct_valid_cvr(winner_cvr, collapse_whitespace = FALSE, drop_invalid = TRUE),  # distinct valid field CVRs
         semi_tier = case_when(
           nc == nn & nn == nk & nn == ncons ~ 1L,   # all agree
           nc == nn & (nk == nn | nk == 1L) & (ncons == nn | ncons == 1L) ~ 2L,  # cvr & name agree; country/consortium agree-or-single)
           TRUE ~ 3L))  # cvr & name disagree (or a partial-count country/flag)

cat("separation tier counts:\n")
print(w_semi %>% count(semi_tier))

### 2.1.2 tier 1 -- split cvr + name + country + consortium flag on ';'
winners_semi_t1 <- w_semi %>% 
  filter(semi_tier == 1L) %>%
  select(tender_id, lot_id, winner_cvr, winner_name, winner_country, consortium_flag) %>%
  separate_longer_delim(c(winner_cvr, winner_name, winner_country, consortium_flag), delim = ";") %>%
  group_by(tender_id, lot_id) %>% 
  mutate(winner_number = row_number()) %>% 
  ungroup() %>%
  mutate(semi_tier = "1_all_agree")

### 2.1.3 tier 2 -- split cvr + name on ';'
# The country/consortium entry is applied to each winner
winners_semi_t2 <- w_semi %>% 
  filter(semi_tier == 2L) %>%
  select(tender_id, lot_id, winner_cvr, winner_name, winner_country, consortium_flag, nc, nk, nn, ncons) %>%
  rowwise() %>% 
  # Apply consortium/country to each winner
  mutate(consortium_flag = ifelse(ncons == 1, 
                                  paste(rep(consortium_flag, nc), collapse = ";"),
                                  consortium_flag),
         winner_country = ifelse(nk == 1, 
                                 paste(rep(winner_country, nc), collapse = ";"),
                                 winner_country)) %>% 
  ungroup() %>% 
  separate_longer_delim(c(winner_cvr, winner_name, winner_country, consortium_flag), delim = ";") %>%
  group_by(tender_id, lot_id) %>%
  mutate(winner_number = row_number()) %>%
  ungroup() %>%
  mutate(semi_tier = "2_country_differs") %>%
  select(-nc, -nk, -nn, -ncons)  

### 2.1.4 tier 3 -- cvr & name counts disagree, or only country/flag partial
# More precise extraction required since naive separate_longer_delim() 
# would mispair everything after a mid-string gap.

# --- tier 3a: more valid CVRs than names -> keep every field CVR 
winners_semi_t3a <- w_semi %>% 
  filter(semi_tier == 3L, nc != nn, nvc > nn) %>%
  # Extract all valid CVR candidates from each lot:
  lot_field_cvrs() %>%
  group_by(tender_id, lot_id) %>% 
  mutate(winner_number = row_number()) %>% 
  ungroup() %>%
  transmute(tender_id, lot_id, winner_cvr = cvr,
            winner_name = NA_character_, winner_country = NA_character_,
            consortium_flag = NA_character_, winner_number,
            semi_tier = "3_more_cvrs_than_names", registry_score = NA_real_)

# --- tier 3b: fewer/equal valid CVRs than names. 
# Split the names (the anchor) and line up country/flag positionally
# leave winner_cvr NA and tag the rows "3b_pending" so the matching code
# (i.e. 2_1) can pair each name to the lot's OWN field CVRs via the registry.
t3b_src <- w_semi %>%
  filter(semi_tier == 3L, nc != nn, nvc <= nn)

# names are the anchor (one winner_number per name); country + flag lined up positionally.
# Keep empty name pieces as rows (a lot whose name field is all ';' -> empty winner rows).
t3_names <- semi_split(t3b_src, "winner_name") %>% # Splits by winner name and does some other operations; see functions.R.
  mutate(winner_name = str_trim(winner_name))

# recycle a single country to every name (nn pieces) so we send DK lots to matching if needed
# the flag stays positional (recycling it could re-route a paired comma-name into name-matching)
t3_context <- semi_split(t3b_src %>% 
                           mutate(winner_country = recycle_single(winner_country, nn)),
                         "winner_country") %>%
  mutate(winner_country = str_trim(winner_country)) %>%
  full_join(semi_split(t3b_src, "consortium_flag") %>% mutate(consortium_flag = str_trim(consortium_flag)),
            by = c("tender_id", "lot_id", "winner_number"))

winners_semi_t3b <- t3_names %>%
  left_join(t3_context, by = c("tender_id", "lot_id", "winner_number")) %>%
  mutate(winner_cvr = NA_character_,        # NA -> matched in 2_1; field CVRs paired there (needs keys)
         registry_score = NA_real_,
         semi_tier = case_when(
           winner_name != "" & winner_name != "NA" ~ "3b_pending",  # 2_1 does the registry field pairing
           TRUE                                    ~ "3_blank")) %>% # empty placeholder row
  select(tender_id, lot_id, winner_cvr, winner_name, winner_country, consortium_flag,
         winner_number, semi_tier, registry_score)

# --- tier 3c: EQUAL cvr & name counts (only aux partial) -> positional lockstep --------------
t3c_src <- w_semi %>% filter(semi_tier == 3L, nc == nn)

# cvr & name align 1:1, so split them lockstep; country + flag joined best-effort by position
t3c_anchor <- t3c_src %>%
  select(tender_id, lot_id, winner_cvr, winner_name) %>%
  separate_longer_delim(c(winner_cvr, winner_name), delim = ";") %>%
  group_by(tender_id, lot_id) %>% 
  mutate(winner_number = row_number()) %>% 
  ungroup()

t3c_context <- semi_split(t3c_src %>% 
                            mutate(winner_country = recycle_single(winner_country, nn)),
                          "winner_country") %>%
  mutate(winner_country = str_trim(winner_country)) %>%
  full_join(semi_split(t3c_src, "consortium_flag") %>% mutate(consortium_flag = str_trim(consortium_flag)),
            by = c("tender_id", "lot_id", "winner_number"))

winners_semi_t3c <- t3c_anchor %>%
  left_join(t3c_context, by = c("tender_id", "lot_id", "winner_number")) %>%
  mutate(semi_tier = "3_name_country_positional", registry_score = NA_real_) %>%
  select(tender_id, lot_id, winner_cvr, winner_name, winner_country, consortium_flag,
         winner_number, semi_tier, registry_score)

# combine the tiers and trim. Each tier already carries consortium_flag (split lockstep in
# tiers 1-2/3c, positionally joined in 3b, NA in 3a), so no separate flag join is needed.
winners <- bind_rows(winners_semi_t1, 
                     winners_semi_t2,
                     winners_semi_t3a, 
                     winners_semi_t3b, 
                     winners_semi_t3c) %>%
  mutate(across(c(winner_cvr, winner_name, winner_country, consortium_flag), str_trim))

# 3 Consortiums
# Above logic splits only on ';'. More splitting can happen on ',' for consortia
# These are flagged with "Ja" -> TRUE (case-insensitive), or a de-facto
#   flag for any winner whose CVR field already holds >=2 CVRs
winners <- winners %>%
  mutate(is_consortium = replace_na(str_to_lower(consortium_flag) == "ja", FALSE) |
           str_count(replace_na(winner_cvr, ""), "(?<![0-9])[0-9]{8}(?![0-9])") >= 2L)

cat("Number of consortia rows detected:\n")
print(winners %>% count(is_consortium))

## 3.1 Consortium splitting
winners_consort <- winners %>%
  filter(is_consortium) %>%
  mutate(consortium_name = winner_name,
         consortium_cvr  = winner_cvr)

### 3.1.1 Count after standardising delimiters
winners_consort <- winners_consort %>%
  mutate(winner_cvr = gsub("[.:]", ",", winner_cvr),
         winner_country = gsub("[.:]", ",", winner_country)) %>%
  mutate(n_cvr_implied = str_count(replace_na(winner_cvr, ""), ",") + 1L,
         n_name_implied = str_count(replace_na(winner_name, ""), ",") + 1L,
         n_country_implied = str_count(replace_na(winner_country, ""), ",") + 1L) %>%
  mutate(to_split = (n_cvr_implied == n_name_implied) & (n_name_implied == n_country_implied))

# First split: naive on ','
winners_consort_s1 <- winners_consort %>%
  filter(to_split) %>%
  separate_longer_delim(cols = c(winner_cvr, winner_name, winner_country), delim = ",")

# For leftovers, find rows where to_split check failed due to only one listed country
winners_consort_s1_resid <- winners_consort %>%
  filter(!to_split) %>%
  mutate(to_split_s2 = n_cvr_implied == n_name_implied &
           (n_cvr_implied != n_country_implied) &
           (n_country_implied == 1))

winners_consort_s2 <- winners_consort_s1_resid %>%
  filter(to_split_s2) %>%
  separate_longer_delim(cols = c(winner_cvr, winner_name), delim = ",")

# For leftovers of step 2, split by name only and send to matching. Keep ALL of them (including foreign
# or blank-country consortia) so no winner row is silently dropped -- the matcher's DK gate simply leaves
# the non-Danish ones unmatched, rather than us discarding them here.
to_review_via_match <- winners_consort_s1_resid %>%
  filter(!to_split_s2)

to_review_via_match <- to_review_via_match %>%
  separate_longer_delim(winner_name, ",")

## 3.2 Put consortium tranches back together
clean_winner_data <- bind_rows(
  winners %>% filter(!is_consortium) %>% mutate(type = "simple split on ;"),
  winners_consort_s1 %>% mutate(type = "simple consort split on ,"),
  winners_consort_s2 %>% mutate(type = "only split on name, cvr, ignore country"),
  to_review_via_match %>% mutate(type = "no easy split - send to matching")
) %>%
  # Bridge to the existing production CVR-cleaning (§4): the member CVR becomes the "candidate
  # original", and winner_cvr_clean is seeded from it (§4 then standardises/validates it).
  rename(winner_cvr_candidate_original = winner_cvr) %>%
  mutate(winner_cvr_clean = winner_cvr_candidate_original)

## 3.3 Recycled-consortium repair (conservative; fully-DK consortia only). A consortium listing more
## member firms than distinct valid CVRs has had a CVR recycled onto members it may not belong to
## Repair it ONLY when every member's country is Danish -- the CVR registry is Danish, so name-matching a foreign
## member is meaningless, and touching a mixed DK/foreign consortium would leave the foreign member on
## the recycled copy. For an eligible consortium: clear the recycled CVR and tag every member
## "3b_pending" so 2_1 pairs each listed CVR to the member it name-matches (graft A) and sends the rest
## to the registry matcher (graft C). The general DK-gate normalisation below then collapses each member's
## all-Danish country to "DK" so the matcher's exact `winner_country == "DK"` gate lets the unpaired
## members through. Registry-free (counts + country).
clean_winner_data <- clean_winner_data %>%
  mutate(.country_up = str_to_upper(replace_na(winner_country, "")),
         .member_dk  = str_detect(.country_up, "DK") & str_remove_all(.country_up, "DK|[^A-Z]") == "") %>%
  group_by(tender_id, lot_id, winner_number) %>%
  mutate(.n_members   = n(),
         .n_valid_cvr = compute_distinct_valid_cvr(paste(winner_cvr_candidate_original, collapse = ";"),
                                                   collapse_whitespace = FALSE, drop_invalid = TRUE),
         .recycled    = replace_na(is_consortium, FALSE) & .n_valid_cvr >= 1L &
                        .n_members > .n_valid_cvr & all(.member_dk)) %>%   # all() => whole consortium DK
  ungroup()

cat(sprintf("Recycled-consortium repair (fully-DK only): %d members in %d winners re-routed\n",
            sum(clean_winner_data$.recycled),
            clean_winner_data %>% filter(.recycled) %>% distinct(tender_id, lot_id, winner_number) %>% nrow()))

clean_winner_data <- clean_winner_data %>%
  mutate(winner_cvr_candidate_original = ifelse(.recycled, NA_character_, winner_cvr_candidate_original),
         winner_cvr_clean              = ifelse(.recycled, NA_character_, winner_cvr_clean),
         semi_tier                     = ifelse(.recycled, "3b_pending", semi_tier)) %>%
  select(-.country_up, -.member_dk, -.n_members, -.n_valid_cvr, -.recycled)

# DK-gate normalisation (ALL winners -- one consistent rule for the main and consortium tranches).
# Source/consortium country fields are often all-Danish but not a clean "DK": an un-split consortium
# country ("DK,DK,DK"), or the malformed source value "DK04". Those fail the matcher's exact
# `winner_country == "DK"` gate in 2_1 and silently exclude Danish firms. Collapse any country whose
# tokens are ALL Danish (DK, optionally with trailing digits) to "DK"; leave mixed DK+foreign (e.g.
# "DK,DE") untouched. This REPLACES the §3.3-local country fix, so recycled and non-recycled winners
# are treated identically. winner_country_original (joined below) preserves the raw value.
clean_winner_data <- clean_winner_data %>%
  mutate(winner_country = if_else(
    replace_na(grepl("DK", toupper(winner_country)) &
                 gsub("DK|[^A-Z]", "", toupper(winner_country)) == "", FALSE),
    "DK", winner_country))

# Join the full production tender-lot context + original (raw) winner fields, so the winner
# rows carry all the production columns.
clean_winner_data <- clean_winner_data %>%
  left_join(tender_lot_data, by = c("tender_id", "lot_id")) %>%
  left_join(original_winner_data, by = c("tender_id", "lot_id"))

# Number the consortia within each lot: members of the same consortium share a
# consortium_number (1, 2, ...); non-consortium rows get NA.
clean_winner_data <- clean_winner_data %>%
  group_by(tender_id, lot_id) %>%
  mutate(consortium_number = dense_rank(if_else(is_consortium, winner_number, NA_integer_))) %>%
  ungroup()

# Keep only lots that had winners (as production 1_1 does).
clean_winner_data <- clean_winner_data %>% filter(n_lot_winners > 0)


# 4 Clean up/standardise CVR numbers
## Cleaning flags treat NAs as FALSE: a missing source value is not counted as
## evidence that a cleaning operation was performed.
clean_winner_data <- clean_winner_data %>%
  mutate(
    # Remove white space
    flag_cvr_ws = coalesce(str_detect(winner_cvr_clean, "\\s"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "\\s+"),
    
    # Remove alphabetical letters
    flag_cvr_alphabet = coalesce(str_detect(winner_cvr_clean, "[[:alpha:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:alpha:]]"),
    
    # Remove all punctuation
    flag_cvr_punct = coalesce(str_detect(winner_cvr_clean, "[[:punct:]]"), FALSE),
    winner_cvr_clean = str_remove_all(winner_cvr_clean, "[[:punct:]]+"),
    
    # Flag if any standardisation performed
    flag_cvr_standardised = coalesce(
      flag_cvr_ws | 
        flag_cvr_alphabet | 
        flag_cvr_punct,
      FALSE
    )
    )

## 4.1 Standardise winner names for matching
winner_name_prepared <- prepare_cvr_name(clean_winner_data$winner_name)

clean_winner_data <- clean_winner_data %>%
  mutate(
    winner_name_basic = winner_name_prepared$name_basic,
    winner_name_match = winner_name_prepared$name_clean,
    winner_name_no_spaces = winner_name_prepared$name_no_spaces,
    winner_name_broad = winner_name_prepared$name_broad,
    winner_firm_type = winner_name_prepared$firm_type,
    winner_name_first_letter = winner_name_prepared$first_letter
  )

## 4.2 Initial winner CVR quality flags
## Quality flags treat NAs as FALSE: missing values are captured by explicit
## missingness flags, not by propagating NA through boolean indicators.
# Flag valid CVR numbers (exactly 8 digits, no letters or special characters)
# missing/invalid = FALSE, valid = TRUE
clean_winner_data <- clean_winner_data %>% 
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

# Flag transformed winner CVR number (not equal to original winner CVR number)
# Don't do this for multiple winners because the original winner CVR number 
# is not necessarily wrong in this case (it may just be the first of multiple 
# CVRs listed in the original data, which we have now separated into multiple rows).
clean_winner_data <- clean_winner_data %>% 
  mutate(
    flag_winner_cvr_changed = coalesce(
      winner_cvr_clean != winner_cvr_candidate_original,
      FALSE
    )
  )

## 4.3 Fill missing CVRs when the same winner name has one valid CVR elsewhere
### 4.3.1 Count the distinct valid CVRs observed for each exact winner name
# This uses the original winner name rather than a standardised name. Exact-name
# matching is more conservative because it does not combine similar-looking firms.
valid_invalid_cvr_winner_key <- clean_winner_data %>%
  filter(!is.na(winner_name), winner_name != "") %>%
  distinct(winner_name, winner_cvr_clean, valid_cvr) %>%
  mutate(
    n_valid_cvr = sum(valid_cvr),
    n_total_cvr = n(), # Includes missing and invalid cleaned CVR values
    .by = winner_name
  )

### 4.3.2 Record which KFST lots supplied each valid winner-name/CVR pair
# lot_id uniquely identifies the original KFST lot. Keeping every source lot
# makes each borrowed CVR traceable back to the rows that supplied it.
valid_cvr_sources <- clean_winner_data %>%
  filter(valid_cvr, !is.na(winner_name), winner_name != "") %>%
  summarise(
    lot_id_borrowed_from = paste(sort(unique(lot_id)), collapse = ";"),
    .by = c(winner_name, winner_cvr_clean)
  )

### 4.3.3 Keep only names linked to exactly one distinct valid CVR
# Names linked to several valid CVRs are ambiguous and are not filled.
single_valid_cvr_key <- valid_invalid_cvr_winner_key %>%
  filter(n_valid_cvr == 1, n_total_cvr > 1, valid_cvr) %>%
  rename(winner_cvr_valid_from_same_name = winner_cvr_clean) %>%
  select(-valid_cvr, -n_valid_cvr, -n_total_cvr) %>%
  distinct()

single_valid_cvr_key <- single_valid_cvr_key %>%
  left_join(
    valid_cvr_sources,
    by = c(
      "winner_name",
      "winner_cvr_valid_from_same_name" = "winner_cvr_clean"
    )
  )

### 4.3.4 Join the same-name CVR onto the full winner data
# Missing winner names cannot match each other. This prevents unrelated rows
# with missing names from borrowing a CVR from one another.
n_winner_rows_before_cvr_fill <- nrow(clean_winner_data)

clean_winner_data <- left_join(
  clean_winner_data,
  single_valid_cvr_key,
  by = "winner_name",
  na_matches = "never"
)

if (nrow(clean_winner_data) != n_winner_rows_before_cvr_fill) {
  stop("Joining same-name CVRs changed the number of KFST winner rows.")
}

### 4.3.5 Fill only rows whose cleaned CVR is missing
# Use winner_cvr_clean here rather than winner_cvr_original. A multiple-winner
# source row can contain CVRs overall while one separated winner still has none.
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_fill_missing_cvr = coalesce(
      (is.na(winner_cvr_clean) | winner_cvr_clean == "") &
        !is.na(winner_cvr_valid_from_same_name) &
        winner_cvr_valid_from_same_name != "",
      FALSE
    )
  )

clean_winner_data <- clean_winner_data %>%
  mutate(
    winner_cvr_clean = ifelse(
      flag_fill_missing_cvr,
      winner_cvr_valid_from_same_name,
      winner_cvr_clean
    ),
    # Source lots are relevant only when a CVR was actually borrowed.
    lot_id_borrowed_from = ifelse(
      flag_fill_missing_cvr,
      lot_id_borrowed_from,
      NA_character_
    )
  )

# Recalculate validity after filling the missing CVRs.
clean_winner_data <- clean_winner_data %>%
  mutate(valid_cvr = coalesce(str_detect(winner_cvr_clean, "^\\d{8}$"), FALSE))

n_filled_winner_cvrs <- sum(clean_winner_data$flag_fill_missing_cvr)
cat("Number of missing winner CVRs filled from the same exact winner name:",
    n_filled_winner_cvrs, "\n")

if (any(clean_winner_data$flag_fill_missing_cvr & !clean_winner_data$valid_cvr)) {
  stop("At least one borrowed KFST winner CVR is not a valid eight-digit CVR.")
}

if (any(clean_winner_data$flag_fill_missing_cvr &
        (is.na(clean_winner_data$lot_id_borrowed_from) |
           clean_winner_data$lot_id_borrowed_from == ""))) {
  stop("At least one borrowed KFST winner CVR is missing its source lot.")
}

## 4.4 Other winner quality flags
# Flag missing CVR number
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_cvr =
      coalesce(
        is.na(winner_cvr_clean) | winner_cvr_clean == "",
        FALSE
      )
  )

# Flag missing winner name
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_name = 
           coalesce(
             is.na(winner_name) | winner_name == "",
             FALSE
           )
  )

# Foreign winner
clean_winner_data <- clean_winner_data %>%
  mutate(flag_foreign_winner = coalesce(toupper(trimws(winner_country)) != "DK", FALSE))

# Missing country
clean_winner_data <- clean_winner_data %>%
  mutate(flag_missing_winner_country = coalesce(is.na(winner_country), FALSE))

# Flag when n winners extracted agrees with original data
clean_winner_data <- clean_winner_data %>% 
  mutate(n_winners_extracted = n(), .by = c("tender_id", "lot_id"))
clean_winner_data <- clean_winner_data %>%
  mutate(flag_mismatch_winner_count = 
           coalesce(n_winners_extracted != n_lot_winners, FALSE))

# Single bidder
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_single_bidder = coalesce(n_bids_received == 1, FALSE)
  )

# Multi-lot tender
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_multilot = coalesce(n_lots > 1, FALSE)
  )

# Cancelled procurement
clean_winner_data <- clean_winner_data %>%
  mutate(flag_cancelled = coalesce(tender_cancelled != "Nej", FALSE))

# Observation review
clean_winner_data <- clean_winner_data %>%
  mutate(
    flag_missing_cvr_with_name = coalesce(
      flag_missing_winner_cvr & !flag_missing_winner_name,
      FALSE
    ),
    flag_check_fuzzy_match = coalesce(
      flag_missing_winner_cvr & !flag_missing_winner_name,
      FALSE
    ),
    flag_review_cvr = coalesce(!flag_missing_winner_cvr & !valid_cvr, FALSE),
    flag_review_n_winners = coalesce(flag_mismatch_winner_count, FALSE), 
    flag_no_winner_info = coalesce(
      flag_missing_winner_cvr & 
        flag_missing_winner_name & 
        flag_missing_winner_country,
      FALSE
    ),
    flag_verify_cvr_external = coalesce(
      case_when(
        flag_missing_cvr_with_name ~ TRUE, 
        flag_review_cvr ~ TRUE, 
        flag_no_winner_info ~ FALSE, # Cannot verify without information.
        valid_cvr ~ FALSE,
        TRUE ~ FALSE
      ),
      FALSE
    )
  )

# 5 Buyers
## Buyers do not have CVR numbers, but they have names.
buyer_data <- data %>%
  select(tender_id, lot_id, buyer_name, joint_tender) # joint_tender already recoded to English above
original_buyer_data <- buyer_data # Store original for later joining

## 5.1 Separate into single and multiple buyer tenders
## According to the documentation (page 27, variable 19: 'Navn på ordregiver')
## multiple contracting authorities are separated by a semicolon.
## Buyer name is always populated (no NAs); so this split completely covers the data
single_buyer_data <- buyer_data %>% 
  filter(!str_detect(buyer_name, ";"))
multi_buyer_data <- buyer_data %>%
  filter(str_detect(buyer_name, ";"))

## 5.2 Split multiple buyers into one row per buyer.
## Note, I don't need the extract_multiple_cvr() function because 
## the documentation is clear about how multiple buyers are separated
multiple_buyer_long <- multi_buyer_data %>%
  separate_rows(buyer_name, sep = ";")

## 5.3 Clean up/add buyer_numbers
multiple_buyer_long <- multiple_buyer_long %>%
  mutate(
    buyer_name = str_squish(buyer_name),
    buyer_number = row_number(),
    source = "multiple listed buyers",
    .by = c(tender_id, lot_id)
  )

## 5.4 Clean single buyer data
## If only one buyer is listed, keep one row. Joint tenders with unlisted buyers
## remain one row because the unlisted buyers cannot be separated from this field.
single_buyer_data <- single_buyer_data %>%
  mutate(
    buyer_number = 1,
    source = "single buyer or joint tender with unlisted buyers"
  )

## 5.5 Bind single and multiple buyers
clean_buyer_data <- bind_rows(single_buyer_data, multiple_buyer_long) %>%
  arrange(tender_id, buyer_number) %>%
  select(tender_id, lot_id, buyer_number, buyer_name, source)

## 5.6 Join original tender data and original buyer data
clean_buyer_data <- left_join(clean_buyer_data, tender_lot_data %>% select(-buyer_name), # Don't need to add buyer_name here. 
                               by = c("tender_id", "lot_id"),
                              suffix = c("", "_original"))
clean_buyer_data <- left_join(clean_buyer_data, original_buyer_data, 
                               by = c("tender_id", "lot_id"),
                               suffix = c("", "_original"))


## 5.7 Standardise buyer names for matching
buyer_name_prepared <- prepare_cvr_name(clean_buyer_data$buyer_name)

clean_buyer_data <- clean_buyer_data %>%
  mutate(
    buyer_name_basic = buyer_name_prepared$name_basic,
    buyer_name_match = buyer_name_prepared$name_clean,
    buyer_name_no_spaces = buyer_name_prepared$name_no_spaces,
    buyer_name_broad = buyer_name_prepared$name_broad,
    buyer_firm_type = buyer_name_prepared$firm_type,
    buyer_name_first_letter = buyer_name_prepared$first_letter
  )

## 5.8 Quality/processing flags
## Flag joint tenders with unlisted buyers 
## (i.e. joint tenders that do not have multiple buyers 
## listed in the buyer_name field)
clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    flag_joint_unlisted_buyers = coalesce(
      joint_tender == "joint" &
        source == "single buyer or joint tender with unlisted buyers",
      FALSE
    )
  )

# Flag single buyer name changes and missingness
clean_buyer_data <- clean_buyer_data %>%
  mutate(
    flag_single_buyer_name_changed = coalesce(
      buyer_name != buyer_name_original &
        source == "single buyer or joint tender with unlisted buyers",
      FALSE
    )
  )

# Flag missing buyer names
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_missing_buyer_name = coalesce(is.na(buyer_name) | buyer_name == "", FALSE))

# Flag extracted n_buyers with implied number from original buyer name
clean_buyer_data <- clean_buyer_data %>% 
  mutate(n_buyers_extracted = n(), .by = c(tender_id, lot_id))

clean_buyer_data <- clean_buyer_data %>% 
  mutate(
    n_buyers_listed_original = str_count(buyer_name_original, ";") + 1,
    flag_buyer_count_agree = coalesce(n_buyers_extracted == n_buyers_listed_original, FALSE)
  )

# Flag fuzzy match check (only requires non-missing buyer name; no CVR numbers available)
clean_buyer_data <- clean_buyer_data %>% 
  mutate(flag_check_fuzzy_match = coalesce(!flag_missing_buyer_name, FALSE))

# Ensure natural row grains for each of winner/buyer side.
# Winner grain = tender-lot-winner-MEMBER: consortium members share a winner_number but are
# distinct firms, so the member's candidate CVR + name are part of the grain. Deduping only on
# (winner_number, winner_cvr_clean) would collapse name-only members (all NA winner_cvr_clean)
# into one row and silently drop real consortium members.
clean_winner_data <- clean_winner_data %>%
  distinct(tender_id, lot_id, winner_number, consortium_number,
           winner_cvr_candidate_original, winner_name, .keep_all = TRUE)
# buyer = tender-lot-buyer
clean_buyer_data <- clean_buyer_data %>%
  distinct(tender_id, lot_id, buyer_number, .keep_all = TRUE)

# Check that amount fields (incl. the EUR/DKK versions carried through the joins)
# are present in both saved KFST outputs.
required_amount_cols <- c("tender_amount", "lot_amount",
                          "tender_amount_eur", "tender_amount_dkk",
                          "lot_amount_eur", "lot_amount_dkk")
stopifnot(all(required_amount_cols %in% names(clean_winner_data)))
stopifnot(all(required_amount_cols %in% names(clean_buyer_data)))

# 6 Save
saveRDS(clean_winner_data, file.path(dirs$clean_data, "clean_winner_data_kfst.rds"))
saveRDS(clean_buyer_data, file.path(dirs$clean_data, "clean_buyer_data_kfst.rds"))
