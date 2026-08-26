# Score the quality of matched OpenTender winner CVRs.
# For every winner with a final CVR and a name, measure how well the (cleaned) winner name
# agrees with the REGISTERED name of that final CVR: the best (max) levenshtein ratio over the
# CVR's registered names (main + bi-names). This is an independent quality signal that applies
# to BOTH provenances -- CVRs kept from the original source data AND CVRs obtained by name
# matching -- because it scores winner_cvr_final regardless of how the CVR was found.
# Augments clean_winner_data_ot_name_matched.rds in place with three new columns:
#   cvr_name_match_quality      -- the best name<->registry levenshtein ratio (0-100)
#   cvr_name_match_quality_name -- the registered name that produced that best score
#   cvr_name_is_substring       -- is the cleaned winner name a verbatim substring of that name?
#                                  (rescues terse-but-correct names, e.g. "axa" in "axa forsikring ...")
# NOTE: distinct from name_match_score, which is the matcher's own fuzzy confidence and exists
# only for name-matched rows; this metric is computed for all rows with a final CVR + name.
# Author: Jack Mulqueeney
# Date: 20 August 2026

rm(list = ls())

# Load config
source("config.R")

# Packages
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
}))

# Source functions
source(file.path(PROJECT_DIR, "code", "functions.R"))

# Directories
clean_data_dir <- dirs$clean_data

# 1 Load the matched OpenTender winners
ot_path <- file.path(clean_data_dir, "clean_winner_data_ot_name_matched.rds")
winner_data <- as.data.table(readRDS(ot_path))

# The quality columns this step adds. Defined up front so the step is IDEMPOTENT: drop any that
# already exist (from a previous run of this script) BEFORE snapshotting the base, so re-running 2_6
# on its own output re-derives them cleanly instead of tripping the guard below.
new_cols <- c("cvr_name_match_quality", "cvr_name_match_quality_basic", "cvr_name_match_quality_nospaces",
              "cvr_name_match_quality_broad", "cvr_name_match_quality_name", "cvr_name_is_substring",
              "flag_cvr_recovered_from_invalid")
winner_data[, (intersect(new_cols, names(winner_data))) := NULL]

# Baseline snapshot: we must re-save an object identical to this one EXCEPT for the added columns.
orig_cols <- copy(names(winner_data))
orig_snapshot <- copy(winner_data)

# 2 Registry lookup: cvr (%08d) -> cleaned registered name FORMS (main + bi-names), built by 1_3.
cvr_key_quality <- unique(rbindlist(list(
  as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_name_key.rds")))[
    , .(cvr = sprintf("%08d", as.integer(cvr)), name_match, name_basic, name_no_spaces, name_broad)],
  as.data.table(readRDS(file.path(clean_data_dir, "clean_cvr_biname_key.rds")))[
    , .(cvr = sprintf("%08d", as.integer(cvr)), name_match, name_basic, name_no_spaces, name_broad)]
)))

# 3 Data quality: how well the winner's name agrees with the REGISTERED name of its final CVR -- the
# best levenshtein ratio over that CVR's registered names. Computed for EACH prepared name form: the
# clean form is the strict default (cvr_name_match_quality); basic/no-spaces/broad are looser variants
# (kept so you can pick your own quality bar). Mirrors the KFST PART-6 block in 2_1.
winner_data[, q_row_id := .I]   # local key (match_row_id is dropped before 2_3 saves)
for (qf in list(c("winner_name_match",     "name_match",     "cvr_name_match_quality"),
                c("winner_name_basic",     "name_basic",     "cvr_name_match_quality_basic"),
                c("winner_name_no_spaces", "name_no_spaces", "cvr_name_match_quality_nospaces"),
                c("winner_name_broad",     "name_broad",     "cvr_name_match_quality_broad"))) {
  win_col <- qf[1]; key_col <- qf[2]; q_col <- qf[3]
  reg_lookup <- unique(data.table(cvr = cvr_key_quality$cvr,
                                  reg_name = cvr_key_quality[[key_col]]))[!is.na(reg_name) & reg_name != ""]
  qual <- data.table(q_row_id = winner_data$q_row_id,
                     cvr = as.character(winner_data$winner_cvr_final),
                     win_name = winner_data[[win_col]])
  qual <- qual[!is.na(cvr) & cvr != "" & !is.na(win_name) & win_name != ""]
  qual <- merge(qual, reg_lookup, by = "cvr", allow.cartesian = TRUE)
  qual[, score := levenshtein_ratio(win_name, reg_name, pairwise = TRUE)]
  setorder(qual, q_row_id, -score)
  best_qual <- qual[, .SD[1L], by = q_row_id]
  winner_data[, (q_col) := NA_real_]
  winner_data[best_qual, on = "q_row_id", (q_col) := i.score]
  if (q_col == "cvr_name_match_quality") {   # keep the matched registry name for the strict (clean) form
    winner_data[, cvr_name_match_quality_name := NA_character_]
    winner_data[best_qual, on = "q_row_id", cvr_name_match_quality_name := i.reg_name]
  }
}

# is the cleaned winner name a verbatim substring of that best registered name? Only where a registered
# name was found. str_detect is called only on non-empty patterns (empty winner name would warn).
winner_data[, cvr_name_is_substring := NA]
winner_data[!is.na(cvr_name_match_quality_name) & fcoalesce(winner_name_match, "") != "",
            cvr_name_is_substring := str_detect(cvr_name_match_quality_name, fixed(winner_name_match))]
winner_data[, q_row_id := NULL]

# Provenance: winner_cvr_final was recovered by matching BECAUSE the raw candidate field held no valid,
# REGISTERED CVR -- so the final differs from the candidate due to its invalidity. Mirrors KFST 2_1.
reg_cvrs_prov <- unique(cvr_key_quality$cvr)
cand_prov <- ifelse(is.na(winner_data$winner_cvr_candidate), "", as.character(winner_data$winner_cvr_candidate))
cand_has_reg_prov <- vapply(regmatches(cand_prov, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", cand_prov, perl = TRUE)),
                            function(v) any(v %chin% reg_cvrs_prov), logical(1))
winner_data[, flag_cvr_recovered_from_invalid :=
  cand_prov != "" & !cand_has_reg_prov & !is.na(winner_cvr_final) & as.character(winner_cvr_final) != ""]
cat("recovered-from-invalid-candidate rows:", winner_data[flag_cvr_recovered_from_invalid == TRUE, .N], "\n")

# 4 Report: overall distribution + a provenance slice (source CVR vs name-matched CVR).
# (new_cols is defined at the top so the idempotency drop + this report + the guard all agree.)
cat("data-quality scored rows:", winner_data[!is.na(cvr_name_match_quality), .N], "of", nrow(winner_data),
    "| median:", round(median(winner_data$cvr_name_match_quality, na.rm = TRUE), 1),
    "| share >=90:", round(mean(winner_data$cvr_name_match_quality >= 90, na.rm = TRUE), 3),
    "| winner name is substring of registry name:", round(mean(winner_data$cvr_name_is_substring, na.rm = TRUE), 3), "\n")
provenance <- fifelse(grepl("^source", fcoalesce(winner_data$name_match_step_code, "")),
                      "original (source) CVR", "name-matched CVR")
prov_summary <- data.table(provenance = provenance, q = winner_data$cvr_name_match_quality
  )[!is.na(q), .(n = .N, median_q = round(median(q), 1), pct_ge90 = round(mean(q >= 90), 3)), by = provenance]
cat("quality by CVR provenance:\n"); print(prov_summary)

# 5 GUARD: the re-saved object must equal the current one except for the 3 added columns. Abort
#   otherwise, so this step can never silently rewrite the canonical table with anything else.
stopifnot(
  nrow(winner_data) == nrow(orig_snapshot),
  identical(setdiff(names(winner_data), new_cols), orig_cols),
  identical(as.data.frame(winner_data[, ..orig_cols]), as.data.frame(orig_snapshot))
)

# 6 Save (augment in place)
saveRDS(winner_data, ot_path)
cat("augmented", basename(ot_path), "with:", paste(new_cols, collapse = ", "), "\n")
