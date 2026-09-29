#!/usr/bin/env Rscript
# =====================================================================================================
# STAGE 1 -- build the event universe, screen the eligible control pool, top up the employment pull, and
# emit ONE unified firm-quarter panel.
#
# This is the only stage that touches external inputs:
#   <clean>/tender_data_2006_2026.*                        the combined delivered dataset (events)
#   <clean>/clean_cvr_name_key.rds                         the ~2.27M-firm registry (sector + kommune)
#   <employment>/cvr_employment_history_control_full.parquet   the Sep-16 full-universe control pull
#   <employment>/cvr_employment_history_virk.csv           the winner-side pull (legacy CSV, read-only)
#   <employment>/cvr_employment_status_control_full.csv    ledger: who has already been QUERIED
#   <employment>/cvr_employment_history_virk_status.csv    ledger: ditto, winner side
#   the Virk API                                           only for the incremental top-up
#
# Outputs (the ONLY inputs stage 2 may open):
#   01_firm_panel.parquet        one row per (firm, quarter) at MATCH_FREQ, winners + controls
#   01_events.rds                one row per (winning cvr, award QUARTER) competitive event
#   01_eligible_controls.rds     the screened never-winner pool, with sector/kommune
#   01_pull_report.rds           what was screened, what was pulled, what is still missing
#
# WHY THE PULL IS CHEAP: the Sep-16 control_full run already used this exact division-and-kommune screen
# (CVR_EMPFULL_SECTOR_LEVEL defaults to "division"), so re-screening against the new winner universe
# leaves only ~1-2k CVRs unqueried. The ledgers are what make that diff possible -- always diff against
# WHO WAS QUERIED, not who returned data, or you re-pull every firm that legitimately has no employment.
#
#   Rscript code/matching/1_build_universe.R
# Options (env):
#   MATCH_FREQ            frequency variant to keep (default quarterly_spliced)
#   MATCH_EMP_RULE        either | fte | employees   (default either -- see 0_matching_utils.R)
#   MATCH_DIRECT_AWARD_RULE  strict | loose          (default strict: direct_award %in% FALSE)
#   MATCH_DO_PULL         1/0 hit the Virk API for the incremental top-up (default 1; forced 0 in test mode)
#   MATCH_TEST_N          >0 => dry run on the first N events, artefacts suffixed _testN
#   MATCH_CORES / MATCH_BATCH_SIZE / MATCH_PU_SCROLL   pull parallelism
#   MATCH_OVERWRITE       1 to replace existing artefacts
# =====================================================================================================

rm(list = ls())
source(file.path(getwd(), "code", "matching", "0_matching_utils.R"))
match_setup(extra_libs = c("httr", "jsonlite", "parallel", "dplyr"))

FREQ    <- match_env_chr("MATCH_FREQ", "quarterly_spliced")
TEST_N  <- match_test_n()          # 0 = full run; shared accessor, so the tag can never disagree
TEST    <- !is.na(TEST_N) && TEST_N > 0L
DO_PULL <- if (TEST) FALSE else match_env_lgl("MATCH_DO_PULL", TRUE)
P       <- match_paths()
emp_dir <- dirs$employment

cat(sprintf("STAGE 1 | freq=%s | emp_rule=%s | event grain=quarter | pull=%s%s\n", FREQ,
            match_env_chr("MATCH_EMP_RULE", "either"), DO_PULL,
            if (TEST) sprintf(" | TEST MODE (first %d events)", TEST_N) else ""))
cat(sprintf("output dir: %s\n", P$dir))

# ---- 1. the competitive winner universe -------------------------------------------------------------
match_rule_banner("1. winner universe (combined dataset)")
wu       <- winner_universe()
events   <- wu$events
winners  <- wu$winners                    # competitive winners == the control-pool exclusion set
if (TEST) {
  keep_ev <- utils::head(events$ev, TEST_N)
  events[, test_subset := ev %in% keep_ev]
  cat(sprintf("  TEST MODE: flagged the first %d events as the study subset (%d distinct winners);\n",
              length(keep_ev), uniqueN(events[test_subset == TRUE]$cvr)))
  cat("             the full award history is retained so the winner arm stays correct.\n")
} else events[, test_subset := TRUE]

# ---- 2. registry screen ------------------------------------------------------------------------------
# Candidates = registry firms sharing BOTH a winner sector-division and a winner HQ kommune (the only
# cells any event can ever match on), minus the competitive winners. Firms whose only wins were direct
# awards are deliberately NOT removed -- they pool with the never-winners.
match_rule_banner("2. registry screen")
reg <- firm_registry()
cat(sprintf("  registry firms: %d\n", nrow(reg)))
wreg <- reg[cvr %chin% winners] # Winner frame
S <- sort(unique(wreg$division[!is.na(wreg$division)])) # All winner divisions
K <- sort(unique(wreg$kommune[!is.na(wreg$kommune)])) # All winner communes
cat(sprintf("  winners in registry: %d of %d | divisions %d | kommuner %d\n",
            nrow(wreg), length(winners), length(S), length(K)))

eligible <- reg[division %chin% S & kommune %chin% K & !(cvr %chin% winners)]
cat(sprintf("  eligible control pool (division AND kommune, non-winner): %d\n", nrow(eligible)))

# Event placeability: an event needs its winner's division+kommune to sit the cascade on. Unplaceable
# events are FLAGGED, not dropped -- stage 2's sometimes-winner arm must see EVERY competitive award
# date to decide who is disqualified within a given event window, including awards it cannot itself
# study. Stage 2 treats only `placeable` events.
events <- merge(events, reg[, .(cvr, division, kommune, sector6)], by = "cvr", all.x = TRUE)
events[, placeable := !is.na(division) & !is.na(kommune)]
cat(sprintf("  events placeable: %d of %d (%.1f%%) | unplaceable (kept, flagged): %d\n",
            events[placeable == TRUE, .N], nrow(events),
            100 * events[placeable == TRUE, .N] / nrow(events), events[placeable == FALSE, .N]))
setorder(events, cvr, event_qidx)
events[, ev := .I]

# ---- 3. incremental pull ------------------------------------------------------------------------------
# Diff against WHO WAS QUERIED (the ledgers), never against who returned rows.
match_rule_banner("3. incremental employment pull")
missing_ledgers <- character(0)
  # args:  f = ledger FILENAME (not a path) inside dirs$employment.
  # A missing ledger is recorded in `missing_ledgers` and treated as empty, so a fresh data root
  # still works -- but that also makes every firm look unqueried, which is why MATCH_MAX_PULL
  # backstops it below.
  # returns: unique 8-digit CVRs that have ever been QUERIED (not necessarily returned data).
read_ledger <- function(f) {
  p <- file.path(emp_dir, f)
  if (!file.exists(p)) {
    cat(sprintf("  ledger MISSING (treated as empty): %s\n", f))
    missing_ledgers <<- c(missing_ledgers, f)
    return(character(0))
  }
  x <- fread(p, select = "cvr", colClasses = list(character = "cvr"), showProgress = FALSE)
  unique(as_cvr8(x$cvr))
}
queried <- unique(c(read_ledger("cvr_employment_status_control_full.csv"),
                    read_ledger("cvr_employment_history_virk_status.csv")))
queried <- queried[!is.na(queried)]
cat(sprintf("  already queried (both ledgers): %d\n", length(queried)))

need_controls <- setdiff(eligible$cvr, queried)
need_winners  <- setdiff(unique(events$cvr), queried)
cat(sprintf("  incremental controls to pull: %d\n", length(need_controls)))
cat(sprintf("  incremental winners  to pull: %d\n", length(need_winners)))

n_need   <- length(need_controls) + length(need_winners)
MAX_PULL <- match_env_int("MATCH_MAX_PULL", 25000L)
if (DO_PULL && n_need > MAX_PULL) {
  stop(sprintf(paste0(
    "the incremental pull is %d CVRs, above MATCH_MAX_PULL=%d.\n",
    "  Expected is ~2k: the Sep-16 control_full run already queried this same division-and-kommune\n",
    "  screen, so only a handful should be new.%s\n",
    "  A number this large almost always means a status ledger is absent, so every firm reads as\n",
    "  unqueried -- that is the ~11-hour, ~4,100-batch full pull, not a top-up.\n",
    "  Fix the ledger path, or set MATCH_MAX_PULL=%d to proceed deliberately."),
    n_need, MAX_PULL,
    if (length(missing_ledgers))
      paste0("\n  MISSING LEDGER(S): ", paste(missing_ledgers, collapse = ", ")) else
      "\n  (both ledgers were found, so this may be genuine growth.)",
    n_need), call. = FALSE)
}

pull_summary <- data.table()
if (DO_PULL && n_need > 0L) {
  pull_summary <- virk_pull_cvrs(c(need_controls, need_winners), P$shards)
} else if (!DO_PULL) {
  cat("  MATCH_DO_PULL=0 (or test mode): skipping the API. The panel is built from what is on disk.\n")
} else {
  cat("  nothing missing -- no API call needed.\n")
}

# ---- 4. assemble the unified firm-quarter panel -------------------------------------------------------
# Three sources, newest-wins on collision:
#   (c) this run's shards   (d) the winner-side pull   (e) control_full
# Only MATCH_FREQ is kept, and only the columns the pipeline actually uses -- the full 67-column,
# 65.8M-row control_full is far too wide to carry forward.
match_rule_banner("4. assemble the firm panel")
WANT <- c("cvr", "frequency", "year", "quarter", "month", "fte", "employees",
          "industry_code", "kommune_code", "hq_kommune_code", "employment_source")

# Defensive column selection: take what exists, report what does not, so a schema drift upstream
# surfaces as a message rather than a cryptic failure.
  # args:  available = column names actually present in a source; label = name for the message.
  # returns: the intersection with WANT, printing any absent columns so upstream schema drift
  #          shows up as a message instead of a cryptic failure later.
select_existing <- function(available, label) {
  keep <- intersect(WANT, available)
  miss <- setdiff(WANT, available)
  if (length(miss)) cat(sprintf("  [%s] absent columns (filled NA): %s\n", label, paste(miss, collapse = ", ")))
  keep
}

panel_parts <- list()

cf_path <- file.path(emp_dir, "cvr_employment_history_control_full.parquet")
if (file.exists(cf_path)) {
  ds   <- arrow::open_dataset(cf_path)
  keep <- select_existing(names(ds), "control_full")
  part <- as.data.table(dplyr::collect(dplyr::select(dplyr::filter(ds, frequency == FREQ),
                                                    dplyr::all_of(keep))))
  part[, src := "control_full"]
  cat(sprintf("  control_full   : %d rows | %d firms\n", nrow(part), uniqueN(part$cvr)))
  panel_parts[["control_full"]] <- part
} else cat("  control_full   : MISSING -- skipped\n")

vk_path <- file.path(emp_dir, "cvr_employment_history_virk.csv")   # legacy CSV input, read-only
if (file.exists(vk_path)) {
  part <- fread(vk_path, na.strings = "", colClasses = list(character = "cvr"), showProgress = FALSE)
  keep <- select_existing(names(part), "winner_virk")
  part <- part[frequency == FREQ, ..keep]
  part[, src := "winner_virk"]
  cat(sprintf("  winner_virk    : %d rows | %d firms\n", nrow(part), uniqueN(part$cvr)))
  panel_parts[["winner_virk"]] <- part
} else cat("  winner_virk    : MISSING -- skipped\n")

sh <- read_shards(P$shards, "emp")
if (nrow(sh)) {
  keep <- select_existing(names(sh), "new_shards")
  sh   <- sh[frequency == FREQ, ..keep]
  sh[, src := "new_shards"]
  cat(sprintf("  new_shards     : %d rows | %d firms\n", nrow(sh), uniqueN(sh$cvr)))
  panel_parts[["new_shards"]] <- sh
} else cat("  new_shards     : none\n")

if (!length(panel_parts)) stop("no employment source available -- cannot build the panel", call. = FALSE)

panel <- rbindlist(panel_parts, use.names = TRUE, fill = TRUE)
panel[, cvr := as_cvr8(cvr)]
panel <- panel[!is.na(cvr)]
panel[, src := factor(src, levels = c("new_shards", "winner_virk", "control_full"))]   # newest first
setorder(panel, cvr, year, quarter, src)
n_before <- nrow(panel)
panel <- unique(panel, by = c("cvr", "frequency", "year", "quarter"))
cat(sprintf("  deduped %d -> %d rows (newest source wins on collision)\n", n_before, nrow(panel)))

panel[, qidx := qidx_of(year, quarter)]
panel[, firm_type := fifelse(cvr %chin% winners, "winner", "control")]
if (!"hq_kommune_code" %in% names(panel)) panel[, hq_kommune_code := NA_character_]
panel[, hq_kommune_code := as.character(hq_kommune_code)]
if ("industry_code" %in% names(panel)) add_industry_hierarchy(panel, "industry_code") else {
  panel[, `:=`(industry_code6 = NA_character_, industry_class = NA_character_,
               industry_group = NA_character_, industry_division = NA_character_)]
  cat("  WARNING: no industry_code column -- the industry rungs of the cascade will not fire.\n")
}
# Row-level "this firm was employing in this quarter": fte > 0 OR employees > 0 (see emp_positive()).
# The rule is HARDCODED to "either" rather than left to MATCH_EMP_RULE. This is the function's only call
# site in the pipeline, so that env var currently changes nothing -- but report$emp_rule below still
# records whatever it is set to, so a run can report "fte" while having used "either". Fix the report or
# pass the var through; do not assume the knob works.
panel[, is_employed := emp_positive(panel, "either")]   # column name differs from the function on purpose

cat(sprintf("  panel: %d rows | %d firms (%d winner, %d control) | %d-%d\n",
            nrow(panel), uniqueN(panel$cvr),
            panel[firm_type == "winner", uniqueN(cvr)], panel[firm_type == "control", uniqueN(cvr)],
            min(panel$year), max(panel$year)))

# ---- 5. checks ----------------------------------------------------------------------------------------
match_rule_banner("5. checks")
stopifnot(all(grepl("^[0-9]{8}$", panel$cvr)))
stopifnot(all(grepl("^[0-9]{8}$", events$cvr)))
stopifnot(all(grepl("^[0-9]{8}$", eligible$cvr)))
cat("  OK  every cvr is 8 digits\n")

# The control_full pull excluded the OLD winner set, which was built from the three per-source files and
# included direct-award winners. Some competitive winners under the NEW definition therefore sit in it.
# They must be tagged `winner`, never offered as never-winner controls.
leak <- intersect(eligible$cvr, winners)
stopifnot(length(leak) == 0L)
cat(sprintf("  OK  eligible pool is winner-free (panel rows tagged winner: %d firms)\n",
            panel[firm_type == "winner", uniqueN(cvr)]))

by_year <- panel[is_employed == TRUE, .(firms = uniqueN(cvr)), by = year]
by_year <- check_year_continuity(by_year)
cat("  firms/year (positive employment):\n")
print(by_year[order(year)], nrows = 100)

if (length(queried)) {
  unresolved <- setdiff(unique(panel$cvr), queried)
  cat(sprintf("  note: %d panel firms are absent from the ledgers (pre-ledger vintages)\n",
              length(unresolved)))
}

# ---- 6. write -------------------------------------------------------------------------------------------
match_rule_banner("6. write")
eligible_out <- eligible[, .(cvr, sector6, division, kommune)]
# has_employment is an EVER condition, not a window condition: TRUE if the firm was employing in ANY
# quarter of the whole panel. Its only consumer is 2_match_controls.R:106, where it gates entry to the
# never-winner control arm -- a never-winner with no employment record anywhere can never clear
# per-event eligibility, so carrying it would only bloat the candidate scan. It is a coarse "is this a
# real operating firm" gate and nothing more.
# The threshold is > 0, the same one used by match eligibility (2_match_controls.R) and by stage 3's
# fte > MATCH_MIN_FTE. Deliberately aligned: a firm this gate admits is one the later screens can also
# admit, so pool membership never silently excludes a firm the analysis would have accepted.
eligible_out[, has_employment := cvr %chin% panel[is_employed == TRUE, unique(cvr)]]
cat(sprintf("  eligible pool with positive employment on disk: %d of %d (%.1f%%)\n",
            eligible_out[has_employment == TRUE, .N], nrow(eligible_out),
            100 * eligible_out[has_employment == TRUE, .N] / nrow(eligible_out)))

report <- list(
  run_at            = Sys.time(),
  freq              = FREQ,
  emp_rule          = match_env_chr("MATCH_EMP_RULE", "either"),
  direct_award_rule = match_env_chr("MATCH_DIRECT_AWARD_RULE", "strict"),
  direct_award_counts = wu$direct_award_counts,
  n_winner_rows     = wu$n_winner_rows,
  n_winners         = length(winners),
  n_events          = nrow(events),
  n_events_placeable = events[placeable == TRUE, .N],
  n_eligible        = nrow(eligible),
  n_queried         = length(queried),
  missing_ledgers   = missing_ledgers,
  need_controls     = length(need_controls),
  need_winners      = length(need_winners),
  did_pull          = DO_PULL,
  pull_summary      = pull_summary,
  panel_rows        = nrow(panel),
  panel_firms       = uniqueN(panel$cvr),
  firms_by_year     = by_year,
  test_n            = if (TEST) TEST_N else NA_integer_)

write_tab(panel,        P$firm_panel)
write_obj(events,       P$events)
write_obj(eligible_out, P$eligible)
write_obj(report,       P$pull_report)
cat(sprintf("  -> %s (%.0f MB)\n", basename(P$firm_panel), file.size(P$firm_panel) / 1e6))
cat(sprintf("  -> %s | %s | %s\n", basename(P$events), basename(P$eligible), basename(P$pull_report)))
cat("\nSTAGE 1 complete.\n")
