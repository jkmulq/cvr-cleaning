# =====================================================================================================
# Shared helpers for the code/matching/ pipeline. LIBRARY ONLY -- sourcing this file must have no side
# effects beyond defining functions/constants and creating the output directory.
#
#   0_matching_utils.R   <- you are here
#   1_build_universe.R   -> 01_firm_panel.parquet, 01_events.rds, 01_eligible_controls.rds
#   2_match_controls.R   -> 02_matched_panel.parquet, 02_match_table.rds, 02_match_report.rds
#   3_build_reg_data.R   -> 03_reg_data_h{h}.parquet, 03_reg_meta.rds   (h comes from the match)
#   4_run_regressions.R  -> 04_estimates.rds, 04_coefs.parquet, 04_figures/
#
# THE CHAIN IS STRICTLY LINEAR: stage n opens ONLY the 0{n-1}_* artefacts. Stage 1 is the single point
# where external inputs (the combined tender dataset, the CVR registry, the legacy status ledgers, the
# control_full pull, the Virk API) enter the pipeline. This file is shared *code*, never shared state,
# so it does not violate that rule.
#
# NO CSVs are written anywhere in this pipeline: every artefact is parquet (tabular) or rds (small
# lists/metadata), including the pull's per-batch shards. The two legacy status ledgers are CSV but are
# read-only inputs that we consume and never rewrite.
#
# WHY THE VIRK HELPERS ARE NOT RELOCATED HERE. The plan called for moving employment helpers out of
# code/functions.R. A caller grep says don't: functions.R's Virk block is an interlocking web
# (virk_scalar alone has 30 internal call sites) whose entry points generate_cvr_lookup_from_virk() and
# test_cvr_lookup_sample() are used by code/processing/0_build_cvr_lookup.R -- the CLEANING pipeline.
# Relocating it into code/matching/ would make cleaning depend on matching. The employment domain logic
# was already factored out into code/scraping/employment_functions.R (the 37 parse/splice/context
# functions), which itself documents a dependency on functions.R for format_virk_cvr()/virk_scalar().
# So we SOURCE both and own only matching-specific code here.
# =====================================================================================================

# ---- setup -----------------------------------------------------------------------------------------
# Walk up to the .Rproj marker so any stage runs from any working directory, then source config + the
# two upstream libraries. Call this at the top of every stage.
# args:
#   extra_libs  character vector of extra packages to attach on top of data.table/arrow, e.g.
#               c("httr","parallel") for the pull stage. Attached, not just namespaced, because
#               arrow's dplyr verbs only dispatch when dplyr is on the search path.
# returns: the project root path (invisibly); also sets the working directory and PROJECT_ROOT.
match_setup <- function(extra_libs = character()) {
  root <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(root, "cvr-cleaning.Rproj")) && dirname(root) != root) root <- dirname(root)
  if (!file.exists(file.path(root, "cvr-cleaning.Rproj")))
    stop("could not locate the project root (cvr-cleaning.Rproj) from ", getwd(), call. = FALSE)
  setwd(root)
  suppressWarnings(suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    for (l in extra_libs) library(l, character.only = TRUE)
  }))
  source(file.path(root, "config.R"))
  source(file.path(root, "code", "functions.R"))                              # read_clean, Virk helpers
  source(file.path(root, "code", "scraping", "employment_functions.R"))       # splice logic
  assign("PROJECT_ROOT", root, envir = .GlobalEnv)
  invisible(root)
}

# ---- paths -----------------------------------------------------------------------------------------
# Pipeline artefacts live in their own directory so they never mingle with the legacy employment/ files.
# `tag` suffixes every artefact (set from MATCH_TEST_N) so a dry run can NEVER overwrite a full run.
# args:
#   tag  suffix stamped on every artefact, e.g. "test200". "" = the full run. Defaults to
#        match_tag(), which derives it from MATCH_TEST_N -- pass it explicitly to READ another
#        run's artefacts, e.g. match_paths("test200") in an interactive session.
# returns: named list of paths. `reg_data`, `cobid_roster` and `cobid_panel` are FUNCTIONS of the
#        window h, so call P$reg_data(8), P$cobid_panel(4). The cobid paths also take the stage-5
#        score: P$cobid_panel(8, "log") -> 05_cobidder_panel_h8_log.parquet. The window is in the FILENAME because a
#        genuine h=4 design is a separate match, not an h=8 artefact trimmed to 4 -- see the note at
#        the top of 3_build_reg_data.R.
match_paths <- function(tag = match_tag()) {
  d <- file.path(dirs$data, "matching")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  sfx <- if (nzchar(tag)) paste0("_", tag) else ""
  list(
    dir            = d,
    shards         = file.path(d, paste0("_shards", sfx)),
    firm_panel     = file.path(d, paste0("01_firm_panel",        sfx, ".parquet")),
    events         = file.path(d, paste0("01_events",            sfx, ".rds")),
    eligible       = file.path(d, paste0("01_eligible_controls", sfx, ".rds")),
    pull_report    = file.path(d, paste0("01_pull_report",       sfx, ".rds")),
    matched_panel  = file.path(d, paste0("02_matched_panel",     sfx, ".parquet")),
    match_table    = file.path(d, paste0("02_match_table",       sfx, ".rds")),
    match_report   = file.path(d, paste0("02_match_report",      sfx, ".rds")),
    match_ckpt     = file.path(d, paste0("02_match_checkpoint",  sfx, ".rds")),
    reg_data       = function(h) file.path(d, sprintf("03_reg_data_h%d%s.parquet", h, sfx)),
    reg_meta       = file.path(d, paste0("03_reg_meta",          sfx, ".rds")),
    reg_attrition  = file.path(d, paste0("03_fte_screen_attrition", sfx, ".parquet")),
    # score = "level" keeps the original filenames, so existing level artefacts stay valid
    cobid_roster   = function(h, score = "level") file.path(d, sprintf("05_cobidder_roster_h%d%s%s.rds",
                       h, if (score == "log") "_log" else "", sfx)),
    cobid_panel    = function(h, score = "level") file.path(d, sprintf("05_cobidder_panel_h%d%s%s.parquet",
                       h, if (score == "log") "_log" else "", sfx)),
    estimates      = file.path(d, paste0("04_estimates",         sfx, ".rds")),
    est_panel      = file.path(d, paste0("04_estimation_panel",  sfx, ".parquet")),
    sweep          = file.path(d, paste0("04_qscore_sweep",      sfx, ".parquet")),
    coefs          = file.path(d, paste0("04_coefs",             sfx, ".parquet")),
    figures        = file.path(d, paste0("04_figures",           sfx))
  )
}

# THE single reader of MATCH_TEST_N. Everything -- the tag and every stage's TEST flag -- goes through
# this, because the two used to read it independently with DIFFERENT defaults (a stage defaulted to 200
# while match_tag() defaulted to 0). The result was a run that behaved as a 200-event test while writing
# to UNTAGGED full-run paths, silently colliding with real output. One accessor makes that impossible.
# 0 (the default) means a full run.
match_test_n <- function() {
  v <- Sys.getenv("MATCH_TEST_N", "")
  if (!nzchar(v)) return(0L)
  n <- suppressWarnings(as.integer(v))
  if (is.na(n) || n < 0L) 0L else n
}

# MATCH_TEST_N>0 marks every artefact `testN`, so dry runs are quarantined from full-run outputs.
match_tag <- function() {
  n <- match_test_n()
  if (n <= 0L) "" else paste0("test", n)
}

# ---- env helpers -----------------------------------------------------------------------------------
# args:  name = environment variable to read; default = value when it is unset or empty.
# NB returns NA (not the default) if the variable is set to something non-numeric -- that is
# deliberate, so a typo like MATCH_H=eight surfaces rather than silently reverting to 8.
match_env_int <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) return(as.integer(default))
  suppressWarnings(as.integer(v))
}
# args:  name = environment variable to read; default = value when it is unset or empty.
match_env_num <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) return(as.numeric(default))
  suppressWarnings(as.numeric(v))
}
# args:  name = environment variable to read; default = value when it is unset or empty.
match_env_chr <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (nzchar(v)) v else default
}
# args:  name = environment variable to read; default = value when unset.
# Truthy strings: 1, true, t, yes, y (case-insensitive). Everything else is FALSE.
match_env_lgl <- function(name, default) {
  v <- tolower(trimws(Sys.getenv(name, "")))
  if (!nzchar(v)) return(isTRUE(default))
  v %in% c("1", "true", "t", "yes", "y")
}
# Space/comma-separated env list -> character vector.
# args:  name = environment variable holding a space/comma-separated list; default = same,
#        as a string (e.g. "full_full full_firsthalf").
# returns: character vector, empty strings dropped.
match_env_list <- function(name, default) {
  v <- match_env_chr(name, default)
  out <- strsplit(trimws(v), "[ ,]+")[[1]]
  out[nzchar(out)]
}

# ---- io (parquet / rds only; never CSV) --------------------------------------------------------------
# Refuse to clobber an existing artefact unless MATCH_OVERWRITE=1, so a half-thought rerun cannot
# silently destroy an expensive output.
# args:  path = file that is about to be written.
# Errors if it already exists unless MATCH_OVERWRITE=1. Stops a casual rerun from destroying an
# expensive artefact.
match_guard_overwrite <- function(path) {
  if (file.exists(path) && !match_env_lgl("MATCH_OVERWRITE", FALSE))
    stop(sprintf("refusing to overwrite %s -- set MATCH_OVERWRITE=1 to replace it", path), call. = FALSE)
  invisible(TRUE)
}
# args:
#   x      data.table/data.frame to write as parquet (this pipeline writes no CSVs)
#   path   destination .parquet
#   guard  TRUE = refuse to clobber unless MATCH_OVERWRITE=1. Set FALSE only for files the
#          caller has already decided to replace (e.g. per-batch shards).
write_tab <- function(x, path, guard = TRUE) {
  if (guard) match_guard_overwrite(path)
  arrow::write_parquet(as.data.frame(x), path)
  invisible(path)
}
# args:  path = .parquet written by write_tab().
# Errors with a 'run the previous stage first' hint if absent, rather than a bare arrow error.
read_tab <- function(path) {
  if (!file.exists(path)) stop("missing input: ", path, "\n  (run the previous stage first)", call. = FALSE)
  as.data.table(arrow::read_parquet(path))
}
# args:
#   x      any R object (used here for small lists/tables: events, match table, reports)
#   path   destination .rds, gzip-compressed
#   guard  as write_tab(): TRUE refuses to clobber unless MATCH_OVERWRITE=1
write_obj <- function(x, path, guard = TRUE) {
  if (guard) match_guard_overwrite(path)
  saveRDS(x, path, compress = "gzip")
  invisible(path)
}
# args:  path = .rds written by write_obj(). Same missing-input error as read_tab().
read_obj <- function(path) {
  if (!file.exists(path)) stop("missing input: ", path, "\n  (run the previous stage first)", call. = FALSE)
  readRDS(path)
}

# ---- small shared utilities --------------------------------------------------------------------------
# Zero-pad to the canonical 8-digit CVR, returning NA for anything that is not 8 digits after stripping
# non-numerics. Every stage keys on this, so it lives in exactly one place.
# args:  x = anything CVR-ish (character with punctuation/spaces, integer that lost its
#        leading zeros, factor). Non-numerics are stripped, then zero-padded to 8.
# returns: character vector; NA for anything that is not 8 digits after cleaning.
as_cvr8 <- function(x) {
  y <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(x))))
  out <- ifelse(is.na(y), NA_character_, sprintf("%08d", y))
  out[!is.na(out) & !grepl("^[0-9]{8}$", out)] <- NA_character_
  out
}
# DB07 branchekode is hierarchical: leading digits give coarser levels. 2-digit division.
# args:  x = a DB07 branchekode (numeric or character, up to 6 digits).
# returns: the 2-digit division as character, NA in / NA out.
divof <- function(x) {
  xi <- suppressWarnings(as.integer(x))
  ifelse(is.na(xi), NA_character_, substr(sprintf("%06d", xi), 1L, 2L))
}
# Quarter index: a single integer ordering of (year, quarter) so event time is plain subtraction.
# args:  year, quarter = integer-ish vectors (recycled together).
# returns: INTEGER quarter index. The integer type matters -- pool is keyed on it, and a double
# key would break the binary-search join in process_group().
qidx_of <- function(year, quarter) as.integer(year) * 4L + as.integer(quarter)

# Attach the DB07 hierarchy (6 -> class 4 -> group 3 -> division 2) from a raw industry code column.
# args:
#   dt   data.table, MODIFIED BY REFERENCE (gains industry_code6/_class/_group/_division)
#   col  name of the raw DB07 code column to derive from
# returns: dt invisibly-ish (via dt[]), for chaining.
add_industry_hierarchy <- function(dt, col = "industry_code") {
  stopifnot(is.data.table(dt), col %in% names(dt))
  dt[, industry_code6 := {
    ii <- suppressWarnings(as.integer(get(col)))
    fifelse(is.na(ii), NA_character_, sprintf("%06d", ii))
  }]
  dt[, `:=`(industry_class    = substr(industry_code6, 1L, 4L),
            industry_group    = substr(industry_code6, 1L, 3L),
            industry_division = substr(industry_code6, 1L, 2L))]
  dt[]
}

# Positive-employment test. Reading `fte` ALONE is WRONG on this panel: post-2019 rows come from the
# derived_new_monthly arm, which populates `employees` and leaves `fte` NA/0 on ~23% of rows (2020:
# 479,016 of 628,787 rows passed the fte-only test, vs 627,451 under `either` -- both measured when the
# threshold was >= 1, so the counts shift slightly at > 0, but the ~23% cliff does not). Defaulting to
# `either` removes an artefactual cliff sitting exactly in the era that holds most events.
# args:
#   dt    data.table with an `fte` and/or `employees` column (missing ones count as FALSE)
#   rule  "either" (default) = fte>0 OR employees>0; "fte" = fte only; "employees" = only that.
#         Strictly greater than zero, matching the match-eligibility screen in 2_match_controls.R and
#         stage 3's fte > MATCH_MIN_FTE. One threshold across the pipeline, on purpose -- this used to
#         be >= 1 here, which quietly excluded sub-1-FTE firms from the control pool that the later
#         screens would have admitted.
#         "fte" alone drops ~23% of post-2019 rows, where the derived_new_monthly arm fills
#         `employees` and leaves `fte` NA -- see the note above.
# returns: logical vector, one per row of dt.
emp_positive <- function(dt, rule = match_env_chr("MATCH_EMP_RULE", "either")) {
  has_fte <- "fte" %in% names(dt); has_emp <- "employees" %in% names(dt)
  f <- if (has_fte) !is.na(dt$fte) & dt$fte > 0 else rep(FALSE, nrow(dt))
  e <- if (has_emp) !is.na(dt$employees) & dt$employees > 0 else rep(FALSE, nrow(dt))
  switch(rule,
         either    = f | e,
         fte       = f,
         employees = e,
         stop("MATCH_EMP_RULE must be one of: either, fte, employees (got '", rule, "')", call. = FALSE))
}

# ---- event provenance --------------------------------------------------------------------------------
# Which PROCUREMENT an event is, carried from the combined dataset to the regression data so a matched
# event joins to a lot-level design -- the co-bidder panel above all, whose stack_id is .GRP by
# (tender_id, lot_id) -- by identifier rather than by (winner cvr, event quarter). A timing join cannot
# say WHICH lot of a quarter a stack is.
#
# An event is (cvr, event_qidx), one award quarter, and can span several lots. These describe the
# EARLIEST award in the quarter, which is also the row that sets first_award_date -- so provenance and
# event timing always come from the same lot. Ties break on (data_source, tender_id, lot_id).
#   event_data_source    KFST | OpenTender | TED
#   event_tender_id      source tender id  (NOT unique across sources: pair with data_source)
#   event_lot_id         source lot id     (ditto)
#   event_ted_notice_id  the contract-award notice. The only identifier byte-identical ACROSS sources
#                        (98_final_data_checks.R check 6b), so it is the key for a cross-source join.
#   event_lot_key        "data_source|tender_id|lot_id" -- the composite that IS unique, pre-pasted.
EVENT_META_COLS <- c("event_data_source", "event_tender_id", "event_lot_id",
                     "event_ted_notice_id", "event_lot_key")
EVENT_META_SRC  <- c("data_source", "tender_id", "lot_id", "ted_notice_id")

# ---- the winner universe ------------------------------------------------------------------------------
# Events come from the COMBINED delivered dataset, not the per-source winner files -- those disagree
# (find_control_firms_never_winners.R used OT+KFST only, missing ~20% of winning firms).
#
# THE EVENT GRAIN IS THE QUARTER, not the calendar year. It used to be (cvr, award_year), which threw
# away every award quarter after a firm's first in a year -- those events were not merged, they were
# DISCARDED, and the loss is what 14_estudy_matched_vs_cobidder_validation.Rmd's `leak` table counts.
# Two consequences beyond the larger sample, both of them fixes:
#   - the sometimes-winner buffer becomes complete. 2_match_controls.R builds award_idx from these
#     rows, so under the year grain a firm with a Q1 and a Q3 award was INVISIBLE to the buffer in Q3
#     and could be offered as a clean control during a quarter it had actually won in.
#   - a firm can now be treated more than once, and two of its events can sit close enough that each
#     is inside the other's study window. The firm fixed effect pools them; it does not separate them.
#
# Two DIFFERENT sets come out of here and the distinction matters:
#   $events   competitive award events we study: entity==winner & flag_awarded & build_prod
#             & direct_award==FALSE, one row per (cvr, event_qidx) -- i.e. per award QUARTER.
#   $winners  the EXCLUSION set barring a firm from the never-winner control pool. Built from
#             COMPETITIVE winner rows only, so a firm whose wins were all DIRECT awards is NOT
#             excluded and pools with the never-winners (a deliberate choice: in this pipeline
#             "never-winner" means "never won competitively", not "absent from procurement data").
#
# direct_award has NAs. The default rule is strict (`%in% FALSE`, known-competitive only); set
# MATCH_DIRECT_AWARD_RULE=loose to keep NAs via !(direct_award %in% TRUE).
# args:
#   tender_stem  path STEM (no extension) of the combined dataset; read_clean() picks
#                .parquet/.rds/.csv in that order. Override with MATCH_TENDER_FILE.
#   da_rule      "strict" (default) keeps only direct_award == FALSE, i.e. known-competitive.
#                "loose" also keeps NAs via !(direct_award %in% TRUE). direct_award has NAs in
#                the combined table, so this choice moves the event count -- the split is printed.
#   verbose      TRUE prints the funnel (rows -> competitive -> dated -> events) as it narrows.
# returns: list with
#   $events   one row per (cvr, event_qidx) COMPETITIVE event -- one per award QUARTER -- with
#             first_award_date, award_year, award_quarter, the EVENT_META_COLS provenance columns,
#             and a deterministic `ev` id (ordered by cvr, event_qidx)
#   $winners  the EXCLUSION set: competitive winners only. A firm whose wins were all DIRECT
#             awards is absent here, so it stays available as a never-winner control.
winner_universe <- function(tender_stem = match_env_chr("MATCH_TENDER_FILE",
                                                        file.path(dirs$clean_data, "tender_data_2006_2026")),
                            da_rule = match_env_chr("MATCH_DIRECT_AWARD_RULE", "strict"),
                            verbose = TRUE) {
  d <- as.data.table(read_clean(tender_stem))
  w <- d[entity == "winner" & flag_awarded %in% TRUE & build_prod %in% TRUE]
  w[, cvr := as_cvr8(cvr_final)]
  w <- w[!is.na(cvr)]

  da <- table(factor(ifelse(is.na(w$direct_award), "NA", ifelse(w$direct_award, "TRUE", "FALSE")),
                     levels = c("FALSE", "TRUE", "NA")))
  keep <- if (identical(da_rule, "loose")) !(w$direct_award %in% TRUE) else (w$direct_award %in% FALSE)
  comp <- w[keep]

  winners <- sort(unique(comp$cvr))              # exclusion set: competitive winners only
  comp[, award_date := as.Date(award_date)]
  dated <- comp[!is.na(award_date)]

  # ---- event provenance (see EVENT_META_COLS) -------------------------------------------------------
  for (cc in EVENT_META_SRC) if (!cc %in% names(dated)) dated[, (cc) := NA_character_]
  # "" is missing, not a value: tender_id / lot_id are blank on some rows and a blank that survives as
  # an empty string is a join key that silently matches every other blank.
  for (cc in EVENT_META_SRC) {
    v <- as.character(dated[[cc]])
    v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
    set(dated, j = cc, value = v)
  }
  dated[, event_qidx := qidx_of(year(award_date), quarter(award_date))]
  # NA, not "KFST|NA|NA": a composite built from a missing part is not a usable key, and paste() would
  # manufacture one that looks fine and joins wrongly.
  dated[, lot_key := fifelse(is.na(tender_id) | is.na(lot_id), NA_character_,
                             paste(data_source, tender_id, lot_id, sep = "|"))]

  # Sorted first, so [1L] IS the earliest award in the quarter and every provenance column comes from
  # that one row -- no second pass, no merge, and the ids cannot describe a different lot from the date.
  # The (data_source, tender_id, lot_id) tail of the sort makes the pick deterministic when two lots
  # share the earliest date; which one wins does not matter for a quarterly event study, only that the
  # same one wins every run.
  setorderv(dated, c("cvr", "event_qidx", "award_date", "data_source", "tender_id", "lot_id"))
  events <- dated[, .(first_award_date    = award_date[1L],
                      n_awards_in_quarter = .N,
                      data_sources        = paste(sort(unique(data_source)), collapse = "|"),
                      event_data_source   = data_source[1L],
                      event_tender_id     = tender_id[1L],
                      event_lot_id        = lot_id[1L],
                      event_ted_notice_id = ted_notice_id[1L],
                      event_lot_key       = lot_key[1L]),
                  by = .(cvr, event_qidx)]
  events[, `:=`(award_year = year(first_award_date), award_quarter = quarter(first_award_date))]
  stopifnot(events[, all(event_qidx == qidx_of(award_year, award_quarter))])
  setorder(events, cvr, event_qidx)              # deterministic ev regardless of join order
  events[, ev := .I]

  if (verbose) {
    cat(sprintf("  winner rows (awarded & production)      : %d  | cvrs %d\n", nrow(w), uniqueN(w$cvr)))
    cat(sprintf("  direct_award  FALSE %d | TRUE %d | NA %d  -> rule '%s'\n",
                da[["FALSE"]], da[["TRUE"]], da[["NA"]], da_rule))
    cat(sprintf("  competitive rows                        : %d  | cvrs %d\n", nrow(comp), length(winners)))
    cat(sprintf("  dated rows                              : %d  | cvrs %d\n", nrow(dated), uniqueN(dated$cvr)))
    cat(sprintf("  events (cvr x award QUARTER)            : %d  | %d-%d\n",
                nrow(events), min(events$award_year), max(events$award_year)))
    cat(sprintf("  events 2019+                            : %d (%.1f%%)\n",
                events[award_year >= 2019, .N], 100 * events[award_year >= 2019, .N] / nrow(events)))
  }
  list(events = events[], winners = winners, n_winner_rows = nrow(w), direct_award_counts = da)
}

# ---- the firm registry ---------------------------------------------------------------------------------
# Current-snapshot sector + HQ kommune for ~2.27M firms. Used for BOTH arms so the screen is symmetric:
# it covers 95% of winners and 100% of the control pool, where the annual Virk snapshot covers only 70%
# of winners. Caveat to keep in mind: these attributes are time-INVARIANT, while the employment panel
# carries time-varying industry/kommune. Symmetry and coverage beat point-in-time accuracy for a screen.
# args:  path = the CVR name key (written by code/processing/1_3_process_keys.R).
# returns: one row per firm, keyed on cvr: cvr, sector6, kommune, division. The source has many
#          rows per firm (name history), so this collapses with unique(by="cvr") -- keeping the
#          FIRST row, which assumes sector/kommune are constant within a CVR.
firm_registry <- function(path = file.path(dirs$clean_data, "clean_cvr_name_key.rds")) {
  if (!file.exists(path))
    stop("the CVR registry is missing:\n  ", path,
         "\n  It is written by code/processing/1_3_process_keys.R (from the raw lookup that",
         "\n  code/processing/0_build_cvr_lookup.R scrapes). Run the processing pipeline first.",
         call. = FALSE)
  key <- as.data.table(readRDS(path))
  reg <- unique(key[, .(cvr     = as_cvr8(cvr),
                        sector6 = suppressWarnings(as.integer(hovedbranche_code)),
                        kommune = as.character(hq_kommune_code))], by = "cvr")
  rm(key); invisible(gc())
  reg <- reg[!is.na(cvr)]
  reg[, division := divof(sector6)]
  setkey(reg, cvr)
  reg[]
}

# ---- match protocols (eligibility window x ranking window) ----------------------------------------------
# Two windows that the original code conflated:
#   elig  the pre-period quarters a control must SURVIVE (complete, positive, computable gap)
#   rank  the pre-period quarters that SCORE it
# Ranking on the early pre-period (full_firsthalf) leaves [-h/2, -1] unused by the match, so the quarters
# just before the event stay an honest pre-trend check instead of something matching flattened by
# construction. Each entry maps h -> an integer vector of event-time offsets (negative = pre-period).
#
# Because eligibility demands a complete series over all of [-h,-1], every retained control has n_pre==h,
# so "first half" is unambiguous -- no ragged-series edge case. floor(h/2) for odd h.
#
# SCORE is the third field: how a candidate's distance from the treated firm is measured over the
# rank window. Both are RMS over the ranked quarters, so both are scale-free and comparable across events:
#   level  sqrt(mean((fte_c - fte_t)^2)) / mean(fte_t)    -- the original criterion
#   log    sqrt(mean((log fte_c - log fte_t)^2))          -- proportional gap per quarter
# The level score penalises a too-big control more than an equally-proportional too-small one (0.8x
# scores 0.20, 1.25x scores 0.25), which tilts selection toward smaller firms in a right-skewed pool.
# The log score is symmetric (0.8x and 1.25x both score 0.22) and weights every quarter equally.
MATCH_PROTOCOLS <- list(
  full_full = list(
    label = "elig [-h,-1], rank [-h,-1], level score",
    score = "level",
    elig  = function(h) seq.int(-h, -1L),
    rank  = function(h) seq.int(-h, -1L)),
  full_firsthalf = list(
    label = "elig [-h,-1], rank [-h,-(floor(h/2)+1)], level score",
    score = "level",
    elig  = function(h) seq.int(-h, -1L),
    rank  = function(h) seq.int(-h, -(floor(h / 2) + 1L))),
  full_lasthalf = list(
    label = "elig [-h,-1], rank [-floor(h/2),-1], level score",
    score = "level",
    elig  = function(h) seq.int(-h, -1L),
    rank  = function(h) seq.int(-floor(h / 2), -1L)),
  full_full_log = list(
    label = "elig [-h,-1], rank [-h,-1], log score",
    score = "log",
    elig  = function(h) seq.int(-h, -1L),
    rank  = function(h) seq.int(-h, -1L)),
  full_firsthalf_log = list(
    label = "elig [-h,-1], rank [-h,-(floor(h/2)+1)], log score",
    score = "log",
    elig  = function(h) seq.int(-h, -1L),
    rank  = function(h) seq.int(-h, -(floor(h / 2) + 1L)))
)
# Which protocols a run uses. Default ships both windows under both scores; full_lasthalf is a foil.
match_protocols <- function() {
  nm <- match_env_list("MATCH_PROTOCOLS", "full_full full_firsthalf full_full_log full_firsthalf_log")
  bad <- setdiff(nm, names(MATCH_PROTOCOLS))
  if (length(bad)) stop("unknown MATCH_PROTOCOLS: ", paste(bad, collapse = ", "),
                        "\n  available: ", paste(names(MATCH_PROTOCOLS), collapse = ", "), call. = FALSE)
  nm
}

# ---- cascade rules --------------------------------------------------------------------------------------
# The rungs, fine -> coarse. Each is (industry column, hold kommune?, label). A rung fires only if it
# yields >= MATCH_MIN_RUNG candidates (default 5) -- the original code broke at the first NON-EMPTY rung,
# so an event could "match" on industry6+kommune against a single candidate that then won rank 1
# uncontested. The LAST rung keeps a >0 floor: there is nothing coarser to fall through to.
cascade_staggered_industry_kommune <- function() list(
  list(col = "industry_code6",    komm = TRUE,  label = "industry6, kommune"),
  list(col = "industry_class",    komm = TRUE,  label = "industry4 (class), kommune"),
  list(col = "industry_group",    komm = TRUE,  label = "industry3 (group), kommune"),
  list(col = "industry_division", komm = TRUE,  label = "industry2 (division), kommune"),
  list(col = "industry_division", komm = FALSE, label = "industry2 (division)"),
  list(col = NULL,                komm = FALSE, label = "any industry")
)
MATCH_RULES <- list(staggered_industry_kommune = cascade_staggered_industry_kommune)
match_rules <- function() {
  nm <- match_env_list("MATCH_RULES", "staggered_industry_kommune")
  bad <- setdiff(nm, names(MATCH_RULES))
  if (length(bad)) stop("unknown MATCH_RULES: ", paste(bad, collapse = ", "),
                        "\n  available: ", paste(names(MATCH_RULES), collapse = ", "), call. = FALSE)
  nm
}

# ---- Virk pull worker ------------------------------------------------------------------------------------
# Refactored out of code/scraping/employment_controls_full.R (STEP 2+3) rather than duplicated, with one
# change: shards are PARQUET, not CSV. The one-hit design is unchanged -- the filtered query also requests
# the full employment _source, so a CVR that passes the server-side ever-employed filter comes back WITH
# its series; no second round-trip.
#
# Shards are the durable copy: a status shard is written LAST, and its presence marks a batch complete, so
# an interrupted run resumes by skipping finished batches and loses nothing.
virk_api <- function() {
  cred <- get_virk_credentials()
  list(cred        = cred,
       company_url = "http://distribution.virk.dk/cvr-permanent/virksomhed/_search",
       pu_url      = "http://distribution.virk.dk/cvr-permanent/produktionsenhed/_search",
       scroll_url  = "http://distribution.virk.dk/_search/scroll")
}

# args:
#   api        the list from virk_api() (credentials + endpoint urls)
#   url        which endpoint to POST to (api$company_url / api$pu_url / api$scroll_url)
#   body       request body as an R list; serialised with auto_unbox
#   params     optional query string params, e.g. list(scroll = "5m") to open a scroll cursor
#   max_tries  retry budget. 429/502/503/504 and connection errors back off exponentially
#              (capped at 30s); any other HTTP status errors immediately.
# returns: the parsed JSON as a nested list (simplifyVector = FALSE).
virk_post <- function(api, url, body, params = NULL, max_tries = 6L) {
  args <- list(url, httr::authenticate(api$cred$user, api$cred$password), httr::content_type_json(),
               body = jsonlite::toJSON(body, auto_unbox = TRUE), httr::timeout(120))
  if (!is.null(params)) args$query <- params
  for (attempt in seq_len(max_tries)) {
    r <- tryCatch(do.call(httr::POST, args), error = function(e) e)
    if (inherits(r, "error")) {
      if (attempt == max_tries) stop(conditionMessage(r))
      Sys.sleep(min(2^attempt, 30)); next
    }
    ct <- httr::content(r, as = "text", encoding = "UTF-8")
    if (httr::status_code(r) == 200L) return(jsonlite::fromJSON(ct, simplifyVector = FALSE))
    if (httr::status_code(r) %in% c(429L, 502L, 503L, 504L) && attempt < max_tries) {
      Sys.sleep(min(2^attempt, 30)); next
    }
    stop("Virk HTTP ", httr::status_code(r), ": ", substr(ct, 1, 200))
  }
}

# args:  path = the nested Elasticsearch field, e.g. "Vrvirksomhed.aarsbeskaeftigelse".
# returns: a query fragment matching docs with >=1 employee in that nested array.
.nested_ge1 <- function(path) list(nested = list(
  path = path, query = list(range = setNames(list(list(gte = 1)), paste0(path, ".antalAnsatte")))))
.comp_emp_filter <- function() list(bool = list(minimum_should_match = 1, should = list(
  .nested_ge1("Vrvirksomhed.aarsbeskaeftigelse"),
  .nested_ge1("Vrvirksomhed.kvartalsbeskaeftigelse"),
  .nested_ge1("Vrvirksomhed.maanedsbeskaeftigelse"))))
.pu_emp_filter <- function() .nested_ge1("VrproduktionsEnhed.erstMaanedsbeskaeftigelse")
.company_fields <- function() c(
  "Vrvirksomhed.cvrNummer", "Vrvirksomhed.virksomhedMetadata.nyesteNavn", "Vrvirksomhed.navne",
  "Vrvirksomhed.binavne", "Vrvirksomhed.stiftelsesDato", "Vrvirksomhed.livsforloeb",
  "Vrvirksomhed.status", "Vrvirksomhed.virksomhedsstatus", "Vrvirksomhed.aarsbeskaeftigelse",
  "Vrvirksomhed.kvartalsbeskaeftigelse", "Vrvirksomhed.maanedsbeskaeftigelse",
  "Vrvirksomhed.virksomhedsform", "Vrvirksomhed.hovedbranche", "Vrvirksomhed.bibranche1",
  "Vrvirksomhed.bibranche2", "Vrvirksomhed.bibranche3", "Vrvirksomhed.beliggenhedsadresse",
  "Vrvirksomhed.virksomhedMetadata.nyesteBeliggenhedsadresse", "Vrvirksomhed.attributter")
.pu_fields <- function() c("VrproduktionsEnhed.pNummer", "VrproduktionsEnhed.virksomhedsrelation",
                           "VrproduktionsEnhed.erstMaanedsbeskaeftigelse")
# args:  res = a parsed Virk search response.
# Handles both ES response shapes (total as a scalar, or as a {value, relation} object).
.hits_total <- function(res) { t <- res$hits$total; as.integer(if (is.list(t)) t$value else t) }

# One batch: company (filtered, one hit) + production units (filtered, scrolled) -> spliced panel shard.
# args:
#   api         from virk_api()
#   cvr_batch   character vector of CVRs for THIS batch (typically 500)
#   bid         zero-padded batch id, e.g. "000007" -- becomes the shard filename suffix
#   shard_dir   where the three parquet shards are written
#   pu_scroll   production-unit scroll page size; larger destabilises the scroll on nested payloads
#   schema_tag  string recorded in the status shard, so a later reader knows which pull
#               schema produced these rows
# returns: a one-row summary (bid, firms, pus, rows, recent_only). The STATUS shard is written
#          LAST on purpose: its presence is what marks the batch complete for resume.
virk_pull_batch <- function(api, cvr_batch, bid, shard_dir, pu_scroll = 500L,
                            schema_tag = "spliced_production_units_v2_location") {
  emp_sh <- file.path(shard_dir, paste0("emp_",    bid, ".parquet"))
  nm_sh  <- file.path(shard_dir, paste0("name_",   bid, ".parquet"))
  st_sh  <- file.path(shard_dir, paste0("status_", bid, ".parquet"))

  res <- virk_post(api, api$company_url, list(
    size = length(cvr_batch), `_source` = .company_fields(),
    query = list(bool = list(must = list(
      list(terms = setNames(list(I(as.integer(cvr_batch))), "Vrvirksomhed.cvrNummer")),
      .comp_emp_filter())))))
  firms_emp <- lapply(res$hits$hits, function(h) h$`_source`$Vrvirksomhed)
  returned_cvrs <- vapply(firms_emp, function(f) format_virk_cvr(f$cvrNummer), character(1))

  pres <- virk_post(api, api$pu_url, list(
    size = pu_scroll, sort = list("_doc"), `_source` = .pu_fields(),
    query = list(bool = list(must = list(
      list(terms = setNames(list(I(as.integer(cvr_batch))),
                            "VrproduktionsEnhed.virksomhedsrelation.cvrNummer")),
      .pu_emp_filter())))), params = list(scroll = "5m"))
  all_hits <- pres$hits$hits; total <- .hits_total(pres); sid <- pres$`_scroll_id`
  while (length(all_hits) < total) {
    r <- virk_post(api, api$scroll_url, list(scroll = "5m", scroll_id = sid))
    h <- r$hits$hits
    if (length(h) == 0) break
    all_hits <- c(all_hits, h); sid <- r$`_scroll_id`
  }
  punits <- lapply(all_hits, function(h) h$`_source`$VrproduktionsEnhed)

  new_monthly <- aggregate_production_unit_monthly(punits)
  if (nrow(new_monthly) > 0) new_monthly <- new_monthly[cvr %chin% cvr_batch]
  recent_only <- setdiff(unique(new_monthly$cvr), returned_cvrs)   # PU-employed, no company arrays
  firms_extra <- if (length(recent_only)) {
    r2 <- virk_post(api, api$company_url, list(
      size = length(recent_only), `_source` = .company_fields(),
      query = list(terms = setNames(list(I(as.integer(recent_only))), "Vrvirksomhed.cvrNummer"))))
    lapply(r2$hits$hits, function(h) h$`_source`$Vrvirksomhed)
  } else list()

  firms <- c(firms_emp, firms_extra)
  firms_by_cvr <- setNames(firms, vapply(firms, function(f) format_virk_cvr(f$cvrNummer), character(1)))
  historical <- if (length(firms) == 0) empty_employment_table() else
    rbindlist(lapply(firms, extract_virk_employment_history), use.names = TRUE, fill = TRUE)
  new_rows <- build_new_monthly_rows(new_monthly, firms_by_cvr)
  native   <- collapse_employment_sources(rbindlist(list(historical, new_rows), use.names = TRUE, fill = TRUE))
  emp      <- add_spliced_frequencies(add_derived_frequencies(native, firms_by_cvr))
  if (nrow(emp)) setorder(emp, cvr, frequency, year, quarter, month)
  names_dt <- if (length(firms) == 0) empty_name_history_table() else
    rbindlist(lapply(firms, extract_name_history), use.names = TRUE, fill = TRUE)

  status <- data.table(cvr = cvr_batch,
                       found_in_virk             = cvr_batch %chin% returned_cvrs,
                       found_in_production_units = cvr_batch %chin% unique(new_monthly$cvr),
                       recent_only               = cvr_batch %chin% recent_only,
                       pulled_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       employment_pull_schema = schema_tag)
  # status LAST: its presence is the completion marker the resume logic keys on
  arrow::write_parquet(as.data.frame(emp),      emp_sh)
  arrow::write_parquet(as.data.frame(names_dt), nm_sh)
  arrow::write_parquet(as.data.frame(status),   st_sh)
  data.table(bid = bid, firms = length(firms), pus = length(punits),
             rows = nrow(emp), recent_only = length(recent_only))
}

# Pull a CVR vector in resumable batches. Returns the per-batch summary; shards land in `shard_dir`.
# args:
#   cvrs        CVRs to pull; deduped and sorted internally, so caller order does not matter
#   shard_dir   shard directory. Batches that already have a status shard are SKIPPED, which is
#               what makes an interrupted run resumable -- point at the same dir to continue.
#   batch_size  CVRs per API call (MATCH_BATCH_SIZE, default 500)
#   n_cores     parallel workers (MATCH_CORES, default detectCores()-2)
#   pu_scroll   passed through to virk_pull_batch()
# returns: rbind of the per-batch summaries; a failed batch contributes an `error` column instead.
virk_pull_cvrs <- function(cvrs, shard_dir,
                           batch_size = match_env_int("MATCH_BATCH_SIZE", 500L),
                           n_cores    = match_env_int("MATCH_CORES", max(1L, parallel::detectCores() - 2L)),
                           pu_scroll  = match_env_int("MATCH_PU_SCROLL", 500L)) {
  cvrs <- sort(unique(cvrs[!is.na(cvrs)]))
  dir.create(shard_dir, recursive = TRUE, showWarnings = FALSE)
  if (!length(cvrs)) { cat("  nothing to pull.\n"); return(data.table()) }

  batches <- split(cvrs, ceiling(seq_along(cvrs) / batch_size))
  names(batches) <- sprintf("%06d", seq_along(batches))
  todo <- names(batches)[!file.exists(file.path(shard_dir, paste0("status_", names(batches), ".parquet")))]
  cat(sprintf("  %d CVRs -> %d batches of %d | %d remaining | %d workers\n",
              length(cvrs), length(batches), batch_size, length(todo), n_cores))
  if (!length(todo)) { cat("  all batches already have shards.\n"); return(data.table()) }

  # macOS kills a forked child that touches the Objective-C runtime, and httr/curl drag it into the
  # parent ("+[NSNumber initialize] may have been in progress ... Crashing instead"). The children die
  # WITHOUT raising an R error, so mclapply reports "ok 0 | errors 0" and the pull silently fetches
  # nothing. Three defences, in order of preference:
  #   1. set the documented escape hatch before forking
  #   2. do not fork at all for a handful of batches -- the pull is usually ~2 batches, where the fork
  #      buys nothing and only risks this crash
  #   3. if the forks deliver nothing anyway, retry serially rather than reporting a phantom success
  if (Sys.info()[["sysname"]] == "Darwin")
    Sys.setenv(OBJC_DISABLE_INITIALIZE_FORK_SAFETY = "YES")

  api <- virk_api()
  run_batch <- function(bid) tryCatch(
    virk_pull_batch(api, batches[[bid]], bid, shard_dir, pu_scroll),
    error = function(e) data.table(bid = bid, error = conditionMessage(e)))

  serial_floor <- match_env_int("MATCH_PULL_SERIAL_MAX", 4L)
  eff_cores <- if (length(todo) <= serial_floor) 1L else n_cores
  if (eff_cores == 1L && n_cores > 1L)
    cat(sprintf("  %d batch(es) only -- running serially (no fork)\n", length(todo)))

  timed <- system.time({
    res <- if (eff_cores == 1L) lapply(todo, run_batch)
           else parallel::mclapply(todo, run_batch, mc.cores = eff_cores, mc.preschedule = FALSE)
  })
  # a fork-killed child yields NULL, not an error row -- count it as a failure, never as a success
  delivered <- vapply(res, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))
  if (!any(delivered) && length(todo) > 0L && eff_cores > 1L) {
    cat("  parallel pull delivered nothing (forked workers died) -- retrying serially\n")
    timed <- system.time({ res <- lapply(todo, run_batch) })
    delivered <- vapply(res, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))
  }
  summ <- rbindlist(res[delivered], use.names = TRUE, fill = TRUE)
  errs <- if ("error" %in% names(summ)) summ[!is.na(error)] else summ[0]
  n_lost <- sum(!delivered)
  cat(sprintf("  pull done in %.1f min | ok %d | errors %d | undelivered %d\n",
              timed[["elapsed"]] / 60, max(0L, nrow(summ) - nrow(errs)), nrow(errs), n_lost))
  if (nrow(errs)) { cat("  FAILED batches (rerun to resume):\n"); print(utils::head(errs, 20)) }
  if (n_lost > 0L)
    warning(sprintf("%d of %d pull batches delivered no result -- rerun to resume those batches",
                    n_lost, length(todo)), call. = FALSE)
  summ[]
}

# Read every completed shard of one kind back as a single table (empty table if there are none).
# args:  shard_dir = directory to scan; prefix = "emp", "name" or "status".
# returns: all matching shards stacked; an EMPTY data.table if there are none, so callers can
#          test nrow() rather than handling NULL.
read_shards <- function(shard_dir, prefix) {
  fs <- Sys.glob(file.path(shard_dir, paste0(prefix, "_*.parquet")))
  if (!length(fs)) return(data.table())
  rbindlist(lapply(fs, function(f) as.data.table(arrow::read_parquet(f))), use.names = TRUE, fill = TRUE)
}

# args:
#   protocol  optional scoring_protocol filter ("full_full", "full_firsthalf"); NULL = all
#   arm       optional arm filter ("never_winner", "winner"); NULL = all
#   tag       artefact tag; "" = the full run, "test2000" = that dry run
# returns: the EXACT rows stage 4 estimated on -- after the qscore cutoff and with stage 4's weights, so
#          weighted.mean(fte, weight) reproduces the figures and feols(...) reproduces the estimates.
#          Note this differs from 03_reg_data_h*.parquet, which is PRE-cutoff and carries no weights.
#
#   d <- load_estimation_panel("full_full", "never_winner")
#   d[, .(m = sum(weight * fte) / sum(weight)), by = .(event_time, treatment)]
load_estimation_panel <- function(protocol = NULL, arm = NULL, tag = match_tag()) {
  d <- read_tab(match_paths(tag)$est_panel)
  if (!is.null(protocol)) d <- d[scoring_protocol %chin% protocol]
  if (!is.null(arm))      { .a <- arm; d <- d[arm %chin% .a] }
  d[]
}

# args:
#   window  half-window as written by the .Rmd: 8 or 4 for quarters, 12 or 6 for months
#   unit    "quarters" (files estudy_wnw_h{8,4}.rds) or "months" (estudy_wnw_m{12,6}.rds)
#   harmonise  TRUE renames columns to match load_estimation_panel(), so the two control designs
#              can be compared directly. `entity` (winner/non-winner) becomes `treatment`
#              (treated/control), `tidx` becomes `qidx`, and `arm` is set to "non_winner_bidder".
# returns: the estimation panel from 13_estudy_winner_vs_nonwinner_matched.Rmd -- the design whose
#          controls are REAL losing bidders on the same TED lot, not synthetic matches. Carries
#          tender_id, lot_id, lot_key and ted_notice_id alongside the opaque stack_id, so a stack can
#          be traced to its procurement and joined to the matched design (see JOINING THE TWO below).
#          A panel saved before that change has none of them -- re-knit the .Rmd.
#
# The two designs answer different questions and this is the point of comparing them:
#   load_estimation_panel()   -> winner vs matched never-winner / sometimes-winner  (large, synthetic)
#   load_nonwinner_panel()    -> winner vs actual co-bidder on the same lot         (small, sharp)
#
# JOINING THE TWO. Both sides carry the lot's identifiers, built to the same recipe, so join on those
# and not on (winner cvr, event quarter) -- a timing join cannot say which lot of a quarter a stack is.
# Matched side: event_lot_key / event_ted_notice_id (EVENT_META_COLS), describing the earliest award of
# the event's quarter. Co-bidder side: lot_key / ted_notice_id, one lot per stack.
#
#   merge(unique(m[, .(ev, event_lot_key)]), unique(nw[, .(stack_id, lot_key)]),
#         by.x = "event_lot_key", by.y = "lot_key")
#
#   m  <- load_estimation_panel("full_full", "never_winner")
#   nw <- load_nonwinner_panel(8)
#   rbind(m[, .(design = "matched",  event_time, fte, weight, treatment)],
#         nw[, .(design = "cobidder", event_time, fte, weight, treatment)])
load_nonwinner_panel <- function(window = 8, unit = c("quarters", "months"), harmonise = TRUE) {
  unit <- match.arg(unit)
  f <- file.path(dirs$employment,
                 sprintf("estudy_wnw_%s%d.rds", if (unit == "quarters") "h" else "m", window))
  if (!file.exists(f))
    stop("missing: ", f, "\n  Knit or run code/analysis/13_estudy_winner_vs_nonwinner_matched.Rmd",
         " (it writes this when ESWNW_SAVE=1, the default).", call. = FALSE)
  x <- readRDS(f)
  if (is.null(x$panel)) stop(basename(f), " has no $panel -- it predates the panel-saving change.",
                             call. = FALSE)
  d <- as.data.table(x$panel)
  if (harmonise) {
    if ("entity" %in% names(d)) {
      d[, treatment := fifelse(entity == "winner", "treated", "control")]
      d[, treated := as.integer(entity == "winner")]
    }
    if ("tidx" %in% names(d) && !"qidx" %in% names(d)) setnames(d, "tidx", "qidx")
    d[, arm := "non_winner_bidder"]
    d[, scoring_protocol := NA_character_]
  }
  setattr(d, "window", x$window); setattr(d, "unit", x$unit); setattr(d, "run_at", x$run_at)
  d[]
}

# args:  x = numeric vector; w = weights of the same length, zero or NA wherever x is NA.
# returns: list(n, mean, sd). n counts non-missing x; mean and sd are weighted with divisor sum(w),
#          i.e. the biased weighted variance, not the reliability-corrected form.
# Shared so the balance statistic cannot drift between reports that both claim to compute it.
# NOTE 13_estudy_winner_vs_nonwinner_matched.Rmd still carries an inline copy in its `compare-wnw-tables`
# chunk; point that at this one next time the file is touched.
wstats <- function(x, w) {
  sw <- sum(w, na.rm = TRUE)
  mu <- sum(x * w, na.rm = TRUE) / sw
  list(n = sum(!is.na(x)), mean = mu,
       sd = sqrt(sum(w * (x - mu)^2, na.rm = TRUE) / sw))
}

# ---- reporting -------------------------------------------------------------------------------------------
# args:  title = section heading to print between rules. Cosmetic; keeps long logs scannable.
match_rule_banner <- function(title) {
  cat("\n", strrep("=", 100), "\n", title, "\n", strrep("=", 100), "\n", sep = "")
}
# Year-over-year continuity check on a firms-per-year table. A >15% break is a WARNING, not a stop, and it
# names the year: under MATCH_EMP_RULE=fte the artefactual 2019->2020 cliff trips this, which is the point.
# args:
#   by_year  data.table with `year` and `firms` columns; MODIFIED BY REFERENCE (gains `chg`)
#   thresh   fractional year-over-year change that triggers the warning
# Warns rather than stops, and names the offending years: under MATCH_EMP_RULE=fte the
# artefactual 2019->2020 cliff trips this, which is the signal, not a failure.
check_year_continuity <- function(by_year, thresh = 0.15) {
  setorder(by_year, year)
  by_year[, chg := c(NA_real_, diff(firms) / utils::head(firms, -1))]
  bad <- by_year[!is.na(chg) & abs(chg) > thresh]
  if (nrow(bad)) {
    warning(sprintf("firms/year breaks >%.0f%%: %s", 100 * thresh,
                    paste(sprintf("%d (%+.1f%%)", bad$year, 100 * bad$chg), collapse = ", ")),
            call. = FALSE)
  }
  by_year[]
}
