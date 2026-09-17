#!/usr/bin/env Rscript
# Build a (windowed) stacked event-study panel ON DEMAND from:
#   - the compact match table produced by find_control_firms_never_winners.R
#       (matched_controls_h*_type*.rds): one row per (event, firm) = the treated firm + all its eligible
#       never-winner and sometimes-winner controls, flagged by control_type + control_protocol.
#   - the cached all-firms employment panel (firm_employment_panel_all.rds): one row per firm-quarter.
#
# The matching step deliberately stores only the LIST of controls; joining every (event x control) to its
# full series is ~100M+ rows. This helper joins the employment on and WINDOWS it, so you only ever
# materialise what a given analysis needs (a control reused across events is joined to each, tagged by
# stack_id). Source it (no side effects) and call build_event_study_panel().
#
# Running this file materialises the windowed long panel and SAVES it (via the run block at the bottom,
# configured with ESP_* env vars). build_event_study_panel() is defined first, so you can also call it
# directly on your own tables.
#
#   Rscript code/analysis/build_event_study_panel.R
# Options (env, all optional):
#   ESP_MATCH_FILE   match table    (default <employment>/matched_controls_h8_typestaggered.rds)
#   ESP_PANEL_FILE   firm panel     (default <employment>/firm_employment_panel_all.rds)
#   ESP_OUTPUT_FILE  output .rds    (default <employment>/event_study_panel_h8.rds)
#   ESP_WINDOW_LO / ESP_WINDOW_HI   event-time bounds in quarters (default -8 / 8)
#   ESP_FREQ         frequency variant to use (default quarterly_spliced)
#   ESP_CONTROL_TYPES  space/comma-sep arms to keep (default: treated never_winner winner)
#   ESP_CHUNK_EVENTS events per join chunk -- lower it if memory is tight (default 3000)
#
# Direct call after sourcing the function:
#   panel <- build_event_study_panel(mt, firm, window = c(-8, 8),
#                                    control_types = c("treated", "never_winner"))

suppressWarnings(suppressPackageStartupMessages(library(data.table)))

# match_tab      : the matched_controls_*.rds table (event x firm rows).
# firm_panel     : the firm_employment_panel_all.rds table (firm-quarter rows).
# window         : c(lo, hi) event_time bounds in quarters (inclusive); NULL = full series (large!).
# freq           : which frequency variant of the panel to use.
# control_types  : which arms to include ("treated" is the treated firm; keep it in for a usable panel).
build_event_study_panel <- function(match_tab,
                                     firm_panel,
                                     window = NULL,
                                     freq = "quarterly_spliced",
                                     control_types = c("treated", "never_winner", "winner"),
                                     chunk_events = 3000L) {
  match_tab  <- as.data.table(match_tab)
  firm_panel <- as.data.table(firm_panel)
  stopifnot(all(c("cvr", "event_qidx", "control_type", "stack_id") %in% names(match_tab)))
  stopifnot(all(c("cvr", "qidx", "frequency") %in% names(firm_panel)))

  mt <- match_tab[control_type %chin% control_types]
  fp <- firm_panel[frequency == freq]
  data.table::setkey(fp, cvr)

  # One row per (event x firm x quarter). The naive `fp[mt, on="cvr", allow.cartesian=TRUE]` materialises
  # the FULL cross (every event-firm x ALL of that firm's quarters) BEFORE windowing -- for ~1M event-firm
  # pairs x ~40 quarters that is tens of millions of transient rows and will OOM. Instead process the events
  # in chunks: for each chunk, join only the firms it needs, window immediately, and keep just the in-window
  # rows. Peak memory is one chunk's transient cross, not the whole thing; the result is identical.
  stack_ids <- unique(mt$stack_id)
  n_chunks  <- max(1L, ceiling(length(stack_ids) / chunk_events))
  groups    <- split(stack_ids, cut(seq_along(stack_ids), n_chunks, labels = FALSE))
  if (!is.null(window)) stopifnot(length(window) == 2L)

  parts <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    m <- mt[stack_id %in% groups[[i]]]
    f <- fp[cvr %chin% unique(m$cvr)]                        # only firms this chunk needs
    p <- f[m, on = "cvr", allow.cartesian = TRUE, nomatch = NULL]
    p[, event_time := qidx - event_qidx]
    if (!is.null(window)) p <- p[event_time %between% window]
    parts[[i]] <- p
  }

  panel <- data.table::rbindlist(parts, use.names = TRUE)
  panel[, flag_found_control := TRUE]
  setkey(panel, stack_id, cvr, qidx)
  panel[]
}

# ---- Materialise the windowed long panel and save it -----------------------------------------------
# Inputs/window/output are set via ESP_* env vars (see header).
# Anchor paths off the project root so this works from any working directory.
.proj <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(d, "cvr-cleaning.Rproj")) && dirname(d) != d) d <- dirname(d)
  d
})
setwd(.proj)
source(file.path(.proj, "config.R"))
emp <- dirs$employment

match_file <- Sys.getenv("ESP_MATCH_FILE",  unset = file.path(emp, "matched_controls_h8_typestaggered.rds"))
panel_file <- Sys.getenv("ESP_PANEL_FILE",  unset = file.path(emp, "firm_employment_panel_all.rds"))
out_file   <- Sys.getenv("ESP_OUTPUT_FILE", unset = file.path(emp, "event_study_panel_h8.rds"))
win_lo <- as.integer(Sys.getenv("ESP_WINDOW_LO", "-8"))
win_hi <- as.integer(Sys.getenv("ESP_WINDOW_HI",  "8"))
freq   <- Sys.getenv("ESP_FREQ", "quarterly_spliced")
ctypes <- Sys.getenv("ESP_CONTROL_TYPES", "treated never_winner winner")
ctypes <- strsplit(ctypes, "[ ,]+")[[1]]; ctypes <- ctypes[nzchar(ctypes)]
chunk_events <- as.integer(Sys.getenv("ESP_CHUNK_EVENTS", "3000"))  # lower this if memory is tight

cat(sprintf("Match table: %s\nFirm panel:  %s\nWindow: [%d, %d] | freq: %s | arms: %s | chunk_events: %d\n",
            match_file, panel_file, win_lo, win_hi, freq, paste(ctypes, collapse = ", "), chunk_events))
mt <- as.data.table(readRDS(match_file))
fp <- as.data.table(readRDS(panel_file))

panel <- build_event_study_panel(mt, fp, window = c(win_lo, win_hi), freq = freq,
                                 control_types = ctypes, chunk_events = 100)
rm(mt, fp); invisible(gc())   # free the inputs before the (large) save
cat(sprintf("\nPanel built: %d rows | %d events | %d distinct firms\n",
            nrow(panel), uniqueN(panel$stack_id), uniqueN(panel$cvr)))
print(panel[, .(rows = .N, firms = uniqueN(cvr)), by = control_type][order(-rows)])

saveRDS(panel, out_file)
cat(sprintf("Saved -> %s (%.0f MB)\n", out_file, file.size(out_file) / 1e6))
