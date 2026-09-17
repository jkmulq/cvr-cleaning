#!/usr/bin/env Rscript
# Pull employment history for the firms in the event-study eligible-controls table(s)
# (matched_controls_*.rds from find_control_firms_never_winners.R): the treated winners plus ALL their
# eligible never-winner and sometimes-winner controls.
#
# Key point: employment is one series per DISTINCT firm. A firm that controls 50 events is fetched ONCE,
# so the pull is bounded by the number of distinct firms in the eligible table -- NOT by the (event x firm)
# cross-product. This script collects those distinct CVRs and hands them to employment_2_controls.R's Virk
# engine in TARGETED mode (CVR_EMPLOYMENT_TARGET_FILE), so there is no duplicated pull machinery. The pull
# is resumable (status ledger) and writes its own output file.
#
# Usage (from the repo root):
#   Rscript code/scraping/employment_3_event_firms.R
#
# Options (env):
#   CVR_EMPLOYMENT_EVENT_TABLES   glob for match tables    (default <employment>/matched_controls_*.rds)
#   CVR_EMPLOYMENT_EVENT_ARMS     control_type arms to keep, space/comma-sep (default: all arms)
#   CVR_EMPLOYMENT_ONLY_MISSING   "true" -> pull only firms not already recovered by employment_1/2
#                                 (default false: pull the full target set into a self-contained file)
#   CVR_EMPLOYMENT_OUTPUT_FILE    output RDS (default <employment>/cvr_employment_history_event_firms.rds)
#   (plus every employment_2 option: CVR_EMPLOYMENT_BATCH_SIZE, _SCROLL_SIZE, _OVERWRITE, ... )

rm(list = ls())

# Anchor paths off the project root (walk up to the .Rproj marker) so relative source() paths resolve
# regardless of the working directory.
report_project_dir <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(d, "cvr-cleaning.Rproj")) && dirname(d) != d) d <- dirname(d)
  d
})
setwd(report_project_dir)
source(file.path(report_project_dir, "config.R"))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
emp <- dirs$employment

# 1. Distinct target CVRs from the eligible-controls table(s). Skip resume checkpoints + test fixtures.
tables <- Sys.glob(Sys.getenv("CVR_EMPLOYMENT_EVENT_TABLES",
                              unset = file.path(emp, "matched_controls_*.rds")))
tables <- tables[!grepl("_rawlist|_test", tables)]
if (!length(tables)) stop("No matched_controls_*.rds found in ", emp, call. = FALSE)
mt <- rbindlist(lapply(tables, function(f) as.data.table(readRDS(f))[, .(cvr, control_type)]), fill = TRUE)

arms <- Sys.getenv("CVR_EMPLOYMENT_EVENT_ARMS")
if (nzchar(arms)) mt <- mt[control_type %chin% strsplit(arms, "[ ,]+")[[1]][nzchar(strsplit(arms, "[ ,]+")[[1]])]]

target <- sort(unique(sprintf("%08d", as.integer(mt$cvr))))
target <- target[grepl("^[0-9]{8}$", target)]
cat(sprintf("Eligible tables: %d  ->  %d distinct target firms\n", length(tables), length(target)))
print(mt[, .(distinct_firms = uniqueN(cvr)), by = control_type][order(-distinct_firms)])

# 2. Coverage estimate vs what employment_1 (winners) / employment_2 (controls) already pulled.
recovered <- function(status_path) {
  if (!file.exists(status_path)) return(character(0))
  s <- tryCatch(fread(status_path, colClasses = "character"), error = function(e) NULL)
  if (is.null(s) || !"cvr" %in% names(s)) return(character(0))
  unique(sprintf("%08d", as.integer(s$cvr)))
}
already <- unique(c(recovered(file.path(emp, "cvr_employment_history_virk_status.csv")),
                    recovered(file.path(emp, "cvr_employment_history_control_status.csv"))))
gap <- setdiff(target, already)
cat(sprintf("Already queried by employment_1/2: %d  |  genuinely NEW (gap): %d\n",
            length(intersect(target, already)), length(gap)))
batch <- as.integer(Sys.getenv("CVR_EMPLOYMENT_BATCH_SIZE", "1000"))
cat(sprintf("Company-endpoint batches: full=%d, gap-only=%d (batch=%d)\n",
            ceiling(length(target) / batch), ceiling(length(gap) / batch), batch))

# 3. Choose the pull set and write it for employment_2's targeted mode.
only_missing <- tolower(Sys.getenv("CVR_EMPLOYMENT_ONLY_MISSING", "false")) == "true"
to_pull <- if (only_missing) gap else target
target_file <- file.path(emp, "event_firm_cvrs.rds")
saveRDS(data.table(cvr = to_pull), target_file)
cat(sprintf("Wrote %d target CVRs (%s) -> %s\n",
            length(to_pull), if (only_missing) "gap only" else "full set", target_file))
if (!length(to_pull)) { cat("Nothing to pull -- all target firms already recovered.\n"); quit(save = "no", status = 0) }

# 4. Hand off to employment_2_controls.R in TARGETED mode. Sys.setenv values survive that script's
#    rm(list = ls()); it reads them, pulls the target list, and writes its own resumable output.
out_file <- Sys.getenv("CVR_EMPLOYMENT_OUTPUT_FILE",
                       unset = file.path(emp, "cvr_employment_history_event_firms.rds"))
Sys.setenv(CVR_EMPLOYMENT_TARGET_FILE = target_file,
           CVR_EMPLOYMENT_OUTPUT_FILE = out_file)
cat("Handing off to employment_2_controls.R (targeted mode) ->", out_file, "\n")
source(file.path("code", "scraping", "employment_2_controls.R"))
