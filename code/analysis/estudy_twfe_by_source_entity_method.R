#!/usr/bin/env Rscript
# Event studies, one per (data_source, entity, cvr_method) cell -- winners & buyers separate.
# ---------------------------------------------------------------------------------------------------
# Firm-fixed-effect event study of (log) quarterly-spliced employment around the firm's FIRST award, run
# SEPARATELY for every (data_source, entity, cvr_method) cell. NO matched controls (naive within-sample --
# biased under staggered timing; the rigorous design is estudy_winner_vs_nonwinner_matched.Rmd). FE = firm
# only: on a treated-only balanced panel, adding calendar-time FE makes event-time collinear (degenerate
# SEs), so firm FE + event-time dummies is the identified spec. Balanced panel: strictly positive FTE in
# each of the +/-8 event quarters. Estimation is ONE feols() call split by cell (fsplit) -- no loops/helpers.
#
#   Rscript code/analysis/estudy_twfe_by_source_entity_method.R
# Options (env): ETW_WINDOW (default 8), ETW_FREQ (quarterly_spliced), ETW_MIN_FIRMS (30),
#                ETW_MIN_FTE (0), ETW_ENTITIES (default "winner buyer").
# ---------------------------------------------------------------------------------------------------
suppressWarnings(suppressPackageStartupMessages({ library(data.table); library(fixest) }))

# ---- setup ----
project_dir <- normalizePath(getwd())
while (!file.exists(file.path(project_dir, "cvr-cleaning.Rproj")) && dirname(project_dir) != project_dir)
  project_dir <- dirname(project_dir)
setwd(project_dir)
source("config.R"); source("code/functions.R")
emp_dir <- dirs$employment

H         <- as.integer(Sys.getenv("ETW_WINDOW", "8"))
FREQ      <- Sys.getenv("ETW_FREQ", "quarterly_spliced")
MIN_FIRMS <- as.integer(Sys.getenv("ETW_MIN_FIRMS", "30"))
MIN_FTE   <- as.numeric(Sys.getenv("ETW_MIN_FTE", "0"))
ENTITIES  <- strsplit(trimws(Sys.getenv("ETW_ENTITIES", "winner buyer")), "[ ,]+")[[1]]

# ---- 1. firm quarterly employment panel (one row per cvr-quarter) ----
message("1. loading firm employment panel ...")
emp <- as.data.table(readRDS(file.path(emp_dir, "firm_employment_panel_all.rds")))
if (file.exists(file.path(emp_dir, "cvr_employment_history_nonwinner.rds")))
  emp <- rbind(emp, as.data.table(readRDS(file.path(emp_dir, "cvr_employment_history_nonwinner.rds"))),
               use.names = TRUE, fill = TRUE)
emp[, cvr := as.character(cvr)]
firm <- emp[frequency == FREQ & !is.na(year) & !is.na(quarter),
            .(cvr, qidx = year * 4L + quarter, fte = as.numeric(fte))]
firm <- unique(firm, by = c("cvr", "qidx"))
message(sprintf("   %d firm-quarters, %d firms", nrow(firm), uniqueN(firm$cvr)))

# ---- 2. events: first award per firm, within each (data_source, entity, cvr_method) cell ----
message("2. building events per cell ...")
cmb <- as.data.table(read_clean(file.path(dirs$clean_data, "tender_data_2006_2026")))
cmb <- cmb[entity %chin% ENTITIES & !is.na(cvr_final) & cvr_final != "" & !is.na(award_date)]
cmb[, cvr := sprintf("%08d", suppressWarnings(as.integer(gsub("[^0-9]", "", cvr_final))))]
cmb <- cmb[grepl("^[0-9]{8}$", cvr)]
cmb[, award_date := as.Date(award_date)]

# one row per (cell, firm): stack the three method slices, tag each with its cell label
ev <- rbind(
  cmb[grepl("production", cvr_method), .(data_source, entity, method = "production", cvr, award_date)],
  cmb[grepl("extraction", cvr_method), .(data_source, entity, method = "extraction", cvr, award_date)],
  cmb[grepl("name_match", cvr_method), .(data_source, entity, method = "name_match", cvr, award_date)]
)
ev[, cell := paste(data_source, entity, method, sep = " | ")]
ev <- ev[, .(award_date = min(award_date)), by = .(cell, cvr)]          # first award per firm per cell
ev[, event_qidx := year(award_date) * 4L + quarter(award_date)]

# ---- 3. join employment, window to +/-H, keep balanced firms with positive FTE in every period ----
message("3. building balanced panels ...")
panel <- firm[ev, on = "cvr", allow.cartesian = TRUE, nomatch = NULL]   # firm-quarters x cell
panel[, event_time := qidx - event_qidx]
panel <- panel[event_time %between% c(-H, H) & !is.na(fte) & fte > MIN_FTE]
panel[, n_periods := uniqueN(event_time), by = .(cell, cvr)]            # balanced = all 2H+1 periods present
panel <- panel[n_periods == 2L * H + 1L]
panel[, n_firms := uniqueN(cvr), by = cell]
panel <- panel[n_firms >= MIN_FIRMS]                                    # drop cells with too few firms
panel[, event_time := relevel(factor(event_time), ref = "-1")]
message(sprintf("   %d firm-quarter rows across %d cells", nrow(panel), uniqueN(panel$cell)))
print(panel[, .(firms = uniqueN(cvr), obs = .N), by = cell][order(cell)])

# ---- 4. ONE feols call: a separate event study per cell (fsplit) ----
# FE = firm only. In a treated-only balanced event-time panel, adding calendar-time (qidx) FE makes the
# event-time dummies collinear with the two-way FE -> degenerate standard errors; firm FE + event-time
# dummies is the identified spec here. (Netting out calendar shocks needs control firms -- that is exactly
# the matched-control design in estudy_winner_vs_nonwinner_matched.Rmd.)
message("4. estimating event studies (one per cell) ...")
est <- feols(c(fte, log(fte)) ~ i(event_time, ref = "-1") | cvr,
             data = panel, cluster = ~cvr, fsplit = ~cell)

# ---- 5. save + print ----
saveRDS(list(models = est, panel = panel, window = H, freq = FREQ),
        file.path(emp_dir, sprintf("estudy_twfe_by_cell_h%d.rds", H)))
message("saved -> ", file.path(emp_dir, sprintf("estudy_twfe_by_cell_h%d.rds", H)))
cat("\n===== TWFE event studies by (data_source, entity, cvr_method) -- log(fte) =====\n")
print(etable(est, dict = c("(log(fte))" = "log FTE")))
