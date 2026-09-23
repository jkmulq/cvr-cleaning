#!/usr/bin/env Rscript
# Phase B of match-then-pull: LOCAL per-event control eligibility on the annual employment panel from
# employment_annual_universe.R (no Virk calls). For each winner-event, eligible controls = never-winner
# firms with positive employment in [award_year-2, award_year+2], matched on the broadest-first cascade:
#   division + kommune  ->  (if < MIN) division  ->  (if < MIN) any firm employed in the window.
# Reports avg eligible per event, the rung distribution, and the UNIQUE union of control CVRs = the size
# of the quarterly employment pull we'd then do (Phase C).
#
# Run:  LC_ALL=en_US.UTF-8 Rscript code/analysis/eligibility_from_annual_panel.R
# Env:  ELIG_WINDOW_YRS (default 2), ELIG_MIN_CONTROLS (default 20), ELIG_ANNUAL_FILE (default newest pull)

rm(list = ls())
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
report_project_dir <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(d, "cvr-cleaning.Rproj")) && dirname(d) != d) d <- dirname(d)
  d
})
setwd(report_project_dir); source(file.path(report_project_dir, "config.R"))
cd <- dirs$clean_data
hwin <- as.integer(Sys.getenv("ELIG_WINDOW_YRS", "2"))
MIN  <- as.integer(Sys.getenv("ELIG_MIN_CONTROLS", "20"))

# 1. Annual panel -> firm-employed-year rows (positive headcount only), with 2-digit sector division.
af <- Sys.getenv("ELIG_ANNUAL_FILE",
                 unset = tail(sort(Sys.glob(file.path(dirs$cvr_key, "cvr_annual_employment_virk_*.csv"))), 1))
cat("annual panel:", basename(af), "\n")
ann <- fread(af, colClasses = list(character = c("cvr", "kommune")))
ann[, cvr := sprintf("%08d", as.integer(cvr))]
ann[, division := ifelse(is.na(sector), NA_character_, substr(sprintf("%06d", sector), 1, 2))]
emp <- ann[!is.na(antal_ansatte) & antal_ansatte >= 1, .(cvr, year, division, kommune)]
firm <- unique(ann[, .(cvr, division, kommune)], by = "cvr")   # firm-level sector/kommune (current snapshot)
cat(sprintf("annual rows: %d | positive firm-years: %d | distinct ever-employed firms: %d\n",
            nrow(ann), nrow(emp), uniqueN(emp$cvr)))

# 2. Winner events: (winning cvr, award_year) across the three winner datasets; winners = the exclusion set.
wf <- c("clean_winner_data_kfst_name_matched.rds","clean_winner_data_ot_name_matched.rds","clean_winner_data_ted_name_matched.rds")
wl <- rbindlist(lapply(wf, function(f) {
  d <- as.data.table(readRDS(file.path(cd, f)))
  d[, .(cvr = sprintf("%08d", as.integer(winner_cvr_final)), award_year = year(as.IDate(award_date)))]
}), fill = TRUE)
wl <- wl[grepl("^[0-9]{8}$", cvr) & !is.na(award_year)]
winners <- unique(wl$cvr)
# One event per (winning firm, award_year); attach the firm's division/kommune.
events <- unique(wl, by = c("cvr", "award_year"))
events <- firm[events, on = "cvr"]                       # add division, kommune of the winning firm
events <- events[!is.na(division) & !is.na(kommune)]     # need sector+kommune to place the event
events[, `:=`(y0 = award_year - hwin, y1 = award_year + hwin, ev = .I)]
cat(sprintf("winners: %d | events (firm x award-year) placeable: %d\n\n", length(winners), nrow(events)))

emp_nw <- emp[!(cvr %chin% winners)]                     # never-winner candidate firm-years

# 3. Cascade. For a rung's keys, candidates for an event = never-winner firms sharing those keys and
#    employed within the event window. Non-equi join on the year window; count distinct firms per event.
cand_pairs <- function(keycols) {
  # join never-winner firm-years to events on key(s) + year within [y0, y1]
  on <- c(keycols, "year>=y0", "year<=y1")
  j <- emp_nw[events, on = on, allow.cartesian = TRUE, nomatch = 0L,
              .(ev = i.ev, cvr = x.cvr)]
  unique(j)
}
p1 <- cand_pairs(c("division", "kommune"))
c1 <- p1[, .(n = uniqueN(cvr)), by = ev]

# events below MIN at rung1 cascade to rung2 (division only); those still below cascade to rung3 (window only).
ev1 <- c1[n >= MIN, ev]
need2 <- setdiff(events$ev, ev1)
p2 <- if (length(need2)) cand_pairs("division")[ev %chin% need2] else p1[0]
c2 <- p2[, .(n = uniqueN(cvr)), by = ev]
ev2 <- c2[n >= MIN, ev]
need3 <- setdiff(need2, ev2)
# rung3: any never-winner firm employed in the window (no sector/kommune)
p3 <- if (length(need3)) {
  ev3d <- events[ev %chin% need3]
  emp_nw[ev3d, on = c("year>=y0", "year<=y1"), allow.cartesian = TRUE, nomatch = 0L, .(ev = i.ev, cvr = x.cvr)][, unique(.SD)]
} else p1[0]

# 4. Per-event eligible sets at the chosen rung, and the union.
elig <- rbindlist(list(
  p1[ev %chin% ev1],
  p2[ev %chin% ev2],
  p3
), use.names = TRUE)
per_ev <- elig[, .(n_elig = uniqueN(cvr)), by = ev]
union_cvr <- unique(elig$cvr)

cat("=== rung usage across events ===\n")
cat(sprintf("  rung1 (division+kommune): %d\n  rung2 (division):        %d\n  rung3 (window only):     %d\n  no candidates:           %d\n",
            length(ev1), length(ev2), length(need3), nrow(events) - length(ev1) - length(ev2) - length(need3)))
cat(sprintf("\navg eligible controls / event: %.0f | median: %.0f\n",
            mean(per_ev$n_elig), median(per_ev$n_elig)))
cat(sprintf("\n*** UNIQUE control firms to pull (quarterly) = %d ***\n", length(union_cvr)))
cat(sprintf("    vs ever-employed universe %d | vs old ~486k random sample\n", uniqueN(emp$cvr)))

# 5. Save the union CVR list (the Phase-C pull target) + per-event eligibility.
out_union <- file.path(dirs$employment, "eligible_control_cvrs.rds")
saveRDS(data.table(cvr = union_cvr), out_union)
saveRDS(elig, file.path(dirs$employment, "eligible_controls_by_event.rds"))
cat(sprintf("\nwrote %d union CVRs -> %s\n", length(union_cvr), out_union))
