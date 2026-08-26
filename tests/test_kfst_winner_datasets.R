# Assertion tests for the production KFST winner-datasets stack
# (data/clean/kfst_winner_datasets_stacked.rds, built by code/processing/3_1_build_kfst_winner_datasets.R).
# Promoted from the retired code/drafts/test_dummy_production_kfst.R and repointed to the production
# outputs + level names (base / extraction / name_only). Prints PASS/FAIL; exits non-zero on any FAIL.
#   LC_ALL=en_US.UTF-8 Rscript tests/test_kfst_winner_datasets.R

suppressWarnings(suppressPackageStartupMessages({library(data.table); library(readxl); library(stringr)}))
source("config.R")
is8 <- function(x) !is.na(x) & grepl("^[0-9]{8}$", x)
fails <- 0L
ok <- function(name, cond) {
  cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", name, "\n")
  if (!isTRUE(cond)) fails <<- fails + 1L
}

d     <- as.data.table(readRDS(file.path(dirs$clean_data, "kfst_winner_datasets_stacked.rds")))
canon <- as.data.table(readRDS(file.path(dirs$clean_data, "clean_winner_data_kfst_name_matched.rds")))

# 1. structure
ok("three datasets present",
   setequal(as.character(unique(d$dataset)), c("base", "extraction", "name_only")))
need <- c("dataset", "tender_id", "lot_id", "winner_number", "winner_name",
          "winner_cvr_final", "annualised_lot_amount", "award_date")
ok("required columns present", all(need %in% names(d)))

# 2. no invalid CVRs anywhere (every non-NA winner_cvr_final is a clean 8-digit)
ok("no invalid winner_cvr_final", d[!is.na(winner_cvr_final) & !is8(winner_cvr_final), .N] == 0L)

# 3. the base slice reproduces the canonical matched winner table (3_1 did not alter CVRs)
a <- sort(unique(d[dataset == "base" & is8(winner_cvr_final), as.character(winner_cvr_final)]))
b <- sort(unique(canon[is8(winner_cvr_final), as.character(winner_cvr_final)]))
ok("base slice CVR set == clean_winner_data_kfst_name_matched.rds", identical(a, b))

# 4. name_only is PURE name matching: it carries no field CVR
ok("name_only has no field CVR (winner_cvr_clean all NA)",
   d[dataset == "name_only", all(is.na(winner_cvr_clean))])

# 5. extraction is field-only: every extraction CVR is a standalone 8-digit run in that lot's raw field
raw <- as.data.table(read_excel(file.path(dirs$raw_data, "kfst", "udbudsdata_kfst.xlsx"),
                                sheet = "2.0 Udbudsdata"))
field <- raw[, .(tender_id = as.character(`Løbenummer`), lot_id = as.character(`Nummerplade`),
                 f = `Vinders CVR`)]
field <- field[, .(cvr = unlist(str_extract_all(f, "(?<![0-9])[0-9]{8}(?![0-9])"))),
               by = .(tender_id, lot_id)]
ex <- unique(d[dataset == "extraction", .(tender_id = as.character(tender_id),
                                          lot_id = as.character(lot_id),
                                          cvr = as.character(winner_cvr_final))])
ok("extraction CVRs all trace to the raw field",
   nrow(ex[!field, on = .(tender_id, lot_id, cvr)]) == 0L)

# 6. grain: no fully-duplicated rows (the context join did not inflate)
ok("no duplicate rows", nrow(d) == nrow(unique(d)))

# 7. lot context populated where applicable (framework lots) via the 1:1 context join
ok("some annualised amounts populated", d[!is.na(annualised_lot_amount), .N] > 0L)

cat("\n== coverage (informational) ==\n")
print(d[, .(rows = .N, cvrs = uniqueN(winner_cvr_final[is8(winner_cvr_final)]),
            lots = uniqueN(paste(tender_id, lot_id))), by = dataset])

cat(sprintf("\n%s (%d failure%s)\n", if (fails == 0) "ALL CHECKS PASSED" else "SOME CHECKS FAILED",
            fails, if (fails == 1) "" else "s"))
if (fails > 0) quit(status = 1)
