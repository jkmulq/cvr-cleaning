# Combine ALL six matched samples into one long dataset for the secure-server delivery:
#   KFST winners, KFST buyers, OpenTender winners, OpenTender buyers, TED winners, TED buyers.
# Author: Jack Mulqueeney. Date: 9 Sep 2026.
#
# Inputs. Winners/buyers that have a concat-and-dedup stack (3_1/3_2/3_3) come from the stack (so their
# production/extraction sub-samples are selectable); the rest come from their matched file:
#   KFST winner  -> kfst_winner_datasets_stacked.rds        (stack: production + extraction)
#   OT winner    -> ot_winner_datasets_stacked.rds          (stack)
#   OT buyer     -> ot_buyer_datasets_stacked.rds            (stack)
#   KFST buyer   -> clean_buyer_data_kfst_name_matched.rds   (matched; no source CVR field to extract)
#   TED winner   -> clean_winner_data_ted_name_matched.rds   (matched)
#   TED buyer    -> clean_buyer_data_ted_name_matched.rds    (matched)
#
# Sample-selection columns (kept SEPARATE because samples overlap -- an agree-lot row belongs to both the
# production and extraction samples):
#   data_source : "KFST" | "OpenTender" | "TED"   (named data_source, not source: OpenTender already has a
#                 native `source` column -- its CVR-cleaning provenance -- which is left untouched)
#   entity      : "winner" | "buyer"
#   dataset     : "production" | "extraction"   (matched files are all "production")
#   build_prod  : logical -- row is in the production/name-matched sample of its data_source+entity
#   build_extr  : logical -- row is in the raw-extraction sample (FALSE for the matched-only sources)
# Any one sample = data_source & entity & build_prod  (or build_extr). e.g. OT winner extraction =
#   data_source == "OpenTender" & entity == "winner" & build_extr.
#
# CVR columns are standardised: the entity's `winner_cvr_*` / `buyer_cvr_*` become `cvr_*` (so winner and
# buyer CVRs share one set of columns). The buyer-context `buyer_cvr_original` carried on winner rows is
# left as-is (it is not the winner's CVR). Winner/buyer country is harmonised to ISO alpha-2 via
# standardise_country() (TED XML uses alpha-3, e.g. DNK -> DK; a no-op for the already-alpha-2 sources).
#
# Rows with no resolved CVR (cvr_final NA/empty) are dropped -- not useful in the server, and it trims size.
#
# OUTPUT (data/clean/): clean_all_samples_combined.{rds,csv}  -- union schema, source-specific columns NA-filled.

rm(list = ls()); source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(tidyverse)}))
source(file.path(PROJECT_DIR, "code", "functions.R"))
clean <- dirs$clean_data
stack_dir <- Sys.getenv("STACK_DIR", unset = clean)   # stacks live with the matched files; override for testing

# file, directory, source, entity, is_stack
spec <- list(
  list("kfst_winner_datasets_stacked.rds",      stack_dir, "KFST",       "winner", TRUE),
  list("ot_winner_datasets_stacked.rds",         stack_dir, "OpenTender", "winner", TRUE),
  list("ot_buyer_datasets_stacked.rds",          stack_dir, "OpenTender", "buyer",  TRUE),
  list("clean_buyer_data_kfst_name_matched.rds", clean,     "KFST",       "buyer",  FALSE),
  list("clean_winner_data_ted_name_matched.rds", clean,     "TED",        "winner", FALSE),
  list("clean_buyer_data_ted_name_matched.rds",  clean,     "TED",        "buyer",  FALSE)
)

parts <- lapply(spec, function(s) {
  f <- s[[1]]; dir <- s[[2]]; src <- s[[3]]; ent <- s[[4]]; is_stack <- s[[5]]
  p <- file.path(dir, f)
  if (!file.exists(p)) stop(sprintf("4_combine_all_datasets: missing input %s", p), call. = FALSE)
  d <- as.data.table(readRDS(p))

  # standardise the entity's own CVR columns -> cvr_* (winner_cvr_* on winner rows, buyer_cvr_* on buyer)
  old <- grep(paste0("^", ent, "_cvr"), names(d), value = TRUE)
  if (length(old)) setnames(d, old, sub(paste0("^", ent, "_cvr"), "cvr", old))

  # sample-selection columns (data_source, not source: OpenTender has its own native `source` column)
  d[, `:=`(data_source = src, entity = ent)]
  if ("dataset" %in% names(d)) d[, dataset := as.character(dataset)] else d[, dataset := "production"]
  if (!is_stack) d[, `:=`(build_prod = TRUE, build_extr = FALSE)]   # matched files: production sample only
  d[]
})

combined <- rbindlist(parts, use.names = TRUE, fill = TRUE)

# Drop rows with no resolved CVR -- they carry no linkable firm and are not useful in the secure server.
# (The stacks already dropped theirs; this removes the unmatched rows the matched TED / KFST-buyer carry.)
combined <- combined[!is.na(cvr_final) & cvr_final != ""]

# Harmonise country to ISO alpha-2 (TED XML uses alpha-3: DNK -> DK, SWE -> SE, ...); no-op for alpha-2.
for (cc in intersect(c("winner_country", "buyer_country"), names(combined)))
  combined[, (cc) := standardise_country(get(cc))]

combined[, dataset := factor(dataset, levels = c("production", "extraction"))]

# lead with the sample-selection + key identity columns, then everything else
lead <- intersect(c("data_source","entity","dataset","build_prod","build_extr","tender_id","lot_id",
                    "cvr_final","winner_name","buyer_name","is_awarded_winner","cvr_number_source"),
                  names(combined))
setcolorder(combined, c(lead, setdiff(names(combined), lead)))

# --- Self-check: every (data_source, entity[, build flag]) slice reproduces its source dataset ----------
# Re-read each input and confirm the combined slice recovers exactly its resolved-CVR (tender, lot, CVR)
# set; for the stacks, also that build_prod / build_extr recover the production / extraction sub-samples.
for (s in spec) {
  f <- s[[1]]; dir <- s[[2]]; src <- s[[3]]; ent <- s[[4]]; is_stack <- s[[5]]
  cvrcol <- paste0(ent, "_cvr_final")
  o  <- as.data.table(readRDS(file.path(dir, f)))
  o  <- o[!is.na(get(cvrcol)) & get(cvrcol) != ""]
  ok <- unique(o[, .(tender_id, lot_id, cvr = get(cvrcol))])
  ck <- unique(combined[data_source == src & entity == ent, .(tender_id, lot_id, cvr = cvr_final)])
  if (nrow(fsetdiff(ok, ck)) || nrow(fsetdiff(ck, ok)))
    stop(sprintf("selection check: (%s, %s) slice does not reproduce %s", src, ent, f), call. = FALSE)
  if (is_stack) {
    for (flag in c("build_prod", "build_extr")) {
      ok2 <- unique(o[get(flag) == TRUE, .(tender_id, lot_id, cvr = get(cvrcol))])
      ck2 <- unique(combined[data_source == src & entity == ent & get(flag) == TRUE,
                             .(tender_id, lot_id, cvr = cvr_final)])
      if (nrow(fsetdiff(ok2, ck2)) || nrow(fsetdiff(ck2, ok2)))
        stop(sprintf("selection check: (%s, %s) %s does not reproduce the stack's %s sample", src, ent, flag, flag), call. = FALSE)
    }
  } else {
    sl <- combined[data_source == src & entity == ent]
    if (!all(sl$build_prod) || any(sl$build_extr))
      stop(sprintf("selection check: matched source (%s, %s) should be build_prod=TRUE, build_extr=FALSE", src, ent), call. = FALSE)
  }
}
cat("selection-column self-check passed: every (data_source, entity[, build flag]) slice reproduces its source dataset.\n")

out_dir <- Sys.getenv("COMBINE_OUT_DIR", unset = clean)
saveRDS(combined, file.path(out_dir, "clean_all_samples_combined.rds"))
fwrite(combined,  file.path(out_dir, "clean_all_samples_combined.csv"))

message(sprintf("clean_all_samples_combined: %d rows, %d cols", nrow(combined), ncol(combined)))
print(combined[, .(rows = .N, production = sum(build_prod), extraction = sum(build_extr)),
               by = .(data_source, entity)][order(data_source, entity)])
message("Written to ", out_dir)
