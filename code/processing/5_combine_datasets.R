# Concatenate the three matched WINNER datasets (KFST, OpenTender, TED) into one, and the three matched
# BUYER datasets into one. The shared-schema columns line up across all rows; each source's unique columns
# are kept and NA-filled for the other sources. A `dataset` column flags the source (KFST / OpenTender /
# TED). ALL three sources are required for each entity -- the script errors if any is missing.
#
# INPUT  (data/clean/):  clean_{winner,buyer}_data_{kfst,ot,ted}_name_matched.rds
# OUTPUT (data/clean/):  clean_winner_data_all_name_matched.{rds,csv}
#                        clean_buyer_data_all_name_matched.{rds,csv}

source("config.R")
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
source(file.path(PROJECT_DIR, "code", "functions.R"))  # standardise_country(), awarded_winner()
clean <- dirs$clean_data

# entity: "winner" or "buyer". Reads the three source files, tags each with `dataset`, and row-binds them
# with fill = TRUE so shared columns align and source-specific columns become NA where they don't exist.
combine <- function(entity) {
  srcs <- c(KFST = "kfst", OpenTender = "ot", TED = "ted")
  parts <- lapply(names(srcs), function(lbl) {
    f <- file.path(clean, sprintf("clean_%s_data_%s_name_matched.rds", entity, srcs[[lbl]]))
    if (!file.exists(f)) stop(sprintf("Missing required input for combine (%s): %s", entity, f), call. = FALSE)
    d <- as.data.table(readRDS(f))
    # OpenTender's matched file already carries a `dataset` column (the annual source CSV). Preserve it as
    # `ot_source_file` before repurposing `dataset` as the KFST/OpenTender/TED source flag.
    if ("dataset" %in% names(d)) setnames(d, "dataset", "ot_source_file")
    d[, dataset := lbl]
    d[]
  })
  combined <- rbindlist(parts, fill = TRUE, use.names = TRUE)

  # Harmonise country coding across sources: the TED XML uses ISO alpha-3 (DNK/SWE/...), KFST & OT use
  # alpha-2. Map bare alpha-3 tokens to alpha-2; messy KFST multi-country strings are left unchanged.
  for (cc in intersect(c("winner_country", "buyer_country"), names(combined)))
    combined[, (cc) := standardise_country(get(cc))]

  # Winner side: convenience flag for an awarded winner (lot awarded & not a flagged non-winning bidder).
  if (entity == "winner") combined[, is_awarded_winner := awarded_winner(combined)]

  # Column order: a small curated lead (source flag, tender/lot ids, the final name + CVR, and — winner
  # side — the awarded-winner flag), then the shared schema (present in ALL three sources), then the
  # source-specific columns.
  name_col <- paste0(entity, "_name")        # winner_name / buyer_name
  cvr_col  <- paste0(entity, "_cvr_final")   # winner_cvr_final / buyer_cvr_final
  lead <- intersect(c("dataset", "tender_id", "lot_id", name_col, cvr_col, "is_awarded_winner"),
                    names(combined))
  shared <- setdiff(Reduce(intersect, lapply(parts, names)), "dataset")
  rest   <- setdiff(names(combined), lead)
  rest   <- c(intersect(shared, rest), setdiff(rest, shared))   # shared cols ahead of source-specific
  setcolorder(combined, c(lead, rest))
  combined[]
}

winner_all <- combine("winner")
buyer_all  <- combine("buyer")

saveRDS(winner_all, file.path(clean, "clean_winner_data_all_name_matched.rds"))
fwrite(winner_all,  file.path(clean, "clean_winner_data_all_name_matched.csv"))
saveRDS(buyer_all,  file.path(clean, "clean_buyer_data_all_name_matched.rds"))
fwrite(buyer_all,   file.path(clean, "clean_buyer_data_all_name_matched.csv"))

message(sprintf("clean_winner_data_all: %d rows, %d cols", nrow(winner_all), ncol(winner_all)))
print(winner_all[, .N, by = dataset])
message(sprintf("clean_buyer_data_all:  %d rows, %d cols", nrow(buyer_all), ncol(buyer_all)))
print(buyer_all[, .N, by = dataset])
message("Written to ", clean)
