# This script builds the date panels from the TED notice lineage, one per source
# (OpenTender + KFST). author: jack mulqueeney. date: 11 aug 2026

source("config.R")

# 0 Prelims
## 0.1 Packages
library(data.table)

## 0.2 Load data
date_data <- as.data.table(readRDS(file.path(dirs$intermediates, "ted", "notice_dates.rds")))

## 0.3 Older notice_dates.rds (OT-only build) has no `source` column -> treat all rows as OpenTender.
if (!"source" %in% names(date_data)) date_data[, source := "ot"]

# 1 Build the wide date panel for one source's long date rows. The logic is identical for OT and KFST;
#   only the tender/lot ID space differs (kept apart upstream by `source`).
build_date_panel <- function(dd) {
  dd <- copy(dd)

  ## 1.1 Collapse the three schema-era contract-award date fields into one name. They encode the same
  ##      event across form eras and rarely co-occur, so coalescing lifts coverage substantially.
  dd[notice_level == "award" &
       date_field %chin% c("DATE_CONCLUSION_CONTRACT", "CONTRACT_AWARD_DATE", "DATE_OF_CONTRACT_AWARD"),
     date_field := "CONTRACT_AWARD_DATE"]

  ## 1.2 Keep the date/notice types of interest
  date_types   <- c("DS_DATE_DISPATCH", "DATE_PUB", "DATE_RECEIPT_TENDERS", "CONTRACT_AWARD_DATE")
  notice_types <- c("planning", "competition", "award")
  doi <- dd[notice_level %chin% notice_types & date_field %chin% date_types & !is.na(date_iso), ]
  doi <- unique(doi[, .(tender_id, lot_id, notice_level, notice_id, date_field, date_iso)])

  ## 1.3 Descriptive name for each notice_level-date_field combo
  doi[, date_name := fcase(
    notice_level == "planning"    & date_field == "DS_DATE_DISPATCH",     "planning_dispatch_date",
    notice_level == "planning"    & date_field == "DATE_PUB",             "planning_publication_date",
    notice_level == "planning"    & date_field == "DATE_RECEIPT_TENDERS", "planning_tender_deadline_date",
    notice_level == "competition" & date_field == "DS_DATE_DISPATCH",     "competition_dispatch_date",
    notice_level == "competition" & date_field == "DATE_PUB",             "competition_publication_date",
    notice_level == "competition" & date_field == "DATE_RECEIPT_TENDERS", "competition_tender_deadline_date",
    notice_level == "award"       & date_field == "DS_DATE_DISPATCH",     "award_dispatch_date",
    notice_level == "award"       & date_field == "DATE_PUB",             "award_publication_date",
    notice_level == "award"       & date_field == "DATE_RECEIPT_TENDERS", "award_tender_deadline_date",
    notice_level == "award"       & date_field == "CONTRACT_AWARD_DATE",  "award_contract_date",
    default = NA_character_
  )]

  ## 1.4 Drop any level-field combo not explicitly named (e.g. a contract-award date mis-tagged onto a
  ##      competition/planning notice) so the cast produces no stray "NA" column.
  doi <- doi[!is.na(date_name)]

  ## 1.5 Pivot wider (one row per tender-lot; earliest date if several). Empty source -> bare key.
  date_cols <- c(
    "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date",
    "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
    "award_dispatch_date", "award_publication_date", "award_tender_deadline_date", "award_contract_date")
  if (nrow(doi)) {
    doi[, date_iso := lubridate::ymd(date_iso)]
    wide <- dcast(doi, tender_id + lot_id ~ date_name, value.var = "date_iso",
                  fun.aggregate = function(x) sort(x)[1])
  } else {
    wide <- unique(dd[, .(tender_id, lot_id)])
  }

  ## 1.6 Schema parity: guarantee all 10 named date columns exist (all-NA where this source genuinely
  ##      has none, e.g. the award-level tender deadline is OpenTender-only), so the OT and KFST panels
  ##      always have identical shape.
  for (col in setdiff(date_cols, names(wide))) wide[, (col) := as.Date(NA)]
  setcolorder(wide, c("tender_id", "lot_id", date_cols))
  wide[]
}

# 2 Save one panel per source. The OpenTender output keeps its original name/schema (no `source`
#   column), so 1_2_process_open_tender.R is untouched; the KFST panel is the new mirror for 1_1.
saveRDS(build_date_panel(date_data[source == "ot"]),
        file.path(dirs$intermediates, "ted", "opentender_notice_dates.rds"))
saveRDS(build_date_panel(date_data[source == "kfst"]),
        file.path(dirs$intermediates, "ted", "kfst_notice_dates.rds"))
