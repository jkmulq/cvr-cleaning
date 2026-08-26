# This script builds the date panel for the OpenTender data
# author: jack mulqueeney
# date: 11 aug 2026

source("config.R")

# 0 Prelims
## 0.1 Packages
library(data.table)

## 0.2 Load data
date_data <- readRDS(file.path(dirs$intermediates, "ted", "notice_dates.rds"))

# 1 Cleaning
## 1.1 Collapse the three schema-era contract-award date fields into one name.
##      They encode the same event (contract awarded/concluded) across form eras
##      and rarely co-occur, so coalescing lifts coverage from ~15.7k to ~26k.
date_data[notice_level == "award" &
            date_field %chin% c("DATE_CONCLUSION_CONTRACT", "CONTRACT_AWARD_DATE", "DATE_OF_CONTRACT_AWARD"),
          date_field := "CONTRACT_AWARD_DATE"]

## 1.2 Extract date/notice types we want
date_types <- c("DS_DATE_DISPATCH", "DATE_PUB", "DATE_RECEIPT_TENDERS", "CONTRACT_AWARD_DATE")
notice_types <- c("planning", "competition", "award")
dates_of_interest <- date_data[notice_level %chin% notice_types &
                                  date_field %chin% date_types &
                                  !is.na(date_iso), ]
dates_of_interest <- unique(dates_of_interest[, .(tender_id, lot_id, notice_level, notice_id, date_field, date_iso)])

## 1.3 Create more descriptive name for notice_level-date_field combos
dates_of_interest[, date_name := fcase(
  # Planning dates
  notice_level == "planning" & date_field == "DS_DATE_DISPATCH", "planning_dispatch_date",
  notice_level == "planning" & date_field == "DATE_PUB", "planning_publication_date",
  notice_level == "planning" & date_field == "DATE_RECEIPT_TENDERS", "planning_tender_deadline_date",
  
  # Competition dates
  notice_level == "competition" & date_field == "DS_DATE_DISPATCH", "competition_dispatch_date",
  notice_level == "competition" & date_field == "DATE_PUB", "competition_publication_date",
  notice_level == "competition" & date_field == "DATE_RECEIPT_TENDERS", "competition_tender_deadline_date",
  
  # Award dates
  notice_level == "award" & date_field == "DS_DATE_DISPATCH", "award_dispatch_date",
  notice_level == "award" & date_field == "DATE_PUB", "award_publication_date",
  notice_level == "award" & date_field == "DATE_RECEIPT_TENDERS", "award_tender_deadline_date",
  notice_level == "award" & date_field == "CONTRACT_AWARD_DATE", "award_contract_date",

  # Other
  default = NA_character_
)]

## 1.4 Drop any level-field combo we did not explicitly name above (e.g. a contract-
##      award date mis-tagged onto a competition/planning notice) so the cast produces
##      no stray "NA" column.
dates_of_interest <- dates_of_interest[!is.na(date_name)]

## 1.5 Pre-process dates
dates_of_interest[, date_iso := lubridate::ymd(date_iso)]

## 1.6 Pivot wider (one row per tender-lot)
dates_of_interest_wide <- dcast(dates_of_interest, tender_id + lot_id ~ date_name,
                                value.var = "date_iso",
                                fun.aggregate = function(x) sort(x)[1])  # take earliest if multiple

## 1.7 Order columns intuitively
setcolorder(dates_of_interest_wide, 
            c("tender_id", "lot_id", 
              "planning_dispatch_date", "planning_publication_date", "planning_tender_deadline_date",
              "competition_dispatch_date", "competition_publication_date", "competition_tender_deadline_date",
              "award_dispatch_date", "award_publication_date", "award_tender_deadline_date", "award_contract_date"))

# 2 Save
saveRDS(dates_of_interest_wide, 
        file.path(dirs$intermediates, "ted", "opentender_notice_dates.rds"))
