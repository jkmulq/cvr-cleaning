# Build tender-lot-grain winner and buyer datasets from the TED extraction, mirroring
# the clean_winner_data / clean_buyer_data shape used for KFST and OpenTender: one row
# per (notice, lot, party) with the lot- and notice-level context joined on. Here the
# TED notice_id plays the role of the tender and `lot` the role of the lot_id.
#
# INPUT (data/intermediates/ted/, from ted_2_extract_party_cvrs.R):
#   ted_extracted_parties.rds   ted_extracted_lots.rds   ted_extracted_cvrs.rds
# OUTPUT (same folder):
#   ted_winner_data.{rds,csv}   one row per (notice_id, lot, supplier org): BOTH
#                               winners and non-winning bidders, flagged by is_winner
#                               (TRUE = winner, FALSE = non-winning bidder).
#   ted_buyer_data.{rds,csv}    one row per (notice_id, lot, buyer org)
#
# Winners are lot-linked in the XML, so they join to the lot directly. Buyers are the
# notice's contracting party (notice-level, no lot), so they are replicated across the
# notice's lots - the same way a buyer appears against every lot of its tender in the
# OpenTender/KFST buyer tables.
#
# Grain is (notice_id, lot_id), where lot_id is a clean 1..N index of the notice's lots
# (shared by both tables). The raw TED lot code is kept as `lot` (joins to
# ted_extracted_lots). The opaque internal TED tender/bid id is not carried.

source("config.R")
suppressWarnings(suppressPackageStartupMessages(library(data.table)))

ted <- file.path(dirs$intermediates, "ted")
parties <- as.data.table(readRDS(file.path(ted, "ted_extracted_parties.rds")))
lots    <- as.data.table(readRDS(file.path(ted, "ted_extracted_lots.rds")))
nsum    <- as.data.table(readRDS(file.path(ted, "ted_extracted_cvrs.rds")))

# Lot-level context (one row per notice_id + lot). Includes the lot-level currency, renamed so it can
# back-fill the party-level currency (buyers have no party amount/currency -> otherwise NA).
lot_cols <- intersect(c("notice_id", "lot", "lot_title", "cpv_main", "contract_type",
                        "lot_estimated_value", "lot_awarded_value", "n_tenders_received", "currency"),
                      names(lots))
lot_ctx <- unique(lots[, ..lot_cols], by = c("notice_id", "lot"))
if ("currency" %in% names(lot_ctx)) setnames(lot_ctx, "currency", "lot_currency")

# Notice-level context (one row per notice) - amounts, classification, procedure, dates, and the
# notice-level currency (second fallback for the row currency).
notice_cols <- intersect(c("notice_id", "amount_awarded", "amount_estimated", "n_lots",
                          "procedure_type", "procedure_group", "is_framework", "is_dps",
                          "direct_award", "date_receipt_tenders", "date_contract_award",
                          "date_award_dispatch", "date_award_publication", "currency"), names(nsum))
notice_ctx <- nsum[, ..notice_cols]
if ("currency" %in% names(notice_ctx)) setnames(notice_ctx, "currency", "notice_currency")

# Canonical lot_id: 1..N per notice across all its lots, ordered by lot number, shared
# by both tables so a given lot gets the same lot_id in the winner and buyer files.
lot_key <- unique(rbindlist(list(
  lot_ctx[, .(notice_id, lot)],
  parties[role %chin% c("winner", "bidder") & !is.na(lot) & lot != "", .(notice_id, lot)])))
lot_key[, ord := suppressWarnings(as.integer(gsub("[^0-9]", "", lot)))]
setorder(lot_key, notice_id, ord, lot, na.last = TRUE)
lot_key[, `:=`(lot_id = rowid(notice_id), ord = NULL)]

build <- function(roles, prefix, keep_is_winner = FALSE) {
  cols <- c("notice_id", "year", "schema", "lot", "name", "cvr_raw", "cvr",
            "country", "amount", "currency", if (keep_is_winner) "is_winner")
  p <- parties[role %chin% roles, ..cols]
  setnames(p, c("name", "cvr", "cvr_raw", "country", "amount"),
           paste0(prefix, c("_name", "_cvr", "_cvr_raw", "_country", "_amount")))

  if (identical(roles, "buyer")) {
    # Buyer is notice-level -> replicate across the notice's lots (keeps buyers whose
    # notice has no extracted lots, at lot = NA).
    p[, lot := NULL]
    p <- merge(p, lot_ctx, by = "notice_id", all.x = TRUE, allow.cartesian = TRUE)
  } else {
    # Winners are lot-linked; non-winning bidders carry lot = NA (notice-level).
    p <- merge(p, lot_ctx, by = c("notice_id", "lot"), all.x = TRUE)
  }
  p <- merge(p, lot_key,    by = c("notice_id", "lot"), all.x = TRUE)   # clean lot_id
  p <- merge(p, notice_ctx, by = "notice_id", all.x = TRUE)
  # Row currency: the party's own currency (winners), falling back to the lot- then notice-level
  # currency. Buyers have no party amount, so they inherit the lot/notice currency of the contract.
  fb <- intersect(c("currency", "lot_currency", "notice_currency"), names(p))
  if (length(fb)) {
    p[, currency := Reduce(fcoalesce, lapply(fb, function(cc) as.character(p[[cc]])))]
    drop_fb <- setdiff(fb, "currency")
    if (length(drop_fb)) p[, (drop_fb) := NULL]
  }
  setcolorder(p, c("notice_id", "lot_id", "lot", "year", "schema",
                   if (keep_is_winner) "is_winner",
                   paste0(prefix, c("_name", "_cvr", "_cvr_raw", "_country", "_amount")),
                   "currency"))
  setorder(p, notice_id, lot_id, na.last = TRUE)
  p[]
}

# Winner side carries BOTH winners and non-winning bidders, flagged by is_winner.
ted_winner_data <- build(c("winner", "bidder"), "winner", keep_is_winner = TRUE)
ted_buyer_data  <- build("buyer", "buyer")

saveRDS(ted_winner_data, file.path(ted, "ted_winner_data.rds"))
fwrite(ted_winner_data,  file.path(ted, "ted_winner_data.csv"))
saveRDS(ted_buyer_data,  file.path(ted, "ted_buyer_data.rds"))
fwrite(ted_buyer_data,   file.path(ted, "ted_buyer_data.csv"))

# ── Diagnostics ───────────────────────────────────────────────────────────────
message(sprintf("ted_winner_data: %d rows (%d winners, %d non-winning bidders) | %d notices | CVR on %.0f%%",
                nrow(ted_winner_data), sum(ted_winner_data$is_winner, na.rm = TRUE),
                sum(!ted_winner_data$is_winner, na.rm = TRUE), uniqueN(ted_winner_data$notice_id),
                100 * mean(!is.na(ted_winner_data$winner_cvr))))
message(sprintf("ted_buyer_data:  %d rows | %d notices | %d notice-lots | buyer CVR on %.0f%%",
                nrow(ted_buyer_data), uniqueN(ted_buyer_data$notice_id),
                uniqueN(ted_buyer_data[, paste(notice_id, lot)]),
                100 * mean(!is.na(ted_buyer_data$buyer_cvr))))
message("cols (winner): ", paste(names(ted_winner_data), collapse = ", "))
message("Written to ", ted)
