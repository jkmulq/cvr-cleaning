library(xml2)
library(httr)
library(dplyr)
library(furrr)
library(progressr)

# ─────────────────────────────────────────────────────────────────────────────
# Extract, per TED notice, an indicator of whether NON-WINNING tenderers exist /
# are listed.
#
# Two things make the naive approach fail:
#   1. TED (behind CloudFront + nginx) rate-limits: firing many parallel requests
#      returns HTTP 429, whose error body is NOT valid XML. We must check the
#      status code, back off, and retry — not blindly parse every response.
#   2. There are TWO XML schemas. Notices from ~2024+ use eForms UBL
#      (root <ContractAwardNotice>, efac:/cbc:/cac: namespaces) and DO list
#      individual tenderers. Notices from 2011–2023 use the legacy TED_EXPORT
#      (R2.0.x) schema, which only publishes a COUNT of tenders received
#      (NB_TENDERS_RECEIVED) — losing bidders are never named there.
#
# Output (one row per notice_id):
#   notice_id, schema, fetch_status, n_tenders_received, n_winners,
#   had_non_winners      (count-based: received > winners — works for both schemas)
#   non_winners_listed   (identity-based: notice names ≥1 losing bidder — eForms only)
#   n_non_winners_named
# Plus a long table `non_winners_named` (notice_id, company, bid_value, currency)
# for eForms notices that list losers.
# ─────────────────────────────────────────────────────────────────────────────

# ── Config ───────────────────────────────────────────────────────────────────

# Paths come from config.R (run from the repo root, as with the other scripts):
# it locates the project root robustly and defines PROJECT_DIR + dirs$*.
source("config.R")

data_file   <- file.path(dirs$clean_data, "clean_winner_data_ot_name_matched.rds")

# All TED intermediates live under data/intermediates/ted/ (kept separate),
# named by role:
#   raw_xml/           – raw fetched notice XML (cache; resumable, re-parse w/o re-fetch)
#   notice_indicators/ – per-chunk notice_indicator RDS
ted_dir     <- file.path(dirs$intermediates, "ted")
cache_dir   <- file.path(ted_dir, "raw_xml")
save_dir    <- file.path(ted_dir, "notice_indicators")
out_prefix  <- "notice_indicator_chunk_" # new prefix; old malformed chunk_*.rds are ignored
chunk_size  <- 500
n_workers   <- 3                         # low concurrency: TED throttles aggressively
max_retries <- 5
base_delay  <- 2                         # seconds; exponential backoff base

# ── Namespaces (eForms UBL) ──────────────────────────────────────────────────

ns <- c(
  cbc  = "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2",
  cac  = "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2",
  efac = "http://data.europa.eu/p27/eforms-ubl-extension-aggregate-components/1"
)

# ── Derive notice_id from the TED award-notice URL ───────────────────────────
# Handles both the legacy udl form (...TED:NOTICE:390492-2011:...) and the new
# form (.../notice/00712164-2024). Returns NA when no TED notice number present.

derive_notice_id <- function(url) {
  vapply(url, function(u) {
    if (is.na(u) || u == "") return(NA_character_)
    m <- regmatches(u, regexpr("[0-9]{6,8}-[0-9]{4}", u, perl = TRUE))
    if (length(m) == 0L) NA_character_ else m[[1]]
  }, character(1), USE.NAMES = FALSE)
}

# ── Fetch one notice's XML: cache-aware, status-aware, with retry/backoff ─────

fetch_notice_xml <- function(notice_id, cache_dir, max_retries, base_delay) {

  if (!dir.exists(cache_dir)) dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_path <- file.path(cache_dir, paste0(notice_id, ".xml"))

  # 1. Cache hit → parse from disk, no network.
  if (file.exists(cache_path) && file.info(cache_path)$size > 0) {
    xml <- tryCatch(xml2::read_xml(cache_path), error = function(e) NULL)
    if (!is.null(xml)) return(list(xml = xml, status = "ok"))
  }

  url <- paste0("https://ted.europa.eu/en/notice/", notice_id, "/xml")

  last_transient <- "throttled"    # records why we're still retrying (exhausted status)

  for (attempt in seq_len(max_retries)) {

    resp <- tryCatch(
      httr::GET(
        url,
        httr::add_headers(`User-Agent` = "Mozilla/5.0",
                          `Accept`     = "application/xml, text/xml, */*"),
        httr::timeout(30)
      ),
      error = function(e) NULL
    )

    # Network-level failure → back off and retry.
    if (is.null(resp)) {
      Sys.sleep(min(base_delay * 2^(attempt - 1) + runif(1, 0, 1), 30))
      next
    }

    code <- httr::status_code(resp)

    if (code == 200) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      xml <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
      if (is.null(xml)) return(list(xml = NULL, status = "parse_error"))
      tryCatch(                                   # cache raw bytes (best effort)
        writeBin(charToRaw(enc2utf8(txt)), cache_path),
        error = function(e) invisible(NULL)
      )
      return(list(xml = xml, status = "ok"))
    }

    if (code == 404) return(list(xml = NULL, status = "not_found"))

    # Retryable: 202 = TED is still rendering the XML async (common for legacy
    # notices under load; re-requesting the same URL returns 200 once ready);
    # 429 = throttled; 5xx = transient server error. Honour Retry-After, else
    # exponential backoff (capped) so a worker never sleeps too long.
    if (code == 202 || code == 429 || code >= 500) {
      last_transient <- if (code == 202) "pending" else "throttled"
      ra   <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))[1]
      wait <- if (!is.na(ra)) ra else base_delay * 2^(attempt - 1) + runif(1, 0, 1)
      Sys.sleep(min(wait, 30))
      next
    }

    return(list(xml = NULL, status = paste0("http_", code)))
  }

  list(xml = NULL, status = last_transient)   # exhausted: "pending" or "throttled"
}

# ── eForms parser: lists individual tenderers (winners + losers) ─────────────

parse_eforms <- function(xml, notice_id, ns) {

  lot_tender_nodes <- xml2::xml_find_all(xml, "//efac:NoticeResult/efac:LotTender", ns)
  tender_ids       <- xml2::xml_text(xml2::xml_find_first(lot_tender_nodes, "cbc:ID", ns))

  winner_ids <- unique(xml2::xml_text(
    xml2::xml_find_all(xml, "//efac:SettledContract/efac:LotTender/cbc:ID", ns)))

  non_winner_ids <- setdiff(tender_ids, winner_ids)

  named <- NULL
  if (length(non_winner_ids) > 0) {

    tender_to_party <- setNames(
      xml2::xml_text(xml2::xml_find_first(lot_tender_nodes, "efac:TenderingParty/cbc:ID", ns)),
      tender_ids)

    tender_to_value <- setNames(
      xml2::xml_text(xml2::xml_find_first(lot_tender_nodes, "cac:LegalMonetaryTotal/cbc:PayableAmount", ns)),
      tender_ids)

    amount_nodes <- xml2::xml_find_first(lot_tender_nodes, "cac:LegalMonetaryTotal/cbc:PayableAmount", ns)
    tender_to_currency <- setNames(
      sapply(amount_nodes, function(n) {
        if (inherits(n, "xml_node")) xml2::xml_attr(n, "currencyID") else NA_character_
      }), tender_ids)

    tendering_party_nodes <- xml2::xml_find_all(xml, "//efac:NoticeResult/efac:TenderingParty", ns)
    party_to_org <- setNames(
      xml2::xml_text(xml2::xml_find_first(tendering_party_nodes, "efac:Tenderer/cbc:ID", ns)),
      xml2::xml_text(xml2::xml_find_first(tendering_party_nodes, "cbc:ID", ns)))

    org_nodes <- xml2::xml_find_all(xml, "//efac:Organization/efac:Company", ns)
    org_to_name <- setNames(
      xml2::xml_text(xml2::xml_find_first(org_nodes, "cac:PartyName/cbc:Name", ns)),
      xml2::xml_text(xml2::xml_find_first(org_nodes, "cac:PartyIdentification/cbc:ID", ns)))

    named <- data.frame(
      notice_id = notice_id,
      company   = unname(org_to_name[party_to_org[tender_to_party[non_winner_ids]]]),
      bid_value = as.numeric(tender_to_value[non_winner_ids]),
      currency  = unname(tender_to_currency[non_winner_ids]),
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  list(
    schema              = "eforms",
    n_tenders_received  = length(tender_ids),
    n_winners           = length(winner_ids),
    non_winners_listed  = length(non_winner_ids) > 0,
    n_non_winners_named = if (is.null(named)) 0L else sum(!is.na(named$company)),
    named               = named
  )
}

# ── Legacy TED_EXPORT parser: count only (losers never individually named) ────
# Uses local-name() XPath to tolerate the many R2.0.x / form variants.
# NB_TENDERS_RECEIVED exists on F03_2014+ forms; very old forms may lack it, in
# which case n_tenders_received is NA (reported honestly, not guessed).

parse_legacy <- function(xml, notice_id) {

  received <- suppressWarnings(as.integer(xml2::xml_text(
    xml2::xml_find_all(xml, "//*[local-name()='NB_TENDERS_RECEIVED']"))))
  n_received <- if (length(received) && any(!is.na(received))) sum(received, na.rm = TRUE) else NA_integer_

  n_winners <- length(xml2::xml_find_all(xml, "//*[local-name()='CONTRACTOR']"))

  list(
    schema              = "legacy",
    n_tenders_received  = n_received,
    n_winners           = n_winners,
    non_winners_listed  = FALSE,          # legacy schema never names losing bidders
    n_non_winners_named = 0L,
    named               = NULL
  )
}

# ── Orchestrate one notice → (summary row, named-losers table) ────────────────

extract_notice <- function(notice_id, ns, cache_dir, max_retries, base_delay) {

  empty <- function(status, schema = NA_character_) {
    list(summary = data.frame(
      notice_id = notice_id, schema = schema, fetch_status = status,
      n_tenders_received = NA_integer_, n_winners = NA_integer_,
      had_non_winners = NA, non_winners_listed = NA, n_non_winners_named = NA_integer_,
      stringsAsFactors = FALSE), named = NULL)
  }

  res <- fetch_notice_xml(notice_id, cache_dir, max_retries, base_delay)
  if (res$status != "ok" || is.null(res$xml)) return(empty(res$status))

  root <- xml2::xml_name(res$xml)
  parsed <- tryCatch({
    if (root %in% c("ContractAwardNotice", "ContractNotice")) {
      parse_eforms(res$xml, notice_id, ns)
    } else if (root == "TED_EXPORT") {
      parse_legacy(res$xml, notice_id)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(parsed)) return(empty("parse_error", schema = root))

  nr <- parsed$n_tenders_received
  nw <- parsed$n_winners
  had <- if (is.na(nr)) NA
         else if (!is.na(nw) && nw > 0) nr > nw
         else nr > 1

  list(
    summary = data.frame(
      notice_id           = notice_id,
      schema              = parsed$schema,
      fetch_status        = "ok",
      n_tenders_received  = nr,
      n_winners           = nw,
      had_non_winners     = had,
      non_winners_listed  = parsed$non_winners_listed,
      n_non_winners_named = parsed$n_non_winners_named,
      stringsAsFactors    = FALSE),
    named = parsed$named
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Pipeline. Guarded so the file can be `source()`d to load just the functions
# (for testing / reuse) by first defining SKIP_TED_RUN.
# ─────────────────────────────────────────────────────────────────────────────

if (!exists("SKIP_TED_RUN")) {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(save_dir,  showWarnings = FALSE, recursive = TRUE)

  data_ot <- readRDS(data_file)
  data_ot$notice_id <- derive_notice_id(data_ot$tender_publications_lastContractAwardUrl)

  unique_ids <- na.omit(unique(data_ot$notice_id))
  n_missing  <- sum(is.na(derive_notice_id(unique(data_ot$tender_publications_lastContractAwardUrl))))
  message(sprintf("Rows: %d | notices with a derivable notice_id: %d",
                  nrow(data_ot), length(unique_ids)))

  chunks <- split(unique_ids, ceiling(seq_along(unique_ids) / chunk_size))
  message(sprintf("Processing %d notices in %d chunks", length(unique_ids), length(chunks)))

  plan(multisession, workers = n_workers)
  handlers(global = TRUE)
  handlers("progress")

  # A notice is "done" only if its status is terminal. "pending"/"throttled"/
  # network/parse failures are transient and get retried on the next run.
  terminal <- c("ok", "not_found")

  for (i in seq_along(chunks)) {

    save_path <- file.path(save_dir, sprintf("%s%03d.rds", out_prefix, i))
    ids_i     <- chunks[[i]]

    prev     <- if (file.exists(save_path)) readRDS(save_path) else NULL
    done_ids <- if (!is.null(prev)) {
      prev$summary$notice_id[prev$summary$fetch_status %in% terminal]
    } else character(0)
    todo_ids <- setdiff(ids_i, done_ids)

    if (length(todo_ids) == 0) {
      message(sprintf("Chunk %d / %d complete, skipping", i, length(chunks)))
      next
    }

    message(sprintf("Chunk %d / %d: %d notices (%d already done, %d to (re)fetch)",
                    i, length(chunks), length(ids_i), length(done_ids), length(todo_ids)))

    with_progress({
      p <- progressor(along = todo_ids)
      res <- future_map(todo_ids, function(id) {
        p()
        extract_notice(id, ns, cache_dir, max_retries, base_delay)
      }, .options = furrr_options(seed = TRUE))
    })

    new_summary <- bind_rows(lapply(res, `[[`, "summary"))
    new_named   <- bind_rows(lapply(res, `[[`, "named"))

    # Merge freshly fetched rows with the previously-completed (terminal) ones.
    if (!is.null(prev)) {
      keep_summary <- prev$summary[prev$summary$notice_id %in% done_ids, , drop = FALSE]
      keep_named   <- if (!is.null(prev$named) && nrow(prev$named) > 0) {
        prev$named[prev$named$notice_id %in% done_ids, , drop = FALSE]
      } else NULL
      chunk_summary <- bind_rows(keep_summary, new_summary)
      chunk_named   <- bind_rows(keep_named, new_named)
    } else {
      chunk_summary <- new_summary
      chunk_named   <- new_named
    }

    saveRDS(list(summary = chunk_summary, named = chunk_named), save_path)
    n_ok <- sum(chunk_summary$fetch_status %in% terminal)
    message(sprintf("Chunk %d saved (%d / %d terminal)", i, n_ok, nrow(chunk_summary)))
  }

  plan(sequential)

  # ── Combine ────────────────────────────────────────────────────────────────

  chunk_files <- list.files(save_dir, pattern = paste0("^", out_prefix, ".*\\.rds$"),
                            full.names = TRUE)
  chunks_read <- lapply(chunk_files, readRDS)

  notice_indicator  <- bind_rows(lapply(chunks_read, `[[`, "summary"))
  non_winners_named <- bind_rows(lapply(chunks_read, `[[`, "named"))

  # ── Diagnostics: make failures & schema coverage visible ─────────────────────

  notice_indicator$year <- suppressWarnings(as.integer(sub(".*-", "", notice_indicator$notice_id)))
  message("\nFetch status by year:")
  print(table(year = notice_indicator$year, status = notice_indicator$fetch_status))
  message("\nNon-winners listed, by schema (eForms only can name losers):")
  print(table(schema = notice_indicator$schema,
              listed = notice_indicator$non_winners_listed, useNA = "ifany"))
  message("\nHad non-winners (count-based), by schema:")
  print(table(schema = notice_indicator$schema,
              had    = notice_indicator$had_non_winners, useNA = "ifany"))

  # ── Join the per-notice indicator back onto the tender rows ──────────────────

  data_ot <- left_join(
    data_ot,
    select(notice_indicator, notice_id, schema, fetch_status,
           n_tenders_received, n_winners, had_non_winners,
           non_winners_listed, n_non_winners_named),
    by = "notice_id"
  )
}
