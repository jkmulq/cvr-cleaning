# Extract, from the underlying TED notice XML, a standalone reference dataset keyed
# by TED notice_id: party CVRs (buyers, winners, non-winning bidders), amounts
# (notice + per-lot, estimated + awarded), lot metadata (CPV, contract nature,
# tenders received), procedure/framework/DPS flags, and every date across the
# award -> competition -> planning lineage. Independent of OpenTender/DIGIWHIST's own
# extraction, so it can be used as a cross-check ("another comparison point") against
# the CVRs / amounts / dates already carried in the KFST and OpenTender datasets.
#
# OUTPUTS (data/intermediates/ted/):
#   ted_extracted_parties.{rds,csv}  LONG - one row per (notice_id, role, org):
#     notice_id, schema, role (buyer|winner|bidder), lot, tender_id,
#     name, cvr_raw, cvr (clean 8-digit DK, else NA), country (ISO3/2), amount,
#     currency
#   ted_extracted_lots.{rds,csv}     LONG - one row per (notice_id, lot):
#     notice_id, schema, lot, lot_title, cpv_main, contract_type
#     (works|supplies|services), lot_estimated_value, lot_awarded_value, currency,
#     n_tenders_received
#   ted_extracted_cvrs.{rds,csv}     NOTICE GRAIN - one row per notice_id:
#     CVRs per role (";"-joined) + counts; amounts (tender_amount = sum of winning
#     tenders; amount_awarded = notice total; amount_estimated); cpv_main;
#     contract_type; n_tenders_received; n_lots; procedure_type/procedure_group/
#     is_dps/is_framework/direct_award (from the lineage); lot_ids + lot_awarded_values
#     (aligned ";"-lists); and key dates (award dispatch/publication, contract-award,
#     tender-receipt deadline)
#   ted_extracted_dates.{rds,csv}    LONG - one row per (award notice, level, date):
#     award_notice_id, notice_level (award|competition|planning), notice_id,
#     date_field, date_value (raw), date_iso (YYYY-MM-DD). Every date-bearing field
#     across the lineage, so competition/planning dates (e.g. the tender-submission
#     deadline) attach to the same award notice_id as the CVRs.
#
# COVERAGE LIMITS (schema-inherent, reported honestly, not guessed):
#   * Non-winning bidders are eForms-only (2024+). The legacy TED_EXPORT schema
#     (2011-2023) never names losing bidders - only a count - so bidder_* is empty
#     there.
#   * Amounts vary: notice-level totals are fairly complete; legacy per-lot values
#     and estimated values are patchy (form-era dependent).
#   * procedure/framework/DPS come from the lineage (legacy competition notices);
#     eForms award notices carry no OJS lineage ref, so those flags are NA for them.
#   * Competition/planning dates depend on the notice_links.rds lineage + the
#     competition_xml/ & planning_xml/ caches (built by the ted_dates_* chain).
#
# Notices = every derivable notice_id from the OpenTender award URLs. Cache-first
# (reuses ted_1_extract_notices.R's fetcher + the ted_dates_* caches), so populated
# caches make this a purely offline parse.
#   Env: TED_EXTRACT_SAMPLE_SIZE  test on N random notices (default: all)
#        TED_EXTRACT_SEED         seed for the sample (default 123)

source("config.R")
suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(xml2)
}))

# Reuse the TED fetch library + the date-lineage helpers without running any
# pipeline: ted_dates_utils.R sources ted_1_extract_notices.R (SKIP_TED_RUN) for
# fetch_notice_xml / derive_notice_id / ns / cache_dir / retries, and adds the
# per-level cache dirs (award_cache_dir / comp_cache_dir / planning_cache_dir),
# the notice_links lineage path (links_rds), read_txt(), and grab1().
source(file.path(PROJECT_DIR, "code", "scraping", "ted_dates_utils.R"))

out_parties_rds <- file.path(dirs$intermediates, "ted", "ted_extracted_parties.rds")
out_parties_csv <- file.path(dirs$intermediates, "ted", "ted_extracted_parties.csv")
out_notice_rds  <- file.path(dirs$intermediates, "ted", "ted_extracted_cvrs.rds")
out_notice_csv  <- file.path(dirs$intermediates, "ted", "ted_extracted_cvrs.csv")
out_dates_rds   <- file.path(dirs$intermediates, "ted", "ted_extracted_dates.rds")
out_dates_csv   <- file.path(dirs$intermediates, "ted", "ted_extracted_dates.csv")
out_lots_rds    <- file.path(dirs$intermediates, "ted", "ted_extracted_lots.rds")
out_lots_csv    <- file.path(dirs$intermediates, "ted", "ted_extracted_lots.csv")

# ── Helpers ───────────────────────────────────────────────────────────────────

# Namespace-agnostic descendant text getters for the legacy TED_EXPORT forms
# (their default-namespace URI varies by year, so match on local-name()).
ln1 <- function(node, name)
  xml_text(xml_find_first(node, sprintf('.//*[local-name()="%s"]', name)))
ln1_any <- function(node, names) {
  for (nm in names) { v <- ln1(node, nm); if (!is.na(v) && v != "") return(v) }
  NA_character_
}

# Normalise an extracted org id to a clean 8-digit Danish CVR (strip a DK prefix /
# spaces / punctuation); NA when the result is not exactly 8 digits (foreign ids,
# VAT numbers, blanks).
clean_cvr <- function(x) {
  d <- gsub("[^0-9]", "", ifelse(is.na(x), "", x))
  ifelse(nchar(d) == 8L, d, NA_character_)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Every date-bearing field (tags containing DATE / DEADLINE) in one notice's XML
# text -> data.table(date_field, date_value). Handles flat YYYYMMDD and nested
# <YEAR>/<MONTH>/<DAY>. Copied from ted_dates_3_extract.R (that file runs a pipeline
# on source, so its functions can't be reused by sourcing it). grab1() comes from
# ted_dates_utils.R.
extract_all_dates <- function(txt) {
  empty <- data.table(date_field = character(), date_value = character())
  if (!nzchar(txt)) return(empty)
  tags <- unique(gsub("^<|[ >/]$", "",
    regmatches(txt, gregexpr("<([A-Z0-9_]*(?:DATE|DEADLINE)[A-Z0-9_]*)[ >/]", txt, perl = TRUE))[[1]]))
  if (!length(tags)) return(empty)
  out <- vector("list", length(tags))
  for (k in seq_along(tags)) {
    tg <- tags[k]
    blocks <- regmatches(txt, gregexpr(sprintf("(?s)<%s[^>]*>.*?</%s>", tg, tg), txt, perl = TRUE))[[1]]
    vals <- character(0)
    for (b in blocks) {
      v <- grab1(b, tg)
      if (is.na(v) || !grepl("[0-9]", v)) {                # else nested Y/M/D
        y <- grab1(b, "YEAR"); m <- grab1(b, "MONTH"); d <- grab1(b, "DAY")
        v <- if (!is.na(y)) paste(y, m %||% "", d %||% "", sep = "-") else NA_character_
      }
      if (!is.na(v) && nzchar(v)) vals <- c(vals, v)
    }
    if (length(vals)) out[[k]] <- data.table(date_field = tg, date_value = unique(vals))
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out)) rbindlist(out) else empty
}

# Normalise a raw date value to YYYY-MM-DD (NA if not parseable).
to_iso <- function(v) {
  v <- trimws(as.character(v)); out <- rep(NA_character_, length(v))
  flat <- grepl("^[0-9]{8}", v)
  out[flat] <- sprintf("%s-%s-%s", substr(v[flat], 1, 4), substr(v[flat], 5, 6), substr(v[flat], 7, 8))
  nest <- !flat & grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", v)
  if (any(nest)) {
    p <- tstrsplit(v[nest], "-", fixed = TRUE)
    out[nest] <- sprintf("%04d-%02d-%02d", as.integer(p[[1]]), as.integer(p[[2]]), as.integer(p[[3]]))
  }
  out
}

# eForms org directory: ORG id -> name / CVR (raw CompanyID) / country (ISO3).
# Used for every role; country helps explain a missing (foreign) CVR.
eforms_org_lookup <- function(xml) {
  o <- xml_find_all(xml, "//efac:Organization/efac:Company", ns)
  id   <- xml_text(xml_find_first(o, "cac:PartyIdentification/cbc:ID", ns))
  nm   <- xml_text(xml_find_first(o, "cac:PartyName/cbc:Name", ns))
  cvr  <- xml_text(xml_find_first(o, "cac:PartyLegalEntity/cbc:CompanyID", ns))
  ctry <- xml_text(xml_find_first(o, ".//cbc:IdentificationCode[@listName='country']", ns))
  list(name = setNames(nm, id), cvr = setNames(cvr, id), country = setNames(ctry, id))
}

# Empty long-parties skeleton (kept identical everywhere so rbindlist never fights
# over columns / types).
empty_parties <- function()
  data.table(notice_id = character(), role = character(), lot = character(),
             tender_id = character(), name = character(), cvr_raw = character(),
             country = character(), amount = numeric(), currency = character())

# COUNTRY attribute (VALUE="DK") on a legacy node's descendant, "" if absent.
legacy_country <- function(node) {
  n <- xml_find_first(node, './/*[local-name()="COUNTRY"]')
  if (inherits(n, "xml_node")) { v <- xml_attr(n, "VALUE"); if (is.na(v)) "" else v } else ""
}

# ── eForms parser: buyer + winners + non-winning bidders (all with CVR) ────────
# Every LotTender is a tender; those referenced by a LotResult with
# winner-selection-status = "selec-w" are winners, the rest are non-winning
# bidders. A tendering party can carry several Tenderer org ids (a consortium),
# each of which becomes its own row (same tender_id / amount / lot).
parse_eforms_parties <- function(xml, notice_id) {
  org <- eforms_org_lookup(xml)

  lt <- xml_find_all(xml, "//efac:NoticeResult/efac:LotTender", ns)
  if (length(lt)) {
    lt_id    <- xml_text(xml_find_first(lt, "cbc:ID", ns))
    lt_party <- setNames(xml_text(xml_find_first(lt, "efac:TenderingParty/cbc:ID", ns)), lt_id)
    lt_lot   <- setNames(xml_text(xml_find_first(lt, "efac:TenderLot/cbc:ID", ns)), lt_id)  # each tender's lot
    amt_node <- xml_find_first(lt, "cac:LegalMonetaryTotal/cbc:PayableAmount", ns)
    lt_amt   <- setNames(suppressWarnings(as.numeric(xml_text(amt_node))), lt_id)
    lt_cur   <- setNames(vapply(seq_along(lt), function(i) {
      n <- amt_node[[i]]; if (inherits(n, "xml_node")) xml_attr(n, "currencyID") else NA_character_
    }, character(1)), lt_id)

    tp <- xml_find_all(xml, "//efac:NoticeResult/efac:TenderingParty", ns)
    tp_orgs <- lapply(tp, function(p) xml_text(xml_find_all(p, "efac:Tenderer/cbc:ID", ns)))
    names(tp_orgs) <- xml_text(xml_find_first(tp, "cbc:ID", ns))

    # WINNERS = tenders with a concluded (settled) contract. Do NOT use the LotResult's
    # LotTender list: a LotResult references EVERY received tender for the lot (winners
    # AND losers), so that flags losing tenderers as winners. Verified on 00006213-2026
    # (2 winners of 6) and 00376656-2025 (18 of 36). Fallback: if the notice records no
    # settled contracts at all, fall back to any selec-w LotResult's tenders so a
    # genuine award is not dropped (e.g. a framework establishment with no call-offs).
    win_ten <- unique(xml_text(xml_find_all(
      xml, "//efac:NoticeResult/efac:SettledContract/efac:LotTender/cbc:ID", ns)))
    win_ten <- win_ten[!is.na(win_ten) & win_ten != ""]
    if (!length(win_ten)) {
      win_ten <- unique(unlist(lapply(xml_find_all(xml, "//efac:LotResult", ns), function(r) {
        st <- xml_text(xml_find_first(r, './/cbc:TenderResultCode[@listName="winner-selection-status"]', ns))
        if (identical(st, "selec-w")) xml_text(xml_find_all(r, "efac:LotTender/cbc:ID", ns)) else character(0)
      })))
      win_ten <- win_ten[!is.na(win_ten) & win_ten != ""]
    }

    tenders <- rbindlist(lapply(lt_id, function(t) {
      party  <- unname(lt_party[t])
      orgids <- if (!is.na(party) && party %in% names(tp_orgs)) tp_orgs[[party]] else character(0)
      if (!length(orgids)) return(NULL)
      role <- if (t %in% win_ten) "winner" else "bidder"
      data.table(notice_id = notice_id, role = role,
                 lot       = unname(lt_lot[t]),   # from the tender's own TenderLot (winners + losers)
                 tender_id = t,
                 name    = ifelse(is.na(org$name[orgids]),    "", org$name[orgids]),
                 cvr_raw = ifelse(is.na(org$cvr[orgids]),     "", org$cvr[orgids]),
                 country = ifelse(is.na(org$country[orgids]), "", org$country[orgids]),
                 amount = unname(lt_amt[t]), currency = unname(lt_cur[t]))
    }), fill = TRUE)
  } else tenders <- empty_parties()

  buyer_ids <- xml_text(xml_find_all(
    xml, "//cac:ContractingParty/cac:Party/cac:PartyIdentification/cbc:ID", ns))
  buyers <- if (length(buyer_ids)) data.table(
    notice_id = notice_id, role = "buyer", lot = NA_character_, tender_id = NA_character_,
    name    = ifelse(is.na(org$name[buyer_ids]),    "", org$name[buyer_ids]),
    cvr_raw = ifelse(is.na(org$cvr[buyer_ids]),     "", org$cvr[buyer_ids]),
    country = ifelse(is.na(org$country[buyer_ids]), "", org$country[buyer_ids]),
    amount = NA_real_, currency = NA_character_) else empty_parties()

  rbindlist(list(buyers, tenders), use.names = TRUE, fill = TRUE)
}

# ── Legacy TED_EXPORT parser: buyer + winners (no bidders in this schema) ──────
# Winners live in AWARD_CONTRACT (F03) / AWARD_OF_CONTRACT (old) blocks; blocks
# marked not-awarded contribute nothing. CVR = NATIONALID; amount = VAL_TOTAL.
parse_legacy_parties <- function(xml, notice_id) {
  ac <- xml_find_all(
    xml, '//*[local-name()="AWARD_CONTRACT" or local-name()="AWARD_OF_CONTRACT"]')

  winners <- rbindlist(lapply(seq_along(ac), function(k) {
    a <- ac[[k]]
    if (length(xml_find_all(a, paste0(
      './/*[local-name()="NO_AWARDED_CONTRACT"',
      ' or local-name()="PROCUREMENT_UNSUCCESSFUL"',
      ' or local-name()="PROCUREMENT_DISCONTINUED"]'))))
      return(NULL)
    lot <- ln1(a, "LOT_NO"); if (is.na(lot)) lot <- ln1(a, "LOT_NUMBER")
    ctr <- xml_find_all(
      a, './/*[local-name()="CONTRACTOR" or local-name()="ECONOMIC_OPERATOR_NAME_ADDRESS"]')
    if (!length(ctr)) return(NULL)
    amt_node <- xml_find_first(a, './/*[local-name()="VAL_TOTAL" or local-name()="VALUE"]')
    amt <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", xml_text(amt_node))))
    cur <- if (inherits(amt_node, "xml_node")) xml_attr(amt_node, "CURRENCY") else NA_character_
    data.table(
      notice_id = notice_id, role = "winner", lot = ifelse(is.na(lot), "", lot),
      tender_id = paste0(notice_id, "-ac", k),
      name    = vapply(ctr, ln1_any, character(1), names = c("OFFICIALNAME", "ORGANISATION")),
      cvr_raw = vapply(ctr, function(c) { v <- ln1(c, "NATIONALID"); if (is.na(v)) "" else v },
                       character(1)),
      country = vapply(ctr, legacy_country, character(1)),
      amount = amt, currency = cur)
  }), fill = TRUE)
  if (nrow(winners)) winners[is.na(name), name := ""]

  cb <- xml_find_all(
    xml, '//*[local-name()="ADDRESS_CONTRACTING_BODY" or local-name()="CA_CE_CONCESSIONAIRE_PROFILE"]')
  buyers <- if (length(cb)) rbindlist(lapply(cb, function(b) data.table(
    notice_id = notice_id, role = "buyer", lot = NA_character_, tender_id = NA_character_,
    name    = { v <- ln1_any(b, c("OFFICIALNAME", "ORGANISATION")); ifelse(is.na(v), "", v) },
    cvr_raw = { v <- ln1(b, "NATIONALID"); ifelse(is.na(v), "", v) },
    country = legacy_country(b),
    amount = NA_real_, currency = NA_character_)), fill = TRUE) else empty_parties()

  rbindlist(list(buyers, winners), use.names = TRUE, fill = TRUE)
}

# One-row notice-level metadata (shared shape for both schemas).
meta_row <- function(notice_id, amount_awarded, amount_estimated, currency,
                     cpv_main, contract_type, n_tenders_received, n_lots)
  data.table(notice_id = notice_id, amount_awarded = amount_awarded,
             amount_estimated = amount_estimated, currency = currency,
             cpv_main = cpv_main, contract_type = contract_type,
             n_tenders_received = n_tenders_received, n_lots = n_lots)

nz1 <- function(x) { x <- x[!is.na(x) & x != ""]; if (length(x)) x[1] else NA_character_ }
# Always returns double so a per-group aggregate never mixes integer sums with a
# double NA (data.table requires one type across groups).
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else as.numeric(sum(x, na.rm = TRUE))

# ── eForms notice + lot metadata: values, CPV, contract nature, tenders ────────
parse_eforms_meta <- function(xml, notice_id) {
  amt_node <- xml_find_first(xml, "//efac:NoticeResult/cbc:TotalAmount", ns)
  amount_awarded <- suppressWarnings(as.numeric(xml_text(amt_node)))
  currency <- if (inherits(amt_node, "xml_node")) xml_attr(amt_node, "currencyID") else NA_character_

  lots <- xml_find_all(xml, "//cac:ProcurementProjectLot", ns)
  lot_dt <- if (length(lots)) rbindlist(lapply(lots, function(L) {
    proj <- xml_find_first(L, "cac:ProcurementProject", ns)
    est  <- xml_find_first(proj, ".//cbc:EstimatedOverallContractAmount", ns)
    data.table(
      notice_id = notice_id,
      lot       = xml_text(xml_find_first(L, "cbc:ID[@schemeName='Lot']", ns)),
      lot_title = xml_text(xml_find_first(proj, "cbc:Name", ns)),
      cpv_main  = xml_text(xml_find_first(proj, ".//cbc:ItemClassificationCode[@listName='cpv']", ns)),
      contract_type = xml_text(xml_find_first(proj, ".//cbc:ProcurementTypeCode[@listName='contract-nature']", ns)),
      lot_estimated_value = suppressWarnings(as.numeric(xml_text(est))),
      currency = if (inherits(est, "xml_node")) xml_attr(est, "currencyID") else currency)
  }), fill = TRUE) else data.table()

  lr <- xml_find_all(xml, "//efac:LotResult", ns)
  if (length(lr) && nrow(lot_dt)) {
    lrt <- rbindlist(lapply(lr, function(r) {
      lid <- xml_text(xml_find_first(r, "efac:TenderLot/cbc:ID", ns))
      nt <- NA_integer_
      for (s in xml_find_all(r, ".//*[local-name()='ReceivedSubmissionsStatistics']")) {
        if (identical(xml_text(xml_find_first(s, ".//*[local-name()='StatisticsCode']")), "tenders")) {
          nt <- suppressWarnings(as.integer(xml_text(
            xml_find_first(s, ".//*[local-name()='StatisticsNumeric']")))); break
        }
      }
      data.table(lot = lid, n_tenders_received = nt)
    }), fill = TRUE)[!is.na(lot), .(n_tenders_received = sum_or_na(n_tenders_received)), by = lot]
    lot_dt <- merge(lot_dt, lrt, by = "lot", all.x = TRUE)
  } else if (nrow(lot_dt)) lot_dt[, n_tenders_received := NA_integer_]

  meta <- meta_row(notice_id, amount_awarded,
                   if (nrow(lot_dt)) sum_or_na(lot_dt$lot_estimated_value) else NA_real_, currency,
                   if (nrow(lot_dt)) nz1(lot_dt$cpv_main) else NA_character_,
                   if (nrow(lot_dt)) nz1(lot_dt$contract_type) else NA_character_,
                   if (nrow(lot_dt)) sum_or_na(lot_dt$n_tenders_received) else NA_real_,
                   if (nrow(lot_dt)) nrow(lot_dt) else NA_integer_)
  list(meta = meta, lots = lot_dt)
}

# ── Legacy notice + lot metadata (per-lot values are best-effort: coverage varies
#    across form eras; notice-level VAL_TOTAL / VAL_ESTIMATED_TOTAL are reliable) ──
parse_legacy_meta <- function(xml, notice_id) {
  vt <- xml_find_first(xml, "//*[local-name()='VALUE'][@TYPE='PROCUREMENT_TOTAL']")
  if (!inherits(vt, "xml_node")) vt <- xml_find_first(xml, "//*[local-name()='VAL_TOTAL']")
  amount_awarded <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", xml_text(vt))))
  currency <- if (inherits(vt, "xml_node")) xml_attr(vt, "CURRENCY") else NA_character_
  est <- xml_find_first(xml, "//*[local-name()='VAL_ESTIMATED_TOTAL']")
  amount_estimated <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", xml_text(est))))
  cpv_node <- xml_find_first(xml, "//*[local-name()='ORIGINAL_CPV' or local-name()='CPV_CODE']")
  cpv_main <- if (inherits(cpv_node, "xml_node")) xml_attr(cpv_node, "CODE") else NA_character_
  tc <- xml_find_first(xml, "//*[local-name()='TYPE_CONTRACT']")
  contract_type <- if (inherits(tc, "xml_node")) { v <- xml_attr(tc, "CTYPE"); if (is.na(v)) xml_text(tc) else v } else NA_character_
  if (is.na(contract_type) || contract_type == "") {   # older forms use NC_CONTRACT_NATURE text
    nc <- xml_find_first(xml, "//*[local-name()='NC_CONTRACT_NATURE']")
    if (inherits(nc, "xml_node")) contract_type <- xml_text(nc)
  }
  nt <- suppressWarnings(as.integer(xml_text(xml_find_all(xml, "//*[local-name()='NB_TENDERS_RECEIVED']"))))
  n_tenders <- if (length(nt) && any(!is.na(nt))) sum(nt, na.rm = TRUE) else NA_real_

  ac <- xml_find_all(xml, '//*[local-name()="AWARD_CONTRACT" or local-name()="AWARD_OF_CONTRACT"]')
  lot_dt <- if (length(ac)) rbindlist(lapply(ac, function(a) {
    lot <- ln1(a, "LOT_NO"); if (is.na(lot)) lot <- ln1(a, "LOT_NUMBER")
    vtl <- xml_find_first(a, './/*[local-name()="VAL_TOTAL"]')
    data.table(
      notice_id = notice_id, lot = ifelse(is.na(lot), "", lot),
      lot_title = ln1(a, "TITLE"), cpv_main = cpv_main, contract_type = contract_type,
      lot_estimated_value = NA_real_,
      lot_awarded_value = suppressWarnings(as.numeric(gsub("[^0-9.]", "", xml_text(vtl)))),
      currency = if (inherits(vtl, "xml_node")) xml_attr(vtl, "CURRENCY") else currency,
      n_tenders_received = suppressWarnings(as.integer(ln1(a, "NB_TENDERS_RECEIVED"))))
  }), fill = TRUE) else data.table()

  meta <- meta_row(notice_id, amount_awarded, amount_estimated, currency, cpv_main,
                   contract_type, n_tenders, if (length(ac)) length(ac) else NA_integer_)
  list(meta = meta, lots = lot_dt)
}

# Fetch (cache-first) + dispatch on root element -> parties, meta (1 row), lots.
extract_notice_parties <- function(notice_id) {
  res <- suppressWarnings(fetch_notice_xml(notice_id, cache_dir, max_retries, base_delay))
  if (res$status != "ok" || is.null(res$xml))
    return(list(parties = empty_parties(), meta = NULL, lots = NULL,
                schema = NA_character_, fetch_status = res$status))

  root <- xml2::xml_name(res$xml)
  s <- if (root %in% c("ContractAwardNotice", "ContractNotice")) "eforms"
       else if (root == "TED_EXPORT") "legacy" else NA_character_
  if (is.na(s))
    return(list(parties = empty_parties(), meta = NULL, lots = NULL,
                schema = root, fetch_status = "parse_error"))

  pfun <- if (s == "eforms") parse_eforms_parties else parse_legacy_parties
  mfun <- if (s == "eforms") parse_eforms_meta    else parse_legacy_meta
  p <- tryCatch(pfun(res$xml, notice_id), error = function(e) NULL)
  if (is.null(p))
    return(list(parties = empty_parties(), meta = NULL, lots = NULL,
                schema = s, fetch_status = "parse_error"))
  # A metadata failure must NOT drop the parties: parse it separately, tolerate NULL.
  m <- tryCatch(mfun(res$xml, notice_id), error = function(e) NULL)

  if (nrow(p)) p[, schema := s]
  meta <- if (!is.null(m)) m$meta else NULL; if (!is.null(meta) && nrow(meta)) meta[, schema := s]
  lots <- if (!is.null(m)) m$lots else NULL; if (!is.null(lots) && nrow(lots)) lots[, schema := s]
  list(parties = p, meta = meta, lots = lots, schema = s, fetch_status = "ok")
}

# ── Run (guarded: define SKIP_TED_EXTRACT_RUN to source the functions only) ────
if (!exists("SKIP_TED_EXTRACT_RUN")) {

# Notices = the UNION of TED award notices referenced by OpenTender AND KFST (winner
# + buyer). Using OpenTender alone missed ~1,766 Danish TED awards that only KFST
# references, so both sources are pooled here. Ids come from each dataset's award-URL
# column (OT: tender_publications_lastContractAwardUrl; KFST: award_url).
notice_url_specs <- list(
  list(f = "clean_winner_data_ot.rds",   url = "tender_publications_lastContractAwardUrl"),
  list(f = "clean_buyer_data_ot.rds",    url = "tender_publications_lastContractAwardUrl"),
  list(f = "clean_winner_data_kfst.rds", url = "award_url"),
  list(f = "clean_buyer_data_kfst.rds",  url = "award_url"))
notice_ids <- unique(unlist(lapply(notice_url_specs, function(s) {
  p <- file.path(dirs$clean_data, s$f)
  if (!file.exists(p)) return(character(0))
  d <- readRDS(p)
  if (!s$url %in% names(d)) return(character(0))
  derive_notice_id(d[[s$url]])
})))
notice_ids <- notice_ids[!is.na(notice_ids)]

# Optional: restrict to notices already in the XML cache, skipping the slow TED fetch
# for any not yet pulled. Lets the corrected extraction be validated on the cached set
# immediately; uncached notices are fetched + folded in on a later full run.
if (identical(Sys.getenv("TED_EXTRACT_CACHED_ONLY"), "true")) {
  n0 <- length(notice_ids)
  notice_ids <- notice_ids[is_cached(notice_ids, cache_dir)]
  message(sprintf("CACHED_ONLY: keeping %d of %d cached notices (skipping %d uncached).",
                  length(notice_ids), n0, n0 - length(notice_ids)))
}

samp <- suppressWarnings(as.integer(Sys.getenv("TED_EXTRACT_SAMPLE_SIZE", "")))
if (!is.na(samp) && samp > 0 && samp < length(notice_ids)) {
  set.seed(suppressWarnings(as.integer(Sys.getenv("TED_EXTRACT_SEED", "123"))))
  notice_ids <- sample(notice_ids, samp)
}
message(sprintf("Extracting party CVRs from %d TED notices (cache-first)...", length(notice_ids)))

res <- vector("list", length(notice_ids))
for (i in seq_along(notice_ids)) {
  res[[i]] <- tryCatch(extract_notice_parties(notice_ids[i]),
                       error = function(e) list(parties = empty_parties(), meta = NULL,
                                                lots = NULL, schema = NA_character_,
                                                fetch_status = "error"))
  if (i %% 1000 == 0) message(sprintf("  ...%d / %d", i, length(notice_ids)))
}

parties  <- rbindlist(lapply(res, `[[`, "parties"), use.names = TRUE, fill = TRUE)
meta_all <- rbindlist(Filter(Negate(is.null), lapply(res, `[[`, "meta")), use.names = TRUE, fill = TRUE)
lots_all <- rbindlist(Filter(Negate(is.null), lapply(res, `[[`, "lots")), use.names = TRUE, fill = TRUE)
# Normalise contract nature to {works, supplies, services} across eras/vocabularies
# (eForms "services", legacy CTYPE "SERVICES", legacy NC text "Service contract").
norm_ctype <- function(x) {
  x <- tolower(trimws(x))
  fifelse(grepl("work", x), "works",
    fifelse(grepl("suppl", x), "supplies",
      fifelse(grepl("servic", x), "services", x)))
}
if (nrow(meta_all)) meta_all[, contract_type := norm_ctype(contract_type)]
if (nrow(lots_all)) lots_all[, contract_type := norm_ctype(contract_type)]
status  <- data.table(
  notice_id    = notice_ids,
  schema       = vapply(res, function(r) if (is.null(r$schema)) NA_character_ else r$schema, character(1)),
  fetch_status = vapply(res, `[[`, character(1), "fetch_status"))

if (nrow(parties)) {
  parties[, cvr := clean_cvr(cvr_raw)]
  # Explicit winner/non-winner flag for the supplier side (buyers are NA).
  parties[, is_winner := fifelse(role == "winner", TRUE, fifelse(role == "bidder", FALSE, NA))]
  parties[, year := suppressWarnings(as.integer(sub(".*-", "", notice_id)))]
  setcolorder(parties, c("notice_id", "year", "schema", "role", "is_winner", "lot", "tender_id",
                         "name", "cvr_raw", "cvr", "country", "amount", "currency"))
}

# Notice-grain collapse: unique clean CVRs per role, plus the awarded total
# (summed over unique winning tenders so consortium members don't double-count).
join_cvr <- function(x) { u <- unique(x[!is.na(x) & x != ""]); if (!length(u)) NA_character_ else paste(u, collapse = ";") }
notice_summary <- parties[, {
  wr <- role == "winner"; br <- role == "buyer"; dr <- role == "bidder"
  wa <- unique(data.table(tid = tender_id[wr], a = amount[wr]))
  .(n_buyers    = sum(br),
    n_winners   = uniqueN(tender_id[wr]),
    n_bidders   = uniqueN(tender_id[dr]),
    buyer_cvrs  = join_cvr(cvr[br]),
    winner_cvrs = join_cvr(cvr[wr]),
    bidder_cvrs = join_cvr(cvr[dr]),
    tender_amount = if (all(is.na(wa$a))) NA_real_ else sum(wa$a, na.rm = TRUE),
    currency    = { cc <- currency[wr & !is.na(currency)]; if (length(cc)) cc[[1]] else NA_character_ })
}, by = notice_id]

# Left-join onto the full notice list so failed / empty notices still appear.
notice_summary <- merge(status, notice_summary, by = "notice_id", all.x = TRUE)
for (col in c("n_buyers", "n_winners", "n_bidders"))
  notice_summary[is.na(get(col)), (col) := 0L]
notice_summary[, year := suppressWarnings(as.integer(sub(".*-", "", notice_id)))]
setcolorder(notice_summary, c("notice_id", "year", "schema", "fetch_status", "tender_amount",
                              "currency", "n_buyers", "n_winners", "n_bidders",
                              "buyer_cvrs", "winner_cvrs", "bidder_cvrs"))

# ── Lot-level table + notice metadata. Per-lot awarded value: eForms is derived
#    from the winning tenders (parties); legacy carries it on the AWARD_CONTRACT
#    block (best-effort - coverage varies by form era).
lot_awarded <- if (nrow(parties)) parties[
  role == "winner" & !is.na(lot) & lot != "",
  .(a = { u <- unique(data.table(tender_id, amount)); sum_or_na(u$amount) }),
  by = .(notice_id, lot)] else data.table(notice_id = character(), lot = character(), a = numeric())

lots <- copy(lots_all)
if (nrow(lots)) {
  if (!"lot_awarded_value" %in% names(lots)) lots[, lot_awarded_value := NA_real_]
  lots[lot_awarded, on = .(notice_id, lot), lot_awarded_value := fcoalesce(lot_awarded_value, i.a)]
  lots[, year := suppressWarnings(as.integer(sub(".*-", "", notice_id)))]
  setcolorder(lots, c("notice_id", "year", "schema", "lot", "lot_title", "cpv_main",
                      "contract_type", "lot_estimated_value", "lot_awarded_value",
                      "currency", "n_tenders_received"))
}

# Enrich the notice summary: notice-level metadata (estimated/awarded totals, CPV,
# contract type, tender + lot counts), the procedure type / framework / DPS flags
# from the pre-built lineage, and an aligned per-lot awarded-value list.
if (nrow(meta_all))
  notice_summary <- merge(notice_summary, meta_all[, .(
    notice_id, amount_estimated, amount_awarded, cpv_main, contract_type,
    n_tenders_received, n_lots)], by = "notice_id", all.x = TRUE)
if (file.exists(links_rds))
  notice_summary <- merge(notice_summary, unique(as.data.table(readRDS(links_rds))[, .(
    notice_id = award_notice_id, procedure_type, procedure_group, is_dps,
    is_framework, direct_award)]), by = "notice_id", all.x = TRUE)
if (nrow(lots))
  notice_summary <- merge(notice_summary, lots[order(notice_id, lot), .(
    lot_ids = paste(lot, collapse = ";"),
    lot_awarded_values = paste(fifelse(is.na(lot_awarded_value), "",
                                       format(lot_awarded_value, scientific = FALSE, trim = TRUE)), collapse = ";")),
    by = notice_id], by = "notice_id", all.x = TRUE)

# ── Date lineage: every date from the award notice + its competition / planning
#    ancestors (award -> competition -> planning via notice_links.rds), keyed by the
#    same award notice_id as the CVRs above. Reuses the pre-built lineage + the
#    cached competition_xml/ and planning_xml/ notices (mirrors the ted_dates_* chain).
suppressWarnings(suppressPackageStartupMessages(library(parallel)))

extract_level_dates <- function(ids, cache, level) {
  ids <- unique(ids[!is.na(ids)])
  if (!length(ids)) return(data.table())
  message(sprintf("  dates: %d %s notices...", length(ids), level))
  setDTthreads(1L)
  res <- mclapply(ids, function(id) {
    dt <- extract_all_dates(read_txt(id, cache))
    if (!nrow(dt)) return(NULL)
    dt[, source_notice_id := id][]
  }, mc.cores = max(1L, detectCores() - 1L), mc.preschedule = TRUE)
  setDTthreads(0L)
  out <- rbindlist(res[!vapply(res, is.null, logical(1))], use.names = TRUE, fill = TRUE)
  if (nrow(out)) out[, date_iso := to_iso(date_value)]
  out
}

dates <- data.table(award_notice_id = character(), notice_level = character(),
                    notice_id = character(), date_field = character(),
                    date_value = character(), date_iso = character())
if (file.exists(links_rds)) {
  lin <- unique(as.data.table(readRDS(links_rds))[
    award_notice_id %chin% notice_ids,
    .(award_notice_id, competition_notice_id, planning_notice_id)])
  message(sprintf("Extracting the date lineage for %d award notices...", nrow(lin)))

  # Award dates for EVERY notice we pulled (their XML is cached after the main loop),
  # so the KFST-only notices not in the OT-built lineage still get award-level dates.
  # Competition/planning still come from the lineage (OT-derived).
  aw <- extract_level_dates(notice_ids,                award_cache_dir,    "award")
  co <- extract_level_dates(lin$competition_notice_id, comp_cache_dir,     "competition")
  pl <- extract_level_dates(lin$planning_notice_id,    planning_cache_dir, "planning")

  # Key each level's dates back to the award notice_id. A competition / planning
  # notice can serve several awards, so those are one-to-many joins via the lineage.
  awd <- if (nrow(aw)) aw[, .(award_notice_id = source_notice_id, notice_level = "award",
                              notice_id = source_notice_id, date_field, date_value, date_iso)]
  cod <- if (nrow(co)) merge(
    lin[!is.na(competition_notice_id), .(award_notice_id, notice_id = competition_notice_id)],
    co[, .(notice_id = source_notice_id, date_field, date_value, date_iso)],
    by = "notice_id", allow.cartesian = TRUE)[, .(award_notice_id, notice_level = "competition",
                                                  notice_id, date_field, date_value, date_iso)]
  pld <- if (nrow(pl)) merge(
    lin[!is.na(planning_notice_id), .(award_notice_id, notice_id = planning_notice_id)],
    pl[, .(notice_id = source_notice_id, date_field, date_value, date_iso)],
    by = "notice_id", allow.cartesian = TRUE)[, .(award_notice_id, notice_level = "planning",
                                                  notice_id, date_field, date_value, date_iso)]
  parts <- Filter(Negate(is.null), list(awd, cod, pld))
  if (length(parts)) {
    dates <- rbindlist(parts, use.names = TRUE, fill = TRUE)
    setorder(dates, award_notice_id, notice_level, date_field)
  }

  # Fold a few analytically-central dates into the summary, each taken from the
  # correct lineage level (so the award publication/dispatch aren't conflated with
  # the earlier competition/planning ones), earliest ISO per notice within a spec.
  # The full set of dates lives in ted_extracted_dates; these are convenience cols.
  key_specs <- list(
    date_award_dispatch    = list(level = "award",       fields = c("DS_DATE_DISPATCH", "DATE_DISPATCH_NOTICE", "NOTICE_DISPATCH_DATE")),
    date_award_publication = list(level = "award",       fields = c("DATE_PUB")),
    date_contract_award    = list(level = "award",       fields = c("CONTRACT_AWARD_DATE", "DATE_CONCLUSION_CONTRACT", "DATE_OF_CONTRACT_AWARD")),
    date_receipt_tenders   = list(level = "competition", fields = c("DATE_RECEIPT_TENDERS", "DT_DATE_FOR_SUBMISSION", "RECEIPT_LIMIT_DATE")))
  for (col in names(key_specs)) {
    sp  <- key_specs[[col]]
    agg <- dates[notice_level == sp$level & date_field %chin% sp$fields & !is.na(date_iso),
                 .(v = min(date_iso)), by = award_notice_id]
    notice_summary[, (col) := NA_character_]
    if (nrow(agg)) notice_summary[agg, on = .(notice_id = award_notice_id), (col) := i.v]
  }
} else message("notice_links.rds not found - skipping date lineage (run ted_dates_2_lineage.R first).")

dir.create(dirname(out_parties_rds), showWarnings = FALSE, recursive = TRUE)
saveRDS(parties, out_parties_rds);        fwrite(parties, out_parties_csv)
saveRDS(notice_summary, out_notice_rds);  fwrite(notice_summary, out_notice_csv)
saveRDS(lots, out_lots_rds);              fwrite(lots, out_lots_csv)
saveRDS(dates, out_dates_rds);            fwrite(dates, out_dates_csv)

# ── Diagnostics ───────────────────────────────────────────────────────────────
message("\n── Fetch status ──");        print(status[, .N, by = fetch_status][order(-N)])
message("── Schema ──");                print(status[, .N, by = schema][order(-N)])
if (nrow(parties)) {
  message("── Parties by role (rows | with clean CVR | with country) ──")
  print(parties[, .(rows = .N, with_cvr = sum(!is.na(cvr)),
                    with_country = sum(!is.na(country) & country != "")), by = role][order(-rows)])
  message(sprintf("Notices with >=1 winner CVR: %d | >=1 buyer CVR: %d | >=1 bidder CVR: %d",
                  notice_summary[!is.na(winner_cvrs), .N],
                  notice_summary[!is.na(buyer_cvrs), .N],
                  notice_summary[!is.na(bidder_cvrs), .N]))
}
if (nrow(meta_all)) {
  message("── Notice metadata coverage (non-NA / all notices) ──")
  for (c in c("amount_awarded", "amount_estimated", "cpv_main", "contract_type",
              "n_tenders_received", "procedure_group"))
    if (c %in% names(notice_summary))
      message(sprintf("  %-20s %d / %d", c, notice_summary[!is.na(get(c)), .N], nrow(notice_summary)))
}
if (nrow(lots))
  message(sprintf("── Lots: %d rows / %d notices | awarded value %d (%.0f%%) | estimated %d | title %d",
                  nrow(lots), uniqueN(lots$notice_id), lots[!is.na(lot_awarded_value), .N],
                  100 * mean(!is.na(lots$lot_awarded_value)), lots[!is.na(lot_estimated_value), .N],
                  lots[!is.na(lot_title) & lot_title != "", .N]))
if (nrow(dates)) {
  message("── Dates by lineage level (rows | notices | distinct fields) ──")
  print(dates[, .(rows = .N, notices = uniqueN(award_notice_id), fields = uniqueN(date_field)),
              by = notice_level][order(-rows)])
  message(sprintf("ISO-parseable date values: %.1f%%", 100 * mean(!is.na(dates$date_iso))))
}
message(sprintf("\nWritten:\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s",
                out_parties_rds, out_parties_csv, out_notice_rds, out_notice_csv,
                out_lots_rds, out_lots_csv, out_dates_rds, out_dates_csv))

} # end SKIP_TED_EXTRACT_RUN guard
