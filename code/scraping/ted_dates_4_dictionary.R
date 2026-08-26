# Build a TED field dictionary (XML element -> plain-English description) from the
# official EU Publications Office R2.0.9 schema label files, and annotate the
# extracted notice dates with it.
#
# Downloads two files from the TED schemas page and JOINS them:
#   Forms_Labels_R209.xlsx        label-key -> text (EN + other EU languages)
#   XML_Labels_Mapping_R209.zip   (XML Labels mapping R2.09.xlsx): per form,
#                                 XML element name -> Description + label-key
# Join on the label-key gives element -> Description (+ official EN label).
# Source: https://op.europa.eu/en/web/eu-vocabularies/e-procurement/tedschemas
#
# The coded-data-section date fields (DS_DATE_DISPATCH, DATE_PUB, ...) are TED
# internal metadata, NOT in the form-label mapping, so a small curated supplement
# (flagged source = "coded_data_curated") is added for them.
#
# OUTPUT
#   data/intermediates/ted/schema/ted_field_dictionary.{rds,csv}   element -> description
#   data/intermediates/ted/notice_dates_described.{rds,csv}        (if notice_dates.rds exists)

source("config.R")
suppressWarnings(suppressPackageStartupMessages({library(data.table); library(readxl)}))

ted_dir    <- file.path(dirs$intermediates, "ted")
schema_dir <- file.path(ted_dir, "schema")
dir.create(schema_dir, recursive = TRUE, showWarnings = FALSE)
lab_xlsx <- file.path(schema_dir, "Forms_Labels_R209.xlsx")
map_zip  <- file.path(schema_dir, "XML_Labels_Mapping_R209.zip")
map_xlsx <- file.path(schema_dir, "XML Labels mapping R2.09.xlsx")

lab_url <- "https://op.europa.eu/documents/3938058/9351229/Forms_Labels_R209.xlsx/ff1f70e3-7aad-1648-d564-559c49ee70c4?t=1637781832007"
map_url <- "https://op.europa.eu/documents/3938058/5358176/XML_Labels_Mapping_R209.zip/66222029-17af-8448-a193-93de964c16b3"

# ── 1. Download (cache-first) ─────────────────────────────────────────────────
if (!file.exists(lab_xlsx)) download.file(lab_url, lab_xlsx, mode = "wb", method = "libcurl", quiet = TRUE)
if (!file.exists(map_xlsx)) {
  if (!file.exists(map_zip)) download.file(map_url, map_zip, mode = "wb", method = "libcurl", quiet = TRUE)
  unzip(map_zip, exdir = schema_dir)
}

# ── 2. Mapping: element -> Description + label-key (all form sheets) ───────────
form_sheets <- grep("^F[0-9]", excel_sheets(map_xlsx), value = TRUE)
mp <- rbindlist(lapply(form_sheets, function(s) {
  d <- suppressWarnings(as.data.table(read_excel(map_xlsx, sheet = s, col_types = "text")))
  # columns are, by position: Form section | Field ID | Description | XML element name | Type | Label-key
  setnames(d, 1:6, c("section", "field_id", "description", "xml_element", "type_range", "label_key"))
  d[, .(form = s, description, xml_element, label_key)]
}), fill = TRUE)

# explode newline-separated element / label cells, keep clean element tokens
mp[, xml_element := gsub("\r", "", xml_element)]
mp[, label_key   := gsub("\r", "", label_key)]
elem <- mp[!is.na(xml_element),
           .(element = trimws(unlist(strsplit(xml_element, "\n")))),
           by = .(form, description, label_key)]
elem[, element := gsub("\\[[^]]*\\]", "", element)]   # drop [@attr="x"] predicates
elem <- elem[grepl("^[A-Z][A-Z0-9_]*$", element)]     # keep pure element names
elem[, label_key1 := trimws(tstrsplit(label_key, "\n")[[1]])]

# ── 3. Labels: label-key -> English text; join on ────────────────────────────
lab <- as.data.table(read_excel(lab_xlsx, sheet = "2014", col_types = "text"))
elem <- unique(lab[, .(label_key1 = Label, label_en = EN)])[elem, on = "label_key1"]

# ── 4. Collapse to one row per element (prefer the most specific description) ─
elem[, dlen := nchar(description)]
dict <- elem[order(-dlen)][, .(
  description = description[1],
  label_en    = label_en[!is.na(label_en)][1],
  forms       = paste(sort(unique(form)), collapse = ",")
), by = element]
dict[, source := "form_mapping"]

# ── 5. Supplement fields absent from the R2.0.9 form mapping ──────────────────
# Two groups, both clearly flagged in `source` so they can be filtered out:
#  (a) coded_data      - TED internal coded-data-section metadata (not a form field)
#  (b) legacy_pre2014  - old-directive (pre-2014) schema tags used by 2011-2013
#                        notices; described from the field name + standard-form concept
supplement <- rbindlist(list(
  data.table(source = "coded_data",
    element = c("DS_DATE_DISPATCH", "DATE_PUB", "DATE_OJ", "DELETION_DATE",
                "DT_DATE_FOR_SUBMISSION", "DD_DATE_REQUEST_DOCUMENT", "NOTICE_DISPATCH_DATE"),
    description = c("Date the notice was dispatched to the OJ (coded data section)",
                   "Date of publication in the OJ S / TED (coded data section)",
                   "Date of the referenced prior OJ S publication (coded data section)",
                   "Date the record is scheduled for deletion from TED (administrative)",
                   "Time limit for receipt of tenders / requests (coded data; incl. time)",
                   "Time limit for receipt of requests for documents (coded data)",
                   "Date of dispatch of the notice (coded data section)")),
  data.table(source = "legacy_pre2014",
    element = c("CONTRACT_AWARD_DATE", "DATE_OF_CONTRACT_AWARD", "RECEIPT_LIMIT_DATE",
                "START_DATE", "END_DATE", "PERIOD_WORK_DATE_STARTING", "PROCEDURE_DATE_STARTING",
                "DISPATCH_INVITATIONS_DATE", "DATE_LIMIT_RECEIPT_INTEREST",
                "DATE_LIMIT_RECEIPT_APPLICATION", "DATE_DECISION_JURY"),
    description = c("Date of contract award / conclusion",
                   "Date of contract award",
                   "Time limit for receipt of tenders or requests to participate",
                   "Start date of the contract / period of performance",
                   "End date of the contract / period of performance",
                   "Scheduled start of works or services",
                   "Estimated date for start of the award procedure",
                   "Estimated date of dispatch of invitations to tender / negotiate",
                   "Time limit for receipt of expressions of interest",
                   "Time limit for receipt of applications",
                   "Date of the jury's decision"))
), fill = TRUE)
supplement[, `:=`(label_en = NA_character_, forms = NA_character_)]
dict <- rbind(dict, supplement[!element %in% dict$element], use.names = TRUE)

setorder(dict, element)
saveRDS(dict, file.path(schema_dir, "ted_field_dictionary.rds"))
fwrite(dict, file.path(schema_dir, "ted_field_dictionary.csv"))
message(sprintf("Field dictionary: %d elements (form_mapping %d, coded_data %d, legacy_pre2014 %d) -> %s",
                nrow(dict), dict[source == "form_mapping", .N], dict[source == "coded_data", .N],
                dict[source == "legacy_pre2014", .N], file.path(schema_dir, "ted_field_dictionary.csv")))

# ── 6. Annotate notice_dates with the descriptions (if present) ───────────────
nd_rds <- file.path(ted_dir, "notice_dates.rds")
if (file.exists(nd_rds)) {
  nd <- as.data.table(readRDS(nd_rds))
  nd <- dict[, .(date_field = element, date_field_description = description, date_field_label_en = label_en)][
    nd, on = "date_field"]
  saveRDS(nd, file.path(ted_dir, "notice_dates_described.rds"))
  fwrite(nd, file.path(ted_dir, "notice_dates_described.csv"))
  message(sprintf("Annotated notice_dates: %.1f%% of rows carry a description -> notice_dates_described.{rds,csv}",
                  100 * mean(!is.na(nd$date_field_description))))
  miss <- nd[is.na(date_field_description), .N, by = date_field][order(-N)]
  if (nrow(miss)) { cat("\nDate fields still without a description (extend the coded supplement if needed):\n"); print(miss) }
} else {
  message("notice_dates.rds not found - run ted_dates_3_extract.R, then re-run this to annotate it.")
}
