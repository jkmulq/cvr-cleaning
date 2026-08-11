# Stage 2 of 2 for the TED notice lineage: BUILD notice_links from cached XML.
#
# Pure parsing + assembly, no network - reads only the caches populated by
# 1_2a_fetch_notices.R, so it can be re-run offline any time. Produces one row per
# award notice tracing award -> competition -> planning:
#
#   award_notice_id, award_url,
#   competition_notice_id, competition_url, competition_xml_url, competition_xml_status,
#   procedure_type      (raw TED PT_* code; NA for older/utilities forms),
#   procedure_group     (open / restricted / negotiated / competitive_dialogue /
#                        innovation_partnership / without_call; NA if no PT_*),
#   is_dps              (TRUE = dynamic purchasing system - stays open for years),
#   is_framework        (TRUE = establishes a framework agreement),
#   planning_notice_id,    planning_url,    planning_xml_url,    planning_xml_status,
#   earliest_notice_id  (deepest ancestor we hold an id for),
#   direct_award        (TRUE = awarded without a call for competition; the prior
#                        publication is a VEAT pre-announcement, so competition = NA)
#
# *_xml_status is "ok" if the notice's XML is on disk, else "missing" (re-run
# 1_2a_fetch_notices.R to pick up anything missing; its log has the fetch outcomes).
#
# Optional env var: NOTICE_LINEAGE_SAMPLE_SIZE  (must match the fetch run to align)

source("code/scraping/notice_lineage_utils.R")

xml_status <- function(ids, cache) {
  fifelse(is.na(ids), NA_character_, fifelse(is_cached(ids, cache), "ok", "missing"))
}

links <- award_universe()

# ── award -> competition (+ direct_award) ─────────────────────────────────────
message("Parsing award XMLs (award -> competition)...")
aw <- parse_award_level(links$award_notice_id, award_cache_dir)
links[aw, on = "award_notice_id",
      `:=`(competition_notice_id = i.competition_notice_id, direct_award = i.direct_award)]
links[, competition_url        := detail_url(competition_notice_id)]
links[, competition_xml_url    := xml_url(competition_notice_id)]
links[, competition_xml_status := xml_status(competition_notice_id, comp_cache_dir)]

# ── competition -> planning, + procedure type / DPS / framework flags ─────────
comp_ids <- unique(links[!is.na(competition_notice_id), competition_notice_id])
message(sprintf("Parsing %d competition XMLs (planning ref + procedure type + DPS/framework)...", length(comp_ids)))
cmeta <- map_competition_meta(comp_ids, comp_cache_dir)
links[cmeta, on = "competition_notice_id",
      `:=`(planning_notice_id = i.planning_notice_id,
           procedure_type     = i.procedure_type,
           procedure_group    = i.procedure_group,
           is_dps             = i.is_dps,
           is_framework       = i.is_framework)]
links[, planning_url        := detail_url(planning_notice_id)]
links[, planning_xml_url    := xml_url(planning_notice_id)]
links[, planning_xml_status := xml_status(planning_notice_id, planning_cache_dir)]

# ── walk back one level + deepest ancestor ────────────────────────────────────
cached_plan <- unique(links[planning_xml_status == "ok", planning_notice_id])
has_earlier <- if (length(cached_plan)) !is.na(map_priors(cached_plan, planning_cache_dir)) else logical(0)
message(sprintf("Planning notices that themselves cite an earlier notice: %d of %d (%.1f%%)",
                sum(has_earlier), length(has_earlier),
                if (length(has_earlier)) 100 * sum(has_earlier) / length(has_earlier) else 0))
message(if (sum(has_earlier) == 0)
          "  -> planning notices are the earliest document in their chain."
        else "  -> some chains go back further; a further hop could be added if needed.")

links[, earliest_notice_id := fcoalesce(planning_notice_id, competition_notice_id, award_notice_id)]

# ── save + summary ────────────────────────────────────────────────────────────
setcolorder(links, c(
  "award_notice_id", "award_url",
  "competition_notice_id", "competition_url", "competition_xml_url", "competition_xml_status",
  "procedure_type", "procedure_group", "is_dps", "is_framework",
  "planning_notice_id", "planning_url", "planning_xml_url", "planning_xml_status",
  "earliest_notice_id", "direct_award"))
setorder(links, award_notice_id)
saveRDS(links, links_rds)
fwrite(links, links_csv)

message("\nNotice lineage built.")
message(sprintf("  award notices:              %d", nrow(links)))
message(sprintf("  -> competition link:        %d (%.1f%%)  [xml ok: %d, missing: %d]",
                links[!is.na(competition_notice_id), .N], 100 * mean(!is.na(links$competition_notice_id)),
                links[competition_xml_status == "ok", .N], links[competition_xml_status == "missing", .N]))
message(sprintf("  -> direct awards:           %d", links[direct_award %in% TRUE, .N]))
message(sprintf("  -> planning link:           %d (%.1f%%)  [xml ok: %d, missing: %d]",
                links[!is.na(planning_notice_id), .N], 100 * mean(!is.na(links$planning_notice_id)),
                links[planning_xml_status == "ok", .N], links[planning_xml_status == "missing", .N]))
message(sprintf("  -> DPS notices: %d   framework agreements: %d   (exclude these for bid-window analysis)",
                links[is_dps %in% TRUE, .N], links[is_framework %in% TRUE, .N]))
message("  competition procedure_group:")
print(links[!is.na(competition_notice_id), .N, keyby = procedure_group])
message("  earliest document reached, per award row:")
print(links[, .N, keyby = .(level = fifelse(!is.na(planning_notice_id), "planning",
                                     fifelse(!is.na(competition_notice_id), "competition", "award")))])
message(sprintf("Written: %s\n         %s", links_rds, links_csv))
