# Stage 1 of 2 for the TED notice lineage: FETCH all XML, in the required order.
#
# Fetching is inherently STAGED because each level's targets are discovered by
# parsing the level above it:
#   1. award notices     - ids come from the OpenTender award URLs
#   2. competition notices - ids are the prior-publication ref inside each award XML
#   3. planning notices  - ids are the prior-publication ref inside each competition XML
# So this script alternates fetch -> parse-to-discover -> fetch across the three
# levels. It writes NO table; it only populates the XML caches. Build the
# notice_links table afterwards with 1_2b_build_notice_lineage.R (offline, no fetch).
#
# Cache-first + resumable: only ids not already on disk are fetched, so re-running
# picks up notices that previously came back throttled/pending. raw_xml/ is shared
# with 2_extract_ted_notices.R, so award XMLs it already pulled are not re-fetched.
#
# Optional env var: NOTICE_LINEAGE_SAMPLE_SIZE  (limit to first N award notices)

source("code/scraping/notice_lineage_utils.R")

# ── Stage 1/3: award notices (ids from OpenTender URLs) ───────────────────────
message("== Stage 1/3: award notices ==")
aw <- award_universe()
fetch_notices(aw$award_notice_id, award_cache_dir, "Award notices")

# ── Stage 2/3: competition notices (discovered by parsing the award XMLs) ─────
message("\n== Stage 2/3: competition notices ==")
message("Parsing award XMLs to discover competition ids...")
comp_ids <- parse_award_level(aw$award_notice_id, award_cache_dir)[
  !is.na(competition_notice_id), unique(competition_notice_id)]
fetch_notices(comp_ids, comp_cache_dir, "Competition notices")

# ── Stage 3/3: planning notices (discovered by parsing the competition XMLs) ──
message("\n== Stage 3/3: planning notices ==")
message("Parsing competition XMLs to discover planning ids...")
plan_ids <- unique(na.omit(map_priors(comp_ids, comp_cache_dir)))
fetch_notices(plan_ids, planning_cache_dir, "Planning notices")

message("\nFetching complete. Caches populated:")
message(sprintf("  award:       %s", award_cache_dir))
message(sprintf("  competition: %s", comp_cache_dir))
message(sprintf("  planning:    %s", planning_cache_dir))
message("Now run: Rscript code/scraping/1_2b_build_notice_lineage.R")
