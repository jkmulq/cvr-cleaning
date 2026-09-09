#!/usr/bin/env bash

set -euo pipefail

# Run from the repository root, even when the script is called from elsewhere.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR"
RSCRIPT="${RSCRIPT:-Rscript}"
RUN_MATCHING="${RUN_MATCHING:-true}"
BUILD_CVR_LOOKUP="${BUILD_CVR_LOOKUP:-false}"
# Optional post-matching web/API pull. Consumes the matched datasets and needs Virk
# credentials, so it runs after matching and is off by default. (The TED/XML dataset
# chain is now a standard step -- see below -- not an optional flag.)
BUILD_EMPLOYMENT_HISTORY="${BUILD_EMPLOYMENT_HISTORY:-false}"

export PROJECT_DIR

cd "$PROJECT_DIR"

# ── Run log + timing ─────────────────────────────────────────────────────────
# Mirror all stdout/stderr to a timestamped log under logs/ (override with
# LOG_DIR=...), and time the whole pipeline plus each step. A summary of per-step
# timings and the total runtime is printed on exit (success, early exit, or
# failure) via an EXIT trap.
LOG_DIR="${LOG_DIR:-$PROJECT_DIR/logs}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/replication_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG_FILE") 2>&1
PIPELINE_START_EPOCH=$(date +%s)

# Parallel arrays recording each pipeline step and its wall-clock seconds.
STEP_NAMES=()
STEP_SECONDS=()

# Seconds -> "1h 05m 03s" / "5m 03s" / "42s".
format_duration() {
  local total="$1" h m s
  h=$(( total / 3600 ))
  m=$(( (total % 3600) / 60 ))
  s=$(( total % 60 ))
  if (( h > 0 )); then
    printf '%dh %02dm %02ds' "$h" "$m" "$s"
  elif (( m > 0 )); then
    printf '%dm %02ds' "$m" "$s"
  else
    printf '%ds' "$s"
  fi
}

print_step_summary() {
  (( ${#STEP_NAMES[@]} == 0 )) && return 0
  echo
  echo "----------------------------------------------------------------"
  echo "Step timings"
  echo "----------------------------------------------------------------"
  local i
  for i in "${!STEP_NAMES[@]}"; do
    printf '  %-46s %s\n' "${STEP_NAMES[$i]}" "$(format_duration "${STEP_SECONDS[$i]}")"
  done
}

# Runs on any exit: prints the per-step summary and the total pipeline runtime.
on_exit() {
  local status=$?
  print_step_summary
  local total=$(( $(date +%s) - PIPELINE_START_EPOCH ))
  echo "----------------------------------------------------------------"
  if (( status == 0 )); then
    echo "Total pipeline time: $(format_duration "$total")"
  else
    echo "Pipeline FAILED (exit $status) after $(format_duration "$total")"
  fi
  echo "Log file: $LOG_FILE"
  echo "----------------------------------------------------------------"
}
trap on_exit EXIT

echo "Run started:      $(date '+%Y-%m-%d %H:%M:%S')"
echo "Project directory: $PROJECT_DIR"
echo "Rscript: $RSCRIPT"
echo "Run matching: $RUN_MATCHING"
echo "Build CVR lookup from Virk API: $BUILD_CVR_LOOKUP"
echo "Build employment history from Virk API: $BUILD_EMPLOYMENT_HISTORY"
echo "Log file: $LOG_FILE"

if ! command -v "$RSCRIPT" > /dev/null 2>&1; then
  echo "Could not find Rscript command: $RSCRIPT" >&2
  echo "Unset RSCRIPT or run with RSCRIPT=Rscript ./run_replication.sh" >&2
  exit 1
fi

# The pipeline runs each step with `Rscript --vanilla` (below), which skips
# ~/.Renviron. CVR_DATA_DIR (the required data root, honored by config.R) is often
# set in ~/.Renviron rather than exported, so resolve it once via a plain Rscript
# and export it here so every --vanilla step inherits it.
if [ -z "${CVR_DATA_DIR:-}" ]; then
  CVR_DATA_DIR="$("$RSCRIPT" -e 'cat(Sys.getenv("CVR_DATA_DIR"))' 2>/dev/null || true)"
  [ -n "$CVR_DATA_DIR" ] && export CVR_DATA_DIR
fi
# CVR_DATA_DIR is required -- there is no <project>/data fallback. Fail fast with a
# clear message rather than letting the first R step error out.
if [ -z "${CVR_DATA_DIR:-}" ]; then
  echo "CVR_DATA_DIR is not set. Set it to the shared data folder (e.g. your Box path) in" >&2
  echo "~/.Renviron or the environment before running. See .Renviron.example." >&2
  exit 1
fi
echo "Data root (CVR_DATA_DIR): $CVR_DATA_DIR"

require_file() {
  local file_path="$1"

  if [[ ! -f "$file_path" ]]; then
    echo "Missing required input: $file_path" >&2
    return 1
  fi
}

require_any_file() {
  local file_pattern="$1"
  local description="$2"

  if ! compgen -G "$file_pattern" > /dev/null; then
    echo "Missing required input: $description" >&2
    echo "Expected at least one file matching: $file_pattern" >&2
    return 1
  fi
}

echo
echo "Checking local input data"
require_file "data/raw/kfst/udbudsdata_kfst.xlsx"
require_any_file "data/raw/OpenTender/*.csv" "OpenTender CSV files in data/raw/OpenTender/"

if [[ "$RUN_MATCHING" == "true" && "$BUILD_CVR_LOOKUP" != "true" ]]; then
  require_any_file "data/cvr_matching_data/cvr_names_virk_*.csv" "Virk CVR official-name key files in data/cvr_matching_data/"
  require_any_file "data/cvr_matching_data/cvr_binavne_virk_*.csv" "Virk CVR alternative-name key files in data/cvr_matching_data/"
fi

if [[ "${RESTORE_RENV:-false}" == "true" ]]; then
  echo
  echo "Restoring renv package environment"
  "$RSCRIPT" --vanilla -e 'renv::restore(prompt = FALSE)'
fi

# Runs one pipeline R script, timing it and recording the duration for the
# end-of-run summary. Timing is captured even if the script fails.
run_r_script() {
  local script_path="$1"
  local start end elapsed rc

  echo
  echo "==> Running $script_path"
  start=$(date +%s)
  set +e
  "$RSCRIPT" --vanilla "$script_path"
  rc=$?
  set -e
  end=$(date +%s)
  elapsed=$(( end - start ))

  STEP_NAMES+=("$script_path")
  STEP_SECONDS+=("$elapsed")

  if (( rc == 0 )); then
    echo "    done in $(format_duration "$elapsed")"
  else
    echo "    FAILED after $(format_duration "$elapsed") (exit $rc)" >&2
    return "$rc"
  fi
}

run_r_script "code/processing/1_1_process_kfst.R"
run_r_script "code/processing/1_2_process_open_tender.R"

if [[ "$RUN_MATCHING" != "true" ]]; then
  if [[ "$BUILD_EMPLOYMENT_HISTORY" == "true" ]]; then
    echo
    echo "Note: BUILD_EMPLOYMENT_HISTORY needs the matched datasets, so it is" >&2
    echo "skipped when RUN_MATCHING=false. The TED chain and dataset combine are" >&2
    echo "also skipped (they need the matched datasets too)." >&2
  fi
  echo
  echo "Cleaning-only replication complete. Outputs are in data/clean."
  exit 0
fi

if [[ "$BUILD_CVR_LOOKUP" == "true" ]]; then
  run_r_script "code/processing/0_build_cvr_lookup.R"
  require_any_file "data/cvr_matching_data/cvr_names_virk_*.csv" "Virk CVR official-name key files in data/cvr_matching_data/"
  require_any_file "data/cvr_matching_data/cvr_binavne_virk_*.csv" "Virk CVR alternative-name key files in data/cvr_matching_data/"
fi

run_r_script "code/processing/1_3_process_keys.R"
run_r_script "code/processing/2_1_match_kfst.R"
run_r_script "code/processing/2_2_match_kfst_buyers.R"
run_r_script "code/processing/2_3_match_opentender.R"
run_r_script "code/processing/2_4_match_opentender_buyers.R"
run_r_script "code/processing/3_1_build_kfst_winner_datasets.R"
run_r_script "code/processing/3_2_build_ot_winner_datasets.R"
run_r_script "code/processing/3_3_build_ot_buyer_datasets.R"

# Optional post-matching pull (consumes the *_name_matched.rds outputs above).
# BUILD_EMPLOYMENT_HISTORY needs Virk credentials and is resumable.
if [[ "$BUILD_EMPLOYMENT_HISTORY" == "true" ]]; then
  run_r_script "code/scraping/employment_1_winners.R"
fi

# Full TED/XML dataset chain (standard step). Fetch notice XML -> extract parties/lots/CVRs/dates ->
# build tender-lot winner/buyer tables -> name-match winners + buyers (saved to data/clean). Each step is
# cache-first/resumable; the raw notice XML is already fetched by the date chain inside 1_1/1_2, so these
# reuse the cache offline.
run_r_script "code/scraping/ted_1_extract_notices.R"
run_r_script "code/scraping/ted_2_extract_party_cvrs.R"
run_r_script "code/scraping/ted_3_build_winner_buyer_datasets.R"
run_r_script "code/scraping/ted_4_match_winners.R"
run_r_script "code/scraping/ted_5_match_buyers.R"

# Concatenate KFST + OpenTender + TED into one winner dataset and one buyer dataset (shared schema aligned,
# source-specific columns NA-filled, a `dataset` column flags the source). Requires all three sources.
run_r_script "code/processing/5_combine_datasets.R"

echo
echo "Replication complete. Outputs are in data/clean."
