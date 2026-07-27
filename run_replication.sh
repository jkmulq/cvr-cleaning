#!/usr/bin/env bash

set -euo pipefail

# Run from the repository root, even when the script is called from elsewhere.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR"
RSCRIPT="${RSCRIPT:-Rscript}"
RUN_MATCHING="${RUN_MATCHING:-true}"
BUILD_CVR_LOOKUP="${BUILD_CVR_LOOKUP:-false}"
# Optional post-matching web/API pulls. They consume the matched datasets and
# need network access, so they run after matching and are off by default.
BUILD_EMPLOYMENT_HISTORY="${BUILD_EMPLOYMENT_HISTORY:-false}"
EXTRACT_TED_NOTICES="${EXTRACT_TED_NOTICES:-false}"

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
echo "Extract TED notices: $EXTRACT_TED_NOTICES"
echo "Log file: $LOG_FILE"

if ! command -v "$RSCRIPT" > /dev/null 2>&1; then
  echo "Could not find Rscript command: $RSCRIPT" >&2
  echo "Unset RSCRIPT or run with RSCRIPT=Rscript ./run_replication.sh" >&2
  exit 1
fi

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
  if [[ "$BUILD_EMPLOYMENT_HISTORY" == "true" || "$EXTRACT_TED_NOTICES" == "true" ]]; then
    echo
    echo "Note: BUILD_EMPLOYMENT_HISTORY / EXTRACT_TED_NOTICES need the matched" >&2
    echo "datasets, so they are skipped when RUN_MATCHING=false." >&2
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

# Optional post-matching pulls (consume the *_name_matched.rds outputs above).
# BUILD_EMPLOYMENT_HISTORY needs Virk credentials; EXTRACT_TED_NOTICES needs
# internet access. Both are resumable.
if [[ "$BUILD_EMPLOYMENT_HISTORY" == "true" ]]; then
  run_r_script "code/scraping/1_build_cvr_employment_history.R"
fi

if [[ "$EXTRACT_TED_NOTICES" == "true" ]]; then
  run_r_script "code/scraping/2_extract_ted_notices.R"
fi

echo
echo "Replication complete. Outputs are in data/clean."
