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

echo "Project directory: $PROJECT_DIR"
echo "Rscript: $RSCRIPT"
echo "Run matching: $RUN_MATCHING"
echo "Build CVR lookup from Virk API: $BUILD_CVR_LOOKUP"
echo "Build employment history from Virk API: $BUILD_EMPLOYMENT_HISTORY"
echo "Extract TED notices: $EXTRACT_TED_NOTICES"

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

run_r_script() {
  local script_path="$1"

  echo
  echo "Running $script_path"
  "$RSCRIPT" --vanilla "$script_path"
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
