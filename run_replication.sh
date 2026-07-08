#!/usr/bin/env bash

set -euo pipefail

# Run from the repository root, even when the script is called from elsewhere.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR"
RSCRIPT="${RSCRIPT:-Rscript}"
RUN_MATCHING="${RUN_MATCHING:-true}"

export PROJECT_DIR

cd "$PROJECT_DIR"

echo "Project directory: $PROJECT_DIR"
echo "Rscript: $RSCRIPT"

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

if [[ "$RUN_MATCHING" == "true" ]]; then
  require_file "data/cvr_matching_data/cvr_names_full.csv"
  require_file "data/cvr_matching_data/cvr_binavne_full.csv"
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

run_r_script "code/1_1_process_kfst.R"
run_r_script "code/1_2_process_open_tender.R"

if [[ "$RUN_MATCHING" != "true" ]]; then
  echo
  echo "Cleaning-only replication complete. Outputs are in data/clean."
  exit 0
fi

run_r_script "code/1_3_process_keys.R"
run_r_script "code/2_1_match_kfst.R"
run_r_script "code/2_2_match_kfst_buyers.R"
run_r_script "code/2_2_match_opentender.R"
run_r_script "code/2_3_match_opentender_buyers.R"

echo
echo "Replication complete. Outputs are in data/clean."
