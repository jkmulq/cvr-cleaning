#!/usr/bin/env bash

set -euo pipefail

# Run from the repository root by default, even when the script is called from
# another directory. A different location can be supplied with PROJECT_DIR.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${PROJECT_DIR:-$SCRIPT_DIR}"
RSCRIPT="${RSCRIPT:-Rscript}"
RUN_MATCHING="${RUN_MATCHING:-true}"

export PROJECT_DIR

cd "$PROJECT_DIR"

echo "Project directory: $PROJECT_DIR"
echo "Rscript: $RSCRIPT"

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
