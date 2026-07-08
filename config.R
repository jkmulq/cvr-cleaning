# =============================================================================
# Project Configuration — cvr-cleaning
# =============================================================================
# Edit this file once. All scripts source it automatically.
# Open cvr-cleaning.Rproj or run scripts from the project root.
# run_replication.sh does this automatically.

# 1. Project root
#    If running from another location or machine, replace this line with your
#    local project path.
PROJECT_DIR <- normalizePath(getwd(), mustWork = TRUE)

# 2. Derived paths (do not edit)
dirs <- list(
  data = file.path(PROJECT_DIR, "data"),
  cvr_key = file.path(PROJECT_DIR, "data", "cvr_matching_data"),
  raw_data   = file.path(PROJECT_DIR, "data", "raw"),
  clean_data = file.path(PROJECT_DIR, "data", "clean"),
  code = file.path(PROJECT_DIR, "code")
)

# Create any missing output directories
invisible(lapply(dirs[c("raw_data", "clean_data", "code")], dir.create, recursive = TRUE, showWarnings = FALSE))
