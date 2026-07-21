# =============================================================================
# Project Configuration — cvr-cleaning
# =============================================================================
# Edit this file once. All scripts source it automatically.
# Open cvr-cleaning.Rproj or run scripts from the project root.
# run_replication.sh does this automatically.

# 1. Project root
#    Located by searching upward from the working directory for the RStudio
#    project marker, so paths are correct no matter which sub-directory a script
#    or R Markdown report is run from. Falls back to getwd() with a warning.
find_project_root <- function(start = getwd(), marker = "cvr-cleaning.Rproj") {
  d <- normalizePath(start, mustWork = TRUE)
  while (!file.exists(file.path(d, marker))) {
    parent <- dirname(d)
    if (parent == d) {
      warning("Could not locate project root (", marker, "); using getwd().")
      return(normalizePath(getwd(), mustWork = TRUE))
    }
    d <- parent
  }
  d
}
PROJECT_DIR <- find_project_root()

# 2. Derived paths (do not edit)
dirs <- list(
  data = file.path(PROJECT_DIR, "data"),
  cvr_key = file.path(PROJECT_DIR, "data", "cvr_matching_data"),
  raw_data   = file.path(PROJECT_DIR, "data", "raw"),
  clean_data = file.path(PROJECT_DIR, "data", "clean"),
  code = file.path(PROJECT_DIR, "code")
)

# Create any missing data output directories (never the code directory).
invisible(lapply(dirs[c("raw_data", "clean_data")], dir.create, recursive = TRUE, showWarnings = FALSE))
