# =============================================================================
# Project Configuration — cvr-cleaning
# =============================================================================
# Edit this file once. All scripts source it automatically.
# Open the project via cvr-cleaning.Rproj so here::here() resolves
# the root correctly. If you run scripts outside RStudio, set PROJECT_DIR
# explicitly below instead.

library(here)

# 1. Project root
#    When the .Rproj file is open in RStudio, here::here() resolves to the
#    project root automatically — leave PROJECT_DIR as-is.
#    If running outside RStudio, either:
#      - set the PROJECT_DIR environment variable before running the scripts; or
#      - replace the default below with your local project path.
config_value <- function(name, default) {
  value <- Sys.getenv(name, unset = "")

  if (value == "") {
    return(default)
  }

  value
}

PROJECT_DIR <- normalizePath(
  config_value("PROJECT_DIR", here::here()),
  mustWork = FALSE
)

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
