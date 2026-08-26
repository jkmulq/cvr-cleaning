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

# 1b. Data root. Defaults to <project>/data, but can be redirected to a shared folder (e.g. a Box/Dropbox
#     folder synced across machines) by setting CVR_DATA_DIR in ~/.Renviron or the environment. Only the
#     data moves; code/ always stays in the repo. See .Renviron.example.
DATA_ROOT <- Sys.getenv("CVR_DATA_DIR", unset = file.path(PROJECT_DIR, "data"))
DATA_ROOT <- normalizePath(DATA_ROOT, mustWork = FALSE)

# 2. Derived paths (do not edit)
dirs <- list(
  data = DATA_ROOT,
  cvr_key = file.path(DATA_ROOT, "cvr_matching_data"),
  raw_data   = file.path(DATA_ROOT, "raw"),
  clean_data = file.path(DATA_ROOT, "clean"),
  intermediates = file.path(DATA_ROOT, "intermediates"),
  code = file.path(PROJECT_DIR, "code")
)

# Create any missing data directories (never the code directory) so a fresh clone
# or a new data root works out of the box, including the TED XML cache subdir the
# scraping scripts write into. Input dirs are created empty; run_replication.sh
# still verifies the required input files are actually present.
invisible(lapply(c(dirs[setdiff(names(dirs), "code")], file.path(dirs$intermediates, "ted", "raw_xml")),
                 dir.create, recursive = TRUE, showWarnings = FALSE))
