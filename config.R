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
#    If running outside RStudio, comment out the here() line and set the
#    path manually, e.g.:
#      PROJECT_DIR <- "/Users/yourname/path/to/cvr-cleaning"
PROJECT_DIR <- here::here()

# 2. Stata
#    Set STATA_PATH to the full path of your Stata executable.
#    Common locations:
#      StataMP (Mac):  /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp
#      Stata 18 (Mac): /Applications/Stata/StataMP.app/Contents/MacOS/stata-mp
#      Windows:        "C:/Program Files/Stata19/StataMP-64.exe"
STATA_PATH    <- "/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp"
STATA_VERSION <- 19

options("RStata.StataPath"    = STATA_PATH)
options("RStata.StataVersion" = STATA_VERSION)

# 3. Derived paths (do not edit)
dirs <- list(
  data = file.path(PROJECT_DIR, "data"),
  cvr_key = file.path(PROJECT_DIR, "data", "cvr_matching_data"),
  raw_data   = file.path(PROJECT_DIR, "data", "raw"),
  clean_data = file.path(PROJECT_DIR, "data", "clean"),
  code = file.path(PROJECT_DIR, "code")
)

# Create any missing output directories
invisible(lapply(dirs[c("raw_data", "clean_data", "code")], dir.create, recursive = TRUE, showWarnings = FALSE))
