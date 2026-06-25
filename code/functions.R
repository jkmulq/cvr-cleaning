## Helper functions for the CVR data cleaning process.

extract_multiple_cvr <- function(
    data,
    row_id,
    cvr_cols = c("winner_cvr", "winner_name", "winner_country")
) {
  
  # Extract row
  data_row <- data[row_id, ]
  
  # Keep identifiers + originals only once
  out <- data_row %>%
    select(tender_id, lot_id, dplyr::all_of(cvr_cols))
  
  # Container for expanded values (DO NOT include IDs here)
  out_list <- list()
  max_detected <- 0
  
  # Loop over each column separately
  for (col in cvr_cols) {
    
    x <- data_row[[col]]
    
    if (is.na(x)) next
    
    # Standardise separators
    x <- gsub("\\s*,\\s*", ";", x)
    x <- gsub("\\s*;\\s*", ";", x)
    
    # CVR-specific dot handling
    if (col == "winner_cvr") {
      x <- gsub("\\.\\s*(?=[A-Z0-9])", ";", x, perl = TRUE)
    }
    
    # Split
    # Note, I treat an empty space as a firm, not a mistake. 
    # Usually these are firms that don't have a CVR number. 
    parts <- strsplit(x, ";", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts[parts == ""] <- NA_character_ # Treat empty strings as NA (better missing label for firms without CVR)
    
    # Store
    for (i in seq_along(parts)) {
      out_list[[paste0(col, "_", i)]] <- parts[i]
    }
  }
  
  # Find the number of winning firms detected during extraction
  if (length(out_list) == 0) {
    n_detected <- 0
  } else {
    n_detected <- max(readr::parse_number(names(out_list)), na.rm = TRUE)
  }
  max_detected <- max(0, n_detected)
  
  # Return base row if nothing expanded
  if (length(out_list) == 0) {
    return(out %>% mutate(max_detected = max_detected))
  }

  bind_cols(out, tibble(max_detected = max_detected), tibble::as_tibble(out_list))
}

# Prepare CVR text for pattern matching. This keeps labels such as "CVR5EByg:"
# in place but removes whitespace, so spaced CVRs like "DK55 77 52 14" can be
# read as one eight-digit number.
clean_cvr_candidate <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  x[x == ""] <- NA_character_
  x
}

# Extract valid-looking Danish CVRs from each candidate string. The lookarounds
# require exactly eight digits, so longer identifiers such as "111151609" are
# not counted as CVRs.
extract_valid_cvr_candidates <- function(x) {
  x <- clean_cvr_candidate(x)
  unlist(stringr::str_extract_all(x, "(?<!\\d)\\d{8}(?!\\d)"))
}

# Return TRUE only when a source field contains more than one distinct valid CVR.
# Repeated forms of the same CVR, such as "55775214" and "DK55775214", count
# once. Invalid identifiers are ignored.
has_multiple_distinct_valid_cvrs <- function(x, delim = ";") {
  vapply(
    x,
    FUN.VALUE = logical(1),
    FUN = function(value) {
      if (is.na(value) || value == "") {
        return(FALSE)
      }

      # OpenTender winner IDs have already had accepted delimiters converted to
      # semicolons before this helper is called.
      cvr_candidates <- strsplit(value, delim, fixed = TRUE)[[1]]
      valid_cvrs <- extract_valid_cvr_candidates(cvr_candidates)

      length(unique(valid_cvrs)) > 1
    }
  )
}
