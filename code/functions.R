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
