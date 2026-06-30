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
# not counted as CVRs. Whitespace is removed before matching so a spaced CVR can
# be read as one number; consequently, whitespace alone cannot separate two CVRs.
extract_valid_cvr_candidates <- function(x) {
  x <- clean_cvr_candidate(x)
  out <- unlist(stringr::str_extract_all(
    x,
    "(?<!\\d)\\d{8}(?!\\d)"
  ))
  
  if (length(out) == 0) {
    return(NA_character_)
  }
  
  # Return
  return(out)
}

# Find number of unique candidates
compute_distinct_valid_cvr <- function(x) {
  vapply(
    x,
    function(value) {
      cands <- extract_valid_cvr_candidates(value)
      cands <- cands[!is.na(cands)]
      cands <- unique(cands)
      length(cands)
    },
    integer(1),
    USE.NAMES = FALSE
  )
}

# This is an R version of the main fuzzy-matching preparation used in
# Bisnode matching documentation_V7.docx and the referenced Python notebooks.
# It keeps spaces between words because the notebooks use this version for
# their main fuzzy match (step 5).
prepare_cvr_name <- function(x) {
  name_original <- as.character(x)
  name_clean <- tolower(trimws(name_original))

  # Replace accented letters with the spellings used in the
  # notebooks. This happens before "oe" and "aa" are simplified below.
  letter_replacements <- c(
    "ø" = "oe",
    "æ" = "ae",
    "å" = "aa",
    "ö" = "oe",
    "ä" = "ae",
    "á" = "a",
    "é" = "e",
    "è" = "e",
    "à" = "a"
  )

  for (old in names(letter_replacements)) {
    name_clean <- gsub(
      old,
      letter_replacements[[old]],
      name_clean,
      fixed = TRUE
    )
  }

  # Detect legal form only when it is a standalone term, then remove it from
  # the name. Longer expressions come first to avoid partial matches.
  firm_type_patterns <- c(
    "aktieselskabet" = "a/s",
    "aktieselskab" = "a/s",
    "anpartsselskabet" = "aps",
    "anpartsselskab" = "aps",
    "a[.]m[.]b[.]a[.]?" = "amba",
    "a\\s+m\\s+b\\s+a" = "amba",
    "s[.]m[.]b[.]a[.]?" = "smba",
    "f[.]m[.]b[.]a[.]?" = "fmba",
    "a/s[.]?" = "a/s",
    "gmbh" = "a/s",
    "aps" = "aps",
    "i/s" = "i/s",
    "k/s" = "k/s",
    "ks" = "k/s",
    "ivs" = "ivs",
    "p/s" = "p/s",
    "amba[.]?" = "amba",
    "smba" = "smba",
    "fmba" = "fmba",
    "as" = "a/s",
    "ab" = "aps",
    "a/" = "a/s"
  )

  firm_type <- rep("Undetermined", length(name_clean))

  for (pattern in names(firm_type_patterns)) {
    standalone_pattern <- paste0(
      "(?<![[:alnum:]])",
      pattern,
      "(?![[:alnum:]])"
    )
    found <- grepl(standalone_pattern, name_clean, perl = TRUE)
    use_type <- found & firm_type == "Undetermined" & !is.na(found)
    firm_type[use_type] <- firm_type_patterns[[pattern]]
    name_clean <- gsub(standalone_pattern, " ", name_clean, perl = TRUE)
  }

  # Apply the generalizations used before the main fuzzy match.
  name_clean <- gsub("oe", "o", name_clean, fixed = TRUE)
  name_clean <- gsub("aa", "a", name_clean, fixed = TRUE)
  name_clean <- gsub("&", " og ", name_clean, fixed = TRUE)
  name_clean <- gsub("v/", " ", name_clean, fixed = TRUE)
  name_clean <- gsub("/", "", name_clean, fixed = TRUE)
  name_clean <- gsub("[-()]", " ", name_clean)
  name_clean <- gsub("[,.:\"'´`]", "", name_clean)
  name_clean <- gsub("\\s+", " ", trimws(name_clean))

  # Standardize selected whole words. Splitting the name first makes it clear
  # that, for example, "company" is changed but "accompany" is not.
  word_replacements <- c(
    "international" = "int",
    "and" = "og",
    "av" = "af",
    "of" = "af",
    "limited" = "ltd",
    "denmark" = "dk",
    "danmark" = "dk",
    "holdings" = "holding",
    "sweden" = "se",
    "sverige" = "se",
    "corporation" = "corp",
    "company" = "co",
    "comp" = "co",
    "copenhagen" = "kbh",
    "kobenhavn" = "kbh",
    "cph" = "kbh",
    "kopenhamn" = "kbh",
    "i" = "1",
    "ii" = "2",
    "iii" = "3"
  )

  replace_words <- function(value) {
    if (is.na(value) || value == "") return(NA_character_)

    words <- strsplit(value, " ", fixed = TRUE)[[1]]
    replace <- words %in% names(word_replacements)
    words[replace] <- unname(word_replacements[words[replace]])
    paste(words, collapse = " ")
  }

  name_clean <- vapply(
    name_clean,
    replace_words,
    character(1),
    USE.NAMES = FALSE
  )

  # Steps 2 and 3 use the prepared name without spaces.
  name_no_spaces <- gsub(" ", "", name_clean, fixed = TRUE)

  # Steps 4 and 6 remove common, low-information words and ignore word order.
  common_words <- c(
    "af", "arhus", "asset", "assets", "broderna", "brdr", "brodrerne",
    "co", "dansk", "data", "development", "dk", "ejendomsselskabet",
    "finans", "forsikring", "group", "holding", "hotels", "hotel", "int",
    "invest", "kbh", "komplementaerssaelskabet", "livforsikringsselskab",
    "management", "media", "nordic", "nordisk", "og", "scandinavia",
    "scandinavian", "se", "service", "services", "skandia", "software",
    "system", "systems", "ltd", "filial", "for"
  )

  make_broad_name <- function(value) {
    if (is.na(value) || value == "") return(NA_character_)

    words <- strsplit(value, " ", fixed = TRUE)[[1]]
    words <- words[!words %in% common_words]

    if (length(words) == 0) return(NA_character_)

    paste(sort(words), collapse = "")
  }

  name_broad <- vapply(
    name_clean,
    make_broad_name,
    character(1),
    USE.NAMES = FALSE
  )

  tibble::tibble(
    name_original = name_original,
    name_clean = name_clean,
    firm_type = firm_type,
    first_letter = substr(name_clean, 1, 1)
  )
}
