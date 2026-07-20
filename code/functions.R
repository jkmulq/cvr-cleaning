## Helper functions for the CVR data cleaning process.

extract_multiple_cvr <- function(
    data,
    row_id,
    entity_cols,
    cvr_column
) {

  # Extract row
  data_row <- data[row_id, ]

  # Keep identifiers + originals only once
  out <- data_row %>%
    select(tender_id, lot_id, dplyr::all_of(entity_cols))
  
  # Container for expanded values (DO NOT include IDs here)
  out_list <- list()
  max_detected <- 0
  
  # Loop over each column separately
  for (col in entity_cols) {
    
    x <- data_row[[col]]
    
    if (is.na(x)) next
    
    # Standardise separators
    x <- gsub("\\s*,\\s*", ";", x)
    x <- gsub("\\s*;\\s*", ";", x)
    
    # CVR-specific dot handling
    if (col == cvr_column) {
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
  
  # Find the number of entities detected during extraction
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

# CVR-like placeholders that should not be treated as real Danish CVRs.
known_invalid_cvr_numbers <- function() {
  c(
    "00000000",
    "11111111",
    "12345678",
    "99999999"
  )
}

# Recover one clearly formatted Danish CVR only after the conservative extractor
# has found no valid CVR. This protects rows such as "12345678-87654321": the
# normal extractor sees two CVRs there, so this helper leaves the row alone.
recover_formatted_danish_cvr <- function(cvr_candidate,
                                         country,
                                         n_valid_cvr_raw,
                                         known_invalid_cvrs = known_invalid_cvr_numbers()) {
  cvr_candidate <- as.character(cvr_candidate)
  country <- as.character(country)
  n_valid_cvr_raw <- as.integer(n_valid_cvr_raw)
  
  digits_only <- stringr::str_remove_all(cvr_candidate, "[^0-9]")
  
  blank_candidate <- is.na(cvr_candidate) |
    stringr::str_trim(cvr_candidate) == ""
  danish_candidate <- !is.na(country) & country == "DK"
  no_raw_cvr_found <- !is.na(n_valid_cvr_raw) & n_valid_cvr_raw == 0
  one_eight_digit_number <- !is.na(digits_only) &
    stringr::str_detect(digits_only, "^[0-9]{8}$")
  invalid_placeholder <- !is.na(digits_only) &
    digits_only %in% known_invalid_cvrs
  
  recovered_cvr <- ifelse(
    !blank_candidate &
      danish_candidate &
      no_raw_cvr_found &
      one_eight_digit_number &
      !invalid_placeholder,
    digits_only,
    NA_character_
  )
  
  return(recovered_cvr)
}

# Add source context to a name-match table. match_row_id is temporary and
# depends on the row order in a single script run. These context columns make
# exact and fuzzy match tables easy to inspect without rejoining by hand.
add_entity_context_to_matches <- function(matches,
                                          source_rows,
                                          entity) {
  if (nrow(matches) == 0 || !"match_row_id" %in% names(matches)) {
    return(matches)
  }
  
  entity_number_column <- paste0(entity, "_number")
  entity_name_column <- paste0(entity, "_name_in_data")
  entity_basic_column <- paste0(entity, "_name_basic")
  entity_firm_type_column <- paste0(entity, "_firm_type")
  
  required_context_columns <- c(
    "match_row_id",
    "tender_id",
    "lot_id",
    entity_number_column,
    entity_name_column,
    entity_basic_column,
    entity_firm_type_column
  )
  
  missing_context_columns <- setdiff(
    required_context_columns,
    names(source_rows)
  )
  
  if (length(missing_context_columns) > 0) {
    stop(
      "Missing context columns for ", entity, " matches: ",
      paste(missing_context_columns, collapse = ", ")
    )
  }
  
  context_columns <- required_context_columns
  if ("row_id" %in% names(source_rows)) {
    context_columns <- c("row_id", context_columns)
  }
  
  entity_context <- data.table::copy(source_rows[, ..context_columns])
  data.table::setnames(
    entity_context,
    old = c(entity_basic_column, entity_firm_type_column),
    new = c(
      paste0(entity, "_name_basic_in_data"),
      paste0(entity, "_firm_type_in_data")
    )
  )
  
  entity_context[matches, on = "match_row_id"]
}

add_winner_context_to_matches <- function(
    matches,
    source_rows = get("remaining_original", envir = parent.frame())
) {
  add_entity_context_to_matches(
    matches = matches,
    source_rows = source_rows,
    entity = "winner"
  )
}

add_buyer_context_to_matches <- function(
    matches,
    source_rows = get("remaining_original", envir = parent.frame())
) {
  add_entity_context_to_matches(
    matches = matches,
    source_rows = source_rows,
    entity = "buyer"
  )
}

# Legal-form spellings used when preparing CVR names. Keeping this dictionary
# in one place means name preparation and multiple-firm detection use the same
# definition of terms such as A/S, ApS, and I/S.
cvr_firm_type_patterns <- function() {
  c(
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
  firm_type_patterns <- cvr_firm_type_patterns()

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

  # Keep the lightly prepared name for the first exact matching step.
  name_basic <- gsub("\\s+", " ", trimws(name_clean))

  # Apply the generalizations used before the main fuzzy match.
  name_clean <- gsub("oe", "o", name_basic, fixed = TRUE)
  name_clean <- gsub("aa", "a", name_clean, fixed = TRUE)
  name_clean <- gsub("&", " og ", name_clean, fixed = TRUE)
  name_clean <- gsub("v/", " ", name_clean, fixed = TRUE)
  name_clean <- gsub("/", "", name_clean, fixed = TRUE)
  name_clean <- gsub("[-()]", " ", name_clean)
  name_clean <- gsub("[,.:\"'´`«»]", "", name_clean)
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
    name_basic = name_basic,
    name_clean = name_clean,
    name_no_spaces = name_no_spaces,
    name_broad = name_broad,
    firm_type = firm_type,
    first_letter = substr(name_clean, 1, 1)
  )
}


# ========================================
# Multiple-firm name detection
# ========================================

# Create every possible partition of an entity name at plausible firm-name
# delimiters. Legal forms are masked only while finding delimiters, so the slash
# in A/S is ignored while a slash between two firms is retained.
make_name_partitions <- function(value, max_boundaries = 5L) {
  value <- as.character(value)[1]

  # Empty data.table to store results
  empty_partitions <- data.table::data.table(
    partition_id = integer(),
    partition_code = integer(),
    partition_text = character(),
    segment_number = integer(),
    segment_text = character()
  )

  # Return an empty result if the input is missing or blank
  if (is.na(value) || trimws(value) == "") {
    return(list(
      original_name = value,
      working_name = value,
      n_boundaries = 0L,
      n_legal_forms = 0L,
      too_many_delimiters = FALSE,
      flag_joint_venture_text = FALSE,
      flag_consortium_text = FALSE,
      flag_collaboration_text = FALSE,
      partitions = empty_partitions
    ))
  }

  # These words flag possible collaborative bids. 
  # They are not delimiters by themselves: "Konsortiet 1508 A/S" may be a registered firm name.
  flag_joint_venture_text <- grepl(
    "joint[ -]?venture|jointventure|(?<![[:alnum:]])j[.]?\\s*v[.]?(?![[:alnum:]])",
    value,
    ignore.case = TRUE,
    perl = TRUE
  )
  flag_consortium_text <- grepl(
    "konsorti|consorti",
    value,
    ignore.case = TRUE,
    perl = TRUE
  )
  flag_collaboration_text <- (
    flag_joint_venture_text |
      flag_consortium_text |
      grepl(
        "sammenslutningen|i\\s+samarbejde\\s+med|sammen\\s+med",
        value,
        ignore.case = TRUE,
        perl = TRUE
      )
  )

  # Collaboration vocabulary is used only for flagging. First replace phrases
  # that connect two firms with an explicit boundary. Then remove consortium,
  # joint-venture, and association labels from the name that will be tested.
  # The original entity name is retained separately for auditing.
  working_name <- trimws(value)
  working_name <- gsub(
    paste0(
      "(?:",
      "i\\s+konsorti(?:um|e)\\s+med",
      "|i\\s+joint[ -]?venture\\s+med",
      "|joint[ -]?venture\\s+med",
      "|in\\s+joint[ -]?venture\\s+with",
      "|in\\s+j[.]?\\s*v[.]?\\s+with",
      "|i\\s+samarbejde\\s+med",
      "|sammen\\s+med",
      ")"
    ),
    ";",
    working_name,
    ignore.case = TRUE,
    perl = TRUE
  )

  # Remove the consortium term itself. In a compound such as ARC-Konsortiet,
  # this keeps the informative "ARC" portion available for matching.
  consortium_word_pattern <- paste0(
    "-?(?:konsortiet|konsortium|konsortie|consortium)"
  )
  working_name <- gsub(
    paste0(
      consortium_word_pattern,
      "(?:\\s+(?:(?:bestående|bestaaende)\\s+af|mellem|af))?"
    ),
    " ",
    working_name,
    ignore.case = TRUE,
    perl = TRUE
  )
  working_name <- gsub(
    "joint[ -]?venture|jointventure|(?<![[:alnum:]])j[.]?\\s*v[.]?(?![[:alnum:]])",
    " ",
    working_name,
    ignore.case = TRUE,
    perl = TRUE
  )
  working_name <- gsub(
    "(?<![[:alnum:]])sammenslutningen(?:\\s+af)?(?![[:alnum:]])",
    " ",
    working_name,
    ignore.case = TRUE,
    perl = TRUE
  )

  # Remove punctuation left behind by labels such as "(som konsortium)" or
  # "Joint Venture:". Delimiters between actual names remain in place.
  working_name <- gsub(
    "\\(\\s*(?:som|as)?\\s*\\)",
    " ",
    working_name,
    ignore.case = TRUE,
    perl = TRUE
  )
  working_name <- gsub("^[[:space:],;: -]+", "", working_name)
  working_name <- gsub("[[:space:],;: -]+$", "", working_name)
  working_name <- gsub("\\s+", " ", working_name)
  working_name <- trimws(working_name)

  # Replace legal forms with an equal number of spaces. Equal-length
  # replacements preserve character positions in the original working name.
  delimiter_search_name <- working_name
  n_legal_forms <- 0L
  for (pattern in names(cvr_firm_type_patterns())) {
    standalone_pattern <- paste0(
      "(?<![[:alnum:]])",
      pattern,
      "(?![[:alnum:]])"
    )
    locations <- gregexpr(
      standalone_pattern,
      delimiter_search_name,
      ignore.case = TRUE,
      perl = TRUE
    )
    found <- regmatches(delimiter_search_name, locations)[[1]]

    if (length(found) > 0) {
      n_legal_forms <- n_legal_forms + length(found)
      replacement <- strrep(" ", nchar(found))
      regmatches(delimiter_search_name, locations) <- list(replacement)
    }
  }

  # "v/" means "represented by" in these names, not a boundary between two
  # winning firms. Mask it after legal forms and before looking for slashes.
  locations <- gregexpr(
    "(?<![[:alnum:]])v/",
    delimiter_search_name,
    ignore.case = TRUE,
    perl = TRUE
  )
  found <- regmatches(delimiter_search_name, locations)[[1]]
  if (length(found) > 0) {
    replacement <- strrep(" ", nchar(found))
    regmatches(delimiter_search_name, locations) <- list(replacement)
  }

  # A comma followed by and/og/samt is deliberately treated as one boundary.
  # A literal plus sign can also separate two firm names.
  delimiter_pattern <- paste0(
    ",\\s*(?:(?:and|og|samt)\\s+)?",
    "|;",
    "|/",
    "|(?<![[:alnum:]])(?:og|and|samt)(?![[:alnum:]])",
    "|&",
    "|\\+"
  )

  locations <- gregexpr(
    delimiter_pattern,
    delimiter_search_name,
    ignore.case = TRUE,
    perl = TRUE
  )[[1]]

  if (locations[1] == -1L) {
    return(list(
      original_name = value,
      working_name = working_name,
      n_boundaries = 0L,
      n_legal_forms = n_legal_forms,
      too_many_delimiters = FALSE,
      flag_joint_venture_text = flag_joint_venture_text,
      flag_consortium_text = flag_consortium_text,
      flag_collaboration_text = flag_collaboration_text,
      partitions = empty_partitions
    ))
  }

  boundary_lengths <- attr(locations, "match.length")
  n_boundaries <- length(locations)

  if (n_boundaries > max_boundaries) {
    return(list(
      original_name = value,
      working_name = working_name,
      n_boundaries = as.integer(n_boundaries),
      n_legal_forms = n_legal_forms,
      too_many_delimiters = TRUE,
      flag_joint_venture_text = flag_joint_venture_text,
      flag_consortium_text = flag_consortium_text,
      flag_collaboration_text = flag_collaboration_text,
      partitions = empty_partitions
    ))
  }

  # Separate the name into its smallest pieces while retaining the exact text
  # of every delimiter. A later split/keep choice rebuilds larger segments.
  piece_starts <- c(1L, locations + boundary_lengths)
  piece_ends <- c(locations - 1L, nchar(working_name))
  pieces <- substring(working_name, piece_starts, piece_ends)
  delimiters <- substring(
    working_name,
    locations,
    locations + boundary_lengths - 1L
  )

  # Create empty vector of 2^n_boundaries - 1 partitions. 
  # Each partition is a data.table with one row per segment.
  # (At each boundary choose whether to split or not, but remove no split case)
  n_partitions <- 2 ^ n_boundaries - 1L
  partition_rows <- vector("list", n_partitions)
  partition_number <- 0L

  # Descending codes put the version that splits at every boundary first.
  for (partition_code in rev(seq_len(n_partitions))) {
    
    # Code split types as a binary number. Each bit corresponds to a boundary.
    split_here <- as.logical(
      intToBits(partition_code)[seq_len(n_boundaries)]
    )
    
    # Create segments
    segments <- character()
    current_segment <- pieces[1] # Initialise segment

    for (boundary_number in seq_len(n_boundaries)) {
      # If partition requires split at boundary, store current segment and start a new one. 
      if (split_here[boundary_number]) {
        segments <- c(segments, current_segment)
        current_segment <- pieces[boundary_number + 1L]
      } else { # Else, paste current and next segment together with original delimiter.
        current_segment <- paste0(
          current_segment,
          delimiters[boundary_number],
          pieces[boundary_number + 1L]
        )
      }
    }
    segments <- trimws(c(segments, current_segment))

    # Do not test partitions that would create an empty firm name.
    if (any(segments == "")) next

    partition_number <- partition_number + 1L
    partition_text <- paste(segments, collapse = "; ")
    partition_rows[[partition_number]] <- data.table::data.table(
      partition_id = partition_number,
      partition_code = partition_code,
      partition_text = partition_text,
      segment_number = seq_along(segments),
      segment_text = segments
    )
  }

  # Bind
  partitions <- data.table::rbindlist(
    partition_rows[seq_len(partition_number)],
    use.names = TRUE,
    fill = TRUE
  )

  # Return
  list(
    original_name = value,
    working_name = working_name,
    n_boundaries = as.integer(n_boundaries),
    n_legal_forms = n_legal_forms,
    too_many_delimiters = FALSE,
    flag_joint_venture_text = flag_joint_venture_text,
    flag_consortium_text = flag_consortium_text,
    flag_collaboration_text = flag_collaboration_text,
    partitions = partitions
  )
}



# ===============================
# Fuzzy matching helper functions
# ===============================

# The notebook allows the KFST and CVR dates to differ by up to two years.
# Missing CVR start/end dates are treated as open-ended.
keep_valid_dates <- function(candidates) {
  candidates[
    is.na(match_date) |
      (
        (is.na(gyldigfra) | match_date >= gyldigfra - 730L) &
          (is.na(gyldigtil) | match_date <= gyldigtil + 730L)
      )
  ]
}

# Exact joins can return several CVRs for one entity name. Prefer:
#   1. a main CVR name rather than a biname;
#   2. a name active on the exact publication date;
#   3. the oldest registered name.
# The number of possible CVRs is retained for manual review.
select_preferred_exact_match <- function(candidates, step) {
  
  # Remove candidates whose registered-name dates are incompatible
  candidates <- keep_valid_dates(candidates)
  
  # If empty return an empty data.table
  if (nrow(candidates) == 0) {
    return(data.table())
  }
  
  # Record whether the CVR name was active on the tender date
  candidates[, active_on_tender_date := (
    (is.na(gyldigfra) | is.na(match_date) | match_date >= gyldigfra) &
      (is.na(gyldigtil) | is.na(match_date) | match_date <= gyldigtil)
  )]
  
  # Count the distinct CVRs available for each source entity
  candidates[
    ,
    name_match_n_candidates := uniqueN(cvr),
    by = match_row_id
  ]
  
  # Put the preferred candidate first:
  # 1. main name before biname
  # 2. active on the tender date
  # 3. oldest registration
  candidates <- candidates[
    order(
      match_row_id,
      source_order,
      -active_on_tender_date,
      gyldigfra,
      cvr,
      na.last = TRUE
    )
  ]
  
  # Keep the first candidate for each source entity
  selected <- candidates[, .SD[1], by = match_row_id]
  
  # Return only the fields needed later
  selected[, .(
    match_row_id,
    cvr_name_match = cvr,
    registered_name_match = registered_name,
    name_match_source = name_source,
    name_match_step = step,
    name_match_method = "exact",
    name_match_score = 100,
    name_match_n_candidates
  )]
}

# The documentation describes a Levenshtein similarity score from 0 to 100.
levenshtein_ratio <- function(value, candidates) {
  distance <- as.numeric(adist(value, candidates))
  total_length <- nchar(value) + nchar(candidates)
  100 * (total_length - distance) / total_length
}

# Fuzzy matching happens only after the exact steps. For each remaining entity:
#   1. keep CVR names with the same firm type and first letter;
#   2. remove names outside the two-year date allowance;
#   3. calculate similarity scores;
#   4. return the five highest-scoring CVRs.
find_fuzzy_matches <- function(
    rows,
    key,
    entity_name_column,
    key_name_column,
    first_letter_column,
    firm_type_column,
    step
) {
  if (nrow(rows) == 0) return(data.table())
  
  required_row_columns <- c(
    "match_row_id",
    "match_date",
    entity_name_column,
    firm_type_column
  )
  missing_row_columns <- setdiff(required_row_columns, names(rows))

  if (length(missing_row_columns) > 0) {
    stop(
      "find_fuzzy_matches(): rows is missing required columns: ",
      paste(missing_row_columns, collapse = ", ")
    )
  }

  found <- vector("list", nrow(rows))
  
  for (row_number in seq_len(nrow(rows))) {
    row <- rows[row_number]
    row_name <- row[[entity_name_column]]
    
    if (is.na(row_name) || row_name == "") next
    
    row_firm_type_value <- row[[firm_type_column]]
    row_first_letter_value <- substr(row_name, 1, 1)
    
    # Create candidate matches
    # - same firm type
    # - either first letter or first broad letter
    if (first_letter_column == "first_letter") {
      candidates <- key[
        list(row_firm_type_value, row_first_letter_value),
        on = .(firm_type, first_letter),
        nomatch = 0
      ]
    } else {
      candidates <- key[
        list(row_firm_type_value, row_first_letter_value),
        on = .(firm_type, broad_first_letter),
        nomatch = 0
      ]
    }
    
    if (nrow(candidates) == 0) next
    
    # Keep candidate matches with valid registration dates
    candidates[, match_date := row$match_date]
    candidates <- keep_valid_dates(candidates)
    
    # Extract candidate names and keep only non-missings
    candidate_names <- candidates[[key_name_column]]
    keep <- !is.na(candidate_names) & candidate_names != ""
    candidates <- candidates[keep]
    candidate_names <- candidate_names[keep]
    
    if (nrow(candidates) == 0) next
    
    candidates[, score := levenshtein_ratio(
      row_name,
      candidate_names
    )]
    
    # Order candidates by score, then use the date/oldest-firm rule for ties.
    candidates[, active_on_tender_date := (
      (is.na(gyldigfra) | is.na(match_date) | match_date >= gyldigfra) &
        (is.na(gyldigtil) | is.na(match_date) | match_date <= gyldigtil)
    )]
    setorder(
      candidates,
      -score,
      -active_on_tender_date,
      gyldigfra,
      cvr,
      na.last = TRUE
    )
    
    # A CVR can appear several times because it has several registered names or
    # active periods. Keep only its highest-ranked appearance.
    candidates <- unique(candidates, by = "cvr")
    n_top_score_candidates <- uniqueN(
      candidates[score == max(score), cvr]
    )
    candidates <- head(candidates, 5)
    candidates[, fuzzy_candidate_rank := seq_len(.N)]
    
    found[[row_number]] <- candidates[, .(
      match_row_id = row$match_row_id,
      fuzzy_candidate_cvr = cvr,
      fuzzy_candidate_name = registered_name,
      fuzzy_candidate_source = name_source,
      fuzzy_candidate_step = step,
      fuzzy_candidate_score = score,
      fuzzy_candidate_rank,
      n_top_score_candidates
    )]
  }
  
  rbindlist(found, use.names = TRUE, fill = TRUE)
}

# Accept candidate 1 only when it exceeds the threshold and no other CVR has
# the same top score. Tied top candidates cannot be distinguished reliably.
accept_fuzzy_match <- function(candidates, threshold) {
  if (nrow(candidates) == 0) return(data.table())
  
  candidates[
    fuzzy_candidate_rank == 1 &
      fuzzy_candidate_score > threshold &
      n_top_score_candidates == 1L,
    .(
      match_row_id,
      cvr_name_match = fuzzy_candidate_cvr,
      registered_name_match = fuzzy_candidate_name,
      name_match_source = fuzzy_candidate_source,
      name_match_step = fuzzy_candidate_step,
      name_match_method = "fuzzy",
      name_match_score = fuzzy_candidate_score,
      name_match_n_candidates = n_top_score_candidates
    )
  ]
}

# Add a step's matches to matched and remove those rows from remaining.
# This small repeated block mirrors the notebook's matched/remaining workflow.
keep_step_matches <- function(new_matches) {
  if (nrow(new_matches) == 0) return(invisible(NULL))
  
  matched <<- rbindlist(
    list(matched, new_matches),
    use.names = TRUE,
    fill = TRUE
  )
  remaining <<- remaining[
    !new_matches,
    on = "match_row_id"
  ]
  
  invisible(NULL)
}

# ===============================
# Virk CVR API lookup helpers
# ===============================

load_virk_renviron_files <- function() {
  project_dir <- Sys.getenv("PROJECT_DIR")
  if (!nzchar(project_dir)) {
    project_dir <- getwd()
  }

  renviron_paths <- unique(c(
    path.expand("~/.Renviron"),
    file.path(project_dir, ".Renviron"),
    file.path(getwd(), ".Renviron")
  ))

  for (renviron_path in renviron_paths[file.exists(renviron_paths)]) {
    readRenviron(renviron_path)
  }

  invisible(NULL)
}

get_virk_credentials <- function(
    user_var = "VIRK_CVR_USER",
    password_var = "VIRK_CVR_PASSWORD"
) {
  user <- Sys.getenv(user_var)
  password <- Sys.getenv(password_var)

  if (!nzchar(user) || !nzchar(password)) {
    load_virk_renviron_files()
    user <- Sys.getenv(user_var)
    password <- Sys.getenv(password_var)
  }

  if (!nzchar(user) || !nzchar(password)) {
    stop(
      paste(
        "Missing Virk credentials.",
        paste0("Set ", user_var, " and ", password_var, " before querying the Virk API."),
        "For example, add them to .Renviron in your home folder or this project folder.",
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  list(user = user, password = password)
}

virk_post_json <- function(url,
                           body,
                           query = list(),
                           credentials = get_virk_credentials()) {
  response <- httr::POST(
    url,
    query = query,
    httr::authenticate(credentials$user, credentials$password),
    httr::content_type_json(),
    httr::accept_json(),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  )

  if (httr::status_code(response) >= 300) {
    message(httr::content(response, as = "text", encoding = "UTF-8"))
    httr::stop_for_status(response)
  }

  jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

virk_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

format_virk_cvr <- function(x) {
  value <- virk_scalar(x)

  if (is.na(value)) {
    return(NA_character_)
  }

  digits <- gsub("\\D", "", value)
  if (grepl("^[0-9]{1,8}$", digits)) {
    return(sprintf("%08d", as.integer(digits)))
  }

  value
}

extract_virk_period <- function(x, field) {
  if (is.null(x$periode)) {
    return(NA_character_)
  }

  virk_scalar(x$periode[[field]])
}

empty_virk_name_table <- function(name_column = c("name", "binavn")) {
  name_column <- match.arg(name_column)

  out <- data.table::data.table(
    cvr = character(),
    registered_name = character(),
    gyldigfra = character(),
    gyldigtil = character(),
    registration_date = character(),
    lifecycle_start = character(),
    lifecycle_end = character()
  )

  data.table::setnames(out, "registered_name", name_column)
  out
}

bind_virk_name_tables <- function(tables, name_column = c("name", "binavn")) {
  name_column <- match.arg(name_column)

  if (length(tables) == 0) {
    return(empty_virk_name_table(name_column))
  }

  out <- data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)

  if (ncol(out) == 0) {
    return(empty_virk_name_table(name_column))
  }

  data.table::setcolorder(
    out,
    c(
      "cvr", name_column, "gyldigfra", "gyldigtil",
      "registration_date", "lifecycle_start", "lifecycle_end"
    )
  )
  out
}

extract_virk_lifecycle_summary <- function(firm) {
  registration_date <- virk_scalar(firm$stiftelsesDato)
  lifecycle_starts <- character()
  lifecycle_ends <- character()

  if (!is.null(firm$livsforloeb) && length(firm$livsforloeb) > 0) {
    lifecycle_starts <- vapply(
      firm$livsforloeb,
      function(record) extract_virk_period(record, "gyldigFra"),
      character(1)
    )
    lifecycle_ends <- vapply(
      firm$livsforloeb,
      function(record) extract_virk_period(record, "gyldigTil"),
      character(1)
    )
  }

  lifecycle_starts <- lifecycle_starts[!is.na(lifecycle_starts) & lifecycle_starts != ""]
  lifecycle_ends <- lifecycle_ends[!is.na(lifecycle_ends) & lifecycle_ends != ""]

  lifecycle_start <- if (length(lifecycle_starts) == 0) {
    registration_date
  } else {
    min(lifecycle_starts)
  }

  if (is.na(registration_date) || registration_date == "") {
    registration_date <- lifecycle_start
  }

  list(
    registration_date = registration_date,
    lifecycle_start = lifecycle_start,
    lifecycle_end = if (length(lifecycle_ends) == 0) {
      NA_character_
    } else {
      max(lifecycle_ends)
    }
  )
}

extract_virk_main_names <- function(firm) {
  if (is.null(firm$navne) || length(firm$navne) == 0) {
    return(empty_virk_name_table("name"))
  }

  lifecycle <- extract_virk_lifecycle_summary(firm)

  bind_virk_name_tables(
    lapply(firm$navne, function(name_record) {
      data.table::data.table(
        cvr = format_virk_cvr(firm$cvrNummer),
        name = virk_scalar(name_record$navn),
        gyldigfra = extract_virk_period(name_record, "gyldigFra"),
        gyldigtil = extract_virk_period(name_record, "gyldigTil"),
        registration_date = lifecycle$registration_date,
        lifecycle_start = lifecycle$lifecycle_start,
        lifecycle_end = lifecycle$lifecycle_end
      )
    }),
    "name"
  )
}

extract_virk_binavne <- function(firm) {
  if (is.null(firm$binavne) || length(firm$binavne) == 0) {
    return(empty_virk_name_table("binavn"))
  }

  lifecycle <- extract_virk_lifecycle_summary(firm)

  bind_virk_name_tables(
    lapply(firm$binavne, function(name_record) {
      data.table::data.table(
        cvr = format_virk_cvr(firm$cvrNummer),
        binavn = virk_scalar(name_record$navn),
        gyldigfra = extract_virk_period(name_record, "gyldigFra"),
        gyldigtil = extract_virk_period(name_record, "gyldigTil"),
        registration_date = lifecycle$registration_date,
        lifecycle_start = lifecycle$lifecycle_start,
        lifecycle_end = lifecycle$lifecycle_end
      )
    }),
    "binavn"
  )
}

append_virk_lookup_chunk <- function(data, path) {
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  data.table::fwrite(
    unique(data),
    path,
    append = file.exists(path),
    col.names = !file.exists(path),
    na = ""
  )

  invisible(NULL)
}

virk_lookup_source_fields <- function() {
  c(
    "Vrvirksomhed.cvrNummer",
    "Vrvirksomhed.navne",
    "Vrvirksomhed.binavne",
    "Vrvirksomhed.stiftelsesDato",
    "Vrvirksomhed.livsforloeb"
  )
}

virk_lookup_query_body <- function(size) {
  body <- list(
    size = size,
    query = list(
      exists = list(field = "Vrvirksomhed.cvrNummer")
    )
  )

  body[["_source"]] <- virk_lookup_source_fields()
  body
}

test_cvr_lookup_sample <- function(n = 100,
                                   out_dir = "data/cvr_matching_data",
                                   credentials = get_virk_credentials()) {
  search_url <- "http://distribution.virk.dk/cvr-permanent/virksomhed/_search"

  timed <- system.time({
    result <- virk_post_json(
      search_url,
      virk_lookup_query_body(n),
      credentials = credentials
    )

    firms <- lapply(result$hits$hits, function(hit) {
      hit$`_source`$Vrvirksomhed
    })

    names_data <- bind_virk_name_tables(
      lapply(firms, extract_virk_main_names),
      "name"
    )
    binavne_data <- bind_virk_name_tables(
      lapply(firms, extract_virk_binavne),
      "binavn"
    )

    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    names_path <- file.path(out_dir, paste0("cvr_names_sample_", n, ".csv"))
    binavne_path <- file.path(out_dir, paste0("cvr_binavne_sample_", n, ".csv"))

    data.table::fwrite(unique(names_data), names_path, na = "")
    data.table::fwrite(unique(binavne_data), binavne_path, na = "")
  })

  list(
    firms_requested = n,
    firms_returned = length(firms),
    official_name_rows = nrow(names_data),
    binavn_rows = nrow(binavne_data),
    elapsed_seconds = unname(timed[["elapsed"]]),
    seconds_per_100_firms = unname(timed[["elapsed"]]) / length(firms) * 100,
    names_file = names_path,
    binavne_file = binavne_path
  )
}

generate_cvr_lookup_from_virk <- function(
    out_dir = "data/cvr_matching_data",
    batch_size = 1000,
    scroll = "5m",
    names_file = "cvr_names_full.csv",
    binavne_file = "cvr_binavne_full.csv",
    overwrite = FALSE,
    credentials = get_virk_credentials()
) {
  search_url <- "http://distribution.virk.dk/cvr-permanent/virksomhed/_search"
  scroll_url <- "http://distribution.virk.dk/_search/scroll"

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  names_path <- file.path(out_dir, names_file)
  binavne_path <- file.path(out_dir, binavne_file)

  existing_outputs <- c(names_path, binavne_path)[file.exists(c(names_path, binavne_path))]
  if (length(existing_outputs) > 0 && !overwrite) {
    stop(
      paste(
        "Refusing to overwrite existing CVR lookup files:",
        paste(existing_outputs, collapse = "\n"),
        "Set overwrite = TRUE to rebuild them.",
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  if (file.exists(names_path)) file.remove(names_path)
  if (file.exists(binavne_path)) file.remove(binavne_path)

  body <- virk_lookup_query_body(batch_size)
  body$sort <- list("_doc")

  result <- virk_post_json(
    search_url,
    body,
    query = list(scroll = scroll),
    credentials = credentials
  )

  scroll_id <- result$`_scroll_id`
  firms_processed <- 0L
  official_name_rows <- 0L
  binavn_rows <- 0L

  repeat {
    hits <- result$hits$hits
    if (length(hits) == 0) break

    firms <- lapply(hits, function(hit) {
      hit$`_source`$Vrvirksomhed
    })

    names_data <- bind_virk_name_tables(
      lapply(firms, extract_virk_main_names),
      "name"
    )
    binavne_data <- bind_virk_name_tables(
      lapply(firms, extract_virk_binavne),
      "binavn"
    )

    append_virk_lookup_chunk(names_data, names_path)
    append_virk_lookup_chunk(binavne_data, binavne_path)

    firms_processed <- firms_processed + length(firms)
    official_name_rows <- official_name_rows + nrow(names_data)
    binavn_rows <- binavn_rows + nrow(binavne_data)

    message("Processed ", firms_processed, " CVR records")

    result <- virk_post_json(
      scroll_url,
      list(scroll = scroll, scroll_id = scroll_id),
      credentials = credentials
    )
    scroll_id <- result$`_scroll_id`
  }

  list(
    firms_processed = firms_processed,
    official_name_rows = official_name_rows,
    binavn_rows = binavn_rows,
    names_file = names_path,
    binavne_file = binavne_path
  )
}

## Common Procurement Vocabulary (CPV) division lookup.
## The CPV is the EU's standard classification for public procurement subject
## matter (https://ted.europa.eu/). A CPV code is an 8-digit code where the
## first two digits identify the division (the broadest, most interpretable
## grouping). Below is the full list of CPV 2008 divisions.
cpv_division_names <- c(
  "03" = "Agricultural, farming, fishing, forestry and related products",
  "09" = "Petroleum products, fuel, electricity and other sources of energy",
  "14" = "Mining, basic metals and related products",
  "15" = "Food, beverages, tobacco and related products",
  "16" = "Agricultural machinery",
  "18" = "Clothing, footwear, luggage articles and accessories",
  "19" = "Leather and textile fabrics, plastic and rubber materials",
  "22" = "Printed matter and related products",
  "24" = "Chemical products",
  "30" = "Office and computing machinery, equipment and supplies except furniture and software packages",
  "31" = "Electrical machinery, apparatus, equipment and consumables; lighting",
  "32" = "Radio, television, communication, telecommunication and related equipment",
  "33" = "Medical equipments, pharmaceuticals and personal care products",
  "34" = "Transport equipment and auxiliary products to transportation",
  "35" = "Security, fire-fighting, police and defence equipment",
  "37" = "Musical instruments, sport goods, games, toys, handicraft, art materials and accessories",
  "38" = "Laboratory, optical and precision equipments (excl. glasses)",
  "39" = "Furniture (incl. office furniture), furnishings, domestic appliances (excl. lighting) and cleaning products",
  "41" = "Collected and purified water",
  "42" = "Industrial machinery",
  "43" = "Machinery for mining, quarrying, construction equipment",
  "44" = "Construction structures and materials; auxiliary products to construction (except electric apparatus)",
  "45" = "Construction work",
  "48" = "Software package and information systems",
  "50" = "Repair and maintenance services",
  "51" = "Installation services (except software)",
  "55" = "Hotel, restaurant and retail trade services",
  "60" = "Transport services (excl. Waste transport)",
  "63" = "Supporting and auxiliary transport services; travel agencies services",
  "64" = "Postal and telecommunications services",
  "65" = "Public utilities",
  "66" = "Financial and insurance services",
  "70" = "Real estate services",
  "71" = "Architectural, construction, engineering and inspection services",
  "72" = "IT services: consulting, software development, Internet and support",
  "73" = "Research and development services and related consultancy services",
  "75" = "Administration, defence and social security services",
  "76" = "Services related to the oil and gas industry",
  "77" = "Agricultural, forestry, horticultural, aquacultural and apicultural services",
  "79" = "Business services: law, marketing, consulting, recruitment, printing and security",
  "80" = "Education and training services",
  "85" = "Health and social work services",
  "90" = "Sewage, refuse, cleaning and environmental services",
  "92" = "Recreational, cultural and sporting services",
  "98" = "Other community, social and personal services"
)

## CPV 2003 (Regulation 2195/2002) division names for the divisions that were
## dropped/renumbered in the CPV 2008 revision. Older TED tenders (roughly
## pre-2009) use CPV 2003 codes, so datasets that span many years — e.g. the
## OpenTender extracts — mix both vocabularies. This lookup only covers division
## numbers that DO NOT exist in CPV 2008; numbers reused across versions are
## intentionally handled by the CPV 2008 lookup above (see clean_cpv_code()).
## Source: TED CPV correspondence table (2008 <-> 2003).
cpv_division_names_2003 <- c(
  "01" = "Agricultural, horticultural, hunting and related products",
  "02" = "Forestry and logging products",
  "05" = "Fish, fishing products and other by-products of the fishing industry",
  "10" = "Coal, lignite, peat and other coal-related products",
  "11" = "Petroleum, natural gas, oil and associated products",
  "12" = "Uranium and thorium ores",
  "13" = "Metal ores",
  "17" = "Textiles and textile articles",
  "20" = "Wood, wood products, cork products, basketware and wickerwork",
  "21" = "Various types of pulp, paper and paper products",
  "23" = "Petroleum products and fuels",
  "25" = "Rubber, plastic and film products",
  "26" = "Non-metallic mineral products",
  "27" = "Basic metals and associated products",
  "28" = "Fabricated products and materials",
  "29" = "Machinery, equipment, appliances, apparatus and associated products",
  "36" = "Manufactured goods, furniture, handicrafts, special-purpose products and associated consumables",
  "40" = "Electricity, gas, nuclear energy and fuels, steam, hot water and other sources of energy",
  "52" = "Retail trade services",
  "61" = "Water transport services",
  "62" = "Air transport services",
  "67" = "Services auxiliary to financial intermediation",
  "74" = "Architectural, engineering, construction, legal, accounting and other professional services",
  "78" = "Printing, publishing, advertising and marketing services",
  "91" = "Membership organisation services",
  "93" = "Miscellaneous services",
  "95" = "Private households with employed persons",
  "99" = "Services provided by extra-territorial organisations and bodies"
)

## Map a CPV division (first two digits) to the EU procurement contract type:
## Works, Supplies, or Services. This is the standard EU trichotomy used across
## the public procurement directives. The boundaries are stable across the CPV
## 2003 and CPV 2008 vocabularies: division 45 is works, divisions 50-99 are
## services, and everything else is supplies (goods). Returns NA for a missing
## division. Kept coarse on purpose so each group is large enough for regressions.
cpv_division_to_category <- function(division) {
  division_num <- suppressWarnings(as.integer(division))
  dplyr::case_when(
    is.na(division) ~ NA_character_,
    division == "45" ~ "Works",
    division_num >= 50 ~ "Services",
    TRUE ~ "Supplies"
  )
}

## Map a CPV division to one of eight economically-coherent sectors. This is a
## middle ground between the 45 CPV divisions (too sparse for heterogeneity in
## the treatment-effect event studies) and the three-way EU trichotomy (services
## dominates). Each sector holds hundreds of distinct winner firms across the
## KFST and OpenTender data, so the grouping is intended for heterogeneity
## analysis where per-cell sample size matters. Divisions unique to CPV 2003
## (e.g. 74 professional services, 67 auxiliary financial, 52 retail) are placed
## in the sector matching their CPV 2008 successor. Any division not listed falls
## through to manufactured goods/supplies.
cpv_division_to_sector <- function(division) {
  dplyr::case_when(
    is.na(division) ~ NA_character_,
    division %in% c("45", "44", "43", "71") ~ "Construction & engineering",
    division %in% c("60", "61", "62", "63", "34", "64") ~ "Transport & logistics",
    division %in% c("33", "85", "24") ~ "Health, medical & pharma",
    division %in% c("48", "72", "73", "74", "79", "66", "67", "70", "75", "78") ~ "ICT & professional services",
    division %in% c("90", "50", "51", "65", "41") ~ "Environmental & facilities services",
    division %in% c("15", "55", "52", "03", "77", "16", "01", "02", "05") ~ "Food, hospitality & agriculture",
    division %in% c("80", "92", "98", "91", "93", "95", "99") ~ "Education, culture & other public services",
    TRUE ~ "Other manufacturing & goods"
  )
}

## Clean a raw CPV code field into an interpretable division label.
## Tenders can list several CPV codes; as a first pass we keep only the first
## listed code (the primary subject matter). Codes are separated by a semicolon
## in the KFST data and by a comma in the OpenTender data, so we split on both.
## The raw codes need light cleaning: Excel drops leading zeros from some
## numeric-looking codes (e.g. "03000000" -> "3000000"), and some carry a
## supplementary suffix (e.g. "71000000 - IA01"). We extract the leading digit
## sequence and zero-pad it to the standard 8 digits, then map the first two
## digits (the division) to its name.
##
## Datasets spanning many years mix CPV 2008 and CPV 2003 codes. We label a
## division with its CPV 2008 name whenever that number exists in CPV 2008 (the
## current standard and the bulk of the data), and fall back to the CPV 2003
## name only for division numbers that CPV 2008 dropped. A handful of numbers
## were reused with a different meaning across the two versions (e.g. 35, 37);
## for those, pre-2009 rows carry the CPV 2008 label, which may be imprecise.
##
## Returns a list of parallel vectors (mirrors prepare_cvr_name()).
clean_cpv_code <- function(cpv_code) {

  # Keep only the first listed code (comma- or semicolon-separated).
  first_code <- trimws(sub("[,;].*", "", cpv_code))

  # Extract the leading digit sequence, dropping any supplementary suffix,
  # then zero-pad to the standard 8-digit CPV code length.
  code_digits <- stringr::str_extract(first_code, "[0-9]+")
  code_first <- stringr::str_pad(code_digits, width = 8, side = "left", pad = "0")

  # Division = first two digits. Prefer the CPV 2008 name, falling back to the
  # CPV 2003 name for division numbers that only exist in CPV 2003.
  division <- substr(code_first, 1, 2)
  division_name <- unname(cpv_division_names[division])
  division_name <- ifelse(
    is.na(division_name),
    unname(cpv_division_names_2003[division]),
    division_name
  )

  # Coarser groupings for treatment-effect heterogeneity, where each cell needs
  # enough firms to be tractable: the eight-sector scheme and the EU trichotomy.
  sector <- cpv_division_to_sector(division)
  category <- cpv_division_to_category(division)

  list(
    code_first = code_first,
    division = division,
    division_name = division_name,
    sector = sector,
    category = category
  )
}
