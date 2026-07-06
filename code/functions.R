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

# Create every possible partition of a winner name at plausible firm-name
# delimiters. Legal forms are masked only while finding delimiters, so the slash
# in A/S is ignored while a slash between two firms is retained.
make_winner_name_partitions <- function(value, max_boundaries = 5L) {
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
  # The original winner name is retained separately for auditing.
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

# Exact joins can return several CVRs for one winner name. Prefer:
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
  
  # Count the distinct CVRs available for each KFST winner
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
  
  # Keep the first candidate for each KFST winner
  selected <- candidates[, .SD[1], by = match_row_id]
  
  # Return only the fields needed later
  selected[, .(
    match_row_id,
    winner_cvr_name_match = cvr,
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

# Fuzzy matching happens only after the exact steps. For each remaining winner:
#   1. keep CVR names with the same firm type and first letter;
#   2. remove names outside the two-year date allowance;
#   3. calculate similarity scores;
#   4. return the five highest-scoring CVRs.
find_fuzzy_matches <- function(
    rows,
    key,
    winner_name_column,
    key_name_column,
    first_letter_column,
    step,
    firm_type_column = "winner_firm_type"
) {
  if (nrow(rows) == 0) return(data.table())
  
  required_row_columns <- c(
    "match_row_id",
    "match_date",
    winner_name_column,
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
    row_name <- row[[winner_name_column]]
    
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
      winner_cvr_name_match = fuzzy_candidate_cvr,
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
