library("data.table")
library("stringdist")

abstracts_file <- "abstracts.csv"
submissions_file <- "submissions_uRos.csv"
speakers_file <- "speakers.csv"
registered_file <- "participants/list_user_registered.csv"
summary_file <- "participants/speaker_registration_summary.csv"

normalize_export_columns <- function(x) {
  # The SciencesConf exports in this project have an extra empty trailing field.
  if (names(x)[1] == "V1") {
    setnames(x, c(names(x)[-1], "extra_export_field"))
  }
  x
}

ascii_fold <- function(x) {
  is_na <- is.na(x)
  x[is_na] <- ""
  x <- enc2utf8(x)

  entity_map <- c(
    "&agrave;" = "a", "&aacute;" = "a", "&acirc;" = "a",
    "&atilde;" = "a", "&auml;" = "a", "&aring;" = "a",
    "&Agrave;" = "A", "&Aacute;" = "A", "&Acirc;" = "A",
    "&Atilde;" = "A", "&Auml;" = "A", "&Aring;" = "A",
    "&ccedil;" = "c", "&Ccedil;" = "C",
    "&egrave;" = "e", "&eacute;" = "e", "&ecirc;" = "e",
    "&euml;" = "e", "&Egrave;" = "E", "&Eacute;" = "E",
    "&Ecirc;" = "E", "&Euml;" = "E",
    "&igrave;" = "i", "&iacute;" = "i", "&icirc;" = "i",
    "&iuml;" = "i", "&Igrave;" = "I", "&Iacute;" = "I",
    "&Icirc;" = "I", "&Iuml;" = "I",
    "&ntilde;" = "n", "&Ntilde;" = "N",
    "&ograve;" = "o", "&oacute;" = "o", "&ocirc;" = "o",
    "&otilde;" = "o", "&ouml;" = "o", "&oslash;" = "o",
    "&Ograve;" = "O", "&Oacute;" = "O", "&Ocirc;" = "O",
    "&Otilde;" = "O", "&Ouml;" = "O", "&Oslash;" = "O",
    "&ugrave;" = "u", "&uacute;" = "u", "&ucirc;" = "u",
    "&uuml;" = "u", "&Ugrave;" = "U", "&Uacute;" = "U",
    "&Ucirc;" = "U", "&Uuml;" = "U",
    "&yacute;" = "y", "&yuml;" = "y", "&Yacute;" = "Y"
  )
  for (entity in names(entity_map)) {
    x <- gsub(entity, entity_map[[entity]], x, fixed = TRUE)
  }

  replacement_groups <- c(
    a = "[àáâãäåāăąǎǻ]", A = "[ÀÁÂÃÄÅĀĂĄǍǺ]",
    ae = "[æǽ]", AE = "[ÆǼ]",
    c = "[çćčĉċ]", C = "[ÇĆČĈĊ]",
    d = "[ďđð]", D = "[ĎĐÐ]",
    e = "[èéêëēĕėęě]", E = "[ÈÉÊËĒĔĖĘĚ]",
    i = "[ìíîïĩīĭįıǐ]", I = "[ÌÍÎÏĨĪĬĮİǏ]",
    l = "[ĺļľł]", L = "[ĹĻĽŁ]",
    n = "[ñńņň]", N = "[ÑŃŅŇ]",
    o = "[òóôõöøōŏőǒ]", O = "[ÒÓÔÕÖØŌŎŐǑ]",
    oe = "[œ]", OE = "[Œ]",
    r = "[ŕŗř]", R = "[ŔŖŘ]",
    s = "[śŝşš]", S = "[ŚŜŞŠ]",
    ss = "[ß]",
    t = "[ţťŧ]", T = "[ŢŤŦ]",
    u = "[ùúûüũūŭůűųǔ]", U = "[ÙÚÛÜŨŪŬŮŰŲǓ]",
    y = "[ýÿŷ]", Y = "[ÝŸŶ]",
    z = "[źżž]", Z = "[ŹŻŽ]"
  )
  for (replacement in names(replacement_groups)) {
    x <- gsub(replacement_groups[[replacement]], replacement, x, perl = TRUE)
  }

  converted <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  converted[is.na(converted)] <- x[is.na(converted)]
  converted[is_na] <- NA_character_
  converted
}

normalize_title <- function(x) {
  x <- ascii_fold(x)
  x <- tolower(x)
  x <- gsub("[-–—]", " ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

normalize_name <- function(x) {
  x <- ascii_fold(x)
  x <- tolower(x)
  x <- gsub("<[^>]+@[^>]+>", "", x)
  x <- gsub("\\s*\\([0-9]+\\)", "", x, perl = TRUE)
  x <- gsub("\\b(assoc|assistant|prof|dr|phd)\\b", " ", x)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

make_name_token_key <- function(x) {
  x <- normalize_name(x)
  tokens <- strsplit(x, " ", fixed = TRUE)
  vapply(
    tokens,
    function(parts) {
      parts <- parts[nzchar(parts)]
      if (length(parts) < 2L) {
        return(NA_character_)
      }
      paste(sort(parts), collapse = " ")
    },
    character(1)
  )
}

strip_affiliation_markers <- function(x) {
  x <- gsub("\\s*\\([0-9]+\\)", "", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}

split_people <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(character())
  }

  people <- trimws(strsplit(strip_affiliation_markers(x), ",", fixed = TRUE)[[
    1
  ]])
  people[nzchar(people)]
}

collapse_people <- function(people) {
  if (!length(people)) {
    return(NA_character_)
  }

  paste(people, collapse = ", ")
}

make_registered_name_variants <- function(firstname, lastname) {
  firstname <- trimws(firstname)
  lastname <- trimws(lastname)
  display_name <- trimws(gsub("\\s+", " ", paste(firstname, lastname)))
  variants <- c(
    paste(lastname, firstname),
    paste(firstname, lastname)
  )
  variants <- normalize_name(variants[nzchar(trimws(variants))])
  data.table(
    name_key = variants,
    token_key = make_name_token_key(variants),
    registered_name = display_name
  )
}

best_registered_match <- function(
  author,
  registered_names,
  max_distance = 2L
) {
  author_norm <- normalize_name(author)
  author_token_key <- make_name_token_key(author)
  if (!nzchar(author_norm)) {
    return(list(
      matched = FALSE,
      distance = NA_real_,
      registered_name = NA_character_
    ))
  }

  exact_match <- match(author_norm, registered_names$name_key)
  if (!is.na(exact_match)) {
    return(list(
      matched = TRUE,
      distance = 0,
      registered_name = registered_names$registered_name[[exact_match]]
    ))
  }

  if (!is.na(author_token_key)) {
    token_match <- match(author_token_key, registered_names$token_key)
    if (!is.na(token_match)) {
      return(list(
        matched = TRUE,
        distance = 0,
        registered_name = registered_names$registered_name[[token_match]]
      ))
    }
  }

  if (!nrow(registered_names)) {
    return(list(
      matched = FALSE,
      distance = NA_real_,
      registered_name = NA_character_
    ))
  }

  distances <- stringdist::stringdist(
    author_norm,
    registered_names$name_key,
    method = "lv"
  )

  if (
    !is.numeric(distances) || length(distances) == 0 || all(is.na(distances))
  ) {
    return(list(
      matched = FALSE,
      distance = NA_real_,
      registered_name = NA_character_
    ))
  }

  best <- which.min(distances)

  if (!is.numeric(best) || length(best) == 0 || best < 1) {
    return(list(
      matched = FALSE,
      distance = NA_real_,
      registered_name = NA_character_
    ))
  }

  list(
    matched = is.finite(distances[best]) && distances[best] <= max_distance,
    distance = distances[best],
    registered_name = registered_names$registered_name[best]
  )
}

abstracts <- fread(abstracts_file, na.strings = "NA")
submissions <- normalize_export_columns(suppressWarnings(
  fread(submissions_file, na.strings = "NA")
))
registered <- normalize_export_columns(suppressWarnings(
  fread(registered_file, sep = ";")
))

docids <- submissions[
  STATUT == "Accepted",
  .(DOCID, TITLE)
]
docids[, title_key := normalize_title(TITLE)]
docids <- unique(docids[, .(title_key, DOCID)], by = "title_key")

abstracts[, row_id := .I]
abstracts[, title_key := normalize_title(TITLE)]
abstracts <- merge(
  abstracts,
  docids,
  by = "title_key",
  all.x = TRUE,
  sort = FALSE
)
setorder(abstracts, row_id)

registered <- registered[tolower(STATUS) %in% c("registered", "preregistered")]
registered_names <- unique(
  rbindlist(Map(
    make_registered_name_variants,
    registered$FIRSTNAME,
    registered$LASTNAME
  )),
  by = "name_key"
)
registered_names <- registered_names[nzchar(name_key)]

if (nrow(registered_names) == 0) {
  warning("No registered people found. All matches will be FALSE.")
}

authors_by_row <- lapply(abstracts$Authors, split_people)
speakers_by_row <- lapply(abstracts$SPEAKERS, split_people)
max_authors <- max(lengths(authors_by_row), 0L)

detail <- abstracts[, .(DOCID, TITLE, Authors)]
for (author_id in seq_len(max_authors)) {
  author_col <- paste0("author", author_id)
  registered_col <- paste0("registered_author", author_id)
  matched_name_col <- paste0("matched_registered_name", author_id)
  distance_col <- paste0("registration_match_distance", author_id)

  detail[[author_col]] <- vapply(
    authors_by_row,
    function(authors) {
      if (length(authors) < author_id) {
        return(NA_character_)
      }
      authors[[author_id]]
    },
    character(1)
  )

  match_results <- lapply(
    detail[[author_col]],
    best_registered_match,
    registered_names = registered_names
  )

  detail[[registered_col]] <- fifelse(
    is.na(detail[[author_col]]),
    NA,
    vapply(match_results, `[[`, logical(1), "matched")
  )
  detail[[matched_name_col]] <- fifelse(
    is.na(detail[[author_col]]) |
      !vapply(match_results, `[[`, logical(1), "matched"),
    NA_character_,
    vapply(match_results, `[[`, character(1), "registered_name")
  )
  detail[[distance_col]] <- fifelse(
    is.na(detail[[author_col]]),
    NA_real_,
    vapply(match_results, `[[`, numeric(1), "distance")
  )
}

fwrite(detail, speakers_file, na = "NA")

registered_cols <- grep(
  "^registered_author[0-9]+$",
  names(detail),
  value = TRUE
)
at_least_one_author_registered <- if (length(registered_cols)) {
  apply(
    detail[, ..registered_cols],
    1,
    function(x) any(x == TRUE, na.rm = TRUE)
  )
} else {
  rep(FALSE, nrow(detail))
}

speaker_match_results <- lapply(
  speakers_by_row,
  function(speakers) {
    lapply(speakers, best_registered_match, registered_names = registered_names)
  }
)
at_least_one_speaker_registered <- vapply(
  speaker_match_results,
  function(matches) {
    if (!length(matches)) {
      return(FALSE)
    }
    any(vapply(matches, `[[`, logical(1), "matched"), na.rm = TRUE)
  },
  logical(1)
)
registered_speaker_names <- vapply(
  speaker_match_results,
  function(matches) {
    if (!length(matches)) {
      return(NA_character_)
    }
    matched <- vapply(matches, `[[`, logical(1), "matched")
    names <- vapply(matches, `[[`, character(1), "registered_name")
    names <- unique(names[matched & !is.na(names) & nzchar(names)])
    collapse_people(names)
  },
  character(1)
)

summary <- detail[, .(
  DOCID,
  TITLE,
  submission_speaker_names = vapply(speakers_by_row, collapse_people, character(1)),
  registered_speaker_names = registered_speaker_names,
  at_least_one_author_registered = at_least_one_author_registered,
  at_least_one_speaker_registered = at_least_one_speaker_registered
)]

fwrite(summary, summary_file, na = "NA")

print(summary[
  ,
  .N,
  by = .(at_least_one_author_registered, at_least_one_speaker_registered)
])
