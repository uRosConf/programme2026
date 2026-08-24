library(data.table)

capitalize_after_nbsp <- function(x) {
  nbsp <- intToUtf8(160)

  vapply(
    x,
    function(value) {
      if (is.na(value)) {
        return(NA_character_)
      }

      value <- gsub("&nbsp;", nbsp, value, fixed = TRUE)
      if (!grepl(nbsp, value, fixed = TRUE)) {
        return(value)
      }

      chars <- strsplit(value, "", useBytes = FALSE)[[1]]
      capitalize_next <- FALSE

      for (i in seq_along(chars)) {
        if (identical(chars[[i]], nbsp)) {
          chars[[i]] <- " "
          capitalize_next <- TRUE
        } else if (capitalize_next && grepl("[[:alpha:]]", chars[[i]], perl = TRUE)) {
          chars[[i]] <- toupper(chars[[i]])
          capitalize_next <- FALSE
        } else if (capitalize_next && !grepl("[[:space:]]", chars[[i]], perl = TRUE)) {
          capitalize_next <- FALSE
        }
      }

      paste0(chars, collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

clean_people <- function(x) {
  x <- capitalize_after_nbsp(x)
  x <- gsub("<[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}>", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("[[:space:]]+,", ",", x)
  x <- gsub(",[[:space:]]*", ", ", x)
  trimws(x)
}

clean_text <- function(x) {
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

abstracts <- fread("submissions_uRos.csv")
colnames(abstracts) <- c(colnames(abstracts)[-1], "V1")
abstracts <- abstracts[
  STATUT == "Accepted" & TYPDOC != "EMOS session",
  .(SPEAKERS, Authors = AUTHORS, LABOS, TYPDOC, TITLE, ABSTRACT)
]
abstracts[, SPEAKERS := clean_people(SPEAKERS)]
abstracts[, Authors := clean_people(Authors)]
abstracts[, LABOS := clean_text(LABOS)]
abstracts[, TITLE := clean_text(TITLE)]
abstracts[, ABSTRACT := clean_text(ABSTRACT)]
fwrite(abstracts, file = "abstracts.csv")
