library(data.table)

capitalize_after_nbsp <- function(x) {
  nbsp <- intToUtf8(160)
  vapply(
    x,
    function(value) {
      if (is.na(value) || !grepl("\\x{00A0}", value, fixed = FALSE, perl = TRUE)) {
        return(value)
      }

      parts <- strsplit(value, nbsp, fixed = TRUE)[[1]]
      if (length(parts) < 2L) {
        return(value)
      }

      parts[-1] <- vapply(
        parts[-1],
        function(part) {
          if (!nzchar(part)) {
            return(part)
          }
          paste0(toupper(substr(part, 1L, 1L)), substr(part, 2L, nchar(part)))
        },
        character(1)
      )

      paste(parts, collapse = " ")
    },
    character(1)
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
