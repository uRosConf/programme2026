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
        } else if (
          capitalize_next && grepl("[[:alpha:]]", chars[[i]], perl = TRUE)
        ) {
          chars[[i]] <- toupper(chars[[i]])
          capitalize_next <- FALSE
        } else if (
          capitalize_next && !grepl("[[:space:]]", chars[[i]], perl = TRUE)
        ) {
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
keynotes <- data.table(
  SPEAKERS = c("Couch Simon", "Killick Rebecca"),
  Authors = c("Couch Simon", "Killick Rebecca"),
  LABOS = c("Posit PBC", "Clemson University"),
  TYPDOC = c("Keynote presentation", "Keynote presentation"),
  TITLE = c(
    "Practical AI for Data Science",
    "“R and Open-Source: A love story”"
  ),
  ABSTRACT = c(
    'How do we build competent data analysis agents? Data analysis requires a willingness to pause, question conclusions, dig into subtleties, and sit with uncertainty. Frontier LLMs, however, are optimized to push tasks toward completion rather than refraining from answering unanswerable questions. Drawing on his experience building data analysis agents at Posit, Simon will share evaluations that expose where LLM-driven analysis goes wrong and design patterns that keep analyses correct, transparent, and reproducible.',
    'Different people have different motivations for starting and continuing to contribute to open-source software. During the last 20+ years, I have gone through various periods of love, hate, joy, and frustration with R and the wider open-source community. I have found myself, entering my mid-career phase and have been enjoying reflections on how I got to where I am currently, and the pivot points along the way. Open-source has been a constant throughout and that was no accident, but things could have been very different. In this talk, I will share some of these reflections with you and discuss the different ways that we can all make our own contributions, whether for the short-term or as a career path.'
  )
)
fwrite(rbind(abstracts, keynotes), file = "abstracts.csv")
