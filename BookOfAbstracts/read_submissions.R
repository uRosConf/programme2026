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
  x <- gsub(" Assoc. Prof.", " ", x)
  trimws(x)
}

clean_text <- function(x) {
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

clean_md <- function(x) {
  x <- trimws(x)
  whole_bold <- grepl("^\\*\\*.*\\*\\*$", x)
  x[whole_bold] <- sub("^\\*\\*(.*)\\*\\*$", "\\1", x[whole_bold])
  x <- gsub("\\\\\\*", "*", x)
  x <- gsub("\\\\-", "-", x)
  x <- gsub("\\\\\\.", ".", x)
  x <- gsub("\\\\\\|", "|", x)
  x <- gsub("\\*\\*", "", x)
  trimws(x)
}

normalize_title <- function(x) {
  x <- clean_md(x)
  x <- sub("^Keynote:\\s*", "", x, ignore.case = TRUE)
  x <- gsub("[\u2018\u2019\u201c\u201d]", "", x)
  x <- sub(
    "\\s+[-\u2013\u2014]\\s+(i|ii|iii|iv|v)\\s*$",
    "",
    x,
    ignore.case = TRUE
  )
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(tolower(x))
}

split_people <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(character())
  }
  people <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  people[nzchar(people)]
}

normalize_person <- function(x) {
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("\\([^)]*\\)", "", x)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(tolower(x))
}

person_match_keys <- function(x) {
  normalized <- normalize_person(x)
  if (!nzchar(normalized)) {
    return(character())
  }
  words <- strsplit(normalized, " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  unique(c(normalized, paste(sort(words), collapse = " ")))
}

extract_programme_entry <- function(cell) {
  cell <- trimws(cell)
  cell <- sub("^\\s*(?:[-*]|[0-9]+\\.)\\s+", "", cell, perl = TRUE)
  match <- regexec("^\\*\\*(.*?)\\*\\*\\s*-\\s*(.*?)\\s*$", cell, perl = TRUE)
  parts <- regmatches(cell, match)[[1]]
  if (length(parts) != 3L) {
    return(NULL)
  }

  title <- clean_md(parts[[2]])
  authors <- clean_md(parts[[3]])
  authors <- sub("\\s*\\(chair:\\s*.*\\)\\s*$", "", authors, ignore.case = TRUE)
  authors <- gsub("[[:space:]]+", " ", authors)
  authors <- trimws(authors)

  if (!nzchar(title) || !nzchar(authors)) {
    return(NULL)
  }

  list(title = title, authors = authors)
}

load_programme_author_lookup <- function(programme_file = "README.md") {
  lookup <- new.env(parent = emptyenv())
  if (!file.exists(programme_file)) {
    return(lookup)
  }

  lines <- readLines(programme_file, warn = FALSE, encoding = "UTF-8")
  for (line in lines) {
    cells <- if (grepl("^\\s*\\|", line)) {
      strsplit(line, "\\|", perl = TRUE)[[1]]
    } else {
      line
    }

    for (cell in cells) {
      entry <- extract_programme_entry(cell)
      if (is.null(entry)) {
        next
      }

      key <- normalize_title(entry$title)
      if (nzchar(key)) {
        assign(key, entry$authors, envir = lookup)
      }
    }
  }

  lookup
}

programme_authors <- function(title, fallback, lookup) {
  key <- normalize_title(title)
  if (nzchar(key) && exists(key, lookup, inherits = FALSE)) {
    get(key, lookup, inherits = FALSE)
  } else {
    fallback
  }
}

match_people_to_reference <- function(people, reference) {
  people <- split_people(people)
  reference <- split_people(reference)
  if (!length(people) || !length(reference)) {
    return(paste(people, collapse = ", "))
  }

  matched <- vapply(
    people,
    function(person) {
      person_keys <- person_match_keys(person)
      hits <- vapply(
        reference,
        function(ref_person) {
          any(person_match_keys(ref_person) %in% person_keys)
        },
        logical(1)
      )
      if (any(hits)) {
        reference[[which(hits)[[1]]]]
      } else {
        person
      }
    },
    character(1)
  )

  paste(matched, collapse = ", ")
}

# The export has an unnamed trailing field. Read its actual header explicitly.
header <- names(fread(
  text = readLines("submissions_uRos.csv", n = 1L),
  header = TRUE
))
abstracts <- fread("submissions_uRos.csv", skip = 1L, header = FALSE)
stopifnot(ncol(abstracts) %in% c(length(header), length(header) + 1L))
setnames(
  abstracts,
  c(header, if (ncol(abstracts) > length(header)) "EXPORT_EXTRA")
)
submissions <- copy(abstracts)
accepted_emos_docids <- c(
  "757980",
  "754557",
  "757979",
  "757239",
  "758051",
  "758044",
  "758046"
)
excluded_docids <- c(
  "750510",
  "735527",
  "752233",
  "746316",
  "755216",
  "752386",
  "752426",
  "751177",
  "752613",
  "752805",
  "751833",
  "751542",
  "752869",
  "752081",
  "752605",
  "752408",
  "748022",
  "726038"
)
abstracts <- abstracts[
  STATUT == "Accepted" &
    !as.character(DOCID) %in% excluded_docids &
    (TYPDOC != "EMOS session" |
      (TYPDOC == "EMOS session" & DOCID %in% accepted_emos_docids)),
  .(
    DOCID = as.character(DOCID),
    SPEAKERS,
    Authors = AUTHORS,
    LABOS,
    TYPDOC,
    TITLE,
    ABSTRACT,
    Email = MAIL
  )
]
abstracts[, SPEAKERS := clean_people(SPEAKERS)]
abstracts[, Authors := clean_people(Authors)]
abstracts[, LABOS := clean_text(LABOS)]
abstracts[, TITLE := clean_text(TITLE)]
abstracts[, ABSTRACT := clean_text(ABSTRACT)]
programme_lookup <- load_programme_author_lookup()
# Keep submitted co-authors and their affiliation references. Programme names
# supply spelling/order, but must never replace the complete submitted list.
merge_authors <- function(title, submitted) {
  reference <- split_people(programme_authors(title, "", programme_lookup))
  original <- split_people(submitted)
  used <- rep(FALSE, length(original))
  result <- vapply(
    reference,
    function(person) {
      hits <- which(vapply(
        original,
        function(author) {
          any(person_match_keys(author) %in% person_match_keys(person))
        },
        logical(1)
      ))
      if (length(hits) == 1L) {
        used[hits] <<- TRUE
        refs <- regmatches(
          original[hits],
          gregexpr("\\([0-9]+\\)", original[hits])
        )[[1]]
        return(trimws(paste(person, paste(refs, collapse = " "))))
      }
      person
    },
    character(1)
  )
  paste(c(result, original[!used]), collapse = ", ")
}
abstracts[, Authors := mapply(merge_authors, TITLE, Authors, USE.NAMES = FALSE)]
abstracts[,
  SPEAKERS := mapply(
    match_people_to_reference,
    SPEAKERS,
    Authors,
    USE.NAMES = FALSE
  )
]
keynotes <- data.table(
  DOCID = c("invited-couch", "invited-killick", "invited-alexandru"),
  SPEAKERS = c("Simon Couch", "Rebecca Killick", "Ciprian Alexandru"),
  Authors = c("Simon Couch", "Rebecca Killick", "Ciprian Alexandru"),
  LABOS = c(
    "Posit PBC",
    "Clemson University",
    "Ecological University of Bucharest"
  ),
  TYPDOC = c(
    "Keynote presentation",
    "Keynote presentation",
    "Regular presentation"
  ),
  TITLE = c(
    "Practical AI for Data Science",
    "“R and Open-Source: A love story”",
    "From Data Acquisition to Harmonised Outputs: A Reproducible R Workflow for Official Statistics"
  ),
  ABSTRACT = c(
    'How do we build competent data analysis agents? Data analysis requires a willingness to pause, question conclusions, dig into subtleties, and sit with uncertainty. Frontier LLMs, however, are optimized to push tasks toward completion rather than refraining from answering unanswerable questions. Drawing on his experience building data analysis agents at Posit, Simon will share evaluations that expose where LLM-driven analysis goes wrong and design patterns that keep analyses correct, transparent, and reproducible.',
    'Different people have different motivations for starting and continuing to contribute to open-source software. During the last 20+ years, I have gone through various periods of love, hate, joy, and frustration with R and the wider open-source community. I have found myself, entering my mid-career phase and have been enjoying reflections on how I got to where I am currently, and the pivot points along the way. Open-source has been a constant throughout and that was no accident, but things could have been very different. In this talk, I will share some of these reflections with you and discuss the different ways that we can all make our own contributions, whether for the short-term or as a career path.',
    'Statistical production workflows often evolve incrementally. What begins as a collection of R scripts for downloading, cleaning and transforming individual datasets can gradually become difficult to maintain, reproduce and audit, particularly when data originate from heterogeneous sources and require different processing rules.
This presentation describes, from an R user and developer perspective, the redesign of such a workflow into a modular and reproducible R-based pipeline.
The approach separates the workflow into two R packages with distinct responsibilities. The first package, manages data acquisition from heterogeneous sources, including APIs, databases and externally provided files. The second, implements the subsequent cleaning, harmonisation and refinement stages. This separation makes it possible to distinguish source-specific data access from statistical processing logic and to develop and test both components independently.
A metadata-driven approach is used to reduce dataset-specific code. YAML configuration files describe data sources, parameters and processing requirements, while common functions translate these configurations into reproducible operations. The workflow combines package-based development with targets for pipeline orchestration, {renv} for dependency management, structured logging for monitoring and diagnostics, and Git-based version control. Intermediate and final datasets are stored together with information supporting data lineage and reproducibility.
Particular attention is given to practical challenges encountered by an R user when moving from exploratory scripts towards a production-oriented workflow: handling heterogeneous data structures, defining common data models, separating acquisition from transformation, implementing validation checks, managing data vintages, and designing logs that can subsequently support workflow monitoring and visualisation.
'
  )
)
fwrite(
  abstracts[TYPDOC == "Regular presentation", .(Authors, SPEAKERS, Email)],
  file = "emailRegular.csv"
)
fwrite(
  abstracts[TYPDOC == "lightning talk (5min)", .(Authors, SPEAKERS, Email)],
  file = "emailLightning.csv"
)
fwrite(
  abstracts[TYPDOC == "EMOS session", .(Authors, SPEAKERS, Email)],
  file = "emailEMOS.csv"
)
abstracts[, Email := NULL]
abstracts <- rbind(abstracts, keynotes)
fwrite(abstracts, file = "abstracts.csv")
source("BookOfAbstracts/review_submissions.R")
