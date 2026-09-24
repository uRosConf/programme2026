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

abstracts <- fread("submissions_uRos.csv")
colnames(abstracts) <- c(colnames(abstracts)[-1], "V1")
accepted_emos_docids <- c(
  "757980",
  "754557",
  "757979",
  "757239",
  "758051",
  "758044"
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
  "751833"
)
abstracts <- abstracts[
  STATUT == "Accepted" &
    !as.character(DOCID) %in% excluded_docids &
    (TYPDOC != "EMOS session" |
      (TYPDOC == "EMOS session" & DOCID %in% accepted_emos_docids)),
  .(SPEAKERS, Authors = AUTHORS, LABOS, TYPDOC, TITLE, ABSTRACT)
]
abstracts[, SPEAKERS := clean_people(SPEAKERS)]
abstracts[, Authors := clean_people(Authors)]
abstracts[, LABOS := clean_text(LABOS)]
abstracts[, TITLE := clean_text(TITLE)]
abstracts[, ABSTRACT := clean_text(ABSTRACT)]
keynotes <- data.table(
  SPEAKERS = c("Couch Simon", "Killick Rebecca", "Alexandru Ciprian"),
  Authors = c("Couch Simon", "Killick Rebecca", "Alexandru Ciprian"),
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


fwrite(rbind(abstracts, keynotes), file = "abstracts.csv")
