#!/usr/bin/env Rscript

# uRos conference Book of Abstracts renderer
# Reads abstracts.csv (pre-filtered by read_submissions.R) and generates a PDF
# with sections by presentation type (Keynotes, Regular Presentations, Lightning Talks)
#
# Usage:
#   Rscript book_abstracts_pdf.R [INPUT.csv] [OUTPUT.pdf]
#
# Dependencies:
#   data.table, rmarkdown, pagedown, knitr

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) {
  normalizePath(args[[1]], mustWork = TRUE)
} else {
  "abstracts.csv"
}
output <- if (length(args) >= 2L) args[[2]] else "uros2026_book_of_abstracts.pdf"
if (!grepl("\\.pdf$", output, ignore.case = TRUE)) {
  output <- paste0(output, ".pdf")
}
output <- normalizePath(dirname(output), mustWork = TRUE) |>
  file.path(basename(output))

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  getwd()
}
# Look for CSS in parent directory (nicerPDF folder)
css_file <- file.path(script_dir, "programme.css")
if (!file.exists(css_file)) {
  css_file <- file.path(dirname(script_dir), "nicerPDF", "programme.css")
}
if (!file.exists(css_file)) {
  stop(
    "programme.css not found in script directory or ../nicerPDF/",
    call. = FALSE
  )
}
logo_candidates <- c(
  file.path(dirname(script_dir), "uros2026_logo.jpg"),
  file.path(dirname(input), "uros2026_logo.jpg"),
  file.path(getwd(), "uros2026_logo.jpg")
)
logo_matches <- logo_candidates[file.exists(logo_candidates)]
if (!length(logo_matches)) {
  stop("uros2026_logo.jpg not found", call. = FALSE)
}
logo_file <- normalizePath(logo_matches[[1]], mustWork = TRUE)

library(data.table)

required <- c("rmarkdown", "pagedown", "knitr")
missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing)) {
  stop(
    "Missing R packages: ",
    paste(missing, collapse = ", "),
    "\nInstall them with: install.packages(c(",
    paste(sprintf("'%s'", missing), collapse = ", "),
    "))",
    call. = FALSE
  )
}

html_escape <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

value_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("")
  }
  x
}

split_people <- function(x) {
  x <- value_or_empty(x)
  if (!nzchar(trimws(x))) {
    return(character())
  }
  people <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  people[nzchar(people)]
}

normalize_person <- function(x) {
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- gsub("\\([^)]*\\)", "", x)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(tolower(x))
}

render_authors <- function(authors, speakers) {
  author_names <- split_people(authors)
  if (!length(author_names)) {
    author_names <- split_people(speakers)
  }
  if (!length(author_names)) {
    return("")
  }

  speaker_keys <- normalize_person(split_people(speakers))
  author_html <- vapply(
    author_names,
    function(author) {
      refs <- regmatches(author, gregexpr("\\([0-9]+\\)", author))[[1]]
      name <- trimws(gsub("\\([0-9]+\\)", "", author))
      escaped_author <- html_escape(name)
      if (length(refs)) escaped_author <- paste0(escaped_author, "<sup>",
        paste(gsub("[()]", "", refs), collapse = ","), "</sup>")
      if (normalize_person(author) %in% speaker_keys) {
        sprintf('<span class="speaker-author">%s</span>', escaped_author)
      } else {
        escaped_author
      }
    },
    character(1)
  )

  sprintf('<div class="author">%s</div>', paste(author_html, collapse = ", "))
}

render_affiliations <- function(labos) {
  labos <- trimws(value_or_empty(labos))
  if (!nzchar(labos)) {
    return("")
  }

  sprintf('<div class="affiliation">%s</div>', html_escape(labos))
}

# Read and process abstracts
# Input should be abstracts.csv (filtered by read_submissions.R)
abstracts <- fread(input)

required_columns <- c("DOCID", "SPEAKERS", "Authors", "LABOS", "TYPDOC", "TITLE", "ABSTRACT")
if (!all(required_columns %in% names(abstracts))) stop("Regenerate abstracts.csv using read_submissions.R")
if (anyDuplicated(abstracts$DOCID)) stop("Duplicate abstract identifiers")
known_types <- c("Tutorial", "Keynote presentation", "Regular presentation", "lightning talk (5min)", "EMOS session")
if (any(!abstracts$TYPDOC %in% known_types)) stop("Unsupported presentation type")
abstracts[, anchor := paste0("abstract-", DOCID)]
tutorial_abstracts <- abstracts[TYPDOC == "Tutorial"]

# Split by presentation type
keynote_abstracts <- abstracts[TYPDOC == "Keynote presentation", ]
regular_abstracts <- abstracts[TYPDOC == "Regular presentation", ]
lightning_abstracts <- abstracts[TYPDOC == "lightning talk (5min)", ]
emos_abstracts <- abstracts[TYPDOC == "EMOS session", ]

render_abstract <- function(t) {
  authors <- if ("Authors" %in% names(t)) t$Authors else t$SPEAKERS
  authors <- render_authors(authors, t$SPEAKERS)
  affiliations <- if ("LABOS" %in% names(t)) {
    render_affiliations(t$LABOS)
  } else {
    ""
  }
  sprintf(
    '<div class="abstract" id="%s"><h3 class="abstract-title">%s</h3>%s%s<div class="abstract-text">%s</div></div>',
    html_escape(t$anchor),
    html_escape(t$TITLE),
    authors,
    affiliations,
    html_escape(t$ABSTRACT)
  )
}

render_section <- function(abstracts_df, section_title, section_id) {
  if (nrow(abstracts_df) == 0) {
    return("")
  }
  talks <- vapply(
    1:nrow(abstracts_df),
    function(i) {
      render_abstract(abstracts_df[i, ])
    },
    character(1)
  )
  sprintf(
    '<section class="abstract-section" id="%s"><h2>%s</h2><div class="abstract-list">%s</div></section>',
    section_id,
    html_escape(section_title),
    paste(talks, collapse = "\n")
  )
}

read_fragment <- function(name) paste(readLines(file.path(script_dir, name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

render_index <- function() {
  people <- rbindlist(lapply(seq_len(nrow(abstracts)), function(i) {
    names <- trimws(gsub("\\([0-9]+\\)", "", split_people(abstracts$Authors[i])))
    data.table(name = names, anchor = abstracts$anchor[i])
  }))
  people <- people[order(match(anchor, ordered_anchors))]
  people[, key := normalize_person(name)]
  # Equivalent punctuation/case variants share one entry; retain accents in print.
  entries <- people[, .(name = name[1], anchors = list(unique(anchor))), by = key]
  sort_key <- tolower(iconv(entries$name, from = "UTF-8", to = "ASCII//TRANSLIT"))
  entries <- entries[order(sort_key)]
  items <- vapply(seq_len(nrow(entries)), function(i) {
    refs <- sprintf('<a class="index-ref" href="#%s" aria-label="Abstract page"></a>', entries$anchors[[i]])
    sprintf('<div class="index-entry">%s: %s</div>', html_escape(entries$name[i]), paste(refs, collapse = ", "))
  }, character(1))
  paste0('<section class="index-page" id="author-index"><h2>Author Index</h2>',
    '<p>Authors and co-authors are listed alphabetically by their names as printed. References give the first page of each abstract.</p>',
    '<div class="index-list">', paste(items, collapse = "\n"), '</div></section>')
}

sections <- list(
  list(keynote_abstracts, "Keynotes", "keynotes"),
  list(tutorial_abstracts, "Tutorials", "tutorials"),
  list(regular_abstracts, "Regular Presentations", "regular-presentations"),
  list(lightning_abstracts, "Lightning Talks", "lightning-talks"),
  list(emos_abstracts, "EMOS Presentations", "emos-presentations")
)
sections <- Filter(function(x) nrow(x[[1]]) > 0, sections)
ordered_anchors <- unlist(lapply(sections, function(x) x[[1]]$anchor))
contents <- c('<section class="contents-page"><h2>Contents</h2><ul class="contents-list">',
  '<li><a href="#editorial-board">Editorial Board</a></li>',
  vapply(sections, function(x) sprintf('<li><a href="#%s">%s</a></li>', x[[3]], x[[2]]), character(1)),
  '<li><a href="#author-index">Author Index</a></li>',
  '<li><a href="#practical-information">Practical Conference Information</a></li></ul>',
  '<p>Presenting authors are underlined. Superscript numbers identify the affiliations supplied with each submission.</p></section>')
body <- paste(c(read_fragment("front_matter.html"), contents,
  vapply(sections, function(x) do.call(render_section, x), character(1)),
  render_index(), read_fragment("practical_information.html")), collapse = "\n")

tmp <- tempfile("uros-abstracts-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
invisible(file.copy(
  css_file,
  file.path(tmp, "programme.css"),
  overwrite = TRUE
))
invisible(file.copy(
  logo_file,
  file.path(tmp, "uros2026_logo.jpg"),
  overwrite = TRUE
))

# Keep book styling separate from programme styling.
abstracts_css <- file.path(tmp, "abstracts.css")
writeLines(c(readLines(css_file), readLines(file.path(script_dir, "abstracts.css"))), abstracts_css)

rmd <- file.path(tmp, "abstracts.Rmd")
html <- file.path(tmp, "abstracts.html")
writeLines(
  c(
    "---",
    "title: null",
    'pagetitle: "International Conference “Use of R in Official Statistics (uRos)” – Book of Abstracts"',
    "lang: en",
    "output:",
    "  pagedown::html_paged:",
    sprintf("    css: %s", basename(abstracts_css)),
    "    self_contained: true",
    "    toc: false",
    "---",
    "",
    body
  ),
  rmd,
  useBytes = TRUE
)

old <- getwd()
setwd(tmp)
on.exit(setwd(old), add = TRUE)
rmarkdown::render(
  input = basename(rmd),
  output_file = basename(html),
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)

pagedown::chrome_print(
  input = html,
  output = output,
  wait = 1,
  verbose = 0,
  extra_args = c("--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage")
)

cat(normalizePath(output, mustWork = FALSE), "\n")
