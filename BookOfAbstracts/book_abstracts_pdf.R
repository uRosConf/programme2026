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
output <- if (length(args) >= 2L) args[[2]] else "book_abstracts.pdf"
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
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# Read and process abstracts
# Input should be abstracts.csv (filtered by read_submissions.R)
abstracts <- fread(input)

# Exclude EMOS sessions and Tutorials
abstracts <- abstracts[!TYPDOC %in% c("EMOS session", "Tutorial"), ]

# Split by presentation type
keynote_abstracts <- abstracts[TYPDOC == "Keynote presentation", ]
regular_abstracts <- abstracts[TYPDOC == "Regular presentation", ]
lightning_abstracts <- abstracts[TYPDOC == "lightning talk (5min)", ]

render_abstract <- function(t) {
  speakers <- if (nzchar(t$SPEAKERS)) {
    sprintf('<div class="author">%s</div>', html_escape(trimws(t$SPEAKERS)))
  } else {
    ""
  }
  sprintf(
    '<div class="abstract"><div class="abstract-title">%s</div>%s<div class="abstract-text">%s</div></div>',
    html_escape(t$TITLE),
    speakers,
    html_escape(t$ABSTRACT)
  )
}

render_section <- function(abstracts_df, section_title) {
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
    '<section class="abstract-section"><h2>%s</h2><div class="abstract-list">%s</div></section>',
    html_escape(section_title),
    paste(talks, collapse = "\n")
  )
}

render_title_page <- function(logo_src) {
  paste(
    c(
      '<section class="title-page">',
      sprintf(
        '<img class="title-logo" src="%s" alt="uRos 2026 logo">',
        html_escape(logo_src)
      ),
      '<h1>Book of Abstracts</h1>',
      '<div class="programme-label">The Use of R in Official Statistics - uRos2026 Conference</div>',
      '<div class="subtitle">Paris · 18–20 November 2026</div>',
      '</section>'
    ),
    collapse = "\n"
  )
}

render_html_body <- function(keynote_df, regular_df, lightning_df, logo_src) {
  out <- c(
    render_title_page(logo_src)
  )

  out <- c(out, render_section(keynote_df, "Keynotes"))
  out <- c(out, render_section(regular_df, "Regular Presentations"))
  out <- c(out, render_section(lightning_df, "Lightning Talks"))

  paste(out, collapse = "\n")
}

body <- render_html_body(
  keynote_abstracts,
  regular_abstracts,
  lightning_abstracts,
  "uros2026_logo.jpg"
)

tmp <- tempfile("uros-abstracts-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
invisible(file.copy(css_file, file.path(tmp, "programme.css"), overwrite = TRUE))
invisible(file.copy(logo_file, file.path(tmp, "uros2026_logo.jpg"), overwrite = TRUE))

# Create modified CSS for abstracts
abstracts_css <- file.path(tmp, "abstracts.css")
writeLines(
  c(
    readLines(css_file),
    "",
    ".title-page { min-height: 260mm; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; break-after: page; }",
    ".title-logo { display: block; width: 48mm; height: auto; margin: 0 0 12mm; }",
    ".title-page h1 { margin: 0 0 4mm; font-size: 34pt; line-height: 1.05; }",
    ".title-page .programme-label { max-width: 150mm; font-size: 15pt; line-height: 1.25; margin-bottom: 3mm; }",
    ".title-page .subtitle { font-size: 11pt; }",
    ".abstract-section { break-before: page; }",
    ".abstract-section h2 { font-size: 18pt; color: #2879b9; margin-bottom: 4mm; }",
    ".abstract-list { columns: 1; }",
    ".abstract { break-inside: avoid; margin-bottom: 6mm; padding: 3mm; background: #f9f9f9; border-radius: 4px; }",
    ".abstract-title { font-size: 13pt; font-weight: 700; margin-bottom: 2mm; color: #222d3b; }",
    ".abstract .author { color: #657386; font-size: 9pt; margin-bottom: 3mm; font-style: italic; }",
    ".abstract-text { font-size: 9pt; line-height: 1.4; text-align: justify; }",
    ".abstract-text p { margin: 0 0 2mm; }"
  ),
  abstracts_css
)

rmd <- file.path(tmp, "abstracts.Rmd")
html <- file.path(tmp, "abstracts.html")
writeLines(
  c(
    "---",
    "title: null",
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
  verbose = 0
)

cat(normalizePath(output, mustWork = FALSE), "\n")
