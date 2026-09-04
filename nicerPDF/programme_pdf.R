#!/usr/bin/env Rscript

# uRos conference programme renderer
# Markdown -> parsed programme structure -> paged HTML -> PDF
#
# Usage:
#   Rscript programme_pdf.R programme.md programme.pdf
#
# Dependencies:
#   rmarkdown, pagedown, knitr

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: Rscript programme_pdf.R INPUT.md [OUTPUT.pdf]", call. = FALSE)
}

input <- normalizePath(args[[1]], mustWork = TRUE)
output <- if (length(args) >= 2L) args[[2]] else "programme.pdf"
if (!grepl("\\.pdf$", output, ignore.case = TRUE)) output <- paste0(output, ".pdf")
output <- normalizePath(dirname(output), mustWork = TRUE) |> file.path(basename(output))
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  getwd()
}
css_file <- file.path(script_dir, "programme.css")
if (!file.exists(css_file)) stop("programme.css not found next to programme_pdf.R", call. = FALSE)
logo_candidates <- c(
  file.path(dirname(script_dir), "uros2026_logo.jpg"),
  file.path(dirname(input), "uros2026_logo.jpg"),
  file.path(getwd(), "uros2026_logo.jpg")
)
logo_matches <- logo_candidates[file.exists(logo_candidates)]
if (!length(logo_matches)) stop("uros2026_logo.jpg not found", call. = FALSE)
logo_file <- normalizePath(logo_matches[[1]], mustWork = TRUE)

required <- c("rmarkdown", "pagedown", "knitr")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  stop(
    "Missing R packages: ", paste(missing, collapse = ", "),
    "\nInstall them with: install.packages(c(",
    paste(sprintf("'%s'", missing), collapse = ", "), "))",
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

clean_table_cell <- function(x) {
  x <- trimws(x)
  x <- gsub("\\\\\\*", "*", x)
  x <- gsub("\\\\-", "-", x)
  x <- gsub("\\\\\\.", ".", x)
  x <- gsub("\\\\\\|", "|", x)
  trimws(x)
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

normalize_title <- function(x) {
  x <- value_or_empty(x)
  x <- clean_md(x)
  x <- sub("^Keynote:\\s*", "", x, ignore.case = TRUE)
  x <- gsub("[\u2018\u2019\u201c\u201d]", "", x)
  x <- sub("\\s+[-\u2013\u2014]\\s+(i|ii|iii|iv|v)\\s*$", "", x, ignore.case = TRUE)
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(tolower(x))
}

load_speaker_lookup <- function(input) {
  candidates <- c(
    file.path(dirname(input), "abstracts.csv"),
    file.path(getwd(), "abstracts.csv"),
    file.path(dirname(script_dir), "abstracts.csv")
  )
  matches <- candidates[file.exists(candidates)]
  if (!length(matches)) {
    return(new.env(parent = emptyenv()))
  }

  abstracts <- utils::read.csv(
    matches[[1]],
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  required_cols <- c("TITLE", "SPEAKERS")
  if (!all(required_cols %in% names(abstracts))) {
    return(new.env(parent = emptyenv()))
  }

  lookup <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(abstracts))) {
    key <- normalize_title(abstracts$TITLE[[i]])
    if (nzchar(key) && !exists(key, lookup, inherits = FALSE)) {
      assign(key, abstracts$SPEAKERS[[i]], envir = lookup)
    }
  }
  lookup
}

find_speakers <- function(title, speaker_lookup) {
  key <- normalize_title(title)
  if (nzchar(key) && exists(key, speaker_lookup, inherits = FALSE)) {
    get(key, speaker_lookup, inherits = FALSE)
  } else {
    ""
  }
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
      escaped_author <- html_escape(author)
      if (normalize_person(author) %in% speaker_keys) {
        sprintf('<span class="speaker-author">%s</span>', escaped_author)
      } else {
        escaped_author
      }
    },
    character(1)
  )

  paste(author_html, collapse = ", ")
}

split_talk <- function(x) {
  x <- clean_md(x)
  # Split at the final em dash surrounded by spaces; fall back to final " - ".
  pos <- gregexpr(" \u2014 ", x, fixed = TRUE)[[1]]
  if (pos[[1]] != -1L) {
    p <- tail(pos, 1)
    title <- substr(x, 1L, p - 1L)
    author <- substr(x, p + 3L, nchar(x))
  } else {
    pos <- gregexpr(" - ", x, fixed = TRUE)[[1]]
    if (pos[[1]] != -1L) {
      p <- tail(pos, 1)
      title <- substr(x, 1L, p - 1L)
      author <- substr(x, p + 3L, nchar(x))
    } else {
      title <- x
      author <- ""
    }
  }
  list(title = clean_md(title), author = clean_md(author))
}

split_chair <- function(x) {
  x <- clean_md(x)
  chair_match <- regexec("^(.*?)\\s*\\((chair:\\s*[^)]*)\\)\\s*$", x, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(x, chair_match)[[1]]
  if (length(parts) == 3L) {
    list(title = clean_md(parts[[2]]), chair = paste0("(", clean_md(parts[[3]]), ")"))
  } else {
    list(title = x, chair = "")
  }
}

is_table_row <- function(x) grepl("^\\s*\\|.*\\|\\s*$", clean_md(x))

parse_table <- function(lines, i) {
  rows <- list()
  n <- length(lines)
  while (i <= n && is_table_row(lines[[i]])) {
    s <- clean_table_cell(lines[[i]])
    s <- sub("^\\|", "", s)
    s <- sub("\\|$", "", s)
    cells <- strsplit(s, "\\|", fixed = FALSE)[[1]] |> clean_table_cell()
    compact <- gsub(" ", "", cells, fixed = TRUE)
    separator <- all(grepl("^:?-{3,}:?$", compact))
    if (!separator) rows[[length(rows) + 1L]] <- cells
    i <- i + 1L
  }
  list(rows = rows, next_i = i)
}

parse_programme <- function(text) {
  lines <- strsplit(text, "\\r?\\n")[[1]]
  doc <- list(title = "Programme", subtitle = "", days = list())
  current_day <- 0L
  current_session <- 0L
  i <- 1L
  n <- length(lines)

  while (i <= n) {
    raw <- clean_md(lines[[i]])
    if (!nzchar(raw)) { i <- i + 1L; next }

    if (grepl("^# ", raw) && !grepl("^## ", raw)) {
      doc$title <- clean_md(sub("^# ", "", raw))
      i <- i + 1L
      next
    }

    if (length(doc$days) == 0L && !nzchar(doc$subtitle) && !grepl("^#", raw)) {
      doc$subtitle <- clean_md(raw)
      i <- i + 1L
      next
    }

    if (grepl("^## ", raw)) {
      h <- clean_md(sub("^## ", "", raw))
      parts <- strsplit(h, " \u2014 ", fixed = TRUE)[[1]]
      if (length(parts) >= 2L) {
        left <- clean_md(parts[[1]])
        right <- clean_md(paste(parts[-1], collapse = " \u2014 "))
        if (grepl("[0-9]{4}", left)) {
          date <- left; label <- right
        } else {
          label <- left; date <- right
        }
      } else {
        label <- "Programme"; date <- h
      }
      doc$days[[length(doc$days) + 1L]] <- list(label = label, date = date, table = list(), sessions = list())
      current_day <- length(doc$days)
      current_session <- 0L
      i <- i + 1L
      while (i <= n && !nzchar(clean_md(lines[[i]]))) i <- i + 1L
      if (i <= n && is_table_row(lines[[i]])) {
        tt <- parse_table(lines, i)
        doc$days[[current_day]]$table <- tt$rows
        i <- tt$next_i
      }
      next
    }

    if (grepl("^### ", raw) && current_day > 0L) {
      h <- clean_md(sub("^### ", "", raw))
      if (identical(h, "EMOS Presentations")) {
        code <- "EMOS Presentations"; name <- "EMOS Presentations"
      } else if (grepl("^[ABC][0-9]+", h)) {
        code <- sub("^([ABC][0-9]+).*$", "\\1", h)
        name <- sub("^[ABC][0-9]+\\s*(?:\u2014|-)?\\s*", "", h, perl = TRUE)
        if (!nzchar(name)) name <- code
      } else {
        i <- i + 1L
        next
      }
      doc$days[[current_day]]$sessions[[length(doc$days[[current_day]]$sessions) + 1L]] <-
        list(code = code, name = name, talks = list())
      current_session <- length(doc$days[[current_day]]$sessions)
      i <- i + 1L
      next
    }

    if (current_day > 0L && current_session > 0L &&
        grepl("^([-*]|[0-9]+\\.)\\s+", raw)) {
      body <- sub("^([-*]|[0-9]+\\.)\\s+", "", raw)
      talk <- split_talk(body)
      s <- doc$days[[current_day]]$sessions[[current_session]]
      s$talks[[length(s$talks) + 1L]] <- talk
      doc$days[[current_day]]$sessions[[current_session]] <- s
      i <- i + 1L
      next
    }

    i <- i + 1L
  }
  doc
}

render_schedule_cell <- function(x, speaker_lookup) {
  x <- clean_table_cell(x)
  if (!nzchar(x)) return("")

  bold_match <- regexec("^\\*\\*(.*?)\\*\\*\\s*(?:(?:\u2014|-)\\s*)?(.*?)\\s*$", x, perl = TRUE)
  parts <- regmatches(x, bold_match)[[1]]
  if (length(parts) == 3L) {
    title_parts <- split_chair(parts[[2]])
    title <- html_escape(title_parts$title)
    author <- clean_md(parts[[3]])
    speakers <- find_speakers(title_parts$title, speaker_lookup)
    keynote <- grepl("^Keynote:", title, ignore.case = TRUE)
    title_class <- if (keynote) "schedule-title keynote-title" else "schedule-title"
    author_html <- if (nzchar(author)) {
      render_authors(author, speakers)
    } else {
      ""
    }
    meta <- c(author_html, html_escape(title_parts$chair))
    meta <- meta[nzchar(meta)]
    meta_html <- if (length(meta)) {
      paste(sprintf('<div class="author schedule-author">%s</div>', meta), collapse = "")
    } else {
      ""
    }
    return(sprintf('<strong class="%s">%s</strong>%s', title_class, title, meta_html))
  }

  html_escape(clean_md(x))
}

is_event_row <- function(row) {
  labels <- tolower(clean_md(row[-1]))
  labels <- labels[nzchar(labels)]
  if (!length(labels)) return(FALSE)

  is_event_label <- function(x) {
    x %in% c("coffee break", "lunch break", "registration and walk-in",
             "opening", "closing", "emos presentations") ||
      grepl("^keynote:", x)
  }
  all(vapply(labels, is_event_label, logical(1)))
}

render_table <- function(rows, speaker_lookup) {
  if (!length(rows)) return("")
  header <- rows[[1]]
  body <- rows[-1]
  n_cols <- length(header)
  h <- paste0(
    '<table class="schedule"><thead><tr>',
    paste(sprintf("<th>%s</th>", html_escape(header)), collapse = ""),
    "</tr></thead><tbody>"
  )
  for (row in body) {
    event <- is_event_row(row)
    cls <- if (event) ' class="event-row"' else ""
    cells <- vapply(row, render_schedule_cell, character(1), speaker_lookup = speaker_lookup)
    if (length(cells) == 2L && n_cols > 2L) {
      row_html <- paste0(
        sprintf("<td>%s</td>", cells[[1]]),
        sprintf('<td colspan="%d">%s</td>', n_cols - 1L, cells[[2]])
      )
    } else {
      if (length(cells) < n_cols) cells <- c(cells, rep("", n_cols - length(cells)))
      row_html <- paste(sprintf("<td>%s</td>", cells), collapse = "")
    }
    h <- paste0(h, "<tr", cls, ">", row_html, "</tr>")
  }
  paste0(h, "</tbody></table>")
}

render_session <- function(s, speaker_lookup) {
  code_first <- substr(s$code, 1L, 1L)
  track <- if (code_first %in% c("A", "B", "C")) tolower(code_first) else "e"
  lightning <- if (length(s$talks) > 9L) " lightning" else ""
  talks <- vapply(s$talks, function(t) {
    speakers <- find_speakers(t$title, speaker_lookup)
    author <- if (nzchar(t$author)) {
      sprintf('<div class="author">%s</div>', render_authors(t$author, speakers))
    } else {
      ""
    }
    sprintf('<div class="talk"><div class="talk-title">%s</div>%s</div>', html_escape(t$title), author)
  }, character(1))
  sprintf(
    paste0('<article class="session%s">',
           '<div class="session-head track-%s"><div class="code">%s</div>',
           '<div class="name">%s</div></div><div class="talk-list">%s</div></article>'),
    lightning, track, html_escape(s$code), html_escape(s$name), paste(talks, collapse = "")
  )
}

render_html_body <- function(doc, logo_src, speaker_lookup) {
  out <- c(
    '<header class="masthead">',
    sprintf(
      '<img class="conference-logo" src="%s" alt="uRos 2026 logo">',
      html_escape(logo_src)
    ),
    sprintf('<h1>%s</h1>', html_escape(doc$title)),
    '<div class="programme-label">Conference Programme</div>',
    sprintf('<div class="subtitle">%s</div>', html_escape(doc$subtitle)),
    '</header>'
  )
  for (d in seq_along(doc$days)) {
    day <- doc$days[[d]]
    first <- if (d == 1L) " first-day" else ""
    out <- c(out,
      sprintf('<section class="day%s">', first),
      sprintf('<div class="eyebrow">%s</div>', toupper(html_escape(day$label))),
      sprintf('<h2>%s</h2>', html_escape(day$date)),
      render_table(day$table, speaker_lookup)
    )
    if (length(day$sessions)) {
      out <- c(out, '<div class="sessions">',
               vapply(day$sessions, render_session, character(1), speaker_lookup = speaker_lookup), '</div>')
    }
    out <- c(out, '</section>')
  }
  paste(out, collapse = "\n")
}

text <- paste(readLines(input, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
doc <- parse_programme(text)
speaker_lookup <- load_speaker_lookup(input)
body <- render_html_body(doc, "uros2026_logo.jpg", speaker_lookup)

tmp <- tempfile("uros-programme-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
invisible(file.copy(css_file, file.path(tmp, "programme.css"), overwrite = TRUE))
invisible(file.copy(logo_file, file.path(tmp, "uros2026_logo.jpg"), overwrite = TRUE))

rmd <- file.path(tmp, "programme.Rmd")
html <- file.path(tmp, "programme.html")
writeLines(c(
  "---",
  "title: null",
  "output:",
  "  pagedown::html_paged:",
  "    css: programme.css",
  "    self_contained: true",
  "    toc: false",
  "---",
  "",
  body
), rmd, useBytes = TRUE)

old <- getwd(); setwd(tmp); on.exit(setwd(old), add = TRUE)
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
