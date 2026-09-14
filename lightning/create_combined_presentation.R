# ============================================================
# uRos 2026 Lightning Talk Presentation Builder
#
# For every speaker:
#   1 transition/title slide (20 seconds)
#   15 presentation slides (20 seconds each)
#
# Result:
#   One combined PDF
#   16 pages / 5:20 per contribution
# ============================================================


# ---- Packages ------------------------------------------------

packages <- c("pdftools", "qpdf")

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing)) {
  install.packages(missing)
}


# ---- Configuration -------------------------------------------

SECONDS_PER_SLIDE <- 20
EXPECTED_SLIDES   <- 15



TEMP_DIR <- tempfile("uros2026_")
dir.create(TEMP_DIR)


# ---- Programme -----------------------------------------------
OUTPUT_FILE <- "B2_uRos2026_lightning_talks.pdf"
presentations <- data.frame(
  speaker = c(
    "Jane Smith",
    "John Doe & Maria Rossi",
    "Alexander Example"
  ),
  title = c(
    "Using R in Official Statistics",
    "Modern Approaches to Survey Sampling",
    "Machine Learning for Statistical Production"
  ),
  file = c(
    "~/git/programme2026/lightning/B2/dummy1.pdf",
    "~/git/programme2026/lightning/B2/dummy2.pdf",
    "~/git/programme2026/lightning/B2/dummy3.pdf"
  ),
  stringsAsFactors = FALSE
)


# ---- Helper: wrap text ----------------------------------------

wrap_text <- function(x, width = 45) {
  paste(strwrap(x, width = width), collapse = "\n")
}


# ---- Create transition slide ---------------------------------

create_title_slide <- function(speaker, title, filename) {

  # 16:9 dimensions in inches
  pdf(
    filename,
    width = 13.333,
    height = 7.5,
    paper = "special",
    useDingbats = FALSE
  )

  par(
    mar = c(0, 0, 0, 0),
    xaxs = "i",
    yaxs = "i"
  )

  plot.new()
  plot.window(
    xlim = c(0, 100),
    ylim = c(0, 56.25)
  )

  # Background
  rect(
    0, 0, 100, 56.25,
    col = "white",
    border = NA
  )

  # ------------------------------------------------------------
  # Placeholder uRos 2026 logo
  # Replace this block later with rasterImage() or similar.
  # ------------------------------------------------------------

  rect(
    77, 46,
    94, 52.5,
    border = "#333333",
    lwd = 2
  )

  text(
    85.5, 49.8,
    "uRos 2026",
    cex = 2,
    font = 2
  )

  text(
    85.5, 47.4,
    "PARIS",
    cex = 0.75
  )


  # Small heading
  text(
    8, 47,
    "LIGHTNING TALK",
    adj = c(0, 0.5),
    cex = 1.1,
    font = 2
  )


  # Talk title
  title_wrapped <- wrap_text(title, width = 42)

  text(
    50, 31,
    title_wrapped,
    cex = 2.2,
    font = 2
  )


  # Speaker
  speaker_wrapped <- wrap_text(speaker, width = 50)

  text(
    50, 18,
    speaker_wrapped,
    cex = 1.5
  )


  # Footer
  segments(
    8, 6,
    92, 6,
    col = "#AAAAAA"
  )

  text(
    8, 3.5,
    "uRos 2026  |  Paris  |  18-20 November 2026",
    adj = c(0, 0.5),
    cex = 0.8,
    col = "#555555"
  )

  dev.off()
}


# ---- Validate input -------------------------------------------

cat("\nuRos 2026 Lightning Talk Builder\n")
cat("================================\n\n")


if (nrow(presentations) == 0) {
  stop("No presentations supplied.")
}


# Check files
missing_files <- presentations$file[
  !file.exists(presentations$file)
]

if (length(missing_files)) {

  stop(
    "The following files do not exist:\n",
    paste(" -", missing_files, collapse = "\n")
  )
}


# Check number of slides
for (i in seq_len(nrow(presentations))) {

  info <- pdftools::pdf_info(
    presentations$file[i]
  )

  n <- info$pages

  cat(
    sprintf(
      "%02d  %-30s %2d slides\n",
      i,
      presentations$speaker[i],
      n
    )
  )

  if (n != EXPECTED_SLIDES) {

    stop(
      "\n\nPresentation:\n",
      presentations$file[i],
      "\n\nhas ",
      n,
      " slides instead of ",
      EXPECTED_SLIDES,
      "."
    )
  }
}


# ---- Generate title slides ------------------------------------

cat("\nGenerating transition slides...\n")

title_files <- character(nrow(presentations))


for (i in seq_len(nrow(presentations))) {

  title_files[i] <- file.path(
    TEMP_DIR,
    sprintf("title_%03d.pdf", i)
  )

  create_title_slide(
    speaker  = presentations$speaker[i],
    title    = presentations$title[i],
    filename = title_files[i]
  )
}


# ---- Assemble PDFs --------------------------------------------

cat("Combining presentations...\n")


pdf_sequence <- character()


for (i in seq_len(nrow(presentations))) {

  pdf_sequence <- c(
    pdf_sequence,
    title_files[i],
    presentations$file[i]
  )
}


merged_file <- file.path(
  TEMP_DIR,
  "merged.pdf"
)


qpdf::pdf_combine(
  input  = pdf_sequence,
  output = merged_file
)


# ---- Add 20-second automatic advance --------------------------
#
# PDF supports a /Dur entry on each page.
#
# Ghostscript can add this property while rewriting the PDF.
# ---------------------------------------------------------------

cat("Adding 20-second slide timing...\n")


# Create PostScript instructions for Ghostscript

ps_file <- file.path(
  TEMP_DIR,
  "timing.ps"
)


total_pages <- nrow(presentations) * (EXPECTED_SLIDES + 1)


ps_commands <- paste0(
  "[ {Page",
  seq_len(total_pages),
  "} << /Dur ",
  SECONDS_PER_SLIDE,
  " >> /PUT pdfmark"
)


writeLines(
  ps_commands,
  ps_file
)


# Find Ghostscript

gs <- Sys.which("gs")

if (!nzchar(gs)) {

  warning(
    paste0(
      "\nGhostscript was not found.\n\n",
      "The combined PDF has been created, ",
      "but automatic slide timing has NOT been added.\n\n",
      "Install Ghostscript and rerun the script.\n",
      "macOS: brew install ghostscript\n",
      "Ubuntu: sudo apt install ghostscript\n"
    )
  )

  file.copy(
    merged_file,
    OUTPUT_FILE,
    overwrite = TRUE
  )

} else {

  # Ghostscript command
  args <- c(
    "-dBATCH",
    "-dNOPAUSE",
    "-dSAFER",
    "-sDEVICE=pdfwrite",
    "-dCompatibilityLevel=1.7",
    "-dAutoRotatePages=/None",
    paste0("-sOutputFile=", OUTPUT_FILE),
    merged_file,
    ps_file
  )

  status <- system2(
    gs,
    args = args
  )

  if (status != 0) {
    stop("Ghostscript failed to create the timed PDF.")
  }
}


# ---- Report ----------------------------------------------------

pages_per_talk <- EXPECTED_SLIDES + 1

seconds_per_talk <-
  pages_per_talk * SECONDS_PER_SLIDE

total_seconds <-
  total_pages * SECONDS_PER_SLIDE


cat("\n")
cat("================================\n")
cat("uRos 2026 PDF created\n")
cat("================================\n\n")

cat(
  sprintf(
    "Presentations:       %d\n",
    nrow(presentations)
  )
)

cat(
  sprintf(
    "Slides per speaker:  %d + 1 title slide\n",
    EXPECTED_SLIDES
  )
)

cat(
  sprintf(
    "Seconds per slide:   %d\n",
    SECONDS_PER_SLIDE
  )
)

cat(
  sprintf(
    "Time per speaker:    %d:%02d\n",
    seconds_per_talk %/% 60,
    seconds_per_talk %% 60
  )
)

cat(
  sprintf(
    "Total pages:         %d\n",
    total_pages
  )
)

cat(
  sprintf(
    "Total duration:      %d:%02d\n",
    total_seconds %/% 60,
    total_seconds %% 60
  )
)

cat(
  sprintf(
    "Output:              %s\n\n",
    normalizePath(
      OUTPUT_FILE,
      mustWork = FALSE
    )
  )
)
