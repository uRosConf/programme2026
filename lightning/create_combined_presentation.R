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

packages <- c("pdftools", "qpdf", "jpeg")

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing)) {
  install.packages(missing)
}


# ---- Configuration -------------------------------------------

SECONDS_PER_SLIDE <- 20
EXPECTED_SLIDES   <- 15
LOGO_FILE         <- "uros2026_logo.jpg"

if (!file.exists(LOGO_FILE)) {
  stop("Logo file does not exist: ", LOGO_FILE)
}
uros_logo <- jpeg::readJPEG(LOGO_FILE)



TEMP_DIR <- tempfile("uros2026_")
dir.create(TEMP_DIR)


# B2 ---- Programme -----------------------------------------------
OUTPUT_FILE <- "B2_00_uRos2026_lightning_talks.pdf"
presentations <- data.frame(
  speaker = c(
    "Patrice Kiener",
    "Daniel Rakotomalala",
    "Alice Mainguené",
    "Tanguy Barthelemy",
    "Zineb Aoulad Hammou Achou, Zakariae Halhal",
    "Kyrylo Chalyy",
    "Yves Coltier",
    "Gorka Campandegui García",
    "Alexander Kowarik",
    "Michael Levi-Valensin",
    "Reijer Idema",
    "Simona Cafieri"

  ),
  title = c(
    "How many R packages in CRAN do you know?",
    "Template and Function: A Tabular Configuration Pattern for Large Scale Operations on Files with Varying Schemas in Official Statistics",
    "R you useful for maintaining information systems?",
    "{IssueTrackeR}: Handling Tickets in R",
    "From Data to Publication : An Automated R-Based Framework for Consumer Price Index Reporting",
    "Facilitating R-Based Statistical Validation Through EZR: A Case Study in Melanoma High-Frequency Ultrasound",
    "Manual Rework Volume Monitoring and Forecasting Tool",
    "Semi-automated editing of large volumes of foreign trade data for the environmental Economy-Wide Material Flow Account",
    "VIM::vimpute: A Unified Imputation Engine for Flexible Missing Data Workflows in R",
    "Sampling Designs and Sample Quality Control: Using Quarto Documents in Agricultural Surveys",
    "RISQ: Representativity Indicators for Survey Quality",
    "SmartSurveyAgent: A New Approach to Survey Data Collection in Official Statistics"
  ),
  file = c(
    'lightning/B2/B2_01_2026-1119-uRos2026-B2-PatriceKiener-RWsearch.pdf',
    'lightning/B2/B2_02_uros2026_B2_Rakotomalala_Final.pdf',
    'lightning/B2/B2_03_uRos_nov2026_b2_amainguene.pdf',
    'lightning/B2/B2_04_Session_Tanguy_Barthelemy_IssueTrackeR.pdf',
    'lightning/B2/B2_05_uros2026_B2_CPIFramework.pdf',
    'lightning/B2/B2_06_uRos2026_EZR_Chalyy.pdf',
    'lightning/B2/B2_07_-Manual Rework Volume Monitoring and Forecasting Tool.pdf',
    'lightning/B2/B2_08_Gorka Campandegui Garcia - Semi-automated.pdf',
    'lightning/B2/B2_09_vimpute_kowarik.pdf',
    'lightning/B2/B2_10_SamplingDesignsandSampleQualityControl .pdf',
    'lightning/B2/B2_11_uros2026_presentation_idema.pdf',
    'lightning/B2/B2_12_SmartSurveyAgent_cafieri_B2.pdf'
  ),
  stringsAsFactors = FALSE
)

# B5 ---- Programme -----------------------------------------------
OUTPUT_FILE <- "B5_00_uRos2026_lightning_talks.pdf"
presentations <- data.frame(
  speaker = c(
    "Pierre Lamarche",
    "Andrés Christiansen",
    "Ilya Kashnitsky",
    "Marco Caputi",
    "Romuald Thion",
    "Guillaume Osier",
    "Andreea-Denisa Trascan",
    "Thomas Guinhut, Eulalie Delaune",
    "Gianina-Maria Petrașcu",
    "Cristina Boboc",
    "Ana-Maria Ciuhu"

  ),
  title = c(
    "Common pitfalls when switching from SAS to R",
    "ILSAmerge and ILSAstats: Two new R packages for international large-scale assessments",
    "{loomer} - A high-performance auxiliary R package for analyzing the Danish Lifelines dataset",
    "Further microdata anonymisation tools in R",
    "Automatic classification of pastoral-use parcels / Classification automatique des parcelles à usage pastoral",
    "Use of R Packages for Cartography: The Case of Road Traffic Accident Statistics in Luxembourg",
    "Mapping the Gendered Impact of Economic Shocks: A Spatial Analysis of Mancession vs. Shecession across European Regions",
    "Estimating French election results like the pollsters from partial vote counts on election night: a transparent, reproducible R approach based on open public data",
    "The Effects of Skills on Gender Wage Inequality in the Digital Economy",
    "Why Do Some Counties Perform Better? Educational Hubs, High-Tech Development and Skills Mismatch in Romania",
    "AI-Assisted Scientific Literature Review with LitRefineR Using OpenAlex"
  ),
  file = c(
    'lightning/B5/B5_01_Lamarche.pdf',
    'lightning/B5/B5_02_ILSAmerge and ILSAstats - uRos 2026 v1.pdf',
    'lightning/B5/B5_03-Kashnitsky-slides-uros-ikx.pdf',
    'lightning/B5/B5_04_Marco_Caputi_uRos2026.pdf',
    'lightning/B5/B5_05_THION_automatic-classification-parcels.pdf',
    'lightning/B5/B5_06_STATECcartography.pdf',
    'lightning/B5/B5_07_Trascan_Dan_uRos2026.pdf',
    'lightning/B5/B5_08_Estimating_French_election_results_like_the_pollsters.pdf',
    'lightning/B5/B5_09_Petrascu_Trascan_uRos2026.pdf',
    'lightning/B5/B5_10_Boboc_Petrascu_uRos2026.pdf',
    'lightning/B5/B5_11_CiuhuAnaMaria.pdf'
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

  # Fit the logo into the top-right area without stretching it.
  logo_aspect <- dim(uros_logo)[2] / dim(uros_logo)[1]
  logo_width <- min(22, 10 * logo_aspect)
  logo_height <- logo_width / logo_aspect

  rasterImage(
    uros_logo,
    94 - logo_width, 52.5 - logo_height,
    94, 52.5,
    interpolate = TRUE
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

  # Preserve source image resolution and avoid adding JPEG artefacts when
  # Ghostscript rewrites the PDF to add timing. Pass existing JPEG/JPEG2000
  # streams through where possible; use lossless Flate for re-encoded images.
  # This may produce a larger PDF, but cannot restore detail absent in inputs.
  args <- c(
    "-dBATCH",
    "-dNOPAUSE",
    "-dSAFER",
    "-sDEVICE=pdfwrite",
    "-dCompatibilityLevel=1.7",
    "-dAutoRotatePages=/None",
    "-dDownsampleColorImages=false",
    "-dDownsampleGrayImages=false",
    "-dDownsampleMonoImages=false",
    "-dAutoFilterColorImages=false",
    "-dAutoFilterGrayImages=false",
    "-dColorImageFilter=/FlateEncode",
    "-dGrayImageFilter=/FlateEncode",
    "-dPassThroughJPEGImages=true",
    "-dPassThroughJPXImages=true",
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
