install.packages("bslib")
install.packages("data.table")
packages <- c("rmarkdown", "knitr", "pagedown")
missing <- packages[
  !vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
