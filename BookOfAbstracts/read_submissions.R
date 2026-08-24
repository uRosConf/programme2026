library(data.table)

clean_people <- function(x) {
  x <- gsub("<[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}>", "", x)
  x <- gsub("\\x{00A0}", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("[[:space:]]+,", ",", x)
  x <- gsub(",[[:space:]]*", ", ", x)
  trimws(x)
}

abstracts <- fread("submissions_uRos.csv")
colnames(abstracts) <- c(colnames(abstracts)[-1], "V1")
abstracts <- abstracts[
  STATUT == "Accepted" & TYPDOC != "EMOS session",
  .(SPEAKERS, Authors = AUTHORS, TYPDOC, TITLE, ABSTRACT)
]
abstracts[, SPEAKERS := clean_people(SPEAKERS)]
abstracts[, Authors := clean_people(Authors)]
fwrite(abstracts, file = "abstracts.csv")
