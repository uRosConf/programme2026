abstracts <- fread("submissions_uRos.csv")
colnames(abstracts) <- c(colnames(abstracts)[-1], "V1")
abstracts <- abstracts[
  STATUT == "Accepted" & TYPDOC != "EMOS session",
  .(SPEAKERS, TYPDOC, TITLE, ABSTRACT)
]
abstracts[, SPEAKERS := gsub("<[^>]+@[^>]+>", "", SPEAKERS)]
fwrite(abstracts, file = "abstracts.csv")
