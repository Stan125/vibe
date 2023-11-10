# ---- Read supported_classes_and_metrics.csv and convert into data object
scam <- read.table(
  "inst/extdata/supported_classes_and_metrics.csv",
  sep = ",",
  row.names = NULL,
  header = TRUE,
  stringsAsFactors = TRUE
)
save(list = "scam", file = "data/scam.rda")
