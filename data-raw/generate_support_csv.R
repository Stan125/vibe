# ---- Generate the .csv with supported classes, metrics and GOF measures
df <- expand.grid(
  list(
    supported_classes = c("lm", "glm", "gam", "gamlss"),
    varimp_measure = c("hp", "relweights"),
    gof_metric = c("R2e", "deviance", "R2", "log_lik", "R2Mac")
  )
)
df$implemented <- NA
write.csv(df, "inst/extdata/supported_classes_and_metrics.csv",
  row.names = FALSE
)
