### This is the list of supported classes ###
supported_classes <- c("lm", "glm", "gam", "gamlss")
varimp_measure <- c("hier.part", "relweights")
gof_metric <- c("R2e", "deviance", "R2", "log_lik")
df <- expand.grid(supported_classes, varimp_measure, gof_metric)

save(list = "supported_classes", file = "R/sysdata.rda")
