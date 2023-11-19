# ---- Goodness-of-fit test script

# ---- Libraries
library("mgcv")
library("gamlss")

# ---- Test goodness of fit
if (file.exists("models.RData")) {
  load("models.RData")
}
