# ---- Cleanup script
if (file.exists("vibe_plots.pdf")) {
  file.remove("vibe_plots.pdf")
}
if (file.exists("models.RData")) {
  file.remove("models.RData")
}
