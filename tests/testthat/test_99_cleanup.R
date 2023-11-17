# ---- Cleanup script
if (file.exists("vibe_plots.pdf")) {
  file.remove("vibe_plots.pdf")
}
if (file.remove("models.RData")) {
  file.remove("models.RData")
}
