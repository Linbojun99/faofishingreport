# Figures and tables of the CMSY++ step of the tutorial on FAO area 61
# (vignettes/articles/tutorial-61.Rmd and zh/tutorial-61.Rmd), which cannot run CMSY++ when the
# website is built (JAGS is not installed there). Run from the package root after cmsy_setup():
#   Rscript data-raw/tutorial61_figures.R

args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args) > 1) args[2] else ".", quiet = TRUE)
out <- if (length(args) > 0) args[1] else file.path("vignettes", "articles", "figures", "tutorial-61")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)
work <- file.path(tempdir(), "tutorial61")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
setwd(work)
fao_settings(dpi = 90, silent = TRUE)

# The ten species with the largest recent catch of area 61, as in the tutorial
nwp <- cmsy_area("Pacific, Northwest", top = 10, seed = 1, forecast = FALSE, out_dir = "cmsy_areas",
                 charts = "kobe_all", lang = "en")
first <- nwp$summary$Stock[1]
for (lang in c("en", "zh")) {
  dir <- file.path(out, lang)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  cmsy_plot(nwp, charts = c("kobe_all", "overview", "status_time"), lang = lang, annotations = "legend")
  cmsy_plot(nwp, stocks = first, charts = "management", lang = lang)
  file.copy(file.path("figures", "png", "cmsy", "summary", c("kobe_all.png", "overview.png", "status_time.png")), dir)
  file.copy(file.path("figures", "png", "cmsy", first, "management.png"), file.path(dir, "management_first.png"))
  s <- nwp$summary
  table <- data.frame(Stock = s$Stock, species = fao_translate(s$species_en, "species", lang),
                      B_Bmsy = round(s$B_Bmsy, 2), F_Fmsy = round(s$F_Fmsy, 2),
                      MSY = round(s$MSY), status = s$status, stringsAsFactors = FALSE)
  if (lang == "zh") {
    table$status <- c("maximally sustainably fished" = "\u6700\u5927\u53ef\u6301\u7eed\u6355\u635e",
                      overfished = "\u8fc7\u5ea6\u6355\u635e")[table$status]
  }
  fao_write_csv(table, file.path(dir, "cmsy_summary.csv"))
}
print(list.files(out, recursive = TRUE))
