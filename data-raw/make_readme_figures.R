# Makes the example charts shown in README.md (man/figures/readme-*.png).
# Run from the package root after installing the package:  Rscript data-raw/make_readme_figures.R

library(faofishingreport)
fao_settings(reset = TRUE)
fao_settings(dpi = 90)

package_root <- getwd()
work_dir <- tempfile("readme_figures_")
dir.create(work_dir)
setwd(work_dir)

quiet <- function(expr) invisible(utils::capture.output(suppressMessages(suppressWarnings(expr))))
copy_figure <- function(from, to) {
  file.copy(file.path(work_dir, "figures", "png", from), file.path(package_root, "man", "figures", to), overwrite = TRUE)
}

quiet(FAO34 <- fishing_area_data("Atlantic, Eastern Central"))
copy_figure("fishing_trend.png", "readme-area-trend.png")

quiet(stock_status(FAO34$area_data))
copy_figure("summary/Atlantic__Eastern_Central_stock_status_catch.png", "readme-stock-status.png")

quiet(species_country(FAO34$area_data, "European pilchard(=Sardine)", lang = "both"))
copy_figure("species/European_pilchard__Sardine__country_ranked_wrap.png", "readme-bilingual.png")

fao_settings(map_projection = "robinson", map_center = 180, map_palette = "YlOrRd")
quiet(country_analysis("Morocco"))
copy_figure("country/Moroccofishing_area_map.png", "readme-area-map.png")
fao_settings(reset = TRUE)
fao_settings(dpi = 90)

# Mean temperature of the catch, with the FishBase traits bundled with the package
quiet(mean_temperature_catch(FAO34$area_data, version = "builtin", data_dir = work_dir))
copy_figure("summary/Atlantic__Eastern_Central_mtc.png", "readme-mtc.png")

# CMSY++ Kobe plot of the ten main species (needs JAGS: cmsy_setup())
quiet(cmsy <- cmsy_area(FAO34$area_data, top = 10, lang = "en", out_dir = file.path(work_dir, "cmsy"),
                        charts = "kobe_all", forecast = FALSE, silent = TRUE))
file.copy(file.path(cmsy$dir, "figures", "summary", "kobe_all.png"),
          file.path(package_root, "man", "figures", "readme-cmsy-kobe.png"), overwrite = TRUE)

setwd(package_root)
message("README figures updated in man/figures/")
