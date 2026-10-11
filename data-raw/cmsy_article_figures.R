# Figures and tables of the CMSY++ articles (vignettes/articles/cmsy.Rmd and zh/cmsy.Rmd), which
# cannot run CMSY++ when the website is built (JAGS is not installed there). The example is
# bigeye grunt (Brachydeuterus auritus) in FAO area 34, the healthiest of the 30 stocks with the
# largest recent catch in the area. Run from the package root after cmsy_setup():
#   Rscript data-raw/cmsy_article_figures.R

pkgload::load_all(".", quiet = TRUE)
out <- normalizePath(file.path("vignettes", "articles", "figures", "cmsy"), mustWork = FALSE)
work <- file.path(tempdir(), "cmsy_article")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
setwd(work)
fao_settings(dpi = 90, silent = TRUE)

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
input <- cmsy_input(FAO34$area_data, table = FALSE)

# The 30 stocks with the largest catch in 2015-2024, with the start years suggested by CMSY++
recent <- stats::aggregate(ct ~ Stock, data = input$catch[input$catch$yr >= 2015, ], FUN = mean)
top <- head(recent$Stock[order(-recent$ct)], 30)
all <- cmsy_run(input, stocks = top, seed = 1, auto_start = TRUE, plots = "analysis", table = FALSE)
grunt <- cmsy_run(input, stocks = "Brachydeuterus auritus", seed = 1, auto_start = TRUE, table = FALSE,
                  plots = c("analysis", "management", "kobe", "prior_posterior", "retrospective"))
grunt_1950 <- cmsy_run(input, stocks = "Brachydeuterus auritus", seed = 1, table = FALSE, plots = "analysis",
                       out_dir = "cmsy_1950")

for (lang in c("en", "zh")) {
  dir <- file.path(out, lang)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  cmsy_plot(grunt, charts = c("analysis", "management", "prior_posterior", "retrospective"), lang = lang)
  cmsy_plot(grunt, charts = "kobe", annotations = TRUE, lang = lang)
  cmsy_plot(all, charts = c("overview", "kobe_all", "status_time", "map"), lang = lang, annotations = "legend")
  cmsy_forecast(grunt, catch = 100000, seed = 1, lang = lang, annotations = "legend")
  cmsy_compare(`1950` = grunt_1950, `CMSY++` = grunt, lang = lang, annotations = "legend")
  stock_files <- c("analysis", "management", "kobe", "prior_posterior", "retrospective", "forecast")
  summary_files <- c("overview", "kobe_all", "status_time", "map")
  file.copy(file.path("figures", "png", "cmsy", "GRB_34", paste0(stock_files, ".png")),
            file.path(dir, paste0("grunt_", stock_files, ".png")))
  file.copy(file.path("figures", "png", "cmsy", "summary", paste0(summary_files, ".png")),
            file.path(dir, paste0("area34_", summary_files, ".png")))
  file.copy(file.path("figures", "png", "cmsy", "compare", "GRB_34.png"), file.path(dir, "grunt_compare.png"))
  tables <- cmsy_tables(grunt, lang = lang, table = FALSE)
  fao_write_csv(tables$priors, file.path(dir, "priors_table.csv"))
  fao_write_csv(tables$results, file.path(dir, "results_table.csv"))
}
top_table <- all$summary[, c("Stock", "species_en", "scientific_name", "first_year", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl",
                             "F_Fmsy", "p_green", "status")]
top_table$rank <- match(top_table$Stock, top)
fao_write_csv(top_table[order(top_table$rank), ], file.path(out, "top30_area34.csv"))
print(list.files(out, recursive = TRUE))
