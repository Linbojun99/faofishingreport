# Figures of the trade and employment articles (vignettes/articles/trade-employment.Rmd and
# zh/trade-employment.Rmd), made beforehand so that the website does not download the FAO trade
# data (16 MB) when it is built. Run from the package root (online, or with the trade and
# employment data already in fao_data_dir()):
#   Rscript data-raw/trade_employment_article_figures.R

pkgload::load_all(".", quiet = TRUE)
out <- file.path(normalizePath("."), "vignettes", "articles", "figures", "trade-employment")
work <- file.path(tempdir(), "trade_employment_article")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
setwd(work)
fao_settings(dpi = 90)

trade <- load_fao_trade()
employment <- load_fao_employment()
data <- load_fao_data()

for (lang in c("en", "zh")) {
  dir <- file.path(out, lang)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  unlink("figures", recursive = TRUE)
  catch_trade(c("Norway", "Iceland", "Peru", "Thailand"), data = data, trade = trade, label = "four", lang = lang,
              table = FALSE)
  catch_trade("Norway", data = data, trade = trade, lang = lang, table = FALSE)
  employment_summary(employment = employment, lang = lang, table = FALSE)
  employment_summary(c("Norway", "Peru", "China"), employment = employment, label = "three", lang = lang, table = FALSE)
  catch_per_fisher(c("Norway", "Iceland", "Peru", "Indonesia", "China", "India"), data = data, employment = employment,
                   label = "six", lang = lang, table = FALSE)
  keep <- c("four_catch_trade.png", "Norway_catch_trade_groups.png", "World_employment.png",
            "three_employment_composition.png", "six_catch_per_fisher.png")
  file.copy(file.path("figures", "png", "summary", keep), dir, overwrite = TRUE)
}

# Numbers quoted in the articles
four <- catch_trade(c("Norway", "Iceland", "Peru", "Thailand"), data = data, trade = trade, plot = FALSE, table = FALSE)
print(four$yearly[four$yearly$year == max(four$yearly$year),
                  c("country_en", "catch_tonnes", "export_tonnes", "import_tonnes", "export_catch_pct",
                    "counted_catch_pct", "counted_export_value_pct", "meal_oil_export_tonnes")])
