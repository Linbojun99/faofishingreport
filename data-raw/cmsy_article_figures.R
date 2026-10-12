# Figures and tables of the CMSY++ articles (vignettes/articles/cmsy.Rmd and zh/cmsy.Rmd), which
# cannot run CMSY++ when the website is built (JAGS is not installed there). The examples are the
# 30 stocks with the largest catch in 2015-2024 in FAO area 27 (Northeast Atlantic) and two of them
# shown in detail: the healthiest (highest probability of the green Kobe quadrant in its last year)
# and the most clearly overfished (highest probability of B/Bmsy below 0.8). Run from the package
# root after cmsy_setup():
#   Rscript data-raw/cmsy_article_figures.R

pkgload::load_all(".", quiet = TRUE)
out <- file.path("vignettes", "articles", "figures", "cmsy")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)
work <- file.path(tempdir(), "cmsy_article")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
setwd(work)
fao_settings(dpi = 90, silent = TRUE)

FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
input <- cmsy_input(FAO27$area_data, table = FALSE)

# The 30 stocks with the largest catch in 2015-2024, with the start years suggested by CMSY++
recent <- stats::aggregate(ct ~ Stock, data = input$catch[input$catch$yr >= 2015, ], FUN = mean)
top <- head(recent$Stock[order(-recent$ct)], 30)
all <- cmsy_run(input, stocks = top, seed = 1, auto_start = TRUE, plots = "analysis", table = FALSE)

# The two examples: the stock most likely in the green Kobe quadrant (B >= Bmsy and F <= Fmsy) and
# the stock most likely overfished (B/Bmsy < 0.8); ties go to the larger catch
ok <- all$summary[cmsy_has_results(all$summary$run_status), ]
ok <- ok[order(match(ok$Stock, top)), ]
examples <- c(green = ok$Stock[which.max(ok$p_green)], overfished = ok$Stock[which.max(ok$p_overfished)])
message("Examples: ", paste(names(examples), examples, collapse = ", "))

values <- list()
for (kind in names(examples)) {
  stock <- examples[[kind]]
  detail <- cmsy_run(input, stocks = stock, seed = 1, auto_start = TRUE, table = FALSE, out_dir = paste0("cmsy_", kind),
                     plots = c("analysis", "management", "kobe", "prior_posterior", "retrospective"))
  detail_1950 <- cmsy_run(input, stocks = stock, seed = 1, table = FALSE, plots = "analysis",
                          out_dir = paste0("cmsy_1950_", kind))
  forecast_catch <- signif(detail$summary$MSY, 1)   # MSY of the summary is in tonnes
  for (lang in c("en", "zh")) {
    dir <- file.path(out, lang)
    if (kind == "green") {
      unlink(dir, recursive = TRUE)
      dir.create(dir, recursive = TRUE)
    }
    cmsy_plot(detail, charts = c("analysis", "management", "prior_posterior", "retrospective"), lang = lang)
    cmsy_plot(detail, charts = "kobe", annotations = TRUE, lang = lang)
    cmsy_forecast(detail, catch = forecast_catch, seed = 1, lang = lang, annotations = "legend")
    cmsy_compare(`1950` = detail_1950, `CMSY++` = detail, lang = lang, annotations = "legend")
    stock_files <- c("analysis", "management", "kobe", "prior_posterior", "retrospective", "forecast")
    file.copy(file.path("figures", "png", "cmsy", stock, paste0(stock_files, ".png")),
              file.path(dir, paste0(kind, "_", stock_files, ".png")), overwrite = TRUE)
    file.copy(file.path("figures", "png", "cmsy", "compare", paste0(stock, ".png")),
              file.path(dir, paste0(kind, "_compare.png")), overwrite = TRUE)
    tables <- cmsy_tables(detail, lang = lang, table = FALSE)
    fao_write_csv(tables$priors, file.path(dir, paste0(kind, "_priors_table.csv")))
    fao_write_csv(tables$results, file.path(dir, paste0(kind, "_results_table.csv")))
    if (kind == "green") {
      cmsy_plot(all, charts = c("overview", "kobe_all", "status_time", "map"), lang = lang, annotations = "legend")
      summary_files <- c("overview", "kobe_all", "status_time", "map")
      file.copy(file.path("figures", "png", "cmsy", "summary", paste0(summary_files, ".png")),
                file.path(dir, paste0("area27_", summary_files, ".png")), overwrite = TRUE)
    }
  }
  s <- detail$summary
  values[[kind]] <- data.frame(
    example = kind, Stock = stock, species_en = s$species_en, scientific_name = s$scientific_name,
    rank = match(stock, top), first_year = s$first_year, B_Bmsy = s$B_Bmsy, B_Bmsy_lcl = s$B_Bmsy_lcl,
    B_Bmsy_ucl = s$B_Bmsy_ucl, F_Fmsy = s$F_Fmsy, p_green = s$p_green, p_red = s$p_red,
    p_overfished = s$p_overfished, MSY_t = s$MSY, forecast_catch_t = forecast_catch,
    B_Bmsy_1950 = detail_1950$summary$B_Bmsy, first_year_1950 = detail_1950$summary$first_year
  )
}
top_table <- all$summary[, c("Stock", "species_en", "scientific_name", "first_year", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl",
                             "F_Fmsy", "p_green", "status")]
top_table$rank <- match(top_table$Stock, top)
fao_write_csv(top_table[order(top_table$rank), ], file.path(out, "top30_area27.csv"))
# The numbers quoted in the text of the articles
fao_write_csv(do.call(rbind, values), file.path(out, "example_values.csv"))
unlink(file.path(out, "top30_area34.csv"))
print(do.call(rbind, values))
print(list.files(out, recursive = TRUE))
