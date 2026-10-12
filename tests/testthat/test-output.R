area27 <- local({
  data <- load_fao_data("bundled")
  data[data$FAO.major.fishing.area..Name. == "Atlantic, Northeast", ]
})

test_that("rank_range lists the ranks to show", {
  expect_equal(fao_rank_select(1:10), 1:10)
  expect_equal(fao_rank_select(c(6, 5, 5)), c(5L, 6L))
  expect_equal(fao_rank_select(3), 3L)
  for (wrong in list(0, 1.5, NA, "1", numeric(0), Inf, c(-1, 2))) {
    expect_error(fao_rank_select(wrong), "whole numbers of at least 1")
  }
  # two ranks that are not neighbours: once per session, a note on what changed in 1.0.0
  .fao_cache$rank_note <- NULL
  withr::defer(.fao_cache$rank_note <- NULL)
  expect_message(fao_rank_select(c(1, 10)), "c\\(1, 10\\) shows ranks 1 and 10. Use 1:10")
  expect_no_message(fao_rank_select(c(1, 10)))
  .fao_cache$rank_note <- NULL
  expect_no_message(fao_rank_select(c(5, 6)))
  expect_no_message(fao_rank_select(1:10))
  expect_equal(fao_ranks_text(1:10), "1 to 10")
  expect_equal(fao_ranks_text(c(5, 9)), "5, 9")
})

test_that("ranks beyond the data are left out, and none at all is an error", {
  data <- data.frame(name = c("a", "b", "c"), tonnes = 3:1)
  expect_equal(fao_rank_slice(data, c(2L, 5L))$name, "b")
  expect_error(fao_rank_slice(data, 4:5, "species"), "nothing at the ranks in `rank_range` \\(4, 5\\): the data have 3 species")
})

test_that("the analyses keep only the ranks asked for", {
  withr::local_dir(withr::local_tempdir())
  total <- sort(tapply(area27$Production, area27$ASFIS.species..Name., sum), decreasing = TRUE)
  sp <- suppressMessages(fishing_area_species(area27, rank_range = c(1, 3), plot = FALSE, table = FALSE,
                                              timeseries_analysis = FALSE, lang = "en"))
  expect_equal(as.character(sp$area_species_ranked_total$ASFIS.species..Name.), names(total)[c(1, 3)])
  expect_setequal(as.character(unique(sp$area_species_ranked$ASFIS.species..Name.)), names(total)[c(1, 3)])
  sp56 <- fishing_area_species(area27, rank_range = 5:6, plot = FALSE, table = FALSE, timeseries_analysis = FALSE, lang = "en")
  expect_equal(as.character(sp56$area_species_ranked_total$ASFIS.species..Name.), names(total)[5:6])
  expect_error(fishing_area_species(area27, rank_range = 0, plot = FALSE, table = FALSE), "whole numbers")
  expect_equal(formals(fishing_area_species)$rank_range, quote(1:10))
  for (fn in c("fishing_area_country", "species_country", "country_species", "country_analysis")) {
    expect_equal(formals(get(fn))$rank_range, quote(1:10), info = fn)
  }
})

test_that("an analysis shows a summary of its results when it finishes", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.show_data = TRUE, faofishingreport.dpi = 72)
  output <- capture.output(result <- diversity_concentration(area27))
  text <- paste(output, collapse = "\n")
  expect_match(text, "diversity_concentration\\(\\): Atlantic, Northeast")
  expect_match(text, "Charts saved in figures/png/summary/: Atlantic__Northeast_diversity.png")
  expect_match(text, "Tables saved in tables/: Atlantic__Northeast_diversity.csv")
  expect_match(text, "earlier rows")
  expect_s3_class(result, "data.frame")
  # the result is returned invisibly, so a call without assignment prints only the summary
  capture.output(visible <- withVisible(diversity_concentration(area27, plot = FALSE, table = FALSE))$visible)
  expect_false(visible)

  ranked <- capture.output(suppressMessages(fishing_area_species(area27, rank_range = c(5, 6), plot = FALSE,
                                                                 table = FALSE, timeseries_analysis = FALSE)))
  expect_true(any(grepl("Ranks 5 to 6 of", ranked)))
  expect_true(any(grepl("^ +5 ", ranked)) && any(grepl("^ +6 ", ranked)))
  expect_true(any(grepl("Results returned: \\$area_species", ranked)))

  withr::local_options(faofishingreport.show_data = FALSE)
  expect_output(diversity_concentration(area27, plot = FALSE, table = FALSE), NA)
})

test_that("the functions called by another analysis or by the report stay quiet", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.show_data = TRUE)
  outer <- function() {
    run <- fao_run_begin()
    on.exit(fao_run_end(run), add = TRUE)
    diversity_concentration(area27, plot = FALSE, table = FALSE)
    fao_run_result("done", run, "outer()")
  }
  output <- capture.output(outer())
  expect_false(any(grepl("diversity_concentration", output)))
  expect_true(any(grepl("outer\\(\\)", output)))
  expect_equal(.fao_cache$depth, 0)
  # an error inside an analysis does not leave the next call quiet
  expect_error(fishing_area_species(area27, rank_range = 2000, plot = FALSE, table = FALSE), "nothing at the ranks")
  expect_equal(.fao_cache$depth, 0)
})

test_that("plot_show draws each chart as it is saved, and only then", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.dpi = 72)
  shown <- 0
  local_mocked_bindings(fao_show_plot = function(plot) shown <<- shown + 1)
  diversity_concentration(area27, table = FALSE)
  expect_equal(shown, 0)
  diversity_concentration(area27, table = FALSE, plot_show = TRUE)
  expect_equal(shown, 1)
  expect_false(isTRUE(getOption("faofishingreport.plot_show")))
  withr::local_options(faofishingreport.plot_show = TRUE)
  diversity_concentration(area27, table = FALSE)
  expect_equal(shown, 2)
  diversity_concentration(area27, table = FALSE, plot_show = FALSE)
  expect_equal(shown, 2)
  expect_error(diversity_concentration(area27, plot_show = "yes"), "`plot_show` must be TRUE or FALSE")
  # every analysis that draws charts has the argument
  for (fn in c("fishing_area_data", "fishing_area_species", "fishing_area_country", "species_country",
               "country_species", "country_analysis", "stock_status", "peak_trend", "area_overview",
               "species_group_composition", "data_quality", "diversity_concentration", "country_group_summary",
               "regime_shift", "coastal_distant", "catch_shocks", "collapse_analysis", "catch_synchrony",
               "pelagic_demersal", "distant_water_profile", "catch_trade", "employment_summary",
               "catch_per_fisher", "compare_releases", "mean_temperature_catch", "fao_areas", "fishing_area_map",
               "cmsy_area", "cmsy_plot", "cmsy_mcmc", "cmsy_forecast", "cmsy_compare")) {
    expect_true("plot_show" %in% names(formals(get(fn))), info = fn)
  }
})

test_that("tables in the console keep years plain, separate thousands and align Chinese names", {
  x <- data.frame(Year = c(2023L, 2024L), country = c("挪威", "Iceland"), tonnes = c(1234567, 89),
                  share = c(0.12345, 12.5), stringsAsFactors = FALSE)
  shown <- fao_display_table(x)
  expect_equal(shown$Year, c("2023", "2024"))
  expect_equal(shown$tonnes, c("1,234,567", "89"))
  expect_equal(shown$share, c("0.123", "12.5"))
  expect_equal(length(unique(nchar(shown$country, type = "width"))), 1)
})

test_that("show_data and plot_show are settings", {
  withr::local_options(faofishingreport.show_data = NULL, faofishingreport.plot_show = NULL)
  expect_true(fao_setting("show_data"))
  expect_false(fao_setting("plot_show"))
  fao_settings(show_data = FALSE, plot_show = TRUE)
  expect_false(getOption("faofishingreport.show_data"))
  expect_true(getOption("faofishingreport.plot_show"))
  expect_error(fao_settings(show_data = "no"), "show_data must be TRUE or FALSE")
  expect_error(fao_settings(plot_show = NA), "plot_show must be TRUE or FALSE")
})
