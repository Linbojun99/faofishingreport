area <- local({
  data <- load_fao_data("bundled")
  data[data$FAO.major.fishing.area..Name. == "Atlantic, Eastern Central", ]
})

test_that("the bundled data has the package format", {
  expect_named(area, c("Country..Name.", "ASFIS.species..Name.", "FAO.major.fishing.area..Name.",
                       "Unit..Name.", "Unit", "Year", "Production", "Production.Type"))
  expect_gte(max(area$Year), 2024)
})

test_that("groups are added to the data", {
  grouped <- fao_add_groups(utils::head(area, 100))
  expect_true(all(c("isscaap_en", "taxon_level", "income_en", "inland_marine_en") %in% names(grouped)))
})

test_that("the summary analyses return their results without writing files", {
  withr::local_dir(withr::local_tempdir())
  expect_s3_class(species_group_composition(area, plot = FALSE, table = FALSE)$table, "data.frame")
  trend <- peak_trend(area, plot = FALSE, table = FALSE)
  expect_equal(trend$name[1], "Total")
  expect_true(all(trend$pct_of_peak <= 100))
  quality <- data_quality(area, plot = FALSE, table = FALSE)
  expect_named(quality, c("data_status", "taxon_level", "countries"))
  diversity <- diversity_concentration(area, plot = FALSE, table = FALSE)
  expect_true(all(diversity$hhi_countries > 0 & diversity$hhi_countries <= 10000))
  groups <- country_group_summary(area, plot = FALSE, table = FALSE)
  expect_s3_class(groups$table, "data.frame")
  fleets <- coastal_distant(area, plot = FALSE, table = FALSE)
  expect_true(all(levels(fleets$yearly$category) %in% c("Coastal countries", "Distant-water countries", "Unknown")))
  expect_length(list.files(), 0)
})

test_that("peak_trend works for all series, including short ones", {
  data <- data.frame(ASFIS.species..Name. = c(rep("A", 12), "B"), Country..Name. = "X",
                     FAO.major.fishing.area..Name. = "Area", Year = c(2001:2012, 2012),
                     Production = c(1:12, 5))
  result <- peak_trend(data, top_n = Inf, plot = FALSE, table = FALSE, lang = "en")
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$p_value[result$name_en == "B"]))
  expect_identical(result$trend[result$name_en == "B"], "none")
})

test_that("breakpoint periods start after and end at the breakpoints", {
  periods <- fao_bp_periods(c(21, 37), 1950:2024)
  expect_identical(periods$start_year, c(1950L, 1971L, 1987L))
  expect_identical(periods$end_year, c(1970L, 1986L, 2024L))
  expect_identical(fao_bp_periods(NA, 1950:2024)$end_year, 2024L)
  # years of the series, also when a year is missing
  expect_identical(fao_bp_periods(2, c(2000L, 2001L, 2003L, 2004L))$start_year, c(2000L, 2003L))
})

test_that("intermediate tables are printed only when asked", {
  withr::local_options(faofishingreport.verbose = NULL)
  expect_silent(fao_verbose_print(data.frame(a = 1)))
  expect_silent(fao_verbose_message("count"))
  withr::local_options(faofishingreport.verbose = TRUE)
  expect_output(fao_verbose_print(data.frame(a = 1)), "a")
  expect_message(fao_verbose_message("count"), "count")
})

test_that("the species and country functions print nothing and report gaps as messages", {
  withr::local_dir(withr::local_tempdir())
  data <- data.frame(Country..Name. = rep(c("Japan", "Canada"), each = 30), ASFIS.species..Name. = "Pacific cod",
                     FAO.major.fishing.area..Name. = "Pacific, Northeast",
                     Year = c(1981:2010, 1981:1995, 1997:2011), Production = c(1:30, 30:1))
  expect_output(expect_message(result <- species_country(data, "Pacific cod", rank_range = c(1, 2), plot = FALSE,
                                                          table = FALSE),
                               "Canada is not continuous"), NA)
  expect_length(result, 2)
  expect_output(country_species(data, "Japan", rank_range = c(1, 1), plot = FALSE, table = FALSE), NA)
  expect_length(list.files(), 0)
})


test_that("the main species and countries can be ranked by recent years, as in the report", {
  withr::local_dir(withr::local_tempdir())
  recent <- area[area$Year > max(area$Year) - 5, ]
  top <- function(column) names(utils::head(sort(tapply(recent$Production, recent[[column]], sum), decreasing = TRUE), 10))
  sp <- fishing_area_species(area, rank_range = c(1, 10), rank_years = 5, plot = FALSE, table = FALSE, lang = "en")
  expect_setequal(as.character(sp$area_species_ranked_total$ASFIS.species..Name.), top("ASFIS.species..Name."))
  co <- fishing_area_country(area, rank_range = c(1, 10), rank_years = 5, plot = FALSE, table = FALSE, lang = "en")
  expect_setequal(as.character(co$area_country_ranked_total$Country..Name.), top("Country..Name."))
  expect_error(fao_report(34, species_rank_years = 10), "cannot be set")
})

test_that("the data of an area keep their FAO release", {
  withr::local_dir(withr::local_tempdir())
  nwp <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE, timeseries_analysis = FALSE, data_version = "bundled")
  expect_equal(attr(nwp$area_data, "fao_version"), fao_data_info("bundled")$release)
})


test_that("regime_shift() leaves out species without catch", {
  withr::local_dir(withr::local_tempdir())
  set.seed(3)
  data <- data.frame(Country..Name. = "Morocco", ASFIS.species..Name. = rep(c("European pilchard(=Sardine)", "Atlantic cod"), each = 40),
                     FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Year = rep(1985:2024, 2),
                     Production = c(c(rep(100, 20), rep(500, 20)) + stats::rnorm(40, 0, 5), rep(0, 40)))
  out <- suppressMessages(regime_shift(data, plot = FALSE, table = FALSE, lang = "en"))
  expect_equal(out$breakpoints$species_en, "European pilchard(=Sardine)")
  expect_match(out$breakpoints$breakpoint_years, "2005")
})

test_that("the table of species_country() keeps the Other nei it ranked", {
  withr::local_dir(withr::local_tempdir())
  data <- data.frame(Country..Name. = rep(c("Morocco", "Other nei"), each = 30), ASFIS.species..Name. = "European pilchard(=Sardine)",
                     FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Year = rep(1995:2024, 2),
                     Production = rep(c(100, 1000), each = 30))
  out <- suppressMessages(species_country(data, "European pilchard(=Sardine)", rank_range = c(1, 2), plot = FALSE,
                                          table = TRUE, timeseries_analysis = FALSE, lang = "en"))
  expect_setequal(as.character(unique(out$Country..Name.)), c("Morocco", "Other nei"))
  csv <- utils::read.csv(file.path("tables", "European_pilchard__Sardine__country_ranked_table.csv"), check.names = FALSE)
  expect_setequal(csv$Country..Name., c("Morocco", "Other nei"))
})

test_that("the report text follows the groups, null model and P/D groups of its analyses", {
  ctx <- new.env()
  ctx$lang <- "en"
  ctx$area_data <- area
  ctx$area <- list(name = "Atlantic, Eastern Central")
  ctx$args <- list(groups = list(group = "major"), collapse = list(null_model = FALSE),
                   pelagic = list(pelagic = "Tunas, bonitos, billfishes"))
  expect_equal(fao_section_groups_values(ctx)$g1, "PISCES")
  ctx$results <- list(collapse = list(value = suppressMessages(collapse_analysis(area, null_model = FALSE, plot = FALSE, table = FALSE))),
                      pelagic = list(value = suppressMessages(pelagic_demersal(area, pelagic = "Tunas, bonitos, billfishes",
                                                                              plot = FALSE, table = FALSE))))
  collapse <- fao_section_collapse_values(ctx)
  expect_equal(collapse$collapse_variant, "mullon2005_nonull")
  expect_false(grepl("random", collapse$collapse_conclusion))
  expect_match(fao_section_pelagic_values(ctx)$pelagic_methods, "pelagic, Tunas, bonitos, billfishes; demersal")
})


test_that("the Mann-Kendall test is corrected for autocorrelation, and peak_trend() adjusts for many series", {
  set.seed(11)
  years <- 1:30
  mk <- fao_mann_kendall(years, 100 + 2 * years + stats::rnorm(30))
  expect_lt(mk[["p"]], 0.001)
  expect_gt(mk[["tau"]], 0.8)
  # a trendless but strongly autocorrelated series: the corrected p value is larger
  walk <- as.numeric(stats::arima.sim(list(ar = 0.9), 30)) + 100
  mk <- fao_mann_kendall(years, walk)
  expect_gte(mk[["p"]], mk[["p_iid"]])
  expect_true(all(is.na(fao_mann_kendall(1:2, c(1, 2)))))
  result <- peak_trend(area, top_n = 10, plot = FALSE, table = FALSE, lang = "en")
  expect_true(all(c("p_value", "p_value_iid", "q_value") %in% names(result)))
  expect_true(is.na(result$q_value[1]))
  expect_true(all(result$q_value[-1] >= result$p_value[-1] - 1e-12, na.rm = TRUE))
})

test_that("decade means divide by the years of the data, whatever items are chosen", {
  # B: 70 t a year, 0.07 thousand t (not on a rounding boundary, which differs between platforms)
  data <- data.frame(Year = c(2009, 2000:2009), Production = c(1000, rep(70, 10)),
                     ASFIS.species..Name. = c("A", rep("B", 10)), stringsAsFactors = FALSE)
  one <- list(species_table = data.frame(name_en = "A"), species_names = list(plain = "A"))
  two <- list(species_table = data.frame(name_en = c("A", "B")), species_names = list(plain = c("A", "B")))
  expect_equal(fao_report_decade_table(data, one, "species", "en")[[2]], "0.1")       # 1000 t / 10 years, in 1000 t
  expect_equal(fao_report_decade_table(data, two, "species", "en")[[2]], c("0.1", "0.1"))
})

test_that("variance contributions add up to 100% and see through compensating series", {
  set.seed(5)
  years <- 1990:2019
  swing <- stats::rnorm(30, 0, 200)
  small <- stats::rnorm(30, 0, 10)
  data <- data.frame(Country..Name. = "Chile", FAO.major.fishing.area..Name. = "Pacific, Southeast", Year = rep(years, 3),
                     ASFIS.species..Name. = rep(c("Anchoveta(=Peruvian anchovy)", "Chilean jack mackerel", "Pacific sardine"), each = 30),
                     Production = c(1000 + swing, 1000 - swing, 500 + small), stringsAsFactors = FALSE)
  out <- suppressMessages(catch_synchrony(data, detrend = "none", plot = FALSE, table = FALSE))
  s <- out$series
  expect_equal(sum(s$variance_contribution_pct), 100)
  expect_equal(s$variance_contribution_pct[s$series_en == "Pacific sardine"], 100)
  expect_gt(min(s$sd_share_pct[s$series_en != "Pacific sardine"]), 40)    # the SD share misleads
})

test_that("the report says what the portfolio effect and the MTC slope are", {
  ctx <- new.env()
  ctx$lang <- "en"
  ctx$args <- list()
  ctx$results <- list(synchrony = list(value = list(
    summary = data.frame(n_series = 2, synchrony_phi = 1, cv_total = 0.3, mean_cv_series = 0.2, portfolio_effect_pct = -44,
                         mean_pairwise_correlation = 1, positive_correlations_pct = 100),
    series = data.frame(series_en = c("Atlantic cod", "Atlantic herring"), variance_contribution_pct = c(60, 40)),
    windows = NULL)))
  sync <- fao_section_synchrony_values(ctx)
  expect_match(sync$sync_effect, "more variable")
  expect_false(grepl("stable", sync$sync_effect))
  ctx$results$mtc <- list(value = list(
    trend = data.frame(period_start = 1970, period_end = 2024, mtc_slope_per_decade = -0.0178, trend = "none", kendall_p = 0.5,
                       sst_slope_per_decade = 0.1, cor_mtc_sst = 0.1),
    yearly = data.frame(year = 2024, mtc = 20, catch_used_tonnes = 1, catch_tonnes = 1),
    species = data.frame(species_en = "Atlantic cod", used = TRUE, weight_pct = 100, temp_pref_mean = 8)))
  expect_match(fao_section_mtc_values(ctx)$mtc_change, "-0.02")
})
