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
