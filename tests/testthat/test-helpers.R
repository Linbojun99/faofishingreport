test_that("year axes end at the last year", {
  expect_equal(tail(fao_year_breaks(1950:2024), 3), c(2015, 2020, 2024))
  expect_equal(tail(fao_year_breaks(1950:2021), 2), c(2015, 2021))
  expect_equal(tail(fao_year_breaks(1950:2020), 1), 2020)
  expect_equal(fao_year_breaks(1950:2024, by = 20), c(1950, 1970, 1990, 2010, 2024))
})

test_that("decades and pie chart periods", {
  expect_equal(fao_decade(c(1950, 1959, 2024)), c("1950s", "1950s", "2020s"))
  expect_equal(fao_pie_period(c(2015, 2020, 2024), 2024), c("2010s", "2020", "2024"))
})

test_that("taxonomic levels come from the FAO taxonomic code", {
  expect_equal(fao_taxon_level(c("121009102201", "1210101110XX", "1020010XXXXX", "1990XXXXXXXX110")),
               c("species", "genus", "family", "higher"))
})

test_that("file names are safe", {
  expect_equal(fao_file_name("European pilchard(=Sardine)"), "European_pilchard__Sardine_")
})

test_that("fao_data_info describes the bundled release", {
  info <- fao_data_info(data_dir = withr::local_tempdir())
  expect_s3_class(info, "fao_data_info")
  expect_identical(info$source, "bundled")
  expect_identical(info$release, fao_bundled_release())
  expect_match(info$url, paste0("Capture_", info$release, "\\.zip$"))
  expect_length(info$years, 2)
  expect_match(info$citation, paste0("release ", info$release))
  expect_output(print(info), "Global Capture Production")
  expect_error(load_fao_data("1999.1.0", data_dir = withr::local_tempdir()), "has not been downloaded")
})
