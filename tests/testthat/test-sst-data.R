# Sea surface temperature products and area means, tested offline

test_that("the products are described, and ERSST v5 is the bundled default", {
  products <- fao_sst_products()
  expect_equal(products$product, c("ersst5", "hadisst1", "cobe2", "oisst21"))
  expect_false(any(is.na(products[, c("name", "grid", "data_sources", "licence", "citation", "doi")])))
  expect_equal(products$product[products$bundled], "ersst5")
  expect_error(fao_area_sst(product = "ersst4"), "Unknown SST product")
})

test_that("the bundled SST table has every marine area, year and range", {
  withr::local_options(faofishingreport.data_dir = withr::local_tempdir())
  sst <- fao_area_sst()
  expect_named(sst, c("product", "area_code", "area_en", "year", "sst_mean", "sst_p10", "sst_p90",
                      "sst_min_month", "sst_max_month", "n_cells", "source", "downloaded"))
  expect_equal(length(unique(sst$area_code)), 19)
  for (code in unique(sst$area_code)) {
    years <- sst$year[sst$area_code == code]
    expect_equal(years, seq(1950, max(years)))
  }
  expect_true(all(sst$sst_mean > -2 & sst$sst_mean < 32))
  expect_true(all(sst$sst_p10 <= sst$sst_p90))
  expect_true(all(sst$sst_min_month <= sst$sst_mean & sst$sst_mean <= sst$sst_max_month))
  expect_equal(fao_area_sst("Atlantic, Eastern Central"), fao_area_sst(34))
  expect_error(fao_area_sst("99"), "Unknown FAO major marine fishing area")
})

test_that("a product that has not been downloaded gives a clear error", {
  withr::local_options(faofishingreport.data_dir = withr::local_tempdir())
  expect_error(fao_area_sst(product = "hadisst1"), "download_sst_data\\(\"hadisst1\"\\)")
})

test_that("weighted quantiles and block means are computed", {
  expect_equal(fao_weighted_quantile(c(3, 1, 2), c(1, 1, 1), c(0.1, 0.5, 0.9)), c(1, 2, 3))
  expect_equal(fao_weighted_quantile(c(1, 2), c(9, 1), 0.9), 1)
  x <- array(c(1, 2, 3, NA, 5, 6, 7, 8), c(2, 2, 2))
  block <- fao_sst_block_mean(x, lon = c(0.25, 0.75), lat = c(10.25, 10.75), n = 2)
  expect_equal(as.vector(block$sst), c(2, 6.5))
  expect_equal(c(block$lon, block$lat), c(0.5, 10.5))
})

test_that("area means, spatial and seasonal ranges follow the weights", {
  # Two cells of one area, weights 3 and 1; cell 1 is at 10 C all year, cell 2 goes from 20 to 31 C
  sst <- array(c(rbind(rep(10, 12), 20:31)), c(2, 1, 12))
  weights <- data.frame(cell = 1:2, area_code = "34", area_en = "Atlantic, Eastern Central", weight = c(3, 1))
  stats <- fao_sst_area_stats(sst, weights, 2000)
  monthly <- (3 * 10 + 1 * (20:31)) / 4
  expect_equal(stats$sst_mean, round(mean(monthly), 3))
  expect_equal(c(stats$sst_min_month, stats$sst_max_month), round(range(monthly), 3))
  expect_equal(c(stats$sst_p10, stats$sst_p90), c(10, 25.5))
  expect_equal(stats$n_cells, 2)
})

test_that("NetCDF files of the products are read as longitude x latitude x month", {
  skip_if_not_installed("ncdf4")
  file <- withr::local_tempfile(fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(90, 270))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(10, -10))
  time <- ncdf4::ncdim_def("time", "days since 1891-1-1 00:00:00",
                           as.numeric(as.Date(sprintf("2000-%02d-01", 1:12)) - as.Date("1891-01-01")), unlim = TRUE)
  var <- ncdf4::ncvar_def("sst", "degC", list(lon, lat, time), missval = 1e20)
  nc <- ncdf4::nc_create(file, var)
  values <- array(seq_len(48), c(2, 2, 12))
  values[1, 1, 1] <- -1000
  ncdf4::ncvar_put(nc, var, values)
  ncdf4::nc_close(nc)

  y <- fao_sst_read_year(file, "hadisst1", 2000)
  expect_equal(dim(y$sst), c(2, 2, 12))
  expect_equal(y$lon, c(-90, 90))
  expect_equal(y$lat, c(-10, 10))
  expect_equal(y$months, sprintf("%02d", 1:12))
  # Longitude 270 becomes -90 and comes first; latitude -10 comes first; -1000 (sea ice) becomes -1.8
  expect_equal(y$sst[, , 2], matrix(c(8, 7, 6, 5), 2))
  expect_equal(y$sst[2, 2, 1], -1.8)
  expect_length(fao_sst_read_year(file, "cobe2", 2001)$months, 0)
})
