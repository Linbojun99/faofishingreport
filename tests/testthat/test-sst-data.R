# Sea surface temperature: built-in table, products, and the user's own NetCDF files (offline)

# Writes a NetCDF file of SST on a lon x lat grid, one value per time step for all cells
write_sst_nc <- function(file, lon, lat, dates, values, units = "degC", origin = "1891-01-01") {
  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  time_dim <- ncdf4::ncdim_def("time", paste("days since", origin),
                               as.numeric(as.Date(dates) - as.Date(origin)), unlim = TRUE)
  var <- ncdf4::ncvar_def("sst", units, list(lon_dim, lat_dim, time_dim), missval = 1e20)
  nc <- ncdf4::nc_create(file, var)
  ncdf4::ncvar_put(nc, var, values)
  ncdf4::nc_close(nc)
  file
}

test_that("the products are described, and ERSST v5 is the one built in", {
  products <- fao_sst_products()
  expect_equal(products$product, c("ersst5", "hadisst1", "cobe2", "oisst21"))
  expect_false(any(is.na(products[, c("name", "grid", "data_sources", "licence", "citation", "doi", "homepage")])))
  expect_equal(products$product[products$built_in], "ersst5")
  expect_equal(products$product[which.min(products$mb_per_year)], "ersst5")
})

test_that("the built-in SST table has every marine area, year and range, and says what it is", {
  withr::local_options(faofishingreport.data_dir = withr::local_tempdir())
  expect_message(sst <- fao_area_sst(), "SST: NOAA ERSST v5, built into the package")
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
  expect_equal(suppressMessages(fao_area_sst("Atlantic, Eastern Central")), suppressMessages(fao_area_sst(34)))
  expect_error(suppressMessages(fao_area_sst("99")), "Unknown FAO major marine fishing area")
})

test_that("unknown or missing SST data give clear errors", {
  withr::local_options(faofishingreport.data_dir = withr::local_tempdir())
  expect_error(fao_area_sst(sst = "ersst4"), "SST data not found")
  expect_error(fao_area_sst(sst = "hadisst1"), "download_sst_data\\(\"hadisst1\"\\)")
  expect_error(fao_settings(sst = "no/such/file.nc"), "SST data not found")
  expect_error(download_sst_data("ersst4"), "Unknown SST product")
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

test_that("NetCDF files are read as longitude x latitude x step, in degrees C", {
  skip_if_not_installed("ncdf4")
  file <- withr::local_tempfile(fileext = ".nc")
  values <- array(seq_len(48), c(2, 2, 12))
  values[1, 1, 1] <- -1000
  write_sst_nc(file, lon = c(90, 270), lat = c(10, -10), dates = sprintf("2000-%02d-01", 1:12), values = values)
  meta <- fao_sst_nc_meta(file)
  expect_equal(meta$variable, "sst")
  expect_equal(meta$lon, c(-90, 90))
  expect_equal(meta$lat, c(-10, 10))
  x <- fao_sst_nc_read(meta, 1:12)
  expect_equal(dim(x), c(2, 2, 12))
  # Longitude 270 becomes -90 and comes first; latitude -10 comes first; -1000 (sea ice) becomes -1.8
  expect_equal(x[, , 2], matrix(c(8, 7, 6, 5), 2))
  expect_equal(x[2, 2, 1], -1.8)
  expect_equal(fao_sst_nc_read(meta, c(2, 5))[, , 2], x[, , 5])

  kelvin <- withr::local_tempfile(fileext = ".nc")
  write_sst_nc(kelvin, lon = c(90, 270), lat = c(10, -10), dates = sprintf("2000-%02d-01", 1:12),
               values = values + 273.15, units = "K")
  # The file stores single-precision values
  expect_equal(fao_sst_nc_read(fao_sst_nc_meta(kelvin), 2), x[, , 2, drop = FALSE], tolerance = 1e-5)
})

test_that("daily values are averaged by month, and incomplete years are left out", {
  skip_if_not_installed("ncdf4")
  file <- withr::local_tempfile(fileext = ".nc")
  days <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
  values <- array(rep(as.numeric(format(days, "%d")), each = 4), c(2, 2, length(days)))
  write_sst_nc(file, lon = c(0.5, 1.5), lat = c(0.5, 1.5), dates = days, values = values, origin = "2000-01-01")
  months <- fao_sst_year_months(list(fao_sst_nc_meta(file)), 2001)
  expect_equal(months[1, 1, 1], mean(1:31))
  expect_equal(months[2, 2, 2], mean(1:28))
  expect_null(fao_sst_year_months(list(fao_sst_nc_meta(file)), 2002))
})

test_that("your own NetCDF file gives the table of every area, computed once and saved", {
  skip_if_not_installed("ncdf4")
  withr::local_options(faofishingreport.data_dir = withr::local_tempdir())
  file <- withr::local_tempfile(fileext = ".nc")
  lon <- seq(5, 355, by = 10)
  lat <- seq(85, -85, by = -10)
  dates <- c(sprintf("2000-%02d-15", 1:12), sprintf("2001-%02d-15", 1:6))
  # Every cell at (10 + month) degrees C, in kelvin
  values <- array(rep(273.15 + 10 + as.integer(substr(dates, 6, 7)), each = length(lon) * length(lat)),
                  c(length(lon), length(lat), length(dates)))
  write_sst_nc(file, lon, lat, dates, values, units = "K")

  expect_message(sst <- fao_area_sst(sst = file), "Computing the yearly SST")
  expect_equal(length(unique(sst$area_code)), 19)
  expect_equal(unique(sst$year), 2000)             # 2001 has only 6 months
  expect_equal(unique(sst$sst_mean), 16.5)
  expect_equal(unique(sst$sst_min_month), 11)
  expect_equal(unique(sst$sst_max_month), 22)
  expect_equal(unique(sst$source), basename(file))
  # The second time the saved table is read, and the message says which file is used
  expect_message(again <- fao_area_sst("34", sst = file), basename(file))
  expect_equal(again, sst[sst$area_code == "34", ], ignore_attr = TRUE)
  # The same file as the setting of the session
  withr::local_options(faofishingreport.sst = file)
  expect_equal(suppressMessages(fao_area_sst("34")), again)
})
