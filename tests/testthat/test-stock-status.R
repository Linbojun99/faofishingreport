test_that("each year is classified relative to the peak catch", {
  catch <- c(10, 40, 100, 60, 30, 5, 20, 70)
  expect_equal(
    fao_stock_status(catch),
    c("developing", "developing", "exploited", "exploited", "overexploited",
      "collapsed", "rebuilding", "exploited")
  )
  expect_equal(fao_stock_status(c(1, 2, 10)), c("developing", "developing", "exploited"))
})

test_that("stock_status() classifies the stocks of a fishing area", {
  data <- load_fao_data("bundled")
  area <- data[data$FAO.major.fishing.area..Name. == "Atlantic, Eastern Central", ]
  result <- stock_status(area, plot = FALSE, table = FALSE)
  expect_named(result, c("yearly", "stocks", "status"))
  expect_gt(nrow(result$stocks), 50)
  shares <- tapply(result$yearly$stocks_pct, result$yearly$Year, sum)
  expect_true(all(abs(shares - 100) < 1e-6))
})
