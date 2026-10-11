# Analyses that follow published methods, tested offline on the bundled data or made-up data

fake_catch <- function(country, species, area, years, catch, type = "actual") {
  data.frame(Country..Name. = country, ASFIS.species..Name. = species, FAO.major.fishing.area..Name. = area,
             Unit..Name. = "Tonnes - live weight", Unit = "TLW, Number", Year = years, Production = catch,
             Production.Type = type, stringsAsFactors = FALSE)
}

test_that("releases are compared record by record", {
  old <- rbind(fake_catch("Chile", "Anchoveta", "Pacific, Southeast", 2000:2002, c(100, 200, 300)),
               fake_catch("Chile", "Old name", "Pacific, Southeast", 2000, 50),
               fake_catch("Peru", "Anchoveta", "Pacific, Southeast", 2001, 40))
  new <- rbind(fake_catch("Chile", "Anchoveta", "Pacific, Southeast", 2000:2003, c(100, 250, 300.2, 400)),
               fake_catch("Chile", "New name", "Pacific, Southeast", 2000, 50),
               fake_catch("Chile", "Jack mackerel", "Pacific, Southeast", 2001, 10))
  out <- fao_compare_data(old, new, tolerance = 0.5)
  change <- function(sp, y, country = "Chile") {
    out$records$change[out$records$species == sp & out$records$Year == y & out$records$country == country]
  }
  expect_equal(change("Anchoveta", 2001), "revised")
  expect_equal(change("Anchoveta", 2002), "unchanged")
  expect_equal(change("Anchoveta", 2003), "new year")
  expect_equal(change("Old name", 2000), "reclassified")
  expect_equal(change("New name", 2000), "reclassified")
  expect_equal(change("Jack mackerel", 2001), "added")
  expect_equal(out$records$change[out$records$country == "Peru"], "removed")
  y2001 <- out$by_year[out$by_year$Year == 2001, ]
  expect_equal(y2001$change_tonnes, (250 + 10) - (200 + 40))
  expect_equal(c(y2001$n_revised, y2001$n_added, y2001$n_removed), c(1L, 1L, 1L))
  expect_equal(out$by_year$n_reclassified[out$by_year$Year == 2000], 2L)
})

test_that("a sudden fall in catch is found as a shock by both published methods", {
  years <- 1961:2013
  set.seed(1)
  smooth <- 1000 + 10 * (years - 1961) + stats::rnorm(length(years), 0, 5)
  drop <- smooth
  drop[years == 1990] <- drop[years == 1990] * 0.4
  for (method in c("cottrell2019", "gephart2017")) {
    shocks <- fao_find_shocks(years, drop, fao_shock_settings(method))
    expect_true(1990 %in% shocks$Year, info = method)
    s <- shocks[shocks$Year == 1990, ]
    expect_true(s$loss_pct > 50 && s$loss_pct < 70, info = method)
    expect_equal(s$recovery_years, 1L, info = method)
    expect_false(1990 %in% fao_find_shocks(years, smooth, fao_shock_settings(method))$Year, info = method)
  }
  # The first years cannot be shocks: there is no baseline for them
  early <- smooth
  early[3] <- early[3] * 0.3
  expect_false(years[3] %in% fao_find_shocks(years, early, fao_shock_settings("cottrell2019"))$Year)
})

test_that("shock frequencies are shocks per series with catch", {
  withr::local_dir(withr::local_tempdir())
  years <- 1961:2013
  set.seed(2)
  base <- function() 1000 + 10 * (years - 1961) + stats::rnorm(length(years), 0, 5)
  a <- base(); a[years == 1990] <- a[years == 1990] * 0.4
  data <- rbind(fake_catch("Chile", "Anchoveta", "Pacific, Southeast", years, a),
                fake_catch("Peru", "Anchoveta", "Pacific, Southeast", years, base()),
                fake_catch("Un. Sov. Soc. Rep.", "Anchoveta", "Pacific, Southeast", 1961:1991,
                           base()[1:31]))
  out <- suppressMessages(catch_shocks(data, by = "country", plot = FALSE, table = FALSE))
  expect_equal(out$shocks$Year, 1990L)
  expect_equal(out$by_year$shock_frequency_pct[out$by_year$Year == 1990], 100 / 3)
  expect_equal(out$series$last_year[out$series$Country..Name. == "Un. Sov. Soc. Rep."], 1991L)
  rate <- out$by_group[out$by_group$grouping == "country" & out$by_group$group == "Chile", "shocks_per_series"]
  expect_equal(rate, 1)
  expect_length(list.files(), 0)
})

test_that("collapses and their shapes follow Mullon et al. (2005)", {
  years <- 1950:2000
  plateau <- c(seq(100, 1000, length.out = 20), rep(900, 16), 300, 200, rep(50, 13))   # high until 2 years before
  smooth <- c(seq(100, 1000, length.out = 20), seq(1000, 50, length.out = 21), rep(50, 10))
  erratic <- c(seq(100, 1000, length.out = 20), rep(c(150, 600), 8), 200, 300, 150, rep(50, 12))
  steady <- rep(1000, length(years))
  short_dip <- c(rep(1000, 30), rep(50, 3), rep(1000, 18))                             # only 3 years below
  expect_equal(fao_mullon_collapse(years, plateau)$type, "plateau")
  expect_equal(fao_mullon_collapse(years, smooth)$type, "smooth")
  expect_equal(fao_mullon_collapse(years, erratic)$type, "erratic")
  expect_false(fao_mullon_collapse(years, steady)$collapsed)
  expect_false(fao_mullon_collapse(years, short_dip)$collapsed)
  expect_equal(fao_mullon_collapse(years, smooth)$collapse_year, years[which(smooth < 100)[1]])
  for (catch in list(plateau, smooth, erratic, steady, short_dip)) {
    expect_equal(fao_mullon_is_collapsed(catch), fao_mullon_collapse(years, catch)$collapsed)
  }
})

test_that("Worm et al. (2006) collapse shares and the random-series check", {
  catch <- c(5, 50, 100, 80, 5, 5, 60, 70)                 # starts in year 2, collapsed in years 5-6
  state <- fao_worm_states(catch)
  expect_equal(state$start, 2)
  expect_equal(which(state$collapsed), c(5L, 6L))
  shares <- fao_worm_shares(list(state, fao_worm_states(rep(10, 8))), 1:8)
  expect_equal(shares$pct_current[5], 50)
  expect_equal(shares$pct_cumulative[8], 50)
  set.seed(3)
  sims <- fao_null_series(list(list(catch = c(NA, NA, 1:48))), 1:50, n_sim = 200)
  values <- unlist(lapply(sims, function(s) s[[1]][-(1:2)]))
  expect_true(all(is.na(sims[[1]][[1]][1:2])))
  expect_equal(stats::sd(values) / mean(values), 0.8, tolerance = 0.15)
})

test_that("collapse_analysis() returns its tables without writing files", {
  withr::local_dir(withr::local_tempdir())
  area <- local({
    data <- load_fao_data("bundled")
    data[data$FAO.major.fishing.area..Name. == "Atlantic, Eastern Central", ]
  })
  set.seed(4)
  m <- suppressMessages(collapse_analysis(area, n_sim = 5, plot = FALSE, table = FALSE))
  expect_named(m, c("series", "summary", "by_year", "by_group"))
  expect_false(any(m$series$Country..Name. %in% c("Un. Sov. Soc. Rep.", "South Africa", "China")))
  expect_equal(sum(m$summary$n[-1]), m$summary$n[1])
  w <- suppressMessages(collapse_analysis(area, method = "worm2006", n_sim = 5, plot = FALSE, table = FALSE))
  expect_true(all(w$by_year$pct_cumulative >= w$by_year$pct_current, na.rm = TRUE))
  expect_true(all(c("null_pct_current", "null_pct_cumulative") %in% names(w$by_year)))
  expect_length(list.files(), 0)
})

test_that("synchrony is 1 for synchronous series and 0 for compensating ones", {
  years <- 2000:2019
  wave <- sin(seq_along(years))
  same <- cbind(a = 100 + 10 * wave, b = 200 + 20 * wave)
  opposite <- cbind(a = 100 + 10 * wave, b = 100 - 10 * wave)
  expect_equal(fao_synchrony(same, years, "none")$phi, 1)
  expect_equal(fao_synchrony(opposite, years, "none")$phi, 0, tolerance = 1e-10)
  # A shared trend makes series look synchronous unless it is removed (Leps et al. 2019)
  set.seed(5)
  trend <- cbind(a = 10 * seq_along(years) + stats::rnorm(20), b = 10 * seq_along(years) + stats::rnorm(20))
  expect_gt(fao_synchrony(trend, years, "none")$phi, 0.9)
  expect_lt(fao_synchrony(trend, years, "linear")$phi, 0.9)
  # CV of the total = sqrt(phi) x weighted mean CV (Thibaut and Connolly 2013)
  m <- fao_synchrony(cbind(trend, c = 50 + stats::rnorm(20, 0, 5)), years, "linear")
  expect_equal(m$cv_total, sqrt(m$phi) * m$weighted_cv)
  expect_equal(m$portfolio, 100 * (m$mean_cv - m$cv_total) / m$cv_total)
})

test_that("catch_synchrony() chooses series as Ong et al. (2021) and returns its tables", {
  withr::local_dir(withr::local_tempdir())
  years <- 1990:2019
  set.seed(6)
  data <- rbind(fake_catch("Chile", "Anchoveta(=Peruvian anchovy)", "Pacific, Southeast", years, 1000 + stats::rnorm(30, 0, 100)),
                fake_catch("Chile", "Chilean jack mackerel", "Pacific, Southeast", years, 500 + stats::rnorm(30, 0, 50)),
                fake_catch("Chile", "Pacific sardine", "Pacific, Southeast", years, c(rep(0, 10), rep(300, 20))),
                fake_catch("Chile", "Araucanian herring", "Pacific, Southeast", years, rep(5, 30)))
  out <- suppressMessages(catch_synchrony(data, window = 10, plot = FALSE, table = FALSE))
  expect_named(out, c("summary", "series", "windows", "correlation"))
  expect_setequal(out$series$series_en, c("Anchoveta(=Peruvian anchovy)", "Chilean jack mackerel"))   # 2/3 non-zero; median 5 t
  expect_equal(nrow(out$windows), length(years) - 10 + 1)
  expect_true(out$summary$synchrony_phi >= 0 && out$summary$synchrony_phi <= 1)
  expect_length(list.files(), 0)
})

test_that("the distant-water profile splits a country's catch by adjacent and distant areas", {
  withr::local_dir(withr::local_tempdir())
  dw <- suppressMessages(distant_water_profile("Spain", plot = FALSE, table = FALSE, data_version = "bundled"))
  expect_named(dw, c("yearly", "areas", "distant"))
  y <- dw$yearly
  expect_true(all(y$distant_pct >= 0 & y$distant_pct <= 100, na.rm = TRUE))
  expect_equal(y$n_areas, y$n_adjacent_areas + y$n_distant_areas)
  expect_setequal(dw$areas$area_en[dw$areas$adjacent],
                  c("Atlantic, Northeast", "Atlantic, Eastern Central", "Mediterranean and Black Sea"))
  expect_equal(sum(dw$areas$share_of_distant_pct, na.rm = TRUE), 100)
  expect_error(suppressMessages(distant_water_profile("Atlantis", plot = FALSE, table = FALSE, data_version = "bundled")),
               "No catch")
  expect_length(list.files(), 0)
})
