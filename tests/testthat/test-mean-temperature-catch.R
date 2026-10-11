# Mean temperature of the catch, tested offline on made-up catch and match tables

mtc_catch <- function() {
  rows <- function(area, species, catch) {
    data.frame(Country..Name. = "Morocco", ASFIS.species..Name. = species,
               FAO.major.fishing.area..Name. = area, Unit..Name. = "Tonnes - live weight",
               Unit = "TLW, Number", Year = 2000:2003, Production = catch, Production.Type = "actual",
               stringsAsFactors = FALSE)
  }
  rbind(
    rows("Atlantic, Eastern Central", "Cold fish", c(100, 100, 100, 100)),
    rows("Atlantic, Eastern Central", "Warm fish", c(0, 100, 300, 100)),
    rows("Atlantic, Eastern Central", "Fishes nei", c(100, 100, 100, 250)),
    rows("Pacific, Northwest", "Cold fish", c(50, 50, 50, 50)),
    rows("Pacific, Northwest", "Unknown fish", c(50, 50, 50, 50))
  )
}

mtc_match <- data.frame(
  species_en = c("Cold fish", "Warm fish", "Fishes nei", "Unknown fish"),
  fao_scientific_name = c("Gadus morhua", "Sardinella aurita", "Sardinella spp", "Fishus unknownus"),
  match_type = c("exact", "synonym", "genus", "not found"),
  temp_pref_mean = c(10, 20, 30, NA),
  url = c("https://www.fishbase.se/summary/Gadus-morhua.html", NA, NA, NA),
  stringsAsFactors = FALSE
)

run_mtc <- function(...) {
  suppressMessages(mean_temperature_catch(mtc_catch(), match = mtc_match, plot = FALSE, table = FALSE, ...))
}

test_that("the MTC is the catch-weighted mean of the items with a preferred temperature", {
  result <- run_mtc()
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$year, 2000:2003)
  expect_equal(ec$mtc, c(10, 15, 17.5, 15))
  expect_equal(ec$coverage_pct, 100 * c(100, 200, 400, 200) / c(200, 300, 500, 450))
  expect_equal(ec$n_items, c(1, 2, 2, 2))
  expect_equal(ec$area_code, rep("34", 4))
})

test_that("only the chosen kinds of match are used", {
  result <- run_mtc(use_match = "exact")
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$mtc, rep(10, 4))
  with_group <- run_mtc(use_match = c("exact", "synonym", "genus"))
  ec <- with_group$yearly[with_group$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$mtc[1], (10 * 100 + 30 * 100) / 200)
  species <- run_mtc()$species
  species <- species[species$area_en == "Atlantic, Eastern Central", ]
  expect_equal(species$used, c(FALSE, TRUE, TRUE))
  expect_equal(species$reason[species$species_en == "Fishes nei"], "match type \"genus\" not used")
  expect_equal(species$reason[species$species_en == "Warm fish"], "")
})

test_that("valid names found in WoRMS are used, and only items with status matched", {
  match <- mtc_match
  unknown <- match$species_en == "Unknown fish"
  match$match_type[unknown] <- "WoRMS valid name (exact)"
  match$temp_pref_mean[unknown] <- 20
  match$worms_url <- ifelse(unknown, "https://www.marinespecies.org/aphia.php?p=taxdetails&id=1", NA)
  match$status <- "matched"
  run <- function(match) {
    result <- suppressMessages(mean_temperature_catch(mtc_catch(), match = match, plot = FALSE, table = FALSE))
    result$yearly <- result$yearly[result$yearly$area_en == "Pacific, Northwest", ]
    result
  }
  result <- run(match)
  expect_equal(result$yearly$mtc, rep(15, 4))
  expect_equal(result$yearly$coverage_pct, rep(100, 4))
  expect_equal(result$species$worms_url[result$species$species_en == "Unknown fish"],
               "https://www.marinespecies.org/aphia.php?p=taxdetails&id=1")
  match$status[unknown] <- "pending online check"
  expect_equal(run(match)$yearly$mtc, rep(10, 4))
})

test_that("each area is calculated separately", {
  result <- run_mtc()
  expect_equal(unique(result$yearly$area_en), c("Atlantic, Eastern Central", "Pacific, Northwest"))
  nw <- result$yearly[result$yearly$area_en == "Pacific, Northwest", ]
  expect_equal(nw$mtc, rep(10, 4))
  expect_equal(nw$coverage_pct, rep(50, 4))
  expect_equal(nw$area_code, rep("61", 4))
})

test_that("years below the minimum coverage are marked and left out of the trend", {
  result <- run_mtc(min_coverage = 60)
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$low_coverage, c(TRUE, FALSE, FALSE, TRUE))
  trend <- result$trend[result$trend$area_en == "Atlantic, Eastern Central", ]
  expect_equal(trend$n_years, 2)
  expect_true(is.na(trend$mtc_slope_per_decade))
  expect_true(all(result$yearly$low_coverage[result$yearly$area_en == "Pacific, Northwest"]))
})

test_that("trends are calculated over all years and from trend_from", {
  catch <- mtc_catch()
  catch <- catch[catch$FAO.major.fishing.area..Name. == "Atlantic, Eastern Central", ]
  catch$Production[catch$ASFIS.species..Name. == "Warm fish"] <- c(100, 200, 300, 400)
  result <- suppressMessages(mean_temperature_catch(catch, match = mtc_match, trend_from = 2001,
                                                    min_coverage = 0, plot = FALSE, table = FALSE))
  expect_equal(result$trend$period_start, c(2000, 2001))
  expect_equal(result$trend$n_years, c(4, 3))
  expect_true(all(result$trend$mtc_slope_per_decade > 0))
  mtc <- result$yearly$mtc
  expect_equal(result$trend$mtc_slope_per_decade[1], 10 * unname(stats::coef(stats::lm(mtc ~ I(2000:2003)))[2]))
})

test_that("the MTC is compared with the SST of the area, also by year-to-year differences", {
  result <- run_mtc(min_coverage = 0)
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$sst_mean, fao_area_sst("34")$sst_mean[match(2000:2003, fao_area_sst("34")$year)])

  sst <- data.frame(area_en = "Atlantic, Eastern Central", year = 2000:2003, sst_mean = c(20, 21, 23, 22))
  result <- run_mtc(min_coverage = 0, sst = sst)
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$sst_mean, c(20, 21, 23, 22))
  trend <- result$trend[result$trend$area_en == "Atlantic, Eastern Central", ]
  expect_equal(trend$cor_mtc_sst, stats::cor(ec$mtc, ec$sst_mean))
  expect_equal(trend$cor_differences, stats::cor(diff(ec$mtc), diff(ec$sst_mean)))
  expect_equal(trend$sst_slope_per_decade, 10 * unname(stats::coef(stats::lm(c(20, 21, 23, 22) ~ I(2000:2003)))[2]))

  result <- run_mtc(sst = FALSE)
  expect_true(all(is.na(result$yearly$sst_mean)))
  expect_error(run_mtc(sst = data.frame(year = 2000)), "sst must be")
})

test_that("the species table traces the MTC of the whole period to each item", {
  result <- run_mtc()
  species <- result$species[result$species$area_en == "Atlantic, Eastern Central", ]
  expect_equal(species$species_en, c("Fishes nei", "Warm fish", "Cold fish"))
  expect_equal(sum(species$catch_pct), 100)
  expect_equal(sum(species$weight_pct, na.rm = TRUE), 100)
  # Contributions add up to the MTC of the whole period
  expect_equal(sum(species$contribution_c, na.rm = TRUE), (10 * 400 + 20 * 500) / 900)
  expect_match(species$worms_url[species$species_en == "Cold fish"], "tName=Gadus%20morhua$")
  expect_match(species$worms_url[species$species_en == "Fishes nei"], "tName=Sardinella$")
})

test_that("a match table without the needed columns is refused", {
  expect_error(suppressMessages(
    mean_temperature_catch(mtc_catch(), match = mtc_match[, c("species_en", "match_type")],
                           plot = FALSE, table = FALSE)), "temp_pref_mean")
})

test_that("charts and tables are saved", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.dpi = 72)
  one <- mtc_catch()
  one <- one[one$FAO.major.fishing.area..Name. == "Atlantic, Eastern Central", ]
  suppressMessages(mean_temperature_catch(one, match = mtc_match, label = "one", lang = "both"))
  expect_true(file.exists("figures/png/summary/one_mtc.png"))
  expect_setequal(list.files("tables"), c("one_mtc.csv", "one_mtc_trend.csv", "one_mtc_species.csv"))
  suppressMessages(mean_temperature_catch(mtc_catch(), match = mtc_match, label = "two", table = FALSE))
  expect_true(all(file.exists(c("figures/png/summary/two_mtc.png", "figures/png/summary/two_mtc_coverage.png"))))
})
