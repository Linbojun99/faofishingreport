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
