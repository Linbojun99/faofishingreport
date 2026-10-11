# Pelagic/demersal ratio, tested offline on made-up catch data

pd_catch <- function() {
  rows <- function(area, species, catch, years = 2000:2003) {
    data.frame(Country..Name. = "Morocco", ASFIS.species..Name. = species,
               FAO.major.fishing.area..Name. = area, Unit..Name. = "Tonnes - live weight",
               Unit = "TLW, Number", Year = years, Production = catch, Production.Type = "actual",
               stringsAsFactors = FALSE)
  }
  rbind(
    rows("Atlantic, Eastern Central", "European pilchard(=Sardine)", c(300, 400, 500, 600)),  # Herrings, sardines, anchovies
    rows("Atlantic, Eastern Central", "Skipjack tuna", c(50, 50, 50, 50)),                    # Tunas, bonitos, billfishes
    rows("Atlantic, Eastern Central", "Senegalese hake", c(100, 100, 200, 300)),              # Cods, hakes, haddocks
    rows("Atlantic, Eastern Central", "Common octopus", c(20, 20, 20, 20)),                   # Squids, cuttlefishes, octopuses
    rows("Pacific, Northwest", "Japanese anchovy", c(10, 10, 10, 10)),
    rows("Pacific, Northwest", "Alaska pollock(=Walleye poll.)", c(10, 20, 40, 80)),
    rows("Asia - Inland waters", "Freshwater fishes nei", c(5, 5, 5, 5))
  )
}

pelagic <- "Herrings, sardines, anchovies"
demersal <- "Cods, hakes, haddocks"

run_pd <- function(...) {
  suppressMessages(pelagic_demersal(pd_catch(), pelagic = pelagic, demersal = demersal, plot = FALSE,
                                    table = FALSE, ...))
}

test_that("the species groups of the test data are as expected", {
  groups <- fao_group_table()$species
  isscaap <- groups$isscaap_en[match(c("European pilchard(=Sardine)", "Skipjack tuna", "Senegalese hake",
                                       "Common octopus", "Japanese anchovy", "Alaska pollock(=Walleye poll.)"),
                                     groups$name_en)]
  expect_equal(isscaap, c(pelagic, "Tunas, bonitos, billfishes", demersal, "Squids, cuttlefishes, octopuses",
                          pelagic, demersal))
})

test_that("the P/D ratio is the pelagic catch divided by the demersal catch, by area and year", {
  result <- run_pd()
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$year, 2000:2003)
  expect_equal(ec$pd_ratio, c(300, 400, 500, 600) / c(100, 100, 200, 300))
  expect_equal(ec$pelagic_pct, 100 * c(300, 400, 500, 600) / c(400, 500, 700, 900))
  expect_equal(ec$other_tonnes, rep(70, 4))
  expect_equal(ec$counted_pct, 100 * c(400, 500, 700, 900) / c(470, 570, 770, 970))
  expect_equal(ec$unidentified_pct, rep(0, 4))
  nw <- result$yearly[result$yearly$area_en == "Pacific, Northwest", ]
  expect_equal(nw$pd_ratio, 10 / c(10, 20, 40, 80))
})

test_that("inland areas are left out", {
  expect_message(pelagic_demersal(pd_catch(), pelagic = pelagic, demersal = demersal, plot = FALSE, table = FALSE),
                 "Inland fishing areas are left out")
  expect_false("Asia - Inland waters" %in% run_pd()$yearly$area_en)
})

test_that("the ratio of a decade is the mean of its yearly ratios, with the lowest and highest", {
  result <- run_pd()
  ec <- result$decades[result$decades$area_en == "Atlantic, Eastern Central", ]
  yearly <- c(300, 400, 500, 600) / c(100, 100, 200, 300)
  expect_equal(ec$period, "2000s")
  expect_equal(c(ec$pd_mean, ec$pd_min, ec$pd_max), c(mean(yearly), min(yearly), max(yearly)))
  expect_equal(ec$n_years, 4)
})

test_that("the two published definitions count the ISSCAAP groups as documented", {
  groups <- fao_pd_groups()
  expect_true(all(groups$isscaap_en %in% fao_group_table()$species$isscaap_en))
  small <- groups$isscaap_en[groups$de_leiva_moreno == "pelagic"]
  expect_equal(small, "Herrings, sardines, anchovies")
  expect_true(all(c("Shrimps, prawns", "Miscellaneous coastal fishes") %in%
                    groups$isscaap_en[groups$de_leiva_moreno == "demersal"]))
  expect_true(all(c("Tunas, bonitos, billfishes", "Miscellaneous pelagic fishes") %in%
                    groups$isscaap_en[groups$grainger_garcia == "pelagic"]))
  expect_true("Marine fishes not identified" %in% groups$isscaap_en[groups$grainger_garcia == "demersal"])

  # Default: small pelagics against demersal fishes and crustaceans; tunas and octopus not counted
  ec <- suppressMessages(pelagic_demersal(pd_catch(), plot = FALSE, table = FALSE))$yearly
  ec <- ec[ec$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$pd_ratio, c(300, 400, 500, 600) / c(100, 100, 200, 300))
  # All pelagic fishes: tunas counted as pelagic
  all_fishes <- suppressMessages(pelagic_demersal(pd_catch(), definition = "grainger_garcia", plot = FALSE,
                                                  table = FALSE))$yearly
  all_fishes <- all_fishes[all_fishes$area_en == "Atlantic, Eastern Central", ]
  expect_equal(all_fishes$pd_ratio, (c(300, 400, 500, 600) + 50) / c(100, 100, 200, 300))
  expect_message(pelagic_demersal(pd_catch(), plot = FALSE, table = FALSE), "de Leiva Moreno et al. 2000")
})

test_that("the groups can be chosen, and the group table says how each is counted", {
  result <- suppressMessages(pelagic_demersal(pd_catch(), pelagic = c(pelagic, "Tunas, bonitos, billfishes"),
                                              demersal = demersal, plot = FALSE, table = FALSE))
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_equal(ec$pd_ratio, (c(300, 400, 500, 600) + 50) / c(100, 100, 200, 300))
  groups <- result$groups
  expect_equal(groups$counted_as[groups$isscaap_en == "Tunas, bonitos, billfishes"], "pelagic")
  expect_equal(groups$counted_as[groups$isscaap_en == "Squids, cuttlefishes, octopuses"], "not counted")
  expect_error(pelagic_demersal(pd_catch(), pelagic = "Sardines", plot = FALSE, table = FALSE), "Unknown ISSCAAP group")
  expect_error(pelagic_demersal(pd_catch(), pelagic = pelagic, demersal = pelagic, plot = FALSE, table = FALSE),
               "both pelagic and demersal")
})

test_that("a year without demersal catch has no ratio", {
  data <- pd_catch()
  data$Production[data$ASFIS.species..Name. == "Senegalese hake" & data$Year == 2001] <- 0
  result <- suppressMessages(pelagic_demersal(data, pelagic = pelagic, demersal = demersal, plot = FALSE, table = FALSE))
  ec <- result$yearly[result$yearly$area_en == "Atlantic, Eastern Central", ]
  expect_true(is.na(ec$pd_ratio[ec$year == 2001]))
})

test_that("charts and tables are saved", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.dpi = 72)
  suppressMessages(pelagic_demersal(pd_catch(), pelagic = pelagic, demersal = demersal, label = "test", lang = "both"))
  expect_true(all(file.exists(file.path("figures/png/summary",
                                        c("test_pelagic_demersal_ratio.png", "test_pelagic_demersal.png",
                                          "test_pelagic_demersal_share.png")))))
  expect_setequal(list.files("tables"), c("test_pelagic_demersal.csv", "test_pelagic_demersal_decades.csv",
                                          "test_pelagic_demersal_groups.csv"))
})
