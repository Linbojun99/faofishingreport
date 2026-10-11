# Trade data and the comparison of catch and trade, tested offline on made-up data

# A small FI_Trade zip with the files and columns of the FAO release
fake_trade_zip <- function(dir) {
  write <- function(x, file) utils::write.csv(x, file.path(dir, file), row.names = FALSE, na = "")
  write(data.frame(UN_Code = c("578", "604"), Name_En = c("Norway", "Peru")), "CL_FI_COUNTRY_GROUPS.csv")
  write(data.frame(Code = c("034.1.5.2.19", "081.42.1.5.9.90"), ISSCAAP = c("32", "39"),
                   Name_En = c("Cod, fresh or chilled", "Fishmeals, nei")), "CL_FI_COMMODITY_ISSCFC.csv")
  rows <- data.frame(TRADE_FLOW.ALPHA_CODE = c("E", "I", "E", "E"), COUNTRY_REPORTER.UN_CODE = c("578", "578", "604", "578"),
                     COMMODITY.FAO_CODE = c("034.1.5.2.19", "034.1.5.2.19", "081.42.1.5.9.90", "034.1.5.2.19"),
                     PERIOD = c(2020, 2020, 2020, 2021), STATUS = c("A", "X", "A", "I"))
  write(cbind(rows, MEASURE = "Q_tpw", VALUE = c(100, 20, 500, 90))[-4, ], "TRADE_QUANTITY.csv")   # no quantity for 2021
  write(cbind(rows, MEASURE = "V_USD_1000", VALUE = c(300, 50, 700, 250)), "TRADE_VALUE.csv")
  zip_file <- file.path(dir, "FI_Trade_test.zip")
  withr::with_dir(dir, utils::zip(zip_file, list.files(dir, pattern = "\\.csv$"), flags = "-q"))
  zip_file
}

test_that("a trade release is converted with quantity, value, flow and ISSCAAP group", {
  skip_if(Sys.which("zip") == "", "no zip program")
  data <- fao_convert_trade(fake_trade_zip(withr::local_tempdir()))
  expect_named(data, c("Country..Name.", "Commodity..Code.", "Commodity..Name.", "ISSCAAP.group..Code.",
                       "ISSCAAP.group..Name.", "Trade.flow", "Year", "Quantity", "Value", "Quantity.Type", "Value.Type"))
  expect_equal(nrow(data), 4)
  cod <- data[data$Country..Name. == "Norway" & data$Year == 2020, ]
  expect_equal(cod$Trade.flow, c("Exports", "Imports"))
  expect_equal(cod$Quantity, c(100, 20))
  expect_equal(cod$Value, c(300, 50))
  expect_equal(cod$Value.Type, c("actual", "partner data"))
  expect_equal(unique(data$ISSCAAP.group..Name.[data$ISSCAAP.group..Code. == 32]), "Cods, hakes, haddocks")
  expect_equal(data$ISSCAAP.group..Name.[data$Country..Name. == "Peru"], "Marine fishes not identified")
  value_only <- data[data$Year == 2021, ]
  expect_true(is.na(value_only$Quantity))
  expect_equal(value_only$Value, 250)
})

test_that("saved trade data are used offline", {
  dir <- withr::local_tempdir()
  withr::local_options(faofishingreport.online = FALSE)
  saveRDS(data.frame(Country..Name. = "Norway", Year = 2020L, Quantity = 1), fao_trade_file("2026.1.0", dir))
  expect_message(load_fao_trade(data_dir = dir), "release 2026.1.0")
  expect_error(load_fao_trade(data_dir = withr::local_tempdir()), "no trade data")
})

test_that("the groups mostly caught are those with at least 90% capture in every year", {
  groups <- fao_capture_groups()
  counted <- groups$isscaap_en[groups$counted]
  expect_true(all(c("Cods, hakes, haddocks", "Herrings, sardines, anchovies", "Tunas, bonitos, billfishes",
                    "Squids, cuttlefishes, octopuses") %in% counted))
  expect_false(any(c("Salmons, trouts, smelts", "Shrimps, prawns", "Oysters", "Marine fishes not identified") %in% counted))
  expect_false(any(c("Corals", "Sponges", "Brown seaweeds") %in% groups$isscaap_en))     # not aquatic animals
  expect_true(all(groups$capture_pct_min[groups$counted] >= 90))
  # Shrimps were mostly caught until 1985
  expect_true(groups$counted[groups$isscaap_en == "Shrimps, prawns"] == FALSE)
  early <- fao_capture_groups(years = 1976:1985)
  expect_true(early$counted[early$isscaap_en == "Shrimps, prawns"])
  expect_true("Flounders, halibuts, soles" %in% with(fao_capture_groups(80), isscaap_en[counted]))
})

trade_catch_data <- function() {
  rows <- function(species, catch) {
    data.frame(Country..Name. = "Norway", ASFIS.species..Name. = species,
               FAO.major.fishing.area..Name. = "Atlantic, Northeast", Unit..Name. = "Tonnes - live weight",
               Unit = "TLW, Number", Year = 2023:2024, Production = catch, Production.Type = "actual",
               stringsAsFactors = FALSE)
  }
  rbind(rows("Atlantic cod", c(1000, 1000)), rows("Atlantic herring", c(500, 500)), rows("Atlantic salmon", c(50, 50)))
}

trade_data <- function() {
  row <- function(code, group_code, group, flow, year, quantity, value, type = "actual") {
    data.frame(Country..Name. = "Norway", Commodity..Code. = code, Commodity..Name. = code,
               ISSCAAP.group..Code. = group_code, ISSCAAP.group..Name. = group, Trade.flow = flow, Year = year,
               Quantity = quantity, Value = value, Quantity.Type = type, Value.Type = type, stringsAsFactors = FALSE)
  }
  cod <- "Cods, hakes, haddocks"
  herring <- "Herrings, sardines, anchovies"
  rbind(
    row("034.1.5.2.19", 32, cod, "Exports", 2023:2024, 300, 900),
    row("034.1.5.2.19", 32, cod, "Exports", 2023:2024, NA, 100),                         # value only
    row("034.1.5.2.19", 32, cod, "Imports", 2023:2024, 100, 200),
    row("034.1.5.2.19", 32, cod, "Re-exports", 2023:2024, 10, 40),
    row("034.1.5.5.19", 35, herring, "Exports", 2023:2024, 600, 1200, "partner data"),   # more than the catch
    row("034.1.2.3.4", 23, "Salmons, trouts, smelts", "Exports", 2023:2024, 2000, 10000),
    row("081.42.1.5.9.90", 39, "Marine fishes not identified", "Exports", 2023:2024, 50, 60)
  )
}

run_trade <- function(...) {
  suppressMessages(catch_trade("Norway", data = trade_catch_data(), trade = trade_data(), plot = FALSE, table = FALSE, ...))
}

test_that("catch and trade are compared for the groups mostly caught only", {
  result <- run_trade()
  y <- result$yearly[result$yearly$year == 2023, ]
  expect_equal(y$catch_tonnes, 1500)
  expect_equal(y$export_tonnes, 900)
  expect_equal(y$import_tonnes, 100)
  expect_equal(y$reexport_tonnes, 10)
  expect_equal(y$export_value, 900 + 100 + 1200)
  expect_equal(y$export_unit_value, (900 + 1200) / 900)       # priced rows only, USD per kg
  expect_equal(y$import_unit_value, 2)
  expect_equal(y$export_catch_pct, 100 * 900 / 1500)
  expect_equal(y$net_export_tonnes, 800)
  expect_equal(y$counted_catch_pct, 100 * 1500 / 1550)
  expect_equal(y$counted_export_value_pct, 100 * 2200 / (2200 + 10000 + 60))
  expect_equal(y$estimated_export_value_pct, 100 * 1200 / 2200)
  expect_equal(y$meal_oil_export_tonnes, 50)
  expect_equal(y$meal_oil_export_value, 60)
  expect_equal(nrow(result$yearly), 2)

  g <- result$by_group[result$by_group$year == 2023, ]
  expect_setequal(g$isscaap_en[g$catch_tonnes > 0 | g$export_tonnes > 0], c("Cods, hakes, haddocks", "Herrings, sardines, anchovies"))
  expect_true(g$exports_exceed_catch[g$isscaap_en == "Herrings, sardines, anchovies"])
  expect_false(g$exports_exceed_catch[g$isscaap_en == "Cods, hakes, haddocks"])
  expect_false("Salmons, trouts, smelts" %in% result$by_group$isscaap_en)

  groups <- result$groups
  expect_equal(groups$compared[groups$isscaap_en == "Salmons, trouts, smelts"], FALSE)
  expect_equal(groups$compared[groups$isscaap_en == "Cods, hakes, haddocks"], TRUE)
  expect_equal(groups[[grep("^export_value_[0-9]", names(groups))]][groups$isscaap_en == "Salmons, trouts, smelts"], 10000)    # mean yearly value
  suppressMessages(expect_message(catch_trade("Norway", data = trade_catch_data(), trade = trade_data(), plot = FALSE, table = FALSE),
                 "exports exceed the catch in Herrings, sardines, anchovies \\(2 years\\)"))
})

test_that("trade is converted to live weight with the factors given", {
  factors <- data.frame(commodity_code = "034.1.5.2.19", factor = 2.5)
  y <- run_trade(conversion = factors)$yearly[1, ]
  expect_equal(y$export_tonnes, 900)                        # product weight kept
  expect_equal(y$export_tonnes_lw, 300 * 2.5 + 600)         # herring without a factor stays in product weight
  expect_equal(y$import_tonnes_lw, 250)
  expect_equal(y$converted_pct, 100 * 410 / 1010)
  expect_equal(y$apparent_supply_tonnes, 1500 + 250 - 1350 - 25)
  expect_equal(y$export_catch_pct, 100 * 1350 / 1500)
  expect_error(run_trade(conversion = data.frame(code = 1)), "commodity_code")
  expect_error(run_trade(conversion = data.frame(commodity_code = "a", factor = -1)), "positive")
})

test_that("countries without data and the years are checked", {
  expect_error(run_trade(years = 1990), "No year")
  expect_error(suppressMessages(catch_trade("Peru", data = trade_catch_data(), trade = trade_data(), plot = FALSE,
                                            table = FALSE)), "No catch data found for: Peru")
  expect_equal(run_trade(years = 2024)$yearly$year, 2024)
})

test_that("charts and tables are saved", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.dpi = 72)
  suppressMessages(catch_trade("Norway", data = trade_catch_data(), trade = trade_data(), lang = "both"))
  expect_true(all(file.exists(file.path("figures/png/summary", c("Norway_catch_trade.png", "Norway_catch_trade_groups.png")))))
  expect_setequal(list.files("tables"), c("Norway_catch_trade.csv", "Norway_catch_trade_by_group.csv",
                                          "Norway_catch_trade_groups.csv"))
})
