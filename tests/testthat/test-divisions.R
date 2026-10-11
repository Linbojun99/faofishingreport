# Catch by division (FAO regional data) and the analyses by division, tested offline

# A small FI_Regional zip with the files and columns of the FAO release
fake_regional_zip <- function(dir) {
  write <- function(x, file) utils::write.csv(x, file.path(dir, file), row.names = FALSE, na = "")
  write(data.frame(UN_Code = c("504", "380"), Name_En = c("Morocco", "Italy")), "CL_FI_COUNTRY_GROUPS.csv")
  write(data.frame(`3A_Code` = c("PIL", "HKE"), Name_En = c("European pilchard(=Sardine)", "European hake"),
                   Scientific_Name = c("Sardina pilchardus", "Merluccius merluccius"), check.names = FALSE),
        "CL_FI_SPECIES_GROUPS.csv")
  write(data.frame(Code = c("37.1.1", "37.2.1", "37.9.0"), Name_En = c("Balearic", "Adriatic", "Not known (GFCM area)"),
                   Local_Name = NA), "CL_FI_WATERAREA_DIVISION.csv")
  write(data.frame(COUNTRY.UN_CODE = c("380", "380", "504", "380"), SPECIES.ALPHA_3_CODE = c("PIL", "HKE", "PIL", "PIL"),
                   DIVISION.CODE = c("37.1.1", "37.2.1", "37.9.0", "37.1.1"), PERIOD = c(2000, 2000, 2001, 2000),
                   VALUE = c(100, 50, 7, 3), STATUS = c("A", "E", "A", "A"), MEASURE = c("Q_tlw", "Q_tlw", "Q_tlw", "Q_no_1")),
        "GFCM_Capture_Quantity.csv")
  zip_file <- file.path(dir, "FI_Regional_test.zip")
  withr::with_dir(dir, utils::zip(zip_file, list.files(dir, pattern = "\\.csv$"), flags = "-q"))
  zip_file
}

test_that("a regional release is converted to the package format with the division columns", {
  skip_if(Sys.which("zip") == "", "no zip program")
  zip_file <- fake_regional_zip(withr::local_tempdir())
  data <- fao_convert_divisions(zip_file)
  expect_named(data, c("Country..Name.", "ASFIS.species..Name.", "FAO.major.fishing.area..Name.", "Unit..Name.",
                       "Unit", "Year", "Production", "Production.Type", "Division", "Division..Name.", "Region"))
  expect_equal(nrow(data), 3)                                  # tonnes only
  expect_equal(unique(data$FAO.major.fishing.area..Name.), "Mediterranean and Black Sea")
  expect_equal(sort(unique(data$Division..Name.)), c("37.1.1 Balearic", "37.2.1 Adriatic", "37.9.0 Not known (GFCM area)"))
  expect_equal(data$Production.Type[data$Division == "37.2.1"], "estimate")
  expect_equal(unique(data$Region), "GFCM")
  expect_true(all(c("Italy", "Morocco") %in% data$Country..Name.))
})

test_that("saved regional data are used offline, by region", {
  dir <- withr::local_tempdir()
  withr::local_options(faofishingreport.online = FALSE)
  data <- data.frame(Country..Name. = "Italy", ASFIS.species..Name. = "European hake", Year = 2000L, Production = 1,
                     Division = c("37.1.1", "34.1.1"), Division..Name. = c("37.1.1 Balearic", "34.1.1 Morocco coastal"),
                     Region = c("GFCM", "CECAF"), stringsAsFactors = FALSE)
  saveRDS(data, fao_division_file("2026.1.0", dir))
  med <- suppressMessages(load_fao_divisions("GFCM", data_dir = dir))
  expect_equal(med$Division, "37.1.1")
  expect_message(load_fao_divisions(data_dir = dir), "release 2026.1.0")
  expect_error(load_fao_divisions("ICES", data_dir = dir), "Unknown region")
  expect_error(load_fao_divisions(data_dir = withr::local_tempdir()), "no regional data")
})

test_that("each division code finds the SST unit of the FAO map, else its subarea", {
  available <- c("34.1", "34.1.1", "34.2", "37.1.1", "51.3")
  expect_equal(fao_division_sst_unit(c("34.1.1", "34.2.0", "34.1.9", "51.3.1", "34.0.0", "34.9.0", "37.1.1"), available),
               c("34.1.1", "34.2", "34.1", "51.3", NA, NA, "37.1.1"))
})

test_that("the built-in SST table of subareas and divisions covers areas 34, 37, 47 and 51", {
  withr::local_options(faofishingreport.data_dir = withr::local_tempdir())
  expect_message(div <- fao_area_sst(level = "division"), "subareas and divisions of areas 34, 37, 47 and 51")
  expect_named(div, names(suppressMessages(fao_area_sst())))
  expect_setequal(unique(sub("\\..*$", "", div$area_code)), c("34", "37", "47", "51"))
  expect_true(all(c("37.1.1", "37.4.2", "34.2", "51.3") %in% div$area_code))
  expect_true(all(div$sst_p10 <= div$sst_p90))
  expect_equal(unique(div$source), "NOAA ERSST v5")
  expect_equal(suppressMessages(fao_area_sst("Black Sea", level = "division"))$area_code[1], "37.4.2")
  expect_error(suppressMessages(fao_area_sst("37.1.1")), "Unknown FAO major marine fishing area")
})

division_catch <- function() {
  rows <- function(division, name, species, catch) {
    data.frame(Country..Name. = "Italy", ASFIS.species..Name. = species,
               FAO.major.fishing.area..Name. = "Mediterranean and Black Sea", Unit..Name. = "Tonnes - live weight",
               Unit = "TLW, Number", Year = 2000:2002, Production = catch, Production.Type = "actual",
               Division = division, Division..Name. = name, Region = "GFCM", stringsAsFactors = FALSE)
  }
  rbind(
    rows("37.2.1", "37.2.1 Adriatic", "European pilchard(=Sardine)", c(300, 300, 300)),
    rows("37.2.1", "37.2.1 Adriatic", "European hake", c(100, 150, 300)),
    rows("37.1.1", "37.1.1 Balearic", "European pilchard(=Sardine)", c(50, 50, 50)),
    rows("37.1.1", "37.1.1 Balearic", "European hake", c(100, 100, 100))
  )
}

test_that("the pelagic/demersal ratio is calculated by division", {
  expect_error(pelagic_demersal(division_catch()[, 1:8], by = "division", plot = FALSE, table = FALSE),
               "load_fao_divisions")
  result <- suppressMessages(pelagic_demersal(division_catch(), by = "division", plot = FALSE, table = FALSE))
  expect_equal(unique(result$yearly$area_code), c("37.1.1", "37.2.1"))            # sorted by code
  adriatic <- result$yearly[result$yearly$area_code == "37.2.1", ]
  expect_equal(adriatic$pd_ratio, 300 / c(100, 150, 300))
  expect_equal(result$yearly$pd_ratio[result$yearly$area_code == "37.1.1"], rep(0.5, 3))
  expect_equal(unique(result$decades$area_code), c("37.1.1", "37.2.1"))
  # By area, the divisions are summed
  area <- suppressMessages(pelagic_demersal(division_catch(), plot = FALSE, table = FALSE))
  expect_equal(area$yearly$pd_ratio, 350 / c(200, 250, 400))
})

test_that("the mean temperature of the catch is calculated by division, with the SST of each division", {
  match <- data.frame(species_en = c("European pilchard(=Sardine)", "European hake"), match_type = "exact",
                      temp_pref_mean = c(10, 20), stringsAsFactors = FALSE)
  sst <- data.frame(area_code = c("37.1.1", "37.2.1"), year = 2000, sst_mean = c(19, 17))
  result <- suppressMessages(mean_temperature_catch(division_catch(), match = match, sst = sst, by = "division",
                                                    min_coverage = 0, plot = FALSE, table = FALSE))
  yearly <- result$yearly
  expect_equal(unique(yearly$area_code), c("37.1.1", "37.2.1"))
  adriatic <- yearly[yearly$area_code == "37.2.1", ]
  expect_equal(adriatic$mtc, (10 * 300 + 20 * c(100, 150, 300)) / (300 + c(100, 150, 300)))
  expect_equal(adriatic$sst_mean, c(17, NA, NA))
  expect_equal(unique(yearly$sst_unit), c("37.1.1", "37.2.1"))
  # The built-in SST table of divisions is used by default
  default <- suppressMessages(mean_temperature_catch(division_catch(), match = match, by = "division",
                                                     min_coverage = 0, plot = FALSE, table = FALSE))
  expect_false(anyNA(default$yearly$sst_mean))
})
