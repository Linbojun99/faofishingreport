# Employment data, employment summaries and catch per fisher, tested offline on made-up data

# A small FI_Employment zip with the files and columns of the FAO release
fake_employment_zip <- function(dir) {
  write <- function(x, file) utils::write.csv(x, file.path(dir, file), row.names = FALSE, na = "")
  write(data.frame(UN_Code = c("578", "156"), Name_En = c("Norway", "China")), "CL_FI_COUNTRY_GROUPS.csv")
  write(data.frame(Code = c("A", "C", "I", "U"), Name_En = c("Aquaculture", "Marine Coastal Fishing",
                                                             "Inland Waters Fishing", "Unspecified")), "CL_FI_WORK_AREA.csv")
  write(data.frame(Code = c("1", "2", "4"), Name_En = c("Full time", "Part time", "Status unspecified")), "CL_FI_OCCUPATION.csv")
  write(data.frame(Code = c("U", "F", "M"), Name_En = c("Unknown gender", "Female", "Male")), "CL_FI_GENDER.csv")
  write(data.frame(COUNTRY.UN_CODE = c("578", "578", "156"), WORKAREA.CODE = c("C", "A", "U"), OCCUPATION.CODE = c("1", "2", "4"),
                   GENDER.CODE = c("M", "F", "U"), MEASURE = "Q_no_1", PERIOD = 2024, VALUE = c(100, 20, 5), STATUS = c("A", "I", "B")),
        "EMPLOY_NUMBER.csv")
  writeLines(c("<html><body>", "<div id=\"NORWAY\">", "<h4>NORWAY</h4>",
               "<p>Excludes foreign fishers &amp; crews.<p></p>Second\nline of the note.</p>", "</div>",
               "<div id=\"CHINA\">", "<h4>CHINA</h4>", "<p>Note on China.</p>", "</div>", "</body></html>"),
             file.path(dir, "NOTES_EMPLOY_COUNTRY_En.html"))
  zip_file <- file.path(dir, "FI_Employment_test.zip")
  withr::with_dir(dir, utils::zip(zip_file, list.files(dir, pattern = "\\.(csv|html)$"), flags = "-q"))
  zip_file
}

test_that("an employment release is converted with work area, working time, sex and the country notes", {
  skip_if(Sys.which("zip") == "", "no zip program")
  data <- fao_convert_employment(fake_employment_zip(withr::local_tempdir()))
  expect_named(data, c("Country..Name.", "Work.area..Code.", "Work.area", "Working.time", "Sex", "Year", "Number", "Number.Type"))
  expect_equal(data$Work.area, c("Marine Coastal Fishing", "Aquaculture", "Unspecified"))
  expect_equal(data$Working.time, c("Full time", "Part time", "Status unspecified"))
  expect_equal(data$Sex, c("Male", "Female", "Unknown gender"))
  expect_equal(data$Number.Type, c("actual", "estimate", "series break"))
  notes <- attr(data, "country_notes")
  expect_equal(notes$country_en, c("Norway", "China"))
  expect_equal(notes$note[1], "Excludes foreign fishers & crews.\nSecond line of the note.")
})

test_that("saved employment data are used offline", {
  dir <- withr::local_tempdir()
  withr::local_options(faofishingreport.online = FALSE)
  saveRDS(data.frame(Country..Name. = "Norway", Year = 2024L, Number = 1), fao_employment_file("2026.1.0", dir))
  expect_message(load_fao_employment(data_dir = dir), "release 2026.1.0")
  expect_error(load_fao_employment(data_dir = withr::local_tempdir()), "no employment data")
})

employment_data <- function() {
  rows <- function(area, time, sex, number, type = "actual") {
    data.frame(Country..Name. = "Norway", Work.area..Code. = area, Work.area = area, Working.time = time, Sex = sex,
               Year = 2022:2024, Number = number, Number.Type = type, stringsAsFactors = FALSE)
  }
  data <- rbind(
    rows("A", "Full time", "Male", 100),
    rows("C", "Full time", "Male", c(50, 50, 60)),
    rows("C", "Part time", "Female", 20),
    rows("M", "Status unspecified", "Unknown gender", 5, "estimate"),
    rows("I", "Occasional", "Unknown gender", 30),
    rows("U", "Status unspecified", "Unknown gender", 10)
  )
  attr(data, "country_notes") <- data.frame(country_en = "Norway", note = "Test note.", stringsAsFactors = FALSE)
  data
}

fisher_catch <- function() {
  rows <- function(species, area, catch) {
    data.frame(Country..Name. = "Norway", ASFIS.species..Name. = species, FAO.major.fishing.area..Name. = area,
               Unit..Name. = "Tonnes - live weight", Unit = "TLW, Number", Year = 2022:2024, Production = catch,
               Production.Type = "actual", stringsAsFactors = FALSE)
  }
  rbind(rows("Atlantic cod", "Atlantic, Northeast", 850), rows("Freshwater fishes nei", "Europe - Inland waters", 60))
}

test_that("employment is summarised by work area, sex and working time", {
  result <- suppressMessages(employment_summary("Norway", employment = employment_data(), plot = FALSE, table = FALSE))
  y <- result$yearly[result$yearly$year == 2024, ]
  expect_equal(c(y$aquaculture, y$marine_coastal, y$marine_nei, y$marine_fishing, y$inland, y$unspecified, y$total),
               c(100, 80, 5, 85, 30, 10, 225))
  expect_equal(y$female_pct, 100 * 20 / 180)
  expect_equal(y$sex_unknown_pct, 100 * 45 / 225)
  expect_equal(y$full_time_pct, 100 * 160 / 210)
  expect_equal(y$occasional_pct, 100 * 30 / 210)
  expect_equal(y$time_unknown_pct, 100 * 15 / 225)
  expect_equal(y$estimated_pct, 100 * 5 / 225)
  expect_equal(result$notes$note, "Test note.")
  suppressMessages(expect_message(employment_summary("Norway", employment = employment_data(), plot = FALSE, table = FALSE),
                                  "FAO note on Norway: Test note."))
  world <- suppressMessages(employment_summary(employment = employment_data(), plot = FALSE, table = FALSE))
  expect_equal(unique(world$yearly$country_en), "World")
  expect_error(employment_summary("Peru", employment = employment_data(), plot = FALSE, table = FALSE), "No employment data")
})

test_that("catch per fisher divides marine and inland catch by marine and inland fishers", {
  result <- suppressMessages(catch_per_fisher("Norway", data = fisher_catch(), employment = employment_data(),
                                              plot = FALSE, table = FALSE))
  y <- result$yearly[result$yearly$year == 2024, ]
  expect_equal(c(y$marine_fishers, y$inland_fishers, y$fishers), c(85, 30, 115))
  expect_equal(y$marine_catch_per_fisher, 10)
  expect_equal(y$inland_catch_per_fisher, 2)
  expect_equal(y$catch_per_fisher, 910 / 115)
  expect_equal(y$fish_farmers, 100)
  expect_equal(y$unspecified_pct, 100 * 10 / 225)
  expect_equal(y$fishers_estimated_pct, 100 * 5 / 115)
  expect_equal(y$fishers_time_unknown_pct, 100 * 5 / 115)
  expect_equal(result$yearly$fishers_unchanged, c(FALSE, TRUE, FALSE))

  fte <- suppressMessages(catch_per_fisher("Norway", weights = "fte", data = fisher_catch(), employment = employment_data(),
                                           plot = FALSE, table = FALSE))$yearly
  fte <- fte[fte$year == 2024, ]
  expect_equal(fte$marine_fishers, 60 + 20 * 60 / 95 + 5)
  expect_equal(fte$inland_fishers, 30 * 15 / 95)
  expect_equal(fte$fishers_headcount, 115)
  expect_equal(fte$marine_catch_per_fisher, 850 / (60 + 20 * 60 / 95 + 5))
  suppressMessages(expect_message(catch_per_fisher("Norway", data = fisher_catch(), employment = employment_data(),
                                                   plot = FALSE, table = FALSE), "FAO note on Norway"))
})

test_that("charts and tables are saved", {
  withr::local_dir(withr::local_tempdir())
  withr::local_options(faofishingreport.dpi = 72)
  suppressMessages(employment_summary("Norway", employment = employment_data(), lang = "both"))
  suppressMessages(catch_per_fisher("Norway", data = fisher_catch(), employment = employment_data(), lang = "zh"))
  expect_true(all(file.exists(file.path("figures/png/summary", c("Norway_employment.png", "Norway_employment_composition.png",
                                                                 "Norway_catch_per_fisher.png")))))
  expect_setequal(list.files("tables"), c("Norway_employment.csv", "Norway_catch_per_fisher.csv"))
})
