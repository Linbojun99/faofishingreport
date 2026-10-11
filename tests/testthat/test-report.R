test_that("report numbers are Chinese numerals in Chinese reports", {
  expect_identical(fao_report_number(1, "zh"), "一")
  expect_identical(fao_report_number(12, "zh"), "十二")
  expect_identical(fao_report_number(20, "zh"), "二十")
  expect_identical(fao_report_number(3, "en"), "3")
})

test_that("Chinese text has no spaces next to Chinese characters", {
  expect_identical(fao_report_zh_spacing("2024 年 509.9 万 t"), "2024年509.9万t")
  expect_identical(fao_report_zh_spacing("1.1 数据", numbered = TRUE), "1.1 数据")
  expect_identical(fao_report_zh_spacing("图1-2 渔获量", numbered = TRUE), "图1-2 渔获量")
  expect_identical(fao_report_zh_spacing("Mann-Kendall test"), "Mann-Kendall test")
})

test_that("references follow GB/T 7714, APA and FAO styles", {
  ref <- list(type = "J", author = "Kleisner, K.; Zeller, D.; Froese, R.; Pauly, D.", year = "2013",
              title = "Using global catch data", container = "Fish and Fisheries", volume = "14",
              issue = "3", pages = "293-311")
  expect_identical(fao_ref_entry(ref, "numeric"),
                   "Kleisner K, Zeller D, Froese R, et al. Using global catch data[J]. Fish and Fisheries, 2013, 14(3): 293-311.")
  expect_match(fao_ref_entry(ref, "apa"), "^Kleisner, K., Zeller, D., Froese, R., & Pauly, D. \\(2013\\)")
  expect_match(fao_ref_entry(ref, "fao"), "^Kleisner, K., Zeller, D., Froese, R. & Pauly, D. 2013.")
  expect_identical(fao_ref_short("Bai, J.; Perron, P.", "apa"), "Bai & Perron")
  expect_identical(fao_ref_short(ref$author, "apa"), "Kleisner et al.")
})

test_that("report text templates exist in both languages", {
  texts <- fao_extdata_csv("report_text.csv")
  texts <- texts[texts$key != "abstract_results", ]   # "Results show that" has no English sentence
  expect_false(any(is.na(texts$en) | texts$en == ""))
  expect_false(any(is.na(texts$zh) | texts$zh == ""))
  expect_match(fao_report_text("report_no", "en", list(no = 2)), "Report 2")
})

test_that("proceedings and databases with editors are formatted", {
  jags <- list(type = "C", author = "Plummer, M.", year = "2003", title = "JAGS",
               container = "Proceedings of DSC 2003", place = "Vienna", pages = "1-10")
  expect_identical(fao_ref_entry(jags, "numeric"), "Plummer M. JAGS: Proceedings of DSC 2003[C]. Vienna, 2003, 1-10.")
  expect_match(fao_ref_entry(jags, "apa"), "In \\*Proceedings of DSC 2003\\* \\(pp. 1–10\\)")
  fishbase <- list(type = "DB", author = "Froese, R.; Pauly, D.", editors = TRUE, year = "2026",
                   title = "FishBase", version = "version 06/2026", accessed = "2026-10-10",
                   url = "https://www.fishbase.se")
  expect_match(fao_ref_entry(fishbase, "apa"), "^Froese, R., & Pauly, D. \\(Eds.\\). \\(2026\\). \\*FishBase\\* \\(Version 06/2026\\)")
  expect_match(fao_ref_entry(fishbase, "fao"), "^Froese, R. & Pauly, D., eds. 2026. FishBase.")
})

test_that("catch ranges are written in one unit", {
  expect_identical(fao_report_catch_range(775500, 1076000, "en"), "0.78–1.08 million t")
  expect_identical(fao_report_catch_range(48200, 82400, "en"), "48.2–82.4 thousand t")
  expect_identical(fao_report_catch_range(5618000, 9915000, "zh"), "561.8–991.5 万 t")
})

test_that("settings of the analyses are passed by section prefix", {
  sections <- fao_report_sections()
  args <- fao_report_args(list(species_h_value = 0.2, status_smooth = TRUE, cmsy_top = 5), sections)
  expect_identical(args$species$h_value, 0.2)
  expect_true(args$status$smooth)
  expect_identical(args$cmsy$top, 5)
  expect_error(fao_report_args(list(species_hvalue = 0.2), sections), "not an argument of fishing_area_species")
  expect_error(fao_report_args(list(spcies_h_value = 0.2), sections), "Unknown argument")
  expect_error(fao_report_args(list(0.2), sections), "must be named")
})

test_that("new sections take their settings and extra settings", {
  sections <- fao_report_sections()
  expect_true(all(c("shocks", "collapse", "synchrony", "pelagic", "mtc", "distant") %in% names(sections)))
  args <- fao_report_args(list(collapse_method = "worm2006", collapse_seed = 2, distant_countries = 5,
                               mtc_version = "builtin", pelagic_definition = "grainger_garcia"), sections)
  expect_identical(args$collapse$method, "worm2006")
  expect_identical(args$collapse$seed, 2)
  expect_identical(args$distant$countries, 5)
  expect_error(fao_report_args(list(shocks_seed = 1), sections), "not an argument of catch_shocks")
  # every method of a section has its paragraph
  methods <- unique(unlist(lapply(sections, `[[`, "methods")))
  expect_true(all(methods %in% names(fao_report_methods)))
  texts <- fao_extdata_csv("report_text.csv")
  expect_true(all(fao_report_methods %in% texts$key))
  expect_true(all(paste0("topic_", names(sections)) %in% texts$key))
  expect_true(all(paste0("method_", methods) %in% texts$key))
})
