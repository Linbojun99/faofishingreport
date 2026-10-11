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
  expect_false(any(is.na(texts$en) | texts$en == ""))
  expect_false(any(is.na(texts$zh) | texts$zh == ""))
  expect_match(fao_report_text("report_no", "en", list(no = 2)), "Report 2")
})
