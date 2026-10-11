test_that("the language follows lang, then ZH, then the settings", {
  withr::local_options(faofishingreport.lang = NULL)
  expect_equal(fao_lang(), "en")
  expect_equal(fao_lang(ZH = TRUE), "zh")
  expect_equal(fao_lang("both", ZH = TRUE), "both")
  withr::local_options(faofishingreport.lang = "zh")
  expect_equal(fao_lang(), "zh")
  expect_error(fao_lang("fr"))
})

test_that("chart labels exist in all languages", {
  expect_equal(fao_label("million_tonnes", "en"), "Million Tonnes")
  expect_equal(fao_label("million_tonnes", "zh"), "百万吨")
  expect_equal(fao_label("million_tonnes", "both"), "百万吨 (Million Tonnes)")
  expect_equal(fao_label("top_share_country", "en", n = 5), "Top 5 countries share (%)")
  expect_error(fao_label("no_such_label", "en"))
})

test_that("names are translated with official FAO names", {
  expect_equal(fao_translate(c("Morocco", "Spain"), "country", "zh"), c("摩洛哥", "西班牙"))
  expect_equal(fao_translate("Morocco", "country", "both"), "摩洛哥 Morocco")
  expect_equal(fao_translate("Morocco", "country", "en"), "Morocco")
  expect_equal(fao_translate("No such country", "country", "zh"), "No such country")
  expect_equal(fao_translate(c("Morocco", NA), "country", "zh")[2], NA_character_)
  expect_equal(fao_translate("Atlantic, Eastern Central", "area", "zh"), "中东大西洋")
})
