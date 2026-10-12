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

test_that("species without an FAO Chinese name get their FishBase name", {
  expect_equal(fao_translate("Pacific cod", "species", "zh"), "太平洋鳕")
  expect_equal(fao_translate("Pacific cod", "species", "both"), "太平洋鳕 Pacific cod")
  expect_equal(fao_translate("Pacific ocean perch", "species", "zh"), "Pacific ocean perch")
  fishbase <- fao_fishbase_names_zh()
  expect_equal(anyDuplicated(fishbase$name_en), 0)
  expect_true(all(grepl("^[一-鿿]+$", fishbase$name_zh)))
  # the names of the FAO lists come first
  official <- fao_name_table()
  official <- official[official$type == "species" & !official$name_en %in% fishbase$name_en, ]
  expect_equal(anyDuplicated(fao_name_table()[c("type", "name_en")]), 0)
  expect_gt(nrow(official), 0)
})

test_that("UTF-8 CSV files are read with their Chinese text in any locale", {
  file <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8("key,zh\ndepth,水深\n"))), file)
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_no_warning(x <- fao_read_csv_utf8(file, colClasses = "character"))
  expect_equal(names(x), c("key", "zh"))
  expect_equal(x$zh, "水深")
  expect_equal(Encoding(x$zh), "UTF-8")
  # the same file without the byte order mark
  writeBin(charToRaw(enc2utf8("key,zh\ndepth,水深\n")), file)
  expect_no_warning(y <- fao_read_csv_utf8(file, colClasses = "character"))
  expect_equal(y, x)
})
