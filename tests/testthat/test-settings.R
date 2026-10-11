test_that("settings are set, validated and reset", {
  withr::defer(fao_settings(reset = TRUE))
  fao_settings(theme = "classic", palette = "lancet", map_center = "0-360")
  expect_equal(fao_setting("theme"), "classic")
  expect_equal(fao_map_center(), 180)
  expect_error(fao_settings(palette = "no such palette"))
  expect_error(fao_settings(map_center = 90))
  expect_error(fao_settings(map_projection = "no such projection"))
  expect_error(fao_settings(colour = "red"))
  fao_settings(reset = TRUE)
  expect_equal(fao_setting("theme"), "bw")
})

test_that("palettes give the number of colours asked for", {
  expect_length(fao_palette(3, "npg"), 3)
  expect_length(fao_palette(25, "npg"), 25)
  expect_length(fao_palette(4, "Set2"), 4)
  expect_length(fao_palette(4, "Okabe-Ito"), 4)
  expect_equal(fao_palette(2, c("red", "blue")), c("red", "blue"))
})

test_that("fonts are chosen for the operating system", {
  expect_equal(fao_detect_font("Windows", c("Arial", "SimSun")), "SimSun")
  expect_equal(fao_detect_font("Windows", c("Arial", "Microsoft YaHei")), "Microsoft YaHei")
  expect_equal(fao_detect_font("Windows", c("Arial")), "sans")
  expect_equal(fao_detect_font("Linux", c("Noto Sans CJK SC")), "Noto Sans CJK SC")
})
