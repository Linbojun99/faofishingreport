test_that("bundled CMSY++ files match the recorded checksums", {
  sources <- cmsy_sources()
  bundled <- sources[sources$bundled, ]
  expect_setequal(bundled$file, c("CMSY++16.R", "ffnn.bin", "CMSYUserGuideMarch2021.pdf",
                                  "Train_Catch_9e.csv", "Train_ID_9e.csv"))
  for (i in seq_len(nrow(bundled))) {
    expect_true(cmsy_file_ok(cmsy_file(bundled$file[i]), bundled[i, ]), info = bundled$file[i])
  }
  expect_true(all(c("url", "md5", "sha256", "license", "citation") %in% names(sources)))
  expect_true(all(nzchar(sources$url)))
})

test_that("the operating system can be detected or set", {
  expect_true(cmsy_os() %in% c("windows", "macos", "linux"))
  expect_equal(cmsy_os("Mac"), "macos")
  expect_equal(cmsy_os("windows"), "windows")
  withr::with_options(list(faofishingreport.cmsy_os = "linux"), expect_equal(cmsy_os(), "linux"))
  expect_error(cmsy_os("solaris"), "must be")
})

test_that("the JAGS installer fits the system and R version", {
  expect_equal(cmsy_jags_choice("recommended", "windows", "4.5.1")$file, "JAGS-4.3.2.exe")
  expect_equal(cmsy_jags_choice("recommended", "windows", "4.3.2")$file, "JAGS-4.3.1.exe")
  expect_equal(cmsy_jags_choice("recommended", "macos", "4.6.0")$file, "JAGS-4.3.2.pkg")
  expect_equal(cmsy_jags_choice("recommended", "macos", "4.2.3")$file, "JAGS-4.3.1.pkg")
  expect_equal(cmsy_jags_choice("recommended", "linux", "4.6.0")$file, "JAGS-4.3.2.tar.gz")
  expect_equal(cmsy_jags_choice("latest", "macos", "4.6.0")$version, "5.0.0")
  expect_error(cmsy_jags_choice("5.0.0", "windows", "4.4.0"), "R 4.5 and 4.6")
  expect_equal(cmsy_rjags_choice("4.3.2", "macos", "arm64")$version, "4-17")
  expect_equal(cmsy_rjags_choice("5.0.0", "macos", "arm64")$file, "rjags_5-3-R4.6.1-arm64.tgz")
})

test_that("the CMSY++ code is changed for each system as the user guide says", {
  original <- readLines(cmsy_file("CMSY++16.R"), warn = FALSE)
  for (os in c("windows", "macos", "linux")) {
    code <- cmsy_patch_code(original, os)
    expect_false(any(grepl("windows(", code, fixed = TRUE) & !grepl("^\\s*#", code) &
                       !grepl("cmsy_open_device", code)), info = os)
    expect_false(any(grepl("antialias=\"cleartype\"", code, fixed = TRUE)), info = os)
    expect_false(any(grepl("^library\\(snpar\\)|snpar::runs.test|rstudioapi|install.packages", code)), info = os)
    expect_equal(sum(code == "source(\"cmsy_settings.R\")"), 1)
    expect_true(sum(grepl("cmsy_open_device(", code, fixed = TRUE)) >= 7)
    expect_silent(parse(text = code))
  }
  changes <- cmsy_changes("macos", "screen")
  expect_match(changes$what[changes$change == "Chart windows"], "quartz")
  expect_match(changes$what[changes$change == "JPEG antialias"], "default")
  expect_match(cmsy_changes("windows", "file")$what[6], "kept")
})

test_that("the settings file is valid R and its runs test matches snpar", {
  sys <- list(os = "macos", os_detected = "macos", device = "file", antialias = "default")
  code <- cmsy_settings_code("COD_27", sys, c("analysis", "kobe"), seed = 1)
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_equal(env$stocks, "COD_27")
  expect_true(env$kobe.plot && env$mgraphs && env$write.rdata)
  expect_equal(env$cmsy_antialias, "default")
  # Runs test by hand: x = c(1, 2, -1, -2, 3, -3), median 0 -> 1 1 0 0 1 0: 4 runs, m = n = 3
  x <- c(1, 2, -1, -2, 3, -3)
  z <- (4 - 2 * 3 * 3 / 6 - 1) / sqrt(2 * 3 * 3 * (2 * 3 * 3 - 6) / (36 * 5))
  expect_equal(env$cmsy_runs_test(x)$p.value, 2 * min(pnorm(z), 1 - pnorm(z)))
})

test_that("the priors table gives FishBase values with their links", {
  cod <- cmsy_priors("Gadus morhua", lang = "en")
  expect_equal(nrow(cod), 1)
  expect_equal(cod$SpecCode, 69)
  expect_equal(cod$resilience, "Medium")
  expect_equal(c(cod$r_low, cod$r_high), c(0.336, 0.764))
  expect_match(cod$url, "fishbase.se/summary/69$")
  expect_match(cod$r_source, "prior r")
  all <- cmsy_priors(lang = "en")
  expect_true(all(all$match_type %in% c("exact", "synonym", "ambiguous synonym", "not found", "not a species")))
  expect_true(all(!is.na(all$url)))
  expect_equal(cmsy_category_of_r(c(1.2, 0.5, 0.03)), c("High", "Medium", "Very low"))
})

test_that("catch series have consecutive years, with gaps interpolated", {
  s <- data.frame(species = "A", Year = c(1990, 1991, 1993, 1994, 1995), Tonnes = c(0, 10, 30, 0, 50))
  out <- cmsy_series(s, min_years = 3, min_catch = 0, max_filled = 1)
  expect_equal(out$catch$Year, 1991:1995)
  expect_equal(out$catch$Tonnes, c(10, 20, 30, 40, 50))
  expect_equal(out$meta$filled_years, 2)
  expect_equal(cmsy_ascii("Bay / Iberia: Sea"), "Bay - Iberia- Sea")
  expect_false(grepl("[^ -~]", cmsy_ascii("Pe\u00f1\u00edscola")))
})

test_that("cmsy_input() makes catch-only CMSY++ input files with prior sources", {
  data <- data.frame(
    Country..Name. = rep(c("Morocco", "Senegal"), each = 30),
    ASFIS.species..Name. = "European pilchard(=Sardine)",
    FAO.major.fishing.area..Name. = "Atlantic, Eastern Central",
    Unit..Name. = "Tonnes - live weight", Unit = "TLW", Year = rep(1990:2019, 2),
    Production = c(seq(1000, 30000, length.out = 30), rep(500, 30)), Production.Type = "actual",
    stringsAsFactors = FALSE
  )
  input <- cmsy_input(data, table = FALSE, lang = "en")
  expect_s3_class(input, "cmsy_input")
  expect_equal(input$id$Stock, "PIL_34")
  expect_equal(input$id$btype, "None")
  expect_true(all(is.na(input$catch$bt)))
  expect_equal(nrow(input$catch), 30)
  expect_equal(input$catch$ct[1], 1500)
  expect_equal(input$id$Resilience, "Medium")
  expect_match(input$stocks$r_source, "FishBase prior r")
  expect_false(grepl("[/:]", input$id$Comment))

  user <- cmsy_input(data, table = FALSE, lang = "en",
                     priors = data.frame(alpha3 = "PIL", endb.low = 0.2, endb.hi = 0.5, Resilience = "High"))
  expect_equal(c(user$id$endb.low, user$id$endb.hi), c(0.2, 0.5))
  expect_equal(user$stocks$r_source, "user (priors)")
  expect_error(cmsy_input(data, table = FALSE, priors = data.frame(alpha3 = "PIL", Resilience = "Very Low")),
               "Very low")

  by_country <- cmsy_input(data, by = "country", table = FALSE, lang = "en")
  expect_setequal(by_country$id$Stock, c("PIL_34_MAR", "PIL_34_SEN"))
})

test_that("cmsy_run() runs the bundled CMSY++ code", {
  skip_on_cran()
  skip_on_ci()
  sys <- cmsy_system(quiet = TRUE)
  skip_if(is.na(sys$jags) || !all(sys$packages$ok), "JAGS or CMSY++ packages not installed")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  data <- data.frame(
    Country..Name. = "Morocco", ASFIS.species..Name. = "European pilchard(=Sardine)",
    FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Unit..Name. = "Tonnes - live weight",
    Unit = "TLW", Year = 1980:2019, Production = 1e5 * (1 + sin(seq(0, 3, length.out = 40))),
    Production.Type = "actual", stringsAsFactors = FALSE
  )
  input <- cmsy_input(data, table = FALSE)
  result <- cmsy_run(input, seed = 1, parallel = 1, table = FALSE, plots = "analysis")
  expect_equal(result$summary$run_status, "ok")
  expect_true(result$summary$status %in% c("maximally sustainably fished", "overfished"))
  expect_true(file.exists(file.path(result$summary$folder, "PIL_34_AN.jpg")))
  expect_equal(nrow(result$series), 40)
  expect_true(all(c("CMSY++ code", "JAGS", "FishBase") %in% result$sources$component))
  expect_true(file.exists(file.path(result$run_dir, "code_changes.csv")))
})
