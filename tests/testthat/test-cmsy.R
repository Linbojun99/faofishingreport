# Tests that run CMSY++ need JAGS. They run locally (not on CRAN) and on CI when
# FAOFISHINGREPORT_CMSY_TESTS is "true" (.github/workflows/cmsy-cross-platform.yaml).
skip_if_no_cmsy <- function() {
  if (!identical(Sys.getenv("FAOFISHINGREPORT_CMSY_TESTS"), "true")) {
    skip_on_cran()
    skip_on_ci()
  }
  sys <- cmsy_system(silent = TRUE)
  skip_if(is.na(sys$jags) || !all(sys$packages$ok), "JAGS or CMSY++ packages not installed")
}

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
  expect_equal(cmsy_input(data, species = "Sardina pilchardus", table = FALSE)$id$Stock, "PIL_34")
  expect_error(cmsy_input(data, species = "Gadus morhua", table = FALSE), "No species")
  named <- cmsy_input(data, table = FALSE, priors = data.frame(species = "Sardina pilchardus", endb.low = 0.3, endb.hi = 0.7))
  expect_equal(named$id$endb.low, 0.3)
})

test_that("cmsy_run() runs the bundled CMSY++ code", {
  skip_if_no_cmsy()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  data <- data.frame(
    Country..Name. = "Morocco", ASFIS.species..Name. = "European pilchard(=Sardine)",
    FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Unit..Name. = "Tonnes - live weight",
    Unit = "TLW", Year = 1980:2019, Production = 1e5 * (1 + sin(seq(0, 3, length.out = 40))),
    Production.Type = "actual", stringsAsFactors = FALSE
  )
  input <- cmsy_input(data, table = FALSE)
  result <- cmsy_run(input, seed = 1, parallel = 1, table = FALSE, plots = "analysis", silent = TRUE)
  quick <- cmsy_run(input, seed = 1, parallel = 1, table = FALSE, plots = "analysis", silent = TRUE, out_dir = "quick",
                    settings = list(n.iter = 6000, n.burnin = 3000))
  expect_equal(quick$summary$run_status, "ok")
  expect_equal(quick$settings$used[quick$settings$setting == "n.iter"], "6000")
  expect_match(quick$sources$version[quick$sources$component == "CMSY++ settings"], "n.iter = 6000")
  expect_length(quick$details$PIL_34$r, 2 * (6000 - 3000) / 10)
  expect_equal(result$summary$run_status, "ok")
  expect_true(result$summary$status %in% c("maximally sustainably fished", "overfished"))
  expect_true(file.exists(file.path(result$summary$folder, "PIL_34_AN.jpg")))
  expect_equal(nrow(result$series), 40)
  expect_true(all(c("CMSY++ code", "JAGS", "FishBase") %in% result$sources$component))
  expect_true(file.exists(file.path(result$run_dir, "code_changes.csv")))
  expect_true(all(c("Biomass", "B_k", "Catch_pred", "F") %in% names(result$series)))
  expect_length(result$details$PIL_34$r, length(result$details$PIL_34$k))
  expect_true(all(c("start_bk_prior_low", "end_bk_prior_source", "url", "fao_data_url") %in% names(result$summary)))

  expect_false(any(result$settings$changed))
  expect_true(file.exists(file.path(result$run_dir, "settings_used.csv")))
  log <- readLines(file.path(result$summary$folder, "log.txt"))
  expect_true(any(grepl("Results for Management", log)))     # CMSY++ output reached the log
  plain <- cmsy_plot(result, charts = c("kobe", "prior_posterior", "management"), lang = "en", silent = TRUE)
  expect_null(plain$PIL_34_kobe$labels$title)
  expect_equal(plain$PIL_34_kobe$theme$legend.position, "none")
  annotated <- cmsy_plot(result, stocks = "Sardina pilchardus", charts = "kobe", annotations = TRUE, lang = "both",
                         silent = TRUE)
  expect_match(annotated$PIL_34_kobe$labels$title, "Kobe")
  projection <- cmsy_forecast(result, years = 3, seed = 1, plot = FALSE, table = FALSE, silent = TRUE)
  expect_setequal(unique(projection$projection$scenario), c("0.5 Fmsy", "0.6 Fmsy", "0.8 Fmsy", "0.95 Fmsy", "status_quo"))
  tables <- cmsy_tables(result, table = FALSE, lang = "en")
  expect_equal(nrow(tables$results), 1)
  expect_true("Last-year B/Bmsy (95% CI)" %in% names(tables$results))
  comparison <- cmsy_compare(a = result, b = result, silent = TRUE)
  expect_equal(nrow(comparison$last_year), 2)
  expect_true(file.exists(file.path("figures", "png", "cmsy", "PIL_34", "prior_posterior.png")))
})

test_that("species are found by English, Chinese or scientific name, code, or both names", {
  zh <- cmsy_species_lookup()$species_zh[cmsy_species_lookup()$alpha3 == "PIL"]
  expect_equal(cmsy_match_species("PIL"), "PIL")
  expect_equal(cmsy_match_species("european pilchard(=sardine)"), "PIL")
  expect_equal(cmsy_match_species("Sardina pilchardus"), "PIL")
  expect_equal(cmsy_match_species(zh), "PIL")
  expect_equal(cmsy_match_species(paste(zh, "European pilchard(=Sardine)")), "PIL")
  expect_equal(cmsy_match_species(paste0(zh, " (European pilchard(=Sardine))")), "PIL")
  expect_equal(cmsy_match_species(c("Gadus morhua", "YFT")), c("COD", "YFT"))
  expect_error(cmsy_match_species("sardine"), "matches")
  expect_error(cmsy_match_species("no such fish"), "No species")
  stocks <- data.frame(Stock = c("PIL_34", "PIL_27", "SAA_34"), alpha3 = c("PIL", "PIL", "SAA"))
  expect_setequal(cmsy_match_stocks(c("Sardina pilchardus", "SAA_34"), stocks), c("PIL_34", "PIL_27", "SAA_34"))
  cod <- cmsy_priors("COD", lang = "both")
  expect_match(cod$data_url, "fb/v26.06/parquet/estimate.parquet$")
  expect_match(cod$rule_url, "oceanrep")
  expect_true(all(c("species_en", "species_zh") %in% names(cod)))
})

test_that("chart annotations, equilibrium curve and Mohn's rho", {
  expect_equal(cmsy_annotations(FALSE), character(0))
  expect_equal(cmsy_annotations(TRUE), cmsy_annotation_types)
  expect_equal(cmsy_annotations(c("title", "legend")), c("title", "legend"))
  expect_error(cmsy_annotations("colours"))
  expect_equal(cmsy_equilibrium_curve(c(0, 0.5, 1)), c(0, 1, 0))
  expect_equal(cmsy_equilibrium_curve(0.25), 0.75)
  peels <- data.frame(peel = factor(rep(c("All", "2019", "2018"), c(3, 2, 1)), levels = c("All", "2019", "2018")),
                      Year = c(2018, 2019, 2020, 2018, 2019, 2018), value = c(1, 1, 1, 1.2, 1.1, 0.9))
  expect_equal(cmsy_mohns_rho(peels), mean(c(0.1, -0.1)))
})

test_that("projections follow the CMSY++ model, status classes and tables", {
  # At B = Bmsy (B/k = 0.5) and F = Fmsy, the stock stays at Bmsy without process error
  out <- cmsy_project_stock(r = rep(0.4, 5), k = rep(1000, 5), bk = rep(0.5, 5), catch_last = 100, last_year = 2020,
                            years = 5, f = 1, status_quo = FALSE, catch = NULL, sigma = 0)
  s <- out$summary
  expect_equal(s$B_Bmsy, rep(1, 6), tolerance = 1e-8)
  expect_equal(s$Catch[-1], rep(100, 5), tolerance = 1e-8)
  expect_equal(s$Year, 2020:2025)
  # Below B/k = 0.25 productivity is reduced, as in CMSY++
  low <- cmsy_project_stock(r = 0.4, k = 1000, bk = 0.2, catch_last = 0, last_year = 2020, years = 1, f = 0,
                            status_quo = FALSE, catch = NULL, sigma = 0)$summary
  expect_equal(low$B_Bmsy[2] / 2, 0.2 + 4 * 0.2 * 0.4 * 0.2 * 0.8)
  expect_equal(as.character(cmsy_status_class(c(1.1, 0.9, 0.6, 0.3, 0.1), "palomares", "en")),
               c("Healthy", "Slightly overfished", "Overfished", "Grossly overfished", "Collapsed"))
  expect_equal(as.character(cmsy_status_class(c(0.79, 0.8), "fao", "en")),
               c("Overfished", "Maximally sustainably fished"))
  expect_equal(cmsy_scenario_label(c("0.8 Fmsy", "status_quo", "catch_100000"), 2024, "en"),
               c("0.8 Fmsy", "Status quo F (2024)", "Constant catch 100,000 t"))
})

test_that("progress bars print only when not silent, and auto start keeps ten years", {
  expect_silent(cmsy_progress(3, "Test", silent = TRUE)$update(1))
  expect_output({b <- cmsy_progress(2, "Test", silent = FALSE); b$update(2, "x"); b$close()}, "2/2")
  catch <- data.frame(yr = 2000:2015, ct = c(rep(10, 13), 1000, 1000, 1000))
  id <- data.frame(StartYear = 2000, EndYear = 2015, stb.low = NA)
  expect_equal(cmsy_auto_start(catch, id), 2000)
  catch$ct <- c(rep(10, 3), rep(1000, 13))
  expect_equal(cmsy_auto_start(catch, id), cmsy_auto_start(catch, id))
  expect_true(cmsy_auto_start(catch, id) <= 2006)
})

test_that("cmsy_area() runs the main species of an area into sorted folders", {
  skip_if_no_cmsy()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  withr::local_options(faofishingreport.dpi = 72)
  data <- data.frame(
    Country..Name. = "Morocco", ASFIS.species..Name. = c(rep("European pilchard(=Sardine)", 40), rep("Bigeye grunt", 40)),
    FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Unit..Name. = "Tonnes - live weight",
    Unit = "TLW", Year = rep(1980:2019, 2), Production = c(1e5 * (1 + sin(seq(0, 3, length.out = 40))), seq(1e4, 5e4, length.out = 40)),
    Production.Type = "actual", stringsAsFactors = FALSE
  )
  result <- cmsy_area(data, top = 1, out_dir = "areas", charts = c("kobe", "overview"), forecast = FALSE, silent = TRUE)
  expect_equal(result$summary$Stock, "PIL_34")
  expect_true(all(dir.exists(file.path(result$dir, c("input", "runs", "figures", "tables")))))
  expect_true(file.exists(file.path(result$dir, "figures", "PIL_34", "kobe.png")))
  expect_true(file.exists(file.path(result$dir, "tables", "results_table.csv")))
  expect_true(file.exists(file.path(result$dir, "runs", "PIL_34", "cmsy_output.csv")))
})

test_that("CMSY++ settings keep their defaults unless changed, and are checked", {
  d <- cmsy_defaults(lang = "en")
  expect_equal(nrow(d), length(cmsy_default_settings))
  expect_false(any(d$changed))
  expect_equal(d$cmsy_default[d$setting == "n.iter"], "60000")
  expect_equal(d$cmsy_default[d$setting == "rk.cor.beta"], "2.52, 3.37")
  changed <- cmsy_defaults(list(sigmaR = 0.2, n.chains = 3L), lang = "zh")
  expect_equal(changed$setting[changed$changed], c("sigmaR", "n.chains"))
  expect_error(cmsy_check_settings(list(sigma = 0.2)), "Unknown")
  expect_error(cmsy_check_settings(list(retros = TRUE)), "arguments of cmsy_run")
  expect_error(cmsy_check_settings(list(nbk = 4)), "nbk")
  expect_error(cmsy_check_settings(list(n.burnin = 70000)), "smaller than n.iter")
  expect_error(cmsy_check_settings(list(rk.cor.beta = 2)), "2 numbers")
  expect_error(cmsy_check_settings(list(bt4pr = "yes")), "TRUE or FALSE")
  expect_error(cmsy_check_settings(list(0.2)), "named list")
  # The settings file sets them, and the changed code uses the MCMC settings
  sys <- list(os = "windows", os_detected = "macos", device = "file", antialias = "cleartype")
  env <- new.env()
  eval(parse(text = cmsy_settings_code("COD_27", sys, "analysis", 1, cmsy_check_settings(list(n.iter = 5000, n.burnin = 2000)))),
       envir = env)
  expect_equal(c(env$cmsy_n_iter, env$cmsy_n_burnin, env$cmsy_n_thin, env$CV.C, env$n.chains), c(5000, 2000, 10, 0.15, 2))
  expect_equal(env$rk.cor.beta, c(2.52, 3.37))
  expect_true(is.na(env$select.yr))
  original <- readLines(cmsy_file("CMSY++16.R"), warn = FALSE)
  expect_true(any(grepl("n.iter = 60000)", cmsy_patch_code(original, "windows"), fixed = TRUE)))
  code <- cmsy_patch_code(original, "windows", cmsy_check_settings(list(n.iter = 100000, n.burnin = 50000, n.thin = 20)))
  expect_true(any(grepl("n.iter = 100000)", code, fixed = TRUE)))
  expect_true(any(grepl("n.burnin = 50000, n.thin = 20,", code, fixed = TRUE)))
})
