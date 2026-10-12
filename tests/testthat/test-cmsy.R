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
  expect_error(cmsy_jags_choice("5.0.0", "windows", "4.4.0"), "No JAGS 5.0.0 build")
  expect_error(cmsy_jags_choice("recommended", "windows", "4.7.0"), "No build for R 4.7.0")
  expect_equal(cmsy_jags_choice("recommended", "windows", "4.2.3")$file, "JAGS-4.3.1.exe")
  expect_equal(cmsy_jags_choice("latest", "windows", "4.6.1")$file, "JAGS-5.0.0.exe")
  expect_equal(cmsy_rjags_choice("4.3.2", "macos", "arm64")$version, "4-17")
  expect_equal(cmsy_rjags_choice("5.0.0", "macos", "arm64", "4.6.1")$file, "rjags_5-3-R4.6.1-arm64.tgz")
  expect_equal(cmsy_rjags_choice("5.0.0", "macos", "x86_64", "4.6.0")$file, "rjags_5-3-R4.6.1-x86_64.tgz")
  expect_equal(cmsy_rjags_choice("5.0.0", "macos", "arm64", "4.4.0")$file, "rjags_5-3.tar.gz")
  expect_equal(cmsy_rjags_choice("5.0.0", "windows", "x86_64", "4.5.2")$file, "rjags_5-3.zip")
  # Every installer named in the table is in the registry, with its checksums
  registry <- cmsy_sources("JAGS")
  expect_true(all(cmsy_jags_compat$installer %in% registry$file))
  expect_true(all(!is.na(registry$sha256[registry$file %in% cmsy_jags_compat$installer])))
  table <- cmsy_jags_versions(silent = TRUE)
  expect_equal(sum(table$this_computer), 1)
  expect_setequal(unique(table$system), c("Windows", "macOS", "Linux"))
  expect_equal(cmsy_jags_versions("windows", r_version = "4.4.1", silent = TRUE)$installer[1:2],
               c("JAGS-4.3.2.exe", "JAGS-4.3.1.exe"))
  # Version text of JAGS builds, as rjags reports it
  expect_equal(cmsy_jags_number("5.0.0 (vecLib-gcd-aarch64)"), "5.0.0")
  expect_equal(cmsy_jags_number("5.0.0-beta"), "5.0.0")
  expect_equal(cmsy_jags_number(" 4.3.2 "), "4.3.2")
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
  # a resilience without r.low and r.hi replaces the FishBase r range by the range of that resilience
  expect_match(user$stocks$r_source, "^user \\(priors\\): resilience \"High\"")
  expect_true(is.na(user$id$r.low) && is.na(user$id$r.hi))
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
  # the draws of each chain are kept for cmsy_mcmc(), with the settings of the run
  expect_equal(dim(quick$details$PIL_34$mcmc$draws)[1:2], c((6000 - 3000) / 10, 2))
  expect_equal(unname(quick$details$PIL_34$mcmc$settings["n.iter"]), 6000)
  diagnostics <- cmsy_mcmc(quick, plot = FALSE, table = FALSE, silent = TRUE)$diagnostics
  expect_true(all(c("r", "k", "msy", "bk_last") %in% diagnostics$parameter))
  expect_equal(diagnostics$median[diagnostics$parameter == "msy"], quick$summary$MSY, tolerance = 0.05)
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
  # a run that ended with an error after the main analysis keeps its results, as "partial"
  expect_equal(result$summary$run_status, "ok")
  partial <- cmsy_read_stock(result$summary$folder, "PIL_34", list(exit = "error: retrospective failed", seconds = 1))
  expect_equal(partial$summary$run_status, "partial")
  expect_match(partial$summary$message, "retrospective failed")
  expect_false(is.null(partial$details))
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
  changed <- cmsy_defaults(list(CV.C = 0.2, n.chains = 3L), lang = "zh")
  expect_equal(changed$setting[changed$changed], c("CV.C", "n.chains"))
  # sigmaR and ni are set in CMSY++16.R but not used by it: they cannot be changed
  expect_equal(d$applies_to[d$setting %in% c("sigmaR", "ni")], c("not used", "not used"))
  expect_error(cmsy_check_settings(list(sigmaR = 0.5)), "does not use it")
  expect_error(cmsy_check_settings(list(ni = 5)), "does not use it")
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


test_that("projections follow the CMSY++ timing, and F/Fmsy is the scenario value", {
  # r = 0.4, k = 1000, B/k = 0.5, no process error: Fmsy = 0.2, the last year keeps its catch (100)
  z <- cmsy_project_stock(rep(0.4, 10), rep(1000, 10), rep(0.5, 10), 100, 2024, 2, 0.5, FALSE, NULL, 0)$summary
  expect_equal(z$Year, 2024:2026)
  expect_equal(z$B_Bmsy, c(1, 1, 1.1))       # 2025: 500 + 100 - 100; 2026: 500 + 100 - 50
  expect_equal(z$Catch, c(100, 50, 55))      # 0.5 Fmsy = 0.1 times the biomass of the same year
  expect_equal(z$F_Fmsy, c(1, 0.5, 0.5))
  # below half of Bmsy, Fmsy is reduced linearly as in CMSY++16.R, and F/Fmsy stays the scenario
  low <- cmsy_project_stock(rep(0.4, 10), rep(1000, 10), rep(0.2, 10), 50, 2024, 2, 0.5, FALSE, NULL, 0)$summary
  expect_equal(low$F_Fmsy, c((50 / 200) / (0.2 * 0.8), 0.5, 0.5))
  sq <- cmsy_project_stock(rep(0.4, 10), rep(1000, 10), rep(0.5, 10), 100, 2024, 2, NULL, TRUE, 300, 0)$summary
  expect_equal(sq$Catch[sq$scenario == "status_quo"], c(100, 100, 100))
  expect_equal(sq$Catch[sq$scenario == "catch_300"][2], 300)
})

test_that("stocks whose data end in different years are projected to the same year", {
  stock <- list(r = rep(0.4, 10), k = rep(1000, 10), bk_last = rep(0.5, 10))
  result <- structure(list(summary = data.frame(Stock = c("A", "B"), species_en = "Atlantic cod", run_status = "ok",
                                                catch_last = 100, last_year = c(2024, 2023)),
                           details = list(A = stock, B = stock)), class = "cmsy_result")
  expect_message(out <- cmsy_forecast(result, years = 2, f = 0.5, sigma = 0, plot = FALSE, table = FALSE, lang = "en",
                                      silent = FALSE), "end in different years")
  expect_equal(vapply(split(out$projection$Year, out$projection$Stock), max, numeric(1)), c(A = 2026, B = 2026))
  expect_equal(unique(out$share$Year), 2024:2026)
  expect_true(all(out$share$n_stocks == 2))
  expect_match(out$projection$scenario_label[out$projection$Stock == "B" & out$projection$scenario == "status_quo"][1], "2023")
  expect_match(out$share$scenario_label[out$share$scenario == "status_quo"][1], "each stock")
})

test_that("a prior row applies to the stocks of its most specific identifier", {
  meta <- data.frame(Stock = c("PIL_34", "PIL_27"), alpha3 = "PIL", ASFIS.species..Name. = "European pilchard(=Sardine)",
                     endb.low = NA_real_, Resilience = "Medium", r.low = 0.4, r.hi = 0.9, r_source = "FishBase",
                     resilience_source = "FishBase", prior_source = "default", stringsAsFactors = FALSE)
  out <- cmsy_apply_priors(meta, data.frame(Stock = "PIL_34", alpha3 = "PIL", endb.low = 0.2))
  expect_equal(out$endb.low, c(0.2, NA))
  # rows for the species first, then rows for one stock, whatever their order
  out <- cmsy_apply_priors(meta, data.frame(Stock = c("PIL_27", NA), alpha3 = c(NA, "PIL"), endb.low = c(0.3, 0.1)))
  expect_equal(out$endb.low, c(0.1, 0.3))
  expect_warning(cmsy_apply_priors(meta, data.frame(Stock = "PIL_34", alpha3 = "ANE", endb.low = 0.2)), "does not match")
  # a resilience alone replaces the r range; with r.low and r.hi, those are used
  out <- cmsy_apply_priors(meta, data.frame(alpha3 = "PIL", Resilience = "Very low"))
  expect_true(all(is.na(out$r.low) & is.na(out$r.hi)))
  expect_equal(out$Resilience, c("Very low", "Very low"))
  out <- cmsy_apply_priors(meta, data.frame(alpha3 = "PIL", Resilience = "Low", r.low = 0.1, r.hi = 0.3))
  expect_equal(c(out$r.low, out$r.hi), c(0.1, 0.1, 0.3, 0.3))
  expect_equal(out$r_source, c("user (priors)", "user (priors)"))
})

test_that("cmsy_input() returns the excluded stocks when no stock is ready", {
  data <- data.frame(Country..Name. = "Morocco", ASFIS.species..Name. = "European pilchard(=Sardine)",
                     FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Unit..Name. = "Tonnes - live weight",
                     Unit = "TLW", Year = 1990:2019, Production = 1000, Production.Type = "actual", stringsAsFactors = FALSE)
  expect_warning(input <- cmsy_input(data, table = FALSE, min_catch = 1e9, silent = TRUE), "No stock is ready")
  expect_equal(nrow(input$id), 0)
  expect_equal(nrow(input$catch), 0)
  expect_true("Resilience" %in% names(input$id))
  expect_equal(nrow(input$excluded), 1)
  expect_match(input$excluded$reason, "total catch below")
  # the FAO release recorded with the data is the one used
  attr(data, "fao_version") <- "2000.1.0"
  expect_equal(cmsy_input(data, table = FALSE, silent = TRUE)$fao_version, "2000.1.0")
})

test_that("cmsy_area() keeps the results of an earlier run when the input is wrong", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  data <- data.frame(Country..Name. = "Morocco", ASFIS.species..Name. = "European pilchard(=Sardine)",
                     FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Unit..Name. = "Tonnes - live weight",
                     Unit = "TLW", Year = 1990:2019, Production = 1000, Production.Type = "actual", stringsAsFactors = FALSE)
  earlier <- file.path("areas", fao_file_name(fao_summary_label(data, NULL)), "tables")
  dir.create(earlier, recursive = TRUE)
  writeLines("earlier", file.path(earlier, "results.csv"))
  expect_error(cmsy_area(data, species = "Gadus morhua", out_dir = "areas", silent = TRUE), "No species")
  expect_error(suppressWarnings(cmsy_area(data, out_dir = "areas", silent = TRUE, min_catch = 1e9)), "No stock")
  expect_equal(readLines(file.path(earlier, "results.csv")), "earlier")
})

test_that("the report only reuses CMSY++ results of its area and FAO release", {
  result <- structure(list(summary = data.frame(Stock = "PIL_34", area = "Atlantic, Eastern Central",
                                                fao_data_url = "https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip"),
                           input = list(fao_version = "2026.1.0")), class = "cmsy_result")
  expect_true(fao_report_check_cmsy(result, "Atlantic, Eastern Central", "2026.1.0"))
  expect_error(fao_report_check_cmsy(result, "Pacific, Northeast", "2026.1.0"), "not of Pacific, Northeast")
  expect_error(fao_report_check_cmsy(result, "Atlantic, Eastern Central", "2027.1.0"), "release 2026.1.0")
  result$input <- NULL                          # results of cmsy_run(): the release is in the links
  expect_error(fao_report_check_cmsy(result, "Atlantic, Eastern Central", "2027.1.0"), "release 2026.1.0")
})


test_that("runs without results are failed or timed out, with how they ended", {
  dir <- withr::local_tempdir()
  failed <- cmsy_read_stock(dir, "A", list(exit = "error: no viable r-k pairs", seconds = 3))
  expect_equal(failed$summary$run_status, "failed")
  expect_equal(failed$summary$exit, "error: no viable r-k pairs")
  timeout <- cmsy_read_stock(dir, "A", list(exit = "timeout after 60 s", seconds = 60))
  expect_equal(timeout$summary$run_status, "timeout")
  expect_equal(cmsy_has_results(c("ok", "partial", "failed", "timeout")), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("B/k and r priors are checked as pairs, with valid ranges and years", {
  id <- data.frame(Stock = c("A", "B"), StartYear = 1980, EndYear = 2020, r.low = NA, r.hi = NA, stb.low = NA,
                   stb.hi = NA, int.yr = NA, intb.low = NA, intb.hi = NA, endb.low = NA, endb.hi = NA)
  expect_true(cmsy_check_priors(id))
  half <- id
  half$endb.low[1] <- 0.12
  expect_error(cmsy_check_priors(half), "endb.low and endb.hi must be given together \\(A\\)")
  wrong <- id
  wrong$stb.low[2] <- 0.6
  wrong$stb.hi[2] <- 0.4
  expect_error(cmsy_check_priors(wrong), "between 0 and 1")
  no_year <- id
  no_year$intb.low[1] <- 0.2
  no_year$intb.hi[1] <- 0.4
  expect_error(cmsy_check_priors(no_year), "need int.yr")
  no_year$int.yr[1] <- 2030
  expect_error(cmsy_check_priors(no_year), "between StartYear and EndYear")
  data <- data.frame(Country..Name. = "Morocco", ASFIS.species..Name. = "European pilchard(=Sardine)",
                     FAO.major.fishing.area..Name. = "Atlantic, Eastern Central", Unit..Name. = "Tonnes - live weight",
                     Unit = "TLW", Year = 1990:2019, Production = 1000, Production.Type = "actual", stringsAsFactors = FALSE)
  expect_error(cmsy_input(data, table = FALSE, silent = TRUE, priors = data.frame(alpha3 = "PIL", endb.low = 0.12)),
               "must be given together")
})

test_that("stocks of countries without an ISO3 code keep their own identifiers", {
  expect_equal(cmsy_stock_id("COD", "27", c(NA, NA, "NOR"), c("280", NA, "578"), c("Germany, Fed. Rep. of", "Some Land", "Norway")),
               c("COD_27_UN280", "COD_27_SOMELAND", "COD_27_NOR"))
  data <- data.frame(Country..Name. = rep(c("Germany, Fed. Rep. of", "Germany,New Länder"), each = 30),
                     ASFIS.species..Name. = "Atlantic cod", FAO.major.fishing.area..Name. = "Atlantic, Northeast",
                     Unit..Name. = "Tonnes - live weight", Unit = "TLW", Year = rep(1961:1990, 2), Production = 5000,
                     Production.Type = "actual", stringsAsFactors = FALSE)
  input <- cmsy_input(data, by = "country", table = FALSE, silent = TRUE)
  expect_equal(anyDuplicated(input$id$Stock), 0)
  expect_equal(anyDuplicated(paste(input$catch$Stock, input$catch$yr)), 0)
  expect_false(any(grepl("XXX", input$id$Stock)))
  twice <- input$id[c(1, 1), ]
  expect_error(cmsy_check_priors(twice), "used more than once")
  expect_error(cmsy_check_priors(input$id, input$catch[c(1, 1), ]), "more than one catch in a year")
})

test_that("the start year suggested by CMSY++ uses the smoothing bandwidth of the run", {
  years <- 1970:2010
  catch <- data.frame(Stock = "A", yr = years, ct = c(rep(10, 15), rep(1000, 26)))
  id <- data.frame(Stock = "A", StartYear = 1970, EndYear = 2010, stb.low = NA)
  expected <- function(bw) {
    smooth <- stats::ksmooth(years, catch$ct / 1000, kernel = "normal", n.points = length(years), bandwidth = bw)$y
    years[which(smooth >= 0.4 * max(smooth))][1]
  }
  expect_equal(cmsy_auto_start(catch, id, bw = 3), expected(3))
  expect_equal(cmsy_auto_start(catch, id, bw = 10), expected(10))
  expect_false(expected(3) == expected(10))
})

test_that("the report says when results are completed runs with unchecked B/k priors", {
  s <- data.frame(Stock = c("A", "B"), start_bk_prior_source = c("CMSY++ neural network", "user"),
                  int_bk_prior_source = c("CMSY++ neural network", "user"),
                  end_bk_prior_source = c("CMSY++ neural network", "user"))
  mixed <- fao_report_cmsy_priors(s, c(TRUE, TRUE), "en")
  expect_equal(mixed$unchecked, c(TRUE, FALSE))
  expect_match(mixed$text, "priors of 1 stock were set by the user; for the other 1 stock they were")
  expect_match(mixed$text, "not of validated stock assessments")
  nn <- fao_report_cmsy_priors(s[1, ], TRUE, "en")
  expect_match(nn$text, "completed model runs, not of validated stock assessments")
  expect_match(fao_report_cmsy_priors(s[1, ], TRUE, "zh")$text, "核实")
  user <- fao_report_cmsy_priors(s[2, ], TRUE, "en")
  expect_false(user$unchecked)
  expect_equal(user$text, "The relative biomass (B/k) priors of all stocks were set by the user.")
  # a stock with one prior of the neural network is unchecked; failed runs are left out
  s$end_bk_prior_source[2] <- "CMSY++ neural network"
  expect_equal(fao_report_cmsy_priors(s, c(TRUE, TRUE), "en")$unchecked, c(TRUE, TRUE))
  expect_equal(fao_report_cmsy_priors(s, c(FALSE, TRUE), "en")$unchecked, c(FALSE, TRUE))
})

test_that("cmsy_mcmc() gives Rhat and effective sample sizes, and tells convergence from too few draws", {
  set.seed(1)
  n <- 1000
  ar1 <- function(rho, mean) as.numeric(stats::arima.sim(list(ar = rho), n)) * 0.01 + mean
  make <- function(shift, rho) {
    draws <- array(NA_real_, dim = c(n, 2, 4), dimnames = list(NULL, NULL, c("r", "k", "bk_last", "msy")))
    draws[, 1, "r"] <- ar1(rho, 0.5)
    draws[, 2, "r"] <- ar1(rho, 0.5 + shift)
    draws[, , "k"] <- 1e6 + stats::rnorm(2 * n, 0, 1e4)
    draws[, , "bk_last"] <- stats::rbeta(2 * n, 20, 20)
    draws[, , "msy"] <- draws[, , "r"] * draws[, , "k"] / 4
    list(settings = c(n.chains = 2, n.iter = 20000, n.burnin = 10000, n.thin = 10), draws = draws)
  }
  summary <- data.frame(Stock = c("GOOD", "STUCK"), species_en = "Atlantic cod", run_status = "ok", stringsAsFactors = FALSE)
  result <- structure(list(summary = summary, details = list(GOOD = list(mcmc = make(0, 0.2)), STUCK = list(mcmc = make(0.2, 0.99)))),
                      class = "cmsy_result")
  out <- cmsy_mcmc(result, plot = FALSE, table = FALSE, silent = TRUE)
  expect_equal(out$settings$n_kept, c(n, n))
  expect_equal(out$settings$n_draws, c(2 * n, 2 * n))
  good <- out$diagnostics[out$diagnostics$Stock == "GOOD" & out$diagnostics$parameter == "r", ]
  stuck <- out$diagnostics[out$diagnostics$Stock == "STUCK" & out$diagnostics$parameter == "r", ]
  expect_lt(good$rhat, 1.05)
  expect_true(good$converged && good$enough_draws)
  expect_gt(stuck$rhat, 1.1)
  expect_false(stuck$converged)
  expect_lt(stuck$n_eff, 400)
  expect_message(cmsy_mcmc(result, plot = FALSE, table = FALSE, silent = FALSE), "not converged \\(Rhat > 1.1\\): STUCK")
  # results of earlier versions have no draws
  old <- result
  old$details <- list(GOOD = list(), STUCK = list())
  expect_error(cmsy_mcmc(old, plot = FALSE, table = FALSE), "earlier version")
  skip_if_not_installed("patchwork")
  expect_s3_class(cmsy_chart_mcmc(result$details$GOOD, "zh", character(0)), "patchwork")
})

test_that("the MCMC draws saved by the run are read with k in tonnes and MSY added", {
  draws <- array(c(rep(0.4, 6), rep(1000, 6), rep(0.5, 6), rep(0.3, 6), rep(-10, 6)), dim = c(3, 2, 5),
                 dimnames = list(NULL, NULL, c("r", "k", "P[1]", "P[40]", "deviance")))
  out <- cmsy_read_mcmc(list(settings = c(n.chains = 2, n.iter = 60000, n.burnin = 30000, n.thin = 10), draws = draws))
  expect_equal(dimnames(out$draws)[[3]], c("r", "k", "bk_first", "bk_last", "deviance", "msy"))
  expect_equal(unname(out$draws[1, 1, "k"]), 1e6)
  expect_equal(unname(out$draws[1, 1, "msy"]), 0.4 * 1e6 / 4)
  expect_null(cmsy_read_mcmc(NULL))
})

test_that("the report gives the MCMC settings and names the stocks that did not converge", {
  set.seed(2)
  n <- 1000
  chains <- function(shift, rho) {
    draws <- array(NA_real_, dim = c(n, 2, 4), dimnames = list(NULL, NULL, c("r", "k", "bk_last", "msy")))
    for (j in 1:2) draws[, j, "r"] <- as.numeric(stats::arima.sim(list(ar = rho), n)) * 0.01 + 0.5 + (j - 1) * shift
    draws[, , "k"] <- 1e6 + stats::rnorm(2 * n, 0, 1e4)
    draws[, , "bk_last"] <- stats::rbeta(2 * n, 20, 20)
    draws[, , "msy"] <- draws[, , "r"] * draws[, , "k"] / 4
    list(settings = c(n.chains = 2, n.iter = 60000, n.burnin = 30000, n.thin = 10), draws = draws)
  }
  summary <- data.frame(Stock = c("COD_27", "HER_27"), species_en = c("Atlantic cod", "Atlantic herring"),
                        run_status = "ok", stringsAsFactors = FALSE)
  result <- structure(list(summary = summary, details = list(COD_27 = list(mcmc = chains(0, 0.2)),
                                                             HER_27 = list(mcmc = chains(0.2, 0.99)))),
                      class = "cmsy_result")
  names <- fao_report_species_names(summary$species_en, "en", fao_group_table()$species)
  text <- fao_report_cmsy_mcmc(result, names, "en")
  expect_match(text, "2 chains of 60,000 iterations \\(30,000 discarded as burn-in, keeping one draw in 10\\)")
  expect_match(text, "The chains of Atlantic herring had not converged")
  expect_false(grepl("Atlantic cod had not", text))
  zh <- fao_report_cmsy_mcmc(result, fao_report_species_names(summary$species_en, "zh", fao_group_table()$species), "zh")
  expect_match(zh, "60000")
  # results of earlier versions have no draws: no sentence
  result$details <- list(COD_27 = list(), HER_27 = list())
  expect_equal(fao_report_cmsy_mcmc(result, names, "en"), "")
})
