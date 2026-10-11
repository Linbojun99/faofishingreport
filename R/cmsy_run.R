# Running the bundled CMSY++ code (CMSY++16.R) on the stocks of cmsy_input(): each stock in its
# own folder and R process, with a copy of the code adapted to this operating system.

#' Run CMSY++ on FAO Catch Data
#'
#' This function runs the CMSY++ code bundled with the package (`CMSY++16.R`, January 2021, see
#' [cmsy_sources()]) on the stocks prepared by [cmsy_input()], and collects the results: MSY,
#' Bmsy and Fmsy, relative stock size (B/Bmsy) and exploitation (F/Fmsy) in every year with
#' their 95% confidence limits, the probability of each Kobe quadrant in the last year, and the
#' status of each stock.
#'
#' Each stock runs in its own folder under `out_dir` and in a separate R process, with a copy of
#' the code changed for this computer (see [cmsy_system()]: the `windows()` and `antialias`
#' changes of the user guide for macOS and Linux, and removing RStudio, the archived snpar
#' package and the automatic package installation). The folders keep the original CMSY++ output
#' (`cmsy_output.csv`, the JPEG charts and the log), so each result can be checked.
#'
#' Since FAO data have no abundance index, only the catch-only part of CMSY++ runs. Following FAO
#' (2026), whose global review uses CMSY++ among its Tier 3 (data-limited) methods, the status is
#' given in two classes only: "maximally sustainably fished" (B/Bmsy at least 0.8 in the last
#' year) or "overfished" (B/Bmsy below 0.8); catch-only results are not used to call a stock
#' underfished.
#'
#' @inheritParams cmsy_system
#' @inheritParams summary_params
#' @param input The result of [cmsy_input()].
#' @param stocks Optional stocks to run: `Stock` identifiers (e.g. "PIL_34") or species, by
#'        English or Chinese FAO name, scientific name or FAO 3-alpha code (all stocks of the
#'        species). Default is NULL, all stocks of `input`.
#' @param out_dir The folder for the CMSY++ output. Default is "cmsy" in the working directory.
#' @param parallel The number of stocks run at the same time. Each uses two more processes for
#'        its two MCMC chains. Default is half the number of processor cores.
#' @param plots The CMSY++ charts to save as JPEG files: any of "analysis", "management", "kobe",
#'        "prior_posterior" and "retrospective". Default is the first three.
#' @param seed Optional random seed, so runs can be repeated with the same results.
#' @param auto_start A logical value. When the mean (smoothed) catch of the first three years is
#'        below a third of the maximum, the start year is before 2000 and no start B/k prior is
#'        given, CMSY++ warns that the start biomass is ambiguous (depleted or unexploited) and
#'        suggests starting in the first year with at least 40% of the maximum catch (reported
#'        in `suggested_start_year`). If TRUE, the stock starts in that year. CMSY++'s own
#'        setting for this (`auto.start`) stops with an error in `CMSY++16.R` (the year range no
#'        longer matches the catch years), so the same rule is applied here to `StartYear`
#'        before the run. Default is FALSE, as in CMSY++; the user guide prefers setting
#'        `StartYear` or the start B/k prior (`stb.low`, `stb.hi`) in [cmsy_input()].
#' @return Invisibly, a list of class `cmsy_result` with `summary` (one row per stock: reference
#'         points, last-year status with 95% limits, Kobe probabilities, the priors used and the
#'         links to their sources), `series` (by year: catch, predicted catch, B/k, biomass,
#'         B/Bmsy, F and F/Fmsy, with 95% limits), `kobe` (the last-year draws), `details`
#'         (per stock: posterior draws of r, k and B/k, the priors, retrospective runs), `logs`,
#'         `sources` (the versions of CMSY++, JAGS, R and the data) and `changes` (the changes
#'         made to the CMSY++ code). The summary is saved in `tables/cmsy/`.
#' @references
#' Froese, R., Winker, H., Coro, G., et al. (2023). New developments in the analysis of catch
#' time series as the basis for fish stock assessments: the CMSY++ method. Acta Ichthyologica et
#' Piscatoria 53: 173-189. \doi{10.3897/aiep.53.105910}
#'
#' FAO (2026). The State of World Fisheries and Aquaculture 2026, Box 1.3 (assessment tiers and
#' status classes). Rome, FAO.
#' @export
#' @examples
#' \dontrun{
#' cmsy_setup()
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' input <- cmsy_input(FAO34$area_data)
#' result <- cmsy_run(input, stocks = input$stocks$Stock[1:3], seed = 1)
#' result$summary
#' cmsy_plot(result, lang = "zh")
#' }
cmsy_run <- function(input, stocks = NULL, out_dir = "cmsy", parallel = NULL,
                     plots = c("analysis", "management", "kobe"), os = "auto", device = c("file", "screen"),
                     seed = NULL, auto_start = FALSE, table = TRUE, lang = NULL) {
  if (!inherits(input, "cmsy_input")) stop("`input` must be the result of cmsy_input().", call. = FALSE)
  device <- match.arg(device)
  plots <- match.arg(plots, c("analysis", "management", "kobe", "prior_posterior", "retrospective"), several.ok = TRUE)
  lang <- fao_lang(lang)
  sys <- cmsy_system(os = os, device = device, quiet = TRUE)
  missing <- sys$packages$package[!sys$packages$ok]
  if (length(missing) > 0 || is.na(sys$jags)) {
    cmsy_print_system(sys)
    stop("CMSY++ needs JAGS and the R packages ", paste(sys$packages$package, collapse = ", "),
         ". Run cmsy_setup() first.", call. = FALSE)
  }

  stocks <- if (is.null(stocks)) input$id$Stock else cmsy_match_stocks(stocks, input$stocks)
  if (length(stocks) == 0) stop("No stocks to run.", call. = FALSE)

  run_dir <- file.path(out_dir, input$name)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- normalizePath(run_dir)
  code <- cmsy_patch_code(readLines(cmsy_file("CMSY++16.R"), warn = FALSE), sys$os)
  code_file <- file.path(run_dir, "CMSY++16_faofishingreport.R")
  writeLines(code, code_file)
  utils::write.csv(sys$changes, file.path(run_dir, "code_changes.csv"), row.names = FALSE)

  jobs <- lapply(stocks, function(stock) {
    dir <- file.path(run_dir, cmsy_safe_name(stock))
    unlink(dir, recursive = TRUE)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(cmsy_file("ffnn.bin"), file.path(dir, "ffnn.bin"))
    utils::write.csv(input$catch[input$catch$Stock == stock, ], file.path(dir, "catch.csv"), row.names = FALSE, na = "NA")
    id <- input$id[input$id$Stock == stock, ]
    if (auto_start) id$StartYear <- cmsy_auto_start(input$catch[input$catch$Stock == stock, ], id)
    utils::write.csv(id, file.path(dir, "id.csv"), row.names = FALSE, na = "NA")
    writeLines(cmsy_settings_code(stock, sys, plots, seed), file.path(dir, "cmsy_settings.R"))
    writeLines(c(paste0("setwd(", deparse(dir), ")"),
                 paste0("source(", deparse(code_file), ", echo = FALSE)")), file.path(dir, "run.R"))
    list(stock = stock, dir = dir)
  })

  if (is.null(parallel)) parallel <- max(1L, floor(parallel::detectCores(logical = FALSE) / 2), na.rm = TRUE)
  parallel <- max(1L, min(as.integer(parallel), length(jobs)))
  rscript <- sys$rscript
  # Child R processes (and the JAGS chains they start) use the same package libraries
  old_libs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = cmsy_lib_paths())
  on.exit(if (is.na(old_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_libs), add = TRUE)
  run_one <- function(job) {
    start <- Sys.time()
    status <- suppressWarnings(system2(rscript, shQuote(file.path(job$dir, "run.R")),
                                       stdout = file.path(job$dir, "log.txt"), stderr = file.path(job$dir, "log.txt")))
    list(stock = job$stock, exit = status, seconds = as.numeric(difftime(Sys.time(), start, units = "secs")))
  }
  message("Running CMSY++ on ", length(jobs), " stock", if (length(jobs) > 1) "s", " (", parallel,
          " at a time) in ", run_dir)
  runs <- if (parallel > 1) {
    cl <- parallel::makePSOCKcluster(parallel)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::parLapplyLB(cl, jobs, run_one)
  } else {
    lapply(seq_along(jobs), function(i) {
      message("  [", i, "/", length(jobs), "] ", jobs[[i]]$stock)
      run_one(jobs[[i]])
    })
  }

  result <- cmsy_collect(jobs, runs, input, lang)
  result$sources <- cmsy_run_sources(sys, input, seed)
  result$changes <- sys$changes
  result$run_dir <- run_dir
  utils::write.csv(result$sources, file.path(run_dir, "sources.csv"), row.names = FALSE)
  if (table) {
    fao_ensure_dirs(file.path("tables", "cmsy"))
    fao_write_csv(result$summary, file.path("tables", "cmsy", paste0(input$name, "_results.csv")))
    if (!is.null(result$series)) fao_write_csv(result$series, file.path("tables", "cmsy", paste0(input$name, "_series.csv")))
    fao_write_csv(result$sources, file.path("tables", "cmsy", paste0(input$name, "_sources.csv")))
  }
  ok <- sum(result$summary$run_status == "ok")
  message("CMSY++ finished: ", ok, " of ", nrow(result$summary), " stocks", if (ok < nrow(result$summary))
    " (see $summary$run_status and the log.txt file of each stock)", ".")
  invisible(result)
}

#' @export
print.cmsy_result <- function(x, ...) {
  cat("CMSY++ results (catch only) for", nrow(x$summary), "stocks\n")
  cols <- c("Stock", "species_name", "last_year", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "F_Fmsy", "status")
  print(utils::head(x$summary[, intersect(cols, names(x$summary))], 20), row.names = FALSE)
  invisible(x)
}

# A copy of CMSY++16.R changed for this system (see cmsy_changes()). Every change checks that the
# original text is found, so a different version of the code is not changed by mistake.
cmsy_patch_code <- function(code, os) {
  replace_line <- function(code, starts, new) {
    hit <- which(startsWith(trimws(code), starts))
    if (length(hit) != 1) stop("CMSY++ code: expected one line starting with \"", starts, "\".", call. = FALSE)
    code[hit] <- new
    code
  }
  tag <- "# [changed by faofishingreport] "
  code <- replace_line(code, "list.of.packages <-", paste0(tag, "packages are installed by cmsy_setup()"))
  code <- replace_line(code, "new.packages <-", tag)
  code <- replace_line(code, "if(length(new.packages)) install.packages", tag)
  code <- replace_line(code, "library(snpar)", paste0(tag, "snpar (archived on CRAN) replaced by cmsy_runs_test()"))
  code <- replace_line(code, "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))",
                       paste0(tag, "the working directory is set by cmsy_run()"))
  settings_line <- which(startsWith(code, "write.rdata  <-"))
  if (length(settings_line) != 1) stop("CMSY++ code: settings section not found.", call. = FALSE)
  code <- append(code, c(paste0(tag, "settings, file names and system-specific functions of cmsy_run()"),
                         "source(\"cmsy_settings.R\")"), after = settings_line)

  windows_call <- "if\\(grepl\\(\"win\",tolower\\(Sys\\.info\\(\\)\\['sysname'\\]\\)\\)\\) \\{windows\\(([0-9]+),([0-9]+)\\)\\}"
  if (!any(grepl(windows_call, code))) stop("CMSY++ code: windows() calls not found.", call. = FALSE)
  code <- gsub(windows_call, "cmsy_open_device(\\1,\\2) # [changed by faofishingreport] was windows() on Windows only", code)
  if (!any(grepl("antialias=\"cleartype\"", code, fixed = TRUE))) stop("CMSY++ code: antialias not found.", call. = FALSE)
  code <- gsub("antialias=\"cleartype\"", "antialias=cmsy_antialias", code, fixed = TRUE)
  if (sum(grepl("snpar::runs.test(resid)", code, fixed = TRUE)) != 1) stop("CMSY++ code: runs.test not found.", call. = FALSE)
  code <- gsub("snpar::runs.test(resid)", "cmsy_runs_test(resid)", code, fixed = TRUE)
  # Save the results of the main (not retrospective) analysis for cmsy_plot(); nothing else changes
  retro_line <- which(startsWith(trimws(code), "FFmsy.retrospective[[retrosp.step+1]]<-F.Fmsy"))
  if (length(retro_line) != 1) stop("CMSY++ code: end of the analysis of a stock not found.", call. = FALSE)
  code <- append(code, paste0("  if (retrosp.step == 0) cmsy_save_extra() ", tag, "results for cmsy_plot()"),
                 after = retro_line - 1)
  end_line <- which(startsWith(code, "} # end of stocks loop"))
  if (length(end_line) != 1) stop("CMSY++ code: end of the stock loop not found.", call. = FALSE)
  code <- append(code, paste0("  cmsy_save_retro() ", tag, "retrospective results for cmsy_plot()"), after = end_line - 1)
  c(paste0("# CMSY++16.R (Froese, Coro and Winker, January 2021; CC BY 4.0, ", cmsy_oceanrep_page, ")"),
    paste0("# with the changes listed in code_changes.csv, made by faofishingreport ",
           utils::packageVersion("faofishingreport"), " for ", os, " on ", Sys.Date()),
    code)
}

# Settings file sourced by the changed code, after its own settings section
cmsy_settings_code <- function(stock, sys, plots, seed) {
  flag <- function(x) if (x) "TRUE" else "FALSE"
  device <- if (sys$device == "screen") {
    switch(sys$os, windows = "windows(width, height)", macos = "grDevices::quartz(width = width, height = height)",
           linux = "grDevices::x11(width = width, height = height)")
  } else {
    "{grDevices::pdf(NULL, width = width, height = height); grDevices::dev.control(\"enable\")}"
  }
  c(paste0("# Written by faofishingreport::cmsy_run() on ", Sys.time(), " for ", sys$os, " (", sys$os_detected, " detected)"),
    "catch_file   <- \"catch.csv\"",
    "id_file      <- \"id.csv\"",
    "nn_file      <- \"ffnn.bin\"",
    "outfile      <- \"cmsy_output.csv\"",
    paste0("stocks       <- ", deparse(stock)),
    paste0("mgraphs      <- ", flag(any(c("management", "kobe") %in% plots))),
    paste0("kobe.plot    <- ", flag("kobe" %in% plots)),
    paste0("pp.plot      <- ", flag("prior_posterior" %in% plots)),
    paste0("retros       <- ", flag("retrospective" %in% plots)),
    "BSMfits.plot <- FALSE",
    "rk.diags     <- FALSE",
    "save.plots   <- TRUE",
    "close.plots  <- TRUE",
    "write.output <- TRUE",
    "write.pdf    <- FALSE",
    "write.rdata  <- TRUE",
    if (!is.null(seed)) paste0("set.seed(", as.integer(seed), ")"),
    paste0("cmsy_antialias <- \"", sys$antialias, "\""),
    paste0("cmsy_open_device <- function(width, height) ", device),
    "# Approximate two-sided Wald-Wolfowitz runs test, as snpar::runs.test() (snpar 1.0, D. Qiu, GPL >= 2)",
    "cmsy_runs_test <- function(x) {",
    "  x <- x[is.finite(x)]",
    "  N <- length(x)",
    "  x.s <- if (length(unique(x)) == 2) (x == unique(x)[1]) * 1 else (x > stats::median(x)) * 1",
    "  m <- sum(1 - x.s); n <- N - m",
    "  R <- 1 + sum(x.s[-1] != x.s[-N])",
    "  Z <- (R - 2 * m * n / N - 1) / sqrt(2 * m * n * (2 * m * n - N) / (N^2 * (N - 1)))",
    "  list(statistic = c(Runs = R), p.value = 2 * min(stats::pnorm(Z), 1 - stats::pnorm(Z)))",
    "}",
    "# Save the results used by faofishingreport::cmsy_plot()",
    paste0("cmsy_save_extra <- ", paste(deparse(cmsy_extra_saver), collapse = "\n")),
    paste0("cmsy_save_retro <- ", paste(deparse(cmsy_retro_saver), collapse = "\n")))
}

# Run in the CMSY++ process after the main analysis of a stock (see cmsy_patch_code()): saves the
# priors, the posterior draws of r, k and B/k, the yearly estimates with their 95% limits and the
# last-year Kobe draws, from the variables of CMSY++16.R
cmsy_extra_saver <- function() {
  v <- function(name) get0(name, envir = globalenv(), inherits = FALSE)
  yr <- v("yr")
  nyr <- length(yr)
  int.yr <- v("int.yr")
  int_i <- if (length(int.yr) == 1 && !is.na(int.yr)) match(int.yr, yr) else NA
  column <- function(m, i) if (is.null(m) || is.na(i)) NULL else m[, i]
  three <- function(a, b, c) {
    if (is.null(v(a))) return(NULL)
    cbind(est = v(a)[seq_len(nyr)], lcl = v(b)[seq_len(nyr)], ucl = v(c)[seq_len(nyr)])
  }
  bk <- v("all.bk.cmsy")
  saveRDS(list(
    yr = yr, ct.raw = v("ct.raw"), ct.smooth = v("ct"), start.yr = v("start.yr"), end.yr = v("end.yr"),
    start.yr.new = v("start.yr.new"),
    prior.r = v("prior.r"), prior.k = v("prior.k"), prior.msy = v("prior.msy"), MSY.pr = v("MSY.pr"),
    startbio = v("startbio"), intbio = v("intbio"), int.yr = int.yr, endbio = v("endbio"),
    expert = c(r = !is.na(v("r.low")), start = !is.na(v("stb.low")), int = !is.na(v("intb.low")),
               end = !is.na(v("endb.low"))),
    prior_logn = c(mean_log_r = v("mean.log.r"), sd_log_r = v("sd.log.r"), mean_log_k = v("mean.log.k"),
                   sd_log_k = v("sd.log.k"), cor_log_rk = v("cor.log.rk")),
    bk_beta = tryCatch(v("beta.prior")(v("b.prior")), error = function(e) NULL),
    r = v("rs"), k = v("ks"),
    bk_first = column(bk, 1), bk_int = column(bk, int_i), bk_last = column(bk, nyr),
    bk = three("bk.cmsy", "lcl.bk.cmsy", "ucl.bk.cmsy"),
    B = three("B", "lcl.B", "ucl.B"),
    Ft = three("Ft", "lcl.Ft", "ucl.Ft"),
    B_Bmsy = three("B.Bmsy", "lcl.B.Bmsy", "ucl.B.Bmsy"),
    F_Fmsy = three("F.Fmsy", "lcl.F.Fmsy", "ucl.F.Fmsy"),
    Fmsy_adj = three("Fmsy.adj", "lcl.Fmsy.adj", "ucl.Fmsy.adj"),
    catch_pred = three("ct.jags", "lcl.ct.jags", "ucl.ct.jags"),
    MSY = c(v("MSY"), v("lcl.MSY"), v("ucl.MSY")), Bmsy = c(v("Bmsy"), v("lcl.Bmsy"), v("ucl.Bmsy")),
    Fmsy = c(v("Fmsy"), v("lcl.Fmsy"), v("ucl.Fmsy")),
    kobe = data.frame(B_Bmsy = column(v("all.BBmsy.cmsy"), nyr), F_Fmsy = column(v("all.FFmsy.cmsy"), nyr))
  ), "cmsy_extra.rds")
}

# Run at the end of the analysis of a stock: the retrospective runs, if any
cmsy_retro_saver <- function() {
  v <- function(name) get0(name, envir = globalenv(), inherits = FALSE)
  if (isTRUE(v("retros"))) {
    saveRDS(list(years = v("years.retrospective"), F_Fmsy = v("FFmsy.retrospective"),
                 B_Bmsy = v("BBmsy.retrospective")), "cmsy_retro.rds")
  }
}

cmsy_safe_name <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

# Start year suggested by CMSY++16.R when the first catches are low (lines 448-480 of the code):
# catch smoothed with ksmooth(bandwidth = 3), mean of the first three years below 0.33 of the
# maximum, start before 2000 and no start B/k prior -> first year with at least 0.4 of the maximum
cmsy_auto_start <- function(catch, id) {
  catch <- catch[catch$yr >= id$StartYear & catch$yr <= id$EndYear, ]
  smooth <- stats::ksmooth(x = catch$yr, y = catch$ct / 1000, kernel = "normal",
                           n.points = nrow(catch), bandwidth = 3)$y
  if (is.na(id$stb.low) && mean(smooth[1:3]) < 0.33 * max(smooth) && id$StartYear < 2000) {
    catch$yr[which(smooth >= 0.4 * max(smooth))][1]
  } else {
    id$StartYear
  }
}

# rbind of data frames with different columns (failed stocks have only a few)
cmsy_rbind_fill <- function(frames) {
  frames <- Filter(Negate(is.null), frames)
  columns <- unique(unlist(lapply(frames, names)))
  do.call(rbind, lapply(frames, function(f) {
    f[setdiff(columns, names(f))] <- NA
    f[columns]
  }))
}

# Reads the output of each stock folder
cmsy_collect <- function(jobs, runs, input, lang) {
  parts <- lapply(seq_along(jobs), function(i) cmsy_read_stock(jobs[[i]]$dir, jobs[[i]]$stock, runs[[i]]))
  summary <- cmsy_rbind_fill(lapply(parts, `[[`, "summary"))
  info <- input$stocks[match(summary$Stock, input$stocks$Stock),
                       c("species_en", "species_zh", "alpha3", "scientific_name", "area", "country", "r_source",
                         "prior_source", "url", "data_url", "rule_url", "fao_data_url")]
  summary <- cbind(Stock = summary$Stock, species_name = fao_translate(info$species_en, "species", lang),
                   info, summary[, setdiff(names(summary), "Stock")], stringsAsFactors = FALSE)
  rownames(summary) <- NULL
  series <- do.call(rbind, lapply(parts, `[[`, "series"))
  if (!is.null(series)) {
    catch <- input$catch[, c("Stock", "yr", "ct")]
    names(catch) <- c("Stock", "Year", "Catch")
    series <- merge(series, catch, by = c("Stock", "Year"), all.x = TRUE, sort = FALSE)
    series <- series[order(series$Stock, series$Year), c("Stock", "Year", "Catch", setdiff(names(series), c("Stock", "Year", "Catch")))]
    rownames(series) <- NULL
  }
  structure(list(
    summary = summary,
    series = series,
    kobe = do.call(rbind, lapply(parts, `[[`, "kobe")),
    details = stats::setNames(lapply(parts, `[[`, "details"), summary$Stock),
    logs = stats::setNames(lapply(parts, `[[`, "log"), summary$Stock)
  ), class = "cmsy_result")
}

cmsy_read_stock <- function(dir, stock, run) {
  log <- if (file.exists(file.path(dir, "log.txt"))) readLines(file.path(dir, "log.txt"), warn = FALSE) else character(0)
  out_file <- file.path(dir, "cmsy_output.csv")
  extra_file <- file.path(dir, "cmsy_extra.rds")
  empty <- data.frame(Stock = stock, run_status = "failed", message = cmsy_log_error(log),
                      seconds = round(run$seconds), stringsAsFactors = FALSE)
  if (!file.exists(out_file) || !file.exists(extra_file)) {
    return(list(summary = empty, series = NULL, kobe = NULL, details = NULL, log = log))
  }
  out <- utils::read.csv(out_file, stringsAsFactors = FALSE, check.names = FALSE)
  out <- out[nrow(out), ]
  x <- readRDS(extra_file)
  kt <- function(v) 1000 * as.numeric(v)   # CMSY++ works in thousand tonnes
  col <- function(m, j) if (is.null(m)) NA_real_ else as.numeric(m[, j])

  kobe <- data.frame(Stock = stock, B_Bmsy = x$kobe$B_Bmsy, F_Fmsy = x$kobe$F_Fmsy)
  p <- function(cond) round(mean(cond, na.rm = TRUE), 3)
  b <- kobe$B_Bmsy
  f <- kobe$F_Fmsy
  last_b <- as.numeric(out$last.B_Bmsy)
  source_of <- function(expert) if (isTRUE(expert)) "user" else "CMSY++ neural network"
  summary <- data.frame(
    Stock = stock, run_status = "ok", message = cmsy_log_notes(log),
    seconds = round(run$seconds),
    first_year = as.integer(out$start.yr), last_year = as.integer(out$end.yr),
    suggested_start_year = suppressWarnings(as.integer(out$start.yr.new)),
    MSY = kt(out$MSY), MSY_lcl = kt(out$lcl.MSY), MSY_ucl = kt(out$ucl.MSY),
    Bmsy = kt(out$Bmsy), Bmsy_lcl = kt(out$lcl.Bmsy), Bmsy_ucl = kt(out$ucl.Bmsy),
    Fmsy = as.numeric(out$F_msy), Fmsy_lcl = as.numeric(out$lcl.F_msy), Fmsy_ucl = as.numeric(out$ucl.F_msy),
    r = as.numeric(out$r_CMSY), r_lcl = as.numeric(out$lcl.r_CMSY), r_ucl = as.numeric(out$ucl.r_CMSY),
    k = kt(out$k_CMSY), k_lcl = kt(out$lcl.k_CMSY), k_ucl = kt(out$ucl.k_CMSY),
    B_Bmsy = last_b, B_Bmsy_lcl = as.numeric(out$lcl.last.B_Bmsy), B_Bmsy_ucl = as.numeric(out$ucl.last.B_Bmsy),
    F_Fmsy = as.numeric(out$last.F_Fmsy), F_Fmsy_lcl = as.numeric(out$lcl.last.F_Fmsy),
    F_Fmsy_ucl = as.numeric(out$ucl.last.F_Fmsy),
    B_last = kt(out$last.B), B_last_lcl = kt(out$lcl.last.B), B_last_ucl = kt(out$ucl.last.B),
    catch_last = kt(out$LastCatch),
    r_prior_low = x$prior.r[1], r_prior_high = x$prior.r[2],
    k_prior_low = kt(x$prior.k[1]), k_prior_high = kt(x$prior.k[2]), MSY_prior = kt(x$MSY.pr),
    start_bk_prior_low = x$startbio[1], start_bk_prior_high = x$startbio[2],
    start_bk_prior_source = source_of(x$expert[["start"]]),
    int_year = as.integer(x$int.yr), int_bk_prior_low = x$intbio[1], int_bk_prior_high = x$intbio[2],
    int_bk_prior_source = source_of(x$expert[["int"]]),
    end_bk_prior_low = x$endbio[1], end_bk_prior_high = x$endbio[2],
    end_bk_prior_source = source_of(x$expert[["end"]]),
    p_green = p(b >= 1 & f <= 1), p_orange = p(b >= 1 & f > 1), p_yellow = p(b < 1 & f <= 1), p_red = p(b < 1 & f > 1),
    p_overfished = p(b < 0.8),
    status = ifelse(is.na(last_b), NA_character_, ifelse(last_b >= 0.8, "maximally sustainably fished", "overfished")),
    folder = dir, stringsAsFactors = FALSE
  )
  series <- data.frame(
    Stock = stock, Year = as.integer(x$yr),
    Catch_smooth = kt(x$ct.smooth),
    Catch_pred = kt(col(x$catch_pred, 1)), Catch_pred_lcl = kt(col(x$catch_pred, 2)), Catch_pred_ucl = kt(col(x$catch_pred, 3)),
    B_k = col(x$bk, 1), B_k_lcl = col(x$bk, 2), B_k_ucl = col(x$bk, 3),
    Biomass = kt(col(x$B, 1)), Biomass_lcl = kt(col(x$B, 2)), Biomass_ucl = kt(col(x$B, 3)),
    B_Bmsy = col(x$B_Bmsy, 1), B_Bmsy_lcl = col(x$B_Bmsy, 2), B_Bmsy_ucl = col(x$B_Bmsy, 3),
    F = col(x$Ft, 1), F_lcl = col(x$Ft, 2), F_ucl = col(x$Ft, 3),
    F_Fmsy = col(x$F_Fmsy, 1), F_Fmsy_lcl = col(x$F_Fmsy, 2), F_Fmsy_ucl = col(x$F_Fmsy, 3),
    Fmsy_adj = col(x$Fmsy_adj, 1),
    row.names = NULL
  )
  retro_file <- file.path(dir, "cmsy_retro.rds")
  details <- list(
    r = x$r, k = kt(x$k), bk_first = x$bk_first, bk_int = x$bk_int, bk_last = x$bk_last,
    prior_r = x$prior.r, prior_k = kt(x$prior.k), prior_msy = kt(x$prior.msy), prior_logn = x$prior_logn,
    bk_beta = x$bk_beta,
    startbio = x$startbio, intbio = x$intbio, int_year = x$int.yr, endbio = x$endbio, expert = x$expert,
    retrospective = if (file.exists(retro_file)) readRDS(retro_file) else NULL
  )
  list(summary = summary, series = series, kobe = kobe, details = details, log = log)
}

# CMSY++'s own notes on the run (e.g. "Attention: Low catch in 1950 may indicate ...")
cmsy_log_notes <- function(log) {
  start <- grep("Attention:", log)
  if (length(start) == 0) return("")
  lines <- unlist(lapply(start, function(i) log[i:min(length(log), i + 2)]))
  lines <- trimws(lines[!grepl("^\\s*\\*+\\s*$", lines)])
  paste(lines, collapse = " ")
}

cmsy_log_error <- function(log) {
  hit <- grep("ERROR|Error", log, value = TRUE)
  if (length(hit) == 0) return("no output; see log.txt")
  paste(trimws(utils::head(hit, 2)), collapse = " ")
}

# Versions and links of everything a run used
cmsy_run_sources <- function(sys, input, seed) {
  registry <- cmsy_registry()
  bundled <- registry[registry$bundled & registry$file %in% c("CMSY++16.R", "ffnn.bin", "CMSYUserGuideMarch2021.pdf"), ]
  fishbase <- registry[registry$component %in% c("FishBase", "SeaLifeBase"), ]
  row <- function(component, version, url, citation = "") {
    data.frame(component = component, version = version, url = url, citation = citation, stringsAsFactors = FALSE)
  }
  rbind(
    row(bundled$component, bundled$version, bundled$url, bundled$citation),
    row("JAGS", sys$jags, "https://mcmc-jags.sourceforge.io/", cmsy_cite_jags),
    row(sys$packages$package, sys$packages$installed, paste0("https://cran.r-project.org/package=", sys$packages$package)),
    row("R", sys$r_version, "https://www.r-project.org/"),
    row("Operating system", paste0(sys$os, " (", sys$arch, if (sys$os != sys$os_detected) paste0("; detected ", sys$os_detected), ")"), ""),
    row("faofishingreport", as.character(utils::packageVersion("faofishingreport")), "https://github.com/Linbojun99/faofishingreport"),
    row("FAO Global Capture Production", input$fao_version, "https://www.fao.org/fishery/en/collection/capture"),
    row(fishbase$component, fishbase$version, fishbase$url, fishbase$citation),
    row("Status classes", "FAO 2026", "https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html",
        "Maximally sustainably fished: B/Bmsy 0.8-1.2; Tier 2 and 3 stocks are not classified as underfished"),
    row("Random seed", if (is.null(seed)) "none" else as.character(seed), ""),
    row("Changes to the CMSY++ code", paste(sys$changes$change, collapse = "; "), "code_changes.csv in the run folder"),
    row("Run date", format(Sys.time(), "%Y-%m-%d %H:%M"), "")
  )
}
