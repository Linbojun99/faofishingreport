# The CMSY++ part of fao_report(): running cmsy_area(), the text values, the appendix tables
# and the section on the assessment of the main species.

# Runs CMSY++ on the main species of the area, keeping its results in `out_dir`. `args` are the
# settings of cmsy_area() given to fao_report() with the prefix cmsy_ (they win over the defaults)
fao_report_cmsy_run <- function(area_data, args, lang, out_dir, dpi = 300) {
  system <- cmsy_system(silent = TRUE)
  if (is.na(system$jags) || !all(system$packages$ok)) {
    stop("CMSY++ needs JAGS and the R packages ", paste(system$packages$package[!system$packages$ok], collapse = ", "),
         ": run cmsy_setup() once, or use cmsy = FALSE.", call. = FALSE)
  }
  args <- utils::modifyList(list(area = area_data, lang = lang, out_dir = out_dir, forecast = FALSE,
                                 charts = c("management", "kobe_all", "b_bmsy_all"), silent = TRUE), args)
  # the charts of the report are always drawn
  args$charts <- union(args$charts, c("management", "kobe_all", "b_bmsy_all"))
  message("  CMSY++ for the ", if (is.null(args$species)) paste(args$top, "main species") else "chosen species",
          " (a few minutes)")
  old_options <- options(faofishingreport.dpi = dpi, faofishingreport.pptx = FALSE)
  on.exit(options(old_options), add = TRUE)
  result <- suppressWarnings(suppressMessages(do.call(cmsy_area, args)))
  # Saved to write the report again without running CMSY++: cmsy = readRDS(<file>)
  saved <- file.path(result$dir, "cmsy_result.rds")
  saveRDS(result, saved)
  message("  CMSY++ results: ", result$dir, " (reuse them with cmsy = readRDS(\"", saved, "\"))")
  result
}

# A CMSY++ result given to fao_report(): its stocks must be of the area of the report, and its
# input of the same FAO release (cmsy_area() keeps the input; results of cmsy_run() have the
# release in the links of the stocks)
fao_report_check_cmsy <- function(result, area_name, release) {
  areas <- unique(stats::na.omit(result$summary$area))
  if (length(areas) == 0) {
    stop("`cmsy` does not say which FAO area its stocks are in; give the result of cmsy_area(), or use cmsy = TRUE.",
         call. = FALSE)
  }
  if (!all(areas %in% area_name)) {
    stop("`cmsy` holds CMSY++ results of ", paste(areas, collapse = ", "), ", not of ", area_name,
         ". Give the result of cmsy_area() for ", area_name, ", or use cmsy = TRUE.", call. = FALSE)
  }
  version <- result$input$fao_version
  if (is.null(version)) {
    urls <- unique(stats::na.omit(result$summary$fao_data_url))
    version <- unique(sub("^.*Capture_(.*)[.]zip$", "\\1", urls))
  }
  if (length(version) > 0 && !is.na(release) && !all(version == release)) {
    stop("`cmsy` was run on the FAO release ", paste(version, collapse = ", "), ", but the report uses release ",
         release, ". Use cmsy = TRUE to run CMSY++ on the data of the report, or data_version = \"", version[1],
         "\" if that release is on this computer.", call. = FALSE)
  }
  invisible(TRUE)
}

# Values of the CMSY++ text: counts of the stocks, and the text of every species
fao_report_cmsy_values <- function(result, lang) {
  text <- function(.key, ...) fao_report_text(.key, lang, list(...))
  list_text <- function(x) fao_report_list(x, lang)
  s <- result$summary
  ok <- cmsy_has_results(s$run_status)
  names <- fao_report_species_names(s$species_en, lang, fao_group_table()$species)
  b <- s$B_Bmsy
  f <- s$F_Fmsy
  overfished <- ok & b < 0.8
  missing <- c(as.character(result$input$excluded$species_name), names$plain[!ok])
  counts <- list(n_ok = sum(ok), n_overfished = sum(overfished), n_msf = sum(ok & b >= 0.8),
                 n_green = sum(ok & b >= 1 & f <= 1, na.rm = TRUE), n_red = sum(ok & b < 1 & f > 1, na.rm = TRUE))
  # counts of stocks with their verb: "no stock was", "1 stock was", "2 stocks were" (in Chinese
  # "mei you zhong qun" for none)
  were <- function(n) {
    if (lang == "zh") return(if (n == 0) "\u6ca1\u6709\u79cd\u7fa4" else paste0(n, " \u4e2a\u79cd\u7fa4"))
    if (n == 0) "no stock was" else if (n == 1) "1 stock was" else paste(n, "stocks were")
  }
  counts <- c(counts, list(msf_were = were(counts$n_msf), overfished_were = were(counts$n_overfished),
                           green_were = were(counts$n_green), red_were = were(counts$n_red),
                           overfished_verb = if (counts$n_overfished == 1) "was" else "were"))
  number <- function(x) sprintf("%.2f", x)
  catch <- function(x) fao_report_catch(x, lang)
  status <- function(i) {
    label <- cmsy_label(if (b[i] >= 0.8) "status_msf" else "status_overfished", lang)
    if (lang == "zh") label else tolower(label)
  }
  species <- lapply(seq_len(nrow(s)), function(i) {
    if (!ok[i]) return(list(ok = FALSE, values = list(species = names$full[i], message = s$message[i])))
    list(ok = TRUE, values = list(
      species = names$full[i], first_year = s$first_year[i], last_year = s$last_year[i],
      r = number(s$r[i]), r_lcl = number(s$r_lcl[i]), r_ucl = number(s$r_ucl[i]),
      k = catch(s$k[i]), k_range = fao_report_catch_range(s$k_lcl[i], s$k_ucl[i], lang),
      msy = catch(s$MSY[i]), msy_range = fao_report_catch_range(s$MSY_lcl[i], s$MSY_ucl[i], lang),
      bbmsy = number(b[i]), bbmsy_lcl = number(s$B_Bmsy_lcl[i]), bbmsy_ucl = number(s$B_Bmsy_ucl[i]),
      ffmsy = number(f[i]), ffmsy_lcl = number(s$F_Fmsy_lcl[i]), ffmsy_ucl = number(s$F_Fmsy_ucl[i]),
      catch_last = catch(s$catch_last[i]), catch_msy = fao_report_pct(100 * s$catch_last[i] / s$MSY[i]),
      status = status(i), p_green = fao_report_pct(100 * s$p_green[i]), p_orange = fao_report_pct(100 * s$p_orange[i]),
      p_yellow = fao_report_pct(100 * s$p_yellow[i]), p_red = fao_report_pct(100 * s$p_red[i])
    ))
  })
  table <- data.frame(
    species = names$plain, scientific = names$marked, years = paste0(s$first_year, "\u2013", s$last_year),
    r = ifelse(ok, number(s$r), "-"),
    k = ifelse(ok, sprintf("%.1f", s$k / if (lang == "zh") 1e4 else 1e3), "-"),
    msy = ifelse(ok, sprintf("%.1f", s$MSY / if (lang == "zh") 1e4 else 1e3), "-"),
    bbmsy = ifelse(ok, number(b), "-"), ffmsy = ifelse(ok, number(f), "-"),
    status = ifelse(ok, vapply(seq_len(nrow(s)), function(i) if (ok[i]) cmsy_label(if (b[i] >= 0.8) "status_msf" else "status_overfished", lang) else "-", character(1)), "-"),
    stringsAsFactors = FALSE
  )
  word <- function(key) fao_report_text(key, lang)
  names(table) <- c(word("col_species"), word("col_scientific"), word("col_years"), word("col_r"), word("col_k"),
                    word("col_msy"), word("col_bbmsy"), word("col_ffmsy"), word("col_status"))
  tables <- fao_report_cmsy_tables(result, lang, names)
  priors <- fao_report_cmsy_priors(s, ok, lang)
  partial <- s$run_status %in% "partial"
  c(counts, tables, list(
    has_cmsy = TRUE, cmsy_top = nrow(s) + nrow(result$input$excluded),
    cmsy_priors_text = priors$text,
    cmsy_mcmc_text = fao_report_cmsy_mcmc(result, names, lang),
    cmsy_unchecked = if (any(priors$unchecked)) text("cmsy_unchecked") else "",
    partial_note = if (any(partial)) text("cmsy_partial", list = list_text(names$plain[partial])) else "",
    not_assessed = if (length(missing) > 0) text("cmsy_not_assessed", list = list_text(missing)) else "",
    overfished_list = if (any(overfished)) text("cmsy_overfished_list", list = list_text(names$plain[overfished])) else "",
    cmsy_species = species, cmsy_table = table,
    cmsy_charts = list(
      kobe = file.path(result$dir, "figures", "summary", "kobe_all.png"),
      bbmsy = file.path(result$dir, "figures", "summary", "b_bmsy_all.png"),
      species = file.path(result$dir, "figures", cmsy_safe_name(s$Stock), "management.png"))
  ))
}

# Biological reference points, priors and their sources: data frames for R (with all values
# and links) and tables for the appendix of the report
fao_report_cmsy_tables <- function(result, lang, names) {
  s <- result$summary
  stocks <- result$input$stocks[match(s$Stock, result$input$stocks$Stock), ]
  ok <- cmsy_has_results(s$run_status)
  word <- function(key) fao_report_text(key, lang)
  scale <- if (lang == "zh") 1e4 else 1e3
  ci <- function(x, low, high, digits = 2, divide = 1) {
    f <- function(v) formatC(v / divide, format = "f", digits = digits)
    ifelse(ok, paste0(f(x), " (", f(low), "\u2013", f(high), ")"), "-")
  }
  range <- function(low, high, digits = 2, divide = 1) {
    f <- function(v) formatC(v / divide, format = "f", digits = digits)
    ifelse(is.na(low) | is.na(high), "-", paste0(f(low), "\u2013", f(high)))
  }
  bk_source <- function(x) {
    x <- ifelse(x %in% "user", word("source_user"), ifelse(is.na(x), "-", word("source_network")))
    x
  }
  resilience <- ifelse(is.na(stocks$Resilience), "-", vapply(stocks$Resilience, function(r) {
    tryCatch(fao_report_text(paste0("resilience_", r), lang), error = function(e) r)
  }, character(1)))
  r_source <- fao_report_cmsy_source(stocks$r_source, lang)

  # Data frames for R: one row per stock, with every value and link
  reference_points <- data.frame(
    Stock = s$Stock, species = names$plain, species_en = s$species_en, scientific_name = s$scientific_name,
    first_year = s$first_year, last_year = s$last_year, run_status = s$run_status,
    r = s$r, r_lcl = s$r_lcl, r_ucl = s$r_ucl, k_t = s$k, k_lcl_t = s$k_lcl, k_ucl_t = s$k_ucl,
    MSY_t = s$MSY, MSY_lcl_t = s$MSY_lcl, MSY_ucl_t = s$MSY_ucl, Fmsy = s$Fmsy, Fmsy_lcl = s$Fmsy_lcl,
    Fmsy_ucl = s$Fmsy_ucl, Bmsy_t = s$Bmsy, Bmsy_lcl_t = s$Bmsy_lcl, Bmsy_ucl_t = s$Bmsy_ucl,
    B_Bmsy = s$B_Bmsy, B_Bmsy_lcl = s$B_Bmsy_lcl, B_Bmsy_ucl = s$B_Bmsy_ucl,
    F_Fmsy = s$F_Fmsy, F_Fmsy_lcl = s$F_Fmsy_lcl, F_Fmsy_ucl = s$F_Fmsy_ucl, catch_last_t = s$catch_last,
    p_green = s$p_green, p_orange = s$p_orange, p_yellow = s$p_yellow, p_red = s$p_red, status = s$status,
    stringsAsFactors = FALSE
  )
  priors <- data.frame(
    Stock = s$Stock, species = names$plain, species_en = s$species_en, scientific_name = s$scientific_name,
    resilience = stocks$Resilience, resilience_source = stocks$resilience_source,
    r_prior_low = s$r_prior_low, r_prior_high = s$r_prior_high, r_prior_source = stocks$r_source,
    k_prior_low_t = s$k_prior_low, k_prior_high_t = s$k_prior_high, MSY_prior_t = s$MSY_prior,
    start_bk_prior_low = s$start_bk_prior_low, start_bk_prior_high = s$start_bk_prior_high,
    start_bk_prior_source = s$start_bk_prior_source, int_year = s$int_year,
    int_bk_prior_low = s$int_bk_prior_low, int_bk_prior_high = s$int_bk_prior_high,
    int_bk_prior_source = s$int_bk_prior_source, end_bk_prior_low = s$end_bk_prior_low,
    end_bk_prior_high = s$end_bk_prior_high, end_bk_prior_source = s$end_bk_prior_source,
    species_page_url = stocks$url, fishbase_data_url = stocks$data_url, cmsy_guide_url = stocks$rule_url,
    fao_data_url = stocks$fao_data_url, stringsAsFactors = FALSE
  )

  # Tables of the appendix
  points_table <- data.frame(names$plain, ci(s$r, s$r_lcl, s$r_ucl),
                             ci(s$k, s$k_lcl, s$k_ucl, 1, scale), ci(s$MSY, s$MSY_lcl, s$MSY_ucl, 1, scale),
                             ci(s$Fmsy, s$Fmsy_lcl, s$Fmsy_ucl), ci(s$Bmsy, s$Bmsy_lcl, s$Bmsy_ucl, 1, scale),
                             ci(s$B_Bmsy, s$B_Bmsy_lcl, s$B_Bmsy_ucl), ci(s$F_Fmsy, s$F_Fmsy_lcl, s$F_Fmsy_ucl),
                             stringsAsFactors = FALSE)
  names(points_table) <- c(word("col_species"), word("col_r"), word("col_k"), word("col_msy"), word("col_fmsy"),
                           word("col_bmsy"), word("col_bbmsy"), word("col_ffmsy"))
  priors_table <- data.frame(names$plain, resilience, range(s$r_prior_low, s$r_prior_high, 3),
                             range(s$k_prior_low, s$k_prior_high, 1, scale),
                             range(s$start_bk_prior_low, s$start_bk_prior_high),
                             ifelse(is.na(s$int_year), "-", paste0(s$int_year, ": ", range(s$int_bk_prior_low, s$int_bk_prior_high))),
                             range(s$end_bk_prior_low, s$end_bk_prior_high), stringsAsFactors = FALSE)
  names(priors_table) <- c(word("col_species"), word("col_resilience"), word("col_r_prior"), word("col_k_prior"),
                           word("col_bk_start"), word("col_bk_int"), word("col_bk_end"))
  sources_table <- data.frame(names$plain, r_source, bk_source(s$end_bk_prior_source),
                              ifelse(is.na(stocks$url), "-", stocks$url), stringsAsFactors = FALSE)
  names(sources_table) <- c(word("col_species"), word("col_r_source"), word("col_bk_source"), word("col_page"))
  first <- function(x) { x <- unique(stats::na.omit(x)); if (length(x) > 0) x[1] else "-" }
  list(cmsy_reference_points = reference_points, cmsy_priors = priors,
       cmsy_appendix = list(points = points_table, priors = priors_table, sources = sources_table),
       fishbase_data = first(stocks$data_url), cmsy_code = first(stocks$rule_url), fao_data = first(stocks$fao_data_url))
}

# The source of the r prior in the report language
fao_report_cmsy_source <- function(x, lang) {
  if (lang != "zh") return(ifelse(is.na(x), "-", x))
  x <- sub("^(\\S+) prior r, 95% CL \\(Based on ([0-9]+) full stock assessments?\\.?\\)$",
           "\\1 \u5148\u9a8c r\uff0895% \u7f6e\u4fe1\u9650\uff0c\u57fa\u4e8e \\2 \u4e2a\u5b8c\u6574\u8d44\u6e90\u8bc4\u4f30\uff09", x)
  x <- sub("^(\\S+) prior r, 95% CL \\(Based on ([0-9]+) data-limited stock assessments?\\.?\\)$",
           "\\1 \u5148\u9a8c r\uff0895% \u7f6e\u4fe1\u9650\uff0c\u57fa\u4e8e \\2 \u4e2a\u6570\u636e\u6709\u9650\u7684\u8d44\u6e90\u8bc4\u4f30\uff09", x)
  x <- sub("^(\\S+) resilience \\(([^)]*)\\), CMSY\\+\\+ user guide Table 2$",
           "\\1 \u6062\u590d\u529b\uff08\\2\uff09\uff0cCMSY++ \u7528\u6237\u6307\u5357\u8868 2", x)
  ifelse(is.na(x), "-", x)
}

# The CMSY++ section: overview with the results table and the charts of all stocks, then one
# subsection per species and a summary
fao_report_cmsy_section <- function(x, values, text, part = 3) {
  charts <- values$cmsy_charts
  fig <- function(...) fao_report_ref(x, "figures", c(...))
  x <- fao_report_par(x, paste(part, text("h_cmsy")), "FAOHeading1")
  x <- fao_report_par(x, paste0(part, ".1 ", text("h_cmsy_overview")), "FAOHeading2")
  x <- fao_report_par(x, text("cmsy_p1", tab_cmsy = fao_report_ref(x, "tables", "cmsy"),
                              fig_cmsy = fig("cmsy_kobe", "cmsy_bbmsy")))
  x <- fao_report_table(x, "cmsy", values$cmsy_table, text("tab_cmsy"))
  x <- fao_report_figure(x, "cmsy_kobe", charts$kobe, text("fig_cmsy_kobe"))
  x <- fao_report_figure(x, "cmsy_bbmsy", charts$bbmsy, text("fig_cmsy_bbmsy"))
  for (i in seq_along(values$cmsy_species)) {
    stock <- values$cmsy_species[[i]]
    x <- fao_report_par(x, paste0(part, ".", i + 1, " ", text("h_cmsy_species", species = stock$values$species)), "FAOHeading2")
    if (!stock$ok) {
      x <- fao_report_par(x, do.call(text, c(list("cmsy_species_failed"), stock$values)))
      next
    }
    id <- paste0("cmsy_", i)
    x <- fao_report_par(x, do.call(text, c(list("cmsy_species_p1", fig = fig(id)), stock$values)))
    x <- fao_report_figure(x, id, charts$species[i], text("fig_cmsy_species", species = stock$values$species))
    x <- fao_report_par(x, text("cmsy_species_note"), "FAONote")
  }
  x <- fao_report_par(x, paste0(part, ".", length(values$cmsy_species) + 2, " ", text("h_cmsy_summary")), "FAOHeading2")
  fao_report_par(x, text("cmsy_summary"))
}

# A finished run is not a validated assessment: the B/k priors suggested by the neural network
# "may not be appropriate for your stock" (CMSY++ user guide) and were not checked stock by stock
# unless the user set them. Returns the sentence of the methods on the priors and, per stock,
# whether its results rest on unchecked priors
fao_report_cmsy_priors <- function(s, ok, lang) {
  default_prior <- function(column) if (column %in% names(s)) !s[[column]] %in% "user" else rep(TRUE, nrow(s))
  unchecked <- ok & (default_prior("start_bk_prior_source") | default_prior("int_bk_prior_source") |
                       default_prior("end_bk_prior_source"))
  text <- if (!any(unchecked)) {
    fao_report_text("cmsy_priors_user", lang)
  } else if (all(unchecked[ok])) {
    fao_report_text("cmsy_priors_nn", lang)
  } else {
    stocks <- function(n) paste(n, if (n == 1) "stock" else "stocks")
    n_user <- sum(ok & !unchecked)
    n_nn <- sum(unchecked)
    fao_report_text("cmsy_priors_mixed", lang, list(n_user = n_user, n_nn = n_nn, n_user_stocks = stocks(n_user),
                                                    n_nn_stocks = stocks(n_nn)))
  }
  list(text = text, unchecked = unchecked)
}

# The sentence of the methods on the MCMC: settings, the largest Rhat and smallest effective sample
# size of r, k, MSY and the last-year B/k, and the stocks that had not converged or had too few
# effective draws (cmsy_mcmc()). Empty for results of earlier versions, which have no draws
fao_report_cmsy_mcmc <- function(result, names, lang) {
  mcmc <- tryCatch(cmsy_mcmc(result, plot = FALSE, table = FALSE, silent = TRUE), error = function(e) NULL)
  if (is.null(mcmc)) return("")
  d <- mcmc$diagnostics[mcmc$diagnostics$parameter %in% c("r", "k", "msy", "bk_last"), ]
  set <- mcmc$settings[1, ]
  number <- function(x) format(x, big.mark = if (lang == "zh") "" else ",", scientific = FALSE, trim = TRUE)
  species <- function(stocks) {
    fao_report_list(unique(names$plain[match(stocks, result$summary$Stock)]), lang)
  }
  not_converged <- unique(d$Stock[!d$converged])
  few <- setdiff(unique(d$Stock[!d$enough_draws]), not_converged)
  fao_report_text("cmsy_mcmc", lang, list(
    chains = set$n_chains, iter = number(set$n_iter), burnin = number(set$n_burnin), thin = set$n_thin,
    rhat = sprintf("%.2f", max(d$rhat, na.rm = TRUE)), neff = number(round(min(d$n_eff, na.rm = TRUE))),
    mcmc_rhat = if (length(not_converged) > 0) fao_report_text("cmsy_mcmc_rhat", lang, list(list = species(not_converged))) else "",
    mcmc_neff = if (length(few) > 0) fao_report_text("cmsy_mcmc_neff", lang, list(list = species(few))) else ""
  ))
}
