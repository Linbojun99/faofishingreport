# Further uses of CMSY++ results: projections under fishing scenarios, comparisons of runs or
# with other assessments, and the priors and results tables of reports.

#' Project CMSY++ Results under Fishing Scenarios
#'
#' This function projects each stock forward from the results of [cmsy_run()] under fishing
#' mortality scenarios (by default 0.5, 0.6, 0.8 and 0.95 Fmsy, as in Froese et al. 2018, and the
#' last-year fishing mortality) or constant catches, and draws the projected B/Bmsy and catch.
#'
#' CMSY++16.R has no projection, so it is done here with the CMSY++ model itself: the Schaefer
#' surplus-production model in which productivity declines linearly below B/k = 0.25 (CMSY++16.R and
#' Froese et al. 2023), started from the posterior draws of r, k and last-year B/k of the run (which
#' come from the same MCMC iterations), with lognormal process error of standard deviation `sigma`
#' (0.1 by default, the value `CMSY++16.R` gives to its unused setting `sigmaR`; the CMSY++ model
#' itself estimates its process error). As in CMSY++, the biomass of a year is the biomass before
#' the catch of that year, F is the catch divided by that biomass, and F/Fmsy uses an Fmsy reduced
#' linearly below half of Bmsy (CMSY++16.R). The catch of the last year of the data is kept, so the
#' biomass of the next year is that of the CMSY++ model; the scenarios set the catch from the first
#' projected year on. In the F scenarios the catch is the scenario F times the biomass of the same
#' year, so F/Fmsy is the scenario value.
#'
#' Stocks whose data end in different years are each projected from their own last year to the
#' same final year (the latest last year plus `years`); the share of stocks at or above Bmsy is
#' given from the latest last year on, when every stock has a value.
#'
#' @inheritParams cmsy_plot
#' @param stocks Optional stocks to project: `Stock` identifiers or species, by English or Chinese
#'        FAO name, scientific name or FAO 3-alpha code. Default is NULL, all stocks that ran.
#' @param silent A logical value. If FALSE (default), a progress bar shows how many stocks have
#'        been projected. If TRUE, nothing is printed. The default can be changed for the session
#'        with `fao_settings(silent = TRUE)`.
#' @param years Number of years to project after the latest last year of the stocks. Default is 10.
#' @param f Fishing mortality scenarios as multiples of Fmsy. Default is `c(0.5, 0.6, 0.8, 0.95)`.
#' @param status_quo A logical value. If TRUE (default), the last-year fishing mortality is also
#'        projected.
#' @param catch Optional constant catches to project, in tonnes per year (e.g. the last-year
#'        catch).
#' @param sigma Standard deviation of the lognormal process error. Default is 0.1.
#' @param seed Optional random seed.
#' @param plot,table Whether charts (`figures/png/cmsy/<stock>/forecast.png` and
#'        `figures/png/cmsy/summary/forecast_share.png`) and the table (`tables/cmsy/`) are saved.
#'        Default is TRUE.
#' @return Invisibly, a list with `projection` (by stock, scenario and year: median B/Bmsy,
#'         F/Fmsy and catch with 95% limits, and the probability that B is at least Bmsy) and
#'         `share` (by scenario and year: share of stocks at or above Bmsy, with 95% limits, and
#'         the number of stocks).
#' @references
#' Froese, R., Winker, H., Coro, G., Demirel, N., Tsikliras, A.C., Dimarchopoulou, D., Scarcella,
#' G., Quaas, M. and Matz-Lueck, N. (2018). Status and rebuilding of European fisheries. Marine
#' Policy 93: 159-170. \doi{10.1016/j.marpol.2018.04.018}
#'
#' Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C., Dimarchopoulou, D.,
#' Touloumis, K., Demirel, N., Vianna, G.M.S., Scarcella, G., Schijns, R., Liang, C. and Pauly,
#' D. (2023). New developments in the analysis of catch time series as the basis for fish stock
#' assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189.
#' \doi{10.3897/aiep.53.105910}
#' @inheritParams summary_params
#' @export
#' @examples
#' \dontrun{
#' # result <- cmsy_run(...), see cmsy_run()
#'
#' # Default scenarios: 0.5, 0.6, 0.8 and 0.95 Fmsy, and the last-year fishing mortality
#' projection <- cmsy_forecast(result, seed = 1)
#'
#' # Twenty years, two F scenarios and a constant catch of 50,000 t, in Chinese
#' projection <- cmsy_forecast(result, years = 20, f = c(0.8, 1), catch = 50000, seed = 1, lang = "zh")
#' last <- projection$projection[projection$projection$Year == max(projection$projection$Year), ]
#' last[, c("Stock", "scenario_label", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "p_above_bmsy")]
#'
#' # Share of the stocks at or above Bmsy (when several stocks were run)
#' projection$share
#' }
cmsy_forecast <- function(result, stocks = NULL, years = 10, f = c(0.5, 0.6, 0.8, 0.95), status_quo = TRUE,
                          catch = NULL, sigma = 0.1, seed = NULL, plot = TRUE, table = TRUE, lang = NULL,
                          annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
                          pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), silent = fao_setting("silent")) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  if (!inherits(result, "cmsy_result")) stop("`result` must be the result of cmsy_run().", call. = FALSE)
  lang <- fao_lang(lang)
  ann <- cmsy_annotations(annotations)
  ok <- result$summary[cmsy_has_results(result$summary$run_status), ]
  if (!is.null(stocks)) ok <- ok[ok$Stock %in% cmsy_quietly(cmsy_match_stocks(stocks, ok), silent), ]
  if (nrow(ok) == 0) stop("No stock has CMSY++ results.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  scenarios <- c(if (length(f) > 0) paste0(f, " Fmsy"), if (status_quo) "status_quo", if (!is.null(catch)) paste0("catch_", cmsy_number(catch)))
  bar <- cmsy_progress(nrow(ok), "Forecast", silent)
  on.exit(bar$close(), add = TRUE)

  # Every stock is projected to the same final year, from its own last year
  first_year <- max(ok$last_year)
  end_year <- first_year + years
  if (length(unique(ok$last_year)) > 1 && !silent) {
    message("The data of the stocks end in different years (", paste(range(ok$last_year), collapse = "-"),
            "): each stock is projected from its own last year to ", end_year,
            ", and the share of stocks at or above Bmsy is given from ", first_year, ".")
  }
  draws_all <- list()
  projection <- do.call(rbind, lapply(seq_len(nrow(ok)), function(i) {
    s <- ok[i, ]
    d <- result$details[[s$Stock]]
    out <- cmsy_project_stock(d$r, d$k, d$bk_last, s$catch_last, s$last_year, end_year - s$last_year, f,
                              status_quo, catch, sigma)
    draws_all[[s$Stock]] <<- out$above
    bar$update(i, s$Stock)
    out <- cbind(Stock = s$Stock, out$summary, stringsAsFactors = FALSE)
    out$scenario_label <- cmsy_scenario_label(out$scenario, s$last_year, lang)
    out
  }))

  # Share of stocks at or above Bmsy in each draw, then its median and 95% limits, in the years
  # in which every stock has a value
  share <- do.call(rbind, lapply(scenarios, function(sc) {
    yrs <- first_year:end_year
    do.call(rbind, lapply(yrs, function(y) {
      m <- do.call(cbind, lapply(draws_all, function(a) a[[sc]][, as.character(y)]))
      n <- min(vapply(draws_all, function(a) nrow(a[[sc]]), numeric(1)))
      share <- 100 * rowMeans(m[seq_len(n), , drop = FALSE])
      data.frame(scenario = sc, Year = y, share = stats::median(share),
                 share_lcl = stats::quantile(share, 0.025, names = FALSE),
                 share_ucl = stats::quantile(share, 0.975, names = FALSE), n_stocks = ncol(m))
    }))
  }))
  share$scenario_label <- if (length(unique(ok$last_year)) == 1) {
    cmsy_scenario_label(share$scenario, first_year, lang)
  } else {
    cmsy_scenario_label(share$scenario, NA, lang)
  }

  if (table) {
    fao_ensure_dirs(file.path("tables", "cmsy"))
    fao_write_csv(projection, file.path("tables", "cmsy", "forecast.csv"))
    fao_write_csv(share, file.path("tables", "cmsy", "forecast_share.csv"))
  }
  plots <- list()
  if (plot) {
    for (i in seq_len(nrow(ok))) {
      s <- ok[i, ]
      p <- cmsy_chart_forecast(projection[projection$Stock == s$Stock, ], result$series[result$series$Stock == s$Stock, ],
                               lang, ann)
      title <- paste0(fao_translate(s$species_en, "species", lang), " (", s$Stock, ")\n", cmsy_label("chart_forecast", lang))
      p <- cmsy_finish(p, ann, title, NULL, cmsy_label("forecast_note", lang))
      fao_save_plot(p, file.path("cmsy", cmsy_safe_name(s$Stock), "forecast"), width = 12, height = 8, pptx = pptx)
      plots[[s$Stock]] <- p
    }
    if (nrow(ok) > 1) {
      p <- ggplot2::ggplot(share, ggplot2::aes(x = Year, y = share, colour = scenario_label, fill = scenario_label)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = share_lcl, ymax = share_ucl), alpha = 0.15, colour = NA) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::scale_colour_manual(values = cmsy_scenario_colors(unique(share$scenario_label)), name = NULL) +
        ggplot2::scale_fill_manual(values = cmsy_scenario_colors(unique(share$scenario_label)), name = NULL) +
        ggplot2::scale_y_continuous(limits = c(0, 100)) +
        ggplot2::labs(x = fao_label("year", lang), y = cmsy_label("pct_above_bmsy", lang)) +
        fao_theme()
      p <- cmsy_finish(p, ann, cmsy_label("chart_forecast_share", lang), NULL, cmsy_label("forecast_note", lang))
      fao_save_plot(p, file.path("cmsy", "summary", "forecast_share"), width = 10, height = 6, pptx = pptx)
      plots$share <- p
    }
  }
  fao_run_result(list(projection = projection, share = share, plots = plots), run, "cmsy_forecast()",
                 main = fao_cols(share[share$Year == max(share$Year), ], c("scenario_label", "Year", "share", "share_lcl", "share_ucl")),
                 note = "Share of the stocks (%) at or above Bmsy in the last year projected:", quiet = silent)
}

# Projection of one stock from aligned posterior draws (CMSY++ process equation). As in CMSY++,
# B_t is the biomass of year t before its catch C_t, B_t+1 = B_t + growth(B_t) - C_t, F_t = C_t / B_t
# and F/Fmsy uses Fmsy = r/2 reduced linearly below half of Bmsy (CMSY++16.R). The last year keeps
# its catch; the scenarios set the catch from the first projected year on.
cmsy_project_stock <- function(r, k, bk, catch_last, last_year, years, f, status_quo, catch, sigma) {
  n <- min(length(r), length(k), length(bk))
  r <- r[seq_len(n)]
  k <- k[seq_len(n)]
  p0 <- bk[seq_len(n)]
  fmsy <- function(p) r / 2 * pmin(1, 4 * p)                    # Fmsy reduced below half of Bmsy
  f_last <- pmin(catch_last / (p0 * k), 5)
  run <- function(f_mult = NA, f_fixed = NULL, c_fixed = NA) {
    p <- p0
    ct <- rep(catch_last, n)
    out_b <- out_c <- out_f <- matrix(NA_real_, n, years + 1)
    for (t in 0:years) {
      if (t > 0) {
        growth <- ifelse(p > 0.25, r * p * (1 - p), 4 * p * r * p * (1 - p))
        p <- pmax(p + growth - ct / k, 0.01) * exp(stats::rnorm(n, 0, sigma))
        ct <- if (!is.na(c_fixed)) pmin(c_fixed, 0.99 * p * k) else
          (if (!is.null(f_fixed)) f_fixed else f_mult * fmsy(p)) * p * k
      }
      out_b[, t + 1] <- 2 * p
      out_c[, t + 1] <- ct
      out_f[, t + 1] <- ct / (p * k) / fmsy(p)
    }
    list(b = out_b, c = out_c, f = out_f)
  }
  scenario_runs <- c(if (length(f) > 0) stats::setNames(lapply(f, function(x) run(f_mult = x)), paste0(f, " Fmsy")),
                     if (status_quo) list(status_quo = run(f_fixed = f_last)),
                     if (!is.null(catch)) stats::setNames(lapply(catch, function(x) run(c_fixed = x)), paste0("catch_", cmsy_number(catch))))
  yrs <- last_year + 0:years
  q <- function(m, p) apply(m, 2, stats::quantile, probs = p, na.rm = TRUE, names = FALSE)
  summary <- do.call(rbind, lapply(names(scenario_runs), function(sc) {
    x <- scenario_runs[[sc]]
    data.frame(scenario = sc, Year = yrs, B_Bmsy = q(x$b, 0.5), B_Bmsy_lcl = q(x$b, 0.025), B_Bmsy_ucl = q(x$b, 0.975),
               F_Fmsy = q(x$f, 0.5), Catch = q(x$c, 0.5), Catch_lcl = q(x$c, 0.025), Catch_ucl = q(x$c, 0.975),
               p_above_bmsy = colMeans(x$b >= 1), stringsAsFactors = FALSE)
  }))
  above <- lapply(scenario_runs, function(x) {
    m <- x$b >= 1
    colnames(m) <- yrs
    m
  })
  list(summary = summary, above = above)
}

cmsy_scenario_label <- function(scenario, last_year, lang) {
  out <- scenario
  out[scenario == "status_quo"] <- if (is.na(last_year)) cmsy_label("status_quo_each", lang) else
    cmsy_label("status_quo", lang, year = last_year)
  is_catch <- startsWith(scenario, "catch_")
  for (i in which(is_catch)) {
    out[i] <- cmsy_label("constant_catch", lang,
                         catch = formatC(as.numeric(sub("catch_", "", scenario[i])), format = "d", big.mark = ","))
  }
  out
}

cmsy_number <- function(x) formatC(x, format = "d")

cmsy_scenario_colors <- function(labels) {
  stats::setNames(c("#1A9641", "#66BD63", "#FDAE61", "#F46D43", "#D7191C", "#2C7BB6", "#7B3294", "#666666")[seq_along(labels)],
                  labels)
}

cmsy_chart_forecast <- function(projection, series, lang, ann) {
  colors <- cmsy_scenario_colors(unique(projection$scenario_label))
  panels <- c(cmsy_label("b_bmsy", lang), cmsy_label("catch_kt", lang))
  past <- rbind(data.frame(panel = panels[1], Year = series$Year, value = series$B_Bmsy),
                data.frame(panel = panels[2], Year = series$Year, value = series$Catch / 1000))
  future <- rbind(
    data.frame(panel = panels[1], Year = projection$Year, scenario = projection$scenario_label,
               value = projection$B_Bmsy, lcl = projection$B_Bmsy_lcl, ucl = projection$B_Bmsy_ucl),
    data.frame(panel = panels[2], Year = projection$Year, scenario = projection$scenario_label,
               value = projection$Catch / 1000, lcl = projection$Catch_lcl / 1000, ucl = projection$Catch_ucl / 1000))
  past$panel <- factor(past$panel, levels = panels)
  future$panel <- factor(future$panel, levels = panels)
  refs <- data.frame(panel = factor(panels[c(1, 1)], levels = panels), y = c(1, 0.8), linetype = c("solid", "dashed"))
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = future, ggplot2::aes(x = Year, ymin = lcl, ymax = ucl, fill = scenario), alpha = 0.12) +
    ggplot2::geom_line(data = past, ggplot2::aes(x = Year, y = value), colour = "black", linewidth = 0.7) +
    ggplot2::geom_line(data = future, ggplot2::aes(x = Year, y = value, colour = scenario), linewidth = 0.8) +
    ggplot2::geom_hline(data = refs, ggplot2::aes(yintercept = y, linetype = linetype), colour = "grey30") +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_colour_manual(values = colors, name = NULL) +
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(x = fao_label("year", lang), y = NULL) +
    fao_theme() + cmsy_strip_theme()
}

#' Compare CMSY++ Runs, or CMSY++ with Other Assessments
#'
#' This function draws B/Bmsy and F/Fmsy of the same stocks from several runs of [cmsy_run()]
#' (e.g. with different start years or priors, a sensitivity analysis) and, optionally, from other
#' assessments, and gives a table of their last-year values.
#'
#' @inheritParams cmsy_plot
#' @param stocks Optional stocks to compare: `Stock` identifiers or species, by English or Chinese
#'        FAO name, scientific name or FAO 3-alpha code. Default is NULL, all stocks that ran.
#' @param pptx A logical value. If TRUE, the charts are also saved as editable PowerPoint files in
#'        figures/pptx/cmsy/compare/. Default is FALSE, which can be changed for all functions with
#'        `fao_settings(pptx = TRUE)`.
#' @param silent A logical value. If TRUE, the messages on how the stocks were matched are not
#'        printed. Default is FALSE, or the value set with `fao_settings(silent = )`.
#' @param ... Results of [cmsy_run()], named or not (e.g. `default = run1, start_1980 = run2`).
#' @param external Optional data frame of other estimates, with columns `Stock`, `Year`, `B_Bmsy`
#'        (and optionally `F_Fmsy`) and `source` (the name shown in the legend, e.g. the agency or
#'        model of the other assessment).
#' @return Invisibly, a list with `last_year` (one row per stock and run) and `plots`.
#' @inheritParams summary_params
#' @export
#' @examples
#' \dontrun{
#' # Sensitivity to the start year
#' run_1950 <- cmsy_run(input, stocks = "CAP_27", seed = 1)
#' run_auto <- cmsy_run(input, stocks = "CAP_27", seed = 1, auto_start = TRUE)
#' cmsy_compare(`from 1950` = run_1950, `CMSY++ start year` = run_auto, annotations = "legend")
#'
#' # Sensitivity to a CMSY++ setting (see cmsy_defaults())
#' run_cv <- cmsy_run(input, stocks = "CAP_27", seed = 1, auto_start = TRUE, settings = list(CV.C = 0.3))
#' cmsy_compare(default = run_auto, `CV.C = 0.3` = run_cv)$last_year
#'
#' # With another assessment of the same stock, read from a file with the columns
#' # Stock, Year, B_Bmsy (optionally F_Fmsy) and source
#' other <- read.csv("other_assessment.csv")
#' cmsy_compare(CMSY = run_auto, external = other, annotations = "legend")
#' }
cmsy_compare <- function(..., stocks = NULL, external = NULL, lang = NULL,
                         annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
                         pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), silent = fao_setting("silent")) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  runs <- list(...)
  if (length(runs) == 0 || !all(vapply(runs, inherits, logical(1), "cmsy_result"))) {
    stop("Give one or more results of cmsy_run().", call. = FALSE)
  }
  if (is.null(names(runs))) names(runs) <- paste("Run", seq_along(runs))
  names(runs)[names(runs) == ""] <- paste("Run", which(names(runs) == ""))
  lang <- fao_lang(lang)
  ann <- cmsy_annotations(annotations)
  series <- do.call(rbind, lapply(names(runs), function(nm) {
    x <- runs[[nm]]$series
    data.frame(Stock = x$Stock, Year = x$Year, B_Bmsy = x$B_Bmsy, F_Fmsy = x$F_Fmsy, source = nm)
  }))
  if (!is.null(external)) {
    if (!all(c("Stock", "Year", "B_Bmsy", "source") %in% names(external))) {
      stop("`external` needs the columns Stock, Year, B_Bmsy and source.", call. = FALSE)
    }
    if (!"F_Fmsy" %in% names(external)) external$F_Fmsy <- NA_real_
    series <- rbind(series, external[, c("Stock", "Year", "B_Bmsy", "F_Fmsy", "source")])
  }
  all_summaries <- do.call(rbind, lapply(runs, function(x) x$summary[, c("Stock", "alpha3")]))
  if (!is.null(stocks)) series <- series[series$Stock %in% cmsy_quietly(cmsy_match_stocks(stocks, unique(all_summaries)), silent), ]
  if (nrow(series) == 0) stop("No stock to compare.", call. = FALSE)
  last <- do.call(rbind, lapply(split(series, list(series$Stock, series$source), drop = TRUE), function(x) x[which.max(x$Year), ]))
  rownames(last) <- NULL

  plots <- list()
  panels <- c(cmsy_label("b_bmsy", lang), cmsy_label("f_fmsy", lang))
  for (stock in unique(series$Stock)) {
    x <- series[series$Stock == stock, ]
    long <- rbind(data.frame(panel = panels[1], Year = x$Year, value = x$B_Bmsy, source = x$source),
                  data.frame(panel = panels[2], Year = x$Year, value = x$F_Fmsy, source = x$source))
    long <- long[is.finite(long$value), ]
    long$panel <- factor(long$panel, levels = panels)
    p <- ggplot2::ggplot(long, ggplot2::aes(x = Year, y = value, colour = source)) +
      ggplot2::geom_hline(yintercept = 1, colour = "grey30") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_colour_manual(values = cmsy_scenario_colors(unique(long$source)), name = NULL) +
      ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "left") +
      ggplot2::coord_cartesian(ylim = c(0, NA)) +
      ggplot2::labs(x = fao_label("year", lang), y = NULL) +
      fao_theme() + cmsy_strip_theme()
    p <- cmsy_finish(p, ann, paste0(stock, "\n", cmsy_label("chart_compare", lang)), NULL, NULL)
    fao_save_plot(p, file.path("cmsy", "compare", cmsy_safe_name(stock)), width = 10, height = 8, pptx = pptx)
    plots[[stock]] <- p
  }
  fao_run_result(list(last_year = last, plots = plots), run, "cmsy_compare()",
                 main = fao_cols(last, c("Stock", "source", "Year", "B_Bmsy", "F_Fmsy")),
                 note = "B/Bmsy and F/Fmsy of each run in its last year:", rows = 20, quiet = silent)
}

#' Tables of CMSY++ Priors and Results for Reports
#'
#' This function makes the two tables that CMSY++ studies usually give: the priors of each stock
#' (years used, r and k ranges, MSY prior, and the B/k priors of the first, intermediate and last
#' years, with their sources) and the results (r, k, MSY, Fmsy and Bmsy with their 95%
#' confidence limits, last-year B/Bmsy and F/Fmsy, Kobe probabilities and status). Values are
#' rounded and ranges written as "low-high", with the column names in the chosen language.
#'
#' @inheritParams cmsy_plot
#' @param stocks Optional stocks to put in the tables: `Stock` identifiers or species, by English or
#'        Chinese FAO name, scientific name or FAO 3-alpha code. Default is NULL, all stocks that ran.
#' @param lang The language of the column names and other names of the tables: "en" (English),
#'        "zh" (Chinese) or "both". Default is NULL, which uses the language set with
#'        `fao_settings()` ("en" unless changed).
#' @param silent A logical value. If TRUE, the messages on how the stocks were matched are not
#'        printed. Default is FALSE, or the value set with `fao_settings(silent = )`.
#' @param table A logical value. If TRUE (default), the tables are saved in `tables/cmsy/`.
#' @return Invisibly, a list with the data frames `priors` and `results`. The results give the
#'         status in the two classes of FAO (2026) for catch-only results and in the five classes
#'         of Palomares et al. (2018).
#' @references
#' FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome, FAO. Section "The status
#' of fishery resources", Box 1.3 (assessment tiers and status classes).
#' \url{https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html}
#'
#' Palomares, M.L.D., Froese, R., Derrick, B., Noel, S.-L., Tsui, G., Woroniak, J. and Pauly, D.
#' (2018). A preliminary global assessment of the status of exploited marine fish and
#' invertebrate populations. A report prepared by the Sea Around Us for OCEANA. University of
#' British Columbia and GEOMAR. \url{https://www.fishbase.de/rfroese/OceanaFinalReport_V7a.pdf}
#' @export
#' @examples
#' \dontrun{
#' # result <- cmsy_run(...), see cmsy_run()
#' tables <- cmsy_tables(result, lang = "zh")       # also saved in tables/cmsy/
#' tables$priors
#' tables$results
#'
#' # Column names in both languages, without saving
#' cmsy_tables(result, lang = "both", table = FALSE)$results
#' }
cmsy_tables <- function(result, stocks = NULL, table = TRUE, lang = NULL, silent = fao_setting("silent")) {
  run <- fao_run_begin()
  on.exit(fao_run_end(run), add = TRUE)
  if (!inherits(result, "cmsy_result")) stop("`result` must be the result of cmsy_run().", call. = FALSE)
  lang <- fao_lang(lang)
  s <- result$summary[cmsy_has_results(result$summary$run_status), ]
  if (!is.null(stocks)) s <- s[s$Stock %in% cmsy_quietly(cmsy_match_stocks(stocks, s), silent), ]
  r3 <- function(x) formatC(x, digits = 3, format = "fg")
  big <- function(x) format(round(x), big.mark = ",", trim = TRUE)
  ci <- function(est, lcl, ucl, f = r3) paste0(f(est), " (", f(lcl), "-", f(ucl), ")")
  range <- function(lo, hi, f = r3) paste0(f(lo), "-", f(hi))
  name <- fao_translate(s$species_en, "species", lang)
  source_text <- function(x) ifelse(x == "user", cmsy_label("source_user", lang), cmsy_label("source_nn", lang))
  priors <- data.frame(s$Stock, name, s$scientific_name, paste0(s$first_year, "-", s$last_year),
                       range(s$r_prior_low, s$r_prior_high), s$r_source,
                       range(s$k_prior_low, s$k_prior_high, big), big(s$MSY_prior),
                       paste0(range(s$start_bk_prior_low, s$start_bk_prior_high), " (", source_text(s$start_bk_prior_source), ")"),
                       paste0(s$int_year, ": ", range(s$int_bk_prior_low, s$int_bk_prior_high), " (", source_text(s$int_bk_prior_source), ")"),
                       paste0(range(s$end_bk_prior_low, s$end_bk_prior_high), " (", source_text(s$end_bk_prior_source), ")"),
                       s$url, stringsAsFactors = FALSE)
  names(priors) <- vapply(c("t_stock", "t_species", "t_scientific", "t_years", "t_r_prior", "t_r_source", "t_k_prior",
                            "t_msy_prior", "t_start_bk", "t_int_bk", "t_end_bk", "t_url"), cmsy_label, character(1), lang = lang)
  results <- data.frame(s$Stock, name, ci(s$r, s$r_lcl, s$r_ucl), ci(s$k, s$k_lcl, s$k_ucl, big),
                        ci(s$MSY, s$MSY_lcl, s$MSY_ucl, big), ci(s$Fmsy, s$Fmsy_lcl, s$Fmsy_ucl),
                        ci(s$Bmsy, s$Bmsy_lcl, s$Bmsy_ucl, big), ci(s$B_Bmsy, s$B_Bmsy_lcl, s$B_Bmsy_ucl),
                        ci(s$F_Fmsy, s$F_Fmsy_lcl, s$F_Fmsy_ucl), sprintf("%.0f%%", 100 * s$p_green),
                        sprintf("%.0f%%", 100 * s$p_red), cmsy_status_label(s$status, lang),
                        as.character(cmsy_status_class(s$B_Bmsy, "palomares", lang)), stringsAsFactors = FALSE)
  names(results) <- vapply(c("t_stock", "t_species", "t_r", "t_k", "t_msy", "t_fmsy", "t_bmsy", "t_b_bmsy", "t_f_fmsy",
                             "t_p_green", "t_p_red", "t_status_fao", "t_status_palomares"), cmsy_label, character(1), lang = lang)
  if (table) {
    fao_ensure_dirs(file.path("tables", "cmsy"))
    fao_write_csv(priors, file.path("tables", "cmsy", "cmsy_priors_table.csv"))
    fao_write_csv(results, file.path("tables", "cmsy", "cmsy_results_table.csv"))
  }
  fao_run_result(list(priors = priors, results = results), run, "cmsy_tables()", main = results,
                 note = "Results with their 95% limits:", rows = 20, quiet = silent)
}

utils::globalVariables(c("scenario_label", "share", "share_lcl", "share_ucl", "source", "value"))
