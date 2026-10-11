# Charts of CMSY++ results in the style of the package, in English, Chinese or both: the charts of
# the CMSY++ user guide redrawn from the saved results, and summaries of several stocks.

cmsy_stock_charts <- c("catch", "rk", "biomass", "b_bmsy", "exploitation", "equilibrium",
                       "management", "kobe", "prior_posterior", "retrospective")
cmsy_summary_charts <- c("overview", "kobe_all", "status", "trend")
cmsy_annotation_types <- c("title", "subtitle", "caption", "legend", "labels")

#' Charts of CMSY++ Results
#'
#' This function draws the results of [cmsy_run()]. For each stock it draws the charts of the
#' CMSY++ user guide (Froese et al. 2021), from the results saved by the run:
#' \describe{
#'   \item{catch}{catch with the MSY estimate and its 95\% confidence band (user guide Figure 1A,
#'     2 upper left);}
#'   \item{rk}{the posterior r-k pairs (log scales), the prior ranges of r and k (dotted box) and
#'     the most likely pair with its 95\% limits (Figure 1B, C);}
#'   \item{biomass}{relative biomass B/k with its 95\% band, the B/k prior ranges of the first,
#'     intermediate and last years (dashed if from the neural network, solid if given by the
#'     user), Bmsy (B/k = 0.5) and half of Bmsy (B/k = 0.25, reduced recruitment) (Figure 1D);}
#'   \item{b_bmsy}{relative stock size B/Bmsy with its 95\% band and lines at Bmsy, 0.8 Bmsy (FAO
#'     limit of overfished) and 0.5 Bmsy (Figure 2 upper right);}
#'   \item{exploitation}{F/Fmsy with its 95\% band and Fmsy (Figure 1E, 2 lower left);}
#'   \item{equilibrium}{the Schaefer equilibrium curve of catch/MSY against B/k, indented below
#'     B/k = 0.25, with the predicted path from the first (square) to the last year (triangle)
#'     (Figure 1F);}
#'   \item{management}{catch, B/Bmsy and F/Fmsy over time in one chart (Figure 2);}
#'   \item{kobe}{Kobe plot: the path of B/Bmsy and F/Fmsy, the last-year posterior draws with
#'     the 50, 80 and 95\% probability regions, and the probability of each quadrant (Figure 3);}
#'   \item{prior_posterior}{prior and posterior densities of r, k, MSY and B/k in the first,
#'     intermediate and last years, with the prior-posterior variance ratio PPVR (Figure 4);}
#'   \item{retrospective}{B/Bmsy and F/Fmsy when the last one, two or three years are left out,
#'     with Mohn's rho (Figure 5; needs `cmsy_run(plots = "retrospective")`).}
#' }
#' For several stocks together:
#' \describe{
#'   \item{overview}{last-year B/Bmsy of each stock with its 95\% limits, coloured by status;}
#'   \item{kobe_all}{last-year B/Bmsy and F/Fmsy of all stocks in a Kobe plot;}
#'   \item{status}{share of stocks and of last-year catch by status;}
#'   \item{trend}{median B/Bmsy of the stocks by year (with the interquartile range) and the share
#'     of stocks below 0.8 Bmsy.}
#' }
#'
#' By default the charts show only the axis titles and axis labels, so they can be used directly
#' in reports with captions of their own. Titles, subtitles, captions (method, data and prior
#' sources), legends and text inside the charts (probabilities, PPVR, Mohn's rho) are added with
#' `annotations`.
#'
#' Charts are saved in `figures/png/cmsy/<stock>/` and `figures/png/cmsy/summary/` (and as
#' PowerPoint when `pptx` is TRUE). CMSY++'s own JPEG charts are in the folder of each stock (see
#' `result$summary$folder`).
#'
#' @inheritParams summary_params
#' @param result The result of [cmsy_run()].
#' @param stocks Optional stocks to draw: `Stock` identifiers or species, by English or Chinese
#'        FAO name, scientific name or FAO 3-alpha code. Default is NULL, all stocks that ran.
#' @param charts The charts to draw (see Details). Default is all of them.
#' @param annotations The text added to the axis titles and labels: FALSE (default) none, TRUE
#'        all, or any of "title", "subtitle", "caption", "legend" and "labels" (text inside the
#'        chart). The default can be changed for the session with
#'        `options(faofishingreport.cmsy_annotations = TRUE)`.
#' @return Invisibly, a named list of the ggplot objects.
#' @references
#' Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for CMSY++. GEOMAR,
#' Germany, 17 pp. \url{https://oceanrep.geomar.de/id/eprint/52147/}
#'
#' Mohn, R. (1999). The retrospective problem in sequential population analysis: an
#' investigation using cod fishery and simulated data. ICES Journal of Marine Science 56:
#' 473-488. \doi{10.1006/jmsc.1999.0481}
#' @export
#' @examples
#' \dontrun{
#' cmsy_plot(result, lang = "zh")
#' cmsy_plot(result, stocks = "Sardina pilchardus", charts = c("kobe", "prior_posterior"),
#'           annotations = TRUE, lang = "both")
#' }
cmsy_plot <- function(result, stocks = NULL, charts = c(cmsy_stock_charts, cmsy_summary_charts),
                      annotations = getOption("faofishingreport.cmsy_annotations", FALSE), lang = NULL,
                      pptx = getOption("faofishingreport.pptx", FALSE)) {
  if (!inherits(result, "cmsy_result")) stop("`result` must be the result of cmsy_run().", call. = FALSE)
  charts <- match.arg(charts, c(cmsy_stock_charts, cmsy_summary_charts), several.ok = TRUE)
  ann <- cmsy_annotations(annotations)
  lang <- fao_lang(lang)
  ok <- result$summary[result$summary$run_status == "ok", ]
  if (!is.null(stocks)) ok <- ok[ok$Stock %in% cmsy_match_stocks(stocks, ok), ]
  if (nrow(ok) == 0) stop("No stock has CMSY++ results.", call. = FALSE)
  ok$species_name <- fao_translate(ok$species_en, "species", lang)
  plots <- list()

  for (i in seq_len(nrow(ok))) {
    s <- ok[i, ]
    series <- result$series[result$series$Stock == s$Stock, ]
    d <- result$details[[s$Stock]]
    title <- paste0(s$species_name, " - ", fao_translate(s$area, "area", lang), " (", s$Stock, ")")
    caption <- paste0(cmsy_label("caption", lang), "\n", cmsy_label("priors", lang), ": ", s$r_source,
                      " (", s$url, ")")
    for (chart in intersect(charts, cmsy_stock_charts)) {
      p <- switch(chart,
                  catch = cmsy_chart_catch(s, series, lang, ann),
                  rk = cmsy_chart_rk(s, d, lang, ann),
                  biomass = cmsy_chart_biomass(s, series, d, lang, ann),
                  b_bmsy = cmsy_chart_b_bmsy(series, lang, ann),
                  exploitation = cmsy_chart_exploitation(series, lang, ann),
                  equilibrium = cmsy_chart_equilibrium(s, series, lang, ann),
                  management = cmsy_chart_management(s, series, lang, ann),
                  kobe = cmsy_chart_kobe(s, series, result$kobe[result$kobe$Stock == s$Stock, ], lang, ann),
                  prior_posterior = cmsy_chart_pp(s, d, lang, ann),
                  retrospective = cmsy_chart_retro(d, lang, ann))
      if (is.null(p)) next
      subtitle <- cmsy_label("subtitle", lang, b = sprintf("%.2f (%.2f-%.2f)", s$B_Bmsy, s$B_Bmsy_lcl, s$B_Bmsy_ucl),
                             f = sprintf("%.2f", s$F_Fmsy), status = cmsy_status_label(s$status, lang),
                             msy = format(round(s$MSY), big.mark = ","))
      p <- cmsy_finish(p, ann, paste0(title, "\n", cmsy_label(paste0("chart_", chart), lang)), subtitle, caption)
      size <- cmsy_chart_size(chart)
      fao_save_plot(p, file.path("cmsy", cmsy_safe_name(s$Stock), chart), width = size[1], height = size[2], pptx = pptx)
      plots[[paste0(s$Stock, "_", chart)]] <- p
    }
  }

  summary_charts <- intersect(charts, cmsy_summary_charts)
  if (length(summary_charts) > 0 && nrow(ok) > 1) {
    caption <- paste0(cmsy_label("caption", lang), "\n", cmsy_label("n_stocks", lang, n = nrow(ok)))
    for (chart in summary_charts) {
      p <- switch(chart,
                  overview = cmsy_chart_overview(ok, lang, ann),
                  kobe_all = cmsy_chart_kobe_all(ok, lang, ann),
                  status = cmsy_chart_status(ok, lang, ann),
                  trend = cmsy_chart_trend(result$series[result$series$Stock %in% ok$Stock, ], lang, ann))
      p <- cmsy_finish(p, ann, cmsy_label(paste0("chart_", chart), lang), NULL, caption)
      height <- if (chart == "overview") max(5, 0.3 * nrow(ok) + 2) else cmsy_chart_size(chart)[2]
      fao_save_plot(p, file.path("cmsy", "summary", chart), width = cmsy_chart_size(chart)[1], height = height,
                    pptx = pptx)
      plots[[paste0("summary_", chart)]] <- p
    }
  }
  invisible(plots)
}

# Which annotations to add
cmsy_annotations <- function(x) {
  if (isTRUE(x)) return(cmsy_annotation_types)
  if (is.null(x) || isFALSE(x) || length(x) == 0) return(character(0))
  match.arg(x, cmsy_annotation_types, several.ok = TRUE)
}

# Adds the annotations asked for; without "legend" the legend is removed
cmsy_finish <- function(p, ann, title, subtitle, caption) {
  p <- p + ggplot2::labs(title = if ("title" %in% ann) title, subtitle = if ("subtitle" %in% ann) subtitle,
                         caption = if ("caption" %in% ann) caption)
  if (!"legend" %in% ann) p <- p + ggplot2::theme(legend.position = "none")
  p
}

cmsy_chart_size <- function(chart) {
  switch(chart, management = c(9, 10), prior_posterior = c(12, 7.5), retrospective = c(12, 5),
         kobe = c(8, 7), kobe_all = c(9, 8), rk = c(8, 7), status = c(10, 5), trend = c(10, 8), c(10, 6))
}

# Facet strips placed like axis titles, so panels need no annotations
cmsy_strip_theme <- function() {
  ggplot2::theme(strip.placement = "outside", strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(face = "bold"))
}

cmsy_status_label <- function(status, lang) {
  ifelse(status == "overfished", cmsy_label("status_overfished", lang), cmsy_label("status_msf", lang))
}

cmsy_status_colors <- function(lang) {
  stats::setNames(c("#1A9641", "#D7191C"), c(cmsy_label("status_msf", lang), cmsy_label("status_overfished", lang)))
}

cmsy_chart_catch <- function(s, series, lang, ann) {
  p <- ggplot2::ggplot(series, ggplot2::aes(x = Year)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = s$MSY_lcl / 1000, ymax = s$MSY_ucl / 1000,
                      fill = "grey85") +
    ggplot2::geom_hline(yintercept = s$MSY / 1000, colour = "#7B3294", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = Catch / 1000, colour = "catch"), linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = Catch_smooth / 1000, colour = "smooth"), linewidth = 0.5) +
    ggplot2::scale_colour_manual(values = c(catch = "black", smooth = "#2171B5"),
                                 labels = c(catch = cmsy_label("catch_observed", lang),
                                            smooth = cmsy_label("catch_smooth", lang)), name = NULL) +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(series$Year)) +
    ggplot2::labs(x = fao_label("year", lang), y = cmsy_label("catch_kt", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::annotate("text", x = min(series$Year), y = s$MSY / 1000, vjust = -0.5, hjust = 0,
                               label = paste0("MSY = ", format(round(s$MSY / 1000, 1), big.mark = ","), " (",
                                              round(s$MSY_lcl / 1000, 1), "-", round(s$MSY_ucl / 1000, 1), ")"),
                               family = fao_font(), size = 3.5)
  }
  p
}

cmsy_chart_rk <- function(s, d, lang, ann) {
  draws <- data.frame(r = d$r, k = d$k / 1000)
  if (nrow(draws) > 5000) draws <- draws[sample.int(nrow(draws), 5000), ]
  best <- data.frame(r = s$r, k = s$k / 1000, r_lcl = s$r_lcl, r_ucl = s$r_ucl, k_lcl = s$k_lcl / 1000, k_ucl = s$k_ucl / 1000)
  p <- ggplot2::ggplot(draws, ggplot2::aes(x = r, y = k)) +
    ggplot2::geom_point(colour = "grey45", alpha = 0.25, size = 0.6) +
    ggplot2::annotate("rect", xmin = d$prior_r[1], xmax = d$prior_r[2], ymin = d$prior_k[1] / 1000,
                      ymax = d$prior_k[2] / 1000, fill = NA, colour = "black", linetype = "dotted") +
    ggplot2::geom_errorbar(data = best, ggplot2::aes(x = r, ymin = k_lcl, ymax = k_ucl), width = 0, colour = "#08519C",
                           inherit.aes = FALSE) +
    ggplot2::geom_errorbar(data = best, ggplot2::aes(y = k, xmin = r_lcl, xmax = r_ucl), width = 0, colour = "#08519C",
                           orientation = "y", inherit.aes = FALSE) +
    ggplot2::geom_point(data = best, colour = "#08519C", size = 3) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::labs(x = cmsy_label("r", lang), y = cmsy_label("k_kt", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5, family = fao_font(), size = 3.5,
                               label = sprintf("r = %.3f (%.3f-%.3f)\nk = %s", s$r, s$r_lcl, s$r_ucl,
                                               format(round(s$k / 1000), big.mark = ",")))
  }
  p
}

cmsy_chart_biomass <- function(s, series, d, lang, ann) {
  priors <- data.frame(
    Year = c(min(series$Year), d$int_year, max(series$Year)),
    low = c(d$startbio[1], d$intbio[1], d$endbio[1]), high = c(d$startbio[2], d$intbio[2], d$endbio[2]),
    user = c(d$expert[["start"]], d$expert[["int"]], d$expert[["end"]])
  )
  priors <- priors[!is.na(priors$Year) & !is.na(priors$low), ]
  priors$linetype <- ifelse(priors$user, "solid", "dashed")
  p <- ggplot2::ggplot(series, ggplot2::aes(x = Year, y = B_k)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = B_k_lcl, ymax = B_k_ucl), fill = "grey80") +
    ggplot2::geom_line(colour = "#08519C", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = c(0.5, 0.25), linetype = c("dashed", "dotted"), colour = "grey30") +
    ggplot2::geom_linerange(data = priors, ggplot2::aes(x = Year, ymin = low, ymax = high, linetype = linetype),
                            colour = "#7B3294", linewidth = 1.2, inherit.aes = FALSE) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(series$Year)) +
    ggplot2::coord_cartesian(ylim = c(0, max(1, series$B_k_ucl, priors$high, na.rm = TRUE))) +
    ggplot2::labs(x = fao_label("year", lang), y = cmsy_label("b_k", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::annotate("text", x = max(series$Year), y = c(0.5, 0.25), label = c("Bmsy", "0.5 Bmsy"),
                               hjust = 1, vjust = -0.4, family = fao_font(), size = 3.2)
  }
  p
}

cmsy_chart_b_bmsy <- function(series, lang, ann) {
  p <- ggplot2::ggplot(series, ggplot2::aes(x = Year, y = B_Bmsy)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = B_Bmsy_lcl, ymax = B_Bmsy_ucl), fill = "grey80") +
    ggplot2::geom_line(colour = "#08519C", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = c(1, 0.8, 0.5), linetype = c("solid", "dashed", "dotted"), colour = "grey30") +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(series$Year)) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(x = fao_label("year", lang), y = cmsy_label("b_bmsy", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::annotate("text", x = max(series$Year), y = c(1, 0.8, 0.5),
                               label = c("Bmsy", cmsy_label("fao_limit", lang), "0.5 Bmsy"),
                               hjust = 1, vjust = -0.4, family = fao_font(), size = 3.2)
  }
  p
}

cmsy_chart_exploitation <- function(series, lang, ann) {
  ggplot2::ggplot(series, ggplot2::aes(x = Year, y = F_Fmsy)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = F_Fmsy_lcl, ymax = F_Fmsy_ucl), fill = "grey80") +
    ggplot2::geom_line(colour = "#08519C", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey30") +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(series$Year)) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(x = fao_label("year", lang), y = cmsy_label("f_fmsy", lang)) +
    fao_theme()
}

# Equilibrium catch/MSY at B/k, with the reduced productivity below B/k = 0.25 used by CMSY++
# (CMSY++16.R, panel F of the analysis chart)
cmsy_equilibrium_curve <- function(x) {
  correction <- ifelse(x > 0.25, 1, ifelse(x > 0.125, 4 * x, exp(-10 * (0.125 - x)) * 4 * x))
  (4 * x - (2 * x)^2) * correction
}

cmsy_chart_equilibrium <- function(s, series, lang, ann) {
  curve <- data.frame(x = seq(0, 1, by = 0.002))
  curve$y <- cmsy_equilibrium_curve(curve$x)
  path <- data.frame(x = series$B_k, y = series$Catch_pred / s$MSY, Year = series$Year)
  ends <- path[c(1, nrow(path)), ]
  ends$shape <- c(22, 24)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = curve, ggplot2::aes(x = x, y = y), colour = "black") +
    ggplot2::geom_path(data = path, ggplot2::aes(x = x, y = y), colour = "#08519C") +
    ggplot2::geom_point(data = ends, ggplot2::aes(x = x, y = y, shape = shape), size = 3, fill = "white", colour = "#08519C") +
    ggplot2::scale_shape_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, max(1.2, path$y, na.rm = TRUE))) +
    ggplot2::labs(x = cmsy_label("b_k", lang), y = cmsy_label("catch_msy", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::geom_text(data = ends, ggplot2::aes(x = x, y = y, label = Year), vjust = -1,
                                family = fao_font(), size = 3.2)
  }
  p
}

cmsy_chart_management <- function(s, series, lang, ann) {
  panels <- c(cmsy_label("catch_kt", lang), cmsy_label("b_bmsy", lang), cmsy_label("f_fmsy", lang))
  long <- rbind(
    data.frame(Year = series$Year, panel = panels[1], value = series$Catch / 1000, lcl = NA_real_, ucl = NA_real_),
    data.frame(Year = series$Year, panel = panels[2], value = series$B_Bmsy, lcl = series$B_Bmsy_lcl, ucl = series$B_Bmsy_ucl),
    data.frame(Year = series$Year, panel = panels[3], value = series$F_Fmsy, lcl = series$F_Fmsy_lcl, ucl = series$F_Fmsy_ucl)
  )
  long$panel <- factor(long$panel, levels = panels)
  refs <- data.frame(panel = factor(panels[c(1, 1, 1, 2, 2, 2, 3)], levels = panels),
                     y = c(s$MSY / 1000, s$MSY_lcl / 1000, s$MSY_ucl / 1000, 1, 0.8, 0.5, 1),
                     linetype = c("dashed", "dotted", "dotted", "solid", "dashed", "dotted", "solid"))
  ggplot2::ggplot(long, ggplot2::aes(x = Year, y = value)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), fill = "grey80", na.rm = TRUE) +
    ggplot2::geom_line(colour = "#08519C", linewidth = 0.8) +
    ggplot2::geom_hline(data = refs, ggplot2::aes(yintercept = y, linetype = linetype), colour = "grey30") +
    ggplot2::scale_linetype_identity() +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(long$Year)) +
    ggplot2::labs(x = fao_label("year", lang), y = NULL) +
    fao_theme() + cmsy_strip_theme()
}

cmsy_chart_kobe <- function(s, series, kobe, lang, ann) {
  top <- max(2, stats::quantile(c(series$F_Fmsy, kobe$F_Fmsy), 0.99, na.rm = TRUE) * 1.05)
  right <- max(2, stats::quantile(c(series$B_Bmsy, kobe$B_Bmsy), 0.99, na.rm = TRUE) * 1.05)
  last <- series[nrow(series), ]
  p <- cmsy_kobe_base(top, right) +
    ggplot2::geom_point(data = kobe, ggplot2::aes(x = B_Bmsy, y = F_Fmsy), colour = "grey20", alpha = 0.06, size = 0.5)
  regions <- cmsy_kobe_regions(kobe, right, top)
  if (!is.null(regions)) {
    p <- p + ggplot2::geom_contour(data = regions$grid, ggplot2::aes(x = x, y = y, z = z), breaks = regions$levels,
                                   colour = "grey15", linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_path(data = series, ggplot2::aes(x = B_Bmsy, y = F_Fmsy), colour = "black") +
    ggplot2::geom_point(data = series[1, ], ggplot2::aes(x = B_Bmsy, y = F_Fmsy), shape = 22, size = 3, fill = "white") +
    ggplot2::geom_point(data = last, ggplot2::aes(x = B_Bmsy, y = F_Fmsy), shape = 24, size = 3.5, fill = "white") +
    ggplot2::labs(x = cmsy_label("b_bmsy", lang), y = cmsy_label("f_fmsy", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    probs <- data.frame(x = c(right, 0, 0, right), y = c(0, top, 0, top), hjust = c(1, 0, 0, 1), vjust = c(0, 1, 0, 1),
                        label = sprintf("%.0f%%", 100 * c(s$p_green, s$p_red, s$p_yellow, s$p_orange)))
    p <- p + ggplot2::geom_text(data = probs, ggplot2::aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
                                family = fao_font(), size = 4, fontface = "bold", nudge_x = 0) +
      ggplot2::annotate("text", x = last$B_Bmsy, y = last$F_Fmsy, label = last$Year, vjust = -1.2,
                        family = fao_font(), size = 3.2)
  }
  p
}

# Kobe quadrants (green, yellow, orange, red as in the CMSY++ user guide) and limits
cmsy_kobe_base <- function(top, right) {
  quadrants <- data.frame(xmin = c(1, 0, 0, 1), xmax = c(right, 1, 1, right), ymin = c(0, 1, 0, 1), ymax = c(1, top, 1, top),
                          fill = c("#2ca25f", "#e34a33", "#fee08b", "#fdae61"))
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = quadrants, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
                       alpha = 0.55) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey30") +
    ggplot2::coord_cartesian(xlim = c(0, right), ylim = c(0, top), expand = FALSE)
}

# 50%, 80% and 95% highest-density regions of the last-year Kobe draws (2D kernel density)
cmsy_kobe_regions <- function(kobe, right, top) {
  kobe <- kobe[is.finite(kobe$B_Bmsy) & is.finite(kobe$F_Fmsy), ]
  if (nrow(kobe) < 50 || !requireNamespace("MASS", quietly = TRUE)) return(NULL)
  dens <- MASS::kde2d(kobe$B_Bmsy, kobe$F_Fmsy, n = 120, lims = c(0, right, 0, top))
  z <- sort(as.vector(dens$z), decreasing = TRUE)
  mass <- cumsum(z) / sum(z)
  levels <- vapply(c(0.95, 0.8, 0.5), function(p) z[which(mass >= p)[1]], numeric(1))
  list(grid = data.frame(x = rep(dens$x, times = length(dens$y)), y = rep(dens$y, each = length(dens$x)),
                         z = as.vector(dens$z)),
       levels = unique(levels))
}

# Prior and posterior densities (user guide Figure 4); PPVR = (CV posterior / CV prior)^2
cmsy_chart_pp <- function(s, d, lang, ann) {
  if (is.null(d$prior_logn) || is.null(d$bk_beta)) return(NULL)
  lp <- d$prior_logn
  log_k <- lp[["mean_log_k"]] + log(1000)                    # k in tonnes
  sd_msy <- sqrt(lp[["sd_log_r"]]^2 + lp[["sd_log_k"]]^2 + 2 * lp[["cor_log_rk"]] * lp[["sd_log_r"]] * lp[["sd_log_k"]])
  lognormal <- function(meanlog, sdlog) {
    list(density = function(x) stats::dlnorm(x, meanlog, sdlog),
         range = stats::qlnorm(c(0.001, 0.999), meanlog, sdlog), cv = sqrt(exp(sdlog^2) - 1))
  }
  beta <- function(j) {
    a <- d$bk_beta[1, j]
    b <- d$bk_beta[2, j]
    list(density = function(x) stats::dbeta(x, a, b), range = c(0, 1),
         cv = sqrt(a * b / ((a + b)^2 * (a + b + 1))) / (a / (a + b)))
  }
  years <- c(s$first_year, d$int_year, s$last_year)
  params <- list(
    list(name = cmsy_label("r", lang), post = d$r, prior = lognormal(lp[["mean_log_r"]], lp[["sd_log_r"]]), scale = 1),
    list(name = cmsy_label("k_kt", lang), post = d$k, prior = lognormal(log_k, lp[["sd_log_k"]]), scale = 1000),
    list(name = cmsy_label("msy_kt", lang), post = d$r * d$k / 4,
         prior = lognormal(lp[["mean_log_r"]] + log_k - log(4), sd_msy), scale = 1000),
    list(name = paste0("B/k ", years[1]), post = d$bk_first, prior = beta(1), scale = 1),
    list(name = paste0("B/k ", years[2]), post = d$bk_int, prior = beta(2), scale = 1),
    list(name = paste0("B/k ", years[3]), post = d$bk_last, prior = beta(3), scale = 1)
  )
  params <- Filter(function(p) length(p$post) > 1, params)
  curves <- do.call(rbind, lapply(params, function(p) {
    post <- stats::density(p$post, adjust = 2)
    x <- seq(min(p$prior$range[1], min(post$x)), max(p$prior$range[2], max(post$x)), length.out = 400)
    rbind(data.frame(panel = p$name, type = "prior", x = x / p$scale, y = p$prior$density(x) * p$scale),
          data.frame(panel = p$name, type = "posterior", x = post$x / p$scale, y = post$y * p$scale))
  }))
  curves$panel <- factor(curves$panel, levels = vapply(params, `[[`, character(1), "name"))
  curves$type <- factor(curves$type, levels = c("prior", "posterior"),
                        labels = c(cmsy_label("prior", lang), cmsy_label("posterior", lang)))
  p <- ggplot2::ggplot(curves, ggplot2::aes(x = x, y = y, fill = type)) +
    ggplot2::geom_area(position = "identity", colour = "grey30", linewidth = 0.3, alpha = 0.8) +
    ggplot2::scale_fill_manual(values = stats::setNames(c("grey85", "grey45"), levels(curves$type)), name = NULL) +
    ggplot2::facet_wrap(~panel, scales = "free", ncol = 3, strip.position = "bottom") +
    ggplot2::labs(x = NULL, y = cmsy_label("density", lang)) +
    fao_theme() + cmsy_strip_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  if ("labels" %in% ann) {
    ppvr <- data.frame(panel = factor(vapply(params, `[[`, character(1), "name"), levels = levels(curves$panel)),
                       label = vapply(params, function(p) {
                         sprintf("PPVR = %.2f", (stats::sd(p$post) / mean(p$post))^2 / p$prior$cv^2)
                       }, character(1)))
    p <- p + ggplot2::geom_text(data = ppvr, ggplot2::aes(x = Inf, y = Inf, label = label), hjust = 1.1, vjust = 1.5,
                                inherit.aes = FALSE, family = fao_font(), size = 3.5)
  }
  p
}

# Retrospective analysis (user guide Figure 5) with Mohn's rho of the last year of each peel
cmsy_chart_retro <- function(d, lang, ann) {
  r <- d$retrospective
  if (is.null(r)) return(NULL)
  full_years <- r$years[[1]]
  peels <- lapply(seq_along(r$B_Bmsy), function(i) {
    n <- length(full_years) - (i - 1)
    if (length(r$B_Bmsy[[i]]) < 2 || n < 2) return(NULL)
    years <- full_years[seq_len(n)]
    label <- if (i == 1) cmsy_label("all_years", lang) else as.character(max(years))
    rbind(data.frame(panel = cmsy_label("b_bmsy", lang), peel = label, Year = years, value = r$B_Bmsy[[i]][seq_len(n)]),
          data.frame(panel = cmsy_label("f_fmsy", lang), peel = label, Year = years, value = r$F_Fmsy[[i]][seq_len(n)]))
  })
  data <- do.call(rbind, Filter(Negate(is.null), peels))
  data$peel <- factor(data$peel, levels = unique(data$peel))
  data$panel <- factor(data$panel, levels = c(cmsy_label("b_bmsy", lang), cmsy_label("f_fmsy", lang)))
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Year, y = value, colour = peel)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c("black", "#D7191C", "#1A9641", "#2C7BB6")[seq_along(levels(data$peel))],
                                 name = cmsy_label("retro_legend", lang)) +
    ggplot2::facet_wrap(~panel, scales = "free_y", strip.position = "left") +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(data$Year)) +
    ggplot2::labs(x = fao_label("year", lang), y = NULL) +
    fao_theme() + cmsy_strip_theme()
  if ("labels" %in% ann) {
    rho <- vapply(levels(data$panel), function(pn) cmsy_mohns_rho(data[data$panel == pn, ]), numeric(1))
    labels <- data.frame(panel = factor(levels(data$panel), levels = levels(data$panel)),
                         label = sprintf("Mohn's rho = %.2f", rho))
    p <- p + ggplot2::geom_text(data = labels, ggplot2::aes(x = -Inf, y = Inf, label = label), hjust = -0.1, vjust = 1.5,
                                inherit.aes = FALSE, family = fao_font(), size = 3.5)
  }
  p
}

# Mohn's rho: mean relative difference between the last year of each peel and the full run
cmsy_mohns_rho <- function(data) {
  full <- data[data$peel == levels(data$peel)[1], ]
  rel <- vapply(levels(data$peel)[-1], function(pl) {
    peel <- data[data$peel == pl, ]
    last <- max(peel$Year)
    (peel$value[peel$Year == last] - full$value[full$Year == last]) / full$value[full$Year == last]
  }, numeric(1))
  if (length(rel) == 0) NA_real_ else mean(rel)
}

cmsy_chart_overview <- function(ok, lang, ann) {
  ok$label <- paste0(ok$species_name, " (", ok$Stock, ")")
  ok$label <- stats::reorder(ok$label, ok$B_Bmsy)
  colors <- cmsy_status_colors(lang)
  ok$status_label <- factor(cmsy_status_label(ok$status, lang), levels = names(colors))
  ggplot2::ggplot(ok, ggplot2::aes(x = B_Bmsy, y = label, colour = status_label)) +
    ggplot2::geom_vline(xintercept = c(0.5, 0.8, 1), linetype = c("dotted", "dashed", "solid"), colour = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = B_Bmsy_lcl, xmax = B_Bmsy_ucl), width = 0.3, orientation = "y") +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_colour_manual(values = colors, drop = FALSE, name = cmsy_label("status", lang)) +
    ggplot2::labs(x = cmsy_label("b_bmsy_last", lang), y = NULL) +
    fao_theme()
}

cmsy_chart_kobe_all <- function(ok, lang, ann) {
  top <- max(2, max(ok$F_Fmsy, na.rm = TRUE) * 1.1)
  right <- max(2, max(ok$B_Bmsy, na.rm = TRUE) * 1.1)
  p <- cmsy_kobe_base(top, right) +
    ggplot2::geom_point(data = ok, ggplot2::aes(x = B_Bmsy, y = F_Fmsy, size = catch_last / 1000), shape = 21,
                        fill = "white", colour = "black", alpha = 0.9) +
    ggplot2::scale_size_area(max_size = 9, name = cmsy_label("catch_last_kt", lang)) +
    ggplot2::labs(x = cmsy_label("b_bmsy", lang), y = cmsy_label("f_fmsy", lang)) +
    fao_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::geom_text(data = ok, ggplot2::aes(x = B_Bmsy, y = F_Fmsy, label = Stock), vjust = -1.1,
                                family = fao_font(), size = 2.8)
  }
  p
}

cmsy_chart_status <- function(ok, lang, ann) {
  colors <- cmsy_status_colors(lang)
  status <- factor(cmsy_status_label(ok$status, lang), levels = names(colors))
  measures <- c(cmsy_label("pct_stocks", lang), cmsy_label("pct_catch", lang))
  shares <- rbind(
    data.frame(measure = measures[1], status = levels(status),
               value = 100 * as.numeric(table(status)) / length(status)),
    data.frame(measure = measures[2], status = levels(status),
               value = 100 * as.numeric(tapply(ok$catch_last, status, sum, default = 0)) / sum(ok$catch_last, na.rm = TRUE))
  )
  shares$measure <- factor(shares$measure, levels = measures)
  shares$status <- factor(shares$status, levels = names(colors))
  p <- ggplot2::ggplot(shares, ggplot2::aes(x = status, y = value, fill = status)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::scale_fill_manual(values = colors, name = cmsy_label("status", lang)) +
    ggplot2::facet_wrap(~measure, strip.position = "left") +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    fao_theme() + cmsy_strip_theme()
  if ("labels" %in% ann) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", value)), vjust = -0.4, family = fao_font(), size = 3.5)
  }
  p
}

cmsy_chart_trend <- function(series, lang, ann) {
  by_year <- split(series, series$Year)
  trend <- do.call(rbind, lapply(by_year, function(y) {
    b <- y$B_Bmsy[is.finite(y$B_Bmsy)]
    data.frame(Year = y$Year[1], n = length(b), median = stats::median(b),
               q1 = stats::quantile(b, 0.25, names = FALSE), q3 = stats::quantile(b, 0.75, names = FALSE),
               overfished = 100 * mean(b < 0.8))
  }))
  panels <- c(cmsy_label("b_bmsy_median", lang), cmsy_label("pct_overfished", lang))
  long <- rbind(data.frame(Year = trend$Year, panel = panels[1], value = trend$median, lcl = trend$q1, ucl = trend$q3),
                data.frame(Year = trend$Year, panel = panels[2], value = trend$overfished, lcl = NA_real_, ucl = NA_real_))
  long$panel <- factor(long$panel, levels = panels)
  refs <- data.frame(panel = factor(panels[c(1, 1)], levels = panels), y = c(1, 0.8), linetype = c("solid", "dashed"))
  ggplot2::ggplot(long, ggplot2::aes(x = Year, y = value)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), fill = "grey80", na.rm = TRUE) +
    ggplot2::geom_line(colour = "#08519C", linewidth = 0.8) +
    ggplot2::geom_hline(data = refs, ggplot2::aes(yintercept = y, linetype = linetype), colour = "grey30") +
    ggplot2::scale_linetype_identity() +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(long$Year)) +
    ggplot2::labs(x = fao_label("year", lang), y = NULL) +
    fao_theme() + cmsy_strip_theme()
}

# Chart text of the CMSY++ functions
cmsy_label <- function(key, lang, ...) {
  labels <- list(
    catch_kt = c("Catch (thousand tonnes)", "\u6e14\u83b7\u91cf\uff08\u5343\u5428\uff09"),
    catch_observed = c("Reported catch", "\u62a5\u544a\u6e14\u83b7\u91cf"),
    catch_smooth = c("Smoothed catch (used by CMSY++)", "\u5e73\u6ed1\u6e14\u83b7\u91cf\uff08CMSY++ \u4f7f\u7528\uff09"),
    catch_msy = c("Catch / MSY", "\u6e14\u83b7\u91cf / MSY"),
    catch_last_kt = c("Last-year catch (thousand tonnes)", "\u6700\u540e\u4e00\u5e74\u6e14\u83b7\u91cf\uff08\u5343\u5428\uff09"),
    r = c("Intrinsic growth rate r", "\u5185\u7980\u589e\u957f\u7387 r"),
    k_kt = c("Carrying capacity k (thousand tonnes)", "\u73af\u5883\u5bb9\u7eb3\u91cf k\uff08\u5343\u5428\uff09"),
    msy_kt = c("MSY (thousand tonnes)", "MSY\uff08\u5343\u5428\uff09"),
    b_k = c("Relative biomass B/k", "\u76f8\u5bf9\u751f\u7269\u91cf B/k"),
    b_bmsy = c("Relative stock size B/Bmsy", "\u76f8\u5bf9\u751f\u7269\u91cf B/Bmsy"),
    f_fmsy = c("Relative exploitation F/Fmsy", "\u76f8\u5bf9\u6355\u635e\u6b7b\u4ea1\u7387 F/Fmsy"),
    b_bmsy_last = c("B/Bmsy in the last year (95% confidence limits)", "\u6700\u540e\u4e00\u5e74 B/Bmsy\uff0895% \u7f6e\u4fe1\u9650\uff09"),
    b_bmsy_median = c("Median B/Bmsy (interquartile range)", "B/Bmsy \u4e2d\u4f4d\u6570\uff08\u56db\u5206\u4f4d\u8ddd\uff09"),
    pct_overfished = c("Stocks below 0.8 Bmsy (%)", "\u4f4e\u4e8e 0.8 Bmsy \u7684\u79cd\u7fa4\uff08%\uff09"),
    pct_stocks = c("Share of stocks (%)", "\u79cd\u7fa4\u6570\u5360\u6bd4\uff08%\uff09"),
    pct_catch = c("Share of last-year catch (%)", "\u6700\u540e\u4e00\u5e74\u6e14\u83b7\u91cf\u5360\u6bd4\uff08%\uff09"),
    density = c("Density", "\u6982\u7387\u5bc6\u5ea6"),
    prior = c("Prior", "\u5148\u9a8c"),
    posterior = c("Posterior", "\u540e\u9a8c"),
    all_years = c("All years", "\u5168\u90e8\u5e74\u4efd"),
    retro_legend = c("Last year", "\u672b\u5e74"),
    fao_limit = c("0.8 Bmsy (FAO)", "0.8 Bmsy\uff08FAO\uff09"),
    status = c("Status", "\u72b6\u6001"),
    status_msf = c("Maximally sustainably fished", "\u6700\u5927\u53ef\u6301\u7eed\u6355\u635e"),
    status_overfished = c("Overfished", "\u8fc7\u5ea6\u6355\u635e"),
    priors = c("r prior", "r \u5148\u9a8c"),
    n_stocks = c("{n} stocks", "{n} \u4e2a\u79cd\u7fa4"),
    chart_catch = c("Catch and MSY", "\u6e14\u83b7\u91cf\u4e0e MSY"),
    chart_rk = c("Viable r-k pairs", "r-k \u7ec4\u5408"),
    chart_biomass = c("Relative biomass and its priors", "\u76f8\u5bf9\u751f\u7269\u91cf\u53ca\u5176\u5148\u9a8c"),
    chart_b_bmsy = c("Relative stock size", "\u76f8\u5bf9\u79cd\u7fa4\u5927\u5c0f"),
    chart_exploitation = c("Exploitation", "\u6355\u635e\u5f3a\u5ea6"),
    chart_equilibrium = c("Equilibrium curve", "\u5e73\u8861\u66f2\u7ebf"),
    chart_management = c("Results for management", "\u7ba1\u7406\u7ed3\u679c"),
    chart_kobe = c("Kobe plot", "Kobe \u56fe"),
    chart_prior_posterior = c("Prior and posterior distributions", "\u5148\u9a8c\u4e0e\u540e\u9a8c\u5206\u5e03"),
    chart_retrospective = c("Retrospective analysis", "\u56de\u987e\u6027\u5206\u6790"),
    chart_overview = c("Relative stock size of the stocks in the last year", "\u5404\u79cd\u7fa4\u6700\u540e\u4e00\u5e74\u7684\u76f8\u5bf9\u751f\u7269\u91cf"),
    chart_kobe_all = c("Kobe plot of the stocks in the last year", "\u5404\u79cd\u7fa4\u6700\u540e\u4e00\u5e74\u7684 Kobe \u56fe"),
    chart_status = c("Status of the stocks", "\u79cd\u7fa4\u72b6\u6001"),
    chart_trend = c("Trend of relative stock size", "\u76f8\u5bf9\u751f\u7269\u91cf\u7684\u53d8\u5316"),
    subtitle = c("Last year: B/Bmsy {b}, F/Fmsy {f}, {status}; MSY {msy} t",
                 "\u6700\u540e\u4e00\u5e74\uff1aB/Bmsy {b}\uff0cF/Fmsy {f}\uff0c{status}\uff1bMSY {msy} \u5428"),
    caption = c(paste("CMSY++ catch-only results (FAO data have no abundance index); indicative, not a stock assessment.",
                      "Lines: Bmsy, 0.8 Bmsy (FAO overfished limit), 0.5 Bmsy (reduced recruitment).", sep = "\n"),
                paste0("CMSY++ \u4ec5\u6e14\u83b7\u91cf\u7ed3\u679c\uff08FAO \u6570\u636e\u6ca1\u6709\u4e30\u5ea6\u6307\u6570\uff09\uff0c",
                       "\u4ec5\u4f9b\u53c2\u8003\uff0c\u4e0d\u80fd\u66ff\u4ee3\u79cd\u7fa4\u8bc4\u4f30\u3002\n",
                       "\u53c2\u8003\u7ebf\uff1aBmsy\u30010.8 Bmsy\uff08FAO \u8fc7\u5ea6\u6355\u635e\u754c\u9650\uff09\u3001",
                       "0.5 Bmsy\uff08\u8865\u5145\u91cf\u4e0b\u964d\uff09"))
  )
  text <- labels[[key]]
  if (is.null(text)) stop("Unknown CMSY label: ", key, call. = FALSE)
  long <- key %in% c("subtitle", "caption") || startsWith(key, "chart_")
  out <- switch(lang, en = text[1], zh = text[2],
                both = if (long) paste0(text[2], "\n", text[1]) else paste0(text[2], " (", text[1], ")"))
  values <- list(...)
  for (name in names(values)) out <- gsub(paste0("{", name, "}"), values[[name]], out, fixed = TRUE)
  out
}

utils::globalVariables(c("B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "B_k", "B_k_lcl", "B_k_ucl", "Catch", "Catch_smooth",
                         "F_Fmsy", "F_Fmsy_lcl", "F_Fmsy_ucl", "catch_last", "fill", "high", "hjust", "k", "k_lcl",
                         "k_ucl", "label", "lcl", "linetype", "low", "measure", "panel", "peel", "r", "r_lcl", "r_ucl",
                         "shape", "type", "ucl", "vjust", "x", "xmax", "xmin", "y", "ymax", "ymin", "z"))
