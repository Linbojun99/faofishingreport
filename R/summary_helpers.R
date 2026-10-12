# Shared helpers of the summary analyses (species_group_composition(), stock_status(), ...).

# Name used in output files: the fishing area (or country) of the data, unless given
fao_summary_label <- function(data, label = NULL) {
  if (!is.null(label)) return(label)
  areas <- unique(data$FAO.major.fishing.area..Name.)
  if (length(areas) == 1) return(areas)
  countries <- unique(data$Country..Name.)
  if (length(countries) == 1) return(countries)
  "selected_data"
}

# Output path inside figures/png/summary/ (and figures/pptx/summary/) or tables/
fao_summary_name <- function(label, analysis) {
  paste0(fao_file_name(label), "_", analysis)
}

# The ranks chosen with `rank_range` (fishing_area_species(), species_country(), ...): the ranks
# listed, e.g. 1:10 for the first ten or c(5, 6) for the fifth and sixth. Until 0.9.0 two numbers
# were the first and last rank of a range, so a note says once per session what c(1, 10) means now.
fao_rank_select <- function(rank_range) {
  if (!is.numeric(rank_range) || length(rank_range) == 0 || !all(is.finite(rank_range)) ||
      any(rank_range < 1) || any(rank_range != round(rank_range))) {
    stop("`rank_range` must be the ranks to show, whole numbers of at least 1: e.g. 1:10 for ",
         "ranks 1 to 10, or c(5, 6) for ranks 5 and 6.", call. = FALSE)
  }
  if (length(rank_range) == 2 && abs(diff(rank_range)) > 1 && !isTRUE(.fao_cache$rank_note)) {
    .fao_cache$rank_note <- TRUE
    from <- min(rank_range)
    to <- max(rank_range)
    message("Since version 1.0.0 `rank_range` lists the ranks to show: c(", rank_range[1], ", ",
            rank_range[2], ") shows ranks ", rank_range[1], " and ", rank_range[2], ". Use ",
            from, ":", to, " for ranks ", from, " to ", to, ".")
  }
  sort(unique(as.integer(rank_range)))
}

# Keeps the rows at the chosen ranks of data sorted from the largest catch; stops when there are
# none (e.g. ranks 20 to 30 of an area with 15 species)
fao_rank_slice <- function(data, ranks, what = "species") {
  out <- dplyr::slice(data, ranks)
  if (nrow(out) == 0) {
    shown <- if (length(ranks) > 5) paste(min(ranks), "to", max(ranks)) else paste(ranks, collapse = ", ")
    stop("There is nothing at the ranks in `rank_range` (", shown, "): the data have ",
         nrow(data), " ", what, ".", call. = FALSE)
  }
  out
}

fao_save_table <- function(x, name) {
  fao_ensure_dirs("tables")
  fao_write_csv(x, file.path("tables", paste0(name, ".csv")))
}

# Common look of the summary charts
fao_theme <- function(legend = "bottom") {
  fao_base_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = legend
    )
}

# Colours for categories from the chosen palette; the "other" category is grey
fao_colors <- function(levels, other = NULL) {
  main <- setdiff(levels, other)
  colors <- stats::setNames(fao_palette(length(main)), main)
  if (!is.null(other) && other %in% levels) colors[other] <- "grey70"
  colors[levels]
}

# Keeps the `top_n` categories with the largest catch and combines the rest into `other`.
# Returns the categories as a factor ordered by catch, with `other` last.
fao_top_categories <- function(category, tonnes, top_n, other) {
  totals <- sort(tapply(tonnes, category, sum, na.rm = TRUE), decreasing = TRUE)
  keep <- names(totals)[seq_len(min(top_n, length(totals)))]
  category <- ifelse(category %in% keep, category, other)
  factor(category, levels = c(keep, other)[c(keep, other) %in% category])
}

# Yearly catch by category, with a row for every year and category (missing catch = 0)
fao_yearly_by <- function(data, category) {
  yearly <- stats::aggregate(list(Tonnes = data$Production),
                             by = list(Year = data$Year, category = data[[category]]),
                             FUN = sum, na.rm = TRUE)
  yearly <- tidyr::complete(yearly, Year = seq(min(yearly$Year), max(yearly$Year)), category,
                            fill = list(Tonnes = 0))
  if (is.factor(data[[category]])) {
    yearly$category <- factor(yearly$category, levels = levels(data[[category]]))
  }
  yearly
}

# Stacked area charts of the yearly catch by category: in tonnes and as a share of each year's total
fao_stack_charts <- function(yearly, fill_label, name, lang, pptx, colors = NULL,
                             charts = c("tonnes", "share")) {
  yearly <- dplyr::mutate(dplyr::group_by(yearly, Year),
                          share = if (sum(Tonnes) > 0) 100 * Tonnes / sum(Tonnes) else 0)
  yearly <- dplyr::ungroup(yearly)
  if (is.null(colors)) colors <- fao_colors(levels(yearly$category))

  stack_chart <- function(value, y_label) {
    ggplot2::ggplot(yearly, ggplot2::aes(x = Year, y = .data[[value]], fill = category)) +
      ggplot2::geom_area(colour = "white", linewidth = 0.2) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(yearly$Year), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::labs(x = fao_label("year", lang), y = y_label, fill = fill_label) +
      fao_theme() +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3))
  }
  plots <- list()
  if ("tonnes" %in% charts) {
    yearly$tonnes_m <- yearly$Tonnes / 1e6
    plots$tonnes <- stack_chart("tonnes_m", fao_label("million_tonnes", lang))
    fao_save_plot(plots$tonnes, name, width = 12, height = 7, pptx = pptx)
  }
  if ("share" %in% charts) {
    plots$share <- stack_chart("share", fao_label("share_pct", lang))
    fao_save_plot(plots$share, paste0(name, "_share"), width = 12, height = 7, pptx = pptx)
  }
  invisible(plots)
}

# Table of a yearly catch by category: average yearly catch per decade, catch in each of the
# last five years and the share of each category in the last year
fao_category_table <- function(yearly, category_label) {
  last_year <- max(yearly$Year)
  decades <- dplyr::summarise(dplyr::group_by(yearly, category, period = fao_decade(Year)),
                              value = mean(Tonnes), .groups = "drop")
  decades <- tidyr::pivot_wider(decades, names_from = period, values_from = value)
  recent <- yearly[yearly$Year > last_year - 5, c("category", "Year", "Tonnes")]
  recent <- tidyr::pivot_wider(recent, names_from = Year, values_from = Tonnes)
  last <- yearly[yearly$Year == last_year, c("category", "Tonnes")]
  last[[paste0("share_", last_year, "_pct")]] <- 100 * last$Tonnes / sum(last$Tonnes)
  last$Tonnes <- NULL
  out <- dplyr::left_join(dplyr::left_join(decades, recent, by = "category"), last, by = "category")
  out <- out[order(out$category), ]
  names(out)[1] <- category_label
  out
}

# Common roxygen parameters of the summary analyses
#' @param area_data FAO catch data: `fishing_area_data()$area_data`, or any subset of
#'        `load_fao_data()` (e.g. one country, or several fishing areas).
#' @param plot A logical value. If TRUE (default), charts are saved in `figures/png/summary/`.
#' @param table A logical value. If TRUE (default), tables are saved in `tables/`.
#' @param lang The language of the chart text and names: "en" (English), "zh" (Chinese) or
#'        "both" (Chinese and English). Default is NULL, which uses the language set with
#'        `fao_settings()` ("en" unless changed).
#' @param pptx A logical value. If TRUE, charts are also saved as editable PowerPoint files in
#'        `figures/pptx/summary/`. Default is FALSE, which can be changed for all functions with
#'        `fao_settings(pptx = TRUE)`.
#' @param label The name used in the output file names. Default is the fishing area (or the
#'        country) of the data.
#' @param plot_show A logical value. If TRUE, each chart is also drawn in the plot window (e.g.
#'        the Plots pane of RStudio) as it is saved, to look at it at once. Default is FALSE,
#'        which can be changed for all functions with `fao_settings(plot_show = TRUE)`. The
#'        charts are saved as files (`plot = TRUE`) either way.
#' @name summary_params
#' @keywords internal
NULL

# Start and end year of each period of a breakpoint analysis. `breaks` are the positions of the
# last observation of every period but the last (`strucchange::breakpoints()$breakpoints`, NA
# when there is no break) and `years` the year of each observation of the series.
fao_bp_periods <- function(breaks, years) {
  breaks <- breaks[!is.na(breaks)]
  data.frame(start_year = years[c(1, breaks + 1)], end_year = years[c(breaks, length(years))])
}

# The older functions of fishing areas, species and countries print their intermediate tables only
# when asked with options(faofishingreport.verbose = TRUE); the tables are also returned and saved
fao_verbose_print <- function(x) {
  if (isTRUE(getOption("faofishingreport.verbose", FALSE))) print(x)
  invisible(x)
}

fao_verbose_message <- function(text) {
  if (isTRUE(getOption("faofishingreport.verbose", FALSE))) message(text)
  invisible(text)
}

# Mann-Kendall trend test with the variance of S corrected for autocorrelation by the effective
# sample size of an AR(1) process (Yue and Wang 2004, Water Resources Management 18: 201-218):
# n/n* = 1 + 2 sum_k (1 - k/n) r1^k, where r1 is the lag-1 autocorrelation of the series detrended
# with Sen's slope, corrected for its small-sample bias, E(r1) = rho - (1 + 3 rho)/n (Kendall 1954,
# Biometrika 41: 403-404). The variance is never made smaller (n/n* at least 1). Returns Kendall's
# tau, the corrected p value (`p`), the p value for independent data (`p_iid`), both with the
# continuity correction of S, and the corrected lag-1 autocorrelation (`r1`).
fao_mann_kendall <- function(time, x) {
  keep <- !is.na(time) & !is.na(x)
  time <- time[keep]
  x <- x[keep]
  n <- length(x)
  none <- c(tau = NA_real_, p = NA_real_, p_iid = NA_real_, r1 = NA_real_)
  if (n < 4 || length(unique(x)) < 2 || length(unique(time)) < 2) return(none)
  o <- order(time)
  time <- time[o]
  x <- x[o]
  pairs <- lower.tri(diag(n))
  s <- sum(sign(outer(x, x, "-"))[pairs] * sign(outer(time, time, "-"))[pairs])
  ties <- table(x)
  var_s <- (n * (n - 1) * (2 * n + 5) - sum(ties * (ties - 1) * (2 * ties + 5))) / 18
  slopes <- (outer(x, x, "-") / outer(time, time, "-"))[pairs]
  detrended <- x - stats::median(slopes[is.finite(slopes)]) * time
  r1 <- if (stats::var(detrended) > 0) stats::acf(detrended, lag.max = 1, plot = FALSE)$acf[2] else 0
  r1 <- min(0.99, max(-0.99, (n * r1 + 1) / (n - 3)))
  k <- seq_len(n - 1)
  ratio <- max(1, 1 + 2 * sum((1 - k / n) * r1^k))
  z <- function(v) (s - sign(s)) / sqrt(v)
  c(tau = suppressWarnings(stats::cor(time, x, method = "kendall")),
    p = 2 * stats::pnorm(-abs(z(var_s * ratio))), p_iid = 2 * stats::pnorm(-abs(z(var_s))), r1 = r1)
}
