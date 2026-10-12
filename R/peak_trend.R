#' Peak Catch and Recent Trend
#'
#' This function finds, for the total catch and for the main species, countries or fishing areas,
#' the year and size of the peak catch, the catch in the last year as a percentage of the peak,
#' and the trend over the last `recent_years` years: the average annual growth rate, and a
#' Mann-Kendall trend test (Kendall's tau between year and catch).
#'
#' Catch series are autocorrelated, which makes the usual Mann-Kendall test find too many trends,
#' so the variance of the test is corrected with the effective sample size of an AR(1) process
#' (Yue and Wang 2004), from the lag-1 autocorrelation of the detrended series corrected for its
#' small-sample bias (Kendall 1954; `lag1_autocorrelation`); `p_value_iid` is the p value of the
#' usual test, for comparison. Over ten years the autocorrelation is poorly known and the
#' correction is only partial: strongly autocorrelated series still show trends too often. `trend` tests each series on its own
#' (`p_value` < 0.05): with many series, some trends are found by chance, and `q_value` gives the
#' p values adjusted for the number of series in the result, the total excepted (Benjamini and
#' Hochberg 1995). Read trends with `p_value` < 0.05 as exploratory, and give as significant
#' the trends with `q_value` < 0.05, as [fao_report()] does for the main species and countries.
#'
#' @inheritParams summary_params
#' @param by What to compare: "species" (default), "country" or "area".
#' @param top_n The number of species, countries or areas with the largest total catch to include.
#'        Default is 10; use `Inf` for all.
#' @param recent_years The number of most recent years used for the trend. Default is 10.
#' @return Invisibly, a data frame with one row per series (the first row is the total).
#' @references
#' Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and
#' powerful approach to multiple testing. Journal of the Royal Statistical Society: Series B 57:
#' 289-300. \doi{10.1111/j.2517-6161.1995.tb02031.x}
#'
#' Kendall, M.G. (1954). Note on bias in the estimation of autocorrelation. Biometrika 41: 403-404.
#' \doi{10.1093/biomet/41.3-4.403}
#'
#' Yue, S. and Wang, C.Y. (2004). The Mann-Kendall test modified by effective sample size to
#' detect trend in serially correlated hydrological series. Water Resources Management 18:
#' 201-218. \doi{10.1023/B:WARM.0000043140.61082.60}
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
#' peak_trend(FAO27$area_data, by = "species", lang = "zh")
#' peak_trend(FAO27$area_data, by = "country", lang = "zh")
#' }
peak_trend <- function(area_data, by = c("species", "country", "area"), top_n = 10, recent_years = 10,
                       plot = TRUE, table = TRUE, lang = NULL,
                       pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), label = NULL) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  by <- match.arg(by)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), paste0("peak_trend_", by))
  column <- c(species = "ASFIS.species..Name.", country = "Country..Name.",
              area = "FAO.major.fishing.area..Name.")[[by]]
  last_year <- max(area_data$Year, na.rm = TRUE)

  yearly <- stats::aggregate(list(Tonnes = area_data$Production),
                             by = list(name = area_data[[column]], Year = area_data$Year),
                             FUN = sum, na.rm = TRUE)
  totals <- sort(tapply(yearly$Tonnes, yearly$name, sum), decreasing = TRUE)
  totals <- totals[totals > 0]   # series without any catch
  top <- names(totals)[seq_len(min(top_n, length(totals)))]
  total <- stats::aggregate(list(Tonnes = yearly$Tonnes), by = list(Year = yearly$Year), FUN = sum)
  series <- c(list(total), lapply(top, function(n) yearly[yearly$name == n, c("Year", "Tonnes")]))
  names(series) <- c(fao_label("total", lang), top)

  result <- do.call(rbind, lapply(names(series), function(n) {
    s <- series[[n]]
    years <- seq(min(s$Year[s$Tonnes > 0]), last_year)
    catch <- s$Tonnes[match(years, s$Year)]
    catch[is.na(catch)] <- 0
    recent <- years > last_year - recent_years
    first <- catch[recent][1]
    last <- catch[length(catch)]
    # Series with fewer than four recent years have no trend
    test <- fao_mann_kendall(years[recent], catch[recent])
    data.frame(
      name_en = n,
      peak_year = years[which.max(catch)],
      peak_tonnes = max(catch),
      last_year_tonnes = last,
      pct_of_peak = 100 * last / max(catch),
      growth_pct_per_year = if (first > 0) 100 * ((last / first)^(1 / (sum(recent) - 1)) - 1) else NA,
      kendall_tau = unname(test["tau"]),
      p_value = unname(test["p"]),
      p_value_iid = unname(test["p_iid"]),
      lag1_autocorrelation = unname(test["r1"]),
      trend = ifelse(!is.na(test["p"]) & test["p"] < 0.05, ifelse(test["tau"] > 0, "up", "down"), "none"),
      stringsAsFactors = FALSE
    )
  }))
  is_total <- seq_len(nrow(result)) == 1
  result$q_value <- NA_real_
  result$q_value[!is_total] <- stats::p.adjust(result$p_value[!is_total], method = "BH")
  result <- result[, c(setdiff(names(result), c("trend", "q_value")), "q_value", "trend")]
  result$name <- result$name_en
  result$name[!is_total] <- fao_translate(result$name_en[!is_total], by, lang)
  result <- result[, c("name", setdiff(names(result), "name"))]

  if (plot) {
    trend_labels <- c(up = fao_label("trend_up", lang), down = fao_label("trend_down", lang),
                      none = fao_label("trend_none", lang))
    chart_data <- result
    chart_data$name <- factor(chart_data$name, levels = rev(chart_data$name))
    chart_data$trend <- factor(trend_labels[chart_data$trend], levels = trend_labels)
    chart_data$text <- paste0(round(chart_data$pct_of_peak), "% (", chart_data$peak_year, ")")
    p <- ggplot2::ggplot(chart_data, ggplot2::aes(x = pct_of_peak, y = name, fill = trend)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = text), hjust = -0.1, size = 3.8, family = fao_font()) +
      ggplot2::scale_fill_manual(values = stats::setNames(c("#1A9641", "#D7191C", "grey60"), trend_labels),
                                 drop = FALSE) +
      ggplot2::scale_x_continuous(limits = c(0, 125), breaks = seq(0, 100, 25), expand = c(0, 0)) +
      ggplot2::labs(x = fao_label("peak_share", lang), y = NULL,
                    fill = fao_label("trend", lang, n = recent_years),
                    caption = fao_label("peak_year_note", lang)) +
      fao_theme()
    fao_save_plot(p, file.path("summary", name), width = 12, height = max(4, 0.45 * nrow(result) + 2), pptx = pptx)
  }
  if (table) fao_save_table(result, name)
  fao_run_result(result, run, fao_run_title("peak_trend", area_data, label),
                 main = fao_cols(result, c("name", "peak_year", "peak_tonnes", "last_year_tonnes", "pct_of_peak",
                                           "growth_pct_per_year", "trend")),
                 note = paste0("Peak catch and trend of the last ", recent_years, " years:"))
}


#' Overview of All FAO Marine Fishing Areas
#'
#' This function compares the FAO major marine fishing areas: their catch over time, peak year,
#' catch in the last year as a percentage of the peak, and recent trend (see `peak_trend()`).
#'
#' @inheritParams summary_params
#' @param recent_years The number of most recent years used for the trend. Default is 10.
#' @param data_version The FAO data release to use; see `fishing_area_data()`.
#' @return Invisibly, a data frame with one row per area (the first row is the total).
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' area_overview(lang = "zh")
#' }
area_overview <- function(recent_years = 10, plot = TRUE, table = TRUE, lang = NULL,
                          pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), data_version = "latest") {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  lang <- fao_lang(lang)
  data <- load_fao_data(data_version)
  groups <- fao_group_table()
  marine <- groups$area$name_en[groups$area$inland_marine_en %in% "Marine areas"]
  data <- data[data$FAO.major.fishing.area..Name. %in% marine, ]

  result <- peak_trend(data, by = "area", top_n = Inf, recent_years = recent_years, plot = plot,
                       table = table, lang = lang, pptx = pptx, label = "marine_areas")

  if (plot) {
    yearly <- stats::aggregate(list(Tonnes = data$Production),
                               by = list(area = data$FAO.major.fishing.area..Name., Year = data$Year),
                               FUN = sum, na.rm = TRUE)
    order <- names(sort(tapply(yearly$Tonnes, yearly$area, sum), decreasing = TRUE))
    yearly$area_name <- factor(fao_translate(yearly$area, "area", lang),
                               levels = fao_translate(order, "area", lang))
    peaks <- dplyr::slice_max(dplyr::group_by(yearly, area_name), Tonnes, n = 1, with_ties = FALSE)
    p <- ggplot2::ggplot(yearly, ggplot2::aes(x = Year, y = Tonnes / 1e6)) +
      ggplot2::geom_area(fill = "grey80", colour = "grey30", linewidth = 0.3) +
      ggplot2::geom_point(data = peaks, colour = "#D7191C", size = 2) +
      ggplot2::facet_wrap(~area_name, ncol = 4, scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(yearly$Year, by = 20), expand = c(0, 0)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("million_tonnes", lang)) +
      fao_theme()
    fao_save_plot(p, file.path("summary", "marine_areas_catch"), width = 14, height = 12, pptx = pptx)
  }
  fao_run_result(result, run, "area_overview(): all marine FAO areas",
                 main = fao_cols(result, c("name", "peak_year", "peak_tonnes", "last_year_tonnes", "pct_of_peak",
                                           "growth_pct_per_year", "trend")),
                 note = paste0("Peak catch and trend of the last ", recent_years, " years:"))
}
