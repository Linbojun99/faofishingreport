#' Catch Shocks
#'
#' This function finds the years in which catch fell abruptly ("shocks") in each catch series,
#' with the method of Cottrell et al. (2019) or Gephart et al. (2017), and counts how often shocks
#' occur by year, by FAO fishing area, by country or by species group.
#'
#' Each series (e.g. the catch of one country in one FAO area) is smoothed, and the residuals from
#' the smooth are regressed on the residuals of the year before (a lag-1 regression). Years whose
#' Cook's distance in that regression exceeds a threshold are candidate shocks; only those in which
#' catch fell below the level of the preceding years are kept:
#' \describe{
#'   \item{"cottrell2019" (default)}{LOESS with span 0.6, Cook's distance above 0.3; the loss is
#'     measured against the median catch of the previous 7 years, so the first 7 years of a
#'     series cannot be shocks (Cottrell et al. 2019, Methods, and their code at
#'     \url{https://github.com/cottrellr/shocks}).}
#'   \item{"gephart2017"}{lowess with span 2/3, Cook's distance above 0.35; the loss is measured
#'     against the mean catch of the previous 5 years (Gephart et al. 2017, section 2, and their
#'     code at \url{https://github.com/jagephart/Shock_Detection}).}
#' }
#' A shock has recovered in the first later year in which catch is back to at least 95% of that
#' baseline. The yearly shock frequency is the number of shocks in a year divided by the number of
#' series analysed in that year (Cottrell et al. 2019): a series counts from its first year with
#' catch to the last year, including years in which its catch fell to zero; the shock rate of an area, country or
#' species group is the number of shocks divided by the number of its series (Gephart et al. 2017).
#'
#' Both studies analysed national totals (all species and areas of a country); with `by =
#' "country"` and the data of all areas (`load_fao_data()`) the series are the same. Neither study
#' states a minimum series length; `min_years` is a choice of this package, as is ending the series
#' of countries that no longer exist (e.g. the USSR) in their last year with catch, so that the end
#' of a country is not counted as a shock. Highly variable series
#' (e.g. Peruvian anchoveta) may show no shocks because large changes are usual in them (Gephart et
#' al. 2017).
#'
#' @inheritParams summary_params
#' @param by What a series is: one or more of "country", "area" and "species". Default is
#'        `c("country", "area")`: the catch of each country in each FAO area.
#' @param method "cottrell2019" (default) or "gephart2017".
#' @param min_years Series with catch in fewer years are left out. Default is 20.
#' @param top_n The number of series with the largest catch shown in the timeline chart. Default is 25.
#' @return Invisibly, a list with `shocks` (one row per shock: year, catch, baseline, loss, Cook's
#'         distance and years to recovery), `by_year` (number of series, shocks and shock frequency
#'         in each year), `by_group` (shock rate by area, country, species group and so on) and
#'         `series` (the series analysed).
#' @references
#' Cottrell, R.S., Nash, K.L., Halpern, B.S. et al. (2019). Food production shocks across land and
#' sea. Nature Sustainability 2: 130-137. \doi{10.1038/s41893-018-0210-1}
#'
#' Gephart, J.A., Deutsch, L., Pace, M.L., Troell, M. and Seekell, D.A. (2017). Shocks to fish
#' production: identification, trends, and consequences. Global Environmental Change 42: 24-32.
#' \doi{10.1016/j.gloenvcha.2016.11.003}
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' catch_shocks(FAO34$area_data, lang = "zh")                      # countries in area 34
#' catch_shocks(FAO34$area_data, by = "species")                   # species in area 34
#' catch_shocks(load_fao_data(), by = "country", method = "gephart2017")   # national totals
#' }
catch_shocks <- function(area_data, by = c("country", "area"), method = c("cottrell2019", "gephart2017"),
                         min_years = 20, top_n = 25, plot = TRUE, table = TRUE, lang = NULL,
                         pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  method <- match.arg(method)
  by <- match.arg(by, c("country", "area", "species"), several.ok = TRUE)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), "shocks")
  settings <- fao_shock_settings(method)

  columns <- c(country = "Country..Name.", area = "FAO.major.fishing.area..Name.", species = "ASFIS.species..Name.")
  data <- fao_add_groups(area_data[!is.na(area_data$Production), ])
  data$series <- do.call(paste, c(lapply(columns[by], function(k) data[[k]]), sep = " | "))
  last_year <- max(data$Year)

  yearly <- stats::aggregate(list(Tonnes = data$Production), by = list(series = data$series, Year = data$Year), FUN = sum)
  # Species groups of species series and continents of country series, for the shock rates
  extra <- c(species_group = if ("species" %in% by) "isscaap_en", continent = if ("country" %in% by) "continent_en")
  info <- data[!duplicated(data$series), c("series", unname(columns[by]), unname(extra))]
  group_names <- list(isscaap_en = unique(data[, c("isscaap_en", "isscaap_zh")]),
                      continent_en = unique(data[, c("continent_en", "continent_zh")]))
  # countries without a continent (the USSR, "Other nei") are counted as "Other"
  if ("continent_en" %in% names(info) && anyNA(info$continent_en)) {
    info$continent_en[is.na(info$continent_en)] <- "Other"
    known <- group_names$continent_en[!is.na(group_names$continent_en$continent_en), ]
    group_names$continent_en <- rbind(known, data.frame(continent_en = "Other", continent_zh = "\u5176\u4ed6"))
  }
  # Series of countries that no longer exist (USSR, Yugoslavia SFR, ...) end in their last year with
  # catch, so that the end of the country is not counted as a shock
  former <- if ("country" %in% by) unique(info$series[info$Country..Name. %in% fao_former_countries$name_en]) else character(0)
  results <- lapply(split(yearly, yearly$series), function(s) {
    if (sum(s$Tonnes > 0) < min_years) return(NULL)
    end <- if (s$series[1] %in% former) max(s$Year[s$Tonnes > 0]) else last_year
    years <- seq(min(s$Year[s$Tonnes > 0]), end)
    catch <- s$Tonnes[match(years, s$Year)]
    catch[is.na(catch)] <- 0
    if (sum(catch > 0) < min_years) return(NULL)
    list(series = s$series[1], years = years, catch = catch, shocks = fao_find_shocks(years, catch, settings))
  })
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) stop("No series has catch in at least ", min_years, " years.", call. = FALSE)

  series <- do.call(rbind, lapply(results, function(r) {
    data.frame(series = r$series, first_year = r$years[1], last_year = r$years[length(r$years)],
               n_years = sum(r$catch > 0), total_tonnes = sum(r$catch),
               n_shocks = nrow(r$shocks), stringsAsFactors = FALSE)
  }))
  series <- merge(info, series, by = "series")
  series <- series[order(-series$total_tonnes), ]
  shocks <- do.call(rbind, lapply(results, function(r) if (nrow(r$shocks) > 0) cbind(series = r$series, r$shocks)))
  if (is.null(shocks)) shocks <- cbind(series = character(0), fao_find_shocks(1:3, c(1, 1, 1), settings))
  shocks <- merge(info, shocks, by = "series")
  shocks <- shocks[order(shocks$Year, -shocks$loss_tonnes), ]

  # Series analysed in each year: from their first year with catch to their end, including years
  # in which the catch fell to zero (which can be shocks), so that shocks and series match
  years <- seq(min(series$first_year), last_year)
  analysed <- vapply(years, function(y) sum(vapply(results, function(r) y %in% r$years, logical(1))), integer(1))
  by_year <- data.frame(Year = years, n_series = analysed,
                        n_shocks = vapply(years, function(y) sum(shocks$Year == y), integer(1)))
  by_year$shock_frequency_pct <- ifelse(by_year$n_series > 0, 100 * by_year$n_shocks / by_year$n_series, NA_real_)

  group_columns <- c(columns[by], extra)
  by_group <- do.call(rbind, lapply(names(group_columns), function(g) {
    column <- group_columns[[g]]
    levels <- unique(series[[column]])
    names <- if (g %in% c("country", "area", "species")) fao_translate(levels, g, lang) else {
      table_g <- group_names[[column]]
      fao_pick(levels, table_g[[sub("_en$", "_zh", column)]][match(levels, table_g[[column]])], lang)
    }
    data.frame(grouping = g, group = levels, group_name = names,
               n_series = vapply(levels, function(l) sum(series[[column]] %in% l), integer(1)),
               n_shocks = vapply(levels, function(l) sum(shocks[[column]] %in% l), integer(1)),
               stringsAsFactors = FALSE, row.names = NULL)
  }))
  by_group$shocks_per_series <- by_group$n_shocks / by_group$n_series
  by_group <- by_group[order(by_group$grouping, -by_group$shocks_per_series, -by_group$n_series), ]
  rownames(by_group) <- NULL

  translate_series <- function(x) {
    parts <- strsplit(x, " | ", fixed = TRUE)
    vapply(parts, function(p) paste(vapply(seq_along(by), function(k) fao_translate(p[k], by[k], lang), character(1)),
                                    collapse = " | "), character(1))
  }
  series$series_name <- translate_series(series$series)
  shocks$series_name <- translate_series(shocks$series)
  message(nrow(series), " series analysed (", method, "): ", nrow(shocks), " shocks in ",
          sum(series$n_shocks > 0), " series.")

  if (plot) {
    caption <- fao_label("shock_method_note", lang, n = settings$text)
    p_year <- ggplot2::ggplot(by_year, ggplot2::aes(x = Year, y = shock_frequency_pct)) +
      ggplot2::geom_col(fill = "#D7191C", width = 0.8) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(by_year$Year)) +
      ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("shock_frequency", lang), caption = caption) +
      fao_theme()
    fao_save_plot(p_year, file.path("summary", name), width = 12, height = 6, pptx = pptx)

    top <- utils::head(series, top_n)
    if (nrow(top) > 0) {
      top$series_name <- factor(top$series_name, levels = rev(top$series_name))
      points <- shocks[shocks$series %in% top$series, ]
      points$series_name <- factor(points$series_name, levels = levels(top$series_name))
      p_time <- ggplot2::ggplot() +
        ggplot2::geom_segment(data = top, ggplot2::aes(x = first_year, xend = last_year, y = series_name, yend = series_name),
                              colour = "grey80", linewidth = 2) +
        ggplot2::scale_x_continuous(breaks = fao_year_breaks(years)) +
        ggplot2::labs(x = fao_label("year", lang), y = NULL, size = fao_label("shock_loss", lang), caption = caption) +
        fao_theme()
      if (nrow(points) > 0) {
        p_time <- p_time + ggplot2::geom_point(data = points, ggplot2::aes(x = Year, y = series_name, size = loss_pct),
                                               colour = "#D7191C", alpha = 0.8)
      }
      fao_save_plot(p_time, file.path("summary", paste0(name, "_series")), width = 12,
                    height = max(4, 0.35 * nrow(top) + 2), pptx = pptx)
    }

    for (g in unique(by_group$grouping)) {
      x <- by_group[by_group$grouping == g, ]
      if (nrow(x) < 2) next
      x <- utils::head(x[order(-x$n_series), ], 20)
      x$group_name <- factor(x$group_name, levels = x$group_name[order(x$shocks_per_series)])
      p_group <- ggplot2::ggplot(x, ggplot2::aes(x = shocks_per_series, y = group_name)) +
        ggplot2::geom_col(fill = "#D7191C", width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(n_shocks, "/", n_series)), hjust = -0.1, size = 3.5,
                           family = fao_font()) +
        ggplot2::scale_x_continuous(expand = c(0, 0, 0.15, 0)) +
        ggplot2::labs(x = fao_label("shocks_per_series", lang), y = NULL, caption = caption) +
        fao_theme()
      fao_save_plot(p_group, file.path("summary", paste0(name, "_by_", g)),
                    width = 10, height = max(4, 0.4 * nrow(x) + 2), pptx = pptx)
    }
  }
  if (table) {
    fao_save_table(shocks, name)
    fao_save_table(by_year, paste0(name, "_by_year"))
    fao_save_table(by_group, paste0(name, "_by_group"))
    fao_save_table(series, paste0(name, "_series"))
  }
  invisible(list(shocks = shocks, by_year = by_year, by_group = by_group, series = series))
}

# Parameters of the two published methods
fao_shock_settings <- function(method) {
  switch(method,
    cottrell2019 = list(smooth = function(x, y) stats::residuals(stats::loess(y ~ x, span = 0.6)),
                        threshold = 0.3, baseline_years = 7, baseline = stats::median,
                        text = "Cottrell et al. 2019 (LOESS span 0.6, Cook's D > 0.3, 7-year median)"),
    gephart2017 = list(smooth = function(x, y) y - stats::lowess(x, y, f = 2 / 3)$y,
                       threshold = 0.35, baseline_years = 5, baseline = mean,
                       text = "Gephart et al. 2017 (lowess span 2/3, Cook's D > 0.35, 5-year mean)"))
}

# Shocks of one series: years whose lag-1 residual regression has Cook's distance above the
# threshold and whose catch is below the baseline of the previous years
fao_find_shocks <- function(years, catch, settings) {
  empty <- data.frame(Year = integer(0), catch_tonnes = numeric(0), baseline_tonnes = numeric(0),
                      loss_tonnes = numeric(0), loss_pct = numeric(0), cooks_distance = numeric(0),
                      recovery_years = integer(0))
  n <- length(catch)
  if (n <= settings$baseline_years + 2) return(empty)
  residuals <- tryCatch(settings$smooth(years, catch), error = function(e) NULL)
  if (is.null(residuals) || all(abs(residuals) < 1e-9)) return(empty)
  fit <- stats::lm(residuals[-1] ~ residuals[-n])
  cooks <- suppressWarnings(stats::cooks.distance(fit))
  # Row s of the regression is the residual of year s + 1
  candidates <- which(cooks > settings$threshold) + 1
  candidates <- candidates[candidates > settings$baseline_years]
  if (length(candidates) == 0) return(empty)
  out <- do.call(rbind, lapply(candidates, function(t) {
    baseline <- settings$baseline(catch[(t - settings$baseline_years):(t - 1)])
    later <- which(seq_along(catch) > t & catch >= 0.95 * baseline)
    data.frame(Year = years[t], catch_tonnes = catch[t], baseline_tonnes = baseline,
               loss_tonnes = baseline - catch[t], loss_pct = if (baseline > 0) 100 * (baseline - catch[t]) / baseline else NA_real_,
               cooks_distance = unname(cooks[t - 1]),
               recovery_years = if (length(later) > 0) as.integer(years[later[1]] - years[t]) else NA_integer_)
  }))
  out[out$loss_tonnes > 0, ]
}
