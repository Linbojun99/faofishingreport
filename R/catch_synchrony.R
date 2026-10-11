#' Synchrony and Stability of the Catch
#'
#' This function measures how synchronously the catches of the species (or countries) of the data
#' fluctuate, and how much their asynchrony stabilises the total catch: the synchrony index of
#' Loreau and de Mazancourt (2008), the portfolio effect used by Ong et al. (2021) for FAO catch
#' series, the decomposition of the variability of the total catch of Thibaut and Connolly (2013),
#' and the pairwise correlations of the series.
#'
#' Series are chosen as by Ong et al. (2021): the median catch is above `min_median` (10) tonnes a
#' year and more than `min_nonzero` (3/4) of the years have catch; with `by = "species"`, only
#' items identified to species are used (Ong et al. left out miscellaneous and "nei" items). Each
#' series is detrended with a linear regression on year (as by Ong et al.), since trends make
#' series look synchronous (Leps et al. 2019); use `detrend = "none"` to keep them. From the
#' standard deviations \eqn{\sigma_i} of the series, the standard deviation \eqn{\sigma_T} of the
#' total catch and its mean \eqn{m_T}:
#' \itemize{
#'   \item synchrony \eqn{\phi = \sigma_T^2 / (\sum_i \sigma_i)^2}, from 0 (fluctuations fully
#'         compensate each other) to 1 (perfectly synchronous) (Loreau and de Mazancourt 2008, eq. 9);
#'   \item the coefficient of variation (CV) of the total catch equals \eqn{\sqrt{\phi}} times the
#'         weighted mean CV of the series, \eqn{\sum_i \sigma_i / m_T} (Thibaut and Connolly 2013,
#'         eqs. 2-4): the lower the synchrony, the more stable the total;
#'   \item portfolio effect = (mean CV of the series - CV of the total) / CV of the total x 100 (Ong
#'         et al. 2021, after Carlson and Satterthwaite 2011): how much more variable the single
#'         series are than their total, in percent.
#' }
#' The same measures are computed in moving windows of `window` years (linear trend removed in each
#' window, a choice of this package) to show how synchrony changed over time. Ong et al. (2021)
#' measured synchrony with wavelet coherence, which separates time scales; this function uses the
#' simpler correlation-based measures above.
#'
#' @inheritParams summary_params
#' @param by What a series is: "species" (default) or "country".
#' @param detrend "linear" (default) to remove a linear trend from each series, or "none".
#' @param window The length in years of the moving windows. Default is 20.
#' @param top_n The number of series with the largest catch shown in the charts. Default is 20.
#' @param min_median,min_nonzero Series need a median catch above `min_median` tonnes a year and
#'        catch in more than `min_nonzero` of the years. Defaults are 10 and 0.75 (Ong et al. 2021).
#' @return Invisibly, a list with `summary` (synchrony, CVs and portfolio effect), `series` (mean,
#'         standard deviation, CV and share of the total variability of each series), `windows`
#'         (the measures in each moving window) and `correlation` (pairwise correlations of the series).
#' @references
#' Leps, J., Gotzenberger, L., Valencia, E. and de Bello, F. (2019). Accounting for long-term
#' directional trends on year-to-year synchrony in species fluctuations. Ecography 42: 1728-1741.
#' \doi{10.1111/ecog.04528}
#'
#' Loreau, M. and de Mazancourt, C. (2008). Species synchrony and its drivers: neutral and nonneutral
#' community dynamics in fluctuating environments. American Naturalist 172: E48-E66. \doi{10.1086/589746}
#'
#' Ong, J.J.L., Walter, J.A., Jensen, O.P. and Pinsky, M.L. (2021). Global hotspots of coherent
#' marine fishery catches. Ecological Applications 31: e02321. \doi{10.1002/eap.2321}
#'
#' Thibaut, L.M. and Connolly, S.R. (2013). Understanding diversity-stability relationships: towards
#' a unified model of portfolio effects. Ecology Letters 16: 140-150. \doi{10.1111/ele.12019}
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' catch_synchrony(FAO34$area_data, lang = "zh")
#' catch_synchrony(FAO34$area_data, by = "country", window = 15)
#' }
catch_synchrony <- function(area_data, by = c("species", "country"), detrend = c("linear", "none"), window = 20,
                            top_n = 20, min_median = 10, min_nonzero = 0.75, plot = TRUE, table = TRUE, lang = NULL,
                            pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  by <- match.arg(by)
  detrend <- match.arg(detrend)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), paste0("synchrony_", by))
  column <- c(species = "ASFIS.species..Name.", country = "Country..Name.")[[by]]

  data <- area_data[!is.na(area_data$Production), ]
  if (by == "species") {
    data <- fao_add_groups(data)
    data <- data[data$taxon_level %in% "species", ]
  }
  years <- seq(min(data$Year), max(data$Year))
  yearly <- stats::aggregate(list(Tonnes = data$Production), by = list(series = data[[column]], Year = data$Year), FUN = sum)
  pieces <- split(yearly, yearly$series)
  catch <- vapply(pieces, function(s) {
    x <- s$Tonnes[match(years, s$Year)]
    x[is.na(x)] <- 0
    x
  }, numeric(length(years)))
  catch <- matrix(catch, nrow = length(years), dimnames = list(years, names(pieces)))
  keep <- apply(catch, 2, function(x) stats::median(x) > min_median && mean(x > 0) > min_nonzero)
  catch <- catch[, keep, drop = FALSE]
  if (ncol(catch) < 2) stop("Fewer than two series meet the criteria (median catch above ", min_median,
                            " t and catch in more than ", 100 * min_nonzero, "% of the years).", call. = FALSE)
  catch <- catch[, order(-colSums(catch)), drop = FALSE]

  measures <- fao_synchrony(catch, years, detrend)
  residuals <- measures$residuals
  summary <- data.frame(n_series = ncol(catch), first_year = min(years), last_year = max(years), detrend = detrend,
                        synchrony_phi = measures$phi, cv_total = measures$cv_total, weighted_mean_cv = measures$weighted_cv,
                        mean_cv_series = measures$mean_cv, portfolio_effect_pct = measures$portfolio,
                        mean_pairwise_correlation = measures$mean_correlation,
                        positive_correlations_pct = measures$positive_pct)

  sds <- apply(residuals, 2, stats::sd)
  series <- data.frame(series_en = colnames(catch), mean_tonnes = colMeans(catch), sd_tonnes = sds,
                       cv = sds / colMeans(catch), share_of_variability_pct = 100 * sds / sum(sds),
                       stringsAsFactors = FALSE, row.names = NULL)
  series <- cbind(series = fao_translate(series$series_en, by, lang), series)

  windows <- NULL
  if (length(years) >= window + 2) {
    starts <- seq(1, length(years) - window + 1)
    windows <- do.call(rbind, lapply(starts, function(s) {
      rows <- s:(s + window - 1)
      sub <- catch[rows, , drop = FALSE]
      sub <- sub[, apply(sub, 2, stats::sd) > 0, drop = FALSE]
      if (ncol(sub) < 2) return(NULL)
      m <- fao_synchrony(sub, years[rows], detrend)
      data.frame(first_year = years[rows[1]], last_year = years[rows[window]], mid_year = mean(years[rows]),
                 n_series = ncol(sub), synchrony_phi = m$phi, cv_total = m$cv_total, mean_cv_series = m$mean_cv,
                 portfolio_effect_pct = m$portfolio)
    }))
  }

  correlation <- measures$correlation
  dimnames(correlation) <- list(series$series, series$series)
  message(ncol(catch), " ", by, " series: synchrony phi = ", round(measures$phi, 2), ", portfolio effect = ",
          round(measures$portfolio), "%.")

  if (plot) {
    caption <- fao_label(if (detrend == "linear") "synchrony_note_linear" else "synchrony_note_none", lang)
    if (!is.null(windows)) {
      long <- rbind(
        data.frame(Year = windows$mid_year, value = windows$synchrony_phi, measure = fao_label("synchrony_phi", lang)),
        data.frame(Year = windows$mid_year, value = windows$portfolio_effect_pct, measure = fao_label("portfolio_effect", lang)))
      long$measure <- factor(long$measure, levels = unique(long$measure))
      p_window <- ggplot2::ggplot(long, ggplot2::aes(x = Year, y = value)) +
        ggplot2::geom_line(colour = "#3C5488", linewidth = 1) +
        ggplot2::geom_point(colour = "#3C5488", size = 1) +
        ggplot2::facet_wrap(~measure, ncol = 1, scales = "free_y") +
        ggplot2::labs(x = fao_label("window_mid_year", lang, n = window), y = NULL, caption = caption) +
        fao_theme()
      fao_save_plot(p_window, file.path("summary", name), width = 12, height = 8, pptx = pptx)
    }

    top <- utils::head(series, top_n)
    bars <- rbind(data.frame(item = fao_label("total_catch", lang), cv = measures$cv_total, total = TRUE),
                  data.frame(item = top$series, cv = top$cv, total = FALSE))
    bars$item <- factor(bars$item, levels = rev(bars$item))
    p_cv <- ggplot2::ggplot(bars, ggplot2::aes(x = cv, y = item, fill = total)) +
      ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
      ggplot2::geom_vline(xintercept = measures$cv_total, linetype = "dashed", colour = "#D7191C") +
      ggplot2::scale_fill_manual(values = c(`TRUE` = "#D7191C", `FALSE` = "grey60")) +
      ggplot2::labs(x = fao_label("cv_detrended", lang), y = NULL, caption = caption) +
      fao_theme()
    fao_save_plot(p_cv, file.path("summary", paste0(name, "_cv")), width = 10, height = max(4, 0.35 * nrow(bars) + 2),
                  pptx = pptx)

    k <- min(top_n, ncol(correlation))
    sub <- correlation[seq_len(k), seq_len(k), drop = FALSE]
    heat <- data.frame(row = factor(rep(rownames(sub), times = k), levels = rev(rownames(sub))),
                       col = factor(rep(colnames(sub), each = k), levels = colnames(sub)), r = as.vector(sub))
    p_heat <- ggplot2::ggplot(heat, ggplot2::aes(x = col, y = row, fill = r)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", limits = c(-1, 1)) +
      ggplot2::labs(x = NULL, y = NULL, fill = fao_label("correlation", lang), caption = caption) +
      fao_theme(legend = "right") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
    fao_save_plot(p_heat, file.path("summary", paste0(name, "_correlation")), width = 12, height = 11, pptx = pptx)
  }
  if (table) {
    fao_save_table(summary, name)
    fao_save_table(series, paste0(name, "_series"))
    if (!is.null(windows)) fao_save_table(windows, paste0(name, "_windows"))
    fao_save_table(cbind(series = rownames(correlation), as.data.frame(correlation, check.names = FALSE)),
                   paste0(name, "_correlation"))
  }
  invisible(list(summary = summary, series = series, windows = windows, correlation = correlation))
}

# Synchrony (Loreau & de Mazancourt 2008), CVs (Thibaut & Connolly 2013) and portfolio effect
# (Ong et al. 2021) of a matrix of catch (years x series)
fao_synchrony <- function(catch, years, detrend = "linear") {
  residuals <- if (detrend == "linear") {
    apply(catch, 2, function(x) stats::residuals(stats::lm(x ~ years)))
  } else {
    sweep(catch, 2, colMeans(catch))
  }
  residuals <- matrix(residuals, nrow = nrow(catch), dimnames = dimnames(catch))
  sds <- apply(residuals, 2, stats::sd)
  sd_total <- stats::sd(rowSums(residuals))
  means <- colMeans(catch)
  mean_total <- sum(means)
  cv_total <- sd_total / mean_total
  mean_cv <- mean(sds / means)
  correlation <- suppressWarnings(stats::cor(residuals))
  pairs <- correlation[upper.tri(correlation)]
  list(residuals = residuals, phi = sd_total^2 / sum(sds)^2, cv_total = cv_total, weighted_cv = sum(sds) / mean_total,
       mean_cv = mean_cv, portfolio = 100 * (mean_cv - cv_total) / cv_total, correlation = correlation,
       mean_correlation = mean(pairs, na.rm = TRUE), positive_pct = 100 * mean(pairs > 0, na.rm = TRUE))
}
