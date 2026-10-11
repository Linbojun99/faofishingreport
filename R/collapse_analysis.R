#' Fishery Collapses
#'
#' This function finds collapsed catch series, with the definitions of Mullon et al. (2005) or
#' Worm et al. (2006), and gives how many series collapsed, when, in which areas and species groups
#' and, following Mullon et al. (2005), the shape of the decline before the collapse.
#'
#' \describe{
#'   \item{"mullon2005" (default)}{A series is the catch of one species or genus by one country in
#'     one FAO area. Left out, as by Mullon et al. (2005, p. 112): items not identified to genus or
#'     species; the countries in `exclude_countries` (by default those Mullon et al. left out: the
#'     former USSR, Yugoslavia and Czechoslovakia, whose series end with the country; South Africa
#'     and Namibia, whose series were split; and China, whose series they considered distorted);
#'     series with a missing year among the last `complete_years` (20) years; and series whose mean
#'     catch is below `min_mean_catch` (500) tonnes a year. A series has collapsed when, in a year
#'     after its maximum catch Xmax, catch stays below Xmax/10 for 4 consecutive years; the first
#'     of these years is the collapse year Dc (p. 113). The decline before the collapse is
#'     \emph{plateau} if catch was above Xmax/4 in every year from Dc-8 to Dc-3, \emph{smooth} if the
#'     correlation between catch and time from Dc-10 to Dc is below -0.7, and \emph{erratic}
#'     otherwise (p. 114). Mullon et al. do not say in which order the rules apply; they are applied
#'     here in that order. Collapses less than 10 years after the first year of the series are not
#'     classified. Mullon et al. found 366 collapses among 1,519 series from 1950 to 2000 (24\%):
#'     21\% plateau, 33\% smooth and 45\% erratic.}
#'   \item{"worm2006"}{A fishery is the catch of one species item in one FAO area, all countries
#'     together (Worm et al. 2006 used species in large marine ecosystems from Sea Around Us data).
#'     Fisheries with a total catch below `min_total_catch` (10 000 tonnes) are left out. A fishery
#'     starts when its catch first reaches 10\% of its maximum, and is collapsed in a year when its
#'     catch is below 10\% of its maximum (recovered when it is back above). The function gives,
#'     for each year, the share of fisheries currently collapsed and the share that have collapsed
#'     at least once (Worm et al. 2006, main text and supplementary pp. 4-5).}
#' }
#'
#' Catch data alone cannot tell a collapse of the stock from a fall in catch caused by management,
#' markets or reporting. Wilberg and Miller (2007) showed that random catch series with no trend
#' also give a growing share of "collapsed" fisheries with the definition of Worm et al. (2006),
#' because the maximum of a series grows with its length; with `null_model = TRUE` the function
#' repeats the analysis on such random series (stationary first-order autoregressive lognormal
#' series with a coefficient of variation of 80\% and an autocorrelation of 0.75, as in Wilberg
#' and Miller 2007), with the same start years and lengths as the real series, for comparison.
#'
#' @inheritParams summary_params
#' @param method "mullon2005" (default) or "worm2006".
#' @param exclude_countries Countries left out with method "mullon2005". Default is those left out
#'        by Mullon et al. (2005).
#' @param complete_years With method "mullon2005", series must have a record in each of the last
#'        this many years of the data. Default is 20.
#' @param min_mean_catch With method "mullon2005", the minimum mean catch of a series in tonnes a
#'        year. Default is 500.
#' @param min_total_catch With method "worm2006", the minimum total catch of a fishery in tonnes.
#'        Default is 10 000.
#' @param null_model A logical value. If TRUE (default), the analysis is repeated on random series
#'        with no trend (Wilberg and Miller 2007).
#' @param n_sim The number of random data sets of the null model. Default is 100.
#' @return Invisibly, a list with `series` (one row per series: maximum, collapse year and type),
#'         `summary` (number and share of collapsed series by type, with the null model), `by_year`
#'         (collapses or the share of collapsed fisheries in each year) and `by_group` (share of
#'         collapsed series by FAO area and species group).
#' @references
#' Mullon, C., Freon, P. and Cury, P. (2005). The dynamics of collapse in world fisheries. Fish
#' and Fisheries 6: 111-120. \doi{10.1111/j.1467-2979.2005.00181.x}
#'
#' Wilberg, M.J. and Miller, T.J. (2007). Comment on "Impacts of biodiversity loss on ocean
#' ecosystem services". Science 316: 1285. \doi{10.1126/science.1137946}
#'
#' Worm, B., Barbier, E.B., Beaumont, N. et al. (2006). Impacts of biodiversity loss on ocean
#' ecosystem services. Science 314: 787-790. \doi{10.1126/science.1132294}
#' @export
#' @examples
#' \dontrun{
#' collapse_analysis(load_fao_data(), lang = "zh")                   # Mullon et al. (2005), world
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' collapse_analysis(FAO34$area_data, method = "worm2006")
#' }
collapse_analysis <- function(area_data, method = c("mullon2005", "worm2006"),
                              exclude_countries = c("Un. Sov. Soc. Rep.", "Yugoslavia SFR", "Czechoslovakia",
                                                    "South Africa", "Namibia", "China"),
                              complete_years = 20, min_mean_catch = 500, min_total_catch = 10000,
                              null_model = TRUE, n_sim = 100, plot = TRUE, table = TRUE, lang = NULL,
                              pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  method <- match.arg(method)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), paste0("collapse_", method))
  data <- fao_add_groups(area_data[!is.na(area_data$Production), ])
  first_year <- min(data$Year)
  last_year <- max(data$Year)
  years <- seq(first_year, last_year)

  by_series <- if (method == "mullon2005") {
    data <- data[data$taxon_level %in% c("species", "genus") & !data$Country..Name. %in% exclude_countries, ]
    c("Country..Name.", "ASFIS.species..Name.", "FAO.major.fishing.area..Name.")
  } else {
    c("ASFIS.species..Name.", "FAO.major.fishing.area..Name.")
  }
  if (nrow(data) == 0) stop("No catch series left for the analysis.", call. = FALSE)
  key <- do.call(paste, c(lapply(by_series, function(k) data[[k]]), sep = " | "))
  yearly <- stats::aggregate(list(Tonnes = data$Production), by = list(series = key, Year = data$Year), FUN = sum)
  info <- data[!duplicated(key), c(by_series, "isscaap_en", "isscaap_zh")]
  info$series <- key[!duplicated(key)]

  matrix_catch <- lapply(split(yearly, yearly$series), function(s) {
    catch <- s$Tonnes[match(years, s$Year)]
    list(series = s$series[1], catch = catch)
  })

  if (method == "mullon2005") {
    kept <- Filter(function(s) {
      catch <- s$catch
      start <- which(!is.na(catch) & catch > 0)
      if (length(start) == 0) return(FALSE)
      recent <- utils::tail(catch, complete_years)
      span <- catch[start[1]:length(catch)]
      span[is.na(span)] <- 0
      all(!is.na(recent)) && mean(span) >= min_mean_catch
    }, matrix_catch)
    if (length(kept) == 0) stop("No series meets the criteria of Mullon et al. (2005).", call. = FALSE)
    series <- do.call(rbind, lapply(kept, function(s) cbind(series = s$series, fao_mullon_collapse(years, s$catch))))
    series <- merge(info, series, by = "series")
    series <- series[order(series$collapse_year, -series$max_tonnes), ]
    types <- c("plateau", "smooth", "erratic", "not classified")
    collapsed <- series[series$collapsed, ]
    summary <- data.frame(type = c("collapsed", types),
                          n = c(nrow(collapsed), vapply(types, function(t) sum(collapsed$type %in% t), integer(1))))
    summary$pct_of_series <- 100 * summary$n / nrow(series)
    summary$pct_of_collapses <- if (nrow(collapsed) > 0) 100 * summary$n / nrow(collapsed) else NA_real_
    summary$n_series <- nrow(series)
    if (null_model) {
      sims <- fao_null_series(kept, years, n_sim)
      null_share <- mean(vapply(sims, function(sim) mean(vapply(sim, fao_mullon_is_collapsed, logical(1))), numeric(1)))
      summary$null_pct_of_series <- c(100 * null_share, rep(NA_real_, length(types)))
    }
    by_year <- data.frame(Year = rep(years, length(types)), type = rep(types, each = length(years)))
    by_year$n_collapses <- mapply(function(y, t) sum(collapsed$collapse_year == y & collapsed$type == t),
                                  by_year$Year, by_year$type)
  } else {
    kept <- Filter(function(s) sum(s$catch, na.rm = TRUE) >= min_total_catch, matrix_catch)
    if (length(kept) == 0) stop("No fishery has a total catch of at least ", min_total_catch, " tonnes.", call. = FALSE)
    states <- lapply(kept, function(s) fao_worm_states(s$catch))
    series <- do.call(rbind, lapply(seq_along(kept), function(k) {
      st <- states[[k]]
      catch <- kept[[k]]$catch
      data.frame(series = kept[[k]]$series, max_tonnes = max(catch, na.rm = TRUE),
                 max_year = years[which.max(catch)], start_year = years[st$start],
                 collapsed_last_year = st$collapsed[length(years)],
                 first_collapse_year = if (any(st$collapsed, na.rm = TRUE)) years[which(st$collapsed)[1]] else NA_integer_,
                 stringsAsFactors = FALSE)
    }))
    series <- merge(info, series, by = "series")
    by_year <- fao_worm_shares(states, years)
    if (null_model) {
      sims <- fao_null_series(kept, years, n_sim)
      null <- lapply(sims, function(sim) fao_worm_shares(lapply(sim, fao_worm_states), years))
      by_year$null_pct_current <- rowMeans(sapply(null, `[[`, "pct_current"), na.rm = TRUE)
      by_year$null_pct_cumulative <- rowMeans(sapply(null, `[[`, "pct_cumulative"), na.rm = TRUE)
    }
    summary <- data.frame(type = c("collapsed in the last year", "collapsed at least once"),
                          n = c(sum(series$collapsed_last_year, na.rm = TRUE), sum(!is.na(series$first_collapse_year))))
    summary$n_series <- nrow(series)
    summary$pct_of_series <- 100 * summary$n / nrow(series)
    if (null_model) {
      last <- nrow(by_year)
      summary$null_pct_of_series <- c(by_year$null_pct_current[last], by_year$null_pct_cumulative[last])
    }
    collapsed <- series[!is.na(series$first_collapse_year), ]
  }

  collapsed_flag <- if (method == "mullon2005") series$collapsed else !is.na(series$first_collapse_year)
  groupings <- c(area = "FAO.major.fishing.area..Name.", species_group = "isscaap_en")
  by_group <- do.call(rbind, lapply(names(groupings), function(g) {
    column <- groupings[[g]]
    levels <- unique(series[[column]])
    names <- if (g == "species_group") fao_pick(levels, series$isscaap_zh[match(levels, series$isscaap_en)], lang) else
      fao_translate(levels, "area", lang)
    data.frame(grouping = g, group = levels, group_name = names,
               n_series = vapply(levels, function(l) sum(series[[column]] %in% l), integer(1)),
               n_collapsed = vapply(levels, function(l) sum(series[[column]] %in% l & collapsed_flag), integer(1)),
               stringsAsFactors = FALSE, row.names = NULL)
  }))
  by_group$pct_collapsed <- 100 * by_group$n_collapsed / by_group$n_series
  by_group <- by_group[order(by_group$grouping, -by_group$n_series), c("grouping", "group", "group_name", "n_series",
                                                                       "n_collapsed", "pct_collapsed")]
  rownames(by_group) <- NULL
  series$series_name <- vapply(strsplit(series$series, " | ", fixed = TRUE), function(p) {
    types <- if (method == "mullon2005") c("country", "species", "area") else c("species", "area")
    paste(vapply(seq_along(p), function(k) fao_translate(p[k], types[k], lang), character(1)), collapse = " | ")
  }, character(1))
  message(nrow(series), " series (", method, "): ", nrow(collapsed), " collapsed (",
          round(100 * nrow(collapsed) / nrow(series), 1), "%).")

  if (plot) {
    caption <- fao_label(paste0("collapse_note_", method), lang)
    if (method == "mullon2005") {
      type_labels <- stats::setNames(vapply(paste0("collapse_", c("plateau", "smooth", "erratic", "unclassified")),
                                            fao_label, character(1), lang = lang), types)
      chart <- by_year
      chart$type <- factor(type_labels[chart$type], levels = type_labels)
      colors <- stats::setNames(c("#2C7BB6", "#1A9641", "#D7191C", "grey70"), type_labels)
      p_year <- ggplot2::ggplot(chart, ggplot2::aes(x = Year, y = n_collapses, fill = type)) +
        ggplot2::geom_col(width = 0.85) +
        ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
        ggplot2::scale_x_continuous(breaks = fao_year_breaks(years)) +
        ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
        ggplot2::labs(x = fao_label("year", lang), y = fao_label("n_collapses", lang),
                      fill = fao_label("collapse_type", lang), caption = caption) +
        fao_theme()
      fao_save_plot(p_year, file.path("summary", name), width = 12, height = 6, pptx = pptx)

      # Mean trajectory of each type, catch relative to the maximum, around the collapse year
      classified <- collapsed[collapsed$type != "not classified", ]
      paths <- do.call(rbind, lapply(seq_len(nrow(classified)), function(i) {
        s <- kept[[classified$series[i]]]
        offset <- -10:5
        idx <- match(classified$collapse_year[i] + offset, years)
        rel <- s$catch[idx] / classified$max_tonnes[i]
        data.frame(type = classified$type[i], offset = offset, rel = rel)
      }))
      if (!is.null(paths) && nrow(paths) > 0) {
        paths <- stats::aggregate(rel ~ type + offset, paths, mean)
        paths$type <- factor(type_labels[paths$type], levels = type_labels)
        p_path <- ggplot2::ggplot(paths, ggplot2::aes(x = offset, y = rel, colour = type)) +
          ggplot2::geom_hline(yintercept = c(0.1, 0.25), linetype = "dashed", colour = "grey60") +
          ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 1.5) +
          ggplot2::scale_colour_manual(values = colors, drop = FALSE) +
          ggplot2::labs(x = fao_label("years_from_collapse", lang), y = fao_label("catch_rel_max", lang),
                        colour = fao_label("collapse_type", lang), caption = caption) +
          fao_theme()
        fao_save_plot(p_path, file.path("summary", paste0(name, "_types")), width = 10, height = 6, pptx = pptx)
      }
    } else {
      lines <- data.frame(Year = rep(by_year$Year, 2),
                          what = rep(c(fao_label("collapsed_current", lang), fao_label("collapsed_cumulative", lang)),
                                     each = nrow(by_year)),
                          pct = c(by_year$pct_current, by_year$pct_cumulative), source = "data")
      if (null_model) {
        lines <- rbind(lines, data.frame(Year = rep(by_year$Year, 2), what = unique(lines$what)[rep(1:2, each = nrow(by_year))],
                                         pct = c(by_year$null_pct_current, by_year$null_pct_cumulative), source = "null"))
      }
      lines$source <- factor(c(data = fao_label("catch_data", lang), null = fao_label("null_model", lang))[lines$source],
                             levels = c(fao_label("catch_data", lang), fao_label("null_model", lang)))
      p_share <- ggplot2::ggplot(lines, ggplot2::aes(x = Year, y = pct, colour = what, linetype = source)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_colour_manual(values = c("#D7191C", "#2C7BB6")) +
        ggplot2::scale_x_continuous(breaks = fao_year_breaks(years)) +
        ggplot2::labs(x = fao_label("year", lang), y = fao_label("pct_collapsed", lang), colour = NULL, linetype = NULL,
                      caption = caption) +
        fao_theme()
      fao_save_plot(p_share, file.path("summary", name), width = 12, height = 6, pptx = pptx)
    }

    groups <- by_group[by_group$grouping == "species_group" & by_group$n_series >= 3, ]
    if (nrow(groups) >= 2) {
      groups <- utils::head(groups[order(-groups$n_series), ], 20)
      groups$group_name <- factor(groups$group_name, levels = groups$group_name[order(groups$pct_collapsed)])
      p_group <- ggplot2::ggplot(groups, ggplot2::aes(x = pct_collapsed, y = group_name)) +
        ggplot2::geom_col(fill = "#D7191C", width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(n_collapsed, "/", n_series)), hjust = -0.1, size = 3.5,
                           family = fao_font()) +
        ggplot2::scale_x_continuous(expand = c(0, 0, 0.15, 0)) +
        ggplot2::labs(x = fao_label("pct_collapsed", lang), y = NULL, caption = caption) +
        fao_theme()
      fao_save_plot(p_group, file.path("summary", paste0(name, "_by_group")), width = 10,
                    height = max(4, 0.4 * nrow(groups) + 2), pptx = pptx)
    }
  }
  if (table) {
    fao_save_table(series, name)
    fao_save_table(summary, paste0(name, "_summary"))
    fao_save_table(by_year, paste0(name, "_by_year"))
    fao_save_table(by_group, paste0(name, "_by_group"))
  }
  invisible(list(series = series, summary = summary, by_year = by_year, by_group = by_group))
}

# Collapse of one series by the definition of Mullon et al. (2005); missing years count as zero
fao_mullon_collapse <- function(years, catch) {
  catch[is.na(catch)] <- 0
  xmax <- max(catch)
  peak <- which.max(catch)
  below <- catch < xmax / 10 & seq_along(catch) > peak
  runs <- stats::filter(as.numeric(below), rep(1, 4), sides = 1)  # number of the last 4 years below
  ends <- which(runs == 4)
  out <- data.frame(max_year = years[peak], max_tonnes = xmax, collapsed = length(ends) > 0,
                    collapse_year = NA_integer_, type = NA_character_, stringsAsFactors = FALSE)
  if (!out$collapsed) return(out)
  dc <- ends[1] - 3
  out$collapse_year <- years[dc]
  out$type <- if (dc - 10 < 1) "not classified" else if (all(catch[(dc - 8):(dc - 3)] > xmax / 4)) "plateau" else
    if (suppressWarnings(stats::cor(catch[(dc - 10):dc], 0:10)) < -0.7) "smooth" else "erratic"
  if (is.na(out$type)) out$type <- "erratic"
  out
}

# Whether a series collapsed by the definition of Mullon et al. (2005) (fast version for the null model)
fao_mullon_is_collapsed <- function(catch) {
  catch[is.na(catch)] <- 0
  below <- catch < max(catch) / 10 & seq_along(catch) > which.max(catch)
  runs <- rle(below)
  any(runs$values & runs$lengths >= 4)
}

# State of a fishery in each year by the definition of Worm et al. (2006): it starts when catch
# first reaches 10% of its maximum, and is collapsed in a year when catch is below 10% of the maximum
fao_worm_states <- function(catch) {
  catch[is.na(catch)] <- 0
  threshold <- 0.1 * max(catch)
  start <- which(catch >= threshold)[1]
  started <- seq_along(catch) >= start
  collapsed <- started & catch < threshold
  list(start = start, started = started, collapsed = collapsed, ever = cumsum(collapsed) > 0)
}

fao_worm_shares <- function(states, years) {
  started <- rowSums(sapply(states, `[[`, "started"))
  current <- rowSums(sapply(states, `[[`, "collapsed"))
  ever <- rowSums(sapply(states, `[[`, "ever"))
  data.frame(Year = years, n_fisheries = started, n_collapsed = current, n_ever_collapsed = ever,
             pct_current = ifelse(started > 0, 100 * current / started, NA_real_),
             pct_cumulative = ifelse(started > 0, 100 * ever / started, NA_real_))
}

# Random series with no trend for the null model of Wilberg and Miller (2007): stationary AR(1)
# lognormal series with CV 80% and autocorrelation 0.75, with the start years of the real series
fao_null_series <- function(series, years, n_sim, cv = 0.8, rho = 0.75) {
  sd_log <- sqrt(log(1 + cv^2))
  sd_innovation <- sd_log * sqrt(1 - rho^2)
  starts <- vapply(series, function(s) which(!is.na(s$catch) & s$catch > 0)[1], integer(1))
  lapply(seq_len(n_sim), function(i) {
    lapply(starts, function(start) {
      n <- length(years) - start + 1
      # x[1] ~ N(0, sd_log) and x[t] = rho * x[t-1] + e[t]: a stationary AR(1) process
      x <- stats::filter(c(stats::rnorm(1, 0, sd_log), stats::rnorm(n - 1, 0, sd_innovation)), rho, method = "recursive")
      c(rep(NA_real_, start - 1), exp(as.numeric(x)))
    })
  })
}
