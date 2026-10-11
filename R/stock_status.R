#' Stock Status Plots
#'
#' This function classifies the catch series of each species, every year, by its catch relative
#' to the peak catch of the series (stock status plots), and saves the share of stocks and the
#' share of catch in each status over time.
#'
#' A "stock" is the yearly catch of one FAO species item, summed over all countries in the data
#' (usually one fishing area). Each year of a stock is classified as:
#' \itemize{
#'   \item Developing: before the year of peak catch, with catch at most 50\% of the peak;
#'   \item Exploited: catch above 50\% of the peak;
#'   \item Over-exploited: after the peak, catch between 10\% and 50\% of the peak;
#'   \item Collapsed: after the peak, catch below 10\% of the peak;
#'   \item Rebuilding: after the peak and after a collapse, catch back between 10\% and 50\% of the peak.
#' }
#' Only series that meet the inclusion criteria are classified: items identified to species,
#' genus or family (pooled groups such as "Marine fishes nei" are excluded), at least `min_years`
#' years from the first to the last year with catch, at least `min_consecutive` consecutive years
#' with catch, and a total catch of at least `min_catch` tonnes. Years after the last reported
#' catch count as zero catch.
#'
#' The method uses catch data only. A falling catch can also come from changes in management,
#' markets or reporting, so the results are indicative and do not replace stock assessments.
#'
#' @inheritParams summary_params
#' @param taxon_levels Taxonomic levels of the items to include: any of "species", "genus",
#'        "family" and "higher". Default is species, genus and family.
#' @param min_years Minimum number of years from the first to the last year with catch. Default is 10.
#' @param min_consecutive Minimum number of consecutive years with catch. Default is 5.
#' @param min_catch Minimum total catch of a series, in tonnes. Default is 1000.
#' @param smooth A logical value. If TRUE, catches are smoothed with a 3-year running mean before
#'        they are classified. Default is FALSE.
#' @return Invisibly, a list with `yearly` (number of stocks and catch by status and year),
#'         `stocks` (one row per stock) and `status` (the status of each stock in each year).
#' @references
#' Kleisner, K., Zeller, D., Froese, R. and Pauly, D. (2013). Using global catch data for
#' inferences on the world's marine fisheries. Fish and Fisheries 14: 293-311.
#' \doi{10.1111/j.1467-2979.2012.00469.x}
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' stock_status(FAO34$area_data, lang = "zh")
#' }
stock_status <- function(area_data, taxon_levels = c("species", "genus", "family"),
                         min_years = 10, min_consecutive = 5, min_catch = 1000, smooth = FALSE,
                         plot = TRUE, table = TRUE, lang = NULL,
                         pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), "stock_status")
  last_year <- max(area_data$Year, na.rm = TRUE)

  data <- fao_add_groups(area_data)
  data <- data[!is.na(data$Production) & data$taxon_level %in% taxon_levels, ]
  series <- stats::aggregate(list(Tonnes = data$Production),
                             by = list(species = data$ASFIS.species..Name., Year = data$Year), FUN = sum)

  status <- do.call(rbind, lapply(split(series, series$species), function(s) {
    positive <- s$Year[s$Tonnes > 0]
    if (length(positive) == 0) return(NULL)
    years <- seq(min(positive), last_year)
    catch <- s$Tonnes[match(years, s$Year)]
    catch[is.na(catch)] <- 0
    runs <- rle(catch > 0)
    longest_run <- max(c(0, runs$lengths[runs$values]))
    if (max(positive) - min(positive) + 1 < min_years || longest_run < min_consecutive ||
        sum(catch) < min_catch) {
      return(NULL)
    }
    classified <- if (smooth) fao_running_mean(catch) else catch
    data.frame(species = s$species[1], Year = years, Tonnes = catch,
               status = fao_stock_status(classified), stringsAsFactors = FALSE)
  }))
  if (is.null(status)) {
    stop("No catch series meets the inclusion criteria of the stock status plots.", call. = FALSE)
  }

  status_keys <- c("developing", "exploited", "overexploited", "rebuilding", "collapsed")
  status_labels <- vapply(paste0("status_", status_keys), fao_label, character(1), lang = lang)
  status$status_label <- factor(status_labels[match(status$status, status_keys)], levels = status_labels)

  yearly <- dplyr::summarise(dplyr::group_by(status, Year, status_label),
                             stocks = dplyr::n(), Tonnes = sum(Tonnes), .groups = "drop")
  yearly <- tidyr::complete(yearly, Year, status_label, fill = list(stocks = 0, Tonnes = 0))
  yearly <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(yearly, Year),
                                         stocks_pct = 100 * stocks / sum(stocks),
                                         catch_pct = if (sum(Tonnes) > 0) 100 * Tonnes / sum(Tonnes) else 0))

  stocks <- dplyr::summarise(
    dplyr::group_by(status, species),
    first_year = min(Year),
    peak_year = Year[which.max(Tonnes)],
    peak_tonnes = max(Tonnes),
    last_year_tonnes = Tonnes[Year == last_year],
    pct_of_peak = 100 * last_year_tonnes / peak_tonnes,
    status_last_year = as.character(status_label[Year == last_year]),
    .groups = "drop"
  )
  stocks <- stocks[order(-stocks$peak_tonnes), ]
  stocks <- cbind(species_name = fao_translate(stocks$species, "species", lang), stocks)
  names(stocks)[names(stocks) == "species"] <- "species_en"

  if (plot) {
    colors <- stats::setNames(c("#A6D96A", "#1A9641", "#FDAE61", "#2C7BB6", "#D7191C"), status_labels)
    status_chart <- function(value, y_label) {
      ggplot2::ggplot(yearly, ggplot2::aes(x = Year, y = .data[[value]], fill = status_label)) +
        ggplot2::geom_area(colour = "white", linewidth = 0.2) +
        ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
        ggplot2::scale_x_continuous(breaks = fao_year_breaks(yearly$Year), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::labs(x = fao_label("year", lang), y = y_label, fill = fao_label("stock_status", lang),
                      caption = fao_label("n_stocks_note", lang, n = nrow(stocks))) +
        fao_theme()
    }
    fao_save_plot(status_chart("stocks_pct", fao_label("stocks_pct", lang)),
                  file.path("summary", paste0(name, "_stocks")), width = 12, height = 7, pptx = pptx)
    fao_save_plot(status_chart("catch_pct", fao_label("share_pct", lang)),
                  file.path("summary", paste0(name, "_catch")), width = 12, height = 7, pptx = pptx)
  }
  if (table) {
    fao_save_table(yearly, name)
    fao_save_table(stocks, paste0(name, "_stocks"))
  }
  invisible(list(yearly = yearly, stocks = stocks, status = status))
}

# Status of each year of a catch series relative to its peak (stock status plots)
fao_stock_status <- function(catch) {
  peak_year <- which.max(catch)
  share <- catch / max(catch)
  after_peak <- seq_along(catch) > peak_year
  # Whether the series has collapsed (below 10% of the peak) after its peak, up to each year
  collapsed_before <- cumsum(after_peak & share < 0.1) > 0
  ifelse(share > 0.5, "exploited",
         ifelse(!after_peak, "developing",
                ifelse(share < 0.1, "collapsed",
                       ifelse(collapsed_before, "rebuilding", "overexploited"))))
}

# 3-year centred running mean; the first and last years keep their values
fao_running_mean <- function(x) {
  smoothed <- as.numeric(stats::filter(x, rep(1 / 3, 3), sides = 2))
  smoothed[is.na(smoothed)] <- x[is.na(smoothed)]
  smoothed
}
