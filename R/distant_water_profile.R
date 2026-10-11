#' Distant-Water Profile of a Country
#'
#' This function follows the marine catch of one country over time in the FAO major fishing
#' areas it borders (adjacent areas) and in the areas it does not border (distant-water areas),
#' and counts the FAO areas in which it fishes each year, as a measure of the spatial expansion of
#' its fisheries. It also lists, for each distant-water area, the first and last year with catch,
#' the peak and the recent catch.
#'
#' Distant-water catch is defined as by FAO: the catch of a country in the FAO major fishing areas
#' it does not border; fishing outside its own waters but in an area it borders does not count
#' (Grainger and Garcia 1996, section 2.6; Garibaldi and Limongelli 2003, section 2.1). An area is
#' adjacent to a country as in `coastal_distant()`: the country's land, including overseas
#' territories, lies within 20 km of the FAO area boundary (a rule of this package). FAO records catch by the flag
#' of the vessel and by FAO major area, not by exclusive economic zone (EEZ), so the split shows in
#' which FAO areas a country's fleet fished, not whether the catch came from its own waters, the
#' waters of other countries or the high seas. Swartz et al. (2010) mapped the spatial expansion
#' of world fisheries with Sea Around Us catch maps; this function uses FAO statistics only, at
#' the resolution of FAO major areas.
#'
#' For China, FAO notes that in 2022 China reported 2.3 million tonnes of distant-water catch but
#' gave species and areas only for the catch landed in China (area 61); FAO filled other areas
#' with data from regional fisheries management organizations and entered 1.3 million tonnes as
#' "Marine fishes nei" in area 61 (FAO 2024). Recent distant-water catch of China is therefore
#' partly counted in its adjacent area 61.
#'
#' @inheritParams summary_params
#' @param country_name The country, as named in the FAO data (e.g. "China", "Spain").
#' @param top_n The number of distant-water areas shown separately; the others are combined.
#'        Default is 8.
#' @param min_catch An area counts as fished in a year when the country's catch there is above
#'        this many tonnes. Default is 0 (any reported catch).
#' @param data_version The FAO data release to use; see `load_fao_data()`.
#' @return Invisibly, a list with `yearly` (catch in adjacent and distant-water areas, distant-water
#'         share and number of areas fished, by year), `areas` (one row per FAO area fished) and
#'         `distant` (yearly catch by distant-water area).
#' @references
#' FAO (2024). The State of World Fisheries and Aquaculture 2024. Blue Transformation in action.
#' Rome, FAO. \doi{10.4060/cd0683en}
#'
#' Garibaldi, L. and Limongelli, L. (2003). Trends in oceanic captures and clustering of large
#' marine ecosystems: two studies based on the FAO capture database. FAO Fisheries Technical Paper
#' 435. Rome, FAO. \url{https://www.fao.org/4/y4449e/y4449e00.htm}
#'
#' Grainger, R.J.R. and Garcia, S.M. (1996). Chronicles of marine fishery landings (1950-1994):
#' trend analysis and fisheries potential. FAO Fisheries Technical Paper 359. Rome, FAO.
#' \url{https://www.fao.org/4/w3244e/w3244e00.htm}
#'
#' Swartz, W., Sala, E., Tracey, S., Watson, R. and Pauly, D. (2010). The spatial expansion and
#' ecological footprint of fisheries (1950 to present). PLoS ONE 5: e15143.
#' \doi{10.1371/journal.pone.0015143}
#' @export
#' @examples
#' \dontrun{
#' distant_water_profile("Spain", lang = "zh")
#' distant_water_profile("Korea, Republic of", min_catch = 100)
#' }
distant_water_profile <- function(country_name, top_n = 8, min_catch = 0, plot = TRUE, table = TRUE,
                                  lang = NULL, pptx = getOption("faofishingreport.pptx", FALSE), label = NULL,
                                  data_version = "latest") {
  lang <- fao_lang(lang)
  name <- fao_summary_name(if (is.null(label)) country_name else label, "distant_water")

  data <- load_fao_data(data_version)
  data <- data[data$Country..Name. %in% country_name & !is.na(data$Production), ]
  if (nrow(data) == 0) stop("No catch found for country \"", country_name, "\".", call. = FALSE)
  data <- fao_add_groups(data)
  inland <- !data$inland_marine_en %in% "Marine areas"
  if (any(inland)) {
    message("Inland fishing areas are left out of the distant-water profile.")
    data <- data[!inland, ]
  }
  if (nrow(data) == 0) stop(country_name, " has no catch in marine fishing areas.", call. = FALSE)

  coastal <- fao_coastal_table()
  adjacent_areas <- coastal$area_en[coastal$country_en %in% country_name]
  data$adjacent <- data$FAO.major.fishing.area..Name. %in% adjacent_areas
  fleet_labels <- c(adjacent = fao_label("adjacent_areas", lang), distant = fao_label("distant_areas", lang))
  data$category <- factor(fleet_labels[ifelse(data$adjacent, "adjacent", "distant")], levels = fleet_labels)

  by_area <- stats::aggregate(list(Tonnes = data$Production),
                              by = list(area = data$FAO.major.fishing.area..Name., Year = data$Year), FUN = sum)
  by_area$adjacent <- by_area$area %in% adjacent_areas
  years <- seq(min(data$Year), max(data$Year))
  fished <- by_area[by_area$Tonnes > min_catch, ]

  split_catch <- fao_yearly_by(data, "category")
  yearly <- data.frame(Year = years)
  yearly$adjacent_tonnes <- vapply(years, function(y) sum(by_area$Tonnes[by_area$Year == y & by_area$adjacent]), numeric(1))
  yearly$distant_tonnes <- vapply(years, function(y) sum(by_area$Tonnes[by_area$Year == y & !by_area$adjacent]), numeric(1))
  total <- yearly$adjacent_tonnes + yearly$distant_tonnes
  yearly$distant_pct <- ifelse(total > 0, 100 * yearly$distant_tonnes / total, NA_real_)
  yearly$n_adjacent_areas <- vapply(years, function(y) sum(fished$Year == y & fished$adjacent), integer(1))
  yearly$n_distant_areas <- vapply(years, function(y) sum(fished$Year == y & !fished$adjacent), integer(1))
  yearly$n_areas <- yearly$n_adjacent_areas + yearly$n_distant_areas

  last_year <- max(years)
  areas <- do.call(rbind, lapply(split(by_area, by_area$area), function(a) {
    a <- a[order(a$Year), ]
    with_catch <- a$Year[a$Tonnes > min_catch]
    data.frame(area_en = a$area[1], adjacent = a$adjacent[1],
               first_year = if (length(with_catch) > 0) min(with_catch) else NA_integer_,
               last_year = if (length(with_catch) > 0) max(with_catch) else NA_integer_,
               n_years = length(with_catch),
               peak_year = a$Year[which.max(a$Tonnes)], peak_tonnes = max(a$Tonnes),
               last_5_years_tonnes = sum(a$Tonnes[a$Year > last_year - 5]),
               total_tonnes = sum(a$Tonnes), stringsAsFactors = FALSE)
  }))
  distant_total <- sum(areas$total_tonnes[!areas$adjacent])
  areas$share_of_distant_pct <- ifelse(areas$adjacent | distant_total == 0, NA_real_, 100 * areas$total_tonnes / distant_total)
  areas <- areas[order(!areas$adjacent, -areas$total_tonnes), ]
  areas <- cbind(area = fao_translate(areas$area_en, "area", lang), areas)
  rownames(areas) <- NULL

  distant <- data[!data$adjacent, ]
  yearly_distant <- NULL
  other <- fao_label("other_area", lang)
  if (nrow(distant) > 0) {
    distant$category <- fao_top_categories(fao_translate(distant$FAO.major.fishing.area..Name., "area", lang),
                                           distant$Production, top_n, other)
    yearly_distant <- fao_yearly_by(distant, "category")
  }

  if (plot) {
    fao_stack_charts(split_catch, fao_label("fishing_area", lang), file.path("summary", name), lang, pptx,
                     colors = stats::setNames(c("#2C7BB6", "#D7191C"), fleet_labels))
    if (!is.null(yearly_distant)) {
      fao_stack_charts(yearly_distant, fao_label("distant_areas", lang), file.path("summary", paste0(name, "_areas")),
                       lang, pptx, colors = fao_colors(levels(yearly_distant$category), other), charts = "tonnes")
    }
    counts <- data.frame(Year = rep(years, 2),
                         category = factor(rep(fleet_labels, each = length(years)), levels = fleet_labels),
                         n = c(yearly$n_adjacent_areas, yearly$n_distant_areas))
    p_count <- ggplot2::ggplot(counts, ggplot2::aes(x = Year, y = n, fill = category)) +
      ggplot2::geom_col(width = 0.85) +
      ggplot2::scale_fill_manual(values = stats::setNames(c("#2C7BB6", "#D7191C"), fleet_labels)) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(years)) +
      ggplot2::scale_y_continuous(breaks = function(l) seq(0, ceiling(l[2]), by = max(1, ceiling(l[2] / 8))),
                                  expand = c(0, 0, 0.05, 0)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("n_areas_fished", lang), fill = NULL) +
      fao_theme()
    fao_save_plot(p_count, file.path("summary", paste0(name, "_n_areas")), width = 12, height = 6, pptx = pptx)

    timeline <- areas[!is.na(areas$first_year), ]
    if (nrow(timeline) > 0) {
      timeline$area <- factor(timeline$area, levels = rev(timeline$area[order(timeline$first_year, timeline$area)]))
      timeline$category <- factor(fleet_labels[ifelse(timeline$adjacent, "adjacent", "distant")], levels = fleet_labels)
      p_time <- ggplot2::ggplot(timeline) +
        ggplot2::geom_segment(ggplot2::aes(x = first_year, xend = last_year, y = area, yend = area, colour = category),
                              linewidth = 2) +
        ggplot2::geom_point(ggplot2::aes(x = first_year, y = area, colour = category), size = 2.5) +
        ggplot2::scale_colour_manual(values = stats::setNames(c("#2C7BB6", "#D7191C"), fleet_labels)) +
        ggplot2::scale_x_continuous(breaks = fao_year_breaks(years)) +
        ggplot2::labs(x = fao_label("year", lang), y = NULL, colour = NULL,
                      caption = fao_label("area_years_note", lang, n = min_catch)) +
        fao_theme()
      fao_save_plot(p_time, file.path("summary", paste0(name, "_timeline")), width = 12,
                    height = max(4, 0.4 * nrow(timeline) + 2), pptx = pptx)
    }
  }
  if (table) {
    fao_save_table(yearly, name)
    fao_save_table(areas, paste0(name, "_areas"))
  }
  invisible(list(yearly = yearly, areas = areas, distant = yearly_distant))
}
