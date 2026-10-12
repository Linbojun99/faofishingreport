#' Synchronous Breakpoints in the Catch of the Main Species
#'
#' This function runs a breakpoint analysis (changes in the mean catch, `strucchange::breakpoints`)
#' on the catch series of the `top_n` species with the largest catch, and counts how many species
#' change in each year. Years in which many species change together may point to an ecosystem-wide
#' regime shift, or to changes in fisheries management or reporting.
#'
#' The year of a breakpoint is the first year of the new segment. Species whose series is too
#' short for `h_value` are skipped with a message.
#'
#' @inheritParams summary_params
#' @param top_n The number of species with the largest total catch to analyse. Default is 20.
#' @param h_value The minimum segment size passed to `strucchange::breakpoints()`: a fraction of
#'        the series length (default 0.15) or a number of years.
#' @return Invisibly, a list with `breakpoints` (one row per species) and `yearly` (number of
#'         species with a breakpoint in each year).
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
#' regime_shift(FAO27$area_data, lang = "zh")
#' }
regime_shift <- function(area_data, top_n = 20, h_value = 0.15, plot = TRUE, table = TRUE, lang = NULL,
                         pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), label = NULL) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), "regime_shift")
  last_year <- max(area_data$Year, na.rm = TRUE)

  yearly <- stats::aggregate(list(Tonnes = area_data$Production),
                             by = list(species = area_data$ASFIS.species..Name., Year = area_data$Year),
                             FUN = sum, na.rm = TRUE)
  # species without any catch have no series to analyse
  totals <- sort(tapply(yearly$Tonnes, yearly$species, sum), decreasing = TRUE)
  totals <- totals[!is.na(totals) & totals > 0]
  top <- names(totals)[seq_len(min(top_n, length(totals)))]

  breaks <- do.call(rbind, lapply(top, function(species) {
    s <- yearly[yearly$species == species, ]
    years <- seq(min(s$Year[s$Tonnes > 0]), last_year)
    catch <- s$Tonnes[match(years, s$Year)]
    catch[is.na(catch)] <- 0
    break_years <- tryCatch({
      bp <- strucchange::breakpoints(stats::ts(catch, start = years[1]) ~ 1, h = h_value)
      if (all(is.na(bp$breakpoints))) integer(0) else years[bp$breakpoints] + 1L
    }, error = function(e) {
      message("Breakpoint analysis skipped for ", species, ": ", conditionMessage(e))
      NULL
    })
    if (is.null(break_years)) return(NULL)
    data.frame(species_en = species, first_year = years[1], total_tonnes = sum(catch),
               n_breakpoints = length(break_years),
               breakpoint_years = paste(break_years, collapse = ", "), stringsAsFactors = FALSE)
  }))
  if (is.null(breaks)) stop("No species could be analysed; try a larger h_value (e.g. 3).", call. = FALSE)
  breaks <- cbind(species = fao_translate(breaks$species_en, "species", lang), breaks)

  break_years <- as.integer(unlist(strsplit(breaks$breakpoint_years[breaks$n_breakpoints > 0], ", ")))
  counts <- data.frame(Year = seq(min(area_data$Year, na.rm = TRUE), last_year))
  counts$n_species <- vapply(counts$Year, function(y) sum(break_years == y), integer(1))

  if (plot) {
    p_counts <- ggplot2::ggplot(counts, ggplot2::aes(x = Year, y = n_species)) +
      ggplot2::geom_col(fill = "#3C5488", width = 0.8) +
      ggplot2::geom_text(data = counts[counts$n_species >= max(2, sort(counts$n_species, decreasing = TRUE)[3], na.rm = TRUE), ],
                         ggplot2::aes(label = Year), vjust = -0.4, size = 3.5, family = fao_font()) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(counts$Year)) +
      ggplot2::scale_y_continuous(breaks = function(l) seq(0, ceiling(l[2])), expand = c(0, 0, 0.1, 0)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("n_breakpoints", lang)) +
      fao_theme()
    fao_save_plot(p_counts, file.path("summary", name), width = 12, height = 6, pptx = pptx)

    points <- do.call(rbind, lapply(seq_len(nrow(breaks)), function(i) {
      years <- as.integer(strsplit(breaks$breakpoint_years[i], ", ")[[1]])
      if (length(years) == 0) return(NULL)
      data.frame(species = breaks$species[i], Year = years)
    }))
    spans <- data.frame(species = breaks$species, start = breaks$first_year, end = last_year)
    species_order <- rev(breaks$species)
    spans$species <- factor(spans$species, levels = species_order)
    p_species <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = spans, ggplot2::aes(x = start, xend = end, y = species, yend = species),
                            colour = "grey75", linewidth = 2) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(counts$Year)) +
      ggplot2::labs(x = fao_label("year", lang), y = NULL) +
      fao_theme()
    if (!is.null(points)) {
      points$species <- factor(points$species, levels = species_order)
      p_species <- p_species +
        ggplot2::geom_point(data = points, ggplot2::aes(x = Year, y = species), colour = "#D7191C", size = 2.5)
    }
    fao_save_plot(p_species, file.path("summary", paste0(name, "_species")), width = 12,
                  height = max(4, 0.35 * nrow(breaks) + 1.5), pptx = pptx)
  }
  if (table) {
    fao_save_table(breaks, name)
    fao_save_table(counts, paste0(name, "_years"))
  }
  fao_run_result(list(breakpoints = breaks, yearly = counts), run, fao_run_title("regime_shift", area_data, label),
                 main = fao_cols(breaks, c("species", "first_year", "n_breakpoints", "breakpoint_years")),
                 note = "Breakpoints of the catch of the main species:")
}
