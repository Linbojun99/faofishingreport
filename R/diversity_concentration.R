#' Diversity and Concentration of the Catch
#'
#' This function computes, for each year:
#' \itemize{
#'   \item the number of species reported (items identified to species with catch) and the
#'         number of countries with catch;
#'   \item the Shannon diversity index of the catch over FAO species items,
#'         \eqn{H = -\sum p_i \ln p_i}, where \eqn{p_i} is the share of item \eqn{i};
#'   \item the Herfindahl-Hirschman index (HHI) of species items and of countries, the sum of
#'         squared percentage shares (0 to 10 000; above 2 500 is usually read as highly concentrated);
#'   \item the share of the `top_n` largest species items and countries.
#' }
#'
#' @inheritParams summary_params
#' @param top_n The number of largest species and countries for the top share. Default is 5.
#' @return Invisibly, a data frame with one row per year.
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' diversity_concentration(FAO34$area_data, lang = "zh")
#' }
diversity_concentration <- function(area_data, top_n = 5, plot = TRUE, table = TRUE, lang = NULL,
                                    pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), "diversity")

  data <- fao_add_groups(area_data)
  data <- data[!is.na(data$Production) & data$Production > 0, ]

  indicators <- do.call(rbind, lapply(split(data, data$Year), function(y) {
    species <- tapply(y$Production, y$ASFIS.species..Name., sum)
    countries <- tapply(y$Production, y$Country..Name., sum)
    p_species <- species / sum(species)
    p_countries <- countries / sum(countries)
    top_share <- function(x) 100 * sum(utils::head(sort(x, decreasing = TRUE), top_n)) / sum(x)
    data.frame(
      Year = y$Year[1],
      n_species = length(unique(y$ASFIS.species..Name.[y$taxon_level %in% "species"])),
      n_countries = length(countries),
      shannon_species = -sum(p_species * log(p_species)),
      hhi_species = sum((100 * p_species)^2),
      hhi_countries = sum((100 * p_countries)^2),
      top_species_pct = top_share(species),
      top_countries_pct = top_share(countries)
    )
  }))
  names(indicators)[names(indicators) == "top_species_pct"] <- paste0("top", top_n, "_species_pct")
  names(indicators)[names(indicators) == "top_countries_pct"] <- paste0("top", top_n, "_countries_pct")

  if (plot) {
    indicator_labels <- c(
      fao_label("n_species", lang), fao_label("n_countries", lang), fao_label("shannon", lang),
      fao_label("hhi_species", lang), fao_label("hhi_country", lang),
      fao_label("top_share_species", lang, n = top_n), fao_label("top_share_country", lang, n = top_n)
    )
    long <- data.frame(
      Year = rep(indicators$Year, 7),
      indicator = factor(rep(indicator_labels, each = nrow(indicators)), levels = indicator_labels),
      value = unlist(indicators[, -1], use.names = FALSE)
    )
    p <- ggplot2::ggplot(long, ggplot2::aes(x = Year, y = value)) +
      ggplot2::geom_line(colour = "#3C5488", linewidth = 1) +
      ggplot2::geom_point(colour = "#3C5488", size = 1) +
      ggplot2::facet_wrap(~indicator, ncol = 2, scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(long$Year)) +
      ggplot2::labs(x = fao_label("year", lang), y = NULL) +
      fao_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
    fao_save_plot(p, file.path("summary", name), width = 12, height = 12, pptx = pptx)
  }
  if (table) fao_save_table(indicators, name)
  invisible(indicators)
}
