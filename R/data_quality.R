#' Data Quality of the Catch Statistics
#'
#' This function shows two aspects of data quality over time:
#' \itemize{
#'   \item the share of the catch reported officially, and the share estimated (by the reporting
#'         country or imputed by FAO), from the FAO status flags;
#'   \item the share of the catch identified to species, genus, family, or only to a higher
#'         group (e.g. "Marine fishes nei"), from the FAO taxonomic codes.
#' }
#' It also lists, for the countries with the largest catch in the last five years, how much of
#' their catch in those years was estimated or identified to species.
#'
#' A large estimated share in the latest years usually means that FAO has not yet received the
#' national reports; these values are often revised in later releases.
#'
#' @inheritParams summary_params
#' @param top_n The number of countries in the country table. Default is 10.
#' @return Invisibly, a list with `data_status` and `taxon_level` (yearly catch by category)
#'         and `countries` (country table).
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' data_quality(FAO34$area_data, lang = "zh")
#' }
data_quality <- function(area_data, top_n = 10, plot = TRUE, table = TRUE, lang = NULL,
                         pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), "data_quality")
  last_year <- max(area_data$Year, na.rm = TRUE)

  data <- fao_add_groups(area_data)
  data <- data[!is.na(data$Production), ]

  status_labels <- c(actual = fao_label("data_actual", lang), estimate = fao_label("data_estimate", lang),
                     other = fao_label("data_other", lang))
  status <- ifelse(data$Production.Type %in% c("actual", "estimate"), data$Production.Type, "other")
  data$data_status <- factor(status_labels[status], levels = status_labels)

  level_labels <- c(species = fao_label("level_species", lang), genus = fao_label("level_genus", lang),
                    family = fao_label("level_family", lang), higher = fao_label("level_higher", lang))
  level <- ifelse(is.na(data$taxon_level), "higher", data$taxon_level)
  data$taxon <- factor(level_labels[level], levels = level_labels)
  data$is_species <- level == "species"

  yearly_status <- fao_yearly_by(data, "data_status")
  yearly_taxon <- fao_yearly_by(data, "taxon")

  recent <- data[data$Year > last_year - 5, ]
  countries <- dplyr::summarise(
    dplyr::group_by(recent, Country..Name.),
    catch_last_5_years = sum(Production),
    estimated_pct = 100 * sum(Production[Production.Type == "estimate"]) / catch_last_5_years,
    species_level_pct = 100 * sum(Production[is_species]) / catch_last_5_years,
    .groups = "drop"
  )
  countries <- utils::head(countries[order(-countries$catch_last_5_years), ], top_n)
  countries <- cbind(country = fao_translate(countries$Country..Name., "country", lang), countries)
  names(countries)[names(countries) == "Country..Name."] <- "country_en"

  if (plot) {
    fao_stack_charts(yearly_status, fao_label("data_status", lang), file.path("summary", paste0(name, "_status")),
                     lang, pptx, colors = stats::setNames(c("#1A9641", "#FDAE61", "grey70"), status_labels),
                     charts = "share")
    fao_stack_charts(yearly_taxon, fao_label("taxon_level", lang), file.path("summary", paste0(name, "_taxon")),
                     lang, pptx, colors = stats::setNames(c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C"), level_labels),
                     charts = "share")
  }
  if (table) {
    fao_save_table(fao_category_table(yearly_status, fao_label("data_status", lang)), paste0(name, "_status"))
    fao_save_table(fao_category_table(yearly_taxon, fao_label("taxon_level", lang)), paste0(name, "_taxon"))
    fao_save_table(countries, paste0(name, "_countries"))
  }
  invisible(list(data_status = yearly_status, taxon_level = yearly_taxon, countries = countries))
}
