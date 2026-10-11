#' Catch by Coastal Countries and Distant-Water Fleets
#'
#' This function splits the catch in each FAO marine fishing area between countries that border
#' the area (coastal countries) and countries that do not (distant-water fleets), and shows the
#' main distant-water countries over time.
#'
#' A country is coastal to an area if its land, including overseas territories, lies within 20 km
#' of the FAO area boundary (see `data-raw/build_coastal_countries.R`). Former countries such as
#' the USSR take the coastline of their successor states. Catch by "Other nei" is shown as unknown.
#' Inland fishing areas are left out.
#'
#' @inheritParams summary_params
#' @param top_n The number of distant-water countries shown separately. Default is 8.
#' @return Invisibly, a list with `yearly` (catch by coastal and distant-water countries),
#'         `distant` (catch by distant-water country) and `table`.
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' coastal_distant(FAO34$area_data, lang = "zh")
#' }
coastal_distant <- function(area_data, top_n = 8, plot = TRUE, table = TRUE, lang = NULL,
                            pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), "coastal_distant")

  data <- fao_add_groups(area_data)
  inland <- !data$inland_marine_en %in% "Marine areas"
  if (any(inland)) {
    message("Inland fishing areas are left out of the coastal and distant-water analysis.")
    data <- data[!inland, ]
  }
  if (nrow(data) == 0) stop("The data contains no marine fishing area.", call. = FALSE)

  coastal <- fao_coastal_table()
  is_coastal <- paste(data$FAO.major.fishing.area..Name., data$Country..Name.) %in%
    paste(coastal$area_en, coastal$country_en)
  fleet_labels <- c(coastal = fao_label("coastal", lang), distant = fao_label("distant", lang),
                    unknown = fao_label("unknown", lang))
  fleet <- ifelse(data$Country..Name. %in% c("Other nei", NA), "unknown",
                  ifelse(is_coastal, "coastal", "distant"))
  data$category <- factor(fleet_labels[fleet], levels = fleet_labels)

  yearly <- fao_yearly_by(data, "category")
  summary_table <- fao_category_table(yearly, fao_label("fleet", lang))

  distant <- data[fleet == "distant", ]
  other <- fao_label("other_distant", lang)
  yearly_distant <- NULL
  if (nrow(distant) > 0) {
    distant$category <- fao_top_categories(fao_translate(distant$Country..Name., "country", lang),
                                           distant$Production, top_n, other)
    yearly_distant <- fao_yearly_by(distant, "category")
  }

  if (plot) {
    fao_stack_charts(yearly, fao_label("fleet", lang), file.path("summary", name), lang, pptx,
                     colors = stats::setNames(c("#2C7BB6", "#D7191C", "grey70"), fleet_labels))
    if (!is.null(yearly_distant)) {
      fao_stack_charts(yearly_distant, fao_label("distant", lang), file.path("summary", paste0(name, "_countries")),
                       lang, pptx, colors = fao_colors(levels(yearly_distant$category), other), charts = "tonnes")
    }
  }
  if (table) {
    fao_save_table(summary_table, name)
    if (!is.null(yearly_distant)) {
      fao_save_table(fao_category_table(yearly_distant, fao_label("country", lang)), paste0(name, "_countries"))
    }
  }
  invisible(list(yearly = yearly, distant = yearly_distant, table = summary_table))
}
