#' Catch by Country Group
#'
#' This function summarises the catch by groups of countries over time: World Bank income group,
#' continent, or region, as given in the FAO country code list. It saves stacked area charts of
#' the catch and of each group's share, and a table by decade.
#'
#' @inheritParams summary_params
#' @param group "income" (default) for the World Bank income groups, "continent" or "region".
#' @param top_n The number of groups shown separately; the others are combined. Default is 8.
#' @return Invisibly, a list with `yearly` (catch by group and year) and `table`.
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' country_group_summary(FAO34$area_data, group = "income", lang = "zh")
#' }
country_group_summary <- function(area_data, group = c("income", "continent", "region"), top_n = 8,
                                  plot = TRUE, table = TRUE, lang = NULL,
                                  pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  group <- match.arg(group)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), paste0("country_", group))

  data <- fao_add_groups(area_data)
  category <- fao_pick(data[[paste0(group, "_en")]], data[[paste0(group, "_zh")]], lang)
  category[is.na(category)] <- fao_label("unknown", lang)
  other <- fao_label("other_country_group", lang)
  data$category <- fao_top_categories(category, data$Production, top_n, other)

  yearly <- fao_yearly_by(data, "category")
  group_label <- fao_label(c(income = "income_group", continent = "continent", region = "region")[[group]], lang)
  summary_table <- fao_category_table(yearly, group_label)

  if (plot) {
    fao_stack_charts(yearly, group_label, file.path("summary", name), lang, pptx,
                     colors = fao_colors(levels(yearly$category), other))
  }
  if (table) fao_save_table(summary_table, name)
  invisible(list(yearly = yearly, table = summary_table))
}
