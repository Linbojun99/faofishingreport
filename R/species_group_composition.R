#' Catch Composition by Species Group
#'
#' This function summarises the catch by ISSCAAP species group (e.g. "Herrings, sardines,
#' anchovies") or by major group (fishes, crustaceans, molluscs, ...) over time. It saves
#' stacked area charts of the catch and of each group's share, and a table by decade.
#'
#' ISSCAAP groups come from the FAO species code list. FAO gives no Chinese names for them; the
#' Chinese names used are in `inst/extdata/group_names_zh.csv` (unofficial translations).
#'
#' @inheritParams summary_params
#' @param group "isscaap" (default) for the ISSCAAP species groups, or "major" for the major groups.
#' @param top_n The number of groups shown separately; the others are combined. Default is 8.
#' @return Invisibly, a list with `yearly` (catch by group and year) and `table`.
#' @family summary analyses
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' species_group_composition(FAO34$area_data, lang = "zh")
#' }
species_group_composition <- function(area_data, group = c("isscaap", "major"), top_n = 8,
                                      plot = TRUE, table = TRUE, lang = NULL,
                                      pptx = getOption("faofishingreport.pptx", FALSE),
                                      label = NULL) {
  group <- match.arg(group)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), paste0("group_", group))

  data <- fao_add_groups(area_data)
  category <- fao_pick(data[[paste0(group, "_en")]], data[[paste0(group, "_zh")]], lang)
  category[is.na(category)] <- fao_label("unknown", lang)
  data$category <- fao_top_categories(category, data$Production, top_n, fao_label("other_group", lang))

  yearly <- fao_yearly_by(data, "category")
  group_label <- fao_label(if (group == "isscaap") "species_group" else "major_group", lang)
  summary_table <- fao_category_table(yearly, group_label)

  if (plot) {
    fao_stack_charts(yearly, group_label, file.path("summary", name), lang, pptx,
                     colors = fao_colors(levels(yearly$category), fao_label("other_group", lang)))
  }
  if (table) fao_save_table(summary_table, name)
  invisible(list(yearly = yearly, table = summary_table))
}
