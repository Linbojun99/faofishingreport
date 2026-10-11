#' @keywords internal
"_PACKAGE"

# Functions the analysis code calls without a package prefix. Importing them here lets
# faofishingreport::fishing_area_data() and friends work even without library(faofishingreport).
#' @importFrom magrittr %>%
#' @importFrom rlang := .data
#' @importFrom dplyr arrange desc filter group_by if_else left_join mutate pull slice summarise ungroup
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot ggsave guides labs position_stack theme_minimal
#' @importFrom strucchange breakpoints
#' @importFrom forcats fct_reorder
#' @importFrom stats coef na.omit reorder ts
#' @importFrom utils head str
NULL

# Column names used in dplyr and ggplot2 code of the analysis functions
utils::globalVariables(c(
  "area_name", "area_species_production", "ASFIS.species..Name.", "avg_production",
  "catch_last_5_years", "category", "country_production", "Country..Name.", "Decade",
  "end", "end_year", "FAO.major.fishing.area..Name.", "Global_Tonnes", "global_yearly_total",
  "intercept", "is_species", "last_year_tonnes", "mean_Percentage_area", "n_species",
  "pct_of_peak", "peak_tonnes", "percentage", "Percentage", "Percentage_area",
  "percentage_country", "percentage_of_global", "percentage_of_total", "period", "Production",
  "Production.Type", "Rank", "species", "species_area_production", "species_production", "start",
  "start_year", "status_label", "text", "Tonnes", "total_2021_production",
  "total_species_production", "total_tonnes", "Total_Tonnes", "trend", "value", "Year",
  "yearly_production",
  # cmsy_chart_forecast(), cmsy_chart_kobe_all(), compare_releases(), distant_water_profile()
  "area", "change_tonnes", "country", "direction", "first_year", "net_change_tonnes", "release",
  "scenario", "Stock", "tonnes",
  # catch_shocks()
  "group_name", "last_year", "loss_pct", "n_series", "n_shocks", "series_name", "shock_frequency_pct",
  "shocks_per_series"
))
