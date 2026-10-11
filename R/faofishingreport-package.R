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
