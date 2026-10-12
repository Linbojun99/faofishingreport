# Analyze and Plot Specified Species Data Ranked by Country Production

This function takes a data frame and a specified species name, analyzes
the data for the specified species, and identifies the top countries by
production at the ranks chosen with `rank_range`.

## Usage

``` r
species_country(
  area_data,
  species_name,
  rank_range = 1:10,
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  h_value = 0.15,
  polygon_color = "grey90",
  fill_scale = NULL,
  axis_title_face = "bold",
  axis_title_size_map = 14,
  axis_text_size_map = 12,
  legend_text_size_map = 12,
  strip_text_face = "bold",
  axis_text_size = 10,
  axis_title_size = 12,
  strip_text_size = 12,
  ZH = FALSE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show")
)
```

## Arguments

- area_data:

  A data frame containing the fishing area data.

- species_name:

  A string specifying the species to be analyzed.

- rank_range:

  The ranks of the countries to show, ranked by their catch of the
  species: the ranks listed, e.g. `1:10` (default) for the first ten,
  `c(5, 6)` for the fifth and sixth, or `c(1, 10)` for the first and the
  tenth. Ranks beyond the number of countries are left out. Until
  version 0.9.0 two numbers gave the first and the last rank (`c(1, 10)`
  was the first ten).

- plot:

  A logical value indicating whether to generate and save plots.

- table:

  A logical value indicating whether to generate and save tables

- timeseries_analysis:

  A logical value indicating whether to analysis timeseries.

- h_value:

  The minimum segment size passed to
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html):
  a fraction of the series length (default 0.15) or a number of years.

- polygon_color:

  The color of the borders of the polygons in the map plot. Default is
  "grey90".

- fill_scale:

  The colour palette of the map. Default is NULL, which uses
  `map_palette` of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("deep-orange" unless changed); see
  [`?fao_settings`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  for the choices.

- axis_title_face:

  The font face of the axis title. Default is "bold".

- axis_title_size_map:

  The size of the axis title text in the map plot. Default is 14.

- axis_text_size_map:

  The size of the axis text in the map plot. Default is 12.

- legend_text_size_map:

  The size of the legend text in the map plot. Default is 12.

- strip_text_face:

  The font face of the strip text. Default is "bold".

- axis_text_size:

  The size of the axis text. Default is 10.

- axis_title_size:

  The size of the axis title text. Default is 12.

- strip_text_size:

  The size of the strip text. Default is 12.

- ZH:

  A logical value. If TRUE, chart text and names are in Chinese. Default
  is FALSE. `ZH = TRUE` is the same as `lang = "zh"`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  "zh" if `ZH = TRUE`, and otherwise the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- pptx:

  A logical value. Charts are always saved as PNG in `figures/png/`; if
  TRUE they are also saved as editable PowerPoint files in
  `figures/pptx/`. Default is FALSE, which can be changed for all
  functions with `fao_settings(pptx = TRUE)`.

- plot_show:

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

## Value

A data frame with the yearly catch of the species by the ranked
countries (the others grouped); with `timeseries_analysis = TRUE`, a
list of this data frame (`species_country_ranked`) and the periods
between breakpoints of each country (`coefs_data`). The result is
returned invisibly, after a summary in the console (see `show_data` in
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)).
Charts and tables are saved if `plot` and `table` are TRUE.

## See also

Other species and country analyses:
[`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md),
[`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)

## Examples

``` r
if (FALSE) { # \dontrun{
FAO27 <- fishing_area_data("Atlantic, Northeast")
species_country(FAO27$area_data, "Atlantic cod", rank_range = 1:10)    # the ten main countries
species_country(FAO27$area_data, "Atlantic cod", rank_range = c(5, 6)) # the fifth and sixth
} # }
```
