# Fishing Area Species Analysis

This function analyzes the fishing area data and generates plots for
specified species based on their ranking in terms of total tonnes.

## Usage

``` r
fishing_area_species(
  area_data,
  rank_range = 1:10,
  rank_years = NULL,
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  line_width = 1.5,
  point_size = 1.8,
  point_alpha = 0.7,
  point_shape = 1,
  line_alpha = 0.7,
  axis_text_size = 10,
  axis_title_size = 10,
  strip_text_size = 10,
  axis_text_face = "plain",
  axis_title_face = "bold",
  strip_text_face = "bold",
  smooth_method = "loess",
  smooth_linetype = "dashed",
  line_linewidth = 1.5,
  text_hjust = 0.9,
  text_vjust = -4,
  text_size = 4.5,
  text_color = "black",
  facet_ncol = 2,
  h_value = 0.15,
  ZH = FALSE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show")
)
```

## Arguments

- area_data:

  A data frame containing the fishing area data.

- rank_range:

  The ranks of the species to show, ranked by their catch (see
  `rank_years`): the ranks listed, e.g. `1:10` (default) for the first
  ten, `c(5, 6)` for the fifth and sixth, or `c(1, 10)` for the first
  and the tenth. Ranks beyond the number of species are left out. Until
  version 0.9.0 two numbers gave the first and the last rank (`c(1, 10)`
  was the first ten).

- rank_years:

  The number of recent years whose catch ranks the species items, e.g. 5
  for the last five years (as in the tables of
  [`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)).
  Default is NULL: the catch of all years. The charts show the whole
  series either way.

- plot:

  A logical value indicating whether to generate and save plots.

- table:

  A logical value indicating whether to generate and save tables

- timeseries_analysis:

  A logical value indicating whether to analysis timeseries.

- line_width:

  A numeric value specifying the line width. Default is 1.5.

- point_size:

  A numeric value specifying the point size. Default is 1.8.

- point_alpha:

  A numeric value specifying the point alpha. Default is 0.7.

- point_shape:

  A numeric or character value specifying the shape of the points.
  Default is 1.

- line_alpha:

  A numeric value specifying the line alpha. Default is 0.7.

- axis_text_size:

  A numeric value specifying the size of the axis text. Default is 10.

- axis_title_size:

  A numeric value specifying the size of the axis title. Default is 10.

- strip_text_size:

  A numeric value specifying the size of the strip text. Default is 10.

- axis_text_face:

  The font face of the axis text. Default is "plain".

- axis_title_face:

  The font face of the axis title. Default is "bold".

- strip_text_face:

  The font face of the strip text. Default is "bold".

- smooth_method:

  A character string specifying the smoothing method to be used. Default
  is "loess".

- smooth_linetype:

  A character string specifying the line type for the smoothing curve.
  Default is "dashed".

- line_linewidth:

  A numeric value specifying the line width for the line geom. Default
  is 1.5.

- text_hjust:

  A numeric value specifying the horizontal justification of the text
  geom. Default is 0.9.

- text_vjust:

  A numeric value specifying the vertical justification of the text
  geom. Default is -4.

- text_size:

  A numeric value specifying the size of the text geom. Default is 4.5.

- text_color:

  A character string specifying the color of the text geom. Default is
  "black".

- facet_ncol:

  A numeric value specifying the number of columns to be used in
  facet_wrap. Default is 2.

- h_value:

  The minimum segment size passed to
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html):
  a fraction of the series length (default 0.15) or a number of years.

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

A list with `area_species` (yearly catch of every species item in the
area), `area_species_ranked` (yearly catch of the ranked species, with
the others grouped), `area_species_ranked_total` (total catch and rank
of the selected species) and `coefs_data` (start year, end year and mean
catch of each period between breakpoints). Charts and tables are saved
if `plot` and `table` are TRUE.

## See also

Other fishing area analyses:
[`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md),
[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
FAO27 <- fishing_area_data("Atlantic, Northeast")
fishing_area_species(FAO27$area_data, rank_range = 1:10)    # the ten main species
fishing_area_species(FAO27$area_data, rank_range = 11:20)   # the next ten
} # }
```
