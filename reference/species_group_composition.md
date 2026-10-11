# Catch Composition by Species Group

This function summarises the catch by ISSCAAP species group (e.g.
"Herrings, sardines, anchovies") or by major group (fishes, crustaceans,
molluscs, ...) over time. It saves stacked area charts of the catch and
of each group's share, and a table by decade.

## Usage

``` r
species_group_composition(
  area_data,
  group = c("isscaap", "major"),
  top_n = 8,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL
)
```

## Arguments

- area_data:

  FAO catch data: `fishing_area_data()$area_data`, or any subset of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  (e.g. one country, or several fishing areas).

- group:

  "isscaap" (default) for the ISSCAAP species groups, or "major" for the
  major groups.

- top_n:

  The number of groups shown separately; the others are combined.
  Default is 8.

- plot:

  A logical value. If TRUE (default), charts are saved in
  `figures/png/summary/`.

- table:

  A logical value. If TRUE (default), tables are saved in `tables/`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- pptx:

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in `figures/pptx/summary/`. Default is FALSE, which can be
  changed for all functions with `fao_settings(pptx = TRUE)`.

- label:

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

## Value

Invisibly, a list with `yearly` (catch by group and year) and `table`.

## Details

ISSCAAP groups come from the FAO species code list. FAO gives no Chinese
names for them; the Chinese names used are in
`inst/extdata/group_names_zh.csv` (unofficial translations).

## See also

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
species_group_composition(FAO34$area_data, lang = "zh")
} # }
```
