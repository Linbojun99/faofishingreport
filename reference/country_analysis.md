# Analyze and Plot the Global Catch of a Country

This function takes a country name, analyzes the catch of that country
in all FAO fishing areas, and identifies the top species and fishing
areas based on production within the specified rank range.

## Usage

``` r
country_analysis(
  country_name,
  rank_range = c(1, 10),
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  time_analysis_text_displacement = 10,
  h_value = 0.15,
  ZH = FALSE,
  fill_scale = NULL,
  lang = NULL,
  data_version = "latest",
  pptx = getOption("faofishingreport.pptx", FALSE)
)
```

## Arguments

- country_name:

  A string specifying the country to be analyzed.

- rank_range:

  A numeric vector of length 2 specifying the rank range of species to
  be analyzed.

- plot:

  A logical value indicating whether to generate and save plots.

- table:

  A logical value indicating whether to generate and save tables

- timeseries_analysis:

  A logical value indicating whether to analysis timeseries.

- time_analysis_text_displacement:

  The number of years after the first year of data at which the
  percentage labels are placed in the faceted time series plot. Default
  is 10.

- h_value:

  The minimum segment size, which must be greater than the number of
  regressors. For the model `ts_data ~ 1`, this means `h_value` should
  be greater than 1. The default value is 0.15.

- ZH:

  A logical value to Chinese. Default is "FALSE". `ZH = TRUE` is the
  same as `lang = "zh"`.

- fill_scale:

  The colour palette of the map. Default is NULL, which uses
  `map_palette` of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("deep-orange" unless changed); see
  [`?fao_settings`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  for the choices.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  "zh" if `ZH = TRUE`, and otherwise the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- data_version:

  The FAO data release to use: "latest" (default) uses the newest of the
  releases downloaded with
  [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
  and the data bundled with the package; "bundled" uses the data bundled
  with the package; or a release such as "2026.1.0".

- pptx:

  A logical value. Charts are always saved as PNG in `figures/png/`; if
  TRUE they are also saved as editable PowerPoint files in
  `figures/pptx/`. Default is FALSE, which can be changed for all
  functions with `fao_settings(pptx = TRUE)`.

## See also

Other species and country analyses:
[`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md),
[`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md)

## Examples

``` r
if (FALSE) { # \dontrun{
country_analysis(country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE)
} # }
```
