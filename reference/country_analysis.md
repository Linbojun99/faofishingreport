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

  The minimum segment size passed to
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html):
  a fraction of the series length (default 0.15) or a number of years.

- ZH:

  A logical value. If TRUE, chart text and names are in Chinese. Default
  is FALSE. `ZH = TRUE` is the same as `lang = "zh"`.

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

## Value

A list with `country_species_ranked` (yearly catch of the ranked
species), `country_species_ranked_coefs_data` (periods between
breakpoints of each species), `table` (catch by species and FAO area),
`country_species_area_total` (total catch of each species in each area)
and `country_yearly_area_production` (yearly catch in each area). Charts
and tables are saved if `plot` and `table` are TRUE.

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
