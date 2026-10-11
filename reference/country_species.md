# Analyze and Plot the Main Species Caught by a Country in a Fishing Area

This function takes the fishing area data and a country name, analyzes
the catch of that country, and identifies the top species based on
production within the specified rank range.

## Usage

``` r
country_species(
  area_data,
  country_name,
  rank_range = c(1, 10),
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  time_analysis_text_displacement = 10,
  h_value = 0.15,
  ZH = FALSE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE)
)
```

## Arguments

- area_data:

  A data frame containing the fishing area data.

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

## Value

A data frame with the yearly catch of the country by the ranked species
(the others grouped). Charts and tables are saved if `plot` and `table`
are TRUE.

## See also

Other species and country analyses:
[`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md),
[`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md)

## Examples

``` r
if (FALSE) { # \dontrun{
country_species(FAO34[["area_data"]], country_name = "Morocco",c(1, 10), plot=TRUE,table=TRUE) #area_data=Fao34
} # }
```
