# Overview of All FAO Marine Fishing Areas

This function compares the FAO major marine fishing areas: their catch
over time, peak year, catch in the last year as a percentage of the
peak, and recent trend (see \`peak_trend()\`).

## Usage

``` r
area_overview(
  recent_years = 10,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  data_version = "latest"
)
```

## Arguments

- recent_years:

  The number of most recent years used for the trend. Default is 10.

- plot:

  A logical value. If TRUE (default), charts are saved in
  \`figures/png/summary/\`.

- table:

  A logical value. If TRUE (default), tables are saved in \`tables/\`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with \`fao_settings()\` ("en" unless changed).

- pptx:

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in \`figures/pptx/summary/\`. Default is FALSE, which can be
  changed for all functions with \`fao_settings(pptx = TRUE)\`.

- data_version:

  The FAO data release to use; see \`fishing_area_data()\`.

## Value

Invisibly, a data frame with one row per area (the first row is the
total).

## Examples

``` r
if (FALSE) { # \dontrun{
area_overview(lang = "zh")
} # }
```
