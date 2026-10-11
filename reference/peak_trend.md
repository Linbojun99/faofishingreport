# Peak Catch and Recent Trend

This function finds, for the total catch and for the main species,
countries or fishing areas, the year and size of the peak catch, the
catch in the last year as a percentage of the peak, and the trend over
the last \`recent_years\` years: the average annual growth rate, and a
Mann-Kendall trend test (Kendall's tau between year and catch).

## Usage

``` r
peak_trend(
  area_data,
  by = c("species", "country", "area"),
  top_n = 10,
  recent_years = 10,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL
)
```

## Arguments

- area_data:

  FAO catch data: \`fishing_area_data()\$area_data\`, or any subset of
  \`load_fao_data()\` (e.g. one country, or several fishing areas).

- by:

  What to compare: "species" (default), "country" or "area".

- top_n:

  The number of species, countries or areas with the largest total catch
  to include. Default is 10; use \`Inf\` for all.

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

- label:

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

## Value

Invisibly, a data frame with one row per series (the first row is the
total).

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
peak_trend(FAO34$area_data, by = "species", lang = "zh")
peak_trend(FAO34$area_data, by = "country", lang = "zh")
} # }
```
