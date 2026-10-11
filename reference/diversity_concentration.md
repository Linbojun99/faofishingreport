# Diversity and Concentration of the Catch

This function computes, for each year:

- the number of species reported (items identified to species with
  catch) and the number of countries with catch;

- the Shannon diversity index of the catch over FAO species items, \\H =
  -\sum p_i \ln p_i\\, where \\p_i\\ is the share of item \\i\\;

- the Herfindahl-Hirschman index (HHI) of species items and of
  countries, the sum of squared percentage shares (0 to 10 000; above 2
  500 is usually read as highly concentrated);

- the share of the \`top_n\` largest species items and countries.

## Usage

``` r
diversity_concentration(
  area_data,
  top_n = 5,
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

- top_n:

  The number of largest species and countries for the top share. Default
  is 5.

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

Invisibly, a data frame with one row per year.

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
diversity_concentration(FAO34$area_data, lang = "zh")
} # }
```
