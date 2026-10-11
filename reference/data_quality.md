# Data Quality of the Catch Statistics

This function shows two aspects of data quality over time:

- the share of the catch reported officially, and the share estimated
  (by the reporting country or imputed by FAO), from the FAO status
  flags;

- the share of the catch identified to species, genus, family, or only
  to a higher group (e.g. "Marine fishes nei"), from the FAO taxonomic
  codes.

It also lists, for the countries with the largest catch in the last five
years, how much of their catch in those years was estimated or
identified to species.

## Usage

``` r
data_quality(
  area_data,
  top_n = 10,
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

  The number of countries in the country table. Default is 10.

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

Invisibly, a list with \`data_status\` and \`taxon_level\` (yearly catch
by category) and \`countries\` (country table).

## Details

A large estimated share in the latest years usually means that FAO has
not yet received the national reports; these values are often revised in
later releases.

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
data_quality(FAO34$area_data, lang = "zh")
} # }
```
