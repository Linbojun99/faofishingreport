# Catch by Coastal Countries and Distant-Water Fleets

This function splits the catch in each FAO marine fishing area between
countries that border the area (coastal countries) and countries that do
not (distant-water fleets), and shows the main distant-water countries
over time.

## Usage

``` r
coastal_distant(
  area_data,
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

  FAO catch data: \`fishing_area_data()\$area_data\`, or any subset of
  \`load_fao_data()\` (e.g. one country, or several fishing areas).

- top_n:

  The number of distant-water countries shown separately. Default is 8.

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

Invisibly, a list with \`yearly\` (catch by coastal and distant-water
countries), \`distant\` (catch by distant-water country) and \`table\`.

## Details

A country is coastal to an area if its land, including overseas
territories, lies within 20 km of the FAO area boundary (see
\`data-raw/build_coastal_countries.R\`). Former countries such as the
USSR take the coastline of their successor states. Catch by "Other nei"
is shown as unknown. Inland fishing areas are left out.

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
coastal_distant(FAO34$area_data, lang = "zh")
} # }
```
