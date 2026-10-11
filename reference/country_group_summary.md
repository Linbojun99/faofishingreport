# Catch by Country Group

This function summarises the catch by groups of countries over time:
World Bank income group, continent, or region, as given in the FAO
country code list. It saves stacked area charts of the catch and of each
group's share, and a table by decade.

## Usage

``` r
country_group_summary(
  area_data,
  group = c("income", "continent", "region"),
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

- group:

  "income" (default) for the World Bank income groups, "continent" or
  "region".

- top_n:

  The number of groups shown separately; the others are combined.
  Default is 8.

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

Invisibly, a list with \`yearly\` (catch by group and year) and
\`table\`.

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
country_group_summary(FAO34$area_data, group = "income", lang = "zh")
} # }
```
