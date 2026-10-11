# Synchronous Breakpoints in the Catch of the Main Species

This function runs a breakpoint analysis (changes in the mean catch,
\`strucchange::breakpoints\`) on the catch series of the \`top_n\`
species with the largest catch, and counts how many species change in
each year. Years in which many species change together may point to an
ecosystem-wide regime shift, or to changes in fisheries management or
reporting.

## Usage

``` r
regime_shift(
  area_data,
  top_n = 20,
  h_value = 0.15,
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

  The number of species with the largest total catch to analyse. Default
  is 20.

- h_value:

  The minimum segment size passed to \`strucchange::breakpoints()\`: a
  fraction of the series length (default 0.15) or a number of years.

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

Invisibly, a list with \`breakpoints\` (one row per species) and
\`yearly\` (number of species with a breakpoint in each year).

## Details

The year of a breakpoint is the first year of the new segment. Species
whose series is too short for \`h_value\` are skipped with a message.

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
regime_shift(FAO34$area_data, lang = "zh")
} # }
```
