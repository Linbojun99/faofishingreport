# Compare CMSY++ Runs, or CMSY++ with Other Assessments

This function draws B/Bmsy and F/Fmsy of the same stocks from several
runs of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
(e.g. with different start years or priors, a sensitivity analysis) and,
optionally, from other assessments, and gives a table of their last-year
values.

## Usage

``` r
cmsy_compare(
  ...,
  stocks = NULL,
  external = NULL,
  lang = NULL,
  annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
  pptx = getOption("faofishingreport.pptx", FALSE),
  silent = fao_setting("silent")
)
```

## Arguments

- ...:

  Results of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
  named or not (e.g. `default = run1, start_1980 = run2`).

- stocks:

  Optional stocks to compare: `Stock` identifiers or species, by English
  or Chinese FAO name, scientific name or FAO 3-alpha code. Default is
  NULL, all stocks that ran.

- external:

  Optional data frame of other estimates, with columns `Stock`, `Year`,
  `B_Bmsy` (and optionally `F_Fmsy`) and `source` (the name shown in the
  legend, e.g. the agency or model of the other assessment).

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- annotations:

  The text added to the axis titles and labels: FALSE (default) none,
  TRUE all, or any of "title", "subtitle", "caption", "legend" and
  "labels" (text inside the chart). The default can be changed for the
  session with `options(faofishingreport.cmsy_annotations = TRUE)`.

- pptx:

  A logical value. If TRUE, the charts are also saved as editable
  PowerPoint files in figures/pptx/cmsy/compare/. Default is FALSE,
  which can be changed for all functions with
  `fao_settings(pptx = TRUE)`.

- silent:

  A logical value. If TRUE, the messages on how the stocks were matched
  are not printed. Default is FALSE, or the value set with
  `fao_settings(silent = )`.

## Value

Invisibly, a list with `last_year` (one row per stock and run) and
`plots`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sensitivity to the start year
run_1950 <- cmsy_run(input, stocks = "GRB_34", seed = 1)
run_auto <- cmsy_run(input, stocks = "GRB_34", seed = 1, auto_start = TRUE)
cmsy_compare(`from 1950` = run_1950, `CMSY++ start year` = run_auto, annotations = "legend")

# Sensitivity to a CMSY++ setting (see cmsy_defaults())
run_sigma <- cmsy_run(input, stocks = "GRB_34", seed = 1, auto_start = TRUE, settings = list(sigmaR = 0.2))
cmsy_compare(default = run_auto, `sigmaR = 0.2` = run_sigma)$last_year

# With another assessment of the same stock, read from a file with the columns
# Stock, Year, B_Bmsy (optionally F_Fmsy) and source
other <- read.csv("other_assessment.csv")
cmsy_compare(CMSY = run_auto, external = other, annotations = "legend")
} # }
```
