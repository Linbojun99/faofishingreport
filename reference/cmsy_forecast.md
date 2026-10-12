# Project CMSY++ Results under Fishing Scenarios

This function projects each stock forward from the results of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
under fishing mortality scenarios (by default 0.5, 0.6, 0.8 and 0.95
Fmsy, as in Froese et al. 2018, and the last-year fishing mortality) or
constant catches, and draws the projected B/Bmsy and catch.

## Usage

``` r
cmsy_forecast(
  result,
  stocks = NULL,
  years = 10,
  f = c(0.5, 0.6, 0.8, 0.95),
  status_quo = TRUE,
  catch = NULL,
  sigma = 0.1,
  seed = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
  pptx = getOption("faofishingreport.pptx", FALSE),
  silent = fao_setting("silent")
)
```

## Arguments

- result:

  The result of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md).

- stocks:

  Optional stocks to project: `Stock` identifiers or species, by English
  or Chinese FAO name, scientific name or FAO 3-alpha code. Default is
  NULL, all stocks that ran.

- years:

  Number of years to project after the latest last year of the stocks.
  Default is 10.

- f:

  Fishing mortality scenarios as multiples of Fmsy. Default is
  `c(0.5, 0.6, 0.8, 0.95)`.

- status_quo:

  A logical value. If TRUE (default), the last-year fishing mortality is
  also projected.

- catch:

  Optional constant catches to project, in tonnes per year (e.g. the
  last-year catch).

- sigma:

  Standard deviation of the lognormal process error. Default is 0.1.

- seed:

  Optional random seed.

- plot, table:

  Whether charts (`figures/png/cmsy/<stock>/forecast.png` and
  `figures/png/cmsy/summary/forecast_share.png`) and the table
  (`tables/cmsy/`) are saved. Default is TRUE.

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
  PowerPoint files in figures/pptx/cmsy/\<stock\>/ and
  figures/pptx/cmsy/summary/. Default is FALSE, which can be changed for
  all functions with `fao_settings(pptx = TRUE)`.

- silent:

  A logical value. If FALSE (default), a progress bar shows how many
  stocks have been projected. If TRUE, nothing is printed. The default
  can be changed for the session with `fao_settings(silent = TRUE)`.

## Value

Invisibly, a list with `projection` (by stock, scenario and year: median
B/Bmsy, F/Fmsy and catch with 95% limits, and the probability that B is
at least Bmsy) and `share` (by scenario and year: share of stocks at or
above Bmsy, with 95% limits, and the number of stocks).

## Details

CMSY++16.R has no projection, so it is done here with the CMSY++ model
itself: the Schaefer surplus-production model in which productivity
declines linearly below B/k = 0.25 (CMSY++16.R and Froese et al. 2023),
started from the posterior draws of r, k and last-year B/k of the run
(which come from the same MCMC iterations), with lognormal process error
of standard deviation `sigma` (0.1 by default, the value `CMSY++16.R`
gives to its unused setting `sigmaR`; the CMSY++ model itself estimates
its process error). As in CMSY++, the biomass of a year is the biomass
before the catch of that year, F is the catch divided by that biomass,
and F/Fmsy uses an Fmsy reduced linearly below half of Bmsy
(CMSY++16.R). The catch of the last year of the data is kept, so the
biomass of the next year is that of the CMSY++ model; the scenarios set
the catch from the first projected year on. In the F scenarios the catch
is the scenario F times the biomass of the same year, so F/Fmsy is the
scenario value.

Stocks whose data end in different years are each projected from their
own last year to the same final year (the latest last year plus
`years`); the share of stocks at or above Bmsy is given from the latest
last year on, when every stock has a value.

## References

Froese, R., Winker, H., Coro, G., Demirel, N., Tsikliras, A.C.,
Dimarchopoulou, D., Scarcella, G., Quaas, M. and Matz-Lueck, N. (2018).
Status and rebuilding of European fisheries. Marine Policy 93: 159-170.
[doi:10.1016/j.marpol.2018.04.018](https://doi.org/10.1016/j.marpol.2018.04.018)

Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C.,
Dimarchopoulou, D., Touloumis, K., Demirel, N., Vianna, G.M.S.,
Scarcella, G., Schijns, R., Liang, C. and Pauly, D. (2023). New
developments in the analysis of catch time series as the basis for fish
stock assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria
53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

## Examples

``` r
if (FALSE) { # \dontrun{
# result <- cmsy_run(...), see cmsy_run()

# Default scenarios: 0.5, 0.6, 0.8 and 0.95 Fmsy, and the last-year fishing mortality
projection <- cmsy_forecast(result, seed = 1)

# Twenty years, two F scenarios and a constant catch of 50,000 t, in Chinese
projection <- cmsy_forecast(result, years = 20, f = c(0.8, 1), catch = 50000, seed = 1, lang = "zh")
last <- projection$projection[projection$projection$Year == max(projection$projection$Year), ]
last[, c("Stock", "scenario_label", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "p_above_bmsy")]

# Share of the stocks at or above Bmsy (when several stocks were run)
projection$share
} # }
```
