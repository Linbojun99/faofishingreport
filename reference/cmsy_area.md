# Run CMSY++ for the Main Species of a Fishing Area

This function does in one call what
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md),
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
[`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md),
[`cmsy_forecast()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_forecast.md)
and
[`cmsy_tables()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_tables.md)
do step by step, for the species with the largest catch in an FAO
fishing area (or for chosen species): it prepares the catch-only input,
runs CMSY++ on every stock, draws the charts, projects the stocks under
fishing scenarios and writes the tables. Progress bars follow each step.

## Usage

``` r
cmsy_area(
  area,
  top = 10,
  species = NULL,
  recent_years = 10,
  out_dir = "cmsy_areas",
  auto_start = TRUE,
  seed = 1,
  parallel = NULL,
  settings = list(),
  charts = c(cmsy_stock_charts, cmsy_summary_charts),
  classes = c("fao", "palomares"),
  forecast = TRUE,
  annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show"),
  silent = fao_setting("silent"),
  ...
)
```

## Arguments

- area:

  FAO major fishing area name(s), as in
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)
  (e.g. "Atlantic, Northeast"), or FAO catch data such as
  `fishing_area_data()$area_data`.

- top:

  Number of species with the largest catch to assess. Default is 10.

- species:

  Optional species to assess instead of the `top` ones, by English or
  Chinese name, scientific name or FAO 3-alpha code.

- recent_years:

  Catch is ranked by its mean over the last `recent_years` years of the
  data. Default is 10.

- out_dir:

  The folder in which a folder per area is made. Default is
  "cmsy_areas".

- auto_start:

  As in
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
  but TRUE by default here: a stock whose start biomass is ambiguous
  starts in the first year with at least 40% of the maximum catch, as
  suggested by CMSY++.

- seed:

  The random seed of the runs, so that the results can be repeated.
  Default is 1.

- parallel:

  The number of stocks run at the same time. Each uses two more
  processes for its two MCMC chains. Default is half the number of
  processor cores.

- settings:

  Optional named list of CMSY++ settings that replace its defaults, e.g.
  `list(CV.C = 0.2, n.iter = 100000, n.burnin = 50000)`. See
  [`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)
  for the settings, their defaults and what they do. Default is no
  change: CMSY++ runs with its own defaults. The settings used are saved
  in `settings_used.csv` of the run folder and in `result$settings`.

- charts:

  The charts to draw (see
  [`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md)).
  Default is all of them.

- classes:

  The status classes: "fao" (default, two classes) or "palomares" (five
  classes).

- forecast:

  A logical value. If TRUE (default), the stocks are also projected
  under the default fishing scenarios of
  [`cmsy_forecast()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_forecast.md).

- annotations:

  The text added to the axis titles and labels: FALSE (default) none,
  TRUE all, or any of "title", "subtitle", "caption", "legend" and
  "labels" (text inside the chart). The default can be changed for the
  session with `options(faofishingreport.cmsy_annotations = TRUE)`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- pptx:

  A logical value. If TRUE, the charts are also saved as editable
  PowerPoint files in `<out_dir>/<area>/figures_pptx/`. Default is
  FALSE, which can be changed for all functions with
  `fao_settings(pptx = TRUE)`.

- plot_show:

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

- silent:

  A logical value. If FALSE (default), a progress bar shows how many
  stocks have finished, with the elapsed and the estimated remaining
  time, and messages report the run. If TRUE, nothing is printed. The
  default can be changed for the session with
  `fao_settings(silent = TRUE)`.

- ...:

  Further arguments passed to
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)
  (e.g. `priors`, `default_resilience`).

## Value

Invisibly, the result of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
with the elements `input`, `forecast`, `tables` and `dir` (the folder of
the area); a list of these, named by area, for several areas.

## Details

The results of each area are saved in their own folder,
`out_dir/<area>/`:

- input/:

  the CMSY++ catch and ID files, the stocks used, and the main species
  left out with the reason (e.g. no r prior: see `priors` and
  `default_resilience` of
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md));

- runs/:

  the CMSY++ run of each stock (its folder has the original CMSY++
  charts, output and log), the changed CMSY++ code and the list of
  changes;

- figures/:

  one folder of charts per stock and a `summary/` folder for all stocks
  (PNG, and PowerPoint in `figures_pptx/` when `pptx` is TRUE);

- tables/:

  results, yearly series, the priors and results tables for reports,
  projections, the CMSY++ settings used (`settings_used.csv`) and the
  sources of everything used (`sources.csv`).

As in
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md),
FAO data have catches only (no abundance index), so all stocks are
assessed with the catch-only part of CMSY++ and the results are
indicative.

## References

Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C.,
Dimarchopoulou, D., Touloumis, K., Demirel, N., Vianna, G.M.S.,
Scarcella, G., Schijns, R., Liang, C. and Pauly, D. (2023). New
developments in the analysis of catch time series as the basis for fish
stock assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria
53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17 pp.
<https://oceanrep.geomar.de/id/eprint/52147/>

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome,
FAO. Section "The status of fishery resources", Box 1.3 (assessment
tiers and status classes).
<https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html>

## Examples

``` r
if (FALSE) { # \dontrun{
cmsy_setup()                     # once: JAGS and the R packages CMSY++ needs

# The ten species with the largest catch of the last ten years in FAO area 27, in Chinese
area27 <- cmsy_area("Atlantic, Northeast", top = 10, lang = "zh")
area27$summary[, c("Stock", "species_name", "B_Bmsy", "F_Fmsy", "status")]
area27$dir                       # folder with input/, runs/, figures/ and tables/

# Chosen species (scientific name, FAO code, ...), all annotations, five status classes
cmsy_area("Atlantic, Northeast", species = c("Mallotus villosus", "PRA"),
          annotations = TRUE, classes = "palomares")

# Species without a FishBase prior get a resilience (passed on to cmsy_input())
cmsy_area("Atlantic, Northeast", top = 20, default_resilience = "Medium")

# Several areas, quietly, with longer MCMC chains (see cmsy_defaults())
cmsy_area(c("Atlantic, Northeast", "Atlantic, Northwest"), top = 5, silent = TRUE,
          settings = list(n.iter = 100000, n.burnin = 50000))
} # }
```
