# Catch Shocks

This function finds the years in which catch fell abruptly ("shocks") in
each catch series, with the method of Cottrell et al. (2019) or Gephart
et al. (2017), and counts how often shocks occur by year, by FAO fishing
area, by country or by species group.

## Usage

``` r
catch_shocks(
  area_data,
  by = c("country", "area"),
  method = c("cottrell2019", "gephart2017"),
  min_years = 20,
  top_n = 25,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show"),
  label = NULL
)
```

## Arguments

- area_data:

  FAO catch data: `fishing_area_data()$area_data`, or any subset of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  (e.g. one country, or several fishing areas).

- by:

  What a series is: one or more of "country", "area" and "species".
  Default is `c("country", "area")`: the catch of each country in each
  FAO area.

- method:

  "cottrell2019" (default) or "gephart2017".

- min_years:

  Series with catch in fewer years are left out. Default is 20.

- top_n:

  The number of series with the largest catch shown in the timeline
  chart. Default is 25.

- plot:

  A logical value. If TRUE (default), charts are saved in
  `figures/png/summary/`.

- table:

  A logical value. If TRUE (default), tables are saved in `tables/`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- pptx:

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in `figures/pptx/summary/`. Default is FALSE, which can be
  changed for all functions with `fao_settings(pptx = TRUE)`.

- plot_show:

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

- label:

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

## Value

Invisibly, a list with `shocks` (one row per shock: year, catch,
baseline, loss, Cook's distance and years to recovery), `by_year`
(number of series, shocks and shock frequency in each year), `by_group`
(shock rate by area, country, species group and so on) and `series` (the
series analysed).

## Details

Each series (e.g. the catch of one country in one FAO area) is smoothed,
and the residuals from the smooth are regressed on the residuals of the
year before (a lag-1 regression). Years whose Cook's distance in that
regression exceeds a threshold are candidate shocks; only those in which
catch fell below the level of the preceding years are kept:

- "cottrell2019" (default):

  LOESS with span 0.6, Cook's distance above 0.3; the loss is measured
  against the median catch of the previous 7 years, so the first 7 years
  of a series cannot be shocks (Cottrell et al. 2019, Methods, and their
  code at <https://github.com/cottrellr/shocks>).

- "gephart2017":

  lowess with span 2/3, Cook's distance above 0.35; the loss is measured
  against the mean catch of the previous 5 years (Gephart et al. 2017,
  section 2, and their code at
  <https://github.com/jagephart/Shock_Detection>).

A shock has recovered in the first later year in which catch is back to
at least 95% of that baseline. The yearly shock frequency is the number
of shocks in a year divided by the number of series analysed in that
year (Cottrell et al. 2019): a series counts from its first year with
catch to the last year, including years in which its catch fell to zero;
the shock rate of an area, country or species group is the number of
shocks divided by the number of its series (Gephart et al. 2017).

Both studies analysed national totals (all species and areas of a
country); with `by = "country"` and the data of all areas
([`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md))
the series are the same. Neither study states a minimum series length;
`min_years` is a choice of this package, as is ending the series of
countries that no longer exist (e.g. the USSR) in their last year with
catch, so that the end of a country is not counted as a shock. Highly
variable series (e.g. Peruvian anchoveta) may show no shocks because
large changes are usual in them (Gephart et al. 2017).

## References

Cottrell, R.S., Nash, K.L., Halpern, B.S. et al. (2019). Food production
shocks across land and sea. Nature Sustainability 2: 130-137.
[doi:10.1038/s41893-018-0210-1](https://doi.org/10.1038/s41893-018-0210-1)

Gephart, J.A., Deutsch, L., Pace, M.L., Troell, M. and Seekell, D.A.
(2017). Shocks to fish production: identification, trends, and
consequences. Global Environmental Change 42: 24-32.
[doi:10.1016/j.gloenvcha.2016.11.003](https://doi.org/10.1016/j.gloenvcha.2016.11.003)

## Examples

``` r
if (FALSE) { # \dontrun{
FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
catch_shocks(FAO27$area_data, lang = "zh")                      # countries in area 27
catch_shocks(FAO27$area_data, by = "species")                   # species in area 27
catch_shocks(load_fao_data(), by = "country", method = "gephart2017")   # national totals
} # }
```
