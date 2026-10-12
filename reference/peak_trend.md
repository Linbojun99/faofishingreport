# Peak Catch and Recent Trend

This function finds, for the total catch and for the main species,
countries or fishing areas, the year and size of the peak catch, the
catch in the last year as a percentage of the peak, and the trend over
the last `recent_years` years: the average annual growth rate, and a
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

  What to compare: "species" (default), "country" or "area".

- top_n:

  The number of species, countries or areas with the largest total catch
  to include. Default is 10; use `Inf` for all.

- recent_years:

  The number of most recent years used for the trend. Default is 10.

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

Invisibly, a data frame with one row per series (the first row is the
total).

## Details

Catch series are autocorrelated, which makes the usual Mann-Kendall test
find too many trends, so the variance of the test is corrected with the
effective sample size of an AR(1) process (Yue and Wang 2004), from the
lag-1 autocorrelation of the detrended series corrected for its
small-sample bias (Kendall 1954; `lag1_autocorrelation`); `p_value_iid`
is the p value of the usual test, for comparison. Over ten years the
autocorrelation is poorly known and the correction is only partial:
strongly autocorrelated series still show trends too often. `trend`
tests each series on its own (`p_value` \< 0.05): with many series, some
trends are found by chance, and `q_value` gives the p values adjusted
for the number of series in the result, the total excepted (Benjamini
and Hochberg 1995). Read trends with `p_value` \< 0.05 as exploratory,
and give as significant the trends with `q_value` \< 0.05, as
[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
does for the main species and countries.

## References

Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery
rate: a practical and powerful approach to multiple testing. Journal of
the Royal Statistical Society: Series B 57: 289-300.
[doi:10.1111/j.2517-6161.1995.tb02031.x](https://doi.org/10.1111/j.2517-6161.1995.tb02031.x)

Kendall, M.G. (1954). Note on bias in the estimation of autocorrelation.
Biometrika 41: 403-404.
[doi:10.1093/biomet/41.3-4.403](https://doi.org/10.1093/biomet/41.3-4.403)

Yue, S. and Wang, C.Y. (2004). The Mann-Kendall test modified by
effective sample size to detect trend in serially correlated
hydrological series. Water Resources Management 18: 201-218.
[doi:10.1023/B:WARM.0000043140.61082.60](https://doi.org/10.1023/B%3AWARM.0000043140.61082.60)

## See also

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
peak_trend(FAO27$area_data, by = "species", lang = "zh")
peak_trend(FAO27$area_data, by = "country", lang = "zh")
} # }
```
