# Synchrony and Stability of the Catch

This function measures how synchronously the catches of the species (or
countries) of the data fluctuate, and how much their asynchrony
stabilises the total catch: the synchrony index of Loreau and de
Mazancourt (2008), the portfolio effect used by Ong et al. (2021) for
FAO catch series, the decomposition of the variability of the total
catch of Thibaut and Connolly (2013), and the pairwise correlations of
the series.

## Usage

``` r
catch_synchrony(
  area_data,
  by = c("species", "country"),
  detrend = c("linear", "none"),
  window = 20,
  top_n = 20,
  min_median = 10,
  min_nonzero = 0.75,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL
)
```

## Arguments

- area_data:

  FAO catch data: `fishing_area_data()$area_data`, or any subset of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  (e.g. one country, or several fishing areas).

- by:

  What a series is: "species" (default) or "country".

- detrend:

  "linear" (default) to remove a linear trend from each series, or
  "none".

- window:

  The length in years of the moving windows. Default is 20.

- top_n:

  The number of series with the largest catch shown in the charts.
  Default is 20.

- min_median, min_nonzero:

  Series need a median catch above `min_median` tonnes a year and catch
  in more than `min_nonzero` of the years. Defaults are 10 and 0.75 (Ong
  et al. 2021).

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

- label:

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

## Value

Invisibly, a list with `summary` (synchrony, CVs and portfolio effect),
`series` (mean, standard deviation and CV of each series, the share of
its standard deviation in the sum of the standard deviations,
`sd_share_pct`, and its contribution to the variance of the total catch,
`variance_contribution_pct`: its covariance with the total divided by
the variance of the total, negative for a series that damps the total),
`windows` (the measures in each moving window) and `correlation`
(pairwise correlations of the series).

## Details

Series are chosen as by Ong et al. (2021): the median catch is above
`min_median` (10) tonnes a year and more than `min_nonzero` (3/4) of the
years have catch; with `by = "species"`, only items identified to
species are used (Ong et al. left out miscellaneous and "nei" items).
Each series is detrended with a linear regression on year (as by Ong et
al.), since trends make series look synchronous (Lepš et al. 2019); use
`detrend = "none"` to keep them. From the standard deviations
\\\sigma_i\\ of the series, the standard deviation \\\sigma_T\\ of the
total catch and its mean \\m_T\\:

- synchrony \\\phi = \sigma_T^2 / (\sum_i \sigma_i)^2\\, from 0
  (fluctuations fully compensate each other) to 1 (perfectly
  synchronous) (Loreau and de Mazancourt 2008, eq. 9);

- the coefficient of variation (CV) of the total catch equals
  \\\sqrt{\phi}\\ times the weighted mean CV of the series, \\\sum_i
  \sigma_i / m_T\\ (Thibaut and Connolly 2013, eqs. 2-4): the lower the
  synchrony, the more stable the total;

- portfolio effect = (mean CV of the series - CV of the total) / CV of
  the total x 100 (Ong et al. 2021, after Carlson and Satterthwaite
  2011): how much more variable the single series are than their total,
  in percent.

The same measures are computed in moving windows of `window` years
(linear trend removed in each window, a choice of this package) to show
how synchrony changed over time. Ong et al. (2021) measured synchrony
with wavelet coherence, which separates time scales; this function uses
the simpler correlation-based measures above.

## References

Carlson, S.M. and Satterthwaite, W.H. (2011). Weakened portfolio effect
in a collapsed salmon population complex. Canadian Journal of Fisheries
and Aquatic Sciences 68: 1579-1589.
[doi:10.1139/f2011-084](https://doi.org/10.1139/f2011-084)

Lepš, J., Götzenberger, L., Valencia, E. and de Bello, F. (2019).
Accounting for long-term directional trends on year-to-year synchrony in
species fluctuations. Ecography 42: 1728-1741.
[doi:10.1111/ecog.04528](https://doi.org/10.1111/ecog.04528)

Loreau, M. and de Mazancourt, C. (2008). Species synchrony and its
drivers: neutral and nonneutral community dynamics in fluctuating
environments. American Naturalist 172: E48-E66.
[doi:10.1086/589746](https://doi.org/10.1086/589746)

Ong, J.J.L., Walter, J.A., Jensen, O.P. and Pinsky, M.L. (2021). Global
hotspots of coherent marine fishery catches. Ecological Applications 31:
e02321. [doi:10.1002/eap.2321](https://doi.org/10.1002/eap.2321)

Thibaut, L.M. and Connolly, S.R. (2013). Understanding
diversity-stability relationships: towards a unified model of portfolio
effects. Ecology Letters 16: 140-150.
[doi:10.1111/ele.12019](https://doi.org/10.1111/ele.12019)

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
catch_synchrony(FAO34$area_data, lang = "zh")
catch_synchrony(FAO34$area_data, by = "country", window = 15)
} # }
```
