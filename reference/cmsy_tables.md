# Tables of CMSY++ Priors and Results for Reports

This function makes the two tables that CMSY++ studies usually give: the
priors of each stock (years used, r and k ranges, MSY prior, and the B/k
priors of the first, intermediate and last years, with their sources)
and the results (r, k, MSY, Fmsy and Bmsy with their 95% confidence
limits, last-year B/Bmsy and F/Fmsy, Kobe probabilities and status).
Values are rounded and ranges written as "low-high", with the column
names in the chosen language.

## Usage

``` r
cmsy_tables(
  result,
  stocks = NULL,
  table = TRUE,
  lang = NULL,
  silent = fao_setting("silent")
)
```

## Arguments

- result:

  The result of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md).

- stocks:

  Optional stocks to put in the tables: `Stock` identifiers or species,
  by English or Chinese FAO name, scientific name or FAO 3-alpha code.
  Default is NULL, all stocks that ran.

- table:

  A logical value. If TRUE (default), the tables are saved in
  `tables/cmsy/`.

- lang:

  The language of the column names and other names of the tables: "en"
  (English), "zh" (Chinese) or "both". Default is NULL, which uses the
  language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- silent:

  A logical value. If TRUE, the messages on how the stocks were matched
  are not printed. Default is FALSE, or the value set with
  `fao_settings(silent = )`.

## Value

Invisibly, a list with the data frames `priors` and `results`. The
results give the status in the two classes of FAO (2026) for catch-only
results and in the five classes of Palomares et al. (2018).

## References

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome,
FAO. Section "The status of fishery resources", Box 1.3 (assessment
tiers and status classes).
<https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html>

Palomares, M.L.D., Froese, R., Derrick, B., Noel, S.-L., Tsui, G.,
Woroniak, J. and Pauly, D. (2018). A preliminary global assessment of
the status of exploited marine fish and invertebrate populations. A
report prepared by the Sea Around Us for OCEANA. University of British
Columbia and GEOMAR.
<https://www.fishbase.de/rfroese/OceanaFinalReport_V7a.pdf>

## Examples

``` r
if (FALSE) { # \dontrun{
# result <- cmsy_run(...), see cmsy_run()
tables <- cmsy_tables(result, lang = "zh")       # also saved in tables/cmsy/
tables$priors
tables$results

# Column names in both languages, without saving
cmsy_tables(result, lang = "both", table = FALSE)$results
} # }
```
