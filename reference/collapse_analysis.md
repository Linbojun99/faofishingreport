# Fishery Collapses

This function finds collapsed catch series, with the definitions of
Mullon et al. (2005) or Worm et al. (2006), and gives how many series
collapsed, when, in which areas and species groups and, following Mullon
et al. (2005), the shape of the decline before the collapse.

## Usage

``` r
collapse_analysis(
  area_data,
  method = c("mullon2005", "worm2006"),
  exclude_countries = c("Un. Sov. Soc. Rep.", "Yugoslavia SFR", "Czechoslovakia",
    "South Africa", "Namibia", "China"),
  complete_years = 20,
  min_mean_catch = 500,
  min_total_catch = 10000,
  null_model = TRUE,
  n_sim = 100,
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

- method:

  "mullon2005" (default) or "worm2006".

- exclude_countries:

  Countries left out with method "mullon2005". Default is those left out
  by Mullon et al. (2005).

- complete_years:

  With method "mullon2005", series must have a record in each of the
  last this many years of the data. Default is 20.

- min_mean_catch:

  With method "mullon2005", the minimum mean catch of a series in tonnes
  a year. Default is 500.

- min_total_catch:

  With method "worm2006", the minimum total catch of a fishery in
  tonnes. Default is 10 000.

- null_model:

  A logical value. If TRUE (default), the analysis is repeated on random
  series with no trend (Wilberg and Miller 2007).

- n_sim:

  The number of random data sets of the null model. Default is 100.

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

Invisibly, a list with `series` (one row per series: maximum, collapse
year and type), `summary` (number and share of collapsed series by type,
with the null model), `by_year` (collapses or the share of collapsed
fisheries in each year) and `by_group` (share of collapsed series by FAO
area and species group).

## Details

- "mullon2005" (default):

  A series is the catch of one species or genus by one country in one
  FAO area. Left out, as by Mullon et al. (2005, p. 112): items not
  identified to genus or species; the countries in `exclude_countries`
  (by default those Mullon et al. left out: the former USSR, Yugoslavia
  and Czechoslovakia, whose series end with the country; South Africa
  and Namibia, whose series were split; and China, whose series they
  considered distorted); series with a missing year among the last
  `complete_years` (20) years; and series whose mean catch is below
  `min_mean_catch` (500) tonnes a year. A series has collapsed when, in
  a year after its maximum catch Xmax, catch stays below Xmax/10 for 4
  consecutive years; the first of these years is the collapse year Dc
  (p. 113). The decline before the collapse is *plateau* if catch was
  above Xmax/4 in every year from Dc-8 to Dc-3, *smooth* if the
  correlation between catch and time from Dc-10 to Dc is below -0.7, and
  *erratic* otherwise (p. 114). Mullon et al. do not say in which order
  the rules apply; they are applied here in that order. Collapses less
  than 10 years after the first year of the series are not classified.
  Mullon et al. found 366 collapses among 1,519 series from 1950 to 2000
  (24%): 21% plateau, 33% smooth and 45% erratic.

- "worm2006":

  A fishery is the catch of one species item in one FAO area, all
  countries together (Worm et al. 2006 used species in large marine
  ecosystems from Sea Around Us data). Fisheries with a total catch
  below `min_total_catch` (10 000 tonnes) are left out. A fishery starts
  when its catch first reaches 10% of its maximum, and is collapsed in a
  year when its catch is below 10% of its maximum (recovered when it is
  back above). The function gives, for each year, the share of fisheries
  currently collapsed and the share that have collapsed at least once
  (Worm et al. 2006, main text and supplementary pp. 4-5).

Catch data alone cannot tell a collapse of the stock from a fall in
catch caused by management, markets or reporting. Wilberg and Miller
(2007) showed that random catch series with no trend also give a growing
share of "collapsed" fisheries with the definition of Worm et al.
(2006), because the maximum of a series grows with its length; with
`null_model = TRUE` the function repeats the analysis on such random
series (stationary first-order autoregressive lognormal series with a
coefficient of variation of 80% and an autocorrelation of 0.75, as in
Wilberg and Miller 2007), with the same start years and lengths as the
real series, for comparison.

## References

Mullon, C., Freon, P. and Cury, P. (2005). The dynamics of collapse in
world fisheries. Fish and Fisheries 6: 111-120.
[doi:10.1111/j.1467-2979.2005.00181.x](https://doi.org/10.1111/j.1467-2979.2005.00181.x)

Wilberg, M.J. and Miller, T.J. (2007). Comment on "Impacts of
biodiversity loss on ocean ecosystem services". Science 316: 1285.
[doi:10.1126/science.1137946](https://doi.org/10.1126/science.1137946)

Worm, B., Barbier, E.B., Beaumont, N. et al. (2006). Impacts of
biodiversity loss on ocean ecosystem services. Science 314: 787-790.
[doi:10.1126/science.1132294](https://doi.org/10.1126/science.1132294)

## Examples

``` r
if (FALSE) { # \dontrun{
collapse_analysis(load_fao_data(), lang = "zh")                   # Mullon et al. (2005), world
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
collapse_analysis(FAO34$area_data, method = "worm2006")
} # }
```
