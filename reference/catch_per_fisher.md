# Catch per Fisher

This function divides the catch of a country by its number of fishers,
by year: the marine catch (FAO marine areas) by the marine fishers
(coastal, deep-sea and marine not specified), the inland catch by the
inland fishers, and the whole catch by all fishers, from the FAO capture
and employment statistics
([`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
and
[`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md)).
FAO reports production per fisher by region in The State of World
Fisheries and Aquaculture (FAO 2012, table 9) and in its studies of
fisher numbers (FAO 1999); Scherrer et al. (2024) followed marine catch
per fisher worldwide since 1950.

## Usage

``` r
catch_per_fisher(
  country_name,
  weights = c("headcount", "fte"),
  data = NULL,
  employment = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL,
  data_version = "latest"
)
```

## Arguments

- country_name:

  One or more countries, as named in the FAO data (e.g. "Norway").

- weights:

  "headcount" (default) counts fishers as people; "fte" as full-time
  equivalents.

- data:

  Catch data in the format of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is NULL, which loads `load_fao_data(data_version)`.

- employment:

  Employment data from
  [`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md).
  Default is NULL, which loads it.

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

  The name used in the output file names. Default is the country, or
  "selected_countries" for several countries.

- data_version:

  The FAO data release of the catch data; see
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is "latest".

## Value

Invisibly, a list with `yearly` (one row per country and year: marine
and inland catch, marine, inland and all fishers, catch per fisher of
each, the number of fishers as people, fish farmers, workers of
unspecified work area and their share of all workers, the share of
fishers in estimated values or with unknown working time, and whether
the number of fishers is unchanged from the year before) and `notes`
(FAO's notes on the countries). The chart `<label>_catch_per_fisher.png`
is saved in `figures/png/summary/` and the table
`<label>_catch_per_fisher.csv` in `tables/`.

## Counting fishers

FAO counts people, not full-time equivalents: an occasional fisher
counts as one, like a full-time fisher. With `weights = "fte"`, fishers
are counted as full-time equivalents with the weights of Scherrer et al.
(2024; code on Zenodo): part time 60/95, occasional 15/95, and full time
and unknown working time 1. These are the middle of the CWP ranges of
working time (30 to 90 percent, below 30 percent) relative to the middle
of the full-time range (90 to 100 percent). Where working time is
unknown, as for many countries, the two counts are equal.

People whose work area is not specified are not counted as fishers: they
can be fishers or fish farmers (in China, the part-time and occasional
workers of both; FAO country note), so catch per fisher is too high
where many are unspecified. `unspecified_pct` gives their share of all
workers. Fish farmers are not counted.

## Reading the results

- Catch is recorded by the flag of the vessel and fishers as the crews
  of national vessels (CWP handbook), but some countries leave out
  foreign crews (e.g. Norway; FAO country note), and the catch of
  distant-water fleets can come with few national fishers.

- The marine and inland split follows each country's reporting; for
  example, FAO notes that China's marine fishers not specified include
  inland fishers in 1995-2006 and 2014-2020, so use the catch per fisher
  of all fishing for China.

- Estimated values (by FAO or the country) are common
  (`fishers_estimated_pct`), and FAO's are often carried forward
  unchanged (FAO 2024, table T.39): `fishers_unchanged` marks years with
  the same number of fishers as the year before.

- Surveys can find more or fewer fishers than FAO statistics (FAO, Duke
  University and WorldFish 2023, Annex B).

## References

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishers.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/>

FAO (1999). Numbers of fishers 1970-1996. FAO Fisheries Circular 929,
Revision 1. Rome: FAO. <https://www.fao.org/3/x1168t/x1168t.pdf>

FAO (2012). The State of World Fisheries and Aquaculture 2012. Rome:
FAO. <https://www.fao.org/4/i2727e/i2727e.pdf>

FAO (2024). Fishery and Aquaculture Statistics - Yearbook 2021. Rome:
FAO. [doi:10.4060/cc9523en](https://doi.org/10.4060/cc9523en)

FAO, Duke University and WorldFish (2023). Illuminating Hidden Harvests:
the contributions of small-scale fisheries to sustainable development.
Rome: FAO. [doi:10.4060/cc4576en](https://doi.org/10.4060/cc4576en)

Scherrer, K.J.N., Rousseau, Y., Teh, L.C.L., Sumaila, U.R. and
Galbraith, E.D. (2024). Diminishing returns on labour in the global
marine food system. Nature Sustainability 7: 45-52.
[doi:10.1038/s41893-023-01249-8](https://doi.org/10.1038/s41893-023-01249-8)
. Code:
[doi:10.5281/zenodo.7215108](https://doi.org/10.5281/zenodo.7215108)

## Examples

``` r
if (FALSE) { # \dontrun{
employment <- load_fao_employment()
catch_per_fisher(c("Norway", "Iceland", "Peru", "Indonesia"), employment = employment)
catch_per_fisher("India", weights = "fte", employment = employment, lang = "zh")
} # }
```
