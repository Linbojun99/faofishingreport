# Employment in Fisheries and Aquaculture by Year

This function summarises the FAO employment statistics
([`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md))
of one or more countries, or of the world: the number of people in
aquaculture, marine fishing, inland fishing and work areas not
specified, by year, and how many are women and how many work full time,
as FAO does in The State of World Fisheries and Aquaculture (FAO 2024,
2026).

## Usage

``` r
employment_summary(
  country_name = NULL,
  employment = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL
)
```

## Arguments

- country_name:

  One or more countries, as named in the FAO data (e.g. "Norway").
  Default is NULL, the world (all countries summed).

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

  The name used in the output file names. Default is the country,
  "selected_countries" for several countries, or "World".

## Value

Invisibly, a list of two data frames:

- yearly:

  one row per country (or the world) and year: people in aquaculture,
  marine coastal, deep-sea and not specified fishing, all marine
  fishing, inland fishing, work area unspecified and in total; women,
  men and sex unknown; full time, part time, occasional and working time
  unknown; the share of women and of each working time among those
  known, the share with sex or working time unknown, and the share in
  estimated values;

- notes:

  FAO's notes on the countries.

Charts are saved in `figures/png/summary/`: `<label>_employment.png`
(people by work area, stacked) and `<label>_employment_composition.png`
(shares of women and full-time workers, and of unknown sex and working
time). The table `<label>_employment.csv` is saved in `tables/`.

## Details

The shares of women and of full-time workers are calculated among the
people whose sex or working time is known, and the share of people with
unknown sex or working time is given next to them, as FAO does: they are
unknown for a third to a half of the people (FAO 2024, 2026), and the
detail available can change from year to year (FAO metadata), so a
change in a share can come from a change in reporting. `estimated_pct`
is the share of the people in estimated values (estimated by FAO or by
the country). The FAO notes on each country are shown and returned
(there are none for the world as a whole); read them before comparing
years or countries.

## References

FAO (2024). The State of World Fisheries and Aquaculture 2024. Blue
Transformation in action. Rome: FAO.
[doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en)

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome:
FAO. [doi:10.4060/cd8357en](https://doi.org/10.4060/cd8357en)

FAO. Global employment in the primary sector of fisheries and
aquaculture (FI_Employment), metadata and country notes of the release.
<https://www.fao.org/fishery/static/Data/>

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishers.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/>

## Examples

``` r
if (FALSE) { # \dontrun{
employment <- load_fao_employment()
employment_summary(employment = employment, lang = "zh")              # the world
employment_summary(c("Norway", "Viet Nam", "Peru"), employment = employment)
} # }
```
