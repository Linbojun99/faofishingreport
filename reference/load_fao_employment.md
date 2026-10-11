# Employment in Fisheries and Aquaculture: FAO Data

This function returns the FAO statistics of the number of people
employed in the primary sector of fisheries and aquaculture ("Global
employment in the primary sector of fisheries and aquaculture",
FI_Employment), by country, work area, working time, sex and year, from
1995, with FAO's notes on individual countries.

## Usage

``` r
load_fao_employment(version = "latest", data_dir = fao_data_dir())
```

## Arguments

- version:

  The release, e.g. "2026.1.0". Default is "latest".

- data_dir:

  The directory where the data is saved. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## Value

A data frame with one row per country, work area, working time, sex and
year: `Country..Name.` (as in
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)),
`Work.area..Code.` (A, C, D, I, M or U), `Work.area`, `Working.time`,
`Sex`, `Year`, `Number` (people) and `Number.Type` ("actual",
"estimate", ...). The attribute `country_notes` is a data frame of FAO's
notes on individual countries (`country_en`, `note`), and `fao_version`
and `source_url` give the release.

## What the data are

FAO collects the data from countries with an annual questionnaire
(jointly with the OECD for 53 countries) and estimates the figures of
countries that do not report (FAO metadata of release 2026.1.0; FAO
first disseminated this dataset in 2026):

- 223 countries, 1995-2024; 65.4 million people in 2024 (SOFIA 2026
  gives 65.3 million).

- Work areas: aquaculture, marine coastal fishing, marine deep-sea
  fishing, marine fishing not specified (NEI), inland waters fishing,
  and "Unspecified" (9 percent of the people in 2024). Processing and
  subsistence are not published, their coverage being too small.

- Working time, as defined by the Coordinating Working Party on Fishery
  Statistics (CWP): full time (at least 90 percent of working time or
  livelihood), part time (30 to 90 percent), occasional (less than 30
  percent), or unspecified.

- The numbers are people, not full-time equivalents: an occasional
  worker counts as one.

- Working time is unspecified for 38 percent and sex for 41 percent of
  the data (FAO metadata); FAO's imputations make up about 30 percent of
  the data and half of the people in 2024. `Number.Type` "estimate"
  marks them and the estimates of countries.

- National definitions differ, and the level of detail can change over
  the years; read the notes of each country (the attribute
  `country_notes`, also shown by
  [`employment_summary()`](https://linbojun99.github.io/faofishingreport/reference/employment_summary.md)
  and
  [`catch_per_fisher()`](https://linbojun99.github.io/faofishingreport/reference/catch_per_fisher.md)).
  For example, FAO notes that China reports its part-time and occasional
  workers of fishing and aquaculture together as "Unspecified", and that
  its "Marine fishing, NEI" includes inland fishing for 1995-2006 and
  2014-2020.

- FAO revises the whole series between releases, so releases should not
  be compared.

When the computer is online, the newest release is used, downloaded and
converted the first time (a file under 1 MB) and saved in `data_dir`;
offline, or with `fao_settings(online = FALSE)`, the newest saved
release is used.

## References

FAO. Global employment in the primary sector of fisheries and
aquaculture (FI_Employment), metadata and country notes of the release.
<https://www.fao.org/fishery/static/Data/>

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishers.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/>

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome:
FAO. [doi:10.4060/cd8357en](https://doi.org/10.4060/cd8357en)

## Examples

``` r
if (FALSE) { # \dontrun{
employment <- load_fao_employment()
subset(employment, Country..Name. == "Norway" & Year == 2024)
notes <- attr(employment, "country_notes")
notes$note[notes$country_en == "China"]
} # }
```
