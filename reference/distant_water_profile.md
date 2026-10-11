# Distant-Water Profile of a Country

This function follows the marine catch of one country over time in the
FAO major fishing areas it borders (adjacent areas) and in the areas it
does not border (distant-water areas), and counts the FAO areas in which
it fishes each year, as a measure of the spatial expansion of its
fisheries. It also lists, for each distant-water area, the first and
last year with catch, the peak and the recent catch.

## Usage

``` r
distant_water_profile(
  country_name,
  top_n = 8,
  min_catch = 0,
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

  The country, as named in the FAO data (e.g. "China", "Spain").

- top_n:

  The number of distant-water areas shown separately; the others are
  combined. Default is 8.

- min_catch:

  An area counts as fished in a year when the country's catch there is
  above this many tonnes. Default is 0 (any reported catch).

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

- data_version:

  The FAO data release to use; see
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).

## Value

Invisibly, a list with `yearly` (catch in adjacent and distant-water
areas, distant-water share and number of areas fished, by year), `areas`
(one row per FAO area fished) and `distant` (yearly catch by
distant-water area).

## Details

Distant-water catch is defined as by FAO: the catch of a country in the
FAO major fishing areas it does not border; fishing outside its own
waters but in an area it borders does not count (Grainger and Garcia
1996, section 2.6; Garibaldi and Limongelli 2003, section 2.1). An area
is adjacent to a country as in
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md):
the country's land, including overseas territories, lies within 20 km of
the FAO area boundary (a rule of this package). FAO records catch by the
flag of the vessel and by FAO major area, not by exclusive economic zone
(EEZ), so the split shows in which FAO areas a country's fleet fished,
not whether the catch came from its own waters, the waters of other
countries or the high seas. Swartz et al. (2010) mapped the spatial
expansion of world fisheries with Sea Around Us catch maps; this
function uses FAO statistics only, at the resolution of FAO major areas.

For China, FAO notes that in 2022 China reported 2.3 million tonnes of
distant-water catch but gave species and areas only for the catch landed
in China (area 61); FAO filled other areas with data from regional
fisheries management organizations and entered 1.3 million tonnes as
"Marine fishes nei" in area 61 (FAO 2024). Recent distant-water catch of
China is therefore partly counted in its adjacent area 61.

## References

FAO (2024). The State of World Fisheries and Aquaculture 2024. Blue
Transformation in action. Rome, FAO.
[doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en)

Garibaldi, L. and Limongelli, L. (2003). Trends in oceanic captures and
clustering of large marine ecosystems: two studies based on the FAO
capture database. FAO Fisheries Technical Paper 435. Rome, FAO.
<https://www.fao.org/4/y4449e/y4449e00.htm>

Grainger, R.J.R. and Garcia, S.M. (1996). Chronicles of marine fishery
landings (1950-1994): trend analysis and fisheries potential. FAO
Fisheries Technical Paper 359. Rome, FAO.
<https://www.fao.org/4/w3244e/w3244e00.htm>

Swartz, W., Sala, E., Tracey, S., Watson, R. and Pauly, D. (2010). The
spatial expansion and ecological footprint of fisheries (1950 to
present). PLoS ONE 5: e15143.
[doi:10.1371/journal.pone.0015143](https://doi.org/10.1371/journal.pone.0015143)

## Examples

``` r
if (FALSE) { # \dontrun{
distant_water_profile("Spain", lang = "zh")
distant_water_profile("Korea, Republic of", min_catch = 100)
} # }
```
