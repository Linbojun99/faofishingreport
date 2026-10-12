# FAO Major Fishing Areas

This function lists the FAO major fishing areas for statistical
purposes, defined by the Coordinating Working Party on Fishery
Statistics (CWP): the 19 marine areas and the inland waters of each
continent. For each area it gives its code, name, ocean, catch in the
last year of the data and the link to its official FAO map. With
`plot = TRUE`, a world map of the marine areas labelled with their codes
is saved as `figures/png/area/fao_major_areas.png`.

## Usage

``` r
fao_areas(
  data = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = fao_setting("pptx")
)
```

## Arguments

- data:

  FAO catch data as returned by
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md),
  used for the catch of the last year. Default is NULL, the latest
  release
  ([`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)).

- plot:

  A logical value. If TRUE (default), the world map is saved in
  `figures/png/area/`.

- table:

  A logical value. If TRUE (default), the table is saved in `tables/`.

- lang:

  The language of the map text and names: "en" (English), "zh" (Chinese)
  or "both" (Chinese and English). Default is NULL, which uses the
  language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- pptx:

  A logical value. If TRUE, the map is also saved as an editable
  PowerPoint file in `figures/pptx/area/`. Default is the value set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (FALSE unless changed).

## Value

Invisibly, a data frame with one row per area: `area_code`, `name_en`,
`name` (in the chosen language), `type` ("marine" or "inland"), `ocean`,
`catch_last_year` (tonnes), `share_pct` (percentage of the world catch
of the year, marine and inland) and `fao_map` (link to the official FAO
map). The map is in the attribute "plot".

## Details

The official maps are the GIF images of the FAO fishing area maps page
(<https://www.fao.org/fishery/docs/maps/Default.htm>); the package links
to them and does not copy them. The world map is drawn from the area
boundaries bundled with the package (`inst/extdata/FAO_AREAS_CWP.shp`,
FAO, CC BY 4.0), in the projection and central longitude set with
[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).
[`fishing_area_map()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_map.md)
maps one area with its subareas.

## References

CWP (Coordinating Working Party on Fishery Statistics). *CWP Handbook of
Fishery Statistics*: Main water areas (FAO major fishing areas). Rome:
FAO.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/main-water-areas/en/>

FAO. Fishing area maps. In: FAO Fisheries and Aquaculture Division
\[online\]. Rome. <https://www.fao.org/fishery/docs/maps/Default.htm>

FAO (2024). FAO Major Fishing Areas for Statistical Purposes. In: FAO
Fisheries and Aquaculture Division \[online\]. Licence: CC-BY 4.0.
<https://www.fao.org/fishery/geonetwork?uuid=ac02a460-da52-11dc-9d70-0017f293bd28>

## See also

[`fishing_area_map()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_map.md)
for the map of one area and its subareas.

## Examples

``` r
if (FALSE) { # \dontrun{
areas <- fao_areas(lang = "zh")
areas[areas$type == "marine", c("area_code", "name", "catch_last_year")]
} # }
```
