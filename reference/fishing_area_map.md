# Map of an FAO Major Fishing Area and Its Subareas

This function maps one FAO major marine fishing area with its subareas,
divisions or subdivisions, labelled with their codes (e.g. 27.4, the
North Sea), from the boundaries bundled with the package
(`inst/extdata/FAO_AREAS_CWP.shp`, FAO, CC BY 4.0). When the ggOceanMaps
package (Vihtakari 2026,
<https://github.com/MikkoVihtakari/ggOceanMaps>) is installed, the map
is drawn on its bathymetric base map: polar stereographic for areas
reaching beyond 60 degrees of latitude (e.g. area 27), else in longitude
and latitude, with the depth from the low-resolution ETOPO 2022 relief
model bundled with ggOceanMaps (no download needed). Without
ggOceanMaps, the land of Natural Earth is drawn in a Lambert azimuthal
equal-area projection centred on the area, without depth.

## Usage

``` r
fishing_area_map(
  area,
  level = c("subarea", "division", "subdivision", "none"),
  bathymetry = TRUE,
  labels = TRUE,
  engine = c("auto", "ggOceanMaps", "sf"),
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = fao_setting("pptx"),
  plot_show = fao_setting("plot_show")
)
```

## Arguments

- area:

  The FAO major marine fishing area: its code (e.g. 27) or English name
  (e.g. "Atlantic, Northeast").

- level:

  The units drawn inside the area: "subarea" (default), "division",
  "subdivision" or "none" (the outline of the area only). Areas without
  units at the level asked for are drawn with their outline only.

- bathymetry:

  A logical value. If TRUE (default), the depth is shown (ggOceanMaps
  only).

- labels:

  A logical value. If TRUE (default), the units are labelled with their
  codes.

- engine:

  How the map is drawn: "auto" (default, ggOceanMaps when installed,
  else "sf"), "ggOceanMaps" or "sf".

- plot:

  A logical value. If TRUE (default), the map is saved in
  `figures/png/area/`.

- table:

  A logical value. If TRUE (default), the table of units is saved in
  `tables/`.

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

- plot_show:

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

## Value

Invisibly, a list with `map` (the ggplot object), `units` (one row per
unit: `code`, `level`, `name_en`), `engine`, `fao_map` (link to the
official FAO map of the area) and `sources` (the boundaries, base map
and depth data used, to cite with the map).

## Details

The official FAO map of the area is linked in the result (`fao_map`);
see
[`fao_areas()`](https://linbojun99.github.io/faofishingreport/reference/fao_areas.md).

## References

FAO (2024). FAO Major Fishing Areas for Statistical Purposes. In: FAO
Fisheries and Aquaculture Division \[online\]. Licence: CC-BY 4.0.
<https://www.fao.org/fishery/geonetwork?uuid=ac02a460-da52-11dc-9d70-0017f293bd28>

NOAA National Centers for Environmental Information (2022). ETOPO 2022
15 Arc-Second Global Relief Model.
[doi:10.25921/fd45-gt74](https://doi.org/10.25921/fd45-gt74)

Vihtakari, M. (2026). *ggOceanMaps: Plot Data on Oceanographic Maps
using 'ggplot2'*. R package version 3.0.0.
[doi:10.32614/CRAN.package.ggOceanMaps](https://doi.org/10.32614/CRAN.package.ggOceanMaps),
<https://github.com/MikkoVihtakari/ggOceanMaps>

## See also

[`fao_areas()`](https://linbojun99.github.io/faofishingreport/reference/fao_areas.md)
for all areas.

## Examples

``` r
if (FALSE) { # \dontrun{
# The Northeast Atlantic and its subareas (27.1 Barents Sea, ..., 27.14 East Greenland)
ne_atlantic <- fishing_area_map(27, lang = "zh")
ne_atlantic$units

# Its divisions, without depth
fishing_area_map("Atlantic, Northeast", level = "division", bathymetry = FALSE)
} # }
```
