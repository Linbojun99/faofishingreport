# fishing_area_map(): FAO主要渔区及其分区地图

英文原页：[`fishing_area_map()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_map.md)

## 说明

该函数绘制一个FAO主要海洋渔区及其亚区（subarea）、区（division）或分区（subdivision），并标出其代码（例如27.4为北海），边界来自本包自带的渔区图层（inst/extdata/FAO_AREAS_CWP.shp，FAO，CC
BY 4.0）。安装了ggOceanMaps包（Vihtakari
2026，<https://github.com/MikkoVihtakari/ggOceanMaps>）时，地图画在它的海底地形底图上：延伸到纬度60度以外的渔区（例如27区）采用极地立体投影，其他渔区采用经纬度坐标；水深取自ggOceanMaps自带的低分辨率ETOPO
2022地形模型（无需下载）。未安装ggOceanMaps时，以渔区为中心的兰伯特方位等积投影绘制Natural
Earth的陆地，不显示水深。

English

This function maps one FAO major marine fishing area with its subareas,
divisions or subdivisions, labelled with their codes (e.g. 27.4, the
North Sea), from the boundaries bundled with the package
(`inst/extdata/FAO_AREAS_CWP.shp`, FAO, CC BY 4.0). When the ggOceanMaps
package (Vihtakari 2026,
<https://github.com/MikkoVihtakari/ggOceanMaps>) is installed, the map
is drawn on its bathymetric base map: polar stereographic for areas
reaching beyond 60 degrees of latitude (e.g. area 27), else in longitude
and latitude, with the depth from the low-resolution ETOPO 2022 relief
model bundled with ggOceanMaps (no download needed). Without
ggOceanMaps, the land of Natural Earth is drawn in a Lambert azimuthal
equal-area projection centred on the area, without depth.

## 用法

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

## 参数

- `area`:

  FAO主要海洋渔区：代码（例如27）或英文名称（例如”Atlantic,
  Northeast”）。

  English

  The FAO major marine fishing area: its code (e.g. 27) or English name
  (e.g. “Atlantic, Northeast”).

- `level`:

  渔区内绘制的单元：“subarea”（亚区，默认）、“division”（区）、“subdivision”（分区）或”none”（只画渔区边界）。没有该级单元的渔区只画其边界。

  English

  The units drawn inside the area: “subarea” (default), “division”,
  “subdivision” or “none” (the outline of the area only). Areas without
  units at the level asked for are drawn with their outline only.

- `bathymetry`:

  逻辑值。为TRUE（默认）时显示水深（仅限ggOceanMaps）。

  English

  A logical value. If TRUE (default), the depth is shown (ggOceanMaps
  only).

- `labels`:

  逻辑值。为TRUE（默认）时，在各单元上标出其代码。

  English

  A logical value. If TRUE (default), the units are labelled with their
  codes.

- `engine`:

  绘图方式：“auto”（默认，已安装ggOceanMaps时用它，否则用”sf”）、“ggOceanMaps”或”sf”。

  English

  How the map is drawn: “auto” (default, ggOceanMaps when installed,
  else “sf”), “ggOceanMaps” or “sf”.

- `plot`:

  逻辑值。为TRUE（默认）时，地图保存在figures/png/area/。

  English

  A logical value. If TRUE (default), the map is saved in
  `figures/png/area/`.

- `table`:

  逻辑值。为TRUE（默认）时，单元表保存在tables/。

  English

  A logical value. If TRUE (default), the table of units is saved in
  `tables/`.

- `lang`:

  图表文字和名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the map text and names: “en” (English), “zh” (Chinese)
  or “both” (Chinese and English). Default is NULL, which uses the
  language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“en” unless changed).

- `pptx`:

  逻辑值。为TRUE时，地图还另存为可编辑的PowerPoint文件，保存在figures/pptx/area/。默认为[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)中的设置（未更改时为FALSE）。

  English

  A logical value. If TRUE, the map is also saved as an editable
  PowerPoint file in `figures/pptx/area/`. Default is the value set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (FALSE unless changed).

- `plot_show`:

  逻辑值。为TRUE时，每张图在保存的同时也画在绘图窗口中（例如RStudio的Plots窗格），便于立即查看。默认为FALSE，可以用`fao_settings(plot_show = TRUE)`为所有函数统一更改。无论是否显示，图表都会保存为文件（`plot = TRUE`）。

  English

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

## 返回值

以不可见方式返回一个列表，包含`map`（ggplot对象）、`units`（每个单元一行：`code`、`level`、`name_en`）、`engine`（绘图方式）、`fao_map`（该渔区FAO官方地图的链接）和`sources`（所用的边界、底图和水深数据，引用地图时一并引用）。

English

Invisibly, a list with `map` (the ggplot object), `units` (one row per
unit: `code`, `level`, `name_en`), `engine`, `fao_map` (link to the
official FAO map of the area) and `sources` (the boundaries, base map
and depth data used, to cite with the map).

## 详细说明

该渔区的FAO官方地图链接在返回结果的`fao_map`中；见[`fao_areas()`](https://linbojun99.github.io/faofishingreport/reference/fao_areas.md)。

English

The official FAO map of the area is linked in the result (`fao_map`);
see
[`fao_areas()`](https://linbojun99.github.io/faofishingreport/reference/fao_areas.md).

## 参考文献

FAO (2024). FAO Major Fishing Areas for Statistical Purposes. In: FAO
Fisheries and Aquaculture Division \[online\]. Licence: CC-BY 4.0.
<https://www.fao.org/fishery/geonetwork?uuid=ac02a460-da52-11dc-9d70-0017f293bd28>

NOAA National Centers for Environmental Information (2022). ETOPO 2022
15 Arc-Second Global Relief Model.
[doi:10.25921/fd45-gt74](https://doi.org/10.25921/fd45-gt74)

Vihtakari, M. (2026). *ggOceanMaps: Plot Data on Oceanographic Maps
using ‘ggplot2’*. R package version 3.0.0.
[doi:10.32614/CRAN.package.ggOceanMaps](https://doi.org/10.32614/CRAN.package.ggOceanMaps),
<https://github.com/MikkoVihtakari/ggOceanMaps>

## 另请参阅

[`fao_areas()`](https://linbojun99.github.io/faofishingreport/reference/fao_areas.md)
for all areas.

## 示例

``` r

# The Northeast Atlantic and its subareas (27.1 Barents Sea, ..., 27.14 East Greenland)
ne_atlantic <- fishing_area_map(27, lang = "zh")
ne_atlantic$units

# Its divisions, without depth
fishing_area_map("Atlantic, Northeast", level = "division", bathymetry = FALSE)
```
