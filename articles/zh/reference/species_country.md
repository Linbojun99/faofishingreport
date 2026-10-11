# species_country(): 按国家产量排名分析指定物种并绘图

英文原页：[`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md)

## 说明

该函数接收一个数据框和一个物种名称，分析该物种的数据，并按产量找出排名在指定范围内的主要国家。

English

This function takes a data frame and a specified species name, analyzes
the data for the specified species, and identifies the top countries
based on production within the specified rank range.

## 用法

``` r

species_country(
  area_data,
  species_name,
  rank_range = c(1, 10),
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  h_value = 0.15,
  polygon_color = "grey90",
  fill_scale = NULL,
  axis_title_face = "bold",
  axis_title_size_map = 14,
  axis_text_size_map = 12,
  legend_text_size_map = 12,
  strip_text_face = "bold",
  axis_text_size = 10,
  axis_title_size = 12,
  strip_text_size = 12,
  ZH = FALSE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE)
)
```

## 参数

- `area_data`:

  包含渔区数据的数据框，例如[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)返回的`area_data`。

  English

  A data frame containing the fishing area data.

- `species_name`:

  字符串，要分析的物种（物种条目的英文名，与数据中一致，如”European
  pilchard(=Sardine)“）。

  English

  A string specifying the species to be analyzed.

- `rank_range`:

  长度为2的数值向量，指定要分析的国家的排名范围（按该物种历年总产量排序）。默认为`c(1, 10)`。

  English

  A numeric vector of length 2 specifying the rank range of countries to
  be analyzed.

- `plot`:

  逻辑值，是否生成并保存图表。默认为TRUE。

  English

  A logical value indicating whether to generate and save plots.

- `table`:

  逻辑值，是否生成表格并保存到`tables/`目录。默认为TRUE。

  English

  A logical value indicating whether to generate and save tables

- `timeseries_analysis`:

  逻辑值，决定分面图的内容和返回值：为TRUE（默认）时，分面图显示线性趋势线和突变点（相邻突变点之间各时段的平均产量），函数返回一个列表，包含所选国家逐年的产量和它们的突变点；为FALSE时，分面图显示平滑（loess）曲线，函数只返回所选国家逐年的产量。

  English

  A logical value indicating whether to analysis timeseries.

- `h_value`:

  突变点检验（[`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html)的参数`h`）中相邻突变点之间时段的最短长度：小于1时为占序列长度的比例，大于等于1时为年数。默认为0.15，即每个时段至少占序列长度的15%。

  English

  The minimum segment size passed to
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html):
  a fraction of the series length (default 0.15) or a number of years.

- `polygon_color`:

  地图中国家多边形边界线的颜色。默认为”grey90”。

  English

  The color of the borders of the polygons in the map plot. Default is
  “grey90”.

- `fill_scale`:

  地图的色板。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)的`map_palette`（未更改时为”deep-orange”）；可选的色板见[`?fao_settings`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)。

  English

  The colour palette of the map. Default is NULL, which uses
  `map_palette` of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“deep-orange” unless changed); see
  [`?fao_settings`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  for the choices.

- `axis_title_face`:

  地图坐标轴标题的字形（其他图表使用固定的字形）。默认为”bold”（粗体）。

  English

  The font face of the axis title. Default is “bold”.

- `axis_title_size_map`:

  地图坐标轴标题的大小。默认为14。

  English

  The size of the axis title text in the map plot. Default is 14.

- `axis_text_size_map`:

  地图坐标轴刻度文字（经纬度）的大小。默认为12。

  English

  The size of the axis text in the map plot. Default is 12.

- `legend_text_size_map`:

  地图图例文字的大小。默认为12。

  English

  The size of the legend text in the map plot. Default is 12.

- `strip_text_face`:

  分面标签文字的字形。默认为”bold”（粗体）。目前不起作用：本函数的排名图、时间序列图、分面图和饼图使用固定的字号和字形。

  English

  The font face of the strip text. Default is “bold”.

- `axis_text_size`:

  坐标轴刻度文字的大小。默认为10。目前不起作用：本函数的排名图、时间序列图、分面图和饼图使用固定的字号和字形。

  English

  The size of the axis text. Default is 10.

- `axis_title_size`:

  坐标轴标题文字的大小。默认为12。目前不起作用：本函数的排名图、时间序列图、分面图和饼图使用固定的字号和字形。

  English

  The size of the axis title text. Default is 12.

- `strip_text_size`:

  分面标签文字的大小。默认为12。目前不起作用：本函数的排名图、时间序列图、分面图和饼图使用固定的字号和字形。

  English

  The size of the strip text. Default is 12.

- `ZH`:

  逻辑值。为TRUE时，图表文字和名称使用中文。默认为FALSE。`ZH = TRUE`等同于`lang = "zh"`。

  English

  A logical value. If TRUE, chart text and names are in Chinese. Default
  is FALSE. `ZH = TRUE` is the same as `lang = "zh"`.

- `lang`:

  图表文字和名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL：`ZH = TRUE`时使用”zh”，否则使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the chart text and names: “en” (English), “zh”
  (Chinese) or “both” (Chinese and English). Default is NULL, which uses
  “zh” if `ZH = TRUE`, and otherwise the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“en” unless changed).

- `pptx`:

  逻辑值。图表总是以PNG格式保存在figures/png/；为TRUE时还另存为可编辑的PowerPoint文件，保存在figures/pptx/。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. Charts are always saved as PNG in `figures/png/`; if
  TRUE they are also saved as editable PowerPoint files in
  `figures/pptx/`. Default is FALSE, which can be changed for all
  functions with `fao_settings(pptx = TRUE)`.

## 返回值

一个数据框，包含排名范围内各国家逐年捕捞该物种的产量（其余国家合并为一类）；`timeseries_analysis = TRUE`时返回一个列表，包含这个数据框和各国家相邻突变点之间的时段。`plot`和`table`为TRUE时保存图表和表格。

English

A data frame with the yearly catch of the species by the ranked
countries (the others grouped); with `timeseries_analysis = TRUE`, a
list of this data frame and the periods between breakpoints of each
country. Charts and tables are saved if `plot` and `table` are TRUE.

## 另请参阅

Other species and country analyses:
[`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md),
[`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)

## 示例

``` r

species_country(FAO34[["area_data"]], species_name = "European pilchard(=Sardine)",c(1, 10), plot=TRUE,table=TRUE,h_value = 0.15) #area_data=Fao34
```
