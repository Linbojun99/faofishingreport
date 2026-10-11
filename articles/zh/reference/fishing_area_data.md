# fishing_area_data(): 提取指定渔区的数据并绘图（可选）

英文原页：[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)

## 说明

该函数提取并返回指定FAO主要渔区的数据。`plot`为TRUE时，还会生成图表并保存到figures目录。比例图显示该渔区产量占全球所有海洋渔区捕捞产量的比例（内陆渔区则为占全球所有内陆水域捕捞产量的比例）。

English

This function extracts and returns data for a specified FAO major
fishing area. If the plot parameter is TRUE, it will also generate and
save plots to the figures directory. The percentage chart shows the
share of the area in the world catch of all marine areas (or, for an
inland area, of all inland waters).

## 用法

``` r

fishing_area_data(
  area_name,
  plot = TRUE,
  timeseries_analysis = TRUE,
  area_color1 = "grey80",
  area_alpha = 0.7,
  area_color2 = "black",
  line_color = "black",
  line_alpha = 0.8,
  point_size = 2,
  hline_color = "red",
  hline_alpha = 0.7,
  hline_linewidth = 1.5,
  line_linewidth = 1.5,
  axis_text_face = "plain",
  axis_title_face = "bold",
  strip_text_face = "bold",
  axis_text_size = 10,
  axis_title_size = 12,
  strip_text_size = 12,
  polygon_color = "grey90",
  fill_scale = NULL,
  axis_title_size_map = 14,
  axis_text_size_map = 12,
  legend_text_size_map = 12,
  ZH = FALSE,
  lang = NULL,
  data_version = "latest",
  pptx = getOption("faofishingreport.pptx", FALSE)
)
```

## 参数

- `area_name`:

  FAO主要渔区的名称，与数据中的名称一致，如”Atlantic, Eastern Central”。

  English

  The name of the FAO major fishing area.

- `plot`:

  逻辑值，是否生成并保存图表。默认为TRUE。为FALSE时只返回数据。

  English

  A logical indicating whether to generate and save plots. Default is
  TRUE.

- `timeseries_analysis`:

  逻辑值。为TRUE（默认）时，总产量图显示平均产量突变点（Bai-Perron检验，[`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html)）之间的各个时段：用水平线段标出各时段的平均产量，用虚线标出各时段的起始年份。无论取值如何，返回值中都包含`coefs_data`。

  English

  A logical value. If TRUE (default), the total catch chart shows the
  periods between breakpoints in the mean catch (Bai-Perron test,
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html)).

- `area_color1`:

  总产量面积图的填充色。默认为”grey80”。

  English

  The fill colour of the total catch chart. Default is “grey80”.

- `area_alpha`:

  总产量面积图填充色的透明度（0为完全透明，1为不透明）。默认为0.7。

  English

  The alpha transparency for the area plot. Default is 0.7.

- `area_color2`:

  总产量面积图的轮廓线颜色。默认为”black”。

  English

  The outline colour of the total catch chart. Default is “black”.

- `line_color`:

  比例图中折线的颜色。默认为”black”。

  English

  The color to use for line plots. Default is “black”.

- `line_alpha`:

  比例图中折线的透明度。默认为0.8。

  English

  The alpha transparency for line plots. Default is 0.8.

- `point_size`:

  比例图中点的大小。默认为2。

  English

  The size of points in the percentage plot. Default is 2.

- `hline_color`:

  比例图中水平线（各年比例的平均值）的颜色。默认为”red”。

  English

  The color of the horizontal line in the percentage plot. Default is
  “red”.

- `hline_alpha`:

  比例图中水平线的透明度。默认为0.7。

  English

  The alpha transparency of the horizontal line in the percentage plot.
  Default is 0.7.

- `hline_linewidth`:

  水平线的宽度。默认为1.5。

  English

  Line width for the horizontal line. Default is 1.5.

- `line_linewidth`:

  比例图中折线的宽度。默认为1.5。

  English

  Line width for the line geom. Default is 1.5.

- `axis_text_face`:

  总产量图坐标轴刻度文字的字形。默认为”plain”（常规）。

  English

  The font face of the axis text of the total catch chart. Default is
  “plain”.

- `axis_title_face`:

  各图表和地图坐标轴标题的字形。默认为”bold”（粗体）。

  English

  The font face of the axis titles of the charts and the map. Default is
  “bold”.

- `strip_text_face`:

  不起作用（本函数的图表没有分面）。默认为”bold”。

  English

  Not used (the charts have no panels). Default is “bold”.

- `axis_text_size`:

  比例图坐标轴刻度文字的大小。默认为10。

  English

  The size of the axis text of the percentage chart. Default is 10.

- `axis_title_size`:

  总产量图和比例图坐标轴标题的大小。默认为12。

  English

  The size of the axis titles of the total catch and percentage charts.
  Default is 12.

- `strip_text_size`:

  不起作用（本函数的图表没有分面）。默认为12。

  English

  Not used (the charts have no panels). Default is 12.

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

- `ZH`:

  逻辑值。为TRUE时，图表文字和名称使用中文。默认为FALSE。`ZH = TRUE`等同于`lang = "zh"`。

  English

  A logical value. If TRUE, chart text and names are in Chinese. Default
  is FALSE. `ZH = TRUE` is the same as `lang = "zh"`.

- `lang`:

  图表文字的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL：`ZH = TRUE`时使用”zh”，否则使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the chart text: “en” (English), “zh” (Chinese) or
  “both” (Chinese and English). Default is NULL, which uses “zh” if
  `ZH = TRUE`, and otherwise the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“en” unless changed).

- `data_version`:

  所用的FAO数据版本：“latest”（默认）为用[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)下载的各版本与包自带数据中最新的一个；“bundled”为包自带的数据；也可以指定具体版本，如”2026.1.0”。

  English

  The FAO data release to use: “latest” (default) uses the newest of the
  releases downloaded with
  [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
  and the data bundled with the package; “bundled” uses the data bundled
  with the package; or a release such as “2026.1.0”.

- `pptx`:

  逻辑值。图表总是以PNG格式保存在figures/png/；为TRUE时还另存为可编辑的PowerPoint文件，保存在figures/pptx/。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. Charts are always saved as PNG in `figures/png/`; if
  TRUE they are also saved as editable PowerPoint files in
  `figures/pptx/`. Default is FALSE, which can be changed for all
  functions with `fao_settings(pptx = TRUE)`.

## 返回值

一个列表，包含`area_data`（该渔区的FAO数据，数据框，可作为其他分析函数的输入）、`coefs_data`（总产量相邻突变点之间的各个时段：起始年`start_year`、结束年`end_year`和该时段的平均产量`intercept`，单位为吨）和`act_world_map`（地图数据，`plot = FALSE`时为NULL）。

English

A list with `area_data` (the FAO data of the area), `coefs_data` (start
year, end year and mean catch of each period between breakpoints) and
`act_world_map` (the map data).

## 另请参阅

Other fishing area analyses:
[`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md),
[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)

## 示例

``` r

# Firstly, data preparing and loading.
FAO34 <- fishing_area_data(area_name="Atlantic, Eastern Central", plot = TRUE, timeseries_analysis=TRUE)
```
