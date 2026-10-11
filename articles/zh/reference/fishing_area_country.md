# fishing_area_country(): 渔区主要国家分析

英文原页：[`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md)

## 说明

该函数分析渔区数据，并根据各国家按总产量（吨）的排名，为指定排名范围内的国家生成图表。

English

This function analyzes the fishing area data and generates plots for
specified country based on their ranking in terms of total tonnes.

## 用法

``` r

fishing_area_country(
  area_data,
  rank_range = c(1, 10),
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  line_width = 1.5,
  point_size = 1.8,
  point_alpha = 0.7,
  point_shape = 1,
  line_alpha = 0.7,
  axis_text_size = 10,
  axis_title_size = 10,
  strip_text_size = 10,
  axis_text_face = "plain",
  axis_title_face = "bold",
  strip_text_face = "bold",
  smooth_method = "loess",
  smooth_linetype = "dashed",
  line_linewidth = 1.5,
  text_hjust = 0.9,
  text_vjust = -4,
  text_size = 4.5,
  text_color = "black",
  facet_ncol = 2,
  h_value = 0.15,
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

- `rank_range`:

  长度为2的数值向量，指定要分析的国家的排名范围（按历年总产量排序），如`c(1, 10)`为前10名，`c(11, 20)`为其后10名。默认为`c(1, 10)`。

  English

  A numeric vector of length 2 specifying the rank range of country to
  be analyzed.

- `plot`:

  逻辑值，是否生成并保存图表。默认为TRUE。

  English

  A logical value indicating whether to generate and save plots.

- `table`:

  逻辑值，是否生成表格并保存到`tables/`目录（`country_ranked_table.csv`和`country_coefs_data.csv`）。默认为TRUE。

  English

  A logical value indicating whether to generate and save tables

- `timeseries_analysis`:

  逻辑值，决定分面图（`area/area_country_wrap.png`）的内容：为TRUE（默认）时显示线性趋势线和突变点（用水平线段标出相邻突变点之间各时段的平均产量，用虚线标出各时段的起始年份）；为FALSE时改为显示平滑曲线（见`smooth_method`）。带突变点的分面图使用固定的线宽、字号和两列布局，只有`text_hjust`、`text_vjust`、`text_size`和`text_color`对它起作用。无论取值如何，都会计算突变点，并写入返回值和表格。

  English

  A logical value indicating whether to analysis timeseries.

- `line_width`:

  数值，时间序列图（`area/area_country.png`）中折线的宽度。默认为1.5。

  English

  A numeric value specifying the line width. Default is 1.5.

- `point_size`:

  数值，点的大小（时间序列图和不带突变点的分面图）。默认为1.8。

  English

  A numeric value specifying the point size. Default is 1.8.

- `point_alpha`:

  数值，点的透明度（0为完全透明，1为不透明）。默认为0.7。

  English

  A numeric value specifying the point alpha. Default is 0.7.

- `point_shape`:

  数值或字符，点的形状（R的点符号，如1为空心圆，16为实心圆）。默认为1。

  English

  A numeric or character value specifying the shape of the points.
  Default is 1.

- `line_alpha`:

  数值，折线的透明度（时间序列图和不带突变点的分面图）。默认为0.7。

  English

  A numeric value specifying the line alpha. Default is 0.7.

- `axis_text_size`:

  数值，坐标轴刻度文字的大小。默认为10。

  English

  A numeric value specifying the size of the axis text. Default is 10.

- `axis_title_size`:

  数值，坐标轴标题的大小。默认为10。

  English

  A numeric value specifying the size of the axis title. Default is 10.

- `strip_text_size`:

  数值，分面标签文字的大小。默认为10。

  English

  A numeric value specifying the size of the strip text. Default is 10.

- `axis_text_face`:

  坐标轴刻度文字的字形。默认为”plain”（常规）。

  English

  The font face of the axis text. Default is “plain”.

- `axis_title_face`:

  坐标轴标题的字形。默认为”bold”（粗体）。

  English

  The font face of the axis title. Default is “bold”.

- `strip_text_face`:

  分面标签文字的字形。默认为”bold”（粗体）。

  English

  The font face of the strip text. Default is “bold”.

- `smooth_method`:

  字符串，平滑曲线的方法，同[`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)的`method`（如”loess”、“lm”、“gam”），用于不带突变点的分面图（`timeseries_analysis = FALSE`）。默认为”loess”。

  English

  A character string specifying the smoothing method to be used. Default
  is “loess”.

- `smooth_linetype`:

  字符串，平滑曲线的线型（如”dashed”、“solid”、“dotted”）。默认为”dashed”。

  English

  A character string specifying the line type for the smoothing curve.
  Default is “dashed”.

- `line_linewidth`:

  数值，不带突变点的分面图（`timeseries_analysis = FALSE`）中折线的宽度。默认为1.5。

  English

  A numeric value specifying the line width for the line geom. Default
  is 1.5.

- `text_hjust`:

  数值，分面图中占比标签（各国家历年产量占渔区总产量的比例）的水平对齐方式；标签锚定在1957年、产量为0处。默认为0.9。

  English

  A numeric value specifying the horizontal justification of the text
  geom. Default is 0.9.

- `text_vjust`:

  数值，占比标签的垂直对齐方式。默认为-4。

  English

  A numeric value specifying the vertical justification of the text
  geom. Default is -4.

- `text_size`:

  数值，占比标签的字号。默认为4.5。

  English

  A numeric value specifying the size of the text geom. Default is 4.5.

- `text_color`:

  字符串，占比标签的颜色。默认为”black”。

  English

  A character string specifying the color of the text geom. Default is
  “black”.

- `facet_ncol`:

  数值，`facet_wrap`分面的列数，用于不带突变点的分面图（`timeseries_analysis = FALSE`）。默认为2。

  English

  A numeric value specifying the number of columns to be used in
  facet_wrap. Default is 2.

- `h_value`:

  突变点检验（[`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html)的参数`h`）中相邻突变点之间时段的最短长度：小于1时为占序列长度的比例，大于等于1时为年数。默认为0.15，即每个时段至少占序列长度的15%。

  English

  The minimum segment size passed to
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html):
  a fraction of the series length (default 0.15) or a number of years.

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

一个列表，包含`area_country`（渔区内每个国家逐年的产量）、`area_country_ranked`（排名范围内各国家逐年的产量，其余合并为一类）、`area_country_ranked_total`（所选国家的总产量和排名）和`coefs_data`（相邻突变点之间各时段的起始年`start_year`、结束年`end_year`和平均产量`intercept`）。`plot`和`table`为TRUE时保存图表和表格。

English

A list with `area_country` (yearly catch of every country in the area),
`area_country_ranked` (yearly catch of the ranked countries, with the
others grouped), `area_country_ranked_total` (total catch and rank of
the selected countries) and `coefs_data` (start year, end year and mean
catch of each period between breakpoints). Charts and tables are saved
if `plot` and `table` are TRUE.

## 另请参阅

Other fishing area analyses:
[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md),
[`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)

## 示例

``` r

fishing_area_country(FAO34[["area_data"]], c(1, 10), plot=TRUE,table=TRUE,h_value=0.15) #area_data=Fao34
```
