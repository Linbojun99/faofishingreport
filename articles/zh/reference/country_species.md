# country_species(): 分析并绘制某个国家在渔区内的主要捕捞物种

英文原页：[`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)

## 说明

该函数接收渔区数据和一个国家名称，分析该国的渔获量，并按产量找出`rank_range`所选名次的主要物种。

English

This function takes the fishing area data and a country name, analyzes
the catch of that country, and identifies the top species by production
at the ranks chosen with `rank_range`.

## 用法

``` r

country_species(
  area_data,
  country_name,
  rank_range = 1:10,
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  time_analysis_text_displacement = 10,
  h_value = 0.15,
  ZH = FALSE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show")
)
```

## 参数

- `area_data`:

  包含渔区数据的数据框，例如[`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)返回的`area_data`。

  English

  A data frame containing the fishing area data.

- `country_name`:

  字符串，要分析的国家（FAO的英文国家名，与数据中一致，如”Morocco”）。

  English

  A string specifying the country to be analyzed.

- `rank_range`:

  要显示的物种名次，按该国的产量排序：列出名次即可，例如`1:10`（默认）为前十名，`c(5, 6)`为第五和第六名，`c(1, 10)`为第一和第十名。超出物种数量的名次不显示。0.9.0及以前的版本中，两个数表示起止名次（`c(1, 10)`为前十名）。

  English

  The ranks of the species to show, ranked by the catch of the country:
  the ranks listed, e.g. `1:10` (default) for the first ten, `c(5, 6)`
  for the fifth and sixth, or `c(1, 10)` for the first and the tenth.
  Ranks beyond the number of species are left out. Until version 0.9.0
  two numbers gave the first and the last rank (`c(1, 10)` was the first
  ten).

- `plot`:

  逻辑值，是否生成并保存图表。默认为TRUE。

  English

  A logical value indicating whether to generate and save plots.

- `table`:

  逻辑值，是否生成表格并保存到`tables/`目录。默认为TRUE。

  English

  A logical value indicating whether to generate and save tables

- `timeseries_analysis`:

  逻辑值，决定分面图的内容：为TRUE（默认）时显示线性趋势线和突变点（相邻突变点之间各时段的平均产量）；为FALSE时显示平滑（loess）曲线。

  English

  A logical value indicating whether to analysis timeseries.

- `time_analysis_text_displacement`:

  分面时间序列图中占比标签的位置：标签放在数据首个年份（所选物种首次有产量的年份）之后第几年处。默认为10。

  English

  The number of years after the first year of data at which the
  percentage labels are placed in the faceted time series plot. Default
  is 10.

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

- `plot_show`:

  逻辑值。为TRUE时，每张图在保存的同时也画在绘图窗口中（例如RStudio的Plots窗格），便于立即查看。默认为FALSE，可以用`fao_settings(plot_show = TRUE)`为所有函数统一更改。无论是否显示，图表都会保存为文件（`plot = TRUE`）。

  English

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

## 返回值

一个数据框，包含该国家逐年捕捞所选名次各物种的产量（其余物种合并为一类）。`plot`和`table`为TRUE时保存图表和表格。

English

A data frame with the yearly catch of the country by the ranked species
(the others grouped). Charts and tables are saved if `plot` and `table`
are TRUE.

## 另请参阅

Other species and country analyses:
[`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md),
[`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md)

## 示例

``` r

FAO27 <- fishing_area_data("Atlantic, Northeast")
country_species(FAO27$area_data, "Norway", rank_range = 1:10)    # the ten main species
country_species(FAO27$area_data, "Norway", rank_range = c(5, 6)) # the fifth and sixth
```
