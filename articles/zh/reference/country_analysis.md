# country_analysis(): 分析并绘制某个国家的全球渔获量

英文原页：[`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)

## 说明

该函数接收一个国家名称，分析该国在全部FAO渔区的渔获量，并按产量找出`rank_range`所选名次的主要物种和渔区（`rank_range`只用于物种；渔区图表包括该国作业的全部渔区）。

English

This function takes a country name, analyzes the catch of that country
in all FAO fishing areas, and identifies the top species and fishing
areas by production at the ranks chosen with `rank_range`.

## 用法

``` r

country_analysis(
  country_name,
  rank_range = 1:10,
  plot = TRUE,
  table = TRUE,
  timeseries_analysis = TRUE,
  time_analysis_text_displacement = 10,
  h_value = 0.15,
  ZH = FALSE,
  fill_scale = NULL,
  lang = NULL,
  data_version = "latest",
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show")
)
```

## 参数

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

  逻辑值，决定物种分面图的内容：为TRUE（默认）时显示线性趋势线和突变点（相邻突变点之间各时段的平均产量）；为FALSE时显示平滑（loess）曲线。

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

- `fill_scale`:

  地图的色板。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)的`map_palette`（未更改时为”deep-orange”）；可选的色板见[`?fao_settings`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)。

  English

  The colour palette of the map. Default is NULL, which uses
  `map_palette` of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“deep-orange” unless changed); see
  [`?fao_settings`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  for the choices.

- `lang`:

  图表文字和名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL：`ZH = TRUE`时使用”zh”，否则使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the chart text and names: “en” (English), “zh”
  (Chinese) or “both” (Chinese and English). Default is NULL, which uses
  “zh” if `ZH = TRUE`, and otherwise the language set with
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

- `plot_show`:

  逻辑值。为TRUE时，每张图在保存的同时也画在绘图窗口中（例如RStudio的Plots窗格），便于立即查看。默认为FALSE，可以用`fao_settings(plot_show = TRUE)`为所有函数统一更改。无论是否显示，图表都会保存为文件（`plot = TRUE`）。

  English

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

## 返回值

一个列表，包含`country_species_ranked`（所选名次的各物种逐年的产量）、`country_species_ranked_coefs_data`（各物种相邻突变点之间的时段）、`table`（按物种和FAO渔区统计的产量）、`country_species_area_total`（各物种在各渔区的总产量）和`country_yearly_area_production`（各渔区逐年的产量）。`plot`和`table`为TRUE时保存图表和表格。

English

A list with `country_species_ranked` (yearly catch of the ranked
species), `country_species_ranked_coefs_data` (periods between
breakpoints of each species), `table` (catch by species and FAO area),
`country_species_area_total` (total catch of each species in each area)
and `country_yearly_area_production` (yearly catch in each area). Charts
and tables are saved if `plot` and `table` are TRUE.

## 另请参阅

Other species and country analyses:
[`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md),
[`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md)

## 示例

``` r

country_analysis("Norway", rank_range = 1:10)   # the ten main species of Norway
```
