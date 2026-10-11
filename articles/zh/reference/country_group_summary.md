# country_group_summary(): 按国家分组的渔获量

英文原页：[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md)

## 说明

该函数按国家分组汇总渔获量随时间的变化：世界银行收入分组、大洲或地区，分组依据FAO国家代码表。它保存各分组渔获量及各分组占比的堆叠面积图，以及按年代汇总的表格。

English

This function summarises the catch by groups of countries over time:
World Bank income group, continent, or region, as given in the FAO
country code list. It saves stacked area charts of the catch and of each
group’s share, and a table by decade.

## 用法

``` r

country_group_summary(
  area_data,
  group = c("income", "continent", "region"),
  top_n = 8,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL
)
```

## 参数

- `area_data`:

  FAO渔获数据：`fishing_area_data()$area_data`，或[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)的任意子集（例如一个国家，或几个渔区）。

  English

  FAO catch data: `fishing_area_data()$area_data`, or any subset of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  (e.g. one country, or several fishing areas).

- `group`:

  “income”（默认）表示世界银行收入分组，“continent”表示大洲，“region”表示地区。

  English

  “income” (default) for the World Bank income groups, “continent” or
  “region”.

- `top_n`:

  单独显示的分组数，其余分组合并为一类。默认为8。

  English

  The number of groups shown separately; the others are combined.
  Default is 8.

- `plot`:

  逻辑值。为TRUE（默认）时，图表保存在figures/png/summary/。

  English

  A logical value. If TRUE (default), charts are saved in
  `figures/png/summary/`.

- `table`:

  逻辑值。为TRUE（默认）时，表格保存在tables/。

  English

  A logical value. If TRUE (default), tables are saved in `tables/`.

- `lang`:

  图表文字和名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the chart text and names: “en” (English), “zh”
  (Chinese) or “both” (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“en” unless changed).

- `pptx`:

  逻辑值。为TRUE时，图表还另存为可编辑的PowerPoint文件，保存在figures/pptx/summary/。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in `figures/pptx/summary/`. Default is FALSE, which can be
  changed for all functions with `fao_settings(pptx = TRUE)`.

- `label`:

  输出文件名中使用的名称。默认为数据所属的渔区（或国家）。

  English

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

## 返回值

以不可见方式返回一个列表，包含`yearly`（各分组逐年的渔获量）和`table`。

English

Invisibly, a list with `yearly` (catch by group and year) and `table`.

## 另请参阅

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## 示例

``` r

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
country_group_summary(FAO34$area_data, group = "income", lang = "zh")
```
