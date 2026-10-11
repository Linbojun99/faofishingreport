# species_group_composition(): 按物种类群的渔获量组成

英文原页：[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md)

## 说明

该函数按ISSCAAP物种类群（例如”Herrings, sardines,
anchovies”）或按大类（鱼类、甲壳类、软体动物……）汇总渔获量随时间的变化。它保存各类群渔获量及各类群占比的堆叠面积图，以及按年代汇总的表格。

English

This function summarises the catch by ISSCAAP species group
(e.g. “Herrings, sardines, anchovies”) or by major group (fishes,
crustaceans, molluscs, …) over time. It saves stacked area charts of the
catch and of each group’s share, and a table by decade.

## 用法

``` r

species_group_composition(
  area_data,
  group = c("isscaap", "major"),
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

  “isscaap”（默认）表示ISSCAAP物种类群，“major”表示大类。

  English

  “isscaap” (default) for the ISSCAAP species groups, or “major” for the
  major groups.

- `top_n`:

  单独显示的类群数，其余类群合并为一类。默认为8。

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

以不可见方式返回一个列表，包含`yearly`（各类群逐年的渔获量）和`table`。

English

Invisibly, a list with `yearly` (catch by group and year) and `table`.

## 详细说明

ISSCAAP类群来自FAO物种代码表。FAO没有给出这些类群的中文名称，本包使用的中文名称见`inst/extdata/group_names_zh.csv`（非官方译名）。

English

ISSCAAP groups come from the FAO species code list. FAO gives no Chinese
names for them; the Chinese names used are in
`inst/extdata/group_names_zh.csv` (unofficial translations).

## 另请参阅

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## 示例

``` r

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
species_group_composition(FAO34$area_data, lang = "zh")
```
