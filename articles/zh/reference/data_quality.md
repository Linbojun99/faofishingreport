# data_quality(): 捕捞产量统计的数据质量

英文原页：[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md)

## 说明

该函数展示数据质量在两个方面随时间的变化：

- 根据FAO状态标记，官方上报的渔获量占比，以及估算（由上报国家估算或由FAO插补）的渔获量占比；
- 根据FAO分类编码，鉴定到种、属、科或只归到更高类群（例如”Marine fishes
  nei”）的渔获量占比。

它还针对最近五年渔获量最大的几个国家，列出这些年份中其渔获量有多少为估算值、有多少鉴定到种。

English

This function shows two aspects of data quality over time:

- the share of the catch reported officially, and the share estimated
  (by the reporting country or imputed by FAO), from the FAO status
  flags;
- the share of the catch identified to species, genus, family, or only
  to a higher group (e.g. “Marine fishes nei”), from the FAO taxonomic
  codes.

It also lists, for the countries with the largest catch in the last five
years, how much of their catch in those years was estimated or
identified to species.

## 用法

``` r

data_quality(
  area_data,
  top_n = 10,
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

- `top_n`:

  国家表中的国家数。默认为10。

  English

  The number of countries in the country table. Default is 10.

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

以不可见方式返回一个列表，包含`data_status`和`taxon_level`（各类别逐年的渔获量）以及`countries`（国家表）。

English

Invisibly, a list with `data_status` and `taxon_level` (yearly catch by
category) and `countries` (country table).

## 详细说明

最近几年估算占比偏高，通常是因为FAO尚未收到各国的上报数据；这些数值往往会在之后的版本中修订。

English

A large estimated share in the latest years usually means that FAO has
not yet received the national reports; these values are often revised in
later releases.

## 另请参阅

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## 示例

``` r

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
data_quality(FAO34$area_data, lang = "zh")
```
