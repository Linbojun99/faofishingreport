# peak_trend(): 峰值渔获量与近期趋势

英文原页：[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md)

## 说明

该函数针对总渔获量以及主要物种、国家或渔区，找出峰值渔获量出现的年份和数值、最后一年渔获量占峰值的百分比，以及最近`recent_years`年的趋势：年均增长率，以及Mann-Kendall趋势检验（年份与渔获量之间的Kendall
τ）。

English

This function finds, for the total catch and for the main species,
countries or fishing areas, the year and size of the peak catch, the
catch in the last year as a percentage of the peak, and the trend over
the last `recent_years` years: the average annual growth rate, and a
Mann-Kendall trend test (Kendall’s tau between year and catch).

## 用法

``` r

peak_trend(
  area_data,
  by = c("species", "country", "area"),
  top_n = 10,
  recent_years = 10,
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

- `by`:

  比较的对象：“species”（物种，默认）、“country”（国家）或”area”（渔区）。

  English

  What to compare: “species” (default), “country” or “area”.

- `top_n`:

  纳入总渔获量最大的物种、国家或渔区的个数。默认为10；用`Inf`表示全部。

  English

  The number of species, countries or areas with the largest total catch
  to include. Default is 10; use `Inf` for all.

- `recent_years`:

  计算趋势所用的最近年数。默认为10。

  English

  The number of most recent years used for the trend. Default is 10.

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

以不可见方式返回一个数据框，每个序列一行（第一行为总渔获量）。

English

Invisibly, a data frame with one row per series (the first row is the
total).

## 另请参阅

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## 示例

``` r

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
peak_trend(FAO34$area_data, by = "species", lang = "zh")
peak_trend(FAO34$area_data, by = "country", lang = "zh")
```
