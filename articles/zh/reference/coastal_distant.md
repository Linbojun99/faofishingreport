# coastal_distant(): 沿岸国与远洋捕捞国的渔获量

英文原页：[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md)

## 说明

该函数把每个FAO海洋渔区的渔获量分为两部分：濒临该渔区的国家（沿岸国）和不濒临该渔区的国家（远洋捕捞船队），并展示主要远洋捕捞国的渔获量随时间的变化。

English

This function splits the catch in each FAO marine fishing area between
countries that border the area (coastal countries) and countries that do
not (distant-water fleets), and shows the main distant-water countries
over time.

## 用法

``` r

coastal_distant(
  area_data,
  top_n = 8,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show"),
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

  单独显示的远洋捕捞国数量。默认为8。

  English

  The number of distant-water countries shown separately. Default is 8.

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

- `plot_show`:

  逻辑值。为TRUE时，每张图在保存的同时也画在绘图窗口中（例如RStudio的Plots窗格），便于立即查看。默认为FALSE，可以用`fao_settings(plot_show = TRUE)`为所有函数统一更改。无论是否显示，图表都会保存为文件（`plot = TRUE`）。

  English

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

- `label`:

  输出文件名中使用的名称。默认为数据所属的渔区（或国家）。

  English

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

## 返回值

以不可见方式返回一个列表，包含`yearly`（沿岸国和远洋捕捞国的渔获量）、`distant`（各远洋捕捞国的渔获量）和`table`。

English

Invisibly, a list with `yearly` (catch by coastal and distant-water
countries), `distant` (catch by distant-water country) and `table`.

## 详细说明

若一个国家的陆地（包括海外领土）位于FAO渔区边界20
km以内，即视为该渔区的沿岸国（见`data-raw/build_coastal_countries.R`）。苏联等已不存在的国家采用其继承国的海岸线。以”Other
nei”上报的渔获量显示为未知。内陆渔区不纳入分析。

English

A country is coastal to an area if its land, including overseas
territories, lies within 20 km of the FAO area boundary (see
`data-raw/build_coastal_countries.R`). Former countries such as the USSR
take the coastline of their successor states. Catch by “Other nei” is
shown as unknown. Inland fishing areas are left out.

## 另请参阅

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md),
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## 示例

``` r

FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
coastal_distant(FAO27$area_data, lang = "zh")
```
