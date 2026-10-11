# diversity_concentration(): 渔获量的多样性与集中度

英文原页：[`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md)

## 说明

该函数逐年计算：

- 上报的物种数（有渔获量且鉴定到种的条目数）和有渔获量的国家数；
- 基于FAO物种条目计算的渔获量香农多样性指数`H = -\sum p_i \ln p_i`，其中`p_i`为条目`i`的占比；
- 物种条目和国家的赫芬达尔-赫希曼指数（HHI），即各百分比占比的平方和（取值0至10
  000；通常认为高于2 500为高度集中）；
- 最大的`top_n`个物种条目和国家所占的比例。

English

This function computes, for each year:

- the number of species reported (items identified to species with
  catch) and the number of countries with catch;
- the Shannon diversity index of the catch over FAO species items,
  `H = -\sum p_i \ln p_i`, where `p_i` is the share of item `i`;
- the Herfindahl-Hirschman index (HHI) of species items and of
  countries, the sum of squared percentage shares (0 to 10 000; above 2
  500 is usually read as highly concentrated);
- the share of the `top_n` largest species items and countries.

## 用法

``` r

diversity_concentration(
  area_data,
  top_n = 5,
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

  计算前几名所占比例时取的最大物种数和国家数。默认为5。

  English

  The number of largest species and countries for the top share. Default
  is 5.

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

以不可见方式返回一个数据框，每年一行。

English

Invisibly, a data frame with one row per year.

## 另请参阅

Other summary analyses:
[`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md),
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md),
[`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md),
[`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md),
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md),
[`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md),
[`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md),
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)

## 示例

``` r

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
diversity_concentration(FAO34$area_data, lang = "zh")
```
