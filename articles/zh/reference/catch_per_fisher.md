# catch_per_fisher(): 人均渔获

英文原页：[`catch_per_fisher()`](https://linbojun99.github.io/faofishingreport/reference/catch_per_fisher.md)

## 说明

本函数逐年用一个国家的渔获量除以其渔民人数：海洋渔获量（FAO海洋渔区）除以海洋渔民（沿海、深海和未细分的海洋捕捞），内陆渔获量除以内陆渔民，全部渔获量除以全部渔民；数据来自FAO捕捞产量和就业统计（[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)和[`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md)）。FAO在The
State of World Fisheries and
Aquaculture中按区域报告了人均产量（FAO，2012，表9），在其渔民人数研究中也有报告（FAO，1999）；Scherrer等（2024）追踪了全世界1950年以来的海洋人均渔获。

English

This function divides the catch of a country by its number of fishers,
by year: the marine catch (FAO marine areas) by the marine fishers
(coastal, deep-sea and marine not specified), the inland catch by the
inland fishers, and the whole catch by all fishers, from the FAO capture
and employment statistics
([`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
and
[`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md)).
FAO reports production per fisher by region in The State of World
Fisheries and Aquaculture (FAO 2012, table 9) and in its studies of
fisher numbers (FAO 1999); Scherrer et al. (2024) followed marine catch
per fisher worldwide since 1950.

## 用法

``` r

catch_per_fisher(
  country_name,
  weights = c("headcount", "fte"),
  data = NULL,
  employment = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL,
  data_version = "latest"
)
```

## 参数

- `country_name`:

  一个或多个国家，名称与FAO数据中的一致（例如”Norway”）。

  English

  One or more countries, as named in the FAO data (e.g. “Norway”).

- `weights`:

  “headcount”（默认）按人数计算渔民；“fte”按全时当量计算。

  English

  “headcount” (default) counts fishers as people; “fte” as full-time
  equivalents.

- `data`:

  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)格式的渔获数据。默认为NULL，即读取`load_fao_data(data_version)`。

  English

  Catch data in the format of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is NULL, which loads `load_fao_data(data_version)`.

- `employment`:

  [`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md)返回的就业数据。默认为NULL，即自动读取。

  English

  Employment data from
  [`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md).
  Default is NULL, which loads it.

- `plot`:

  逻辑值。为TRUE（默认）时，图表保存在`figures/png/summary/`。

  English

  A logical value. If TRUE (default), charts are saved in
  `figures/png/summary/`.

- `table`:

  逻辑值。为TRUE（默认）时，表格保存在`tables/`。

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

  逻辑值。为TRUE时，图表还另存为可编辑的PowerPoint文件，保存在`figures/pptx/summary/`。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in `figures/pptx/summary/`. Default is FALSE, which can be
  changed for all functions with `fao_settings(pptx = TRUE)`.

- `label`:

  输出文件名中使用的名称。默认为国家名称；有多个国家时为”selected_countries”。

  English

  The name used in the output file names. Default is the country, or
  “selected_countries” for several countries.

- `data_version`:

  渔获数据所用的FAO数据版本；见[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)。默认为”latest”。

  English

  The FAO data release of the catch data; see
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is “latest”.

## 返回值

以不可见方式返回一个列表，包括`yearly`（每个国家每年一行：海洋和内陆渔获量；海洋、内陆和全部渔民人数（按`weights`计）及各自的人均渔获；按人数计的渔民人数、养殖从业者人数、未说明工作领域的从业者人数及其占全部从业者的比例；渔民中估计值所占的比例和工作时间未知者所占的比例；以及渔民人数是否与上一年相同）和`notes`（FAO对这些国家的注释）。图表`<label>_catch_per_fisher.png`保存在`figures/png/summary/`，表格`<label>_catch_per_fisher.csv`保存在`tables/`。

English

Invisibly, a list with `yearly` (one row per country and year: marine
and inland catch, marine, inland and all fishers, catch per fisher of
each, the number of fishers as people, fish farmers, workers of
unspecified work area and their share of all workers, the share of
fishers in estimated values or with unknown working time, and whether
the number of fishers is unchanged from the year before) and `notes`
(FAO’s notes on the countries). The chart `<label>_catch_per_fisher.png`
is saved in `figures/png/summary/` and the table
`<label>_catch_per_fisher.csv` in `tables/`.

## 渔民的计数方法

FAO按人数而不是按全时当量计算：一个临时渔民和一个全职渔民一样算作一人。`weights = "fte"`时，渔民按Scherrer等（2024；代码见Zenodo）的权重折算为全时当量：兼职60/95，临时15/95，全职和工作时间未知的按1。这些权重是CWP工作时间范围（30%至90%、不到30%）的中点，相对于全职范围（90%至100%）中点的比值。工作时间未知时（很多国家都是如此），两种计数结果相同。

工作领域未说明的人员不计为渔民：他们可能是渔民，也可能是养殖从业者（在中国，是两者的兼职和临时从业者；见FAO国家注释），所以未说明工作领域的人员较多时，人均渔获偏高。`unspecified_pct`给出他们占全部从业者的比例。养殖从业者不计入。

English

FAO counts people, not full-time equivalents: an occasional fisher
counts as one, like a full-time fisher. With `weights = "fte"`, fishers
are counted as full-time equivalents with the weights of Scherrer et
al. (2024; code on Zenodo): part time 60/95, occasional 15/95, and full
time and unknown working time 1. These are the middle of the CWP ranges
of working time (30 to 90 percent, below 30 percent) relative to the
middle of the full-time range (90 to 100 percent). Where working time is
unknown, as for many countries, the two counts are equal.

People whose work area is not specified are not counted as fishers: they
can be fishers or fish farmers (in China, the part-time and occasional
workers of both; FAO country note), so catch per fisher is too high
where many are unspecified. `unspecified_pct` gives their share of all
workers. Fish farmers are not counted.

## 如何解读结果

- 渔获量按渔船的船旗统计，渔民按本国渔船的船员统计（CWP手册），但有些国家不计外国船员（例如挪威；见FAO国家注释），而远洋船队的渔获可能只对应很少的本国渔民。
- 海洋和内陆的划分取决于各国的报告方式；例如，FAO注明中国未细分的海洋渔民在1995–2006年和2014–2020年包含内陆渔民，所以中国应使用全部捕捞的人均渔获。
- 估计值（由FAO或该国估计）很常见（`fishers_estimated_pct`），而FAO的估计值常常原样沿用（FAO，2024，表T.39）：`fishers_unchanged`标出渔民人数与上一年相同的年份。
- 调查得到的渔民人数可能多于或少于FAO统计（FAO、Duke
  University和WorldFish，2023，附录B）。

English

- Catch is recorded by the flag of the vessel and fishers as the crews
  of national vessels (CWP handbook), but some countries leave out
  foreign crews (e.g. Norway; FAO country note), and the catch of
  distant-water fleets can come with few national fishers.
- The marine and inland split follows each country’s reporting; for
  example, FAO notes that China’s marine fishers not specified include
  inland fishers in 1995-2006 and 2014-2020, so use the catch per fisher
  of all fishing for China.
- Estimated values (by FAO or the country) are common
  (`fishers_estimated_pct`), and FAO’s are often carried forward
  unchanged (FAO 2024, table T.39): `fishers_unchanged` marks years with
  the same number of fishers as the year before.
- Surveys can find more or fewer fishers than FAO statistics (FAO, Duke
  University and WorldFish 2023, Annex B).

## 参考文献

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishers.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/>

FAO (1999). Numbers of fishers 1970-1996. FAO Fisheries Circular 929,
Revision 1. Rome: FAO. <https://www.fao.org/3/x1168t/x1168t.pdf>

FAO (2012). The State of World Fisheries and Aquaculture 2012. Rome:
FAO. <https://www.fao.org/4/i2727e/i2727e.pdf>

FAO (2024). Fishery and Aquaculture Statistics - Yearbook 2021. Rome:
FAO. [doi:10.4060/cc9523en](https://doi.org/10.4060/cc9523en)

FAO, Duke University and WorldFish (2023). Illuminating Hidden Harvests:
the contributions of small-scale fisheries to sustainable development.
Rome: FAO. [doi:10.4060/cc4576en](https://doi.org/10.4060/cc4576en)

Scherrer, K.J.N., Rousseau, Y., Teh, L.C.L., Sumaila, U.R. and
Galbraith, E.D. (2024). Diminishing returns on labour in the global
marine food system. Nature Sustainability 7: 45-52.
[doi:10.1038/s41893-023-01249-8](https://doi.org/10.1038/s41893-023-01249-8).
Code:
[doi:10.5281/zenodo.7215108](https://doi.org/10.5281/zenodo.7215108)

## 示例

``` r

employment <- load_fao_employment()
catch_per_fisher(c("Norway", "Iceland", "Peru", "Indonesia"), employment = employment)
catch_per_fisher("India", weights = "fte", employment = employment, lang = "zh")
```
