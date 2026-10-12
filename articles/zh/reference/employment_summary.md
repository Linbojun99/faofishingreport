# employment_summary(): 渔业与水产养殖逐年就业情况

英文原页：[`employment_summary()`](https://linbojun99.github.io/faofishingreport/reference/employment_summary.md)

## 说明

本函数汇总一个或多个国家（或全世界）的FAO就业统计（[`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md)）：逐年给出水产养殖、海洋捕捞、内陆捕捞和未说明工作领域的从业人数，以及其中女性有多少、全职有多少，与FAO在The
State of World Fisheries and
Aquaculture中的做法相同（FAO，2024、2026）。

English

This function summarises the FAO employment statistics
([`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md))
of one or more countries, or of the world: the number of people in
aquaculture, marine fishing, inland fishing and work areas not
specified, by year, and how many are women and how many work full time,
as FAO does in The State of World Fisheries and Aquaculture (FAO 2024,
2026).

## 用法

``` r

employment_summary(
  country_name = NULL,
  employment = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show"),
  label = NULL
)
```

## 参数

- `country_name`:

  一个或多个国家，名称与FAO数据中的一致（例如”Norway”）。默认为NULL，即全世界（所有国家相加）。

  English

  One or more countries, as named in the FAO data (e.g. “Norway”).
  Default is NULL, the world (all countries summed).

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

- `plot_show`:

  逻辑值。为TRUE时，每张图在保存的同时也画在绘图窗口中（例如RStudio的Plots窗格），便于立即查看。默认为FALSE，可以用`fao_settings(plot_show = TRUE)`为所有函数统一更改。无论是否显示，图表都会保存为文件（`plot = TRUE`）。

  English

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

- `label`:

  输出文件名中使用的名称。默认为国家名称；有多个国家时为”selected_countries”，汇总全世界时为”World”。

  English

  The name used in the output file names. Default is the country,
  “selected_countries” for several countries, or “World”.

## 返回值

以不可见方式返回一个由两个数据框组成的列表：

- **yearly**：每个国家（或全世界）每年一行：水产养殖、海洋沿海捕捞、深海捕捞和未细分海洋捕捞的人数，海洋捕捞合计、内陆捕捞、未说明工作领域的人数和总人数；女性、男性和性别未知的人数；全职、兼职、临时和工作时间未知的人数；已知者中女性和各类工作时间所占的比例，性别或工作时间未知者所占的比例，以及估计值所占的比例；在报告的第一年与最后一年之间，没有任何记录（未报告）的年份为NA；
- **notes**：FAO对这些国家的注释。

图表保存在`figures/png/summary/`：`<label>_employment.png`（按工作领域分列的人数，堆积图）和`<label>_employment_composition.png`（女性和全职从业者的比例，以及性别未知和工作时间未知的比例）。表格`<label>_employment.csv`保存在`tables/`。

English

Invisibly, a list of two data frames:

- **yearly**: one row per country (or the world) and year: people in
  aquaculture, marine coastal, deep-sea and not specified fishing, all
  marine fishing, inland fishing, work area unspecified and in total;
  women, men and sex unknown; full time, part time, occasional and
  working time unknown; the share of women and of each working time
  among those known, the share with sex or working time unknown, and the
  share in estimated values; NA in the years without any record (not
  reported), between the first and last years reported;
- **notes**: FAO’s notes on the countries.

Charts are saved in `figures/png/summary/`: `<label>_employment.png`
(people by work area, stacked) and `<label>_employment_composition.png`
(shares of women and full-time workers, and of unknown sex and working
time). The table `<label>_employment.csv` is saved in `tables/`.

## 详细说明

与FAO的做法相同，女性和全职从业者的比例是在性别或工作时间已知的人中计算的，同时给出性别或工作时间未知的人所占的比例：三分之一到一半的人性别或工作时间未知（FAO，2024、2026），而且可提供的细分程度可能逐年变化（FAO元数据），所以比例的变化可能来自报告方式的变化。`estimated_pct`是估计值（由FAO或该国估计）所含人数占总人数的比例。本函数会显示并返回FAO对各国的注释（汇总全世界时没有注释）；比较不同年份或国家之前，请先阅读这些注释。

English

The shares of women and of full-time workers are calculated among the
people whose sex or working time is known, and the share of people with
unknown sex or working time is given next to them, as FAO does: they are
unknown for a third to a half of the people (FAO 2024, 2026), and the
detail available can change from year to year (FAO metadata), so a
change in a share can come from a change in reporting. `estimated_pct`
is the share of the people in estimated values (estimated by FAO or by
the country). The FAO notes on each country are shown and returned
(there are none for the world as a whole); read them before comparing
years or countries.

## 参考文献

FAO (2024). The State of World Fisheries and Aquaculture 2024. Blue
Transformation in action. Rome: FAO.
[doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en)

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome:
FAO. [doi:10.4060/cd8357en](https://doi.org/10.4060/cd8357en)

FAO. Global employment in the primary sector of fisheries and
aquaculture (FI_Employment), metadata and country notes of the release.
<https://www.fao.org/fishery/static/Data/>

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishers.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/>

## 示例

``` r

employment <- load_fao_employment()
employment_summary(employment = employment, lang = "zh")              # the world
employment_summary(c("Norway", "Viet Nam", "Peru"), employment = employment)
```
