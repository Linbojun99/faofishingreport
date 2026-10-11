# distant_water_profile(): 一个国家的远洋捕捞概况

英文原页：[`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)

## 说明

本函数追踪一个国家的海洋渔获量随时间的变化，分为其毗邻的FAO主要渔区（邻近渔区）和不毗邻的渔区（远洋渔区），并统计其每年在多少个FAO渔区捕捞，作为其渔业空间扩张的衡量指标。它还为每个远洋渔区列出有渔获量的第一年和最后一年、峰值和近期渔获量。

English

This function follows the marine catch of one country over time in the
FAO major fishing areas it borders (adjacent areas) and in the areas it
does not border (distant-water areas), and counts the FAO areas in which
it fishes each year, as a measure of the spatial expansion of its
fisheries. It also lists, for each distant-water area, the first and
last year with catch, the peak and the recent catch.

## 用法

``` r

distant_water_profile(
  country_name,
  top_n = 8,
  min_catch = 0,
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

  国家，名称与FAO数据中的一致（例如”China”、“Spain”）。

  English

  The country, as named in the FAO data (e.g. “China”, “Spain”).

- `top_n`:

  单独显示的远洋渔区个数，其余渔区合并显示。默认为8。

  English

  The number of distant-water areas shown separately; the others are
  combined. Default is 8.

- `min_catch`:

  一个国家某年在某渔区的渔获量超过这一吨数时，该渔区才算当年有捕捞。默认为0（任何报告的渔获量）。

  English

  An area counts as fished in a year when the country’s catch there is
  above this many tonnes. Default is 0 (any reported catch).

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

  输出文件名中使用的名称。默认为该国家的名称（`country_name`）。

  English

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

- `data_version`:

  使用的FAO数据版本；见[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)。

  English

  The FAO data release to use; see
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).

## 返回值

以不可见方式返回一个列表，包括`yearly`（逐年的邻近渔区和远洋渔区渔获量、远洋渔获量占比和捕捞的渔区数）、`areas`（每个有捕捞的FAO渔区一行）和`distant`（各远洋渔区的逐年渔获量）。

English

Invisibly, a list with `yearly` (catch in adjacent and distant-water
areas, distant-water share and number of areas fished, by year), `areas`
(one row per FAO area fished) and `distant` (yearly catch by
distant-water area).

## 详细说明

远洋渔获量采用FAO的定义：一个国家在其不毗邻的FAO主要渔区中的渔获量；在本国水域以外、但在其邻近渔区中的捕捞不算远洋捕捞（Grainger和Garcia，1996，第2.6节；Garibaldi和Limongelli，2003，第2.1节）。渔区是否与一个国家毗邻的判定与[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md)相同：该国陆地（包括海外领地）位于该FAO渔区边界20公里以内（本包的规则）。FAO按船旗国和FAO主要渔区记录渔获量，而不是按专属经济区（EEZ）记录，因此这种划分显示的是一个国家的船队在哪些FAO渔区捕捞，而不是渔获来自本国水域、他国水域还是公海。Swartz等（2010）用Sea
Around
Us的渔获量地图描绘了全球渔业的空间扩张；本函数只使用FAO统计数据，分辨率为FAO主要渔区。

关于中国，FAO指出，2022年中国报告了230万吨远洋渔获量，但只为在中国上岸的渔获量（61区）给出了物种和渔区；FAO用区域渔业管理组织的数据填补了其他渔区，并把130万吨作为”Marine
fishes
nei”（未另列明的海洋鱼类）计入61区（FAO，2024）。因此，中国近年的远洋渔获量有一部分被计入其毗邻的61区。

English

Distant-water catch is defined as by FAO: the catch of a country in the
FAO major fishing areas it does not border; fishing outside its own
waters but in an area it borders does not count (Grainger and Garcia
1996, section 2.6; Garibaldi and Limongelli 2003, section 2.1). An area
is adjacent to a country as in
[`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md):
the country’s land, including overseas territories, lies within 20 km of
the FAO area boundary (a rule of this package). FAO records catch by the
flag of the vessel and by FAO major area, not by exclusive economic zone
(EEZ), so the split shows in which FAO areas a country’s fleet fished,
not whether the catch came from its own waters, the waters of other
countries or the high seas. Swartz et al. (2010) mapped the spatial
expansion of world fisheries with Sea Around Us catch maps; this
function uses FAO statistics only, at the resolution of FAO major areas.

For China, FAO notes that in 2022 China reported 2.3 million tonnes of
distant-water catch but gave species and areas only for the catch landed
in China (area 61); FAO filled other areas with data from regional
fisheries management organizations and entered 1.3 million tonnes as
“Marine fishes nei” in area 61 (FAO 2024). Recent distant-water catch of
China is therefore partly counted in its adjacent area 61.

## 参考文献

FAO (2024). The State of World Fisheries and Aquaculture 2024. Blue
Transformation in action. Rome, FAO.
[doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en)

Garibaldi, L. and Limongelli, L. (2003). Trends in oceanic captures and
clustering of large marine ecosystems: two studies based on the FAO
capture database. FAO Fisheries Technical Paper 435. Rome, FAO.
<https://www.fao.org/4/y4449e/y4449e00.htm>

Grainger, R.J.R. and Garcia, S.M. (1996). Chronicles of marine fishery
landings (1950-1994): trend analysis and fisheries potential. FAO
Fisheries Technical Paper 359. Rome, FAO.
<https://www.fao.org/4/w3244e/w3244e00.htm>

Swartz, W., Sala, E., Tracey, S., Watson, R. and Pauly, D. (2010). The
spatial expansion and ecological footprint of fisheries (1950 to
present). PLoS ONE 5: e15143.
[doi:10.1371/journal.pone.0015143](https://doi.org/10.1371/journal.pone.0015143)

## 示例

``` r

distant_water_profile("Spain", lang = "zh")
distant_water_profile("Korea, Republic of", min_catch = 100)
```
