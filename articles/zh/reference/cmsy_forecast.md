# cmsy_forecast(): 不同捕捞情景下的CMSY++预测

英文原页：[`cmsy_forecast()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_forecast.md)

## 说明

本函数根据[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)的结果，在不同捕捞死亡率情景（默认为0.5、0.6、0.8和0.95倍Fmsy，与Froese等（2018）相同，以及最后一年的捕捞死亡率）或固定渔获量下对每个种群向前预测，并绘制预测的B/Bmsy和渔获量。

English

This function projects each stock forward from the results of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
under fishing mortality scenarios (by default 0.5, 0.6, 0.8 and 0.95
Fmsy, as in Froese et al. 2018, and the last-year fishing mortality) or
constant catches, and draws the projected B/Bmsy and catch.

## 用法

``` r

cmsy_forecast(
  result,
  stocks = NULL,
  years = 10,
  f = c(0.5, 0.6, 0.8, 0.95),
  status_quo = TRUE,
  catch = NULL,
  sigma = 0.1,
  seed = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show"),
  silent = fao_setting("silent")
)
```

## 参数

- `result`:

  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)的结果。

  English

  The result of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md).

- `stocks`:

  要预测的种群（可选）：`Stock`标识，或物种的FAO英文或中文名称、学名或FAO三字母代码。默认为NULL，即所有已运行的种群。

  English

  Optional stocks to project: `Stock` identifiers or species, by English
  or Chinese FAO name, scientific name or FAO 3-alpha code. Default is
  NULL, all stocks that ran.

- `years`:

  在各种群最后一年中最晚的一年之后预测的年数。默认为10。

  English

  Number of years to project after the latest last year of the stocks.
  Default is 10.

- `f`:

  以Fmsy倍数表示的捕捞死亡率情景。默认为`c(0.5, 0.6, 0.8, 0.95)`。

  English

  Fishing mortality scenarios as multiples of Fmsy. Default is
  `c(0.5, 0.6, 0.8, 0.95)`.

- `status_quo`:

  逻辑值。为TRUE（默认）时，还按最后一年的捕捞死亡率进行预测。

  English

  A logical value. If TRUE (default), the last-year fishing mortality is
  also projected.

- `catch`:

  要预测的固定渔获量（可选），单位为吨/年（例如最后一年的渔获量）。

  English

  Optional constant catches to project, in tonnes per year (e.g. the
  last-year catch).

- `sigma`:

  对数正态过程误差的标准差。默认为0.1。

  English

  Standard deviation of the lognormal process error. Default is 0.1.

- `seed`:

  随机数种子（可选）。

  English

  Optional random seed.

- `plot, table`:

  是否保存图表（`figures/png/cmsy/<stock>/forecast.png`和`figures/png/cmsy/summary/forecast_share.png`）和表格（tables/cmsy/）。默认为TRUE。

  English

  Whether charts (`figures/png/cmsy/<stock>/forecast.png` and
  `figures/png/cmsy/summary/forecast_share.png`) and the table
  (`tables/cmsy/`) are saved. Default is TRUE.

- `lang`:

  图表文字和名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the chart text and names: “en” (English), “zh”
  (Chinese) or “both” (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“en” unless changed).

- `annotations`:

  在坐标轴标题和坐标轴标签之外添加的文字：FALSE（默认）不添加，TRUE全部添加，或”title”、“subtitle”、“caption”、“legend”和”labels”（图内文字）中的任意几项。可以用`options(faofishingreport.cmsy_annotations = TRUE)`更改本次R会话的默认值。

  English

  The text added to the axis titles and labels: FALSE (default) none,
  TRUE all, or any of “title”, “subtitle”, “caption”, “legend” and
  “labels” (text inside the chart). The default can be changed for the
  session with `options(faofishingreport.cmsy_annotations = TRUE)`.

- `pptx`:

  逻辑值。为TRUE时，图表还另存为可编辑的PowerPoint文件，保存在`figures/pptx/cmsy/<stock>/`和`figures/pptx/cmsy/summary/`。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. If TRUE, the charts are also saved as editable
  PowerPoint files in figures/pptx/cmsy/\<stock\>/ and
  figures/pptx/cmsy/summary/. Default is FALSE, which can be changed for
  all functions with `fao_settings(pptx = TRUE)`.

- `plot_show`:

  逻辑值。为TRUE时，每张图在保存的同时也画在绘图窗口中（例如RStudio的Plots窗格），便于立即查看。默认为FALSE，可以用`fao_settings(plot_show = TRUE)`为所有函数统一更改。无论是否显示，图表都会保存为文件（`plot = TRUE`）。

  English

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

- `silent`:

  逻辑值。为FALSE（默认）时，用进度条显示已完成预测的种群数；为TRUE时不输出任何内容。可以用`fao_settings(silent = TRUE)`更改本次R会话的默认值。

  English

  A logical value. If FALSE (default), a progress bar shows how many
  stocks have been projected. If TRUE, nothing is printed. The default
  can be changed for the session with `fao_settings(silent = TRUE)`.

## 返回值

以不可见方式返回一个列表，包含`projection`（按种群、情景和年份：B/Bmsy、F/Fmsy和渔获量的中位数及95%置信限，以及B至少达到Bmsy的概率）和`share`（按情景和年份：处于或高于Bmsy的种群所占比例及其95%置信限，以及种群数）。

English

Invisibly, a list with `projection` (by stock, scenario and year: median
B/Bmsy, F/Fmsy and catch with 95% limits, and the probability that B is
at least Bmsy) and `share` (by scenario and year: share of stocks at or
above Bmsy, with 95% limits, and the number of stocks).

## 详细说明

CMSY++16.R本身没有预测功能，因此这里用CMSY++的模型本身进行预测：生产力在B/k
=
0.25以下线性下降的Schaefer剩余产量模型（CMSY++16.R和Froese等，2023），从本次运行中r、k和最后一年B/k的后验抽样值（来自相同的MCMC迭代）出发，并加入标准差为`sigma`的对数正态过程误差（默认为0.1，即`CMSY++16.R`给其未使用的设置`sigmaR`的值；CMSY++模型本身估计其过程误差）。与CMSY++相同，某一年的生物量是该年捕捞之前的生物量，F是渔获量除以该生物量，F/Fmsy所用的Fmsy在生物量低于Bmsy的一半时线性降低（CMSY++16.R）。保留数据最后一年的渔获量，因此下一年的生物量即为CMSY++模型给出的生物量；各情景从预测的第一年起设定渔获量。在F情景中，渔获量等于情景的F乘以同一年的生物量，因此F/Fmsy等于情景值。

数据结束年份不同的种群，各自从其最后一年预测到同一个最终年份（各种群最后一年中最晚的一年加上`years`）；处于或高于Bmsy的种群所占比例从这一最晚的最后一年起给出，此时每个种群都有值。

English

CMSY++16.R has no projection, so it is done here with the CMSY++ model
itself: the Schaefer surplus-production model in which productivity
declines linearly below B/k = 0.25 (CMSY++16.R and Froese et al. 2023),
started from the posterior draws of r, k and last-year B/k of the run
(which come from the same MCMC iterations), with lognormal process error
of standard deviation `sigma` (0.1 by default, the value `CMSY++16.R`
gives to its unused setting `sigmaR`; the CMSY++ model itself estimates
its process error). As in CMSY++, the biomass of a year is the biomass
before the catch of that year, F is the catch divided by that biomass,
and F/Fmsy uses an Fmsy reduced linearly below half of Bmsy
(CMSY++16.R). The catch of the last year of the data is kept, so the
biomass of the next year is that of the CMSY++ model; the scenarios set
the catch from the first projected year on. In the F scenarios the catch
is the scenario F times the biomass of the same year, so F/Fmsy is the
scenario value.

Stocks whose data end in different years are each projected from their
own last year to the same final year (the latest last year plus
`years`); the share of stocks at or above Bmsy is given from the latest
last year on, when every stock has a value.

## 参考文献

Froese, R., Winker, H., Coro, G., Demirel, N., Tsikliras, A.C.,
Dimarchopoulou, D., Scarcella, G., Quaas, M. and Matz-Lueck, N. (2018).
Status and rebuilding of European fisheries. Marine Policy 93: 159-170.
[doi:10.1016/j.marpol.2018.04.018](https://doi.org/10.1016/j.marpol.2018.04.018)

Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C.,
Dimarchopoulou, D., Touloumis, K., Demirel, N., Vianna, G.M.S.,
Scarcella, G., Schijns, R., Liang, C. and Pauly, D. (2023). New
developments in the analysis of catch time series as the basis for fish
stock assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria
53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

## 示例

``` r

# result <- cmsy_run(...), see cmsy_run()

# Default scenarios: 0.5, 0.6, 0.8 and 0.95 Fmsy, and the last-year fishing mortality
projection <- cmsy_forecast(result, seed = 1)

# Twenty years, two F scenarios and a constant catch of 50,000 t, in Chinese
projection <- cmsy_forecast(result, years = 20, f = c(0.8, 1), catch = 50000, seed = 1, lang = "zh")
last <- projection$projection[projection$projection$Year == max(projection$projection$Year), ]
last[, c("Stock", "scenario_label", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "p_above_bmsy")]

# Share of the stocks at or above Bmsy (when several stocks were run)
projection$share
```
