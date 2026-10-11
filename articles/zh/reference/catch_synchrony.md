# catch_synchrony(): 渔获量的同步性与稳定性

英文原页：[`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md)

## 说明

本函数衡量数据中各物种（或各国家）渔获量波动的同步程度，以及它们的非同步性在多大程度上使总渔获量更稳定：包括Loreau和de
Mazancourt（2008）的同步性指数、Ong等（2021）用于FAO渔获量序列的组合效应、Thibaut和Connolly（2013）对总渔获量变异性的分解，以及序列之间的两两相关系数。

English

This function measures how synchronously the catches of the species (or
countries) of the data fluctuate, and how much their asynchrony
stabilises the total catch: the synchrony index of Loreau and de
Mazancourt (2008), the portfolio effect used by Ong et al. (2021) for
FAO catch series, the decomposition of the variability of the total
catch of Thibaut and Connolly (2013), and the pairwise correlations of
the series.

## 用法

``` r

catch_synchrony(
  area_data,
  by = c("species", "country"),
  detrend = c("linear", "none"),
  window = 20,
  top_n = 20,
  min_median = 10,
  min_nonzero = 0.75,
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

  一条序列由什么构成：“species”（默认）或”country”。

  English

  What a series is: “species” (default) or “country”.

- `detrend`:

  “linear”（默认）表示去除每条序列的线性趋势，“none”表示不去除。

  English

  “linear” (default) to remove a linear trend from each series, or
  “none”.

- `window`:

  滑动窗口的长度（年）。默认为20。

  English

  The length in years of the moving windows. Default is 20.

- `top_n`:

  图表中显示的渔获量最大的序列数。默认为20。

  English

  The number of series with the largest catch shown in the charts.
  Default is 20.

- `min_median, min_nonzero`:

  序列的渔获量中位数须高于`min_median`吨/年，且有渔获量的年份须超过全部年份的`min_nonzero`。默认分别为10和0.75（Ong等，2021）。

  English

  Series need a median catch above `min_median` tonnes a year and catch
  in more than `min_nonzero` of the years. Defaults are 10 and 0.75 (Ong
  et al. 2021).

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

以不可见方式返回一个列表，包括`summary`（同步性、各项CV和组合效应）、`series`（每条序列的平均值、标准差、CV及其占总变异性的比例）、`windows`（每个滑动窗口中的各项指标）和`correlation`（序列之间的两两相关系数）。

English

Invisibly, a list with `summary` (synchrony, CVs and portfolio effect),
`series` (mean, standard deviation, CV and share of the total
variability of each series), `windows` (the measures in each moving
window) and `correlation` (pairwise correlations of the series).

## 详细说明

序列的选取与Ong等（2021）相同：渔获量中位数高于`min_median`（10）吨/年，且超过`min_nonzero`（3/4）的年份有渔获量；`by = "species"`时，只使用鉴定到种的条目（Ong等剔除了其他类（miscellaneous）条目和”nei”（未另列明）条目）。每条序列都对年份做线性回归以去除趋势（与Ong等相同），因为趋势会使序列看起来同步（Lepš等，2019）；用`detrend = "none"`可保留趋势。根据各序列的标准差`\sigma_i`、总渔获量的标准差`\sigma_T`及其平均值`m_T`：

- 同步性`\phi = \sigma_T^2 / (\sum_i \sigma_i)^2`，取值从0（波动完全相互抵消）到1（完全同步）（Loreau和de
  Mazancourt，2008，式9）；
- 总渔获量的变异系数（CV）等于`\sqrt{\phi}`乘以各序列的加权平均CV，即`\sum_i \sigma_i / m_T`（Thibaut和Connolly，2013，式2—4）：同步性越低，总量越稳定；
- 组合效应 =（各序列的平均CV - 总量的CV）/ 总量的CV ×
  100（Ong等，2021，依据Carlson和Satterthwaite，2011）：单条序列的变异性比它们的总量高出多少，以百分比表示。

同样的指标还在长度为`window`年的滑动窗口中计算（在每个窗口内去除线性趋势，这是本包的设定），以显示同步性随时间的变化。Ong等（2021）用小波相干（wavelet
coherence）衡量同步性，可以区分不同的时间尺度；本函数使用的是上述较简单的、基于相关的指标。

English

Series are chosen as by Ong et al. (2021): the median catch is above
`min_median` (10) tonnes a year and more than `min_nonzero` (3/4) of the
years have catch; with `by = "species"`, only items identified to
species are used (Ong et al. left out miscellaneous and “nei” items).
Each series is detrended with a linear regression on year (as by Ong et
al.), since trends make series look synchronous (Lepš et al. 2019); use
`detrend = "none"` to keep them. From the standard deviations `\sigma_i`
of the series, the standard deviation `\sigma_T` of the total catch and
its mean `m_T`:

- synchrony `\phi = \sigma_T^2 / (\sum_i \sigma_i)^2`, from 0
  (fluctuations fully compensate each other) to 1 (perfectly
  synchronous) (Loreau and de Mazancourt 2008, eq. 9);
- the coefficient of variation (CV) of the total catch equals
  `\sqrt{\phi}` times the weighted mean CV of the series,
  `\sum_i \sigma_i / m_T` (Thibaut and Connolly 2013, eqs. 2-4): the
  lower the synchrony, the more stable the total;
- portfolio effect = (mean CV of the series - CV of the total) / CV of
  the total x 100 (Ong et al. 2021, after Carlson and Satterthwaite
  2011): how much more variable the single series are than their total,
  in percent.

The same measures are computed in moving windows of `window` years
(linear trend removed in each window, a choice of this package) to show
how synchrony changed over time. Ong et al. (2021) measured synchrony
with wavelet coherence, which separates time scales; this function uses
the simpler correlation-based measures above.

## 参考文献

Carlson, S.M. and Satterthwaite, W.H. (2011). Weakened portfolio effect
in a collapsed salmon population complex. Canadian Journal of Fisheries
and Aquatic Sciences 68: 1579-1589.
[doi:10.1139/f2011-084](https://doi.org/10.1139/f2011-084)

Lepš, J., Götzenberger, L., Valencia, E. and de Bello, F. (2019).
Accounting for long-term directional trends on year-to-year synchrony in
species fluctuations. Ecography 42: 1728-1741.
[doi:10.1111/ecog.04528](https://doi.org/10.1111/ecog.04528)

Loreau, M. and de Mazancourt, C. (2008). Species synchrony and its
drivers: neutral and nonneutral community dynamics in fluctuating
environments. American Naturalist 172: E48-E66.
[doi:10.1086/589746](https://doi.org/10.1086/589746)

Ong, J.J.L., Walter, J.A., Jensen, O.P. and Pinsky, M.L. (2021). Global
hotspots of coherent marine fishery catches. Ecological Applications 31:
e02321. [doi:10.1002/eap.2321](https://doi.org/10.1002/eap.2321)

Thibaut, L.M. and Connolly, S.R. (2013). Understanding
diversity-stability relationships: towards a unified model of portfolio
effects. Ecology Letters 16: 140-150.
[doi:10.1111/ele.12019](https://doi.org/10.1111/ele.12019)

## 示例

``` r

FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
catch_synchrony(FAO34$area_data, lang = "zh")
catch_synchrony(FAO34$area_data, by = "country", window = 15)
```
