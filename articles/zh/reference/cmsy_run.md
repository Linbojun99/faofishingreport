# cmsy_run(): 用FAO渔获数据运行CMSY++

英文原页：[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)

## 说明

本函数对[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)准备好的种群运行本包附带的CMSY++代码（CMSY++16.R，2021年1月版，见[`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md)），并汇总结果：最大可持续产量（MSY）、Bmsy和Fmsy，每年的相对资源量（B/Bmsy）和相对捕捞死亡率（F/Fmsy）及其95%置信限，最后一年各Kobe象限的概率，以及每个种群的状态。

English

This function runs the CMSY++ code bundled with the package
(`CMSY++16.R`, January 2021, see
[`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md))
on the stocks prepared by
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md),
and collects the results: MSY, Bmsy and Fmsy, relative stock size
(B/Bmsy) and exploitation (F/Fmsy) in every year with their 95%
confidence limits, the probability of each Kobe quadrant in the last
year, and the status of each stock.

## 用法

``` r

cmsy_run(
  input,
  stocks = NULL,
  out_dir = "cmsy",
  parallel = NULL,
  plots = c("analysis", "management", "kobe"),
  os = "auto",
  device = c("file", "screen"),
  seed = NULL,
  auto_start = FALSE,
  settings = list(),
  table = TRUE,
  lang = NULL,
  silent = fao_setting("silent"),
  timeout = 3600
)
```

## 参数

- `input`:

  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)的结果。

  English

  The result of
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md).

- `stocks`:

  要运行的种群（可选）：`Stock`标识（例如”PIL_34”），或物种的FAO英文或中文名称、学名或FAO三字母代码（运行该物种的所有种群）。默认为NULL，即`input`中的所有种群。

  English

  Optional stocks to run: `Stock` identifiers (e.g. “PIL_34”) or
  species, by English or Chinese FAO name, scientific name or FAO
  3-alpha code (all stocks of the species). Default is NULL, all stocks
  of `input`.

- `out_dir`:

  CMSY++输出的文件夹。默认为工作目录下的”cmsy”。

  English

  The folder for the CMSY++ output. Default is “cmsy” in the working
  directory.

- `parallel`:

  同时运行的种群数。每个种群还要为其两条MCMC链另外使用两个进程。默认为处理器核心数的一半。

  English

  The number of stocks run at the same time. Each uses two more
  processes for its two MCMC chains. Default is half the number of
  processor cores.

- `plots`:

  要保存为JPEG文件的CMSY++图表：“analysis”、“management”、“kobe”、“prior_posterior”和”retrospective”中的任意几项。默认为前三项。

  English

  The CMSY++ charts to save as JPEG files: any of “analysis”,
  “management”, “kobe”, “prior_posterior” and “retrospective”. Default
  is the first three.

- `os`:

  采用哪个操作系统的设置：“auto”（默认，自动检测）、“windows”、“macos”或”linux”。

  English

  The operating system whose settings are used: “auto” (default, detect
  it), “windows”, “macos” or “linux”.

- `device`:

  CMSY++在将图表保存为JPEG文件之前绘图所用的设备：“file”（默认），一种屏幕外图形设备，在所有系统上和批处理运行中都可用；或”screen”，即系统自身的图形窗口（`windows()`、[`quartz()`](https://rdrr.io/r/grDevices/quartz.html)或[`x11()`](https://rdrr.io/r/grDevices/x11.html)）。

  English

  Where CMSY++ draws its charts before saving them as JPEG files: “file”
  (default), an off-screen device that works on every system and in
  batch runs, or “screen”, the system’s own window (`windows()`,
  [`quartz()`](https://rdrr.io/r/grDevices/quartz.html) or
  [`x11()`](https://rdrr.io/r/grDevices/x11.html)).

- `seed`:

  随机数种子（可选），使运行可以重复并得到相同的结果。

  English

  Optional random seed, so runs can be repeated with the same results.

- `auto_start`:

  逻辑值。当前三年的平均（平滑）渔获量低于最大值的三分之一、起始年份早于2000年且没有给出起始B/k先验时，CMSY++会警告起始生物量不明确（已衰减还是尚未开发），并建议从渔获量至少达到最大值40%的第一年开始（报告在`suggested_start_year`中）。为TRUE时，种群从该年开始。CMSY++自身的相应设置（`auto.start`）在CMSY++16.R中会报错停止（年份范围不再与渔获量年份一致），因此本函数在运行前将同样的规则应用于`StartYear`。默认为FALSE，与CMSY++相同；用户指南更推荐在[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)中设定`StartYear`或起始B/k先验（`stb.low`、`stb.hi`）。

  English

  A logical value. When the mean (smoothed) catch of the first three
  years is below a third of the maximum, the start year is before 2000
  and no start B/k prior is given, CMSY++ warns that the start biomass
  is ambiguous (depleted or unexploited) and suggests starting in the
  first year with at least 40% of the maximum catch (reported in
  `suggested_start_year`). If TRUE, the stock starts in that year.
  CMSY++’s own setting for this (`auto.start`) stops with an error in
  `CMSY++16.R` (the year range no longer matches the catch years), so
  the same rule is applied here to `StartYear` before the run. Default
  is FALSE, as in CMSY++; the user guide prefers setting `StartYear` or
  the start B/k prior (`stb.low`, `stb.hi`) in
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md).

- `settings`:

  可选的命名列表，给出用于替代CMSY++默认值的设置，例如`list(CV.C = 0.2, n.iter = 100000, n.burnin = 50000)`。各项设置、其默认值和作用见[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)。默认为不作更改：CMSY++使用其自身的默认值运行。所用的设置保存在运行文件夹的`settings_used.csv`和`result$settings`中。

  English

  Optional named list of CMSY++ settings that replace its defaults, e.g.
  `list(CV.C = 0.2, n.iter = 100000, n.burnin = 50000)`. See
  [`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)
  for the settings, their defaults and what they do. Default is no
  change: CMSY++ runs with its own defaults. The settings used are saved
  in `settings_used.csv` of the run folder and in `result$settings`.

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

- `silent`:

  逻辑值。为FALSE（默认）时，用进度条显示已完成的种群数以及已用时间和预计剩余时间，并输出运行信息；为TRUE时不输出任何内容。可以用`fao_settings(silent = TRUE)`更改本次R会话的默认值。

  English

  A logical value. If FALSE (default), a progress bar shows how many
  stocks have finished, with the elapsed and the estimated remaining
  time, and messages report the run. If TRUE, nothing is printed. The
  default can be changed for the session with
  `fao_settings(silent = TRUE)`.

- `timeout`:

  单个种群的最长运行时间（秒）；超过后仍在运行的种群会被终止，并记为”timeout”（如果其主分析已经完成，则记为”partial”）。默认值为3600。

  English

  Maximum time for one stock, in seconds; a stock still running after it
  is stopped and reported as “timeout” (or “partial” if its main
  analysis had finished). Default is 3600.

## 返回值

以不可见方式返回一个`cmsy_result`类的列表，包含`summary`（每个种群一行：生物学参考点、最后一年的状态及其95%置信限、Kobe象限概率、所用的先验及其来源链接）、`series`（逐年：渔获量、预测渔获量、B/k、生物量、B/Bmsy、F和F/Fmsy，以及95%置信限）、`kobe`（最后一年的后验抽样值）、`details`（每个种群：r、k和B/k的后验抽样值、先验、回顾性分析的各次运行）、`logs`、`sources`（CMSY++、JAGS、R和数据的版本）和`changes`（对CMSY++代码所做的修改）。汇总表保存在tables/cmsy/。其中的`run_status`在该种群运行无错误结束时为”ok”；主分析已保存结果、但之后的步骤（回顾性分析或某张图）失败或超时时为”partial”；没有结果时为”failed”或”timeout”；`exit`记录运行是怎样结束的。其他函数使用”ok”和”partial”种群的结果。运行完成并不等于评估经过核实：神经网络给出的B/k先验”may
not be appropriate for your
stock”（可能不适合你的种群，CMSY++用户手册），因此应逐个种群核对（自行设定先验见[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)）。

English

Invisibly, a list of class `cmsy_result` with `summary` (one row per
stock: reference points, last-year status with 95% limits, Kobe
probabilities, the priors used and the links to their sources), `series`
(by year: catch, predicted catch, B/k, biomass, B/Bmsy, F and F/Fmsy,
with 95% limits), `kobe` (the last-year draws), `details` (per stock:
posterior draws of r, k and B/k, the priors, retrospective runs),
`logs`, `sources` (the versions of CMSY++, JAGS, R and the data) and
`changes` (the changes made to the CMSY++ code). The summary is saved in
`tables/cmsy/`. Its `run_status` is “ok” when the run of the stock
finished without error, “partial” when the main analysis saved its
results but a later step (the retrospective analysis or a chart) failed
or ran out of time, and “failed” or “timeout” when there are no results;
`exit` says how the run ended. The other functions use the results of
“ok” and “partial” stocks. A finished run is not a validated assessment:
the B/k priors suggested by the neural network “may not be appropriate
for your stock” (CMSY++ user guide), so check them stock by stock (see
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)
for setting your own).

## 详细说明

每个种群在`out_dir`下各自的文件夹中、在单独的R进程中运行，使用一份针对本机修改过的代码副本（见[`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md)：按照用户指南为macOS和Linux修改`windows()`和`antialias`，并去掉依赖RStudio的代码、已从CRAN存档的snpar包和自动安装包的代码）。这些文件夹保留CMSY++的原始输出（`cmsy_output.csv`、JPEG图表和日志），便于逐一核查每个结果。

FAO数据没有资源丰度指数，因此只运行CMSY++中仅基于渔获量的部分。FAO（2026）的全球评估将CMSY++列为第3级（Tier
3，数据有限）方法之一；按照其做法，状态只分为两类：最大可持续捕捞（“maximally
sustainably
fished”，最后一年B/Bmsy至少为0.8）或过度捕捞（“overfished”，B/Bmsy低于0.8）；仅基于渔获量的结果不用于将种群判定为低度捕捞。

English

Each stock runs in its own folder under `out_dir` and in a separate R
process, with a copy of the code changed for this computer (see
[`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md):
the `windows()` and `antialias` changes of the user guide for macOS and
Linux, and removing RStudio, the archived snpar package and the
automatic package installation). The folders keep the original CMSY++
output (`cmsy_output.csv`, the JPEG charts and the log), so each result
can be checked.

Since FAO data have no abundance index, only the catch-only part of
CMSY++ runs. Following FAO (2026), whose global review uses CMSY++ among
its Tier 3 (data-limited) methods, the status is given in two classes
only: “maximally sustainably fished” (B/Bmsy at least 0.8 in the last
year) or “overfished” (B/Bmsy below 0.8); catch-only results are not
used to call a stock underfished.

## 参考文献

Froese, R., Winker, H., Coro, G., et al. (2023). New developments in the
analysis of catch time series as the basis for fish stock assessments:
the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome,
FAO. Section “The status of fishery resources”, Box 1.3 (assessment
tiers and status classes).
<https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html>

## 示例

``` r

cmsy_setup()                                       # once
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
input <- cmsy_input(FAO34$area_data)

# Bigeye grunt, with the start year suggested by CMSY++ and a fixed seed
result <- cmsy_run(input, stocks = "Brachydeuterus auritus", auto_start = TRUE, seed = 1)
result$summary[, c("Stock", "first_year", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "F_Fmsy", "status")]
head(result$series)                                # catch, B/k, biomass, B/Bmsy, F/Fmsy by year

# Every CMSY++ chart of the user guide, including the retrospective analysis
result <- cmsy_run(input, stocks = "GRB_34", seed = 1, auto_start = TRUE,
                   plots = c("analysis", "management", "kobe", "prior_posterior", "retrospective"))

# Changed CMSY++ settings (see cmsy_defaults()): longer chains and a larger catch error
result <- cmsy_run(input, stocks = "GRB_34", seed = 1,
                   settings = list(n.iter = 100000, n.burnin = 50000, CV.C = 0.2))
result$settings[result$settings$changed, c("setting", "cmsy_default", "used")]

# The ten main stocks, four at a time, without progress bar or messages
result <- cmsy_run(input, stocks = input$stocks$Stock[1:10], parallel = 4, silent = TRUE)

# Settings that would be used on Windows, and charts in the system's own windows
result <- cmsy_run(input, stocks = "GRB_34", os = "windows")
result <- cmsy_run(input, stocks = "GRB_34", device = "screen")

# Versions and links of everything used, for the methods section of a report
result$sources
```
