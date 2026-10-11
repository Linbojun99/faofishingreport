# cmsy_compare(): 比较多次CMSY++运行结果，或与其他评估比较

英文原页：[`cmsy_compare()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_compare.md)

## 说明

本函数绘制多次[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)运行（例如采用不同起始年份或先验的敏感性分析）中同一批种群的B/Bmsy和F/Fmsy，也可以加入其他评估的结果，并给出它们最后一年数值的表格。

English

This function draws B/Bmsy and F/Fmsy of the same stocks from several
runs of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
(e.g. with different start years or priors, a sensitivity analysis) and,
optionally, from other assessments, and gives a table of their last-year
values.

## 用法

``` r

cmsy_compare(
  ...,
  stocks = NULL,
  external = NULL,
  lang = NULL,
  annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
  pptx = getOption("faofishingreport.pptx", FALSE),
  silent = fao_setting("silent")
)
```

## 参数

- `...`:

  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)的结果，可以命名，也可以不命名（例如`default = run1, start_1980 = run2`）。

  English

  Results of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
  named or not (e.g. `default = run1, start_1980 = run2`).

- `stocks`:

  要比较的种群（可选）：`Stock`标识，或物种的FAO英文或中文名称、学名或FAO三字母代码。默认为NULL，即所有已运行的种群。

  English

  Optional stocks to compare: `Stock` identifiers or species, by English
  or Chinese FAO name, scientific name or FAO 3-alpha code. Default is
  NULL, all stocks that ran.

- `external`:

  其他估计结果的数据框（可选），包含`Stock`、`Year`、`B_Bmsy`（可选`F_Fmsy`）和`source`列（`source`为图例中显示的名称，例如另一项评估的机构或模型）。

  English

  Optional data frame of other estimates, with columns `Stock`, `Year`,
  `B_Bmsy` (and optionally `F_Fmsy`) and `source` (the name shown in the
  legend, e.g. the agency or model of the other assessment).

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

  逻辑值。为TRUE时，图表还另存为可编辑的PowerPoint文件，保存在figures/pptx/cmsy/compare/。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. If TRUE, the charts are also saved as editable
  PowerPoint files in figures/pptx/cmsy/compare/. Default is FALSE,
  which can be changed for all functions with
  `fao_settings(pptx = TRUE)`.

- `silent`:

  逻辑值。为TRUE时不输出种群匹配情况的提示信息。默认为FALSE，或`fao_settings(silent = )`设置的值。

  English

  A logical value. If TRUE, the messages on how the stocks were matched
  are not printed. Default is FALSE, or the value set with
  `fao_settings(silent = )`.

## 返回值

以不可见方式返回一个列表，包含`last_year`（每个种群和每次运行一行）和`plots`。

English

Invisibly, a list with `last_year` (one row per stock and run) and
`plots`.

## 示例

``` r

# Sensitivity to the start year
run_1950 <- cmsy_run(input, stocks = "GRB_34", seed = 1)
run_auto <- cmsy_run(input, stocks = "GRB_34", seed = 1, auto_start = TRUE)
cmsy_compare(`from 1950` = run_1950, `CMSY++ start year` = run_auto, annotations = "legend")

# Sensitivity to a CMSY++ setting (see cmsy_defaults())
run_sigma <- cmsy_run(input, stocks = "GRB_34", seed = 1, auto_start = TRUE, settings = list(sigmaR = 0.2))
cmsy_compare(default = run_auto, `sigmaR = 0.2` = run_sigma)$last_year

# With another assessment of the same stock, read from a file with the columns
# Stock, Year, B_Bmsy (optionally F_Fmsy) and source
other <- read.csv("other_assessment.csv")
cmsy_compare(CMSY = run_auto, external = other, annotations = "legend")
```
