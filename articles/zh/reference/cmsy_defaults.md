# cmsy_defaults(): CMSY++设置及其默认值

英文原页：[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)

## 说明

本函数列出CMSY++代码的通用设置（CMSY++16.R中“General settings for the
analysis”一节，以及其JAGS调用中的MCMC设置）、它们在CMSY++中的默认值、[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)所用的值（除非用其`settings`参数更改，否则即为默认值）、每项设置的作用，以及它影响CMSY++的哪一部分。

English

This function lists the general settings of the CMSY++ code (the section
“General settings for the analysis” of `CMSY++16.R` and the MCMC
settings of its JAGS call), their default values in CMSY++, the values
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
uses (the defaults, unless changed with its `settings` argument), what
each one does and which part of CMSY++ it affects.

## 用法

``` r

cmsy_defaults(settings = list(), lang = NULL)
```

## 参数

- `settings`:

  可选的命名列表，给出更改后的设置，例如`list(sigmaR = 0.2)`，用于将它们与默认值并列显示并进行检查。

  English

  Optional named list of changed settings, e.g. `list(sigmaR = 0.2)`, to
  show them next to the defaults and check them.

- `lang`:

  `description`列的语言：“en”、“zh”或”both”。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言。

  English

  The language of the `description` column: “en”, “zh” or “both”.
  Default is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).

## 返回值

一个数据框，每项设置一行，列为`setting`、`cmsy_default`、`used`、`changed`、`applies_to`和`description`。

English

A data frame with one row per setting: `setting`, `cmsy_default`,
`used`, `changed`, `applies_to` and `description`.

## 详细说明

标为”BSM”的设置只在给出资源丰度数据（CPUE或生物量）时才起作用；FAO数据没有资源丰度数据，因此仅用FAO数据时，只有标为”all”的设置会改变结果。CMSY++的图表和输出开关（`mgraphs`、`kobe.plot`、`pp.plot`、`retros`、`save.plots`、`write.output`等）由[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)的`plots`参数设定，文件名则由运行过程本身设定。

English

Settings marked “BSM” only matter when abundance data (CPUE or biomass)
are given; FAO data have none, so with FAO data alone only the settings
marked “all” change the results. The chart and output switches of CMSY++
(`mgraphs`, `kobe.plot`, `pp.plot`, `retros`, `save.plots`,
`write.output`, …) are set by the `plots` argument of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
and the file names by the run itself.

## 参考文献

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17
pp. <https://oceanrep.geomar.de/id/eprint/52147/>

## 示例

``` r

# All settings with their CMSY++ defaults
cmsy_defaults()

# Check changed settings before a run: here a larger process error and longer chains
cmsy_defaults(list(sigmaR = 0.2, n.iter = 100000, n.burnin = 50000), lang = "zh")
```
