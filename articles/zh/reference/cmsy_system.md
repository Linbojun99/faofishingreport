# cmsy_system(): 检查本机的CMSY++运行环境并选择相应设置

英文原页：[`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md)

## 说明

本函数查明电脑的操作系统、R版本和处理器，检查是否已安装JAGS和CMSY++所需的R包，并显示[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)在该系统上将使用的设置。

English

This function finds the operating system, R version and processor of the
computer, checks whether JAGS and the R packages that CMSY++ needs are
installed, and shows the settings that
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
will use on this system.

## 用法

``` r

cmsy_system(
  os = "auto",
  device = c("file", "screen"),
  jags = "recommended",
  silent = fao_setting("silent")
)
```

## 参数

- `os`:

  采用哪个操作系统的设置：“auto”（默认，自动检测）、“windows”、“macos”或”linux”。

  English

  The operating system whose settings are used: “auto” (default, detect
  it), “windows”, “macos” or “linux”.

- `device`:

  CMSY++把图表保存为JPEG文件之前在哪里绘制图表：“file”（默认），即在所有系统上和批处理运行中都能使用的离屏绘图设备；或”screen”，即系统自己的绘图窗口（`windows()`、[`quartz()`](https://rdrr.io/r/grDevices/quartz.html)或[`x11()`](https://rdrr.io/r/grDevices/x11.html)）。

  English

  Where CMSY++ draws its charts before saving them as JPEG files: “file”
  (default), an off-screen device that works on every system and in
  batch runs, or “screen”, the system’s own window (`windows()`,
  [`quartz()`](https://rdrr.io/r/grDevices/quartz.html) or
  [`x11()`](https://rdrr.io/r/grDevices/x11.html)).

- `jags`:

  推荐的JAGS版本：“recommended”（默认；针对当前R版本编译的最新JAGS
  4版本，可与CRAN上的rjags配合使用）、“latest”（JAGS
  5.0.0，需要SourceForge上的rjags 5-x）或版本号，例如”4.3.2”。

  English

  The JAGS version to recommend: “recommended” (default; the newest JAGS
  4 release built for this R version, which works with rjags from CRAN),
  “latest” (JAGS 5.0.0, which needs rjags 5-x from SourceForge) or a
  version number such as “4.3.2”.

- `silent`:

  逻辑值。为TRUE时不输出汇总信息。默认为FALSE，或`fao_settings(silent = )`设置的值。

  English

  A logical value. If TRUE, no summary is printed. Default is FALSE, or
  the value set with `fao_settings(silent = )`.

## 返回值

以不可见方式返回一个列表，包括系统信息（`os`、`os_detected`、`arch`、`r_version`）、已安装的`jags`和`rjags`版本、`packages`表、推荐的`jags_installer`和`rjags_source`、图表的`device`和`antialias`设置，以及对CMSY++代码所做的修改`changes`。

English

Invisibly, a list with the system (`os`, `os_detected`, `arch`,
`r_version`), the installed `jags` and `rjags` versions, the `packages`
table, the recommended `jags_installer` and `rjags_source`, the chart
`device` and `antialias` settings, and the `changes` made to the CMSY++
code.

## 详细说明

CMSY++代码（`CMSY++16.R`）是在Windows上编写的。其用户手册（第12步）要求Mac用户把所有`windows()`调用改为[`quartz()`](https://rdrr.io/r/grDevices/quartz.html)，把`antialias = "cleartype"`改为`antialias = "default"`，在其他系统上则把`windows()`注释掉。代码用`grepl("win", sysname)`判断是否为Windows，而这一判断在macOS（“Darwin”）上也为真，所以在Mac上代码会调用`windows()`并报错停止。[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)会针对本函数检测到的系统，在代码副本中自动完成这些修改；包内的原始代码始终不变。在所有系统上都会进行的其他修改也列在`changes`元素中。

系统是自动检测的。如需使用其他系统的设置（例如检测有误时），可以设置`os`，或用`options(faofishingreport.cmsy_os = "macos")`为本次R会话设置。

English

The CMSY++ code (`CMSY++16.R`) was written on Windows. Its user guide
(step 12) tells Mac users to change every `windows()` call to
[`quartz()`](https://rdrr.io/r/grDevices/quartz.html) and
`antialias = "cleartype"` to `antialias = "default"`, and to comment out
`windows()` on other systems. The code tests for Windows with
`grepl("win", sysname)`, which is also true for macOS (“Darwin”), so on
a Mac it calls `windows()` and stops.
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
makes these changes automatically for the system found here, in a copy
of the code; the bundled original is never changed. The other changes,
made on every system, are listed in the `changes` element.

The system is detected automatically. To use the settings of another
system (e.g. when the detection is wrong), set `os`, or set it for the
session with `options(faofishingreport.cmsy_os = "macos")`.

## 参考文献

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17
pp. <https://oceanrep.geomar.de/id/eprint/52147/>

## 另请参阅

[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
to install what is missing,
[`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md)
for versions and links.

## 示例

``` r

# What this computer has, and the settings cmsy_run() will use on it
sys <- cmsy_system()
sys$changes                      # the changes made to a copy of the CMSY++ code

# The settings that would be used on Windows or macOS, e.g. to check them for a colleague
cmsy_system(os = "windows", silent = TRUE)$changes
cmsy_system(os = "macos", silent = TRUE)$device_call

# The JAGS installer recommended here, and the newest JAGS
cmsy_system(silent = TRUE)$jags_installer[, c("version", "file", "url")]
cmsy_system(jags = "latest", silent = TRUE)$jags_installer[, c("version", "file", "requires")]
```
