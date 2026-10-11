# faofishingreport

[中文说明](#中文说明) | [English](#english)

## 中文说明

`faofishingreport` 是一个 R 包，用 FAO 全球捕捞产量数据（Global Capture Production）按渔区、国家和物种做分析，输出 PNG 图和表格，也可以选择输出可编辑的 PowerPoint 图，方便直接用于报告。

主要功能：

- 按渔区、国家、物种统计捕捞产量，排名、占比、年代均值和最近 5 年产量
- 对捕捞时间序列做突变点检测（`strucchange`）
- 汇总分析：物种类群结构、资源开发状态、峰值与趋势、数据质量、集中度与多样性、国家分组、突变点同步性、沿岸国与远洋捕捞国
- 检查并下载 FAO 最新发布的数据
- 图表可选英文、中文或中英双语，国家、物种、渔区名使用 FAO 官方中文名
- 自动识别 macOS、Windows、Linux，选用系统里已安装的中文字体

### 安装

需要 R 4.2 或以上版本。

```r
install.packages("devtools")
devtools::install_github("Linbojun99/faofishingreport")
```

### 快速上手

```r
library(faofishingreport)

# 检查 FAO 是否发布了新数据，有就下载（只需在 FAO 更新时运行一次）
check_fao_update()
download_fao_data()

# 选一个渔区，分析主要物种和国家
FAO34 <- fishing_area_data("Atlantic, Eastern Central", lang = "zh")
fishing_area_species(FAO34$area_data, c(1, 10), lang = "zh")
fishing_area_country(FAO34$area_data, c(1, 10), lang = "zh")

# 某个物种由哪些国家捕捞；某个国家主要捕什么
species_country(FAO34$area_data, "European pilchard(=Sardine)", c(1, 10), lang = "both")
country_species(FAO34$area_data, "Morocco", c(1, 10), lang = "both")

# 某个国家在全球所有渔区的捕捞
country_analysis("Morocco", c(1, 10), lang = "en")
```

汇总分析的输入同样是 `fishing_area_data()` 返回的渔区数据，也可以是 `load_fao_data()` 的任意子集（例如某个国家）：

```r
species_group_composition(FAO34$area_data, lang = "zh")   # 物种类群结构
stock_status(FAO34$area_data, lang = "zh")                # 资源开发状态
data_quality(FAO34$area_data, lang = "zh")                # 数据质量
coastal_distant(FAO34$area_data, lang = "zh")             # 沿岸国与远洋捕捞国
area_overview(lang = "zh")                                # 全部海洋渔区对比
```

完整示例见 [`Demo/Demo_FAO34.R`](Demo/Demo_FAO34.R)。

说明：

- 资源状态图只用产量数据（Kleisner et al. 2013），产量下降也可能来自管理、市场或统计口径的变化，结果仅供参考，不能代替资源评估。纳入标准（至少 10 年、连续 5 年有产量、总产量不少于 1000 吨、鉴定到种/属/科）可以通过参数调整。
- 沿岸国是指陆地（含海外领地）距离 FAO 渔区边界 20 公里以内的国家，由包里的渔区边界和 Natural Earth 国界计算得到。
- ISSCAAP 类群、大类和收入分组没有 FAO 官方中文名，所用译名在 `inst/extdata/group_names_zh.csv`；图表文字在 `inst/extdata/chart_labels.csv`，都可以直接修改。

### 函数一览

| 函数 | 用途 |
|---|---|
| `fishing_area_data()` | 取出一个渔区的数据，画总产量趋势、占全球比例和各国产量地图 |
| `fishing_area_species()` | 渔区内产量排名前 N 的物种 |
| `fishing_area_country()` | 渔区内产量排名前 N 的国家 |
| `species_country()` | 某物种的主要捕捞国家，含地图和各年代饼图 |
| `country_species()` | 某国家在渔区内的主要捕捞物种 |
| `country_analysis()` | 某国家在全球的捕捞，按物种和渔区拆分，含渔区地图 |
| `species_group_composition()` | 按 ISSCAAP 物种类群或大类（鱼类、甲壳类、软体动物等）的产量结构 |
| `stock_status()` | 资源状态图：按产量相对峰值的比例，把物种分为开发中、充分开发、过度开发、崩溃、恢复中 |
| `peak_trend()` | 物种、国家或渔区的峰值年份、当前占峰值的比例和近 10 年趋势 |
| `area_overview()` | 全部 FAO 海洋渔区的产量变化和峰值对比 |
| `data_quality()` | 官方数据与估算值的比例，以及鉴定到种、属、科的产量比例 |
| `diversity_concentration()` | 上报物种数、香农多样性指数、HHI 集中度、前 N 名占比 |
| `country_group_summary()` | 按收入分组、大洲或地区汇总产量 |
| `regime_shift()` | 主要物种的突变点，以及每年同时发生突变的物种数 |
| `coastal_distant()` | 沿岸国与远洋捕捞国的产量和主要远洋捕捞国 |
| `check_fao_update()` | 查看 FAO 线上最新版本，和本机数据比较 |
| `download_fao_data()` | 下载并转换 FAO 数据 |
| `load_fao_data()` | 读取本机最新的数据 |
| `fao_translate()` | 把国家、物种、渔区英文名翻成中文或中英双语 |
| `fao_font()` | 查看或更换图表字体 |

每个函数的参数说明可以用 `?函数名` 查看，例如 `?species_country`。

### 图表语言

所有分析函数都有 `lang` 参数：

- `lang = "en"`：英文（默认）
- `lang = "zh"`：中文
- `lang = "both"`：中英双语，例如坐标轴写「百万吨 (Million Tonnes)」，国家写「摩洛哥 Morocco」

旧写法 `ZH = TRUE` 仍然可用，等同于 `lang = "zh"`。

中文名优先使用 FAO 官方代码表和 ASFIS 物种名录里的中文名。FAO 没有给出中文名的，用包里原有的对照表补充；仍然没有的，保留英文。

### 自定义设置

用 `fao_settings()` 在脚本开头设置一次，之后所有函数的图表都按这个设置输出；不带参数运行可以查看当前设置，`fao_settings(reset = TRUE)` 恢复默认。

```r
fao_settings(lang = "zh", theme = "classic", palette = "lancet")
fao_settings(map_projection = "robinson", map_center = 180, map_palette = "YlOrRd")
```

| 设置 | 说明 | 默认 |
|---|---|---|
| `lang` | 默认语言：`"en"`、`"zh"`、`"both"` | `"en"` |
| `theme` | 图表主题：`"bw"`、`"classic"`、`"minimal"`、`"light"`、`"linedraw"`、`"gray"`、`"dark"`，或 ggplot2 主题对象 | `"bw"` |
| `base_size`、`font` | 字号、字体 | 12、按系统自动选择 |
| `palette` | 分类色板：ggsci（`"npg"`、`"lancet"`、`"nejm"`、`"jama"` 等）、RColorBrewer（`"Set2"`、`"Dark2"` 等）、`"Okabe-Ito"`、`"viridis"`，或一组颜色 | `"npg"` |
| `map_palette` | 地图色板：Material 色系（`"deep-orange"`、`"blue"`、`"teal"` 等）、RColorBrewer（`"YlOrRd"`、`"Blues"` 等）、viridis 系列，或一组颜色 | `"deep-orange"` |
| `map_center` | `0`：经度 -180～180（大西洋居中）；`180`：经度 0～360（太平洋居中） | `0` |
| `map_projection` | `"longlat"`、`"robinson"`、`"mollweide"`、`"equal_earth"`、`"winkel_tripel"`、`"mercator"`，或 PROJ 字符串、EPSG 代码 | `"longlat"` |
| `map_land`、`map_ocean` | 无数据国家的颜色、海洋颜色 | `"grey90"`、不填色 |
| `grid` | 是否显示背景网格线（地图为经纬网） | `FALSE` |
| `dpi` | PNG 分辨率 | 600 |
| `pptx` | 是否同时输出可编辑的 PowerPoint 图 | `FALSE` |

只画渔区的地图（如 `country_analysis()` 的渔区产量图）不显示国界；按国家上色的地图显示国界。

### 字体和 Windows

加载包时会识别操作系统，并选一个已安装的中文字体：

| 系统 | 首选字体 | 找不到时依次尝试 |
|---|---|---|
| Windows | SimSun（宋体） | Microsoft YaHei（微软雅黑）、SimHei（黑体）、DengXian（等线）、KaiTi（楷体） |
| macOS | Songti SC | PingFang SC、Heiti SC 等 |
| Linux | Noto Serif CJK SC | Noto Sans CJK SC、思源、文泉驿等 |

想换字体可以用 `fao_settings(font = "Microsoft YaHei")` 或 `fao_font("Microsoft YaHei")`。PNG 图用 `ragg` 生成；输出 PowerPoint 图时，里面写入的也是这个字体。

### 数据

- 包里自带 FAO Global Capture Production 2026.1.0 版（1950–2024 年）。
- `download_fao_data()` 下载的数据保存在 `fao_data_dir()` 指定的目录；分析函数默认使用本机最新的数据。
- `fishing_area_data()` 和 `country_analysis()` 可以用 `data_version` 指定版本：`"bundled"` 为包自带数据，也可以写具体版本号。需要复现旧报告时，可以下载旧版，例如 `download_fao_data("2023.1.1")`（1950–2021 年）。

### 输出

图表和表格输出到当前工作目录：

- `figures/png/`：PNG 图（默认输出）；汇总分析的图在 `figures/png/summary/`，文件名以渔区名开头
- `figures/pptx/`：可编辑的 PowerPoint 图，默认不输出。单个函数加 `pptx = TRUE` 即可输出；在脚本开头写 `fao_settings(pptx = TRUE)`，之后所有函数都会输出
- `tables/`：CSV 表格，使用带 BOM 的 UTF-8 编码，在 Excel 里直接打开中文也不会乱码

图表默认不画背景网格线，年份坐标轴一直标到数据的最后一年。需要网格线时，在脚本开头写 `fao_settings(grid = TRUE)`。

### 维护：更新包自带的数据

FAO 发布新版后，在包的根目录运行：

```sh
Rscript data-raw/update_bundled_data.R
```

脚本会下载最新数据和 ASFIS 物种名录，更新 `inst/extdata/` 里的数据、中英文对照表和版本号。

## English

`faofishingreport` is an R package for analysing FAO Global Capture Production statistics by fishing area, country and species. It saves charts as PNG (and, optionally, editable PowerPoint) and tables as CSV, ready for reports.

Features:

- Catch rankings, shares, decade averages and the last five years, by fishing area, country and species
- Breakpoint analysis of catch time series (`strucchange`)
- Summary analyses: species group composition, stock status plots, peak and trend, data quality, diversity and concentration, country groups, synchronous breakpoints, coastal versus distant-water fleets
- Checks for and downloads the latest FAO data release
- Charts in English, Chinese or both, with official FAO Chinese names for countries, species and fishing areas
- Detects macOS, Windows or Linux and uses an installed Chinese font

### Installation

Requires R 4.2 or later.

```r
install.packages("devtools")
devtools::install_github("Linbojun99/faofishingreport")
```

### Quick start

```r
library(faofishingreport)

check_fao_update()     # is there a newer FAO release?
download_fao_data()    # download it (run once per FAO release)

FAO34 <- fishing_area_data("Atlantic, Eastern Central")
fishing_area_species(FAO34$area_data, c(1, 10))
species_country(FAO34$area_data, "European pilchard(=Sardine)", c(1, 10), lang = "both")
country_analysis("Morocco", c(1, 10))
```

Summary analyses take the same area data (or any subset of `load_fao_data()`): `species_group_composition()`, `stock_status()`, `peak_trend()`, `area_overview()`, `data_quality()`, `diversity_concentration()`, `country_group_summary()`, `regime_shift()` and `coastal_distant()`. Their charts are saved in `figures/png/summary/`.

See [`Demo/Demo_FAO34.R`](Demo/Demo_FAO34.R) for a complete example and `?function_name` for each function's arguments.

### Settings

`fao_settings()` sets, once per session, the default language, chart theme (`theme`, `base_size`, `font`), category palette (`palette`), map palette (`map_palette`), map centre (`map_center`: 0 for longitudes -180 to 180, 180 for 0 to 360), map projection (`map_projection`: longlat, robinson, mollweide, equal_earth, winkel_tripel, mercator, or any PROJ string or EPSG code), grid lines, PNG resolution and PowerPoint output. Run `?fao_settings` for all choices.

### Language, fonts and data

- `lang = "en"` (default), `"zh"` or `"both"` sets the language of chart text and names. `ZH = TRUE` still works and means `lang = "zh"`.
- The chart font is chosen for the operating system: SimSun on Windows, Songti SC on macOS, Noto Serif CJK SC on Linux, or the next installed Chinese font. Change it with `fao_font()`.
- The package bundles FAO release 2026.1.0 (1950–2024). Downloaded releases are stored in `fao_data_dir()`, and the analysis functions use the newest data available. Use `data_version` in `fishing_area_data()` and `country_analysis()` to pick a release.
- Charts are saved as PNG in `figures/png/`. Add `pptx = TRUE` to a function, or set `fao_settings(pptx = TRUE)` once, to also save editable PowerPoint files in `figures/pptx/`.
- CSV tables are written as UTF-8 with a byte order mark, so Excel shows Chinese names correctly.
- Charts have no background grid lines by default (`fao_settings(grid = TRUE)` brings them back), and year axes are labelled up to the last year of the data.

## Data source and licence

Catch data: FAO Fisheries and Aquaculture Statistics, Global Capture Production, <https://www.fao.org/fishery/en/collection/capture>. Species names: FAO ASFIS List of Species for Fishery Statistics Purposes. FAO data are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/); please cite FAO as the source when publishing results.

The package code is licensed under GPL-3.
