# fao_settings(): 图表、地图和输出的设置

英文原页：[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)

## 说明

该函数设置本包所有函数在本次R会话余下时间内绘制和保存图表、地图的方式。不带参数调用时显示当前设置；设`reset = TRUE`时恢复默认设置。每项设置都保存为R选项`faofishingreport.<name>`，因此也可以用[`options()`](https://rdrr.io/r/base/options.html)设置。

English

This function sets, for the rest of the R session, how all functions of
the package draw and save charts and maps. Call it without arguments to
see the current settings, or with `reset = TRUE` to go back to the
defaults. Each setting is stored as the R option
`faofishingreport.<name>`, so it can also be set with
[`options()`](https://rdrr.io/r/base/options.html).

## 用法

``` r

fao_settings(..., reset = FALSE)
```

## 参数

- `...`:

  要修改的设置，形式为`name = value`：

  - **lang**：图表的默认语言：“en”（默认）、“zh”或”both”。函数自身的`lang`参数仍然优先。
  - **theme**：图表主题：“bw”（默认）、“classic”、“minimal”、“light”、“linedraw”、“gray”或”dark”，或一个ggplot2主题对象，如`ggplot2::theme_bw(base_size = 14)`。地图和饼图保持简洁（minimal）主题。
  - **base_size**：图表主题的基础字号。默认为12。
  - **font**：图表文字的字体；见[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md)。
  - **palette**：类别（国家、物种、分组）的颜色：ggsci包的色板（“npg”（默认）、“lancet”、“nejm”、“jama”、“aaas”、“jco”、“d3”等）、RColorBrewer的色板（“Set1”、“Set2”、“Dark2”、“Paired”等）、基础R的色板（[`grDevices::palette.pals()`](https://rdrr.io/r/grDevices/palette.html)，如”Okabe-Ito”或”Tableau
    10”；或[`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)，如”viridis”或”Dark
    3”），或一组颜色。
  - **map_palette**：地图数值的颜色：ggsci的Material
    Design色板（“deep-orange”（默认）、“blue”、“teal”、“red”等）、RColorBrewer色板（“YlOrRd”、“Blues”等）、viridis选项（“viridis”、“magma”、“plasma”、“inferno”、“cividis”、“mako”、“rocket”、“turbo”）、[`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)中的名称，或一组颜色。
  - **map_center**：地图的中心经度：0（默认）表示经度-180到180（大西洋居中），180表示经度0到360（太平洋居中）。也可以写成字符串”-180-180”和”0-360”。
  - **map_projection**：地图投影：“longlat”（默认，普通经纬度坐标）、“robinson”、“mollweide”、“equal_earth”、“winkel_tripel”或”mercator”，或任意PROJ字符串或EPSG代码（如”EPSG:3857”；此时不使用`map_center`）。
  - **map_land**：地图上无数据国家的填充色。默认为”grey90”。
  - **map_ocean**：地图上海洋的填充色。默认为NA（不填充）。
  - **grid**：图表是否显示背景网格线（地图上为经纬网）。默认为FALSE。
  - **dpi**：PNG文件的分辨率。默认为600。
  - **pptx**：是否同时把图表保存为可编辑的PowerPoint文件（需要eoffice包）。默认为FALSE。
  - **data_dir**：[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)保存FAO数据版本、所有函数查找这些数据的文件夹，例如一个团队共享的文件夹。默认为NULL，即R的用户数据文件夹；见[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。
  - **online**：函数是否可以联网查找FishBase的最新版本（见[`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md)）。默认为TRUE；设为FALSE时离线使用已下载的副本，例如在网速较慢时。
  - **sst**：[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)和[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)使用的海表温度（SST）数据：“ersst5”（默认，NOAA
    ERSST
    v5，内置于本包）、用[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)下载的数据产品（如”hadisst1”），或你自己的NetCDF文件的路径，或一个存放NetCDF文件的文件夹的路径（见[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)）。
  - **silent**：CMSY++函数（[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)、[`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md)等）是否静默运行，不显示进度条和提示信息。默认为FALSE。这些函数自身的`silent`参数仍然优先。
  - **show_data**：分析函数运行结束时，是否在控制台显示结果摘要：分析的对象、主要结果表的前几行、保存的图表和表格，以及返回结果包含的部分。默认为TRUE。结果以不可见的方式返回：赋值即可保存，例如`x <- stock_status(data)`。
  - **plot_show**：保存图表的同时是否把图画在绘图窗口中（例如RStudio的Plots窗格）。默认为FALSE。函数自身的`plot_show`参数仍然优先。

  English

  Settings to change, as `name = value`:

  - **lang**: Default language of charts: “en” (default), “zh” or
    “both”. The `lang` argument of a function still takes precedence.
  - **theme**: Chart theme: “bw” (default), “classic”, “minimal”,
    “light”, “linedraw”, “gray” or “dark”, or a ggplot2 theme object
    such as `ggplot2::theme_bw(base_size = 14)`. Maps and pie charts
    keep a minimal theme.
  - **base_size**: Base font size of the chart theme. Default is 12.
  - **font**: Font family for chart text; see
    [`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md).
  - **palette**: Colours of categories (countries, species, groups): a
    palette of the ggsci package (“npg” (default), “lancet”, “nejm”,
    “jama”, “aaas”, “jco”, “d3”, …), of RColorBrewer (“Set1”, “Set2”,
    “Dark2”, “Paired”, …), of base R
    ([`grDevices::palette.pals()`](https://rdrr.io/r/grDevices/palette.html),
    e.g. “Okabe-Ito” or “Tableau 10”; or
    [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html),
    e.g. “viridis” or “Dark 3”), or a vector of colours.
  - **map_palette**: Colours of map values: a Material Design palette of
    ggsci (“deep-orange” (default), “blue”, “teal”, “red”, …), an
    RColorBrewer palette (“YlOrRd”, “Blues”, …), a viridis option
    (“viridis”, “magma”, “plasma”, “inferno”, “cividis”, “mako”,
    “rocket”, “turbo”), a
    [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
    name, or a vector of colours.
  - **map_center**: Central longitude of maps: 0 (default) for
    longitudes -180 to 180 (Atlantic in the centre), or 180 for
    longitudes 0 to 360 (Pacific in the centre). The strings “-180-180”
    and “0-360” are also accepted.
  - **map_projection**: Map projection: “longlat” (default, plain
    longitude-latitude), “robinson”, “mollweide”, “equal_earth”,
    “winkel_tripel” or “mercator”, or any PROJ string or EPSG code
    (e.g. “EPSG:3857”; then `map_center` is not applied).
  - **map_land**: Fill colour of countries without data on maps. Default
    is “grey90”.
  - **map_ocean**: Fill colour of the ocean on maps. Default is NA (no
    fill).
  - **grid**: Whether charts have background grid lines (and maps
    graticules). Default is FALSE.
  - **dpi**: Resolution of the PNG files. Default is 600.
  - **pptx**: Whether charts are also saved as editable PowerPoint files
    (needs the eoffice package). Default is FALSE.
  - **data_dir**: Folder where
    [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
    saves FAO releases and where all functions look for them, e.g. a
    folder shared by a team. Default is NULL, the user data folder of R;
    see
    [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).
  - **online**: Whether functions may connect to the internet to look
    for the newest FishBase release (see
    [`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md)).
    Default is TRUE; use FALSE to work offline with the downloaded copy,
    e.g. on a slow connection.
  - **sst**: The sea surface temperature (SST) data used by
    [`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)
    and
    [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md):
    “ersst5” (default, NOAA ERSST v5, built into the package), a product
    downloaded with
    [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)
    (e.g. “hadisst1”), or the path of your own NetCDF file, or of a
    folder of NetCDF files (see
    [`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)).
  - **silent**: Whether the CMSY++ functions
    ([`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
    [`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md),
    …) work silently, without progress bars and messages. Default is
    FALSE. Their `silent` argument still takes precedence.
  - **show_data**: Whether an analysis function shows a short summary of
    its results in the console when it finishes: what was analysed, the
    first rows of its main table, the charts and tables it saved, and
    the parts of the result it returns. Default is TRUE. The results are
    returned invisibly: assign them to keep them,
    e.g. `x <- stock_status(data)`.
  - **plot_show**: Whether charts are also drawn in the plot window
    (e.g. the Plots pane of RStudio) as they are saved. Default is
    FALSE. The `plot_show` argument of a function still takes
    precedence.

- `reset`:

  逻辑值。为TRUE时先把所有设置恢复为默认值。默认为FALSE。

  English

  A logical value. If TRUE, all settings go back to their defaults
  first.

## 返回值

以不可见方式返回一个列表，包含当前的各项设置。

English

Invisibly, a list of the current settings.

## 另请参阅

Other settings and language:
[`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md),
[`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md)

## 示例

``` r

fao_settings()                                   # show the current settings
fao_settings(lang = "zh", palette = "lancet", theme = "classic")
fao_settings(map_center = 180, map_projection = "robinson", map_palette = "YlOrRd")
fao_settings(reset = TRUE)                       # back to the defaults
```
