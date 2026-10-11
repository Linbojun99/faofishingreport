# fao_area_sst(): FAO主要渔区的海表温度

英文原页：[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)

## 说明

本函数返回19个FAO主要海洋渔区的逐年海表温度（SST）：渔区平均值、渔区内的范围和季节范围，每个渔区每年一行，与FAO捕捞统计的形式相同。[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)会用到本函数。它总会说明所用的是哪一套SST数据。

English

This function returns the yearly sea surface temperature (SST) of the 19
FAO major marine fishing areas: the area mean, the range within the area
and the seasonal range, one row per area and year, like the FAO catch
statistics. It is used by
[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md).
It always says which SST data it uses.

## 用法

``` r

fao_area_sst(
  area = NULL,
  sst = fao_setting("sst"),
  variable = NULL,
  data_dir = fao_data_dir()
)
```

## 参数

- `area`:

  FAO渔区代码（例如”34”），或FAO数据中的渔区名称（例如”Atlantic, Eastern
  Central”）。默认为NULL，即全部渔区。

  English

  FAO area codes (e.g. “34”) or names as in the FAO data
  (e.g. “Atlantic, Eastern Central”). Default is NULL, all areas.

- `sst`:

  SST数据：“ersst5”（包内自带）、用[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)下载的产品名称，或者一个NetCDF文件的路径或存放NetCDF文件的文件夹路径。默认为[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)中的`sst`设置，未更改时为”ersst5”。

  English

  The SST data: “ersst5” (built in), the name of a product downloaded
  with
  [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md),
  or the path of a NetCDF file or of a folder of NetCDF files. Default
  is the `sst` setting of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md),
  “ersst5” unless changed.

- `variable`:

  你的NetCDF文件中SST变量的名称。默认为NULL，即按变量名自动识别。

  English

  The name of the SST variable in your NetCDF files. Default is NULL,
  found from its name.

- `data_dir`:

  保存下载数据和计算结果表的目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where downloaded data and computed tables are stored.
  Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## 返回值

一个数据框，每个渔区每年一行：`product`、`area_code`、`area_en`、`year`、`sst_mean`、`sst_p10`、`sst_p90`、`sst_min_month`、`sst_max_month`（单位均为°C）、`n_cells`（渔区内的网格数）、`source`（所用的SST数据）和`downloaded`（数据的下载或计算日期）。

English

A data frame with one row per area and year: `product`, `area_code`,
`area_en`, `year`, `sst_mean`, `sst_p10`, `sst_p90`, `sst_min_month`,
`sst_max_month` (all in °C), `n_cells` (grid cells in the area),
`source` (the SST data used) and `downloaded` (date the data were
downloaded or computed).

## 使用哪一套SST数据

- **包内自带（默认）：NOAA ERSST
  v5**：本包自带19个渔区1950年以来的逐年SST，由NOAA Extended
  Reconstructed SST version 5（Huang等，2017）计算得到。这是一套2° x
  2°网格（约220
  km）的逐月重建数据，仅使用现场观测（ICOADS的船舶和浮标资料，1999年起加入Argo浮标）。它是常用SST产品中最小的，无需下载，整个时段前后一致；但对于地中海和黑海这样的小渔区，它的网格偏粗（`n_cells`给出每个渔区的网格数）。
- **你自己的NetCDF文件**：把一个NetCDF文件的路径，或存放NetCDF文件的文件夹路径，作为`sst`给出，或者用`fao_settings(sst = "path")`为本次R会话统一设置。文件中必须有带经度、纬度和时间维度的网格化SST，逐月或逐日均可（例如HadISST、COBE-SST2、OISST、ERSST，或模式输出）。渔区逐年平均值在第一次使用时计算（可能需要几分钟），并保存在`data_dir`的`sst`文件夹中；之后用同样的文件调用时，直接读取保存的结果表。读取NetCDF文件需要ncdf4包。
- **用[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)下载**：[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)下载[`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md)所列的一种产品（HadISST1、COBE-SST2、OISST
  v2.1，或ERSST
  v5的最新年份），并计算其数据表；之后按名称使用，例如`sst = "hadisst1"`。

English

- **Built in (default): NOAA ERSST v5**: The package comes with the
  yearly SST of the 19 areas from 1950, computed from NOAA Extended
  Reconstructed SST version 5 (Huang et al. 2017): a monthly
  reconstruction on a 2° x 2° grid (about 220 km) from in situ
  observations only (ships and buoys of ICOADS, Argo floats from 1999).
  It is the smallest of the common SST products, needs no download, and
  is consistent over the whole period; its grid is coarse for small
  areas such as the Mediterranean and Black Sea (`n_cells` gives the
  number of grid cells of each area).
- **Your own NetCDF files**: Give the path of a NetCDF file, or of a
  folder of NetCDF files, as `sst`, or once for the session with
  `fao_settings(sst = "path")`. The files must hold a gridded SST with
  longitude, latitude and time, monthly or daily (e.g. HadISST,
  COBE-SST2, OISST, ERSST, or a model output). The yearly area means are
  computed the first time, which can take a few minutes, and saved in
  the `sst` folder of `data_dir`; later calls with the same files read
  the saved table. Reading NetCDF files needs the ncdf4 package.
- **Downloaded with download_sst_data()**:
  [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)
  downloads one of the products listed by
  [`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md)
  (HadISST1, COBE-SST2, OISST v2.1, or the newest years of ERSST v5) and
  computes its table; it is then used by its name,
  e.g. `sst = "hadisst1"`.

## 渔区平均值的计算方法

- 文件：SST变量按名称识别（“sst”、“analysed_sst”、“tos”等），除非用`variable`指定；经度可以是-180到180，也可以是0到360；以开尔文为单位的值换算成°C；有多个深度层时，使用第一层（表层）。低于-5
  °C的值（有些产品用这种值标记海冰，HadISST1用-1000）设为-1.8
  °C，即其他产品在海冰下给出的值。逐日数据先按月平均。
- 细于1°的网格先平均到约1°（取有值网格的平均）。
- 每个网格计入它所重叠的每个FAO主要渔区（`inst/extdata/FAO_AREAS_CWP.shp`，19个海洋渔区），权重为在球面上计算的重叠面积，因此位于边界上的网格由相邻渔区共享，沿岸网格只计算其海洋部分。
- `sst_mean`是12个月渔区平均值的平均；`sst_p10`和`sst_p90`是各网格年平均SST的加权第10和第90百分位数（渔区内的范围）；`sst_min_month`和`sst_max_month`是最冷月和最暖月的渔区平均值（季节范围；在横跨赤道的渔区，南北两个半球的季节会部分抵消，所以这个范围较窄）。只保留12个月齐全的年份。

北冰洋渔区和南极各渔区的数值，有一部分反映的是海冰，而不是实测的水温。不同的SST产品使用不同的观测和方法，所以它们的数值和趋势会有一定差异；换用另一种产品重复分析，可以看出结果在多大程度上依赖所选的产品。

English

- Files: the SST variable is found by its name (“sst”, “analysed_sst”,
  “tos”, …) unless given as `variable`; longitudes may run from -180 to
  180 or from 0 to 360; values in kelvin are converted to °C; if there
  are several depths, the first (the surface) is used. Values below -5
  °C, which some products use to mark sea ice (-1000 in HadISST1), are
  set to -1.8 °C, the value other products give under sea ice. Daily
  values are averaged by month.
- Grids finer than 1° are first averaged to about 1° (mean of the cells
  with a value).
- Each grid cell counts for each FAO major area it overlaps
  (`inst/extdata/FAO_AREAS_CWP.shp`, the 19 marine areas), weighted by
  the area of the overlap computed on the sphere, so cells on a boundary
  are shared and coastal cells count for their sea part only.
- `sst_mean` is the mean of the 12 monthly area means; `sst_p10` and
  `sst_p90` are the weighted 10th and 90th percentiles of the yearly
  mean SST of the cells (the range within the area); `sst_min_month` and
  `sst_max_month` are the coldest and warmest monthly area means (the
  seasonal range; in areas on both sides of the equator the seasons of
  the two hemispheres partly cancel, so this range is narrow there).
  Only years with all 12 months are kept.

Values over the Arctic Sea and the Antarctic areas partly reflect sea
ice rather than measured water temperature. SST products use different
observations and methods, so their values and trends differ somewhat;
repeating an analysis with a second product shows how much a result
depends on the product.

## 参考文献

Hirahara, S., Ishii, M. and Fukuda, Y. (2014). Centennial-scale sea
surface temperature analysis and its uncertainty. Journal of Climate 27:
57-75.
[doi:10.1175/JCLI-D-12-00837.1](https://doi.org/10.1175/JCLI-D-12-00837.1)

Huang, B., Thorne, P.W., Banzon, V.F., Boyer, T., Chepurin, G.,
Lawrimore, J.H., Menne, M.J., Smith, T.M., Vose, R.S. and Zhang, H.-M.
(2017). Extended Reconstructed Sea Surface Temperature, Version 5
(ERSSTv5): Upgrades, Validations, and Intercomparisons. Journal of
Climate 30: 8179-8205.
[doi:10.1175/JCLI-D-16-0836.1](https://doi.org/10.1175/JCLI-D-16-0836.1)

Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B.,
Smith, T. and Zhang, H.-M. (2021). Improvements of the Daily Optimum
Interpolation Sea Surface Temperature (DOISST) Version 2.1. Journal of
Climate 34: 2923-2939.
[doi:10.1175/JCLI-D-20-0166.1](https://doi.org/10.1175/JCLI-D-20-0166.1)

Rayner, N.A., Parker, D.E., Horton, E.B., Folland, C.K., Alexander,
L.V., Rowell, D.P., Kent, E.C. and Kaplan, A. (2003). Global analyses of
sea surface temperature, sea ice, and night marine air temperature since
the late nineteenth century. Journal of Geophysical Research 108 (D14):
4407. [doi:10.1029/2002JD002670](https://doi.org/10.1029/2002JD002670)

HadISST1 dataset description, with the warning of 10 March 2016 and a
link to the analysis by D. Chelton and C. Risien
(<https://ir.library.oregonstate.edu/xmlui/handle/1957/58385>):
<https://coastwatch.pfeg.noaa.gov/erddap/info/erdHadISST/index.html>

Web pages of the products: ERSST v5
<https://www.ncei.noaa.gov/products/extended-reconstructed-sst>;
HadISST1 <https://www.metoffice.gov.uk/hadobs/hadisst/>; COBE-SST2
<https://psl.noaa.gov/data/gridded/data.cobe2.html>; OISST v2.1
<https://www.ncei.noaa.gov/products/optimum-interpolation-sst>

## 示例

``` r

sst34 <- fao_area_sst("34")                          # built in: NOAA ERSST v5
plot(sst_mean ~ year, data = sst34, type = "l", ylim = range(sst34$sst_p10, sst34$sst_p90))
lines(sst_p10 ~ year, data = sst34, lty = 2)
lines(sst_p90 ~ year, data = sst34, lty = 2)

# Your own NetCDF file (e.g. HadISST_sst.nc downloaded from the Met Office), for one call ...
fao_area_sst("34", sst = "D:/data/HadISST_sst.nc")
# ... or for the whole session, including mean_temperature_catch()
fao_settings(sst = "D:/data/HadISST_sst.nc")
```
