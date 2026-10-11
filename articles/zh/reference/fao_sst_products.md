# fao_sst_products(): 海表温度数据产品

英文原页：[`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md)

## 说明

本函数列出本包支持的海表温度（SST）数据产品：包内自带的NOAA ERSST
v5，以及[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)可以下载的产品。对每种产品，它给出提供方、网格、起始年份、数据来源、海冰的处理方式、许可协议、应引用的文献和大致的下载量。其他任何NetCDF文件中的网格化SST数据，也可以通过给出其路径来使用（见[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)）。

English

This function lists the sea surface temperature (SST) products that the
package knows: the built-in NOAA ERSST v5, and the products that
[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)
can download. For each it gives the provider, grid, first year, data
sources, treatment of sea ice, licence, the reference to cite and the
approximate download size. Any other gridded SST in NetCDF files can
also be used by giving its path (see
[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)).

## 用法

``` r

fao_sst_products()
```

## 返回值

一个数据框，每种产品一行：`product`（作为`sst`使用的名称）、`name`（名称）、`provider`（提供方）、`grid`（网格）、`first_year`（第一个完整年份）、`data_sources`（数据来源）、`sea_ice`（海冰的处理）、`licence`（许可协议）、`citation`（引用文献）、`doi`、`homepage`（产品网页）、`download`（下载所用的服务器和数据集）、`mb_per_year`（大致的下载量）和`built_in`（是否随包自带）。

English

A data frame with one row per product: `product` (the name used as
`sst`), `name`, `provider`, `grid`, `first_year` (first complete year),
`data_sources`, `sea_ice`, `licence`, `citation`, `doi`, `homepage` (web
page of the product), `download` (server and dataset), `mb_per_year`
(approximate download size) and `built_in`.

## 详细说明

- **ersst5（包内自带，默认）**：NOAA Extended Reconstructed SST version
  5（Huang等，2017）：2° x
  2°，逐月，1854年起。仅使用现场观测（ICOADS的船舶和浮标资料，1999年起加入Argo浮标）。
- **hadisst1**：英国气象局哈得来中心（Met Office Hadley
  Centre）的HadISST1（Rayner等，2003）：1° x
  1°，逐月，1870年起。使用现场观测（Met Office Marine Data
  Bank，1982年起加入GTS报告；1871–1995年使用ICOADS月中位数），用降维空间最优插值（reduced-space
  optimal
  interpolation）重建。该数据集的说明中附有一则警告（2016年3月10日）：D.
  Chelton和C.
  Risien的分析发现了一些问题，在需要高分辨率或涉及SST梯度的场合，这些问题可能会限制它的使用（见参考文献）。
- **cobe2**：日本气象厅（Japan Meteorological
  Agency）的COBE-SST2（Hirahara等，2014）：1° x
  1°，逐月，1850年起。使用现场SST和海冰观测；在观测稀少的区域，用卫星观测重建其变化。
- **oisst21**：NOAA Optimum Interpolation SST version
  2.1（Huang等，2021）：1981年9月起的逐日0.25° x
  0.25°分析场（逐月平均值取自NOAA
  PSL；本包将其平均到1°）。使用卫星（AVHRR）、船舶、浮标和Argo观测。仅从1982年开始。

为了控制包的大小，只有ERSST
v5的数据表随包提供；它的数据也是下载量最小的（每年约0.8
MB，其他产品为3到11 MB）。各产品的网页见`homepage`列。

English

- **ersst5 (built in, default)**: NOAA Extended Reconstructed SST
  version 5 (Huang et al. 2017): 2° x 2°, monthly, from 1854. In situ
  observations only (ships and buoys of ICOADS, Argo floats from 1999).
- **hadisst1**: Met Office Hadley Centre HadISST1 (Rayner et al. 2003):
  1° x 1°, monthly, from 1870. In situ observations (Met Office Marine
  Data Bank, with GTS reports from 1982; ICOADS monthly medians for
  1871-1995), reconstructed by reduced-space optimal interpolation. The
  dataset description carries a warning (10 March 2016) that problems
  found in an analysis by D. Chelton and C. Risien may limit its use
  where high resolution or SST gradients matter (see References).
- **cobe2**: Japan Meteorological Agency COBE-SST2 (Hirahara et
  al. 2014): 1° x 1°, monthly, from 1850. In situ SST and sea-ice
  observations; satellite observations are used to reconstruct
  variability in regions with few observations.
- **oisst21**: NOAA Optimum Interpolation SST version 2.1 (Huang et
  al. 2021): daily 0.25° x 0.25° analysis from September 1981 (monthly
  means from NOAA PSL; averaged to 1° by the package). Satellite
  (AVHRR), ship, buoy and Argo observations. Only from 1982.

Only the table of ERSST v5 comes with the package, to keep it small; its
data are also the smallest to download (about 0.8 MB per year, against 3
to 11 MB for the others). The web page of each product is given in the
column `homepage`.

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

fao_sst_products()[, c("product", "name", "grid", "first_year", "built_in")]
```
