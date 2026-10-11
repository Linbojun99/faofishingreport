# Download a Sea Surface Temperature Product

This function downloads one of the sea surface temperature (SST)
products listed by
[`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md),
one year at a time, and computes the yearly SST of each FAO major marine
fishing area (see
[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)
for the method). The product can then be used by its name, e.g.
`fao_area_sst(sst = "hadisst1")`,
`mean_temperature_catch(..., sst = "hadisst1")` or
`fao_settings(sst = "hadisst1")`. Files are kept in the `sst` folder of
`data_dir`, with `files.csv` recording where and when each was
downloaded; years already downloaded are skipped, so an interrupted
download can be resumed by running the function again. For ERSST v5,
whose table is built into the package, the function adds the years
published since.

## Usage

``` r
download_sst_data(
  product = "ersst5",
  years = NULL,
  data_dir = fao_data_dir(),
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- product:

  The product: "ersst5", "hadisst1", "cobe2" or "oisst21"; see
  [`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md).

- years:

  The years to download. Default is NULL: from 1950 (1982 for OISST
  v2.1) to the last complete year.

- data_dir:

  The directory where downloaded data is stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- overwrite:

  A logical value. If TRUE, years already downloaded are downloaded
  again. Default is FALSE.

- quiet:

  A logical value. If TRUE, progress messages are not printed. Default
  is FALSE.

## Value

Invisibly, the area table (as returned by
[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)).

## Details

SST data from other sources can be used without this function, by giving
the path of the NetCDF files (see
[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)).
Reading the files needs the ncdf4 package.

## References

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

## Examples

``` r
if (FALSE) { # \dontrun{
fao_sst_products()
download_sst_data("hadisst1")          # about 240 MB
fao_area_sst("34", sst = "hadisst1")
} # }
```
