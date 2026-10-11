# Sea Surface Temperature Products

This function lists the sea surface temperature (SST) products that the
package knows: the built-in NOAA ERSST v5, and the products that
[`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)
can download. For each it gives the provider, grid, first year, data
sources, treatment of sea ice, licence, the reference to cite and the
approximate download size. Any other gridded SST in NetCDF files can
also be used by giving its path (see
[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)).

## Usage

``` r
fao_sst_products()
```

## Value

A data frame with one row per product: `product` (the name used as
`sst`), `name`, `provider`, `grid`, `first_year` (first complete year),
`data_sources`, `sea_ice`, `licence`, `citation`, `doi`, `homepage` (web
page of the product), `download` (server and dataset), `mb_per_year`
(approximate download size) and `built_in`.

## Details

- ersst5 (built in, default):

  NOAA Extended Reconstructed SST version 5 (Huang et al. 2017): 2° x
  2°, monthly, from 1854. In situ observations only (ships and buoys of
  ICOADS, Argo floats from 1999).

- hadisst1:

  Met Office Hadley Centre HadISST1 (Rayner et al. 2003): 1° x 1°,
  monthly, from 1870. In situ observations (Met Office Marine Data Bank,
  with GTS reports from 1982; ICOADS monthly medians for 1871-1995),
  reconstructed by reduced-space optimal interpolation. The dataset
  description carries a warning (10 March 2016) that problems found in
  an analysis by D. Chelton and C. Risien may limit its use where high
  resolution or SST gradients matter (see References).

- cobe2:

  Japan Meteorological Agency COBE-SST2 (Hirahara et al. 2014): 1° x 1°,
  monthly, from 1850. In situ SST and sea-ice observations; satellite
  observations are used to reconstruct variability in regions with few
  observations.

- oisst21:

  NOAA Optimum Interpolation SST version 2.1 (Huang et al. 2021): daily
  0.25° x 0.25° analysis from September 1981 (monthly means from NOAA
  PSL; averaged to 1° by the package). Satellite (AVHRR), ship, buoy and
  Argo observations. Only from 1982.

Only the table of ERSST v5 comes with the package, to keep it small; its
data are also the smallest to download (about 0.8 MB per year, against 3
to 11 MB for the others). The web page of each product is given in the
column `homepage`.

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
fao_sst_products()[, c("product", "name", "grid", "first_year", "built_in")]
#>    product                name
#> 1   ersst5       NOAA ERSST v5
#> 2 hadisst1 Met Office HadISST1
#> 3    cobe2       JMA COBE-SST2
#> 4  oisst21     NOAA OISST v2.1
#>                                                                                        grid
#> 1                                                                    2 x 2 degrees, monthly
#> 2                                                                     1 x 1 degree, monthly
#> 3                                                                     1 x 1 degree, monthly
#> 4 0.25 x 0.25 degree, monthly means of daily analyses (averaged to 1 degree by the package)
#>   first_year built_in
#> 1       1854     TRUE
#> 2       1870    FALSE
#> 3       1850    FALSE
#> 4       1982    FALSE
```
