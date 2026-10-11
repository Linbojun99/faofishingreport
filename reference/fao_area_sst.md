# Sea Surface Temperature of the FAO Major Fishing Areas

This function returns the yearly sea surface temperature (SST) of the 19
FAO major marine fishing areas: the area mean, the range within the area
and the seasonal range, one row per area and year, like the FAO catch
statistics. It is used by
[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md).
It always says which SST data it uses.

## Usage

``` r
fao_area_sst(
  area = NULL,
  sst = fao_setting("sst"),
  level = c("area", "division"),
  variable = NULL,
  data_dir = fao_data_dir()
)
```

## Arguments

- area:

  FAO area codes (e.g. "34") or names as in the FAO data (e.g.
  "Atlantic, Eastern Central"). Default is NULL, all areas.

- sst:

  The SST data: "ersst5" (built in), the name of a product downloaded
  with
  [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md),
  or the path of a NetCDF file or of a folder of NetCDF files. Default
  is the `sst` setting of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md),
  "ersst5" unless changed.

- level:

  "area" (default) for the 19 FAO major marine areas, or "division" for
  the FAO subareas and divisions (e.g. "37.1.1", Balearic), as in the
  catch data of
  [`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md)
  (see Subareas and divisions).

- variable:

  The name of the SST variable in your NetCDF files. Default is NULL,
  found from its name.

- data_dir:

  The directory where downloaded data and computed tables are stored.
  Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## Value

A data frame with one row per area and year: `product`, `area_code`,
`area_en`, `year`, `sst_mean`, `sst_p10`, `sst_p90`, `sst_min_month`,
`sst_max_month` (all in °C), `n_cells` (grid cells in the area),
`source` (the SST data used) and `downloaded` (date the data were
downloaded or computed).

## Which SST data

- Built in (default): NOAA ERSST v5:

  The package comes with the yearly SST of the 19 areas from 1950,
  computed from NOAA Extended Reconstructed SST version 5 (Huang et al.
  2017): a monthly reconstruction on a 2° x 2° grid (about 220 km) from
  in situ observations only (ships and buoys of ICOADS, Argo floats from
  1999). It is the smallest of the common SST products, needs no
  download, and is consistent over the whole period; its grid is coarse
  for small areas such as the Mediterranean and Black Sea (`n_cells`
  gives the number of grid cells of each area).

- Your own NetCDF files:

  Give the path of a NetCDF file, or of a folder of NetCDF files, as
  `sst`, or once for the session with `fao_settings(sst = "path")`. The
  files must hold a gridded SST with longitude, latitude and time,
  monthly or daily (e.g. HadISST, COBE-SST2, OISST, ERSST, or a model
  output). The yearly area means are computed the first time, which can
  take a few minutes, and saved in the `sst` folder of `data_dir`; later
  calls with the same files read the saved table. Reading NetCDF files
  needs the ncdf4 package.

- Downloaded with download_sst_data():

  [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)
  downloads one of the products listed by
  [`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md)
  (HadISST1, COBE-SST2, OISST v2.1, or the newest years of ERSST v5) and
  computes its table; it is then used by its name, e.g.
  `sst = "hadisst1"`.

## Subareas and divisions

With `level = "division"`, the table has one row per FAO subarea and
division of the FAO map in the package
(`inst/extdata/FAO_AREAS_CWP.shp`): `area_code` is its code (e.g.
"34.1.1" or the subarea "34.2") and `area_en` its name. The built-in
ERSST v5 table covers the subareas and divisions of areas 34, 37, 47 and
51, those of the catch data by division
([`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md));
tables from downloaded products or your own files cover all of them.
Many divisions are small for a 2° grid (see `n_cells`; e.g. the Sea of
Azov or the Sea of Marmara have none or a few cells), so a finer SST
product, given as the path of its NetCDF files, suits divisions better.

## How the area means are computed

- Files: the SST variable is found by its name ("sst", "analysed_sst",
  "tos", ...) unless given as `variable`; longitudes may run from -180
  to 180 or from 0 to 360; values in kelvin are converted to °C; if
  there are several depths, the first (the surface) is used. Values
  below -5 °C, which some products use to mark sea ice (-1000 in
  HadISST1), are set to -1.8 °C, the value other products give under sea
  ice. Daily values are averaged by month.

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
sst34 <- fao_area_sst("34")                          # built in: NOAA ERSST v5
plot(sst_mean ~ year, data = sst34, type = "l", ylim = range(sst34$sst_p10, sst34$sst_p90))
lines(sst_p10 ~ year, data = sst34, lty = 2)
lines(sst_p90 ~ year, data = sst34, lty = 2)

# Your own NetCDF file (e.g. HadISST_sst.nc downloaded from the Met Office), for one call ...
fao_area_sst("34", sst = "D:/data/HadISST_sst.nc")
# ... or for the whole session, including mean_temperature_catch()
fao_settings(sst = "D:/data/HadISST_sst.nc")

# Subareas and divisions, e.g. those of the Mediterranean and Black Sea
div <- fao_area_sst(level = "division")
subset(div, startsWith(area_code, "37.") & year == 2024)
} # }
```
