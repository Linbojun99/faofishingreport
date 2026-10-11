# Directory for Downloaded FAO Data

Returns the directory where
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
and
[`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)
save data and where all functions look for it: the folder set with
`fao_settings(data_dir = ...)`, else the environment variable
`FAOFISHINGREPORT_DATA_DIR`, else the user data folder of R
(`tools::R_user_dir("faofishingreport", "data")`).

## Usage

``` r
fao_data_dir()
```

## Value

A character string with the directory path.

## Details

To use the same folder in every R session, e.g. a cloud folder shared by
several computers or a team, add a line
`FAOFISHINGREPORT_DATA_DIR=<folder>` to the `.Renviron` file in your
home folder (`file.path(Sys.getenv("HOME"), ".Renviron")`) and restart
R.

## See also

Other FAO data:
[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fao_data_dir()
fao_settings(data_dir = "D:/shared/fao_data")   # e.g. a folder shared by a team
} # }
```
