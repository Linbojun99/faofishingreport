# Directory for Downloaded FAO Data

Returns the directory where
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
saves releases and where all functions look for them: the folder set
with `fao_settings(data_dir = ...)`, or otherwise the user data folder
of R (`tools::R_user_dir("faofishingreport", "data")`).

## Usage

``` r
fao_data_dir()
```

## Value

A character string with the directory path.

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
