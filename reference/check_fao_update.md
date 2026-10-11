# Check for the Latest FAO Global Capture Production Release

This function reads the FAO fishery data index, lists the Global Capture
Production releases published online, and compares the newest release
with the data bundled with the package and the data downloaded on this
computer.

## Usage

``` r
check_fao_update(data_dir = fao_data_dir(), quiet = FALSE)
```

## Arguments

- data_dir:

  The directory where downloaded releases are stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- quiet:

  A logical value. If TRUE, no message is printed. Default is FALSE.

## Value

Invisibly, a list with `latest` (newest online version), `bundled`
(version bundled with the package), `local` (newest downloaded version,
or NA if none), `update_available` (logical) and `releases` (a data
frame of all online releases).

## See also

Other FAO data:
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md),
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
check_fao_update()
} # }
```
