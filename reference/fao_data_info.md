# Information on the FAO Data Used

Shows which FAO Global Capture Production release the analyses use (the
release that
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
loads), when FAO published it, the years it covers, where it was
downloaded from, its licence and a citation, e.g. for the data section
of a report.

## Usage

``` r
fao_data_info(version = "latest", data_dir = fao_data_dir())
```

## Arguments

- version:

  The release to load, e.g. "2026.1.0". Default is "latest", the newest
  of the downloaded releases and the bundled data. Use "bundled" for the
  data bundled with the package.

- data_dir:

  The directory where downloaded releases are stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## Value

A list of class `fao_data_info` with `release`, `release_date`
(publication date on the FAO website), `years`, `source` ("bundled" with
the package or "downloaded"), `url`, `download_date`, `licence`, `terms`
and `citation`. Printing it shows a summary.

## See also

Other FAO data:
[`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
[`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md),
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md),
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fao_data_info()
fao_data_info()$citation
} # }
```
