# Load FAO Global Capture Production Data

This function loads a release downloaded with \`download_fao_data()\`,
or the data bundled with the package.

## Usage

``` r
load_fao_data(version = "latest", data_dir = fao_data_dir())
```

## Arguments

- version:

  The release to load, e.g. "2026.1.0". Default is "latest", the newest
  of the downloaded releases and the bundled data. Use "bundled" for the
  data bundled with the package.

- data_dir:

  The directory where downloaded releases are stored. Default is
  \`fao_data_dir()\`.

## Value

A data frame with the columns of \`Global_Production_long\`.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- load_fao_data()
FAO34 <- subset(data, FAO.major.fishing.area..Name. == "Atlantic, Eastern Central")
} # }
```
