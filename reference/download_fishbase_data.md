# Download FishBase and SeaLifeBase Data

This function downloads the tables of a FishBase and/or SeaLifeBase
release (the static snapshots of the database tables used by the
rfishbase package, as parquet files) to `data_dir`, so they can be used
offline. By default it downloads the tables the package uses (species,
estimate, families, synonyms, stocks and faoareas: about 15 MB for each
database); a complete release takes about 140 MB for FishBase and 110 MB
for SeaLifeBase.

## Usage

``` r
download_fishbase_data(
  server = c("fishbase", "sealifebase"),
  version = "latest",
  tables = "needed",
  data_dir = fao_data_dir(),
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- server:

  "fishbase", "sealifebase" or both (default).

- version:

  The release to download, e.g. "26.06". Default is "latest".

- tables:

  "needed" (default) for the tables the package uses, "all" for every
  table, or table names, e.g. `c("species", "estimate")`.

- data_dir:

  The directory where the data is saved. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- overwrite:

  A logical value. If TRUE, tables already downloaded are downloaded
  again. Default is FALSE.

- quiet:

  A logical value. If TRUE, progress messages are not printed. Default
  is FALSE.

## Value

Invisibly, a data frame with one row per table: server, version, table,
size and status ("downloaded", "already downloaded" or "failed").

## Details

Downloading beforehand is optional: functions that use FishBase data,
such as
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md),
look online for the newest release first and download the tables they
need. When the computer is offline they use the newest release
downloaded here, and when nothing has been downloaded,
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
uses the table built into the package. Tables already downloaded are
skipped, so an interrupted download can be resumed by running the
function again.

Reading the tables needs the nanoparquet package
(`install.packages("nanoparquet")`). FishBase and SeaLifeBase data are
licensed CC BY-NC 4.0 by the FishBase team: they can be used with
attribution, but not commercially.

## References

Froese, R. and Pauly, D. (Eds.) FishBase. World Wide Web electronic
publication. <https://www.fishbase.se>

Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. World Wide Web
electronic publication. <https://www.sealifebase.se>

Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase:
exploring, manipulating and visualizing FishBase data from R. Journal of
Fish Biology 81(6): 2030-2039.
[doi:10.1111/j.1095-8649.2012.03464.x](https://doi.org/10.1111/j.1095-8649.2012.03464.x)
. Source code: <https://github.com/ropensci/rfishbase>

## Examples

``` r
if (FALSE) { # \dontrun{
check_fishbase_update()
download_fishbase_data()                          # the tables the package uses
download_fishbase_data(tables = "all")            # every table of FishBase and SeaLifeBase
download_fishbase_data("fishbase", tables = c("species", "estimate"))
} # }
```
