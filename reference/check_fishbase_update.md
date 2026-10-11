# Check for the Latest FishBase and SeaLifeBase Releases

This function lists the FishBase and SeaLifeBase releases published
online (the static snapshots used by the rfishbase package) and compares
the newest with the releases downloaded on this computer.

## Usage

``` r
check_fishbase_update(
  server = c("fishbase", "sealifebase"),
  data_dir = fao_data_dir(),
  quiet = FALSE
)
```

## Arguments

- server:

  "fishbase", "sealifebase" or both (default).

- data_dir:

  The directory where downloaded data is stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- quiet:

  A logical value. If TRUE, no message is printed. Default is FALSE.

## Value

Invisibly, a data frame with one row per server: newest online release
(NA when offline), newest downloaded release, number of tables online
and downloaded, and whether an update is available (`update_available`:
TRUE when nothing has been downloaded or a newer release is online; a
release of which only the tables the package needs were downloaded is up
to date).

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
} # }
```
