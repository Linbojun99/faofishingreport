# Read a FishBase or SeaLifeBase Table

This function returns one table of FishBase or SeaLifeBase, such as
"species" (names, habitat, maximum length, ...) or "estimate" (trophic
level, temperature preference, ...). It is read from the copy downloaded
on this computer:

- when the computer is online, the newest release is looked up first,
  and the table is downloaded if it is not on this computer yet;

- when the computer is offline (or with `fao_settings(online = FALSE)`),
  the newest downloaded release that has the table is used, with a
  message.

Releases are fixed snapshots, so a downloaded table is identical to the
one online.

## Usage

``` r
fishbase_table(
  table,
  server = c("fishbase", "sealifebase"),
  version = "latest",
  data_dir = fao_data_dir()
)
```

## Arguments

- table:

  The table name, e.g. "species", "estimate", "ecology", "synonyms" or
  "families".
  [`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)
  shows how many tables a release has; see the rfishbase documentation
  for what they contain.

- server:

  "fishbase" (default) or "sealifebase".

- version:

  The release, e.g. "26.06". Default is "latest".

- data_dir:

  The directory where downloaded data is stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## Value

A data frame, with the attributes `fishbase_server`, `fishbase_version`
and `source_url`.

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
estimate <- fishbase_table("estimate")
attr(estimate, "fishbase_version")
} # }
```
