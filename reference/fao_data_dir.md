# Directory for Downloaded FAO Data

Returns the directory where \`download_fao_data()\` saves releases and
where all functions look for them: the folder set with
\`fao_settings(data_dir = ...)\`, or otherwise the user data folder of R
(\`tools::R_user_dir("faofishingreport", "data")\`).

## Usage

``` r
fao_data_dir()
```

## Value

A character string with the directory path.

## Examples

``` r
if (FALSE) { # \dontrun{
fao_data_dir()
fao_settings(data_dir = "D:/shared/fao_data")   # e.g. a folder shared by a team
} # }
```
