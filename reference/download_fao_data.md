# Download an FAO Global Capture Production Release

This function downloads a Global Capture Production release from FAO,
converts it to the same long format as the data bundled with the package
(\`Global_Production_long\`), and saves it as an \`.rds\` file in
\`data_dir\`.

## Usage

``` r
download_fao_data(
  version = "latest",
  data_dir = fao_data_dir(),
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- version:

  The release to download, e.g. "2026.1.0". Default is "latest".

- data_dir:

  The directory where the converted data is saved. Default is
  \`fao_data_dir()\`.

- overwrite:

  A logical value. If FALSE (default), a release that was already
  downloaded is read from disk instead of being downloaded again.

- quiet:

  A logical value. If TRUE, progress messages are not printed. Default
  is FALSE.

## Value

Invisibly, a data frame with the columns of \`Global_Production_long\`.

## Details

Country and species names are matched through their FAO codes to the
names used by the bundled data, so the result works with the existing
analysis functions and Chinese name tables. Only quantities in tonnes
live weight are kept, as in the bundled data.

## Examples

``` r
if (FALSE) { # \dontrun{
check_fao_update()
new_data <- download_fao_data()
} # }
```
