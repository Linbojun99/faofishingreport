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
  \`fao_data_dir()\`.

- quiet:

  A logical value. If TRUE, no message is printed. Default is FALSE.

## Value

Invisibly, a list with \`latest\` (newest online version), \`bundled\`
(version bundled with the package), \`local\` (newest downloaded
version, or NA if none), \`update_available\` (logical) and \`releases\`
(a data frame of all online releases).

## Examples

``` r
if (FALSE) { # \dontrun{
check_fao_update()
} # }
```
