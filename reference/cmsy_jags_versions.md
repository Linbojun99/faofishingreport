# Which JAGS Version Goes with Which R Version

CMSY++ runs its MCMC in JAGS through the rjags package, and the JAGS
installer must match the operating system and the R version: on Windows
and macOS each JAGS binary is built for a range of R versions, and rjags
from CRAN (4-17) works only with JAGS 4. This function lists, for each
system, the R versions, the JAGS installer, the rjags version it needs,
whether it is the default, and the evidence (the README of the JAGS
downloads, and the runs on Windows, macOS and Linux of the package's
GitHub Actions workflow), and marks the row for this computer.

## Usage

``` r
cmsy_jags_versions(
  os = "all",
  r_version = getRversion(),
  silent = fao_setting("silent")
)
```

## Arguments

- os:

  "all" (default), or one system: "auto" (this computer), "windows",
  "macos" or "linux".

- r_version:

  The R version marked as `this_computer`. Default is the running R.

- silent:

  A logical value. If FALSE (default), the match for this computer is
  printed.

## Value

A data frame with one row per system, JAGS version and range of R
versions.

## Details

In short (JAGS 4, the default):

|  |  |  |
|----|----|----|
| **System** | **R version** | **JAGS installer** |
| Windows | 4.5 or 4.6 | JAGS-4.3.2.exe |
| Windows | 4.2 to 4.4 | JAGS-4.3.1.exe |
| macOS | 4.3 or later | JAGS-4.3.2.pkg |
| macOS | 4.2 | JAGS-4.3.1.pkg |
| Linux | any | package manager (e.g. `apt-get install jags`) or JAGS-4.3.2.tar.gz |

All of them work with rjags 4 from CRAN (4-17; the CRAN binary for R 4.2
on macOS is 4-15). JAGS 5.0.0 (`version = "latest"` in
[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)
and `jags = "latest"` in
[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md))
needs rjags 5-3, which is not yet on CRAN and is downloaded from
SourceForge.

[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)
downloads the matching installer automatically, and
[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
downloads and opens it.

## References

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of DSC 2003, Vienna.
<https://mcmc-jags.sourceforge.io/>

## Examples

``` r
# The whole table, with the row for this computer marked
cmsy_jags_versions()
#> This computer (Linux, R 4.6.1): JAGS 4.3.2 (JAGS-4.3.2.tar.gz) with rjags 4-17 (CRAN, built from source).
#>    system   r_versions  jags                installer
#> 1 Windows 4.5 to 4.6.x 4.3.2           JAGS-4.3.2.exe
#> 2 Windows 4.2 to 4.4.x 4.3.1           JAGS-4.3.1.exe
#> 3   macOS       >= 4.3 4.3.2           JAGS-4.3.2.pkg
#> 4   macOS 4.0 to 4.2.x 4.3.1           JAGS-4.3.1.pkg
#> 5   Linux       >= 4.2 4.3.2        JAGS-4.3.2.tar.gz
#> 6 Windows 4.5 to 4.6.x 5.0.0           JAGS-5.0.0.exe
#> 7   macOS       >= 4.3 5.0.0 JAGS-5.0.0-universal.pkg
#> 8   Linux       >= 4.2 5.0.0        JAGS-5.0.0.tar.gz
#>                                                rjags default this_computer
#> 1                                        4-17 (CRAN)    TRUE         FALSE
#> 2                                        4-17 (CRAN)    TRUE         FALSE
#> 3                                        4-17 (CRAN)    TRUE         FALSE
#> 4 4-15 or later (CRAN; its binary for R 4.2 is 4-15)    TRUE         FALSE
#> 5                     4-17 (CRAN, built from source)    TRUE          TRUE
#> 6                  5-3 (SourceForge, Windows binary)   FALSE         FALSE
#> 7   5-3 (SourceForge: binary for R 4.6, else source)   FALSE         FALSE
#> 8                          5-3 (SourceForge, source)   FALSE         FALSE
#>                                                                                                                                                                                                                                               evidence
#> 1                                            JAGS README: the Windows binary of 4.3.2 "was built with Rtools45 and is suitable for interfacing with R 4.5.x"; Rtools45 builds serve R 4.5.0 to 4.6.x (JAGS 5 README); run on R 4.6 by the package's CI
#> 2                                                            JAGS README: earlier releases "are built with earlier versions of the Rtools tool chain"; 4.3.1 (April 2022) is the build for R before 4.5; run on R 4.2, 4.3 and 4.4 by the package's CI
#> 3                                                                                                                     JAGS README: "JAGS-4.3.2.pkg for compatibility with R version 4.3.0 and later"; run on R 4.6 (Apple silicon) by the package's CI
#> 4                                                                                                    JAGS README: "JAGS-4.3.1.pkg for compatibility with R versions 4.0.x to 4.2.x"; run on R 4.2 (Apple silicon, with rjags 4-15) by the package's CI
#> 5                                                Install JAGS with the package manager (e.g. Debian/Ubuntu: sudo apt-get install jags) or from source; rjags is then built for the installed JAGS and any R version; run on Ubuntu by the package's CI
#> 6                                                                               JAGS 5 README: "For compatibility with R 4.5.0 to R 4.6.x, use binaries in the Rtools45 subfolder"; needs rjags 5-x, not yet on CRAN; run on R 4.6 by the package's CI
#> 7 JAGS 5 README: universal installer built for R for macOS 4.3.0 and later (it also contains JAGS 4.3.2); needs rjags 5-x: SourceForge has binaries for R 4.6, other R versions build it from source; run on R 4.6 (Apple silicon) by the package's CI
#> 8                                                                                                                                                                                                      JAGS 5 source code; needs rjags 5-x from source
#>                                                                    readme
#> 1      https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/
#> 2      https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/
#> 3 https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/
#> 4 https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/
#> 5       https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/
#> 6      https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/Windows/
#> 7        https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/macOS/
#> 8       https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/Source/

# One system, e.g. to tell a colleague on Windows with R 4.4 which installer to use
cmsy_jags_versions("windows", r_version = "4.4.1")
#> No JAGS build is listed for R 4.4.1 on this system; see https://sourceforge.net/projects/mcmc-jags/files/JAGS/
#>    system   r_versions  jags      installer                             rjags
#> 1 Windows 4.5 to 4.6.x 4.3.2 JAGS-4.3.2.exe                       4-17 (CRAN)
#> 2 Windows 4.2 to 4.4.x 4.3.1 JAGS-4.3.1.exe                       4-17 (CRAN)
#> 3 Windows 4.5 to 4.6.x 5.0.0 JAGS-5.0.0.exe 5-3 (SourceForge, Windows binary)
#>   default this_computer
#> 1    TRUE         FALSE
#> 2    TRUE         FALSE
#> 3   FALSE         FALSE
#>                                                                                                                                                                                                    evidence
#> 1 JAGS README: the Windows binary of 4.3.2 "was built with Rtools45 and is suitable for interfacing with R 4.5.x"; Rtools45 builds serve R 4.5.0 to 4.6.x (JAGS 5 README); run on R 4.6 by the package's CI
#> 2                 JAGS README: earlier releases "are built with earlier versions of the Rtools tool chain"; 4.3.1 (April 2022) is the build for R before 4.5; run on R 4.2, 4.3 and 4.4 by the package's CI
#> 3                                    JAGS 5 README: "For compatibility with R 4.5.0 to R 4.6.x, use binaries in the Rtools45 subfolder"; needs rjags 5-x, not yet on CRAN; run on R 4.6 by the package's CI
#>                                                               readme
#> 1 https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/
#> 2 https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/
#> 3 https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/Windows/
```
