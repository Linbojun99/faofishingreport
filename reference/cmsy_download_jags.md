# Download the JAGS Installer that Matches This Computer

This function picks the JAGS installer that matches the operating
system, the R version and the processor (see
[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)),
downloads it from its pinned link, and checks its size and checksums
(MD5, and SHA-256 when R can compute it). For JAGS 5 it also downloads
the matching rjags 5-3 package. It can also download the installers of
other systems or R versions, e.g. to install JAGS on computers without
internet or for colleagues.

## Usage

``` r
cmsy_download_jags(
  version = "recommended",
  os = "auto",
  r_version = getRversion(),
  arch = "auto",
  dest = NULL,
  rjags = TRUE,
  data_dir = fao_data_dir(),
  silent = fao_setting("silent")
)
```

## Arguments

- version:

  "recommended" (default: JAGS 4 for this R version, which works with
  rjags from CRAN), "latest" (JAGS 5.0.0 with rjags 5-3) or a JAGS
  version such as "4.3.1".

- os:

  "auto" (default, this computer), "windows", "macos", "linux" or "all"
  (the installers of all three systems for `r_version`).

- r_version:

  The R version the installer is for. Default is the running R.

- arch:

  The processor, for the rjags 5 binary on macOS: "auto" (default),
  "arm64" (Apple silicon) or "x86_64" (Intel).

- dest:

  Optional folder to copy the files to.

- rjags:

  A logical value. If TRUE (default), the rjags package needed by JAGS 5
  is also downloaded (rjags 4-17 for JAGS 4 is installed from CRAN by
  [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)).

- data_dir:

  The directory where downloads are kept. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- silent:

  A logical value. If FALSE (default), the matching and the download are
  reported.

## Value

Invisibly, a data frame with one row per file: system, R version,
component, version, file, local path, link, SHA-256 and whether the
checksums matched.

## Details

The files are kept in the download folder of the package
(`fao_data_dir()/cmsy/downloads`), so they are downloaded only once, and
copied to `dest` when given. Install JAGS by opening the installer
([`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)
or
[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
do this) and restart R.

## References

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna.
<https://mcmc-jags.sourceforge.io/>

## Examples

``` r
if (FALSE) { # \dontrun{
# The installer for this computer (JAGS 4 for this R version)
cmsy_download_jags()

# JAGS 5.0.0 and the matching rjags 5-3
cmsy_download_jags(version = "latest")

# All three systems for R 4.4, copied to a folder to share in a lab
cmsy_download_jags(os = "all", r_version = "4.4.1", dest = "jags_installers")
} # }
```
