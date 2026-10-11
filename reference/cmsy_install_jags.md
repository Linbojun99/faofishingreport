# Download and Open the JAGS Installer

This function downloads the JAGS installer that matches the operating
system and R version (see
[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)
and
[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)),
checks its size and checksums, and opens it: the `.exe` installer on
Windows and the `.pkg` installer on macOS. On Linux it downloads the
source code and shows how to install JAGS with the package manager or
from source. An installer placed in the package folder `inst/jags/`
(installed as `jags/`) is used instead of downloading.

## Usage

``` r
cmsy_install_jags(
  version = "recommended",
  os = "auto",
  open = TRUE,
  data_dir = fao_data_dir(),
  silent = fao_setting("silent")
)
```

## Arguments

- version:

  The JAGS version: "recommended" (default), "latest" or a version
  number.

- os:

  The operating system whose settings are used: "auto" (default, detect
  it), "windows", "macos" or "linux".

- open:

  A logical value. If TRUE (default), the installer is opened.

- data_dir:

  The directory where downloads are kept. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- silent:

  A logical value. If TRUE, no summary is printed. Default is FALSE, or
  the value set with `fao_settings(silent = )`.

## Value

Invisibly, the path of the downloaded installer.

## References

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna.
<https://mcmc-jags.sourceforge.io/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Download the installer for this system (size and checksum are checked) and open it
cmsy_install_jags()

# Only download it, e.g. to install it later
path <- cmsy_install_jags(open = FALSE)

# Download the Windows installer on a Mac, e.g. for a colleague
cmsy_install_jags(os = "windows", open = FALSE)
} # }
```
