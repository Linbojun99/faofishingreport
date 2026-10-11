# Install JAGS and the R Packages Needed by CMSY++

This function prepares the computer for
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md):
it installs the missing R packages from CRAN, downloads the JAGS
installer that matches this system and R version (see
[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md);
with
[`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md),
which checks its size and checksums) and opens it, and checks the
bundled CMSY++ files. It asks before installing anything when R is used
interactively.

## Usage

``` r
cmsy_setup(
  jags = "recommended",
  os = "auto",
  ask = TRUE,
  data_dir = fao_data_dir(),
  silent = fao_setting("silent")
)
```

## Arguments

- jags:

  The JAGS version to recommend: "recommended" (default; the newest JAGS
  4 release built for this R version, which works with rjags from CRAN),
  "latest" (JAGS 5.0.0, which needs rjags 5-x from SourceForge) or a
  version number such as "4.3.2".

- os:

  The operating system whose settings are used: "auto" (default, detect
  it), "windows", "macos" or "linux".

- ask:

  A logical value. If TRUE (default), ask before installing anything. In
  a non-interactive session nothing is installed when `ask` is TRUE.

- data_dir:

  The directory where downloads are kept. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

- silent:

  A logical value. If TRUE, no summary is printed. Default is FALSE, or
  the value set with `fao_settings(silent = )`.

## Value

Invisibly, the result of
[`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md)
after the installation. With `silent = TRUE` no summary or download
messages are printed (questions are still asked when `ask` is TRUE).

## Details

JAGS is a separate program, so it cannot be installed from R. On Windows
and macOS the installer is opened and you click through it; on Linux the
commands to install it are shown. Restart R after installing JAGS and
run `cmsy_setup()` again to check.

The JAGS installer must match the R version (see
[`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)):
on Windows JAGS 4.3.2 for R 4.5 and 4.6 and JAGS 4.3.1 for R 4.2 to 4.4;
on macOS JAGS 4.3.2 for R 4.3 and later and 4.3.1 for R 4.2; on Linux
the JAGS of the package manager. JAGS 4 is used by default because rjags
on CRAN (4-17) needs JAGS 4. JAGS 5.0.0 (`jags = "latest"`) also
installs rjags 5-3 from SourceForge.

## References

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna.
<https://mcmc-jags.sourceforge.io/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Check the computer and install what is missing (asks first)
cmsy_setup()

# Install without asking, e.g. on a server or in a script
cmsy_setup(ask = FALSE)

# JAGS 5.0.0 with rjags 5-3 (from SourceForge) instead of JAGS 4.3.2
cmsy_setup(jags = "latest")
} # }
```
