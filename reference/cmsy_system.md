# Check the Computer for CMSY++ and Choose the Settings for It

This function finds the operating system, R version and processor of the
computer, checks whether JAGS and the R packages that CMSY++ needs are
installed, and shows the settings that
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
will use on this system.

## Usage

``` r
cmsy_system(
  os = "auto",
  device = c("file", "screen"),
  jags = "recommended",
  silent = fao_setting("silent")
)
```

## Arguments

- os:

  The operating system whose settings are used: "auto" (default, detect
  it), "windows", "macos" or "linux".

- device:

  Where CMSY++ draws its charts before saving them as JPEG files: "file"
  (default), an off-screen device that works on every system and in
  batch runs, or "screen", the system's own window (`windows()`,
  [`quartz()`](https://rdrr.io/r/grDevices/quartz.html) or
  [`x11()`](https://rdrr.io/r/grDevices/x11.html)).

- jags:

  The JAGS version to recommend: "recommended" (default; the newest JAGS
  4 release built for this R version, which works with rjags from CRAN),
  "latest" (JAGS 5.0.0, which needs rjags 5-x from SourceForge) or a
  version number such as "4.3.2".

- silent:

  A logical value. If TRUE, no summary is printed. Default is FALSE, or
  the value set with `fao_settings(silent = )`.

## Value

Invisibly, a list with the system (`os`, `os_detected`, `arch`,
`r_version`), the installed `jags` and `rjags` versions, the `packages`
table, the recommended `jags_installer` and `rjags_source`, the chart
`device` and `antialias` settings, and the `changes` made to the CMSY++
code.

## Details

The CMSY++ code (`CMSY++16.R`) was written on Windows. Its user guide
(step 12) tells Mac users to change every `windows()` call to
[`quartz()`](https://rdrr.io/r/grDevices/quartz.html) and
`antialias = "cleartype"` to `antialias = "default"`, and to comment out
`windows()` on other systems. The code tests for Windows with
`grepl("win", sysname)`, which is also true for macOS ("Darwin"), so on
a Mac it calls `windows()` and stops.
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
makes these changes automatically for the system found here, in a copy
of the code; the bundled original is never changed. The other changes,
made on every system, are listed in the `changes` element.

The system is detected automatically. To use the settings of another
system (e.g. when the detection is wrong), set `os`, or set it for the
session with `options(faofishingreport.cmsy_os = "macos")`.

## References

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17 pp.
<https://oceanrep.geomar.de/id/eprint/52147/>

## See also

[`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
to install what is missing,
[`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md)
for versions and links.

## Examples

``` r
# \donttest{
# What this computer has, and the settings cmsy_run() will use on it
sys <- cmsy_system()
#> CMSY++ on this computer
#>   System:   Linux (detected), x86_64, R 4.6.1
#>   JAGS:     4.3.2, rjags 4.17
#>   Installer for this system: JAGS 4.3.2 (JAGS-4.3.2.tar.gz) https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/
#>   R packages: all installed
#>   Charts:   grDevices::pdf(NULL, width, height) (off-screen); JPEG antialias = "default"
#>   Ready to run cmsy_run().
sys$changes                      # the changes made to a copy of the CMSY++ code
#>                 change applies_to
#> 1 Package installation        all
#> 2                snpar        all
#> 3    Working directory        all
#> 4             Settings        all
#> 5        Chart windows      linux
#> 6       JPEG antialias      linux
#> 7   Results for charts        all
#>                                                                                                                                                                                                                                                                                    what
#> 1                                                                                                                                                                                            Lines that install missing packages on every run are removed (cmsy_setup() installs them).
#> 2 library(snpar) is removed and snpar::runs.test() is replaced by the same approximate runs test, reproduced from snpar 1.0 (Debin Qiu, GPL >= 2; archived on CRAN on 2022-05-10: https://cran.r-project.org/src/contrib/Archive/snpar/; source mirror: https://github.com/cran/snpar).
#> 3                                                                                                                                                           setwd(rstudioapi::getActiveDocumentContext()$path) is removed, so RStudio is not needed; each stock runs in its own folder.
#> 4                                             File names, the stock, the chart switches and the general settings (CMSY++ defaults unless changed with cmsy_run(settings = ), including the MCMC iterations of the JAGS call) are set by cmsy_run() after the original settings section.
#> 5                                                                                               The test grepl("win", sysname), which is also true on macOS ("Darwin"), is replaced; windows(): changed to an off-screen device, so charts are drawn and saved without opening windows.
#> 6                                                                                                                                                                                                 antialias = "cleartype" (Windows only) is changed to "default" (user guide, step 12).
#> 7                 After the main analysis of a stock, its priors, the posterior draws of r, k and B/k, the yearly estimates with 95% limits and the last-year Kobe draws are saved to cmsy_extra.rds (retrospective runs to cmsy_retro.rds) for cmsy_plot(); no calculation is changed.

# The settings that would be used on Windows or macOS, e.g. to check them for a colleague
cmsy_system(os = "windows", silent = TRUE)$changes
#>                 change applies_to
#> 1 Package installation        all
#> 2                snpar        all
#> 3    Working directory        all
#> 4             Settings        all
#> 5        Chart windows    windows
#> 6       JPEG antialias    windows
#> 7   Results for charts        all
#>                                                                                                                                                                                                                                                                                    what
#> 1                                                                                                                                                                                            Lines that install missing packages on every run are removed (cmsy_setup() installs them).
#> 2 library(snpar) is removed and snpar::runs.test() is replaced by the same approximate runs test, reproduced from snpar 1.0 (Debin Qiu, GPL >= 2; archived on CRAN on 2022-05-10: https://cran.r-project.org/src/contrib/Archive/snpar/; source mirror: https://github.com/cran/snpar).
#> 3                                                                                                                                                           setwd(rstudioapi::getActiveDocumentContext()$path) is removed, so RStudio is not needed; each stock runs in its own folder.
#> 4                                             File names, the stock, the chart switches and the general settings (CMSY++ defaults unless changed with cmsy_run(settings = ), including the MCMC iterations of the JAGS call) are set by cmsy_run() after the original settings section.
#> 5                                                                                               The test grepl("win", sysname), which is also true on macOS ("Darwin"), is replaced; windows(): changed to an off-screen device, so charts are drawn and saved without opening windows.
#> 6                                                                                                                                                                                                                                            antialias = "cleartype" is kept (Windows).
#> 7                 After the main analysis of a stock, its priors, the posterior draws of r, k and B/k, the yearly estimates with 95% limits and the last-year Kobe draws are saved to cmsy_extra.rds (retrospective runs to cmsy_retro.rds) for cmsy_plot(); no calculation is changed.
cmsy_system(os = "macos", silent = TRUE)$device_call
#> [1] "grDevices::pdf(NULL, width, height) (off-screen)"

# The JAGS installer recommended here, and the newest JAGS
cmsy_system(silent = TRUE)$jags_installer[, c("version", "file", "url")]
#>    version              file
#> 10   4.3.2 JAGS-4.3.2.tar.gz
#>                                                                                      url
#> 10 https://downloads.sourceforge.net/project/mcmc-jags/JAGS/4.x/Source/JAGS-4.3.2.tar.gz
cmsy_system(jags = "latest", silent = TRUE)$jags_installer[, c("version", "file", "requires")]
#>    version              file                                 requires
#> 13   5.0.0 JAGS-5.0.0.tar.gz C++ compiler, BLAS and LAPACK; rjags 5-x
# }
```
