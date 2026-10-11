# Run CMSY++ on FAO Catch Data

This function runs the CMSY++ code bundled with the package
(`CMSY++16.R`, January 2021, see
[`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md))
on the stocks prepared by
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md),
and collects the results: MSY, Bmsy and Fmsy, relative stock size
(B/Bmsy) and exploitation (F/Fmsy) in every year with their 95%
confidence limits, the probability of each Kobe quadrant in the last
year, and the status of each stock.

## Usage

``` r
cmsy_run(
  input,
  stocks = NULL,
  out_dir = "cmsy",
  parallel = NULL,
  plots = c("analysis", "management", "kobe"),
  os = "auto",
  device = c("file", "screen"),
  seed = NULL,
  auto_start = FALSE,
  settings = list(),
  table = TRUE,
  lang = NULL,
  silent = fao_setting("silent"),
  timeout = 3600
)
```

## Arguments

- input:

  The result of
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md).

- stocks:

  Optional stocks to run: `Stock` identifiers (e.g. "PIL_34") or
  species, by English or Chinese FAO name, scientific name or FAO
  3-alpha code (all stocks of the species). Default is NULL, all stocks
  of `input`.

- out_dir:

  The folder for the CMSY++ output. Default is "cmsy" in the working
  directory.

- parallel:

  The number of stocks run at the same time. Each uses two more
  processes for its two MCMC chains. Default is half the number of
  processor cores.

- plots:

  The CMSY++ charts to save as JPEG files: any of "analysis",
  "management", "kobe", "prior_posterior" and "retrospective". Default
  is the first three.

- os:

  The operating system whose settings are used: "auto" (default, detect
  it), "windows", "macos" or "linux".

- device:

  Where CMSY++ draws its charts before saving them as JPEG files: "file"
  (default), an off-screen device that works on every system and in
  batch runs, or "screen", the system's own window (`windows()`,
  [`quartz()`](https://rdrr.io/r/grDevices/quartz.html) or
  [`x11()`](https://rdrr.io/r/grDevices/x11.html)).

- seed:

  Optional random seed, so runs can be repeated with the same results.

- auto_start:

  A logical value. When the mean (smoothed) catch of the first three
  years is below a third of the maximum, the start year is before 2000
  and no start B/k prior is given, CMSY++ warns that the start biomass
  is ambiguous (depleted or unexploited) and suggests starting in the
  first year with at least 40% of the maximum catch (reported in
  `suggested_start_year`). If TRUE, the stock starts in that year.
  CMSY++'s own setting for this (`auto.start`) stops with an error in
  `CMSY++16.R` (the year range no longer matches the catch years), so
  the same rule is applied here to `StartYear` before the run. Default
  is FALSE, as in CMSY++; the user guide prefers setting `StartYear` or
  the start B/k prior (`stb.low`, `stb.hi`) in
  [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md).

- settings:

  Optional named list of CMSY++ settings that replace its defaults, e.g.
  `list(sigmaR = 0.2, n.iter = 100000, n.burnin = 50000)`. See
  [`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)
  for the settings, their defaults and what they do. Default is no
  change: CMSY++ runs with its own defaults. The settings used are saved
  in `settings_used.csv` of the run folder and in `result$settings`.

- table:

  A logical value. If TRUE (default), tables are saved in `tables/`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- silent:

  A logical value. If FALSE (default), a progress bar shows how many
  stocks have finished, with the elapsed and the estimated remaining
  time, and messages report the run. If TRUE, nothing is printed. The
  default can be changed for the session with
  `fao_settings(silent = TRUE)`.

- timeout:

  Maximum time for one stock, in seconds; a stock still running after it
  is reported as failed. Default is 3600.

## Value

Invisibly, a list of class `cmsy_result` with `summary` (one row per
stock: reference points, last-year status with 95% limits, Kobe
probabilities, the priors used and the links to their sources), `series`
(by year: catch, predicted catch, B/k, biomass, B/Bmsy, F and F/Fmsy,
with 95% limits), `kobe` (the last-year draws), `details` (per stock:
posterior draws of r, k and B/k, the priors, retrospective runs),
`logs`, `sources` (the versions of CMSY++, JAGS, R and the data) and
`changes` (the changes made to the CMSY++ code). The summary is saved in
`tables/cmsy/`.

## Details

Each stock runs in its own folder under `out_dir` and in a separate R
process, with a copy of the code changed for this computer (see
[`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md):
the `windows()` and `antialias` changes of the user guide for macOS and
Linux, and removing RStudio, the archived snpar package and the
automatic package installation). The folders keep the original CMSY++
output (`cmsy_output.csv`, the JPEG charts and the log), so each result
can be checked.

Since FAO data have no abundance index, only the catch-only part of
CMSY++ runs. Following FAO (2026), whose global review uses CMSY++ among
its Tier 3 (data-limited) methods, the status is given in two classes
only: "maximally sustainably fished" (B/Bmsy at least 0.8 in the last
year) or "overfished" (B/Bmsy below 0.8); catch-only results are not
used to call a stock underfished.

## References

Froese, R., Winker, H., Coro, G., et al. (2023). New developments in the
analysis of catch time series as the basis for fish stock assessments:
the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome,
FAO. Section "The status of fishery resources", Box 1.3 (assessment
tiers and status classes).
<https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html>

## Examples

``` r
if (FALSE) { # \dontrun{
cmsy_setup()                                       # once
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
input <- cmsy_input(FAO34$area_data)

# Bigeye grunt, with the start year suggested by CMSY++ and a fixed seed
result <- cmsy_run(input, stocks = "Brachydeuterus auritus", auto_start = TRUE, seed = 1)
result$summary[, c("Stock", "first_year", "B_Bmsy", "B_Bmsy_lcl", "B_Bmsy_ucl", "F_Fmsy", "status")]
head(result$series)                                # catch, B/k, biomass, B/Bmsy, F/Fmsy by year

# Every CMSY++ chart of the user guide, including the retrospective analysis
result <- cmsy_run(input, stocks = "GRB_34", seed = 1, auto_start = TRUE,
                   plots = c("analysis", "management", "kobe", "prior_posterior", "retrospective"))

# Changed CMSY++ settings (see cmsy_defaults()): longer chains and a larger process error
result <- cmsy_run(input, stocks = "GRB_34", seed = 1,
                   settings = list(n.iter = 100000, n.burnin = 50000, sigmaR = 0.15))
result$settings[result$settings$changed, c("setting", "cmsy_default", "used")]

# The ten main stocks, four at a time, without progress bar or messages
result <- cmsy_run(input, stocks = input$stocks$Stock[1:10], parallel = 4, silent = TRUE)

# Settings that would be used on Windows, and charts in the system's own windows
result <- cmsy_run(input, stocks = "GRB_34", os = "windows")
result <- cmsy_run(input, stocks = "GRB_34", device = "screen")

# Versions and links of everything used, for the methods section of a report
result$sources
} # }
```
