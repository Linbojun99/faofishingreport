# MCMC Settings and Convergence of CMSY++ Runs

This function shows how the Markov chain Monte Carlo (MCMC) of each
CMSY++ run was set up and whether its chains converged. CMSY++ fits its
Bayesian state-space Schaefer model with JAGS (Plummer 2003), here with
the settings `n.chains`, `n.iter`, `n.burnin` and `n.thin` (see
[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md);
CMSY++16.R uses 2 chains of 60,000 iterations, the first 30,000
discarded as burn-in and every 10th kept).
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
keeps the draws of each chain of the main parameters; nothing of the
model is changed.

## Usage

``` r
cmsy_mcmc(
  result,
  stocks = NULL,
  rhat_max = 1.1,
  n_eff_min = 400,
  plot = TRUE,
  table = TRUE,
  colors = NULL,
  lang = NULL,
  pptx = fao_setting("pptx"),
  silent = fao_setting("silent")
)
```

## Arguments

- result:

  The result of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
  (or
  [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)).

- stocks:

  Optional stocks: `Stock` identifiers or species, by English or Chinese
  FAO name, scientific name or FAO 3-alpha code. Default is NULL, all
  stocks that ran.

- rhat_max:

  Largest Rhat taken as converged. Default is 1.1.

- n_eff_min:

  Smallest effective sample size (all chains together) taken as enough.
  Default is 400.

- plot:

  A logical value. If TRUE (default), the trace and density chart of
  each stock is saved in `figures/png/cmsy/<stock>/`.

- table:

  A logical value. If TRUE (default), the two tables are saved in
  `tables/cmsy/` (`mcmc_settings.csv` and `mcmc_diagnostics.csv`).

- colors:

  Colours of the chains in the chart: NULL (default) for the
  colour-blind safe Okabe-Ito colours (blue, vermilion, green, ...), the
  name of a palette (any palette of `fao_settings(palette = )`, e.g.
  "npg", "Set1", "Tableau 10" or "viridis"), or a vector of colours, one
  per chain (e.g. `c("black", "#D7191C")`); with fewer colours than
  chains, colours are interpolated.

- lang:

  The language of the chart text and parameter names: "en" (English),
  "zh" (Chinese) or "both". Default is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).

- pptx:

  A logical value. If TRUE, the charts are also saved as editable
  PowerPoint files. Default is the value set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (FALSE unless changed).

- silent:

  A logical value. If FALSE (default), the stocks and parameters that
  did not converge or have too few effective draws are reported. Default
  can be changed with `fao_settings(silent = TRUE)`.

## Value

Invisibly, a list with `settings` (one row per stock: `n_chains`,
`n_iter`, `n_burnin`, `n_thin`, `n_kept` per chain and `n_draws` in
all), `diagnostics` (one row per stock and parameter: `mean`, `sd`,
`lcl`, `median`, `ucl`, `rhat`, `n_eff`, `converged` (Rhat at most
`rhat_max`) and `enough_draws` (effective sample size at least
`n_eff_min`)) and `plots` (the ggplot objects).

## Details

For r, k, MSY (r k / 4, as CMSY++16.R computes it), B/k in the first and
last years, B/Bmsy and F/Fmsy in the last year and the deviance, the
function gives the posterior mean, standard deviation and 95% interval,
the potential scale reduction factor Rhat of Gelman and Rubin (1992),
computed with
[`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html),
and the effective sample size of all chains together, computed with
[`coda::effectiveSize()`](https://rdrr.io/pkg/coda/man/effectiveSize.html)
(Plummer et al. 2006). Rhat compares the variance between chains with
the variance within chains; values near 1 mean the chains have mixed: a
parameter is taken as converged when Rhat is at most `rhat_max` (1.1,
Gelman et al. 2013). The effective sample size is the number of
independent draws the autocorrelated draws are worth; below `n_eff_min`
(400, Vehtari et al. 2021) the 95% limits are imprecise. With the
settings of CMSY++16.R the draws of r and k are strongly autocorrelated,
and their effective sample size is often a few hundred; longer and more
chains raise it, e.g.
`cmsy_run(settings = list(n.chains = 4, n.iter = 300000, n.burnin = 150000, n.thin = 50))`,
which takes about five times as long. Converged chains do not mean that
the priors or the model fit the stock.

With `plot = TRUE`, a chart of each stock shows the draws of each chain
in the order they were kept (trace) and their densities, for r, k, MSY
and the last-year B/k, saved as `figures/png/cmsy/<stock>/mcmc.png`; it
is also the "mcmc" chart of
[`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md).

## References

Gelman, A. and Rubin, D.B. (1992). Inference from iterative simulation
using multiple sequences. *Statistical Science* 7(4): 457-472.
[doi:10.1214/ss/1177011136](https://doi.org/10.1214/ss/1177011136)

Gelman, A., Carlin, J.B., Stern, H.S., Dunson, D.B., Vehtari, A. and
Rubin, D.B. (2013). *Bayesian Data Analysis*, 3rd edition. Boca Raton:
CRC Press. [doi:10.1201/b16018](https://doi.org/10.1201/b16018)

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), Vienna,
Austria.

Plummer, M., Best, N., Cowles, K. and Vines, K. (2006). CODA:
convergence diagnosis and output analysis for MCMC. *R News* 6(1): 7-11.
<https://journal.r-project.org/articles/RN-2006-002/>

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and Buerkner, P.-C.
(2021). Rank-normalization, folding, and localization: an improved Rhat
for assessing convergence of MCMC. *Bayesian Analysis* 16(2): 667-718.
[doi:10.1214/20-BA1221](https://doi.org/10.1214/20-BA1221)

## See also

[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)
for the MCMC settings,
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
to change them.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- cmsy_area("Atlantic, Northeast", top = 10)
mcmc <- cmsy_mcmc(result, lang = "zh")

# Chains in other colours: a palette, or one colour per chain
cmsy_mcmc(result, colors = "Set1")
cmsy_mcmc(result, colors = c("grey30", "#D7191C"))
mcmc$settings
mcmc$diagnostics[!mcmc$diagnostics$converged, ]

# Longer chains for a stock that did not converge
result <- cmsy_run(input, stocks = "PRA_27", seed = 1,
                   settings = list(n.iter = 200000, n.burnin = 100000, n.thin = 20))
cmsy_mcmc(result)
} # }
```
