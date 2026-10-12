# cmsy_mcmc(): CMSY++运行的MCMC设置与收敛诊断

英文原页：[`cmsy_mcmc()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_mcmc.md)

## 说明

该函数显示每次CMSY++运行的马尔可夫链蒙特卡罗（MCMC）是怎样设置的，以及各条链是否收敛。CMSY++用JAGS（Plummer
2003）拟合贝叶斯状态空间Schaefer模型，所用的设置是`n.chains`、`n.iter`、`n.burnin`和`n.thin`（见[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)；CMSY++16.R默认2条链，每条迭代60,000次，前30,000次作为预烧舍弃，之后每10次保留1次）。[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)保存主要参数每条链的抽样，模型本身不作任何改动。

English

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

## 用法

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

## 参数

- `result`:

  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)（或[`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)）的结果。

  English

  The result of
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
  (or
  [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)).

- `stocks`:

  要显示的种群（可选）：种群代码，或物种的英文名、中文名、学名或FAO三字母代码。默认为NULL，即所有运行过的种群。

  English

  Optional stocks: `Stock` identifiers or species, by English or Chinese
  FAO name, scientific name or FAO 3-alpha code. Default is NULL, all
  stocks that ran.

- `rhat_max`:

  视为收敛的最大Rhat。默认为1.1。

  English

  Largest Rhat taken as converged. Default is 1.1.

- `n_eff_min`:

  视为足够的最小有效样本量（所有链合计）。默认为400。

  English

  Smallest effective sample size (all chains together) taken as enough.
  Default is 400.

- `plot`:

  逻辑值。为TRUE（默认）时，把每个种群的轨迹与密度图保存在figures/png/cmsy/\<种群\>/。

  English

  A logical value. If TRUE (default), the trace and density chart of
  each stock is saved in `figures/png/cmsy/<stock>/`.

- `table`:

  逻辑值。为TRUE（默认）时，把两张表保存在tables/cmsy/（mcmc_settings.csv和mcmc_diagnostics.csv）。

  English

  A logical value. If TRUE (default), the two tables are saved in
  `tables/cmsy/` (`mcmc_settings.csv` and `mcmc_diagnostics.csv`).

- `colors`:

  图中各条链的颜色：NULL（默认）为色盲友好的Okabe-Ito配色（蓝、朱红、绿……）；也可以是调色板名称（`fao_settings(palette = )`支持的任何调色板，如”npg”、“Set1”、“Tableau
  10”或”viridis”），或每条链一个颜色的向量（如`c("black", "#D7191C")`）；颜色少于链数时按插值补足。

  English

  Colours of the chains in the chart: NULL (default) for the
  colour-blind safe Okabe-Ito colours (blue, vermilion, green, …), the
  name of a palette (any palette of `fao_settings(palette = )`,
  e.g. “npg”, “Set1”, “Tableau 10” or “viridis”), or a vector of
  colours, one per chain (e.g. `c("black", "#D7191C")`); with fewer
  colours than chains, colours are interpolated.

- `lang`:

  图表文字和参数名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL，即[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言。

  English

  The language of the chart text and parameter names: “en” (English),
  “zh” (Chinese) or “both”. Default is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).

- `pptx`:

  逻辑值。为TRUE时，图表还另存为可编辑的PowerPoint文件。默认为[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)中的设置（未更改时为FALSE）。

  English

  A logical value. If TRUE, the charts are also saved as editable
  PowerPoint files. Default is the value set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (FALSE unless changed).

- `silent`:

  逻辑值。为FALSE（默认）时，报告未收敛或有效样本量不足的种群和参数。可以用`fao_settings(silent = TRUE)`更改默认值。

  English

  A logical value. If FALSE (default), the stocks and parameters that
  did not converge or have too few effective draws are reported. Default
  can be changed with `fao_settings(silent = TRUE)`.

## 返回值

以不可见方式返回一个列表，包含`settings`（每个种群一行：`n_chains`、`n_iter`、`n_burnin`、`n_thin`、每条链保留的抽样数`n_kept`和抽样总数`n_draws`）、`diagnostics`（每个种群每个参数一行：`mean`、`sd`、`lcl`、`median`、`ucl`、`rhat`、`n_eff`、`converged`（Rhat不超过`rhat_max`）和`enough_draws`（有效样本量不少于`n_eff_min`））以及`plots`（ggplot对象）。

English

Invisibly, a list with `settings` (one row per stock: `n_chains`,
`n_iter`, `n_burnin`, `n_thin`, `n_kept` per chain and `n_draws` in
all), `diagnostics` (one row per stock and parameter: `mean`, `sd`,
`lcl`, `median`, `ucl`, `rhat`, `n_eff`, `converged` (Rhat at most
`rhat_max`) and `enough_draws` (effective sample size at least
`n_eff_min`)) and `plots` (the ggplot objects).

## 详细说明

对r、k、MSY（按CMSY++16.R的算法为r k /
4）、第一年和最后一年的B/k、最后一年的B/Bmsy和F/Fmsy以及偏差（deviance），函数给出后验均值、标准差和95%区间，Gelman和Rubin（1992）的潜在尺度缩减因子Rhat（用[`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html)计算），以及所有链合计的有效样本量（用[`coda::effectiveSize()`](https://rdrr.io/pkg/coda/man/effectiveSize.html)计算；Plummer等，2006）。Rhat比较链间方差和链内方差，接近1说明各条链已经混合：Rhat不超过`rhat_max`（1.1，Gelman等，2013）时视为收敛。有效样本量是自相关的抽样相当于多少个独立抽样；低于`n_eff_min`（400，Vehtari等，2021）时，95%区间不够精确。按CMSY++16.R的设置，r和k的抽样自相关很强，有效样本量常常只有几百；加长链并增加链数可以提高有效样本量，例如`cmsy_run(settings = list(n.chains = 4, n.iter = 300000, n.burnin = 150000, n.thin = 50))`，运行时间约为原来的五倍。链收敛并不说明先验或模型适合该种群。

`plot = TRUE`时，每个种群画一张图，按保留顺序显示每条链的抽样（轨迹）及其密度，参数为r、k、MSY和最后一年的B/k，保存为figures/png/cmsy/\<种群\>/mcmc.png；这也是[`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md)的”mcmc”图。

English

For r, k, MSY (r k / 4, as CMSY++16.R computes it), B/k in the first and
last years, B/Bmsy and F/Fmsy in the last year and the deviance, the
function gives the posterior mean, standard deviation and 95% interval,
the potential scale reduction factor Rhat of Gelman and Rubin (1992),
computed with
[`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html),
and the effective sample size of all chains together, computed with
[`coda::effectiveSize()`](https://rdrr.io/pkg/coda/man/effectiveSize.html)
(Plummer et al. 2006). Rhat compares the variance between chains with
the variance within chains; values near 1 mean the chains have mixed: a
parameter is taken as converged when Rhat is at most `rhat_max` (1.1,
Gelman et al. 2013). The effective sample size is the number of
independent draws the autocorrelated draws are worth; below `n_eff_min`
(400, Vehtari et al. 2021) the 95% limits are imprecise. With the
settings of CMSY++16.R the draws of r and k are strongly autocorrelated,
and their effective sample size is often a few hundred; longer and more
chains raise it, e.g.
`cmsy_run(settings = list(n.chains = 4, n.iter = 300000, n.burnin = 150000, n.thin = 50))`,
which takes about five times as long. Converged chains do not mean that
the priors or the model fit the stock.

With `plot = TRUE`, a chart of each stock shows the draws of each chain
in the order they were kept (trace) and their densities, for r, k, MSY
and the last-year B/k, saved as `figures/png/cmsy/<stock>/mcmc.png`; it
is also the “mcmc” chart of
[`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md).

## 参考文献

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

## 另请参阅

[`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)
for the MCMC settings,
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
to change them.

## 示例

``` r

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
```
