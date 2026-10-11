# Settings of CMSY++ and Their Defaults

This function lists the general settings of the CMSY++ code (the section
"General settings for the analysis" of `CMSY++16.R` and the MCMC
settings of its JAGS call), their default values in CMSY++, the values
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
uses (the defaults, unless changed with its `settings` argument), what
each one does and which part of CMSY++ it affects.

## Usage

``` r
cmsy_defaults(settings = list(), lang = NULL)
```

## Arguments

- settings:

  Optional named list of changed settings, e.g. `list(sigmaR = 0.2)`, to
  show them next to the defaults and check them.

- lang:

  The language of the `description` column: "en", "zh" or "both".
  Default is NULL, the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).

## Value

A data frame with one row per setting: `setting`, `cmsy_default`,
`used`, `changed`, `applies_to` and `description`.

## Details

Settings marked "BSM" only matter when abundance data (CPUE or biomass)
are given; FAO data have none, so with FAO data alone only the settings
marked "all" change the results. The chart and output switches of CMSY++
(`mgraphs`, `kobe.plot`, `pp.plot`, `retros`, `save.plots`,
`write.output`, ...) are set by the `plots` argument of
[`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md),
and the file names by the run itself.

## References

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17 pp.
<https://oceanrep.geomar.de/id/eprint/52147/>

## Examples

``` r
# All settings with their CMSY++ defaults
cmsy_defaults()
#>         setting cmsy_default       used changed applies_to
#> 1          CV.C         0.15       0.15   FALSE        all
#> 2       CV.cpue          0.2        0.2   FALSE        BSM
#> 3        sigmaR          0.1        0.1   FALSE        all
#> 4    cor.log.rk        -0.76      -0.76   FALSE     charts
#> 5   rk.cor.beta   2.52, 3.37 2.52, 3.37   FALSE        all
#> 6           nbk            3          3   FALSE        BSM
#> 7         bt4pr        FALSE      FALSE   FALSE        BSM
#> 8    ct_MSY.lim         1.21       1.21   FALSE        all
#> 9  q.biomass.pr     0.9, 1.1   0.9, 1.1   FALSE        BSM
#> 10            n         5000       5000   FALSE     charts
#> 11           ni            3          3   FALSE        all
#> 12          nab            3          3   FALSE        BSM
#> 13           bw            3          3   FALSE        all
#> 14 e.creep.line         TRUE       TRUE   FALSE     charts
#> 15    select.yr           NA         NA   FALSE     output
#> 16     n.chains            2          2   FALSE        all
#> 17       n.iter        60000      60000   FALSE        all
#> 18     n.burnin        30000      30000   FALSE        all
#> 19       n.thin           10         10   FALSE        all
#>                                                                                       description
#> 1                                       Coefficient of variation of the catch (observation error)
#> 2                                              Minimum realistic coefficient of variation of CPUE
#> 3                                                              Process error (standard deviation)
#> 4                                             Correlation of log r and log k, used only in charts
#> 5                                                        Beta prior of the r-k correlation plus 1
#> 6  Number of B/k priors used by BSM: 1 (first year), 2 (and intermediate year), 3 (and last year)
#> 7                                                    Use the abundance data to set the B/k priors
#> 8                      Ratio of catch to the MSY prior above which the B/k prior is kept constant
#> 9                                            Prior range of q when the abundance data are biomass
#> 10                                        Number of points of the r-k cloud in the analysis chart
#> 11                                                   Iterations of r-k-start biomass combinations
#> 12                                         Minimum number of years with abundance data to run BSM
#> 13                                                 Bandwidth of ksmooth() for smoothing the catch
#> 14                              Show the CPUE not corrected for effort creep in the biomass chart
#> 15                            Year for which B, F, B/Bmsy and F/Fmsy are also reported (NA: none)
#> 16                                                                   Number of MCMC chains (JAGS)
#> 17                                                     Number of MCMC iterations per chain (JAGS)
#> 18                                                            Number of burn-in iterations (JAGS)
#> 19                                                             Thinning of the MCMC chains (JAGS)

# Check changed settings before a run: here a larger process error and longer chains
cmsy_defaults(list(sigmaR = 0.2, n.iter = 100000, n.burnin = 50000), lang = "zh")
#>         setting cmsy_default       used changed applies_to
#> 1          CV.C         0.15       0.15   FALSE        all
#> 2       CV.cpue          0.2        0.2   FALSE        BSM
#> 3        sigmaR          0.1        0.2    TRUE        all
#> 4    cor.log.rk        -0.76      -0.76   FALSE     charts
#> 5   rk.cor.beta   2.52, 3.37 2.52, 3.37   FALSE        all
#> 6           nbk            3          3   FALSE        BSM
#> 7         bt4pr        FALSE      FALSE   FALSE        BSM
#> 8    ct_MSY.lim         1.21       1.21   FALSE        all
#> 9  q.biomass.pr     0.9, 1.1   0.9, 1.1   FALSE        BSM
#> 10            n         5000       5000   FALSE     charts
#> 11           ni            3          3   FALSE        all
#> 12          nab            3          3   FALSE        BSM
#> 13           bw            3          3   FALSE        all
#> 14 e.creep.line         TRUE       TRUE   FALSE     charts
#> 15    select.yr           NA         NA   FALSE     output
#> 16     n.chains            2          2   FALSE        all
#> 17       n.iter        60000     100000    TRUE        all
#> 18     n.burnin        30000      50000    TRUE        all
#> 19       n.thin           10         10   FALSE        all
#>                                                         description
#> 1                                          渔获量观测误差的变异系数
#> 2                                               CPUE 的最小变异系数
#> 3                                                过程误差（标准差）
#> 4                             log r 与 log k 的相关系数，仅用于作图
#> 5                                     r-k 相关系数加 1 的 beta 先验
#> 6  BSM 使用的 B/k 先验个数：1（首年）、2（加中间年）、3（再加末年）
#> 7                                           用丰度数据设定 B/k 先验
#> 8                 渔获量与 MSY 先验之比超过此值时，B/k 先验保持不变
#> 9                                   丰度数据为生物量时 q 的先验范围
#> 10                                          分析图中 r-k 点云的点数
#> 11                                     r-k-起始生物量组合的迭代次数
#> 12                                  运行 BSM 所需丰度数据的最少年数
#> 13                                    渔获量平滑（ksmooth()）的带宽
#> 14          在生物量图中显示未按渔效增长（effort creep）校正的 CPUE
#> 15        另外报告该年 B、F、B/Bmsy 和 F/Fmsy 的年份（NA 为不报告）
#> 16                                                MCMC 链数（JAGS）
#> 17                                   每条链的 MCMC 迭代次数（JAGS）
#> 18                                  预烧（burn-in）迭代次数（JAGS）
#> 19                                            MCMC 抽稀间隔（JAGS）
```
