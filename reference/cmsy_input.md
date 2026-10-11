# Prepare FAO Catch Data for CMSY++

This function turns FAO catch data into the two input files of CMSY++
(Froese et al. 2021, 2023): the catch file (`Stock`, `yr`, `ct`, `bt`)
and the ID file, one row per stock, with the priors and settings. Each
stock gets its resilience and r prior from the built-in FishBase and
SeaLifeBase table (see
[`cmsy_priors()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_priors.md)),
with the source of each value.

## Usage

``` r
cmsy_input(
  area_data,
  species = NULL,
  by = c("area", "country"),
  priors = NULL,
  default_resilience = NULL,
  min_years = 10,
  min_catch = 1000,
  max_filled = 0.2,
  table = TRUE,
  lang = NULL,
  label = NULL,
  silent = fao_setting("silent")
)
```

## Arguments

- area_data:

  FAO catch data: `fishing_area_data()$area_data`, or any subset of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  (e.g. one country, or several fishing areas).

- species:

  Optional species to keep, by English or Chinese FAO name, both
  together (as shown with `lang = "both"`), scientific name or FAO
  3-alpha code, e.g.
  `c("European pilchard(=Sardine)", "Sardinella aurita", "BOG")` or
  Chinese names. Default is NULL, all species.

- by:

  "area" (default): one stock per species item and fishing area, summed
  over countries; or "country": one stock per species item, fishing area
  and country.

- priors:

  Optional data frame of priors and settings that replace the defaults,
  one row per stock or species. It identifies the rows by a `Stock`,
  `alpha3`, `species_en` or `species` column (`species`: any name, as in
  the `species` argument) and can have any of the ID file columns
  `Resilience`, `r.low`, `r.hi`, `stb.low`, `stb.hi`, `int.yr`,
  `intb.low`, `intb.hi`, `endb.low`, `endb.hi`, `StartYear`, `EndYear`
  and `Comment`. Values given here are recorded as "user".

- default_resilience:

  Resilience ("High", "Medium", "Low" or "Very low") for stocks whose
  species has no resilience or prior r in FishBase or SeaLifeBase.
  Default is NULL: such stocks are left out, since the user guide gives
  no default.

- min_years:

  Minimum number of years from the first to the last year with catch.
  Default is 10.

- min_catch:

  Minimum total catch of a stock, in tonnes. Default is 1000.

- max_filled:

  Maximum share of the years of a stock that may be filled by
  interpolation. Default is 0.2.

- table:

  A logical value. If TRUE (default), tables are saved in `tables/`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- label:

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

- silent:

  A logical value. If TRUE, no messages are printed. Default is FALSE,
  or the value set with `fao_settings(silent = )`.

## Value

Invisibly, a list of class `cmsy_input` with `catch` and `id` (the two
CMSY++ input tables), `stocks` (one row per stock with its species,
area, years, filled years, catch and the source of its r prior) and
`excluded` (stocks left out, with the reason). When `table` is TRUE the
four tables are saved in `tables/cmsy/`.

## Details

The FAO statistics contain catches only, with no abundance index (CPUE
or survey biomass), so every stock is assessed with the catch-only part
of CMSY++ (CMSY: `btype = "None"`, `bt` empty) and the Bayesian Schaefer
model (BSM) is not run. The results then depend mostly on the priors for
relative biomass (B/k), which CMSY++ sets with its neural network unless
they are given in `priors`; they are indicative and do not replace stock
assessments.

A stock is the yearly catch of one FAO species item in one fishing area,
summed over all countries (`by = "area"`, as in
[`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md)),
or of one country (`by = "country"`). Only items identified to species
and measured in tonnes are used. Following the user guide, years must be
consecutive and gaps are not allowed: a stock starts with its first and
ends with its last year with catch, and missing or zero catches in
between are filled by linear interpolation between the adjacent years
(the number of filled years is reported). Stocks are left out when they
have fewer than `min_years` years (CMSY++ uses the first and the last
ten years for its default priors), less than `min_catch` tonnes in
total, more than `max_filled` of their years filled, or no prior for r.

Every stock keeps the links to the sources of its data and priors: `url`
(the FishBase or SeaLifeBase page of the species), `data_url` (the
FishBase or SeaLifeBase table the prior was read from), `rule_url` (the
CMSY++ user guide) and `fao_data_url` (the FAO data release).

## References

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for
CMSY++. GEOMAR, Germany, 17 pp.
<https://oceanrep.geomar.de/id/eprint/52147/>

Froese, R., Winker, H., Coro, G., et al. (2023). New developments in the
analysis of catch time series as the basis for fish stock assessments:
the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189.
[doi:10.3897/aiep.53.105910](https://doi.org/10.3897/aiep.53.105910)

## Examples

``` r
# \donttest{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#> Using FAO Global Capture Production release 2026.1.0 (bundled with the package, 1950-2024)

# All species of the area that CMSY++ can assess with catch only
input <- cmsy_input(FAO34$area_data, table = FALSE)
#> 120 stocks ready for CMSY++ (catch only, no abundance data); 360 left out (see $excluded).
input
#> CMSY++ input from FAO capture 2026.1.0: 120 stocks ready, 360 left out
#>   Stock                  species_en                      area first_year
#>  PIL_34 European pilchard(=Sardine) Atlantic, Eastern Central       1950
#>  SAA_34            Round sardinella Atlantic, Eastern Central       1950
#>  VMA_34      Atlantic chub mackerel Atlantic, Eastern Central       1950
#>  BOA_34                  Bonga shad Atlantic, Eastern Central       1950
#>  SKJ_34               Skipjack tuna Atlantic, Eastern Central       1950
#>  ANE_34            European anchovy Atlantic, Eastern Central       1950
#>  YFT_34              Yellowfin tuna Atlantic, Eastern Central       1955
#>  SAE_34         Madeiran sardinella Atlantic, Eastern Central       1950
#>  BET_34                 Bigeye tuna Atlantic, Eastern Central       1950
#>  LHT_34          Largehead hairtail Atlantic, Eastern Central       1959
#>  last_year Resilience r.low  r.hi
#>       2024     Medium 0.415 0.942
#>       2024       High 0.429 0.976
#>       2024     Medium 0.394 0.895
#>       2024       High 0.151 0.342
#>       2024     Medium 0.346 0.787
#>       2024     Medium 0.390 0.886
#>       2024     Medium 0.238 0.540
#>       2024     Medium 0.379 0.862
#>       2024     Medium 0.341 0.775
#>       2024     Medium 0.139 0.315
head(input$stocks[, c("Stock", "species_en", "first_year", "Resilience", "r.low", "r.hi", "r_source")])
#>    Stock                  species_en first_year Resilience r.low  r.hi
#> 1 PIL_34 European pilchard(=Sardine)       1950     Medium 0.415 0.942
#> 2 SAA_34            Round sardinella       1950       High 0.429 0.976
#> 3 VMA_34      Atlantic chub mackerel       1950     Medium 0.394 0.895
#> 4 BOA_34                  Bonga shad       1950       High 0.151 0.342
#> 5 SKJ_34               Skipjack tuna       1950     Medium 0.346 0.787
#> 6 ANE_34            European anchovy       1950     Medium 0.390 0.886
#>                                                                r_source
#> 1        FishBase prior r, 95% CL (Based on 26 full stock assessments.)
#> 2          FishBase prior r, 95% CL (Based on 1 full stock assessment.)
#> 3 FishBase prior r, 95% CL (Based on 6 data-limited stock assessments.)
#> 4          FishBase prior r, 95% CL (Based on 1 full stock assessment.)
#> 5         FishBase prior r, 95% CL (Based on 9 full stock assessments.)
#> 6        FishBase prior r, 95% CL (Based on 21 full stock assessments.)
table(input$excluded$reason)                      # why the others are left out
#> 
#>                                                       fewer than 10 years 
#>                                                                        31 
#>                                             more than 20% of years filled 
#>                                                                        29 
#> no resilience or r prior: give it in `priors` or set `default_resilience` 
#>                                                                         4 
#>                                                  total catch below 1000 t 
#>                                                                       296 

# Some species only, by English name, scientific name or FAO 3-alpha code
input <- cmsy_input(FAO34$area_data, species = c("European pilchard(=Sardine)", "Sardinella aurita", "GRB"),
                    table = FALSE)
#> 3 stocks ready for CMSY++ (catch only, no abundance data); 0 left out (see $excluded).

# Expert B/k priors (user guide Tables 1 and 3): sardine thought to be depleted in the last year
input <- cmsy_input(FAO34$area_data, species = "PIL", table = FALSE,
                    priors = data.frame(species = "Sardina pilchardus", endb.low = 0.2, endb.hi = 0.5))
#> 1 stocks ready for CMSY++ (catch only, no abundance data); 0 left out (see $excluded).
input$id[, c("Stock", "endb.low", "endb.hi")]
#>    Stock endb.low endb.hi
#> 1 PIL_34      0.2     0.5

# A start year chosen by the user, and a resilience for species without a FishBase prior
input <- cmsy_input(FAO34$area_data, table = FALSE, default_resilience = "Medium",
                    priors = data.frame(alpha3 = "PIL", StartYear = 1973))
#> 124 stocks ready for CMSY++ (catch only, no abundance data); 356 left out (see $excluded).

# One stock per country instead of one per area
by_country <- cmsy_input(FAO34$area_data, species = "PIL", by = "country", table = FALSE)
#> 11 stocks ready for CMSY++ (catch only, no abundance data); 22 left out (see $excluded).
by_country$stocks[, c("Stock", "country", "first_year", "last_year")]
#>         Stock                      country first_year last_year
#> 1  PIL_34_MAR                      Morocco       1950      2024
#> 2  PIL_34_SUN           Un. Sov. Soc. Rep.       1958      1991
#> 3  PIL_34_ESP                        Spain       1950      2024
#> 4  PIL_34_MRT                   Mauritania       1997      2024
#> 5  PIL_34_RUS           Russian Federation       1992      2024
#> 6  PIL_34_UKR                      Ukraine       1992      2017
#> 7  PIL_34_NLD Netherlands (Kingdom of the)       1996      2024
#> 8  PIL_34_BLZ                       Belize       1997      2024
#> 9  PIL_34_LTU                    Lithuania       1992      2024
#> 10 PIL_34_LVA                       Latvia       1992      2016
#> 11 PIL_34_VCT     Saint Vincent/Grenadines       1997      2017
# }
```
