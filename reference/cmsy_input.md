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
  3-alpha code, e.g. `c("Atlantic herring", "Mallotus villosus", "PRA")`
  or Chinese names. Default is NULL, all species.

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
  and `Comment`. Values given here are recorded as "user". A
  `Resilience` given without `r.low` and `r.hi` replaces the r range
  from FishBase or SeaLifeBase with the range of that resilience (CMSY++
  uses `r.low` and `r.hi` whenever both are given). A row applies to the
  stocks of its most specific identifier: only its stock when it has a
  `Stock`, even if it also names the species; else all stocks of its
  species. Rows for a species are applied before rows for one stock, so
  the values given for a stock take precedence.

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
FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
#> Using FAO Global Capture Production release 2026.1.0 (bundled with the package, 1950-2024)
#> 
#> ── fishing_area_data(): Atlantic, Northeast ────────────────────────────────────
#> 142,447 records, 1950-2024; catch in 2024: 7,842,194 t. Periods of the total catch (breakpoints) and their mean catch:
#>  from   to mean_catch_t
#>  1950 1964    7,251,706
#>  1965 2005   10,855,609
#>  2006 2024    8,806,378
#> Results returned: $area_data, $coefs_data, $act_world_map

# All species of the area that CMSY++ can assess with catch only
input <- cmsy_input(FAO27$area_data, table = FALSE)
#> 220 stocks ready for CMSY++ (catch only, no abundance data); 534 left out (see $excluded).
#> 
#> ── cmsy_input(): Atlantic, Northeast ───────────────────────────────────────────
#> 220 stocks ready for CMSY++, 534 left out:
#>   Stock                  species_en first_year last_year Resilience r.low  r.hi
#>  HER_27 Atlantic herring                  1950      2024 Medium     0.288 0.654
#>  COD_27 Atlantic cod                      1950      2024 Medium     0.336 0.764
#>  CAP_27 Capelin                           1950      2024 Medium     0.412 0.937
#>  WHB_27 Blue whiting(=Poutassou)          1950      2024 Medium     0.307 0.698
#>  MAC_27 Atlantic mackerel                 1950      2024 Medium     0.283 0.644
#>  SPR_27 European sprat                    1950      2024 Medium     0.318 0.722
#>  POK_27 Saithe(=Pollock)                  1950      2024 Medium     0.344 0.781
#>  HAD_27 Haddock                           1950      2024 Medium     0.328 0.746
#>  NOP_27 Norway pout                       1959      2024 Medium     0.329 0.747
#>  PIL_27 European pilchard(=Sardine)       1950      2024 Medium     0.415 0.942
#>   ... 210 more rows (220 in all)
#> Results returned: $catch, $id, $stocks, $excluded, $by, $name, $fao_version, $created
input
#> CMSY++ input from FAO capture 2026.1.0: 220 stocks ready, 534 left out
#>   Stock                  species_en                area first_year last_year
#>  HER_27            Atlantic herring Atlantic, Northeast       1950      2024
#>  COD_27                Atlantic cod Atlantic, Northeast       1950      2024
#>  CAP_27                     Capelin Atlantic, Northeast       1950      2024
#>  WHB_27    Blue whiting(=Poutassou) Atlantic, Northeast       1950      2024
#>  MAC_27           Atlantic mackerel Atlantic, Northeast       1950      2024
#>  SPR_27              European sprat Atlantic, Northeast       1950      2024
#>  POK_27            Saithe(=Pollock) Atlantic, Northeast       1950      2024
#>  HAD_27                     Haddock Atlantic, Northeast       1950      2024
#>  NOP_27                 Norway pout Atlantic, Northeast       1959      2024
#>  PIL_27 European pilchard(=Sardine) Atlantic, Northeast       1950      2024
#>  Resilience r.low  r.hi
#>      Medium 0.288 0.654
#>      Medium 0.336 0.764
#>      Medium 0.412 0.937
#>      Medium 0.307 0.698
#>      Medium 0.283 0.644
#>      Medium 0.318 0.722
#>      Medium 0.344 0.781
#>      Medium 0.328 0.746
#>      Medium 0.329 0.747
#>      Medium 0.415 0.942
head(input$stocks[, c("Stock", "species_en", "first_year", "Resilience", "r.low", "r.hi", "r_source")])
#>    Stock               species_en first_year Resilience r.low  r.hi
#> 1 HER_27         Atlantic herring       1950     Medium 0.288 0.654
#> 2 COD_27             Atlantic cod       1950     Medium 0.336 0.764
#> 3 CAP_27                  Capelin       1950     Medium 0.412 0.937
#> 4 WHB_27 Blue whiting(=Poutassou)       1950     Medium 0.307 0.698
#> 5 MAC_27        Atlantic mackerel       1950     Medium 0.283 0.644
#> 6 SPR_27           European sprat       1950     Medium 0.318 0.722
#>                                                         r_source
#> 1 FishBase prior r, 95% CL (Based on 53 full stock assessments.)
#> 2 FishBase prior r, 95% CL (Based on 66 full stock assessments.)
#> 3  FishBase prior r, 95% CL (Based on 5 full stock assessments.)
#> 4  FishBase prior r, 95% CL (Based on 9 full stock assessments.)
#> 5 FishBase prior r, 95% CL (Based on 10 full stock assessments.)
#> 6 FishBase prior r, 95% CL (Based on 17 full stock assessments.)
table(input$excluded$reason)                      # why the others are left out
#> 
#>                                                       fewer than 10 years 
#>                                                                        10 
#>                                             more than 20% of years filled 
#>                                                                        20 
#> no resilience or r prior: give it in `priors` or set `default_resilience` 
#>                                                                        22 
#>                                                  total catch below 1000 t 
#>                                                                       482 

# Some species only, by English name, scientific name or FAO 3-alpha code
input <- cmsy_input(FAO27$area_data, species = c("Atlantic herring", "Mallotus villosus", "PRA"),
                    table = FALSE)
#> 3 stocks ready for CMSY++ (catch only, no abundance data); 0 left out (see $excluded).
#> 
#> ── cmsy_input(): Atlantic, Northeast ───────────────────────────────────────────
#> 3 stocks ready for CMSY++, 0 left out:
#>   Stock       species_en first_year last_year Resilience r.low  r.hi
#>  HER_27 Atlantic herring       1950      2024 Medium     0.288 0.654
#>  CAP_27 Capelin                1950      2024 Medium     0.412 0.937
#>  PRA_27 Northern prawn         1950      2024 Medium     0.408 0.927
#> Results returned: $catch, $id, $stocks, $excluded, $by, $name, $fao_version, $created

# Expert B/k priors (user guide Tables 1 and 3): capelin thought to be depleted in the last year
input <- cmsy_input(FAO27$area_data, species = "CAP", table = FALSE,
                    priors = data.frame(species = "Mallotus villosus", endb.low = 0.2, endb.hi = 0.5))
#> 1 stock ready for CMSY++ (catch only, no abundance data); 0 left out (see $excluded).
#> 
#> ── cmsy_input(): Atlantic, Northeast ───────────────────────────────────────────
#> 1 stocks ready for CMSY++, 0 left out:
#>   Stock species_en first_year last_year Resilience r.low  r.hi
#>  CAP_27 Capelin          1950      2024 Medium     0.412 0.937
#> Results returned: $catch, $id, $stocks, $excluded, $by, $name, $fao_version, $created
input$id[, c("Stock", "endb.low", "endb.hi")]
#>    Stock endb.low endb.hi
#> 1 CAP_27      0.2     0.5

# A start year chosen by the user, and a resilience for species without a FishBase prior
input <- cmsy_input(FAO27$area_data, table = FALSE, default_resilience = "Medium",
                    priors = data.frame(alpha3 = "CAP", StartYear = 1980))
#> 242 stocks ready for CMSY++ (catch only, no abundance data); 512 left out (see $excluded).
#> 
#> ── cmsy_input(): Atlantic, Northeast ───────────────────────────────────────────
#> 242 stocks ready for CMSY++, 512 left out:
#>   Stock                  species_en first_year last_year Resilience r.low  r.hi
#>  HER_27 Atlantic herring                  1950      2024 Medium     0.288 0.654
#>  COD_27 Atlantic cod                      1950      2024 Medium     0.336 0.764
#>  CAP_27 Capelin                           1950      2024 Medium     0.412 0.937
#>  WHB_27 Blue whiting(=Poutassou)          1950      2024 Medium     0.307 0.698
#>  MAC_27 Atlantic mackerel                 1950      2024 Medium     0.283 0.644
#>  SPR_27 European sprat                    1950      2024 Medium     0.318 0.722
#>  POK_27 Saithe(=Pollock)                  1950      2024 Medium     0.344 0.781
#>  HAD_27 Haddock                           1950      2024 Medium     0.328 0.746
#>  NOP_27 Norway pout                       1959      2024 Medium     0.329 0.747
#>  PIL_27 European pilchard(=Sardine)       1950      2024 Medium     0.415 0.942
#>   ... 232 more rows (242 in all)
#> Results returned: $catch, $id, $stocks, $excluded, $by, $name, $fao_version, $created

# One stock per country instead of one per area
by_country <- cmsy_input(FAO27$area_data, species = "CAP", by = "country", table = FALSE)
#> 6 stocks ready for CMSY++ (catch only, no abundance data); 12 left out (see $excluded).
#> 
#> ── cmsy_input(): Atlantic, Northeast ───────────────────────────────────────────
#> 6 stocks ready for CMSY++, 12 left out:
#>       Stock species_en first_year last_year Resilience r.low  r.hi
#>  CAP_27_NOR Capelin          1950      2024 Medium     0.412 0.937
#>  CAP_27_ISL Capelin          1963      2023 Medium     0.412 0.937
#>  CAP_27_SUN Capelin          1950      1991 Medium     0.412 0.937
#>  CAP_27_RUS Capelin          1992      2024 Medium     0.412 0.937
#>  CAP_27_FRO Capelin          1975      2024 Medium     0.412 0.937
#>  CAP_27_GRL Capelin          1974      2024 Medium     0.412 0.937
#> Results returned: $catch, $id, $stocks, $excluded, $by, $name, $fao_version, $created
by_country$stocks[, c("Stock", "country", "first_year", "last_year")]
#>        Stock            country first_year last_year
#> 1 CAP_27_NOR             Norway       1950      2024
#> 2 CAP_27_ISL            Iceland       1963      2023
#> 3 CAP_27_SUN Un. Sov. Soc. Rep.       1950      1991
#> 4 CAP_27_RUS Russian Federation       1992      2024
#> 5 CAP_27_FRO      Faroe Islands       1975      2024
#> 6 CAP_27_GRL          Greenland       1974      2024
# }
```
