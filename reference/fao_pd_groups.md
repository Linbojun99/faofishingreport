# Pelagic and Demersal Species Groups

This function lists the ISSCAAP species groups of marine fishes and
crustaceans, and how each is counted by the two definitions of
[`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md):
"pelagic", "demersal" or "not counted". ISSCAAP groups not listed
(molluscs, diadromous and freshwater fishes, mammals, plants, other
invertebrates) are not counted by either definition.

## Usage

``` r
fao_pd_groups()
```

## Value

A data frame with one row per group: `isscaap_code`, `isscaap_en` (the
name used in the FAO data), `de_leiva_moreno` and `grainger_garcia`.

## References

Caddy, J.F. (1993). Toward a comparative evaluation of human impacts on
fishery ecosystems of enclosed and semi-enclosed seas. Reviews in
Fisheries Science 1: 57-95.
[doi:10.1080/10641269309388535](https://doi.org/10.1080/10641269309388535)

Caddy, J.F., Carocci, F. and Coppola, S. (1998). Have peak fishery
production levels been passed in continental shelf area? Some
perspectives arising from historical trends in production per shelf
area. Journal of Northwest Atlantic Fishery Science 23: 191-219.
[doi:10.2960/J.v23.a13](https://doi.org/10.2960/J.v23.a13)

de Leiva Moreno, J.I., Agostini, V.N., Caddy, J.F. and Carocci, F.
(2000). Is the pelagic-demersal ratio from fishery landings a useful
proxy for nutrient availability? A preliminary data exploration for the
semi-enclosed seas around Europe. ICES Journal of Marine Science 57:
1091-1102.
[doi:10.1006/jmsc.2000.0705](https://doi.org/10.1006/jmsc.2000.0705)

Grainger, R.J.R. and Garcia, S.M. (1996). Chronicles of marine fishery
landings (1950-1994): trend analysis and fisheries potential. FAO
Fisheries Technical Paper 359. Rome: FAO.
<https://www.fao.org/4/w3244e/w3244e00.htm>

Hondorp, D.W., Breitburg, D.L. and Davias, L.A. (2010). Eutrophication
and fisheries: separating the effects of nitrogen loads and hypoxia on
the pelagic-to-demersal ratio and other measures of landings
composition. Marine and Coastal Fisheries 2: 339-361.
[doi:10.1577/C09-020.1](https://doi.org/10.1577/C09-020.1)

## Examples

``` r
fao_pd_groups()
#>    isscaap_code                       isscaap_en de_leiva_moreno
#> 1            31       Flounders, halibuts, soles        demersal
#> 2            32            Cods, hakes, haddocks        demersal
#> 3            33     Miscellaneous coastal fishes        demersal
#> 4            34    Miscellaneous demersal fishes        demersal
#> 5            35    Herrings, sardines, anchovies         pelagic
#> 6            36       Tunas, bonitos, billfishes     not counted
#> 7            37     Miscellaneous pelagic fishes     not counted
#> 8            38          Sharks, rays, chimaeras     not counted
#> 9            39     Marine fishes not identified     not counted
#> 10           42               Crabs, sea-spiders        demersal
#> 11           43    Lobsters, spiny-rock lobsters        demersal
#> 12           44       King crabs, squat-lobsters        demersal
#> 13           45                  Shrimps, prawns        demersal
#> 14           46    Krill, planktonic crustaceans     not counted
#> 15           47 Miscellaneous marine crustaceans        demersal
#>    grainger_garcia
#> 1         demersal
#> 2         demersal
#> 3         demersal
#> 4         demersal
#> 5          pelagic
#> 6          pelagic
#> 7          pelagic
#> 8         demersal
#> 9         demersal
#> 10     not counted
#> 11     not counted
#> 12     not counted
#> 13     not counted
#> 14     not counted
#> 15     not counted
```
