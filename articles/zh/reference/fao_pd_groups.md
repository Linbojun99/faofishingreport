# fao_pd_groups(): 中上层与底层物种类群

英文原页：[`fao_pd_groups()`](https://linbojun99.github.io/faofishingreport/reference/fao_pd_groups.md)

## 说明

本函数列出海洋鱼类和甲壳类的ISSCAAP物种类群，以及[`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md)的两种定义对每个类群的计法：“pelagic”（中上层）、“demersal”（底层）或”not
counted”（不计入）。未列出的ISSCAAP类群（软体动物、洄游性鱼类和淡水鱼类、哺乳动物、植物、其他无脊椎动物）在两种定义中都不计入。

English

This function lists the ISSCAAP species groups of marine fishes and
crustaceans, and how each is counted by the two definitions of
[`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md):
“pelagic”, “demersal” or “not counted”. ISSCAAP groups not listed
(molluscs, diadromous and freshwater fishes, mammals, plants, other
invertebrates) are not counted by either definition.

## 用法

``` r

fao_pd_groups()
```

## 返回值

一个数据框，每个类群一行：`isscaap_code`、`isscaap_en`（FAO数据中使用的名称）、`de_leiva_moreno`和`grainger_garcia`。

English

A data frame with one row per group: `isscaap_code`, `isscaap_en` (the
name used in the FAO data), `de_leiva_moreno` and `grainger_garcia`.

## 参考文献

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

## 示例

``` r

fao_pd_groups()
```
