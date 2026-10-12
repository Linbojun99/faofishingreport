# Catch and Trade of the Species Groups Mostly Caught

This function compares the catch of one or more countries with their
exports and imports of the same ISSCAAP species groups, by year, from
the FAO capture and trade statistics
([`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
and
[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)).
It shows how much of a country's catch leaves as exports, how much of
the same groups it imports, and the value of this trade.

## Usage

``` r
catch_trade(
  country_name,
  min_capture_pct = 90,
  years = NULL,
  conversion = NULL,
  top_n = 8,
  data = NULL,
  trade = NULL,
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  plot_show = fao_setting("plot_show"),
  label = NULL,
  data_version = "latest"
)
```

## Arguments

- country_name:

  One or more countries, as named in the FAO data (e.g. "Norway").

- min_capture_pct:

  A group is compared when capture made up at least this percentage of
  its world production in every year analysed. Default is 90.

- years:

  The years. Default is NULL, all years with both catch and trade data
  (from 1976).

- conversion:

  NULL (default), or a data frame of conversion factors to live weight
  with the columns `commodity_code` (ISSCFC code, as `Commodity..Code.`
  of
  [`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md))
  and `factor` (live weight per product weight).

- top_n:

  The number of groups shown in the chart by group. Default is 8.

- data:

  Catch data in the format of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is NULL, which loads `load_fao_data(data_version)`.

- trade:

  Trade data from
  [`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md).
  Default is NULL, which loads it.

- plot:

  A logical value. If TRUE (default), charts are saved in
  `figures/png/summary/`.

- table:

  A logical value. If TRUE (default), tables are saved in `tables/`.

- lang:

  The language of the chart text and names: "en" (English), "zh"
  (Chinese) or "both" (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  ("en" unless changed).

- pptx:

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in `figures/pptx/summary/`. Default is FALSE, which can be
  changed for all functions with `fao_settings(pptx = TRUE)`.

- plot_show:

  A logical value. If TRUE, each chart is also drawn in the plot window
  (e.g. the Plots pane of RStudio) as it is saved, to look at it at
  once. Default is FALSE, which can be changed for all functions with
  `fao_settings(plot_show = TRUE)`. The charts are saved as files
  (`plot = TRUE`) either way.

- label:

  The name used in the output file names. Default is the country, or
  "selected_countries" for several countries.

- data_version:

  The FAO data release of the catch data; see
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).

## Value

Invisibly, a list of three data frames:

- yearly:

  one row per country and year, for the groups compared: catch (tonnes
  live weight), exports, imports and re-exports (tonnes product weight),
  export and import value (thousand USD), unit values (USD per kg),
  exports as a percentage of the catch, net exports, the share of the
  country's catch and export value in the groups compared, the share of
  the export value estimated, and the trade of fishmeal and fish oil not
  assigned to a group compared. With `conversion`, also the trade in
  live weight equivalent, the share converted and the apparent supply;

- by_group:

  the same by country, group and year. In `yearly` and `by_group`, a
  value is NA when it is not known: the country reported no catch (or no
  trade) in that year, or the values are missing in the FAO data; it is
  zero when the country reported that year but not this group or trade
  flow;

- groups:

  one row per country and ISSCAAP group with catch or exports in the
  last five years: whether it is compared, the lowest share of capture
  in its world production, the mean yearly catch, exports and export
  value of the last five years, and the group's share of the export
  value (`export_value_pct`).

Charts are saved in `figures/png/summary/`: `<label>_catch_trade.png`
(catch, exports and imports by year, one panel per country) and
`<label>_catch_trade_groups.png` (the same for the `top_n` groups; with
several countries, one file per country ending in the country name).
Tables are saved in `tables/`: `<label>_catch_trade.csv`,
`..._by_group.csv` and `..._groups.csv`.

## Which species groups are compared

Trade statistics do not tell whether a product was caught or farmed, so
the trade of a group that is largely farmed (salmons, shrimps, carps,
oysters, ...) cannot be compared with its catch (Golden et al. 2017;
Guillen et al. 2019). The function therefore compares only the groups
mostly caught: capture made up at least `min_capture_pct` percent of the
world production of the group in every year analysed (see
[`fao_capture_groups()`](https://linbojun99.github.io/faofishingreport/reference/fao_capture_groups.md)).
With the default 90 percent and 1976-2024, these are the cods and hakes,
herrings and sardines, tunas, miscellaneous pelagic and demersal fishes,
sharks and rays, shads, lobsters, king crabs, krill, miscellaneous
marine crustaceans, squids and octopuses, and horseshoe crabs, which
held about a third of the value of world exports in 2024. The table
`groups` lists all groups of each country with their catch and exports,
so the groups left out can be seen.

The catch is the country's whole catch (all FAO areas, inland and
marine), because trade is recorded for the country as a whole. Catch is
recorded by the flag of the vessel, and catch landed abroad by a
country's vessels counts as an export of that country, so both refer to
the same fleet.

## Product weight and live weight

Trade quantities are product weight: a tonne of fillets comes from two
to three tonnes of fish, a tonne of fishmeal from four to five. FAO
publishes no conversion factors with the trade data; they are collected
from countries and published in FAO Fisheries Circular 847 (FAO 2000),
and factors for highly processed products are very uncertain (CWP
handbook). Published studies use factors of the European Market
Observatory for Fisheries and Aquaculture Products (EUMOFA) or others
(Gephart et al. 2024; Zhao et al. 2024). Without `conversion`, the
function compares product weight of trade with live weight of catch:
`export_catch_pct` is then the exports in product weight as a percentage
of the catch in live weight, usually lower than the share of the catch
exported. With `conversion`, a table of factors by commodity, the trade
is converted to live weight equivalent (commodities without a factor
stay in product weight; `converted_pct` gives the share of the exports,
imports and re-exports with a factor), and the apparent supply, catch +
imports - exports - re-exports, is calculated as in food balance sheets
(CWP handbook; Zhao et al. 2024).

## Fishmeal and fish oil

FAO assigns fishmeal and fish oil to a species group when the trade
records give the species (e.g. anchoveta meal, herring oil), but most
are traded as "Fishmeals, nei" or "Fish body oils, nei" and classed as
"Marine fishes not identified", which is not compared. They are made
mostly from small pelagic fishes and from fish waste, so the catch of a
reduction fishery (e.g. the Peruvian anchoveta) appears without its main
export. Fishmeal, fish oil, solubles and silages (ISSCFC 081, 411 and
035.01) not assigned to a group compared are reported separately as
`meal_oil_export_tonnes` and `meal_oil_import_tonnes` (product weight)
and `meal_oil_export_value`.

## Reading the results

- Exports above the catch of a group (`exports_exceed_catch`) mean that
  the country processes and re-exports imported fish, that re-exports
  are not reported separately (26 countries reported them in 2024, not
  China), that the product is classed in another group than the catch
  (e.g. crustaceans traded as "Crustaceans nei", group 47), or that the
  data disagree (Asche et al. 2022; Dent and Clarke 2015).

- Exports and imports of the same group are common: different species,
  sizes or products within a group, and processing of imported raw
  material.

- Unit values (`export_unit_value`, USD per kg product weight) are in
  current US dollars, not adjusted for inflation, and change with the
  mix of products.

- `estimated_export_value_pct` is the share of the export value
  estimated (by FAO or the country) or taken from the reports of trade
  partners.

## References

Asche, F., Yang, B., Gephart, J.A., Smith, M.D., Anderson, J.L., Camp,
E.V., Garlock, T.M., Love, D.C., Oglend, A. and Straume, H.-M. (2022).
China's seafood imports - Not for domestic consumption? Science 375:
386-388.
[doi:10.1126/science.abl4756](https://doi.org/10.1126/science.abl4756)

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: conversion factors; food balance sheets.
<https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/conversionfactors/en/>

Dent, F. and Clarke, S. (2015). State of the global market for shark
products. FAO Fisheries and Aquaculture Technical Paper 590. Rome: FAO.
<https://www.fao.org/3/i4795e/i4795e.pdf>

FAO (2000). Conversion factors - landed weight to live weight. FAO
Fisheries Circular 847, Revision 1. Rome: FAO.
<https://www.fao.org/3/x9144t/x9144t.pdf>

FAO (2021). FAO Yearbook. Fishery and Aquaculture Statistics 2019. Rome:
FAO. [doi:10.4060/cb7874t](https://doi.org/10.4060/cb7874t)

Gephart, J.A., Agrawal Bejarano, R., Gorospe, K., Godwin, A., Golden,
C.D., Naylor, R.L., et al. (2024). Globalization of wild capture and
farmed aquatic foods. Nature Communications 15: 8026.
[doi:10.1038/s41467-024-51965-8](https://doi.org/10.1038/s41467-024-51965-8)

Golden, C.D., Seto, K.L., Dey, M.M., Chen, O.L., Gephart, J.A., Myers,
S.S., Smith, M., et al. (2017). Does aquaculture support the needs of
nutritionally vulnerable nations? Frontiers in Marine Science 4: 159.
[doi:10.3389/fmars.2017.00159](https://doi.org/10.3389/fmars.2017.00159)

Guillen, J., Natale, F., Carvalho, N., Casey, J., Hofherr, J., Druon,
J.-N., Fiore, G., Gibin, M., Zanzi, A. and Martinsohn, J.T. (2019).
Global seafood consumption footprint. Ambio 48: 111-122.
[doi:10.1007/s13280-018-1060-9](https://doi.org/10.1007/s13280-018-1060-9)

Zhao, K., Gaines, S.D., García Molinos, J., Zhang, M. and Xu, J. (2024).
Effect of trade on global aquatic food consumption patterns. Nature
Communications 15: 1412.
[doi:10.1038/s41467-024-45556-w](https://doi.org/10.1038/s41467-024-45556-w)

## Examples

``` r
if (FALSE) { # \dontrun{
trade <- load_fao_trade()
norway <- catch_trade("Norway", trade = trade, lang = "zh")
norway$yearly
norway$groups                                   # the groups compared and those left out

# Several countries, one panel each
catch_trade(c("Peru", "Chile", "Morocco"), trade = trade, label = "fishmeal_countries")

# Groups at least 80 % caught (adds flounders, crabs, unidentified marine fishes, ...)
catch_trade("Iceland", min_capture_pct = 80, trade = trade)

# With your own conversion factors to live weight (e.g. from EUMOFA or FAO Circular 847)
factors <- data.frame(commodity_code = c("034.1.5.2.19", "034.4.1.5.2.19"), factor = c(1.17, 2.6))
catch_trade("Norway", trade = trade, conversion = factors)
} # }
```
