# catch_trade(): 以捕捞为主的物种类群的渔获量与贸易

英文原页：[`catch_trade()`](https://linbojun99.github.io/faofishingreport/reference/catch_trade.md)

## 说明

该函数根据FAO捕捞产量统计和贸易统计（[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)和[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)），逐年比较一个或多个国家的渔获量与其同一批ISSCAAP物种类群的出口和进口。它显示一国的渔获量有多少以出口形式流出、该国进口了多少同一批类群的产品，以及这些贸易的价值。

English

This function compares the catch of one or more countries with their
exports and imports of the same ISSCAAP species groups, by year, from
the FAO capture and trade statistics
([`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
and
[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)).
It shows how much of a country’s catch leaves as exports, how much of
the same groups it imports, and the value of this trade.

## 用法

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
  label = NULL,
  data_version = "latest"
)
```

## 参数

- `country_name`:

  一个或多个国家，名称与FAO数据中的一致（例如”Norway”）。

  English

  One or more countries, as named in the FAO data (e.g. “Norway”).

- `min_capture_pct`:

  在分析的每一年中，捕捞至少占类群全球产量的这一百分比时，该类群才参与比较。默认为90。

  English

  A group is compared when capture made up at least this percentage of
  its world production in every year analysed. Default is 90.

- `years`:

  所用的年份。默认为NULL，即渔获数据和贸易数据都有的全部年份（从1976年起）。

  English

  The years. Default is NULL, all years with both catch and trade data
  (from 1976).

- `conversion`:

  NULL（默认），或一个折算为活重的系数数据框，包含两列：`commodity_code`（ISSCFC商品代码，与[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)的`Commodity..Code.`相同）和`factor`（活重与产品重量之比）。

  English

  NULL (default), or a data frame of conversion factors to live weight
  with the columns `commodity_code` (ISSCFC code, as `Commodity..Code.`
  of
  [`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md))
  and `factor` (live weight per product weight).

- `top_n`:

  按类群的图表中显示的类群数。默认为8。

  English

  The number of groups shown in the chart by group. Default is 8.

- `data`:

  渔获数据，格式同[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)。默认为NULL，即读取`load_fao_data(data_version)`。

  English

  Catch data in the format of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).
  Default is NULL, which loads `load_fao_data(data_version)`.

- `trade`:

  贸易数据，来自[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)。默认为NULL，即用[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)读取。

  English

  Trade data from
  [`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md).
  Default is NULL, which loads it.

- `plot`:

  逻辑值。为TRUE（默认）时，图表保存在`figures/png/summary/`。

  English

  A logical value. If TRUE (default), charts are saved in
  `figures/png/summary/`.

- `table`:

  逻辑值。为TRUE（默认）时，表格保存在`tables/`。

  English

  A logical value. If TRUE (default), tables are saved in `tables/`.

- `lang`:

  图表文字和名称的语言：“en”（英文）、“zh”（中文）或”both”（中英双语）。默认为NULL，即使用[`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)设置的语言（未更改时为”en”）。

  English

  The language of the chart text and names: “en” (English), “zh”
  (Chinese) or “both” (Chinese and English). Default is NULL, which uses
  the language set with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  (“en” unless changed).

- `pptx`:

  逻辑值。为TRUE时，图表还另存为可编辑的PowerPoint文件，保存在`figures/pptx/summary/`。默认为FALSE，可以用`fao_settings(pptx = TRUE)`为所有函数统一更改。

  English

  A logical value. If TRUE, charts are also saved as editable PowerPoint
  files in `figures/pptx/summary/`. Default is FALSE, which can be
  changed for all functions with `fao_settings(pptx = TRUE)`.

- `label`:

  输出文件名中使用的名称。默认为国家名称；有多个国家时为”selected_countries”。

  English

  The name used in the output file names. Default is the country, or
  “selected_countries” for several countries.

- `data_version`:

  渔获数据所用的FAO数据版本；见[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)。

  English

  The FAO data release of the catch data; see
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md).

## 返回值

以不可见方式返回一个包含三个数据框的列表：

- **yearly**：所比较的类群合计，每个国家每年一行：渔获量（活重吨），出口、进口和转口量（产品重量吨），出口额和进口额（千美元），单位价值（美元/千克），出口量占渔获量的百分比，净出口量，所比较的类群在该国渔获量和出口额中所占的比例，出口额中估算部分所占的比例，以及没有归入所比较类群的鱼粉和鱼油的贸易。使用`conversion`时，还包括折算为活重当量的贸易量、已折算部分所占的比例和表观供给量；
- **by_group**：同样的内容，按国家、类群和年份。在`yearly`和`by_group`中，数值未知时为NA：该国当年没有报告渔获量（或贸易），或FAO数据中的数值缺失；该国当年有报告、但没有这一类群或贸易流向时为零；
- **groups**：每个国家在最近五年中有渔获或出口的每个ISSCAAP类群一行：是否参与比较，捕捞在该类群全球产量中所占比例的最低值，最近五年的年均渔获量、出口量和出口额，以及该类群占出口额的比例（`export_value_pct`）。

图表保存在`figures/png/summary/`：`<label>_catch_trade.png`（逐年的渔获量、出口和进口，每个国家一个分图）和`<label>_catch_trade_groups.png`（同样的内容，针对前`top_n`个类群；有多个国家时，每个国家一个文件，文件名以国家名结尾）。表格保存在`tables/`：`<label>_catch_trade.csv`、`..._by_group.csv`和`..._groups.csv`。

English

Invisibly, a list of three data frames:

- **yearly**: one row per country and year, for the groups compared:
  catch (tonnes live weight), exports, imports and re-exports (tonnes
  product weight), export and import value (thousand USD), unit values
  (USD per kg), exports as a percentage of the catch, net exports, the
  share of the country’s catch and export value in the groups compared,
  the share of the export value estimated, and the trade of fishmeal and
  fish oil not assigned to a group compared. With `conversion`, also the
  trade in live weight equivalent, the share converted and the apparent
  supply;
- **by_group**: the same by country, group and year. In `yearly` and
  `by_group`, a value is NA when it is not known: the country reported
  no catch (or no trade) in that year, or the values are missing in the
  FAO data; it is zero when the country reported that year but not this
  group or trade flow;
- **groups**: one row per country and ISSCAAP group with catch or
  exports in the last five years: whether it is compared, the lowest
  share of capture in its world production, the mean yearly catch,
  exports and export value of the last five years, and the group’s share
  of the export value (`export_value_pct`).

Charts are saved in `figures/png/summary/`: `<label>_catch_trade.png`
(catch, exports and imports by year, one panel per country) and
`<label>_catch_trade_groups.png` (the same for the `top_n` groups; with
several countries, one file per country ending in the country name).
Tables are saved in `tables/`: `<label>_catch_trade.csv`,
`..._by_group.csv` and `..._groups.csv`.

## 参与比较的物种类群

贸易统计无法区分产品来自捕捞还是养殖，因此大部分来自养殖的类群（鲑鳟类、虾类、鲤类、牡蛎类……）的贸易无法与其渔获量比较（Golden等，2017；Guillen等，2019）。因此本函数只比较以捕捞为主的类群：在分析的每一年中，捕捞至少占该类群全球产量的`min_capture_pct`%（见[`fao_capture_groups()`](https://linbojun99.github.io/faofishingreport/reference/fao_capture_groups.md)）。按默认的90%和1976–2024年，这些类群是鳕和无须鳕类、鲱和沙丁鱼类、金枪鱼类、其他中上层鱼类、其他底层鱼类、鲨鳐类、西鲱类、龙虾类、帝王蟹类、磷虾类、其他海洋甲壳类、鱿鱼和章鱼类，以及鲎类，它们在2024年约占全球出口额的三分之一。`groups`表列出每个国家的全部类群及其渔获量和出口量，因此可以看到哪些类群没有参与比较。

渔获量是该国的全部渔获量（所有FAO渔区，包括内陆和海洋），因为贸易是按整个国家记录的。渔获量按渔船的船旗国记录，而一国渔船在国外上岸的渔获计为该国的出口，因此两者针对的是同一支船队。

English

Trade statistics do not tell whether a product was caught or farmed, so
the trade of a group that is largely farmed (salmons, shrimps, carps,
oysters, …) cannot be compared with its catch (Golden et al. 2017;
Guillen et al. 2019). The function therefore compares only the groups
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

The catch is the country’s whole catch (all FAO areas, inland and
marine), because trade is recorded for the country as a whole. Catch is
recorded by the flag of the vessel, and catch landed abroad by a
country’s vessels counts as an export of that country, so both refer to
the same fleet.

## 产品重量与活重

贸易数量是产品重量：1吨鱼片来自2～3吨鱼，1吨鱼粉来自4～5吨鱼。FAO不随贸易数据发布折算系数；折算系数由各国提供，收录在FAO渔业通报第847号（FAO，2000）中，而深加工产品的系数很不确定（CWP手册）。已发表的研究使用欧洲渔业和水产养殖产品市场观察站（EUMOFA）或其他来源的系数（Gephart等，2024；Zhao等，2024）。不提供`conversion`时，本函数用贸易的产品重量与渔获的活重比较：此时`export_catch_pct`是按产品重量计的出口量占按活重计的渔获量的百分比，通常低于渔获量中实际出口的比例。提供`conversion`（按商品给出的折算系数表）时，贸易量折算为活重当量（没有系数的商品仍按产品重量计；`converted_pct`给出出口、进口和转口中有折算系数的部分所占的比例），并按食物平衡表的方法计算表观供给量，即渔获量 +
进口 − 出口 − 转口（CWP手册；Zhao等，2024）。

English

Trade quantities are product weight: a tonne of fillets comes from two
to three tonnes of fish, a tonne of fishmeal from four to five. FAO
publishes no conversion factors with the trade data; they are collected
from countries and published in FAO Fisheries Circular 847 (FAO 2000),
and factors for highly processed products are very uncertain (CWP
handbook). Published studies use factors of the European Market
Observatory for Fisheries and Aquaculture Products (EUMOFA) or others
(Gephart et al. 2024; Zhao et al. 2024). Without `conversion`, the
function compares product weight of trade with live weight of catch:
`export_catch_pct` is then the exports in product weight as a percentage
of the catch in live weight, usually lower than the share of the catch
exported. With `conversion`, a table of factors by commodity, the trade
is converted to live weight equivalent (commodities without a factor
stay in product weight; `converted_pct` gives the share of the exports,
imports and re-exports with a factor), and the apparent supply, catch +
imports - exports - re-exports, is calculated as in food balance sheets
(CWP handbook; Zhao et al. 2024).

## 鱼粉和鱼油

贸易记录给出物种时（例如秘鲁鳀鱼粉、鲱鱼油），FAO把鱼粉和鱼油归入相应的物种类群；但大部分鱼粉和鱼油以”Fishmeals,
nei”或”Fish body oils, nei”交易，归入不参与比较的”Marine fishes not
identified”（未鉴定的海洋鱼类）。鱼粉和鱼油主要用小型中上层鱼类和鱼类加工废料制成，因此用于加工鱼粉鱼油的渔业（例如秘鲁鳀渔业），其渔获量出现在结果中，而其主要的出口产品却不在其中。没有归入所比较类群的鱼粉、鱼油、鱼溶浆和鱼青贮饲料（ISSCFC
081、411和035.01）单独列为`meal_oil_export_tonnes`和`meal_oil_import_tonnes`（产品重量）以及`meal_oil_export_value`。

English

FAO assigns fishmeal and fish oil to a species group when the trade
records give the species (e.g. anchoveta meal, herring oil), but most
are traded as “Fishmeals, nei” or “Fish body oils, nei” and classed as
“Marine fishes not identified”, which is not compared. They are made
mostly from small pelagic fishes and from fish waste, so the catch of a
reduction fishery (e.g. the Peruvian anchoveta) appears without its main
export. Fishmeal, fish oil, solubles and silages (ISSCFC 081, 411 and
035.01) not assigned to a group compared are reported separately as
`meal_oil_export_tonnes` and `meal_oil_import_tonnes` (product weight)
and `meal_oil_export_value`.

## 结果解读

- 某个类群的出口量高于渔获量（`exports_exceed_catch`）意味着：该国加工进口的鱼后再出口；转口没有单独报告（2024年有26个国家单独报告转口，不含中国）；产品归入了与渔获不同的类群（例如甲壳类以”Crustaceans
  nei”交易，归入类群47）；或者数据之间有出入（Asche等，2022；Dent和Clarke，2015）。
- 同一类群既有出口又有进口很常见：类群内的物种、规格或产品不同，以及进口原料的加工。
- 单位价值（`export_unit_value`，美元/千克产品重量）按当年美元计，未经通胀调整，并随产品结构而变化。
- `estimated_export_value_pct`是出口额中估算（由FAO或该国估算）或取自贸易伙伴报告的部分所占的比例。

English

- Exports above the catch of a group (`exports_exceed_catch`) mean that
  the country processes and re-exports imported fish, that re-exports
  are not reported separately (26 countries reported them in 2024, not
  China), that the product is classed in another group than the catch
  (e.g. crustaceans traded as “Crustaceans nei”, group 47), or that the
  data disagree (Asche et al. 2022; Dent and Clarke 2015).
- Exports and imports of the same group are common: different species,
  sizes or products within a group, and processing of imported raw
  material.
- Unit values (`export_unit_value`, USD per kg product weight) are in
  current US dollars, not adjusted for inflation, and change with the
  mix of products.
- `estimated_export_value_pct` is the share of the export value
  estimated (by FAO or the country) or taken from the reports of trade
  partners.

## 参考文献

Asche, F., Yang, B., Gephart, J.A., Smith, M.D., Anderson, J.L., Camp,
E.V., Garlock, T.M., Love, D.C., Oglend, A. and Straume, H.-M. (2022).
China’s seafood imports - Not for domestic consumption? Science 375:
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

## 示例

``` r

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
```
