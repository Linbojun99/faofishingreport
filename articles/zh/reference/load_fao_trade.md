# load_fao_trade(): 水产品贸易：FAO数据

英文原页：[`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)

## 说明

该函数返回FAO水产品国际贸易统计（“Global aquatic trade - by
species/commodity”，FI_Trade）：1976年以来各国按商品和年份统计的出口、进口和转口的数量和价值。国际渔业商品标准统计分类（ISSCFC）中的每种商品都属于一个ISSCAAP物种类群，因此贸易可以与同一类群的渔获量比较（见[`catch_trade()`](https://linbojun99.github.io/faofishingreport/reference/catch_trade.md)）。

English

This function returns the FAO statistics of the international trade of
aquatic products (“Global aquatic trade - by species/commodity”,
FI_Trade): the quantity and value of the exports, imports and re-exports
of each country by commodity and year, from 1976. Each commodity of the
International Standard Statistical Classification of Fishery Commodities
(ISSCFC) belongs to one ISSCAAP species group, so the trade can be
compared with the catch of the same groups (see
[`catch_trade()`](https://linbojun99.github.io/faofishingreport/reference/catch_trade.md)).

## 用法

``` r

load_fao_trade(version = "latest", data_dir = fao_data_dir())
```

## 参数

- `version`:

  所用的版本，如”2026.1.0”。默认为”latest”（最新版本）。

  English

  The release, e.g. “2026.1.0”. Default is “latest”.

- `data_dir`:

  数据的保存目录。默认为[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)。

  English

  The directory where the data is saved. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## 返回值

一个数据框，每个国家、商品、贸易流向和年份一行：`Country..Name.`（与[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)中的相同）、`Commodity..Code.`（ISSCFC）、`Commodity..Name.`、`ISSCAAP.group..Code.`、`ISSCAAP.group..Name.`（与渔获数据中的物种类群相同）、`Trade.flow`（“Exports”、“Imports”或”Re-exports”，即出口、进口或转口）、`Year`、`Quantity`（吨，产品重量）、`Value`（千美元），以及`Quantity.Type`和`Value.Type`（“actual”、“estimate”、“partner
data”等）。属性`fao_version`和`source_url`给出所用的版本。

English

A data frame with one row per country, commodity, flow and year:
`Country..Name.` (as in
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)),
`Commodity..Code.` (ISSCFC), `Commodity..Name.`, `ISSCAAP.group..Code.`,
`ISSCAAP.group..Name.` (as in the species groups of the catch data),
`Trade.flow` (“Exports”, “Imports” or “Re-exports”), `Year`, `Quantity`
(tonnes, product weight), `Value` (thousand USD), `Quantity.Type` and
`Value.Type` (“actual”, “estimate”, “partner data”, …). The attributes
`fao_version` and `source_url` give the release.

## 数据说明

FAO根据联合国统计司和欧盟统计局的海关统计汇编这些数据，并对应到ISSCFC；未报告的国家用其贸易伙伴的报告补齐（FAO该版本的元数据）。在2026.1.0版本中：

- 225个报告国，1976–2024年，实际使用约1,200种商品。
- 数量为以吨计的净**产品重量**（鱼片、冻鱼、鱼粉……的重量），不是渔获的活重；FAO不随数据发布折算系数。
- 价值以千美元计，出口一般按离岸价（FOB），进口按到岸价（成本、保险加运费，CIF）。
- 一国渔船在外国港口上岸的渔获计为该国的出口。
- 商品无法区分产品来自捕捞还是养殖。
- 很少有国家单独报告转口（2024年为26个，不含中国），因此大部分转口计入出口。
- 未报告国家的数值取自其贸易伙伴的报告时，状态显示为”partner
  data”（从2019年起；2019年以前这类数值显示为”estimate”）。

国家之间（按贸易伙伴）的贸易数据只从2019年起发布，位于单独的文件（FI_Trade_Partners）中，本函数不读取。

联网时使用最新版本：第一次使用时下载（16
MB的文件）并转换（约需一分钟），保存在`data_dir`中；离线时，或设置了`fao_settings(online = FALSE)`时，使用已保存的最新版本。

English

FAO compiles the data from the customs statistics of the United Nations
Statistics Division and Eurostat, mapped to the ISSCFC; countries that
do not report are filled with the reports of their trade partners (FAO
metadata of the release). In release 2026.1.0:

- 225 reporting countries, 1976-2024, about 1,200 commodities in use.
- The quantity is the net **product weight** in tonnes (the weight of
  fillets, frozen fish, fishmeal, …), not the live weight of the catch;
  FAO publishes no conversion factors with the data.
- The value is in thousand US dollars, exports generally free on board
  (FOB) and imports with cost, insurance and freight (CIF).
- Catch of a country’s vessels landed in foreign ports counts as an
  export of that country.
- Commodities do not tell whether the product was caught or farmed.
- Few countries report re-exports separately (26 in 2024, not China), so
  most re-exports are counted as exports.
- The status shows “partner data” for values of non-reporting countries
  taken from the reports of their partners (from 2019; such values are
  shown as “estimate” before 2019).

Trade between countries (by partner) is published only from 2019, in a
separate file (FI_Trade_Partners), not read by this function.

When the computer is online, the newest release is used, downloaded and
converted the first time (a 16 MB file; converting takes a minute) and
saved in `data_dir`; offline, or with `fao_settings(online = FALSE)`,
the newest saved release is used.

## 参考文献

FAO. Global aquatic trade statistics (FI_Trade), metadata of the
release, and the ISSCFC and ISSCAAP classifications.
<https://www.fao.org/fishery/static/Data/>

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishery commodities classification (ISSCFC).
<https://www.fao.org/cwp-on-fishery-statistics/handbook/socio-economic-dimension/fishery-commodities-classification/en/>

## 示例

``` r

trade <- load_fao_trade()
norway <- subset(trade, Country..Name. == "Norway" & Trade.flow == "Exports" & Year == 2024)
head(norway[order(-norway$Value), c("Commodity..Name.", "Quantity", "Value")])
catch_trade("Norway", trade = trade)
```
