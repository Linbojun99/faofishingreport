# Trade of Aquatic Products: FAO Data

This function returns the FAO statistics of the international trade of
aquatic products ("Global aquatic trade - by species/commodity",
FI_Trade): the quantity and value of the exports, imports and re-exports
of each country by commodity and year, from 1976. Each commodity of the
International Standard Statistical Classification of Fishery Commodities
(ISSCFC) belongs to one ISSCAAP species group, so the trade can be
compared with the catch of the same groups (see
[`catch_trade()`](https://linbojun99.github.io/faofishingreport/reference/catch_trade.md)).

## Usage

``` r
load_fao_trade(version = "latest", data_dir = fao_data_dir())
```

## Arguments

- version:

  The release, e.g. "2026.1.0". Default is "latest".

- data_dir:

  The directory where the data is saved. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## Value

A data frame with one row per country, commodity, flow and year:
`Country..Name.` (as in
[`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)),
`Commodity..Code.` (ISSCFC), `Commodity..Name.`, `ISSCAAP.group..Code.`,
`ISSCAAP.group..Name.` (as in the species groups of the catch data),
`Trade.flow` ("Exports", "Imports" or "Re-exports"), `Year`, `Quantity`
(tonnes, product weight), `Value` (thousand USD), `Quantity.Type` and
`Value.Type` ("actual", "estimate", "partner data", ...). The attributes
`fao_version` and `source_url` give the release.

## What the data are

FAO compiles the data from the customs statistics of the United Nations
Statistics Division and Eurostat, mapped to the ISSCFC; countries that
do not report are filled with the reports of their trade partners (FAO
metadata of the release). In release 2026.1.0:

- 225 reporting countries, 1976-2024, about 1,200 commodities in use.

- The quantity is the net **product weight** in tonnes (the weight of
  fillets, frozen fish, fishmeal, ...), not the live weight of the
  catch; FAO publishes no conversion factors with the data.

- The value is in thousand US dollars, exports generally free on board
  (FOB) and imports with cost, insurance and freight (CIF).

- Catch of a country's vessels landed in foreign ports counts as an
  export of that country.

- Commodities do not tell whether the product was caught or farmed.

- Few countries report re-exports separately (26 in 2024, not China), so
  most re-exports are counted as exports.

- The status shows "partner data" for values of non-reporting countries
  taken from the reports of their partners (from 2019; such values are
  shown as "estimate" before 2019).

Trade between countries (by partner) is published only from 2019, in a
separate file (FI_Trade_Partners), not read by this function.

When the computer is online, the newest release is used, downloaded and
converted the first time (a 16 MB file; converting takes a minute) and
saved in `data_dir`; offline, or with `fao_settings(online = FALSE)`,
the newest saved release is used.

## References

FAO. Global aquatic trade statistics (FI_Trade), metadata of the
release, and the ISSCFC and ISSCAAP classifications.
<https://www.fao.org/fishery/static/Data/>

Coordinating Working Party on Fishery Statistics (CWP). Handbook of
Fishery Statistics: fishery commodities classification (ISSCFC).
<https://www.fao.org/cwp-on-fishery-statistics/handbook/socio-economic-dimension/fishery-commodities-classification/en/>

## Examples

``` r
if (FALSE) { # \dontrun{
trade <- load_fao_trade()
norway <- subset(trade, Country..Name. == "Norway" & Trade.flow == "Exports" & Year == 2024)
head(norway[order(-norway$Value), c("Commodity..Name.", "Quantity", "Value")])
catch_trade("Norway", trade = trade)
} # }
```
