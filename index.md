# faofishingreport

> 📘 **中文文档**：[faofishingreport
> 中文网站](https://linbojun99.github.io/faofishingreport/articles/zh/index.html)（网站右上角也可以随时切换中文）

**faofishingreport** turns the [FAO Global Capture
Production](https://www.fao.org/fishery/collection/capture/en)
statistics into charts, maps and tables for reports, by fishing area,
country and species, in English, Chinese or both.

## ✨ Features

- 💾 **Always the latest data**: check for and download new FAO releases
  with one command.
- 🌊 **Fishing area reports**: total catch with breakpoints, main
  species, main countries, maps.
- 🐟 **Species and country profiles**: who catches a species, what a
  country catches, and where.
- 📊 **Nine summary analyses**: species group composition, stock status
  plots, peak and trend, data quality, diversity and concentration,
  country groups, synchronous breakpoints, coastal versus distant-water
  fleets, and an overview of all FAO marine areas.
- 🌐 **English, Chinese or bilingual** charts with the official FAO
  Chinese names.
- 🎨 **Your style**: themes, colour palettes, map projections (Robinson,
  Mollweide, Equal Earth, …) and Pacific-centred maps, set once with
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md).
- 💻 **Windows, macOS and Linux**: a Chinese font is chosen
  automatically; tables open in Excel with Chinese names intact.

## 📦 Installation

faofishingreport needs R 4.2 or later. Install it from GitHub:

``` r

install.packages("remotes")
remotes::install_github("Linbojun99/faofishingreport")
```

For editable PowerPoint charts, also install eoffice:
`install.packages("eoffice")` (on macOS it also needs
[XQuartz](https://www.xquartz.org)).

## 🚀 Quick start

``` r

library(faofishingreport)

# Get the latest FAO release (only needed when FAO publishes a new one)
check_fao_update()
download_fao_data()

# FAO major fishing area 34, Eastern Central Atlantic
FAO34 <- fishing_area_data("Atlantic, Eastern Central")
fishing_area_species(FAO34$area_data, rank_range = c(1, 10))
fishing_area_country(FAO34$area_data, rank_range = c(1, 10))

# Who catches European pilchard, what Morocco catches
species_country(FAO34$area_data, "European pilchard(=Sardine)", lang = "both")
country_species(FAO34$area_data, "Morocco")

# Summary analyses
stock_status(FAO34$area_data)
coastal_distant(FAO34$area_data)
```

Charts are saved in `figures/png/` (and, if you want, editable
PowerPoint in `figures/pptx/`), tables in `tables/`.

## 🖼️ Examples

| Total catch and breakpoints | Stock status plot |
|----|----|
| ![Total catch of FAO area 34 with breakpoints](reference/figures/readme-area-trend.png) | ![Stock status plot of FAO area 34](reference/figures/readme-stock-status.png) |
| **Bilingual charts** | **Robinson projection, Pacific-centred** |
| ![Bilingual chart of the main countries](reference/figures/readme-bilingual.png) | ![Pacific-centred Robinson map of FAO areas](reference/figures/readme-area-map.png) |

## 🔧 Function reference

**💾 FAO data**

| Function | Description |
|----|----|
| [`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md) | Compare the latest FAO release online with the data on your computer |
| [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md) | Download a release and convert it to the package format |
| [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md) | Load the newest downloaded release, or the bundled data |
| [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md) | Folder where downloaded releases are stored |

**🌊 Fishing areas**

| Function | Description |
|----|----|
| [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md) | Catch of one FAO fishing area: trend, breakpoints, share of world catch, map |
| [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md) | Main species of an area, their catch and breakpoints |
| [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md) | Main countries of an area, their catch and breakpoints |

**🐟 Species and countries**

| Function | Description |
|----|----|
| [`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md) | Countries catching a species: ranking, trends, map, pie charts by decade |
| [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md) | Species caught by a country in an area |
| [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md) | A country’s catch in all FAO areas, by species and area, with a map |

**📊 Summary analyses**

| Function | Description |
|----|----|
| [`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md) | Catch by ISSCAAP species group or major group |
| [`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md) | Stock status plots: developing, exploited, over-exploited, collapsed, rebuilding |
| [`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md) | Peak year, last year as % of peak and recent trend of species, countries or areas |
| [`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md) | All FAO marine areas compared |
| [`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md) | Official versus estimated catch, and catch identified to species |
| [`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md) | Species and country numbers, Shannon diversity, HHI, top-N shares |
| [`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md) | Catch by income group, continent or region |
| [`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md) | Breakpoints of the main species and years when many change together |
| [`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md) | Catch by coastal countries and distant-water fleets |

**🎨 Settings and language**

| Function | Description |
|----|----|
| [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md) | Language, theme, palettes, map projection and centre, grid lines, output |
| [`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md) | Font of chart text |
| [`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md) | Chinese or bilingual names of countries, species and areas |

## 📖 Documentation

The [website](https://linbojun99.github.io/faofishingreport/) has a
guide for every part of the package: [🚀 Get
started](https://linbojun99.github.io/faofishingreport/articles/get-started.html)
· [💾 FAO
data](https://linbojun99.github.io/faofishingreport/articles/data.html)
· [🌊 Fishing
areas](https://linbojun99.github.io/faofishingreport/articles/fishing-areas.html)
· [🐟 Species and
countries](https://linbojun99.github.io/faofishingreport/articles/species-countries.html)
· [📈 Status and
trends](https://linbojun99.github.io/faofishingreport/articles/status-trends.html)
· [🔍 Quality, diversity and
fleets](https://linbojun99.github.io/faofishingreport/articles/quality-diversity-fleets.html)
· [🎨
Customise](https://linbojun99.github.io/faofishingreport/articles/customise.html)

## 📝 Citation

``` r

citation("faofishingreport")
```

Please also cite FAO as the source of the data: FAO Fisheries and
Aquaculture, Global Capture Production (FishStat). FAO data are licensed
under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## ⚖️ License

GPL-3 © Hongyu Lin
