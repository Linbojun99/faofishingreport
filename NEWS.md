# faofishingreport (development version)

## New features

* `mean_temperature_catch()`: the mean temperature of the catch (MTC; Cheung et al. 2013) of each FAO fishing area, the catch-weighted mean of the species' preferred temperatures in FishBase and SeaLifeBase, compared with the sea surface temperature of the area. It gives the coverage of the catch, slopes and Mann-Kendall trends over all years and since 1970, the correlation with SST (also of year-to-year differences), and a species table with each item's weight, contribution and links to FishBase, SeaLifeBase and WoRMS. Only species-level items matched by name, by an official synonym, through WoRMS or by a manual correction are used. The preferred temperatures are automatic first matches: users check them with the links, mark them in the `checked` column and run the function again with the checked table; charts say when not all have been checked.
* `fao_area_sst()`: yearly sea surface temperature (SST) of the 19 FAO major marine fishing areas from 1950: the area mean, the range within the area (10th-90th percentile of the grid cells) and the seasonal range (coldest and warmest month). The default product, NOAA ERSST v5, is bundled with the package; HadISST1, COBE-SST2 and OISST v2.1 can be chosen with `product = ...` after downloading them with `download_sst_data()`, which computes the area means on the user's computer. `fao_sst_products()` describes each product: grid, years, data sources, sea ice, licence and citation.
* `download_fishbase_data()`, `check_fishbase_update()` and `fishbase_table()` keep a copy of the FishBase and SeaLifeBase snapshots (the parquet files used by rfishbase; CC BY-NC 4.0, FishBase.org team) for offline use. Downloading is optional: functions download the tables they need when online; `download_fishbase_data()` downloads the tables the package uses (about 30 MB) or every table (`tables = "all"`, about 250 MB), and first says what is downloaded, from where, under which licence and by which rules it is used. When offline, or with `fao_settings(online = FALSE)`, the newest downloaded release is used, and when nothing has been downloaded, `fishbase_match()` uses a table built into the package (FishBase and SeaLifeBase 26.06, made by `data-raw/build_fishbase_traits.R`).
* `fishbase_sources()` lists the data sources, licences and citations, the tables downloaded and what they hold, and every matching rule with its published source.
* The data folder can also be set with the environment variable `FAOFISHINGREPORT_DATA_DIR` (e.g. in `.Renviron`), so several computers or a team can share one folder.
* `fishbase_match()` matches the FAO species items of the data one by one to FishBase and SeaLifeBase, only by published rules: an exact valid name, an official synonym, the valid name in WoRMS (looked up online and saved for offline use), the mean trophic level of the species of a genus, family or higher taxon (Branch et al. 2010), or trophic level 1 for plants. Each row gives the rule and its source and links to the FishBase, SeaLifeBase or WoRMS pages; items no rule covers are marked as failed with search links, and can be corrected in a shared `corrections.csv`.
* CMSY++ catch-only stock assessment (Froese et al. 2023), with the official CMSY++ code, neural network and user guide (March 2021, CC BY 4.0) bundled in `inst/cmsy/`:
  * `cmsy_system()` detects the operating system, R version, JAGS and R packages, and shows the settings used on it: the changes the user guide asks Mac users to make (`windows()` to `quartz()`, `antialias = "cleartype"` to `"default"`) are made automatically in a copy of the code, the system can be set by hand, and charts are saved through an off-screen device on every system. The archived snpar package, RStudio and the automatic package installation are no longer needed.
  * `cmsy_setup()` and `cmsy_install_jags()` install the R packages and download and open the JAGS installer for the system (JAGS 4.3.2 by default, 5.0.0 with `jags = "latest"`), checking its size and checksum; `cmsy_sources()` lists the version, link, checksums, licence and reference of every component.
  * `cmsy_priors()` searches a built-in table (offline) of the resilience and prior r of 3,155 FAO species in FishBase and SeaLifeBase 26.06, with a link to the page of each species; r ranges follow the CMSY++ user guide.
  * `cmsy_input()` turns FAO catch data into the CMSY++ catch and ID files (catch only: FAO data have no abundance index), `cmsy_run()` runs CMSY++ on each stock in its own folder and R process and returns MSY, B/Bmsy, F/Fmsy, Kobe probabilities and the status in FAO's two classes for catch-only results, and `cmsy_plot()` draws the results in English, Chinese or both.

# faofishingreport 0.2.0

## New features

* A package website (<https://linbojun99.github.io/faofishingreport/>) with a guide for every part of the package, illustrated with FAO area 34, in English with a Chinese version (switch with the button at the top right).
* New logo.
* `fao_settings(data_dir = ...)` keeps downloaded FAO data in a chosen folder, e.g. one shared by a team; `fao_data_dir()` returns it.
* `fao_settings()` sets, for the session, the default language, chart theme, font size, category palette, map palette, map centre (longitudes -180 to 180 or 0 to 360), map projection (longitude-latitude, Robinson, Mollweide, Equal Earth, Winkel Tripel, Mercator, or any PROJ string or EPSG code), map colours, grid lines, PNG resolution and PowerPoint output.
* Maps are drawn with sf: countries come from Natural Earth and are matched on ISO3 codes instead of a hand-made name list, FAO area boundaries are simplified for world maps, and maps centred on the Pacific join the parts that the data split at the dateline. Fishing area maps show land without country borders.
* New summary analyses, each saving charts in `figures/png/summary/` and tables in `tables/`:
  * `species_group_composition()`: catch by ISSCAAP species group or major group.
  * `stock_status()`: stock status plots (developing, exploited, over-exploited, collapsed, rebuilding) from catch relative to peak, following Kleisner et al. (2013).
  * `peak_trend()` and `area_overview()`: peak year, last year as a percentage of the peak, growth rate and Mann-Kendall trend, for species, countries or all FAO marine areas.
  * `data_quality()`: share of officially reported and estimated catch, and share identified to species, genus, family or higher.
  * `diversity_concentration()`: number of species and countries, Shannon diversity, HHI and top-N shares.
  * `country_group_summary()`: catch by World Bank income group, continent or region.
  * `regime_shift()`: breakpoints of the main species and the number of species changing in each year.
  * `coastal_distant()`: catch by coastal countries and distant-water fleets, from the FAO area boundaries and Natural Earth country outlines.
* Species, country and area groups (ISSCAAP group, taxonomic level, income group, continent, region, ...) are saved with each FAO release. Chart labels and the unofficial Chinese names of ISSCAAP groups, major groups and income groups are kept in editable CSV files in `inst/extdata/`.
* `check_fao_update()`, `download_fao_data()` and `load_fao_data()` check the FAO website for the latest Global Capture Production release, download it and convert it to the package's data format. `fishing_area_data()` and `country_analysis()` use the newest data available and gain a `data_version` argument.
* All analysis functions gain a `lang` argument: `"en"`, `"zh"` or `"both"` (Chinese and English on the same chart). `ZH = TRUE` still works and means `lang = "zh"`.
* Chinese names of countries, species and fishing areas now come from the FAO official code lists and the ASFIS species list (`fao_translate()`), with the package's earlier tables filling the gaps.
* The chart font is chosen for the operating system (SimSun on Windows, Songti SC on macOS, Noto Serif CJK SC on Linux) and can be changed with `fao_font()`. Previously "Songti SC" was hard-coded, so Chinese text did not display on Windows.
* CSV tables are written as UTF-8 with a byte order mark, so Excel shows Chinese names correctly.
* Charts are saved as PNG by default. Editable PowerPoint files are optional: add `pptx = TRUE` to a function, or set `fao_settings(pptx = TRUE)` for all functions. Previously every chart was saved in both formats.
* eoffice, used only for PowerPoint files, is now suggested instead of required, so the package loads on macOS without XQuartz. Install it with `install.packages("eoffice")` to save PowerPoint files.
* `fishing_area_species()` now saves PNG charts too, in `figures/png/area/` (PowerPoint in `figures/pptx/area/`, previously `figures/`), like `fishing_area_country()`.
* Charts have no background grid lines by default; `options(faofishingreport.grid = TRUE)` brings them back. Year axes are labelled up to the last year of the data.
* The PowerPoint charts of `fishing_area_data()` have the same size as the PNG (12 x 6 inches) instead of the size of the current graphics window.

## Data

* The bundled data is updated to FAO release 2026.1.0 (1950-2024). The previous bundled data (1950-2021) can be downloaded with `download_fao_data("2023.1.1")`.
* The previous bundled data lost the country name of about 24.5 million tonnes of catch (e.g. Türkiye, Réunion); the new data has no missing names.

## Bug fixes

* `species_country()` now uses its `h_value` argument; it was always 0.15 before, so breakpoint results differ from earlier charts.
* Years are no longer fixed at 2021: axis breaks, "last five years" tables, last-year shares and pie chart periods follow the data.
* Every function creates the output folders it needs, so functions can be run in any order.
* `country_analysis()` uses the fishing area map bundled with the package instead of a path on one computer, and its fishing area map no longer fails with current ggplot2.
* `country_species()` and `country_analysis()` no longer fail with `timeseries_analysis = FALSE`, and `country_analysis()` no longer overwrites a chart of `country_species()`.
* `fishing_area_data(plot = FALSE)` no longer fails.
* The fishing area map of `country_analysis()` shows the country's total catch in each area; before, the polygons of each species were drawn on top of each other and the colour showed one species only.
* A species or country whose catch series is too short for the breakpoint analysis is skipped with a message, instead of stopping `country_species()`, `country_analysis()`, `fishing_area_species()` or `fishing_area_country()`.
* Curaçao and Côte d'Ivoire are now coloured on the world maps.
* The species bar charts of `country_species()` and `country_analysis()` label their axis "Species" instead of "Country".

## Other changes

* Requires R 4.2 or later.
* Functions no longer place `Global_Production_long`, `ASFIS_sp` or `name_mapping` in the global environment.
* The package works without `library()` (e.g. `faofishingreport::country_analysis()`), and all required packages are declared, including `sf`, `maps` and `rnaturalearth`.
* Help pages for every function, generated with roxygen2.
* Unit tests (testthat), and GitHub Actions for R CMD check on Windows, macOS and Linux, test coverage and the website.
