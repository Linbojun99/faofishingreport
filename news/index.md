# Changelog

## faofishingreport 0.5.0

### Documentation

- A new tutorial, in English and Chinese, takes a complete assessment of
  the Northwest Pacific (FAO area 61) from an empty R session to a
  finished report: getting the data and knowing its release, every
  analysis of the package step by step, how to read each chart and what
  it shows for the area (with the numbers computed when the page is
  built), the files the package saves and how to read them back, and the
  report as Word, LaTeX and PDF. Its CMSY++ charts are made by
  `data-raw/tutorial61_figures.R`.

### Changes

- [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md),
  [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md),
  [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md),
  [`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md),
  [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)
  and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  no longer print their intermediate tables in the console (they are
  returned and saved in `tables/`);
  `options(faofishingreport.verbose = TRUE)` prints them again.
- The colour bars of the maps are wider, so their labels no longer
  overlap.
- The trend segments of the older charts use `linewidth`, so ggplot2 no
  longer warns that `size` is deprecated for lines.
- The Chinese data-source box at the top of the website articles has no
  spaces between Chinese and Latin text.

## faofishingreport 0.4.0

### New features

- `fao_report(format = )` writes the report as Word (`"docx"`, default),
  LaTeX (`"latex"`) and PDF (`"pdf"`), one or more. The analyses run
  once and all formats have the same text, figures, tables, captions and
  references. The LaTeX file (with its figures in a folder next to it)
  follows the format of the Word report: the `ctexart` class with
  FangSong, SimHei and KaiTi (or the fonts of macOS, or the Fandol fonts
  of TeX Live) for Chinese reports, the `article` class for English
  reports, three-line tables that run over pages, numbered captions,
  page numbers from the title page and PDF bookmarks. PDFs are compiled
  with XeLaTeX through tinytex when it is installed (which installs
  missing LaTeX packages), else with the `xelatex` of the system.
- The website links to the FAO sources (FAO, FAOSTAT, Global Capture
  Production and the FAO fishery data files) in the sidebar of the home
  page and in the footer of every page, with the note “This package is
  not affiliated with FAO; the data come from FAO”
  (“本包与FAO无关联，数据来源为FAO”); the README and the Chinese home
  page give the same links and note.
- The Chinese function reference covers the new functions (62 pages).

### Extensions: other FAO datasets

These functions use FAO fishery statistics other than global capture
production. They are an extension of the package and not part of the
Word report
([`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)).

- Catch by division:
  [`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md)
  downloads and loads the FAO capture data by division of four regional
  fishery bodies (GFCM in area 37, CECAF in area 34, the Southeast
  Atlantic in area 47 and RECOFI in part of area 51), in the package
  format with the columns `Division`, `Division..Name.` and `Region`.
  [`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md)
  and
  [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)
  gain `by = "division"`, and `fao_area_sst(level = "division")` gives
  the SST of the FAO subareas and divisions (built in for areas 34, 37,
  47 and 51 from NOAA ERSST v5; all of them from downloaded products or
  the user’s own NetCDF files). Divisions without a boundary on the FAO
  map use the SST of their subarea, and the functions say when the SST
  of a division rests on fewer than 5 grid cells.
- Trade:
  [`load_fao_trade()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_trade.md)
  downloads and loads the FAO statistics of the trade of aquatic
  products (FI_Trade: exports, imports and re-exports of each country by
  ISSCFC commodity and year from 1976, quantity in tonnes product weight
  and value in thousand USD, each commodity with its ISSCAAP group).
  [`catch_trade()`](https://linbojun99.github.io/faofishingreport/reference/catch_trade.md)
  compares a country’s catch with its exports and imports of the same
  ISSCAAP groups, by year and by group: exports as a share of the catch,
  net trade, unit values, the share of the export value estimated or
  taken from partner reports, and, with conversion factors given by the
  user, trade in live weight equivalent and the apparent supply (catch +
  imports - exports - re-exports). Because trade statistics do not tell
  whether a product was caught or farmed, only the groups mostly caught
  are compared (capture at least 90% of world production in every year;
  [`fao_capture_groups()`](https://linbojun99.github.io/faofishingreport/reference/fao_capture_groups.md),
  from FAO global production data built into the package); fishmeal and
  fish oil not assigned to a species group are reported separately.
- Employment:
  [`load_fao_employment()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_employment.md)
  downloads and loads the FAO statistics of the people employed in
  fisheries and aquaculture (FI_Employment, first disseminated in 2026:
  by country, work area, working time and sex from 1995), with FAO’s
  notes on individual countries.
  [`employment_summary()`](https://linbojun99.github.io/faofishingreport/reference/employment_summary.md)
  gives the people in aquaculture, marine and inland fishing and
  unspecified work areas by year, for countries or the world, with the
  shares of women and full-time workers among those known and the shares
  unknown (as FAO does in The State of World Fisheries and Aquaculture).
  [`catch_per_fisher()`](https://linbojun99.github.io/faofishingreport/reference/catch_per_fisher.md)
  divides marine and inland catch by marine and inland fishers (FAO
  2012, table 9), as people or as full-time equivalents with the weights
  of Scherrer et al. (2024), and flags workers of unspecified work area,
  estimated values and numbers carried forward unchanged. The article
  “Trade and employment” shows both datasets and what others did with
  them.

### Bug fixes

- `fao_area_sst(sst = "ersst5")` and
  [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)
  use the downloaded ERSST v5 tables together with the built-in ones:
  the subareas and divisions the built-in table lacks were left out
  unless the download reached a later year.
- `mean_temperature_catch(by = "division", sst = <table>)` matches an
  SST table keyed by `area_en` to the divisions (it found no SST
  before), and says when no SST matches; the check for the division
  columns now comes before the FishBase matching.
- `fao_capture_groups(years = )` gives the years covered by its table
  when none of `years` is in it.
- The SST area weights computed before are found again (their cache name
  had changed).
- `fao_report(pelagic_by = , mtc_by = )` explains that the analyses by
  division are an extension outside the report.
- Help pages of the new functions: defaults of `label`, the columns of
  `catch_trade()$groups`, `meal_oil_export_value`, the file names with
  `by = "division"`, and the shares of
  [`fao_capture_groups()`](https://linbojun99.github.io/faofishingreport/reference/fao_capture_groups.md).

### Other changes

- The CMSY++ tests of the three-system check run on the installed
  package, so they also run on macOS without XQuartz.

## faofishingreport 0.3.0

### New features

- [`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
  writes a Word report of an FAO fishing area in Chinese or English, in
  one call: cover, title, abstract, data and methods, results and
  conclusions with charts and tables, references and appendix. The text
  is written from templates filled in with the results
  (`inst/extdata/report_text.csv`), as a first draft to complete by
  hand. Chinese reports follow the format of project reports (FangSong
  and Times New Roman, 1.5 line spacing, three-line tables, numbered
  GB/T 7714 references); English reports use Arial by default and APA or
  FAO citations. With `cmsy = TRUE`, the main species (by default as
  many as `top_n`, or `cmsy_top`) are also assessed with CMSY++
  ([`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)),
  and the report ends with a section on the assessment: an overview with
  the results table and Kobe plot, and the results of every species.
  Scientific names are in italics (species and genera) in the text and
  the tables. Every analysis is a section switched on or off (`total`,
  `species`, `countries`, `groups`, `status`, `quality`, `diversity` and
  `fleets` by default; `shocks`, `collapse`, `synchrony`, `pelagic`,
  `mtc`, `distant` and `cmsy` when asked), and the settings of its
  function are given with the section as prefix
  (e.g. `species_h_value = 0.2`, `collapse_method = "worm2006"`,
  `cmsy_top = 5`). The abstract, methods, conclusions and references
  follow the sections of the report. It returns the tables of the report
  and the results of all analyses as data frames, including the CMSY++
  reference points, priors and the links to their sources.
- The website has a guide for every part of the package, illustrated
  with FAO area 34, in English and Chinese, with tables of all arguments
  of every function, examples and a reference list with the DOI or link
  of every method and data source: getting started, FAO data, fishing
  areas, species and countries, composition and stock status, data
  quality and fleets, shocks, collapses and synchrony, sea temperature
  and the mean temperature of the catch, FishBase traits, CMSY++, the
  Word report and customisation. Every function reference page has a
  Chinese version with the English original of each part beside it
  (`data-raw/make_zh_reference.R`), and the language button switches
  between them.
- New version of the logo.
- CMSY++ disclaimer and source: the CMSY++ article,
  [`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md),
  the bundled `inst/cmsy/README.md` and the README state that the
  bundled code is that of the CMSY++ authors (March 2021 release, also
  on GitHub at <https://github.com/SISTA16/cmsyPlusPlus>), run without
  changes to the method, and that faofishingreport is not affiliated
  with or endorsed by them. The runs test of the archived snpar package,
  which CMSY++ uses and
  [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
  reproduces, is credited with its source (snpar 1.0, Debin Qiu; CRAN
  archive and <https://github.com/cran/snpar>) in
  [`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md),
  [`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md),
  the code of each run and the CMSY++ article.
- Sources of the FishBase data:
  [`fishbase_sources()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_sources.md),
  the help pages and the FishBase and CMSY++ articles cite the rfishbase
  project that publishes the data snapshots (Boettiger et al. 2012;
  <https://github.com/ropensci/rfishbase>).
- The packages used by the analyses (officer and xml2 for Word reports,
  eoffice for PowerPoint, nanoparquet for FishBase, ncdf4 for sea
  surface temperature, and the R packages of CMSY++ except rjags and
  R2jags, which need JAGS: see
  [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md))
  are installed with the package. They are loaded only when needed, so
  the package also loads on macOS without XQuartz (needed by eoffice).
- Analyses that follow published methods, each citing its sources:
  - [`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md)
    finds abrupt falls in catch with the method of Cottrell et
    al. (2019; LOESS span 0.6, Cook’s distance \> 0.3 in a lag-1
    residual regression, loss against the 7-year median) or Gephart et
    al. (2017; lowess span 2/3, Cook’s distance \> 0.35, 5-year mean),
    and gives the shock frequency by year and the shock rate by area,
    country, species group or continent, with the loss and the years to
    recovery of each shock.
  - [`collapse_analysis()`](https://linbojun99.github.io/faofishingreport/reference/collapse_analysis.md)
    finds collapsed catch series with the definition of Mullon et
    al. (2005; catch below 1/10 of the maximum for 4 years after the
    maximum, for species x country x area series) and classifies the
    decline before each collapse as plateau, smooth or erratic, or with
    the definition of Worm et al. (2006; catch below 10% of the maximum,
    the share of fisheries collapsed in each year and collapsed at least
    once). It repeats the analysis on random series with no trend
    (Wilberg and Miller 2007) for comparison. On the world marine data
    for 1950-2000 it finds 24.9% of series collapsed, as Mullon et
    al. (24%).
  - [`catch_synchrony()`](https://linbojun99.github.io/faofishingreport/reference/catch_synchrony.md)
    measures how synchronously the catches of species (or countries)
    fluctuate and how much this stabilises the total: the synchrony
    index phi of Loreau and de Mazancourt (2008), the decomposition CV
    of the total = sqrt(phi) x weighted mean CV of Thibaut and Connolly
    (2013), the portfolio effect and series selection of Ong et
    al. (2021), and pairwise correlations, on series detrended with a
    linear trend (Lepš et al. 2019), for the whole period and in moving
    windows.
  - [`distant_water_profile()`](https://linbojun99.github.io/faofishingreport/reference/distant_water_profile.md)
    follows a country’s catch in the FAO areas it borders and in
    distant-water areas, its distant-water share, the number of FAO
    areas it fishes each year and when it entered each area (spatial
    expansion; Swartz et al. 2010), with a note on China’s distant-water
    catch entered in area 61 since 2022 (FAO 2024).
  - [`compare_releases()`](https://linbojun99.github.io/faofishingreport/reference/compare_releases.md)
    compares two FAO releases record by record and lists revised, added,
    removed and reclassified records, with the revision of each year and
    country (the “presentist bias” of Zeller and Pauly 2018).
  - [`pelagic_demersal()`](https://linbojun99.github.io/faofishingreport/reference/pelagic_demersal.md)
    computes the ratio of pelagic to demersal catch (P/D; Caddy 1993) of
    each FAO marine area by year and by decade (mean, lowest and highest
    yearly ratio). ISSCAAP groups are counted as in de Leiva Moreno et
    al. (2000; small pelagics against demersal fishes and crustaceans,
    the default) or Grainger and Garcia (1996; all pelagic against all
    demersal fishes), or as chosen;
    [`fao_pd_groups()`](https://linbojun99.github.io/faofishingreport/reference/fao_pd_groups.md)
    lists how each group is counted. It reports the share of the catch
    counted and of unidentified marine fishes, and its help page states
    what besides nutrients changes the ratio (fishing and markets,
    hypoxia, natural variability of small pelagics, reporting).
- [`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md):
  the mean temperature of the catch (MTC; Cheung et al. 2013) of each
  FAO fishing area, the catch-weighted mean of the species’ preferred
  temperatures in FishBase and SeaLifeBase, compared with the sea
  surface temperature of the area. It gives the coverage of the catch,
  slopes and Mann-Kendall trends over all years and since 1970, the
  correlation with SST (also of year-to-year differences), and a species
  table with each item’s weight, contribution and links to FishBase,
  SeaLifeBase and WoRMS. Only species-level items matched by name, by an
  official synonym, through WoRMS or by a manual correction are used.
  The preferred temperatures are automatic first matches: users check
  them with the links, mark them in the `checked` column and run the
  function again with the checked table; charts say when not all have
  been checked.
- [`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md):
  yearly sea surface temperature (SST) of the 19 FAO major marine
  fishing areas: the area mean, the range within the area (10th-90th
  percentile of the grid cells) and the seasonal range (coldest and
  warmest month). The package comes with one SST product, NOAA ERSST v5
  (2°, 1950-2025, in situ observations only), the smallest to keep the
  package small. Any other gridded SST can be used by giving the path of
  its NetCDF file or folder, as `sst = "path"` or once with
  `fao_settings(sst = "path")`: the area means are computed on the
  user’s computer the first time and saved.
  [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md)
  downloads HadISST1, COBE-SST2 or OISST v2.1 for this, and
  [`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md)
  describes them. Every function says which SST data it uses.
- [`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md),
  [`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)
  and
  [`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md)
  keep a copy of the FishBase and SeaLifeBase snapshots (the parquet
  files used by rfishbase; CC BY-NC 4.0, FishBase.org team) for offline
  use. Downloading is optional: functions download the tables they need
  when online;
  [`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)
  downloads the tables the package uses (about 30 MB) or every table
  (`tables = "all"`, about 250 MB), and first says what is downloaded,
  from where, under which licence and by which rules it is used. When
  offline, or with `fao_settings(online = FALSE)`, the newest downloaded
  release is used, and when nothing has been downloaded,
  [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
  uses a table built into the package (FishBase and SeaLifeBase 26.06,
  made by `data-raw/build_fishbase_traits.R`).
- [`fishbase_sources()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_sources.md)
  lists the data sources, licences and citations, the tables downloaded
  and what they hold, and every matching rule with its published source.
- The data folder can also be set with the environment variable
  `FAOFISHINGREPORT_DATA_DIR` (e.g. in `.Renviron`), so several
  computers or a team can share one folder.
- [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
  matches the FAO species items of the data one by one to FishBase and
  SeaLifeBase, only by published rules: an exact valid name, an official
  synonym, the valid name in WoRMS (looked up online and saved for
  offline use), the mean trophic level of the species of a genus, family
  or higher taxon (Branch et al. 2010), or trophic level 1 for plants.
  Each row gives the rule and its source and links to the FishBase,
  SeaLifeBase or WoRMS pages; items no rule covers are marked as failed
  with search links, and can be corrected in a shared `corrections.csv`.
- CMSY++ catch-only stock assessment (Froese et al. 2023), with the
  official CMSY++ code, neural network and user guide (March 2021, CC BY
  4.0) bundled in `inst/cmsy/`:
  - [`cmsy_system()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_system.md)
    detects the operating system, R version, JAGS and R packages, and
    shows the settings used on it: the changes the user guide asks Mac
    users to make (`windows()` to
    [`quartz()`](https://rdrr.io/r/grDevices/quartz.html),
    `antialias = "cleartype"` to `"default"`) are made automatically in
    a copy of the code, the system can be set by hand, and charts are
    saved through an off-screen device on every system. The archived
    snpar package, RStudio and the automatic package installation are no
    longer needed.
  - [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
    and
    [`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)
    install the R packages and download and open the JAGS installer for
    the system (JAGS 4.3.2 by default, 5.0.0 with `jags = "latest"`),
    checking its size and checksum;
    [`cmsy_sources()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_sources.md)
    lists the version, link, checksums, licence and reference of every
    component.
  - [`cmsy_priors()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_priors.md)
    searches a built-in table (offline) of the resilience and prior r of
    3,155 FAO species in FishBase and SeaLifeBase 26.06, with a link to
    the page of each species; r ranges follow the CMSY++ user guide.
  - [`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md)
    turns FAO catch data into the CMSY++ catch and ID files (catch only:
    FAO data have no abundance index),
    [`cmsy_run()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_run.md)
    runs CMSY++ on each stock in its own folder and R process and
    returns MSY, B/Bmsy, F/Fmsy, Kobe probabilities and the status in
    FAO’s two classes for catch-only results, and
    [`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md)
    draws the results in English, Chinese or both.
  - [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)
    runs CMSY++ in one call for the species with the largest catch in
    one or more fishing areas (or chosen species), with charts,
    projections and tables, and saves each area in its own folder
    (`input/`, `runs/`, `figures/`, `tables/`).
  - [`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md)
    draws every chart of the CMSY++ user guide (analysis and management
    panels, r-k pairs, B/k with its priors, Kobe plot with 50/80/95%
    regions and quadrant probabilities, prior-posterior densities with
    PPVR, retrospective analysis with Mohn’s rho) and the summaries
    common in CMSY papers (overview, Kobe plot of all stocks, status
    shares by number and catch and over time, B/Bmsy histogram and small
    multiples, trends, totals relative to MSY and Bmsy, map), with FAO
    or Palomares et al. (2018) status classes. Charts show only axis
    text by default; `annotations` adds titles, captions with sources,
    legends and labels.
  - [`cmsy_forecast()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_forecast.md)
    projects stocks under Fmsy fractions, status quo F or constant catch
    with the CMSY++ model;
    [`cmsy_compare()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_compare.md)
    compares runs or other assessments;
    [`cmsy_tables()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_tables.md)
    makes the priors and results tables of reports.
  - Species can be given by English or Chinese name, scientific name or
    FAO 3-alpha code everywhere, and every result links to its sources
    (FishBase page and table, CMSY++ user guide, FAO release).
  - Progress bars with elapsed and remaining time; `silent = TRUE`, or
    `fao_settings(silent = TRUE)`, turns them and the messages off.
  - `cmsy_run(settings = )` changes the general settings of CMSY++
    (process error, catch CV, MCMC chains and iterations, …), which
    otherwise keep their CMSY++ defaults;
    [`cmsy_defaults()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_defaults.md)
    lists them with their defaults and meaning, checks changed values,
    and the values used are saved with each run.
  - [`cmsy_jags_versions()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_jags_versions.md)
    tells which JAGS installer (and rjags version) goes with each R
    version on Windows, macOS and Linux, with the evidence for each
    rule, and
    [`cmsy_download_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_download_jags.md)
    downloads the matching installer automatically (checksums checked),
    also for other systems or R versions;
    [`cmsy_setup()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_setup.md)
    and
    [`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)
    use it.
  - The output of the CMSY++ processes is written to `log.txt` from
    within R, the same way on every system. A GitHub Actions workflow
    runs CMSY++ for real on Windows, macOS and Linux (JAGS downloaded
    with
    [`cmsy_install_jags()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_install_jags.md)).

### Bug fixes

- The share chart of
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)
  now gives the share of the area in the world catch of marine areas (or
  of inland waters, for an inland area); inland waters were counted in
  the world catch before.
- `peak_trend(top_n = Inf)` no longer fails on series without catch or
  with fewer than three recent years.
- The periods between breakpoints of
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md),
  [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md),
  [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md),
  [`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md),
  [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)
  and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  started or ended one year too early, and the years were wrong for
  series with a missing year; they now come from the years of the
  series.
- [`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md),
  [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)
  and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  no longer change `options(digits, scipen)` for the rest of the R
  session.
- [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md):
  the axis titles of the total catch chart use `axis_title_size` and
  `axis_title_face` (they used the axis text settings), and
  `axis_text_face` now sets its axis text.
- `collapse_analysis(method = "mullon2005")`: collapses less than 10
  years after the first year with catch of a series are not classified,
  as documented (the years before the series counted before).
- [`catch_shocks()`](https://linbojun99.github.io/faofishingreport/reference/catch_shocks.md):
  countries without a continent (the USSR, “Other nei”) are counted as
  “Other” instead of NA.
- `download_sst_data("ersst5", years = )` adds the downloaded years to
  the built-in ERSST v5 table instead of replacing it.
- [`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)
  no longer reports an update after the default download of the tables
  the package needs.
- A FishBase correction with only a SpecCode, when FishBase cannot be
  reached, is skipped instead of marking the item as matched without
  values.
- [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
  reads a release that is already downloaded without going online.
- [`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md):
  the cover shows the report number once (“Report no.: 1”); author-date
  citations after the authors’ names give only the year (“Caddy
  (1993)”); APA reference lists are ordered by all authors’ names;
  conference papers follow GB/T 7714 (“\[C\]//”); the methods give the
  breakpoint segment length and stock-status criteria actually used, and
  say when catches were smoothed.
- Help pages: corrected defaults and return values
  ([`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md),
  [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md),
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md),
  [`cmsy_area()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_area.md)),
  percent signs that cut lines short, and the descriptions of arguments
  that were copied from other functions
  ([`cmsy_plot()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_plot.md),
  [`cmsy_forecast()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_forecast.md),
  [`cmsy_compare()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_compare.md),
  [`cmsy_tables()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_tables.md)).

## faofishingreport 0.2.0

### New features

- A package website (<https://linbojun99.github.io/faofishingreport/>)
  with a guide for every part of the package, illustrated with FAO area
  34, in English with a Chinese version (switch with the button at the
  top right).
- New logo.
- [`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md)
  shows which FAO release the analyses use, when FAO published it, the
  years it covers, the file it was downloaded from, its licence and a
  citation for reports. Every page of the website shows the release used
  and its download link.
- Function reference pages of the website link to the previous and next
  function and list the other functions of their group; help pages link
  to related functions (“See also”).
- `fao_settings(data_dir = ...)` keeps downloaded FAO data in a chosen
  folder, e.g. one shared by a team;
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)
  returns it.
- [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md)
  sets, for the session, the default language, chart theme, font size,
  category palette, map palette, map centre (longitudes -180 to 180 or 0
  to 360), map projection (longitude-latitude, Robinson, Mollweide,
  Equal Earth, Winkel Tripel, Mercator, or any PROJ string or EPSG
  code), map colours, grid lines, PNG resolution and PowerPoint output.
- Maps are drawn with sf: countries come from Natural Earth and are
  matched on ISO3 codes instead of a hand-made name list, FAO area
  boundaries are simplified for world maps, and maps centred on the
  Pacific join the parts that the data split at the dateline. Fishing
  area maps show land without country borders.
- New summary analyses, each saving charts in `figures/png/summary/` and
  tables in `tables/`:
  - [`species_group_composition()`](https://linbojun99.github.io/faofishingreport/reference/species_group_composition.md):
    catch by ISSCAAP species group or major group.
  - [`stock_status()`](https://linbojun99.github.io/faofishingreport/reference/stock_status.md):
    stock status plots (developing, exploited, over-exploited,
    collapsed, rebuilding) from catch relative to peak, following
    Kleisner et al. (2013).
  - [`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md)
    and
    [`area_overview()`](https://linbojun99.github.io/faofishingreport/reference/area_overview.md):
    peak year, last year as a percentage of the peak, growth rate and
    Mann-Kendall trend, for species, countries or all FAO marine areas.
  - [`data_quality()`](https://linbojun99.github.io/faofishingreport/reference/data_quality.md):
    share of officially reported and estimated catch, and share
    identified to species, genus, family or higher.
  - [`diversity_concentration()`](https://linbojun99.github.io/faofishingreport/reference/diversity_concentration.md):
    number of species and countries, Shannon diversity, HHI and top-N
    shares.
  - [`country_group_summary()`](https://linbojun99.github.io/faofishingreport/reference/country_group_summary.md):
    catch by World Bank income group, continent or region.
  - [`regime_shift()`](https://linbojun99.github.io/faofishingreport/reference/regime_shift.md):
    breakpoints of the main species and the number of species changing
    in each year.
  - [`coastal_distant()`](https://linbojun99.github.io/faofishingreport/reference/coastal_distant.md):
    catch by coastal countries and distant-water fleets, from the FAO
    area boundaries and Natural Earth country outlines.
- Species, country and area groups (ISSCAAP group, taxonomic level,
  income group, continent, region, …) are saved with each FAO release.
  Chart labels and the unofficial Chinese names of ISSCAAP groups, major
  groups and income groups are kept in editable CSV files in
  `inst/extdata/`.
- [`check_fao_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fao_update.md),
  [`download_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fao_data.md)
  and
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  check the FAO website for the latest Global Capture Production
  release, download it and convert it to the package’s data format.
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)
  and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  use the newest data available and gain a `data_version` argument.
- All analysis functions gain a `lang` argument: `"en"`, `"zh"` or
  `"both"` (Chinese and English on the same chart). `ZH = TRUE` still
  works and means `lang = "zh"`.
- Chinese names of countries, species and fishing areas now come from
  the FAO official code lists and the ASFIS species list
  ([`fao_translate()`](https://linbojun99.github.io/faofishingreport/reference/fao_translate.md)),
  with the package’s earlier tables filling the gaps.
- The chart font is chosen for the operating system (SimSun on Windows,
  Songti SC on macOS, Noto Serif CJK SC on Linux) and can be changed
  with
  [`fao_font()`](https://linbojun99.github.io/faofishingreport/reference/fao_font.md).
  Previously “Songti SC” was hard-coded, so Chinese text did not display
  on Windows.
- CSV tables are written as UTF-8 with a byte order mark, so Excel shows
  Chinese names correctly.
- Charts are saved as PNG by default. Editable PowerPoint files are
  optional: add `pptx = TRUE` to a function, or set
  `fao_settings(pptx = TRUE)` for all functions. Previously every chart
  was saved in both formats.
- eoffice, used only for PowerPoint files, is now suggested instead of
  required, so the package loads on macOS without XQuartz. Install it
  with `install.packages("eoffice")` to save PowerPoint files.
- [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)
  now saves PNG charts too, in `figures/png/area/` (PowerPoint in
  `figures/pptx/area/`, previously `figures/`), like
  [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md).
- Charts have no background grid lines by default;
  `options(faofishingreport.grid = TRUE)` brings them back. Year axes
  are labelled up to the last year of the data.
- The PowerPoint charts of
  [`fishing_area_data()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_data.md)
  have the same size as the PNG (12 x 6 inches) instead of the size of
  the current graphics window.

### Data

- The bundled data is updated to FAO release 2026.1.0 (1950-2024). The
  previous bundled data (1950-2021) can be downloaded with
  `download_fao_data("2023.1.1")`.
- The previous bundled data lost the country name of about 24.5 million
  tonnes of catch (e.g. Türkiye, Réunion); the new data has no missing
  names.

### Bug fixes

- [`species_country()`](https://linbojun99.github.io/faofishingreport/reference/species_country.md)
  now uses its `h_value` argument; it was always 0.15 before, so
  breakpoint results differ from earlier charts.
- Years are no longer fixed at 2021: axis breaks, “last five years”
  tables, last-year shares and pie chart periods follow the data.
- Every function creates the output folders it needs, so functions can
  be run in any order.
- [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  uses the fishing area map bundled with the package instead of a path
  on one computer, and its fishing area map no longer fails with current
  ggplot2.
- [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)
  and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  no longer fail with `timeseries_analysis = FALSE`, and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  no longer overwrites a chart of
  [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md).
- `fishing_area_data(plot = FALSE)` no longer fails.
- The fishing area map of
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  shows the country’s total catch in each area; before, the polygons of
  each species were drawn on top of each other and the colour showed one
  species only.
- A species or country whose catch series is too short for the
  breakpoint analysis is skipped with a message, instead of stopping
  [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md),
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md),
  [`fishing_area_species()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_species.md)
  or
  [`fishing_area_country()`](https://linbojun99.github.io/faofishingreport/reference/fishing_area_country.md).
- Curaçao and Côte d’Ivoire are now coloured on the world maps.
- The species bar charts of
  [`country_species()`](https://linbojun99.github.io/faofishingreport/reference/country_species.md)
  and
  [`country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)
  label their axis “Species” instead of “Country”.

### Other changes

- Requires R 4.2 or later.
- Functions no longer place `Global_Production_long`, `ASFIS_sp` or
  `name_mapping` in the global environment.
- The package works without
  [`library()`](https://rdrr.io/r/base/library.html)
  (e.g. [`faofishingreport::country_analysis()`](https://linbojun99.github.io/faofishingreport/reference/country_analysis.md)),
  and all required packages are declared, including `sf`, `maps` and
  `rnaturalearth`.
- Help pages for every function, generated with roxygen2.
- Unit tests (testthat), and GitHub Actions for R CMD check on Windows,
  macOS and Linux, test coverage and the website.
