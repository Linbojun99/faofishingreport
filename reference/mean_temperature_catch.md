# Mean Temperature of the Catch

This function calculates the mean temperature of the catch (MTC) of each
FAO fishing area in the data: the average preferred temperature of the
species caught, weighted by their catch (Cheung et al. 2013), and
compares it with the sea surface temperature (SST) of the area from a
chosen SST product (see
[`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)
and
[`fao_sst_products()`](https://linbojun99.github.io/faofishingreport/reference/fao_sst_products.md)).
A rising MTC means that species of warmer waters make up a larger part
of the catch.

## Usage

``` r
mean_temperature_catch(
  area_data,
  version = "latest",
  min_coverage = 50,
  trend_from = 1970,
  use_match = c("exact", "synonym", "WoRMS valid name (exact)",
    "WoRMS valid name (synonym)", "manual"),
  match = NULL,
  sst = fao_setting("sst"),
  by = c("area", "division"),
  plot = TRUE,
  table = TRUE,
  lang = NULL,
  pptx = getOption("faofishingreport.pptx", FALSE),
  label = NULL,
  data_dir = fao_data_dir()
)
```

## Arguments

- area_data:

  FAO catch data: `fishing_area_data()$area_data`, or any subset of
  [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md)
  (e.g. one country, or several fishing areas).

- version:

  The FishBase and SeaLifeBase release, e.g. "26.06"; see
  [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md).
  Default is "latest".

- min_coverage:

  The minimum share of the catch (in percent) made up by items with a
  preferred temperature. Years below it are faded in the charts, marked
  in the table and left out of the trends and correlations. Default is
  50, a convention of this package (see Coverage); use 0 to keep all
  years.

- trend_from:

  The first year of the second trend period. Default is 1970, the start
  of the period studied by Cheung et al. (2013).

- use_match:

  The kinds of match (`match_type` of
  [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md))
  whose preferred temperature is used. Default is exact names, synonyms,
  valid names found in WoRMS and manual values.

- match:

  The match table to use instead of running
  [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md):
  a data frame, or the path of a CSV file such as the species table of a
  previous run after checking it (see What the user must check). It
  needs the columns `species_en`, `match_type` and `temp_pref_mean`, and
  may have `status`, `checked` and `note`. Default is NULL.

- sst:

  The SST to compare with (see
  [`fao_area_sst()`](https://linbojun99.github.io/faofishingreport/reference/fao_area_sst.md)):
  "ersst5" (NOAA ERSST v5, built into the package), the name of a
  product downloaded with
  [`download_sst_data()`](https://linbojun99.github.io/faofishingreport/reference/download_sst_data.md),
  the path of your own NetCDF file or folder of NetCDF files, FALSE for
  none, or a data frame with the columns `area_code` (or `area_en`),
  `year` and `sst_mean` (and optionally `sst_p10` and `sst_p90`).
  Default is the `sst` setting of
  [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md),
  "ersst5" unless changed. The SST data used is printed, shown in the
  chart legend and given in the column `sst_source` of the yearly table.

- by:

  "area" (default) for the FAO major areas, or "division" for the
  divisions of the catch data of
  [`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md)
  (see Divisions).

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

- label:

  The name used in the output file names. Default is the fishing area
  (or the country) of the data.

- data_dir:

  The directory where downloaded data is stored. Default is
  [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md).

## Value

Invisibly, a list of three data frames:

- yearly:

  one row per area (or division) and year: MTC, coverage (percent),
  whether the coverage is below `min_coverage`, SST (mean and 10th-90th
  percentile range in the area), its product and the unit of the SST
  table used (`sst_unit`), number of items used, total catch and catch
  of the items used;

- trend:

  one row per area and period: slopes of the MTC and SST (°C per
  decade), the Mann-Kendall test of the MTC, and the correlation of MTC
  and SST for the series and for their year-to-year differences;

- species:

  one row per area and FAO species item: preferred temperature, catch,
  weight in and contribution (°C) to the MTC of the whole period, match,
  links, and the columns `checked` and `note` for the user's checks.

Charts are saved in `figures/png/summary/`: `<label>_mtc.png` (MTC and
SST with its range in the area; for a single area with the coverage
below) and, for several areas, `<label>_mtc_coverage.png`. Tables are
saved in `tables/`: `<label>_mtc.csv`, `<label>_mtc_trend.csv` and
`<label>_mtc_species.csv`. With `by = "division"`, `_mtc` in the file
names becomes `_mtc_divisions`.

## Details

For each year \\t\\, \$\$MTC_t = \frac{\sum_i T_i C\_{i,t}}{\sum_i
C\_{i,t}}\$\$ where \\T_i\\ is the preferred temperature of the FAO
species item \\i\\ and \\C\_{i,t}\\ its catch in year \\t\\; both sums
are over the items that have a preferred temperature. The coverage of a
year is the share of its catch made up by these items.

## What the package does and what the user must check

The package computes the MTC, its trends and its comparison with SST.
The preferred temperature of each species is **not** checked by the
package: it is a first, automatic match of the FAO scientific name to
FishBase or SeaLifeBase
([`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)),
given with the links needed to check it. Whether a match and its
temperature are right must be judged by the user, from these pages and
the literature, before the results are used:

1.  Run the function, then open the species table
    (`tables/<label>_mtc_species.csv`). Items are sorted by catch, so
    the first rows matter most.

2.  For each item used, open `url` (the FishBase or SeaLifeBase page of
    the matched species), `search_url` and `worms_url` (the World
    Register of Marine Species). Check that the matched species is the
    FAO item, and that its preferred temperature is plausible for the
    stock in the area.

3.  Write who checked it, or the date, in the column `checked`. Correct
    a wrong value in `temp_pref_mean` and give the source in `note` (to
    use a value for an item that is not used, such as a genus, also set
    its `match_type` to "manual"); or record the correction in the
    corrections file of
    [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md),
    which every later run applies.

4.  Run the function again with `match = "<path of the checked table>"`.

Until every item used has been marked as checked, the function says so
in its messages and in the caption of the charts.

Checking is needed because names do not always match between databases:
FAO ASFIS names change slowly and conservatively, FishBase, SeaLifeBase
and WoRMS follow different classifications, and synonyms and
misspellings are common (Grenié et al. 2023). The preferred temperature
itself is also uncertain (next section).

## Preferred temperatures

The preferred temperature used is `TempPrefMean` of the FishBase or
SeaLifeBase `estimate` table. FishBase gives it as an estimate from
AquaMaps distribution maps (version 10/2019), computed with an
unpublished R script (FishBase reference 123201: Froese 2020, pers.
comm.); how the map cells are chosen and whether surface or bottom
temperature is used is not documented. For some species it is well below
the temperatures of the area where most of their catch comes from; for
example, it is 10.3 °C for European pilchard (Sardina pilchardus), much
of whose catch is taken off north-west Africa. Cheung et al. (2013) used
instead the median of a temperature preference profile computed from
modelled species distributions, and most regional studies used their
values (e.g. Tsikliras and Stergiou 2014; Liang et al. 2018);
Dimarchopoulou et al. (2022) and Lavin et al. (2023) used the FishBase
mean, as here. The level of the MTC therefore depends on the source of
the values, and the MTC of this function is not directly comparable with
published values.

Only matched items whose `match_type` is in `use_match` are used: by
default, items whose FAO scientific name is a valid species name
("exact"), a synonym in the synonym table of FishBase or SeaLifeBase
("synonym"), either of these after looking up the valid name in WoRMS
("WoRMS valid name (...)"), or set by the user ("manual"). Items above
species level (e.g. "Sardinella spp" or "Marine fishes nei") are not
used, as in the studies above, since the species that make up their
catch are not known; some studies give genera the mean of the species of
the genus found in the area (Dimarchopoulou et al. 2022).

## Coverage

The items used never cover the whole catch, and published studies report
coverage from below 30% to over 90% (e.g. Liang et al. 2018; Tsikliras
et al. 2015). No published study sets a minimum coverage: `min_coverage`
(default 50%) is a convention of this package, and should be set for
each analysis. The coverage changes over time because more catch is
reported by species instead of groups, without the earlier years being
revised (Garibaldi 2012); such changes, and catches moved between areas
or reported as "nei" (FAO 2024 reports 1.3 million tonnes of Chinese
distant-water catch recorded as "nei" in Area 61), change the MTC
without any change in temperature. Look at the coverage chart before
reading a trend.

## Trends and comparison with SST

Trends are calculated over all years and over the years from
`trend_from`, using only years whose coverage is at least
`min_coverage`: the slope of a linear regression (°C per decade) and a
Mann-Kendall test (Kendall's tau between year and MTC), as in
[`peak_trend()`](https://linbojun99.github.io/faofishingreport/reference/peak_trend.md).
The MTC is compared with the SST of the area by their slopes and by the
Pearson correlation of the two series and of their year-to-year
differences.

These are simple descriptive statistics. Cheung et al. (2013) estimated
the rate of change of each large marine ecosystem with models that also
included fishing effort, a large-scale ocean index and autocorrelation,
and related the rates of MTC and SST across ecosystems; regional studies
used lagged correlations, regressions and models (Tsikliras and Stergiou
2014; Liang et al. 2018; Kamberi et al. 2025). Both series are
autocorrelated, so the p-values given here, which treat years as
independent, are too small (Yue et al. 2002; Pyper and Peterman 1998).
Two series that both rise over time are correlated even when they are
not related; the correlation of differences avoids this, but can miss a
relation that acts only over decades (Pyper and Peterman 1998). In the
Adriatic Sea, the relation of MTC with SST was no longer significant
once autocorrelation was accounted for (Kamberi et al. 2025).

## Divisions

With `by = "division"` and catch data by division
([`load_fao_divisions()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_divisions.md),
e.g. the GFCM divisions of the Mediterranean and Black Sea; Tsikliras
and Stergiou 2014 used GFCM data of the western, central and eastern
Mediterranean), the MTC is calculated for each division and compared
with the SST of the division (`fao_area_sst(level = "division")`).
Divisions without a boundary on the FAO map use the SST of their subarea
(e.g. 34.1.9 "Northern coastal, not known" uses 34.1, 51.3.1 Oman Sea
uses 51.3); the column `sst_unit` gives the unit used. Codes that are
not places (e.g. 37.0.0 Tunas, 37.9.0 Not known) have no SST. On the 2°
grid of the built-in ERSST v5, some divisions have only a few cells (the
function says which); a finer SST product, given as the path of its
NetCDF files, suits them better.

## Interpretation

A change in the MTC can come from a warmer or cooler ocean, but also
from changes in the species fished, fishing grounds, gear, markets,
management and reporting. For example, in the Arabian Gulf the MTC fell
because of export demand for largehead hairtail (Alajmi et al. 2024),
and in New Zealand fleets moving to deeper, colder waters lowered it
(Lavin et al. 2023). Check which items drive a change with the species
table (`weight_pct`, `contribution_c`) before attributing it to
temperature. A preferred temperature describes the whole range of a
species, not the stock in the area.

## References

Alajmi, A., Ben-Hasan, A., Alrushaid, T., Vahabnezhad, A. and Pauly, D.
(2024). Mean temperature of the catch index can be masked by changes in
catch composition unrelated to ocean warming. Fisheries Management and
Ecology 31: e12659.
[doi:10.1111/fme.12659](https://doi.org/10.1111/fme.12659)

Cheung, W.W.L., Watson, R. and Pauly, D. (2013). Signature of ocean
warming in global fisheries catch. Nature 497: 365-368.
[doi:10.1038/nature12156](https://doi.org/10.1038/nature12156)

Dimarchopoulou, D., Makino, M., Prayoga, M.R., Zeller, D., Vianna,
G.M.S. and Humphries, A.T. (2022). Responses in fisheries catch data to
a warming ocean along a latitudinal gradient in the western Pacific
Ocean. Environmental Biology of Fishes 105: 1347-1362.
[doi:10.1007/s10641-021-01162-z](https://doi.org/10.1007/s10641-021-01162-z)

FAO (2024). The State of World Fisheries and Aquaculture 2024. Rome:
FAO. [doi:10.4060/cd0683en](https://doi.org/10.4060/cd0683en)

Froese, R. (2020). Personal communication: R code (PrefTempBatch_5.R) to
estimate preferred temperature from AquaMaps (version 10/2019). FishBase
reference 123201.
<https://www.fishbase.se/references/FBRefSummary.php?ID=123201>

Garibaldi, L. (2012). The FAO global capture production database: a
six-decade effort to catch the trend. Marine Policy 36: 760-768.
[doi:10.1016/j.marpol.2011.10.024](https://doi.org/10.1016/j.marpol.2011.10.024)

Grenié, M., Berti, E., Carvajal-Quintero, J., Dädlow, G.M.L., Sagouis,
A. and Winter, M. (2023). Harmonizing taxon names in biodiversity data:
a review of tools, databases and best practices. Methods in Ecology and
Evolution 14: 12-25.
[doi:10.1111/2041-210X.13802](https://doi.org/10.1111/2041-210X.13802)

Kamberi, E., Hala, E. and Möllmann, C. (2025). Impact of large-scale
oceanic variability on Adriatic fisheries evidenced through the 'mean
temperature of the catch' approach. Frontiers in Marine Science 12:
1719738.
[doi:10.3389/fmars.2025.1719738](https://doi.org/10.3389/fmars.2025.1719738)

Lavin, C.P., Pauly, D., Dimarchopoulou, D., Liang, C. and Costello, M.J.
(2023). Fishery catch is affected by geographic expansion, fishing down
food webs and climate change in Aotearoa, New Zealand. PeerJ 11: e16070.
[doi:10.7717/peerj.16070](https://doi.org/10.7717/peerj.16070)

Liang, C., Xian, W. and Pauly, D. (2018). Impacts of ocean warming on
China's fisheries catches: an application of "mean temperature of the
catch" concept. Frontiers in Marine Science 5: 26.
[doi:10.3389/fmars.2018.00026](https://doi.org/10.3389/fmars.2018.00026)

Pyper, B.J. and Peterman, R.M. (1998). Comparison of methods to account
for autocorrelation in correlation analyses of fish data. Canadian
Journal of Fisheries and Aquatic Sciences 55: 2127-2140.
[doi:10.1139/f98-104](https://doi.org/10.1139/f98-104)

Tsikliras, A.C. and Stergiou, K.I. (2014). Mean temperature of the catch
increases quickly in the Mediterranean Sea. Marine Ecology Progress
Series 515: 281-284.
[doi:10.3354/meps11005](https://doi.org/10.3354/meps11005)

Tsikliras, A.C., Peristeraki, P., Tserpes, G. and Stergiou, K.I. (2015).
Mean temperature of the catch (MTC) in the Greek Seas based on landings
and survey data. Frontiers in Marine Science 2: 23.
[doi:10.3389/fmars.2015.00023](https://doi.org/10.3389/fmars.2015.00023)

Yue, S., Pilon, P., Phinney, B. and Cavadias, G. (2002). The influence
of autocorrelation on the ability to detect trend in hydrological
series. Hydrological Processes 16: 1807-1829.
[doi:10.1002/hyp.1095](https://doi.org/10.1002/hyp.1095)

Froese, R. and Pauly, D. (Eds.) FishBase. <https://www.fishbase.se>;
Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase.
<https://www.sealifebase.se>; WoRMS Editorial Board. World Register of
Marine Species. <https://www.marinespecies.org>

## Examples

``` r
if (FALSE) { # \dontrun{
FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
mtc <- mean_temperature_catch(FAO34$area_data, lang = "both")
mtc$trend
# The items with the largest catch: check their match and preferred temperature with the links
head(mtc$species[, c("species_name", "match_type", "temp_pref_mean", "catch_pct", "used", "url")], 20)

# After checking tables/Atlantic__Eastern_Central_mtc_species.csv (column `checked`, and
# corrected values with their source in `note`), run again with the checked table
mean_temperature_catch(FAO34$area_data, match = "tables/Atlantic__Eastern_Central_mtc_species.csv")

# Other SST data: your own NetCDF file (or folder), or a product downloaded once
mean_temperature_catch(FAO34$area_data, sst = "D:/data/HadISST_sst.nc", label = "FAO34_hadisst")
download_sst_data("cobe2")
mean_temperature_catch(FAO34$area_data, sst = "cobe2", label = "FAO34_cobe2")

# The GFCM divisions of the Mediterranean and Black Sea
mean_temperature_catch(load_fao_divisions("GFCM"), by = "division")

# The period studied by Cheung et al. (2013)
mean_temperature_catch(subset(FAO34$area_data, Year >= 1970 & Year <= 2006),
                       label = "FAO34_1970_2006")

# All FAO major marine fishing areas, one chart panel per area
marine <- subset(load_fao_data(), FAO.major.fishing.area..Name. %in% fao_area_sst()$area_en)
mean_temperature_catch(marine, label = "marine_areas", lang = "zh")
} # }
```
