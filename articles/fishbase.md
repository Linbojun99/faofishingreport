# 🐠 FishBase and SeaLifeBase traits

📦 **Data**: [FAO Global Capture
Production](https://www.fao.org/fishery/collection/capture/en), release
**2026.1.0** (published by FAO on 2026-03-30; catches 1950–2024),
downloaded from
[Capture_2026.1.0.zip](https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip).
Licence [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
[`fao_data_info()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_info.md)
shows the release you use and how to cite it.

The FAO capture statistics give the catch of each species item, with its
English and scientific name from the ASFIS list, but nothing about its
biology. FishBase (fishes) and SeaLifeBase (other aquatic animals and
plants) give these traits: trophic level, preferred temperature,
habitat, maximum length, vulnerability to fishing, resilience and prior
r. The package uses them in two places: the preferred temperature for
the [mean temperature of the
catch](https://linbojun99.github.io/faofishingreport/articles/sst-mtc.md),
and the resilience and prior r for the [CMSY++
priors](https://linbojun99.github.io/faofishingreport/articles/cmsy.md).

The data come from fixed copies (releases) of the FishBase and
SeaLifeBase tables, which the rfishbase project of rOpenSci publishes as
parquet files (Boettiger et al. 2012; source code at
<https://github.com/ropensci/rfishbase>, files at
<https://data.source.coop/cboettig/fishbase/>). The package matches them
to the FAO species items only by published rules, and every match
carries the links needed to check it.

| Function | What it does | Internet |
|----|----|----|
| [`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md) | Lists the newest release online and the releases downloaded | Yes |
| [`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md) | Downloads the tables of a release | Yes |
| [`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md) | Reads one table of a release | Only if the table has not been downloaded |
| [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) | Matches the FAO species items to FishBase and SeaLifeBase | No with `version = "builtin"` or a downloaded release |
| [`fishbase_sources()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_sources.md) | Lists sources, licences, tables and matching rules | No |

## 📦 Releases, folders and the built-in table

A release is named by its year and month, e.g. `"26.06"` (June 2026).
Releases are fixed snapshots, so a table downloaded once is identical to
the one online. They are published at
<https://data.source.coop/cboettig/fishbase/>, and the package saves the
tables it downloads in the `fishbase` folder of
[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md):

| Path in [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md) | Content |
|----|----|
| `fishbase/fb/<release>/<table>.parquet` | FishBase tables, e.g. `fishbase/fb/26.06/estimate.parquet` |
| `fishbase/slb/<release>/<table>.parquet` | SeaLifeBase tables |
| `fishbase/fb/<release>/files.csv` (and `slb/`) | URL, size and download date of each table |
| `fishbase/corrections.csv` | Your corrections of matches (see [Check and correct the matches](#corrections)) |
| `fishbase/worms_lookup.csv` | Names looked up in WoRMS, reused when the computer is offline |

[`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md)
is the user data folder of R unless it is changed with
`fao_settings(data_dir = ...)`, for example to a folder shared by a
team. Each function also has a `data_dir` argument. Reading the parquet
files needs the nanoparquet package, which is installed with
faofishingreport.

The `version` argument chooses the release:

| `version` | Release used |
|----|----|
| `"latest"` (default) | Online: the newest release online; its tables are downloaded the first time they are needed. Offline, or with `fao_settings(online = FALSE)`: the newest downloaded release, with a message. Offline with nothing downloaded: [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) uses the built-in table; [`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md) stops with an error |
| A release, e.g. `"26.06"` | This release, downloaded if needed |
| `"builtin"` | The built-in table, without connecting to the internet ([`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) and the functions that call it) |

### The built-in table

The package includes the result of
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
for every species item of the FAO data bundled with the package, without
manual corrections. It is the file `inst/extdata/fishbase_traits.rda`,
made by `data-raw/build_fishbase_traits.R`, and its attributes give the
releases it was made from:

``` r

load(system.file("extdata", "fishbase_traits.rda", package = "faofishingreport"))
attr(fishbase_traits, "fishbase_version")   # FishBase and SeaLifeBase releases
#>    fishbase sealifebase 
#>     "26.06"     "26.06"
attr(fishbase_traits, "fao_release")        # FAO release whose species items were matched
#> [1] "2026.1.0"
attr(fishbase_traits, "built")              # date the table was made
#> [1] "2026-10-10"
nrow(fishbase_traits)
#> [1] 3797
table(fishbase_traits$match_type)
#> 
#>                     class                     exact                    family 
#>                        17                      3030                       170 
#>                     genus               not matched                     order 
#>                       358                        76                        25 
#>                    phylum              plant (TL 1)                   synonym 
#>                         7                        71                        38 
#>  WoRMS valid name (exact) WoRMS valid name (family)  WoRMS valid name (genus) 
#>                         3                         1                         1
```

With `version = "builtin"` nothing is downloaded and the results are the
same on every computer. This article uses it, so the website can be
built without downloads. Items that are not in the built-in table, such
as new items of a later FAO release, are marked as “pending online
check”. `version = "latest"` gives the values of the newest release,
which can differ slightly once FishBase publishes a new one.

**Licence.** FishBase and SeaLifeBase data are licensed [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/) by the
FishBase.org team: they can be used with attribution, but not for
commercial purposes. Cite FishBase (Froese and Pauly, Eds.) and
SeaLifeBase (Palomares and Pauly, Eds.) with the release used. The same
licence applies to the built-in table and to the trait values in the
results of these functions. Valid names from WoRMS are licensed CC BY.
[`fishbase_sources()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_sources.md)
lists every source.

## 🔄 Check for new releases: check_fishbase_update()

[`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)
lists the releases published online and compares the newest with the
releases downloaded on this computer. It contacts the data server, so it
is not run here. On a computer where nothing has been downloaded yet, it
prints:

``` r

check_fishbase_update()
#> FishBase
#>   Latest online release: 26.06 (222 tables)
#>   Downloaded locally:    none
#>   Run download_fishbase_data("fishbase") to download the latest release (or the tables of it that are missing).
#> SeaLifeBase
#>   Latest online release: 26.06 (202 tables)
#>   Downloaded locally:    none
#>   Run download_fishbase_data("sealifebase") to download the latest release (or the tables of it that are missing).
```

| Argument | Meaning | Default |
|----|----|----|
| `server` | `"fishbase"`, `"sealifebase"` or both | both |
| `data_dir` | Folder where downloaded data is stored | [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md) |
| `quiet` | If `TRUE`, no message is printed | `FALSE` |

It returns, invisibly, a data frame with one row per database:

| Column | Meaning |
|----|----|
| `server` | `"fishbase"` or `"sealifebase"` |
| `latest_online` | Newest release online (`NA` when offline) |
| `newest_local` | Newest release downloaded (`NA` if none) |
| `tables_online` | Number of tables in the newest online release |
| `tables_local` | Number of tables downloaded of the newest local release |
| `update_available` | `TRUE` when a newer release is online, or when the newest release is downloaded but not all of its tables |

Because of the second condition, `update_available` stays `TRUE` after
the default download of six tables (next section). To see whether a
newer release exists, compare `latest_online` with `newest_local`.

## ⬇️ Download a release: download_fishbase_data()

[`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)
downloads the tables of a FishBase and/or SeaLifeBase release to
`data_dir`, so they can be used offline. Downloading beforehand is
optional: with `version = "latest"`,
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
and
[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)
download the four tables they need the first time they run (about 23 MB
for both databases with release 26.06). Download beforehand to work
offline later, to keep one release for a whole project, or to get every
table.

``` r

download_fishbase_data()                              # the six tables the package uses, both databases
download_fishbase_data(tables = "all")                # every table of FishBase and SeaLifeBase
download_fishbase_data("fishbase", tables = c("species", "estimate"))
download_fishbase_data(version = "25.04")             # an earlier release
```

| Argument | Meaning | Default |
|----|----|----|
| `server` | `"fishbase"`, `"sealifebase"` or both | both |
| `version` | The release to download, e.g. `"26.06"`. A release that is not online gives an error listing the releases that are | `"latest"` |
| `tables` | `"needed"` for the six tables the package uses, `"all"` for every table, or table names, e.g. `c("species", "estimate")` | `"needed"` |
| `data_dir` | Folder where the tables are saved | [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md) |
| `overwrite` | If `TRUE`, tables already downloaded are downloaded again | `FALSE` |
| `quiet` | If `TRUE`, progress messages are not printed | `FALSE` |

Sizes of release 26.06:

| Tables | FishBase | SeaLifeBase |
|----|----|----|
| `tables = "needed"` (6 tables) | 15.8 MB | 13.2 MB |
| The 4 tables used by [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) | 12.6 MB | 10.3 MB |
| `tables = "all"` | 222 tables, about 140 MB | 202 tables, about 108 MB |

The six tables of `tables = "needed"`:

| Table | Content | Used by |
|----|----|----|
| `species` | Genus, species, family code, habitat (`DemersPelag`), maximum length (`Length`), vulnerability to fishing (`Vulnerability`) | [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) |
| `estimate` | Trophic level (`Troph`, `seTroph`), preferred temperature (`TempPrefMean`), prior r (`prior_r`, `lcl_r`, `ucl_r`) | [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md); the built-in CMSY++ prior table |
| `families` | Family, order, class and phylum | [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) |
| `synonyms` | Other names of each species with their status (accepted name, synonym, misapplied name, ambiguous synonym) | [`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md) |
| `stocks` | Resilience (High, Medium, Low, Very low) | the built-in CMSY++ prior table |
| `faoareas` | Occurrence of each species by FAO major fishing area |  |

Before downloading, the function prints what it will download, from
where, under which licence and to which folder. Each table is first
written as a `.part` file and renamed when it is complete. Tables
already downloaded (with the same size as online) are skipped, so an
interrupted download continues when the function is run again; tables
that fail give a warning.

It returns, invisibly, a data frame with one row per table: `server`,
`version`, `table`, `size` (bytes) and `status` (`"downloaded"`,
`"already downloaded"` or `"failed"`).

## 📋 Read a table: fishbase_table()

[`fishbase_table()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_table.md)
returns one table of FishBase or SeaLifeBase as a data frame, read from
the copy on this computer. When the computer is online, it first looks
up the newest release and downloads the table if it is not there yet.
When it is offline, or with `fao_settings(online = FALSE)`, it uses the
newest downloaded release that has the table, and says so. It is not run
here, since it needs downloaded tables; the output shown is that of
release 26.06:

``` r

estimate <- fishbase_table("estimate")
#> Using FishBase release 26.06 (the latest online release).
attr(estimate, "fishbase_version")
#> [1] "26.06"
# European pilchard (Sardina pilchardus) is SpecCode 1350 in FishBase
estimate[estimate$SpecCode == 1350, c("SpecCode", "Troph", "seTroph", "TempPrefMean", "prior_r", "lcl_r", "ucl_r")]
#>      SpecCode Troph seTroph TempPrefMean   prior_r     lcl_r     ucl_r
#> 1243     1350  3.06     0.1         10.3 0.6281019 0.4145473 0.9421529

slb_species <- fishbase_table("species", server = "sealifebase", version = "26.06")
```

| Argument | Meaning | Default |
|----|----|----|
| `table` | The table name, e.g. `"species"`, `"estimate"`, `"ecology"`, `"synonyms"` or `"families"` | (required) |
| `server` | `"fishbase"` or `"sealifebase"` | `"fishbase"` |
| `version` | The release, e.g. `"26.06"` | `"latest"` |
| `data_dir` | Folder where downloaded data is stored | [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md) |

It returns the whole table, with every column (the FishBase 26.06
`estimate` table has 36,761 rows and 70 columns), and three attributes:
`fishbase_server`, `fishbase_version` and `source_url`, the address of
the parquet file. A table that has been read is kept in memory for the
rest of the R session.
[`check_fishbase_update()`](https://linbojun99.github.io/faofishingreport/reference/check_fishbase_update.md)
shows how many tables a release has; the [rfishbase
documentation](https://docs.ropensci.org/rfishbase/) describes what they
contain.

## 🔗 Match the FAO species items: fishbase_match()

[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
goes through the FAO species items of the data one by one. It looks each
up in FishBase or SeaLifeBase by its scientific name in the FAO species
list, and returns the matched record with its trophic level, preferred
temperature, habitat, maximum length and vulnerability to fishing. Each
row gives the rule used and its source, and links to the FishBase,
SeaLifeBase or WoRMS pages, so that the match can be checked. Items that
no rule matches are left empty and marked as failed, with links for
searching the name online.

``` r

fb34 <- fishbase_match(FAO34$area_data, version = "builtin", corrections = FALSE)
#> FAO species items: 665; matched 643, failed 22, pending online check 0. Matched items cover 89.2% of the catch.
```

`corrections = FALSE` leaves out any corrections saved on this computer,
so the example gives the same result everywhere. The items are sorted by
catch, so the first rows cover most of it:

``` r

main <- head(fb34, 15)
main$page <- ifelse(is.na(main$url), "", paste0("[open](", main$url, ")"))
show_table(main[, c("species_name", "fao_scientific_name", "catch_pct", "match_type", "database",
                    "temp_pref_mean", "trophic_level", "page")], n = 15)
```

| species_name | fao_scientific_name | catch_pct | match_type | database | temp_pref_mean | trophic_level | page |
|:---|:---|---:|:---|:---|---:|---:|:---|
| European pilchard(=Sardine) | Sardina pilchardus | 19.84 | exact | FishBase | 10.3 | 3.06 | [open](https://www.fishbase.se/summary/Sardina-pilchardus.html) |
| Marine fishes nei | Actinopterygii | 8.06 | not matched | NA | NA | NA |  |
| Jack and horse mackerels nei | Trachurus spp | 7.82 | genus | FishBase | NA | 3.58 | [open](https://www.fishbase.se/identification/SpeciesList.php?genus=Trachurus) |
| Sardinellas nei | Sardinella spp | 6.69 | genus | FishBase | NA | 2.94 | [open](https://www.fishbase.se/identification/SpeciesList.php?genus=Sardinella) |
| Round sardinella | Sardinella aurita | 6.19 | exact | FishBase | 18.8 | 3.40 | [open](https://www.fishbase.se/summary/Sardinella-aurita.html) |
| Atlantic chub mackerel | Scomber colias | 5.91 | exact | FishBase | 22.5 | 3.91 | [open](https://www.fishbase.se/summary/Scomber-colias.html) |
| Bonga shad | Ethmalosa fimbriata | 4.94 | exact | FishBase | 26.4 | 2.52 | [open](https://www.fishbase.se/summary/Ethmalosa-fimbriata.html) |
| Skipjack tuna | Katsuwonus pelamis | 3.34 | exact | FishBase | 26.2 | 4.43 | [open](https://www.fishbase.se/summary/Katsuwonus-pelamis.html) |
| European anchovy | Engraulis encrasicolus | 2.90 | exact | FishBase | 10.8 | 3.12 | [open](https://www.fishbase.se/summary/Engraulis-encrasicolus.html) |
| Yellowfin tuna | Thunnus albacares | 2.71 | exact | FishBase | 26.7 | 4.41 | [open](https://www.fishbase.se/summary/Thunnus-albacares.html) |
| Madeiran sardinella | Sardinella maderensis | 2.65 | exact | FishBase | 21.0 | 3.20 | [open](https://www.fishbase.se/summary/Sardinella-maderensis.html) |
| Octopuses, etc. nei | Octopodidae | 2.00 | family | SeaLifeBase | NA | 3.66 | [open](https://www.sealifebase.se/Summary/FamilySummary.php?ID=1890) |
| Bigeye tuna | Thunnus obesus | 1.23 | exact | FishBase | 26.6 | 4.42 | [open](https://www.fishbase.se/summary/Thunnus-obesus.html) |
| Porgies, seabreams nei | Sparidae | 1.19 | family | FishBase | NA | 3.46 | [open](https://www.fishbase.se/Summary/FamilySummary.php?ID=330) |
| Cuttlefish, bobtail squids nei | Sepiidae, Sepiolidae | 1.04 | not matched | NA | NA | NA |  |

Species found under their FAO name (`exact`) get all values. Items above
species level, such as “Jack and horse mackerels nei” (*Trachurus* spp),
get the mean trophic level of their genus or family but no preferred
temperature. “Marine fishes nei” has no values: its FAO name,
Actinopterygii, is not found in FishBase, which places these fishes in
classes such as Teleostei, and WoRMS gives the same name as valid. A
value for such an item can be given in the corrections file, with its
source.

| Argument | Meaning | Default |
|----|----|----|
| `area_data` | FAO catch data: `fishing_area_data()$area_data`, or any subset of [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md) | `NULL`: all species in [`load_fao_data()`](https://linbojun99.github.io/faofishingreport/reference/load_fao_data.md) |
| `version` | The FishBase and SeaLifeBase release (see [above](#releases)): `"latest"`, a release such as `"26.06"`, or `"builtin"` | `"latest"` |
| `corrections` | Path of a corrections file, or `FALSE` for none | `NULL`: `corrections.csv` in the `fishbase` folder of `data_dir`, created as an empty template the first time |
| `table` | If `TRUE`, the table is saved as `tables/<label>_fishbase_match.csv` | `TRUE` |
| `lang` | Language of the `species_name` column: `"en"`, `"zh"` or `"both"` | `NULL`: the language set with [`fao_settings()`](https://linbojun99.github.io/faofishingreport/reference/fao_settings.md) |
| `label` | Name used in the file name | The fishing area (or country) of the data |
| `data_dir` | Folder where downloaded data, corrections and WoRMS results are stored | [`fao_data_dir()`](https://linbojun99.github.io/faofishingreport/reference/fao_data_dir.md) |

It returns, invisibly, a data frame with one row per FAO species item,
sorted by catch. Its attribute `fishbase_version` gives the releases
used. The columns:

| Column | Meaning |
|----|----|
| `species_name`, `species_en` | FAO name in the language chosen, and in English |
| `alpha3`, `fao_scientific_name` | FAO 3-alpha code and scientific name |
| `taxon_level`, `major_group` | Level of the item (species, genus, family, higher) and FAO major group (PISCES, CRUSTACEA, …) |
| `catch_tonnes`, `catch_pct`, `cumulative_pct` | Catch of the item over all years of the data, its share of the total catch and the cumulative share (%) |
| `status` | `"matched"`, `"failed"` or `"pending online check"` |
| `match_type` | The rule that matched the item (next section) |
| `database`, `matched_name`, `SpecCode` | Database, name and species code of the record used |
| `n_species` | Number of species of the taxon, for items above species level |
| `trophic_level`, `trophic_level_se` | Trophic level (`Troph`) and its standard error |
| `temp_pref_mean` | Preferred temperature (`TempPrefMean`, °C) |
| `habitat` | Habitat (`DemersPelag`, e.g. pelagic-neritic, demersal, reef-associated) |
| `max_length_cm` | Maximum length (cm) |
| `vulnerability` | Vulnerability to fishing (FishBase index, out of 100) |
| `url` | FishBase or SeaLifeBase page of the record (species page, species list of the genus, family page, or a search) |
| `search_url`, `worms_url` | Search of the FAO name in FishBase or SeaLifeBase, and its WoRMS page |
| `match_note`, `rule_source` | Why an item was matched or not, and the published source of the rule |
| `source` | Database and release, with “(built-in table)” when the built-in table was used |
| `checked`, `note` | Empty columns for recording your checks |

The trophic levels are the FishBase and SeaLifeBase estimates (table
`estimate`, column `Troph`). They come from diet studies or, when there
are none, from related species and body size (Froese and Pauly 2000,
“The FOOD ITEMS table”, boxes 25 and 26). The preferred temperatures
(`TempPrefMean`) are means over the AquaMaps distribution of each
species (FishBase reference 123201; Froese 2020); Cheung et al. (2013)
used medians instead.

### Matching rules

Fishes (FAO major group PISCES) are looked up in FishBase first and all
other items in SeaLifeBase first, following Branch et al. (2010,
Supplementary Information p. 1). The value of `match_type` tells which
rule matched an item:

| `match_type` | Rule | Values given | Source |
|----|----|----|----|
| `exact` | The FAO scientific name is a valid species name in the database | All | Branch et al. (2010) |
| `synonym` | The FAO name is a synonym (status “synonym”) of one valid species in the synonym table of the database. Misapplied names are never used, since they are misidentifications, not other names of the species; ambiguous synonyms, and names that point to several species, are not matched | All | Froese and Pauly (2000), “The SYNONYMS table” |
| `WoRMS valid name (...)` | A name not found by the rules above is looked up online in the World Register of Marine Species (WoRMS), which the FAO ASFIS list follows for most groups. Only an exact WoRMS record with status “accepted”, or with a single valid name, is used; the valid name is then matched again, and the rule it matched is given in brackets, e.g. `WoRMS valid name (exact)` | As for that rule | FAO (2026), pp. 1–3; WoRMS Editorial Board |
| `genus`, `family`, `order`, `class`, `phylum` | An item above species level (e.g. “Sardinella spp” or “Sparidae”) takes the unweighted mean trophic level of the species of that taxon in the database | Trophic level only | Branch et al. (2010); Liang and Pauly (2017) |
| `plant (TL 1)` | Aquatic plants (FAO major group PLANTAE AQUATICAE) | Trophic level 1 | Pauly et al. (1998) |
| `manual` | A correction in the corrections file | The values or record given | Your source, in `note` |
| `not matched` | Species not found (the values of their genus are not used), items that name several taxa (e.g. “Sepiidae, Sepiolidae”), WoRMS records that are uncertain or several | None (`status` is `"failed"`) | No published rule for these cases |

Temperature preference, habitat, length and vulnerability are left empty
for items above species level, since studies of the mean temperature of
the catch leave these items out (Tsikliras and Stergiou 2014; Liang et
al. 2018). Results of the WoRMS look-ups are saved in `worms_lookup.csv`
and reused when the computer is offline; a name not looked up before is
then marked `"pending online check"`. The built-in table was made
online, so its items have all been looked up.

### Coverage of the catch

The share of the catch matched depends on how the catch is reported.
`catch_pct` is the share of the total catch of all years in the data:

``` r

# Share of the catch (%) by kind of match
round(sort(tapply(fb34$catch_pct, fb34$match_type, sum), decreasing = TRUE), 1)
#>        exact        genus  not matched       family        order        class 
#>         60.8         18.1         10.8          8.1          0.9          0.8 
#> plant (TL 1)       phylum      synonym 
#>          0.4          0.2          0.0

# Share of the catch (%) of species-level items with a preferred temperature
species_level <- c("exact", "synonym", "WoRMS valid name (exact)", "WoRMS valid name (synonym)")
round(sum(fb34$catch_pct[fb34$match_type %in% species_level & !is.na(fb34$temp_pref_mean)]), 1)
#> [1] 60.6
```

In area 34, matched items make up 89% of the catch, but much of it comes
from genus and family items, which have a trophic level only.
Species-level items with a preferred temperature make up 61%, and this
is the part of the catch that the mean temperature of the catch can use.
The coverage also changes from year to year;
[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)
shows it for each year (see [below](#mtc)).

### Check and correct the matches

The matches are made automatically and are not checked by the package.
Names do not always agree between databases: FAO ASFIS names change
slowly, FishBase, SeaLifeBase and WoRMS follow different
classifications, and synonyms and misspellings are common (Grenié et
al. 2023). Before using the values:

1.  Open the table saved in `tables/` (here
    `tables/Atlantic__Eastern_Central_fishbase_match.csv`). The items
    are sorted by catch, so the first rows matter most.
2.  For each item, open `url` (the record used), `search_url` and
    `worms_url`, and read `match_note` and `rule_source`. Check that the
    record is the FAO item, and that its values are plausible for the
    stock in the area.
3.  Write who checked it, or the date, in `checked`. Correct wrong or
    missing values in a corrections file, with their source.

A corrections file is a CSV file with one row per FAO item to correct:

| Column | Meaning |
|----|----|
| `alpha3` | FAO 3-alpha code of the item (required) |
| `species_en` | FAO English name (optional, for reading the file) |
| `database`, `SpecCode` | `"FishBase"` or `"SeaLifeBase"`, and the species code of the record to use; its values are then read from the downloaded tables |
| `trophic_level`, `temp_pref_mean`, `habitat`, `max_length_cm`, `vulnerability` | Values to use directly (they replace those of the record) |
| `note` | The source of the correction |

By default
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
reads `corrections.csv` in the `fishbase` folder of `data_dir`, and
creates an empty one there the first time. Corrections are applied every
time the function runs, so a team that shares `data_dir` shares them. A
corrected item gets `status` “matched”, `match_type` “manual”, and its
`rule_source` gives the file and the note.

As an example, “Frigate and bullet tunas” names two species (*Auxis
thazard* and *A. rochei*), so no rule matches it. Here it is given the
values of frigate tuna, the first of the two, from the match of the item
“Frigate tuna”. Whether such a value fits an item has to be judged by
you; the note says where it comes from:

``` r

fri <- fb34[fb34$alpha3 == "FRI", ]
fix <- data.frame(alpha3 = "FRZ", species_en = "Frigate and bullet tunas",
                  trophic_level = fri$trophic_level, temp_pref_mean = fri$temp_pref_mean,
                  note = paste("Values of Auxis thazard,", fri$source, "SpecCode", fri$SpecCode))
write.csv(fix, "my_corrections.csv", row.names = FALSE)

fb34_fixed <- fishbase_match(FAO34$area_data, version = "builtin", corrections = "my_corrections.csv")
#> Applied 1 correction(s) from my_corrections.csv
#> FAO species items: 665; matched 644, failed 21, pending online check 0. Matched items cover 89.4% of the catch.
show_table(fb34_fixed[fb34_fixed$alpha3 == "FRZ", c("species_name", "status", "match_type", "trophic_level",
                                                    "temp_pref_mean", "rule_source")])
```

|  | species_name | status | match_type | trophic_level | temp_pref_mean | rule_source |
|:---|:---|:---|:---|---:|---:|:---|
| 66 | Frigate and bullet tunas | matched | manual | 4.37 | 22 | Manual correction (my_corrections.csv): Values of Auxis thazard, FishBase 26.06 (built-in table) SpecCode 94 |

With downloaded tables, a correction can instead name the record to use,
and all its values are read from it. The built-in table cannot look up a
`SpecCode`, so with `version = "builtin"` give the values themselves:

``` r

fix <- data.frame(alpha3 = "FRZ", database = "FishBase", SpecCode = 94,
                  note = "Record of Auxis thazard; give your source here")
write.csv(fix, "my_corrections.csv", row.names = FALSE)
fishbase_match(FAO34$area_data, version = "26.06", corrections = "my_corrections.csv")
```

[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md),
and
[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)
through it, run
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
with the default corrections file. Corrections meant for them therefore
belong in `corrections.csv` in the `fishbase` folder of `data_dir` (add
rows to it with a spreadsheet program), or in a checked match table
given with `match =` (`mtc_match =` in
[`fao_report()`](https://linbojun99.github.io/faofishingreport/reference/fao_report.md)).

## 📜 Sources and licences: fishbase_sources()

[`fishbase_sources()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_sources.md)
lists where the FishBase, SeaLifeBase and WoRMS data come from, their
licences and how to cite them, the tables downloaded and what they hold,
and the matching rules of
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
with their published sources.
[`download_fishbase_data()`](https://linbojun99.github.io/faofishingreport/reference/download_fishbase_data.md)
prints a summary of the same information.

| Argument | Meaning                        | Default |
|----------|--------------------------------|---------|
| `print`  | If `TRUE`, the list is printed | `TRUE`  |

It returns, invisibly, a data frame with one row per data source, table
or rule, and the columns `type` (`"data"`, `"table"` or `"rule"`),
`item`, `description`, `source` (citation or licence) and `link`. The
data sources:

``` r

src <- fishbase_sources(print = FALSE)
show_table(src[src$type == "data", c("item", "description", "source", "link")])
```

|  | item | description | source | link |
|:---|:---|:---|:---|:---|
| 1 | FishBase | Fishes: names, classification, ecology, trophic level, temperature preference, resilience | Froese, R. and Pauly, D. (Eds.) FishBase. www.fishbase.se. Licence CC BY-NC 4.0 (FishBase.org team): cite, no commercial use | <https://www.fishbase.se> |
| 2 | SeaLifeBase | Other aquatic animals and plants: the same tables as FishBase | Palomares, M.L.D. and Pauly, D. (Eds.) SeaLifeBase. www.sealifebase.se. Licence CC BY-NC 4.0 (FishBase.org team) | <https://www.sealifebase.se> |
| 3 | Snapshots | Versioned copies of the FishBase and SeaLifeBase tables as parquet files, the files downloaded by the package | Published by the rfishbase project of rOpenSci (Boettiger et al. 2012, J. Fish Biol. 81(6): 2030-2039, <doi:10.1111/j.1095-8649.2012.03464.x>) | <https://data.source.coop/cboettig/fishbase/> |
| 4 | rfishbase | R package that publishes the snapshots and reads them; faofishingreport reads the same files directly, without depending on it | Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). J. Fish Biol. 81(6): 2030-2039. Source code and documentation: <https://github.com/ropensci/rfishbase>, <https://docs.ropensci.org/rfishbase/> | <https://github.com/ropensci/rfishbase> |
| 5 | WoRMS | Valid names of species and taxa, looked up online for names not found in FishBase or SeaLifeBase | WoRMS Editorial Board. World Register of Marine Species. VLIZ. <doi:10.14284/170>. Licence CC BY | <https://www.marinespecies.org> |
| 19 | Built-in table | Used only when FishBase cannot be reached and has not been downloaded: FishBase 26.06, SeaLifeBase 26.06, made on 2026-10-10 for the 3797 species items of FAO release 2026.1.0 | Made by data-raw/build_fishbase_traits.R with the rules above; CC BY-NC 4.0 (FishBase.org team) |  |

## 🌡️ Use in the mean temperature of the catch

[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/articles/sst-mtc.md)
computes the mean temperature of the catch (MTC): the mean preferred
temperature of the species caught, weighted by their catch (Cheung et
al. 2013). It takes the preferred temperatures from
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md),
which it runs with its own `version` and `data_dir` (applying the
corrections file), or from a match table given with `match =`. Only
items with `status` “matched” and a `match_type` in `use_match` are
used; by default `exact`, `synonym`, `WoRMS valid name (exact)`,
`WoRMS valid name (synonym)` and `manual`. Genus and family items have
no preferred temperature and are not used.

Here the corrected table of the example above is given with `match =`,
so the frigate and bullet tunas are now used:

``` r

mtc <- mean_temperature_catch(FAO34$area_data, match = fb34_fixed)
```

![Atlantic\_\_Eastern_Central_mtc.png](data:image/png;base64,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P/30V7/61VtvvaVO3bdv36pVq4QQb7zxxu7du4UQLpfr+eefX7Zs2fbt25ctW6a/q626unrFihVjxoy59tprPe52UxRlzZo1Q4cOve+++2pqampra2+44YZZs2YdOXKkqalp9erVq1evLioqevnll3/+859XVFS0u+pTrkXV7gvR1NTUvPbaa/PmzduzZ8+KFSvGjx9/1113qfVUVFQ899xz6g2Bf/zjH8ePH19aWtra2vrYY4+99dZbr7zyyqOPPqqe4Dp27Nibb74phHjllVfeffddIUS7s7WtMDs7u+0qOlq8qanptttumzJlyooVK7Kzs9u+0nY3UduqqqqqXn311Xnz5pWXlz/wwAOjRo267bbbXC7Xc889N2XKlPPOO0/d4J1vnHZrbneLtdsFHk0tLi5esmTJhAkTXC6XLMuvvfbaiBEj1E3arY7Q27Jly09/+lPt/aYOmEwmdViW5c8//3zUqFFtN+Oll166bt26tuMBAN6ieEdjY+O0adO8VDkA9APZ2dmJiYlr165tO8lutyclJe3du1dRlM8++ywxMbG2tlad9MADDyQmJrpcLrV41VVXXX311erwggUL7rnnHkVRXn/99cTExC1btsiyXFxcnJiY+M9//tNjFS6XKzU1VZ1fUZQrr7wyOTnZ6XQqivLRRx+53W5FUZ5//vnExMTKysp2V92VtXTyQlRut3vlypWJiYkFBQWKotTX1ycnJy9ZskRRFFmWH3/88cTExMcffzw/P3/hwoVlZWWXXnrpX/7yF3XZq6++4q1CbwAAIABJREFUevny5frGVFVVqQu2O1u7FXqMKS8vb3dxWZZnzZo1Z84cdROtXbs2MTGxvLzc4/V6bKJ2q3K5XCtWrEhMTPzwww9lWd6zZ09iYuLy5cudTqfT6UxJSbnrrru6snHabWTbl9NRF3g09f7779eKlZWViYmJ7733nvYqut4Rmuuvv/7tt99Wh0tLSxcsWKAoyrp16zZv3qwoyrvvvut0Oo8ePfr666+vWbOmurpaWzA7Ozs9PV0BAHTsoosuKi0t7anaOEMIAL7Rye1bgYGBH3744fjx48V/nvzR7pM2cnNzt23btmjRIrX41FNP3XDDDdrUSZMmmUymgQMHCiHaPrxRkqRrr732jTfekGW5oaHht7/9rcPh+OGHH9Q1qpetRkRECCE6v4S187Wc8oWY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etAPtduDiqI8+eSTt99+u8lkWr169S233JKWlkYP+qeO9sG24ysqKuhBf6AoyoYNGxYuXKgWO9qz2h3PbugPPHpQdOcDjh7E6eni/412Z25qaho4cGAvfDvfe2cI294cqPrggw9uuukmNUKoz9XRL2K1WmNjY3NycrSR+fn5KSkp3m4t2upKD0ZGRh4/fly/iM1mowf9RFd6UOV2u//1r39pzzumB/1EV3qwtrY2IiJC/fCwWCw///nP8/Ly6EE/0fV90GM8PegnPv3003nz5mkHZx31S7vj6UR/4NGDotMPOI8dlh7E6eni/412Z87Ly1u7du2jjz766KOPbtq06bnnnnvmmWe80cjeO0OoPtXUY+S2bduSkpLOOOMMtVhSUvL222/ffPPN6vy7du365S9/KUlSVlZWdXV1dHS02+0uKiqKj4/vtWZD05UeDAgImDJlSllZmdpH33zzzXXXXUcP+omu9KDq22+/vfjii7UiPegnutKDISEh6t3XqoqKimnTptGDfqLr+6DHeHrQH2RnZ2dlZekfO9lRv3Q0nk70rbY9KDrduTx2WHZDnIau/99od+axY8eOHTtWHd6wYUN6evqIESO80U5TR19Y9qz9+/cfOnRo48aNixcvTktLU5+xU1hYOHr0aI8fNP/qq68GDhzocDiOHj2alZUVExMjhHC73Vu3bk1OTi4sLDznnHM8FkEv6HoPOp3OjRs3pqWlVVVVxcfHq29cetDnut6DQojf/e53Tz75ZEBAgDaGHvS5rvdgbm5uTk5OampqfX29xWLJzMwU9KAf6HoPtjueHvQth8OxaNEi/e9EX3LJJbNnz+6oX9odTyf6UEc9KDrol7Y7bEdzAh3p1v+NTt6iqv4QCAEAAAAAp6GsrCwsLCwkJMQblRMIAQAAAMCgfPOzEwAAAAAAnyMQAgAAAIBBEQgBAAAAwKAIhAAAAABgUARCAAAAADAoAiEAAAAAGBSBEADQr3z99deTJ08OCAiIiopavnz5sWPHhBB79+5dsmTJiBEjli9f7na7fd1GAAD8Bb9DCADob/bv3z9mzBghREVFRWxsrDpy9+7dDocjKyvLp00DAMC/cIYQANDfZGRknHPOOUKIxx57TBv54osvTps2zXeNAgDAH3GGEADQD3333XcTJkyQJKmuri4kJOTo0aPHjx+fNWuWOrWkpOShhx5KT08vKSlxu90PPvhgSEiIEMLtdj///PPR0dFFRUUvvvjiokWLlixZYjKZhBBOp3Pv3r3vvvtuWVnZSy+9dO+9965aterhhx9evHixL18nAAA/DmcIAQD90Jlnnjl58mS32/3CCy8IIV5++eWZM2eqkw4fPjxs2LDzzjvvD3/4w4oVK06cODFp0iT1xsLf/va3N9100xVXXHHrrbfed999t9122wcffKAu1dDQsHfv3r/+9a9Hjhx58skn09LSmpqaXnvtNV+9QAAAegSBEADQPz311FNCiHvuuScnJ2fq1KnqiT5FUS699FKn0zlnzhx1trlz5x48eHDnzp1CiC+++OLKK69Ux48aNUoIsXnzZrUYHR196aWXCiEOHjx41llnXXPNNdXV1du2bev1lwUAQE+y+LoBAAB4xeTJkzMzM/ft2zd37tzs7Gx1ZHFxcW5ubnx8fFBQkDqmoKBACJGTk5OVlZWbm6vmRofDsWfPHiFEQ0ODVqHZbBZCSJI0ffp0IUR4eHjvviAAAHoegRAA0G/dfvvtCxYsSE9PV7OcEKK8vFwI0dzcvGbNGnVMaGjoqlWr1IfQCCE+/fTTTZs2LViwQD1D2PZO+5iYGDU0AgDQDxAIAQD9VkBAgMeYqKgoIURLS8t1113nkesURZk7d+633357/Phxq9V64MCBduuUZdlLrQUAoPdxDyEAwECGDRsWHh7e2tp66NAh/XhFUY4fP/7+++9fdtllVqu13WXVs4Uul6s3GgoAQK8gEAIA+i01vOkv+5Qk6d133xVC/F979x4XVbX3D3wNMww3QXBAVLwgqRieoguamnrsgELeeCwxz+mo5WMa2WOdzJOn9OSTltbpOVlZXggr08Ts8YokoqWIiCDqQOINSAcYQBmZQWSGue3fH+s5+7fPnr03W0BB5/N+9eo1e8/aa33Xd609zmLvmZk8eXJZWRnDMDabbc+ePXV1dfSXJ3755Rebzdbc3Lxv3z5CiNlsJv+6Kkj/b7VaO6IrAAAAdwQWhAAAcB9qbm7esGFDSkoKIaSgoODTTz/V6XT0qT/84Q/nzp2LiYl57LHHunbtOm/evCeeeCIkJCQ4OHjr1q03b97s16/fhg0bFi9enJycnJmZ+fHHHzMMc+3aNVqb0WhMTU29fv16R3YPAACgneCH6QEAAAAAANwUrhACAAAAAAC4KSwIAQAAAAAA3BQWhAAAAAAAAG4KC0IAAAAAAAA3hQUhAAAAAACAm8KCEAAAAAAAwE1hQQgAAAAAAOCmsCAEAAAAAABwU1gQAgAAAAAAuCksCAEAAAAAANwUFoQAAAAAAABuCgtCAAAAAAAAN4UFIQAAAAAAgJvCghAAAAAAAMBNYUEIAAAAAADgprAgBAAAAAAAcFNYEAIAAAAAALgpLAgBAAAAAADcFBaEAAAAAAAAbgoLQgAAAAAAADeFBSEAAAAAAICbwoIQAAAAAADATWFBCAAAAAAA4KawIAQAAAAAAHBTWBACAAAAAAC4KSwIAQAAAAAA3BQWhAAAAAAAAG4KC0IAAAAAAAA3hQUhAAAAAACAm8KCEAAAAAAAwE1hQQgAAAAAAOCmsCAEAAAAAABwU1gQAgAAAAAAuCksCAEAAAAAANwUFoQAAAAAAABuCgtCAID7itVq3bdvX1JS0t/+9jexMiaTqba29m5GJabzRNJpyRlQAACAVlMwDNPRMQDAPezGjRtFRUWEkN///vcKhYL71JUrV65cuaJUKkePHt1B0bmj5OTkzz77TKVSNTU1+fn5uRaor69PTU0tKytbt27d3Q+vc0bC09jYuGHDhrCwME9PT71eP3/+fLVa3VHBtDig0EZGo7G4uNjT03P48OF0T11dnVarZRgmJCQkOjqaENLQ0PD111/37dvX6XRWVlbOnTvXdSwefvjhEydOiI2RxLNmszk3N5f3fiwyMjI4OPjkyZPsnq5du0ZFRXl5ebF7BCeq4M7ffvutrKzMtemxY8eqVCrp/GRlZc2aNYvdPHv2bGhoqPQhAHCPYQDAvTmttjbWUFdX5+/vT9/QcD388MNLly5tY+ViamtrP//88ztUeXu5+0HeunUrMTGxxUi0Wu1//dd/3elg5HT/TkRiszvbWENsbOytW7fo402bNul0ujYH1UpiA9ouOv9JdNcirK2t9fX1zcjIYPc4nc5t27bRx0ajccyYMc3NzXSzqalp5MiRN2/e5NZQU1MTHx+/f/9+wfqln6XUavW1a9fYFj/66CMahlqtvn79Ot35pz/96dSpU+whghNVcOfbb7/d2NjIMExJScmjjz5Kn12/fj3booQ9e/a0WAYA7mm4ZRTAfTVfuFg5c/al/g9cmTCpKed4q+vx9PRctGjR+++/z9157ty5Z555xsfHp81hCistLeX+pbxzuvtBVlVV9ezZszNE0iGNFl658fyGE48tz5z3TcHFmputq8ThcJw+fdrX15duJiQkBAUFtV+Mt0dsQNtF5z+J7lqEKpVqy5Yts2bNamhooHsUCoW3tzd9vHDhwvfff5+9Suzj4/P222///e9/59ZQXFz8xRdfpKamCtYv/Szl4eGhVCrp465duzqdThqGSqWiF/G6du26evXql19+mZYRnKhis9fX15denFSpVJ6envTZxx9/3GKxyM0RANy/WrhPAADuBwxjq6523V350jxb+RXCOC1ntZWzXww/lKlwee+lCglR/Ovdg4Rp06Zt2rTp+vXrISEhdM/ly5ejoqJKS0vZMlarNSUlZeDAgQaDQaPRjB8/nhBSUlJSWVnp6em5b9++ZcuWBQUFVVdXr1mzJjQ0NDExcdeuXXv37n3zzTenTJnCbe7cuXMpKSkWiyUkJGT06NEajUaw8pqamk8//TQkJOTpp5+mVW3YsCEkJGTr1q2HDh165pln5s+fT5vr2rXryJEjL1y4kJWV9cYbbzz55JNiMdPyPXr0mDhx4sKFC8PCwlJTU117wQty0KBBn3766bBhw/7zP//T6XSmpaW9++67ly9fJoQIVijYHa7m5ubU1NSBAwfeunXLbDb/8Y9/pFVt27bt9OnTu3fv7tev36OPPiqWLrq/vLx8586de/fuXbhw4bRp0ySGictut3/11Ve9evXq2rXr9evX6YGuGXBtVPBAiUgk2B1MXWOzy07ngs2nblkdhJC80rrXt57e9J9P/PtdzIQQ0j3Ay8N1L4dSqQwPD9+3b9/kyZMJIex6TCwzgqPPOnLkiMFg8PPzKy0tnTdvnlqtrq6uXr58uetkcJ0JK1eudB1QweYEcys9lHJOIjlnEPnXHBY8iQRjkHkS8SK0Wq2CeROrULBpnU5XVVU1YsQI13EfMmRIamrqzJkz9+zZw91vtVo3b968ceNG7s5Ro0Y988wzH3/8sYfH//1h3el0RkREnDhxorm52XURK/0sO/HoA7PZ7O3t/dZbb9FNhULB3o1vtVrZ81dsogrufOedd1xbjImJYR8zDLNt27YXX3xx586dTz/9tMFg+PDDD0eNGpWYmCgYLQDcVzr6EiUA3Hl2+/mevVv3n/nXX1us3mQylZSUfPPNN6tWraJ7GhsbtVptWlraBx98wBZLSEgwGAz08aJFi3Q6nd1u12g0DoeDYZjCwsIlS5bQZ3NycmJjY2traxmGsVgsgYGBTif/JsDNmzenpqZKVE4fZ2dnjxkzht4WpdPpIiMjS0tLGYZxOByBgYH0HrC8vLz4+HjahN1uj4yMpE2LVZuTkzNlypSzZ8/abDa9Xi/WC16QaWlpKSkp9LHZbA4ODmaL8SqU6A4rLi6urq6OPt65cydbs1arnTNnjusY8dKl1WrHjh1bU1PDMExzc7Ofnx+b4Rabnj59ellZGX2clJR0+fJlsQzwGnU9UDoSCeXXGn/3Tkbr/mtqtrdYf01NzcCBAxMSEvLy8tidgpmRGH2GYZqampKSkujjn3/+ub6+nj4WmwyuM4E3oGLNCea2xaGUcxLJOYMY8ZNILAaZJxEvwrafRCkpKVOnTmVcGAyGixcv0uzt3LmT7ty1axftta+vL688vXzH1m80Gn/77TeGYZYvX/7zzz/zCks/y/Lz86MV/vTTTxaLhbvfaDQyDFNfXz9nzpyqqir2KcGJKriTdenSpWHDhonF8OSTT9I8MAyzd+9e+uCHH35IS0tbt27dnDlzioqKxI4FgHsXrhACuAGFwn/yJP5Om73x4EHG6fy/TQ+F3+jRHgEBvFLKgK5yWnA6ndOmTevdu/fixYuVSmV2dnZCQkJJSQlboLKy0mKxdOvWjW5GREScPHly2rRper2e/ondarVevXqVPuvt7R0VFdW9e3dCiJeXl7e3t9VqlbhzTLDyPn36EEJ8fX2jo6PpdcvQ0FCDwfDAAw8QQjw8PIYMGWIymUJCQry9vcPCwujf4JVK5bx589LT0+fMmSNWrbe3d0NDA/2qCfoHeMFeuKaIvZhACOF+kQOvQonuUBUVFQ0NDeyFgri4uL59+86dO1digFwNHjyYfjOEWq0OCgoym82+vr4tNl1TU1NeXh4REUE3v//+e6VSqVAoWsyA4IESkUgH7+elHP+7Hrydt5rtuaV17BdzKBSKsYO7eyr5FwM9PKQuD1KhoaHnz58/fPhwcnJyz549d+/eXVtbK5gZpVIp0XelUnn06NGCgoKYmJinnnqK3S82GVynFo9gc4K5bXEoecTKyzmDaOSuJ9H48ePFYuiok2ju3LliZwrDMISQr7/+euDAgb///e+7devGXpdTSF5SJoScOXNm7NixhJCkpKQPP/yQO9YtPsu1Z8+ePn367N+/n1dm//79QUFBX3311Zo1a3r16sXud52onp6egjul42e9/fbb33777fvvv+90OtlXmKSkJPpgzpw54eHh586d68CbqAHgTsCCEMANeHiEbRD4Fsfad5Yat6YRh52oPb0feqjPtq2tq56+kfLz85sxY8bPP/88btw4p9OpUPzblxjr9Xqz2Xzs2DG6GRUV9dBDDxFCmpubf/rppz59+tBLDfTZFt9+8YhV7lqVw+HgbrKf0uHuDAsLO3/+vES1CoWCrZ8S7AWPk117/ytjLF6FEt1hC4SHh7ObPj4+RqPRarW2+pswPTw8aEgtNq3T6QYNGsRusu/IW8yA2IFikUjrHuD9PzMedd3/0tf5Z3VGm92pVComR4ctn/q7FqsSo1Qqx48fP27cuFdffTUlJSUmJkYsMxJ9V6vV+fn5CxYsKCws/Oijj2bOnEn3i00G16nlyrU5wdy2OJQ8ErOdW0zwDHItRk8i6ROzY08iV+zrWFpa2nPPPXfw4EG6+OzevfutW7d459fNmzc9PT0DAwPp5sWLF9mn9u/fb7PZuGsw6We5EhMTu3Xr1rdvX+66lxAyceLErl27xsTEjBw5kt4ly+JN1FdeeUVspxzjxo2bPXv2e++9V1JSMnToUN6zarV62rRpJ06cmDBhgswKAeCegAUhgPsKXbki4JmpN/el+44a1eWpsW2v8PXXX58zZ06PHj3GjBnDeyokJKSpqYn3+xMWiyUxMfHQoUMeHh7l5eXcp7jv9ux2u/QiQbBy16p4lYjVqdfr+/fvL10t91iJXvAOkegF9ynp7hBCevToUVlZyQ3A39+fvlul9+yJHchyOp3c99bsu/wWm9ZoNFqtlrdTTgYED5SIpHU2vDA0t7Tu+GXDuCGhj/Zt5UUMk8nU2NgYFhZGCFEoFAsWLEhJSXn66acFM9Ni3/v165eenm40GmfMmNGrV6/Y2FgiORl4+3kDKticYG5bHEr55W/3DCL/OolknpikI04iCWPGjBk4cOD3339Pr4N5eXlNmzZNq9VyF0gFBQXz58+nyzaz2Tx06NDHHnuMPvXcc8+dOnWK/Zii9LOC6PK+rq4uODiYuz8kJMTHx6eiooJeZRWcqII75ffd09Nz1qxZBQUFdrudrlqtVqvNZmN/LYP7nTQAcN/At4wCuDGFwufxx7svf7dLXCz51/17bREZGXnz5s0dO3b4+/uTf38vGx4erlKpzp49SzcZhjGZTBUVFaGhofRNVUlJCbsYcDgcdrudrVZwQahWq9nvxxOs3LUqhmG46w36gSX62Gg0sjs3btw4adIkiWp54Yn1ghdk79692Z8U4/62mGuFEt2h+vXrRwhhvw7x2LFjK1asEKxKMBLXYjabjaaixaYjIiKUSmVeXh5bT2Njo1gGuI0KHigRSet4KBSjBoa8NWHwY/2CbvMy8/9nMBiOHj3Kbp45c2bSpElimZEYfULIhQsXCgsLCSGBgYHJyclsT8Umg+vwyZlsgrltcSiJvJNI5hlEhE4imSemRBp587btJ5FOpztx4gRpySeffLJs2TKDwUA3161b97e//Y2NymazrVy58oMPPqCbeXl53MuPSUlJW7ZsYTeln+V1gbu5evVqdj/7R5OXXnopMzOTPhacqII7udUy//oApJj58+evWrWKXYuWlpYuXryYjSQjI4P9zi0AuG/gCiEAtInRaPzxxx8rKyt9fHwiIiL++7//m97NWFRUdPDgwbq6umPHjo0ePVqhUGRnZ7/11lsqlWrAgAHh4eGTJk0KCAjo0qXLokWLYmJi4uPj16xZQ99tZGVlFRcXX716tV+/fjk5OU1NTQcPHpw8eTL3Nqpx48ZNnTo1ICAgNja2Z8+erpUTQkwmE1tVWFhYZmamxWI5efLkE088cerUqaqqqszMzOeff54QolKp9uzZU1FRkZubu3XrVvrhGcGYTSbTgQMHtFptfn7+sGHDCCEDBgxw7QW9pYob5KhRo/75z3/OnDlzxIgRU6dOvXnzZmZmZnx8vGuFgu3y0p6VlfX1118/8sgjTU1N169fX7hwISHEYDBkZGSUlJTk5uaOGDGCexcfNxL6e2vnzp27cuVKeHj48ePHGxsbMzIynn32Wfr5T4mmFQpFTk7OokWLvvvuuwcffDAiImLChAliGeCNkeuBJpNJIpI7MV3lCAsLa2xszMzMVCqVNTU1Xbt2pZf1BDMjMfqEkB49emzdutVisdhsNoPB8MILL9D9gpNh+PDhvJngOqBizbnmVixgrnQwArgAACAASURBVBZPIplnEL1JVfAkEjsxZZ5EvAjbfhJlZWUdOHBgx44d3DyYTKYff/yxpqZm4sSJjz/+OCHEy8tr9+7d7Bo7ODj4hx9+WLduXWRkJMMwly9f3rt3L/2z15kzZ1JSUhQKBf2UYG1tbWlp6TfffPPss8+OHj36119/lXiWvdRmsVhycnJsNlt6enrfvn3pznPnzrH7//d//3fKlCndu3efPn16UlJSQEBAUlKS4ERtbm4WnL3UhQsXdu3aVV1dffjw4REjRgh+WHfQoEH19fWRkZF0MyoqKjY29tSpU2azOT8///Dhwy1+xBcA7jmKtvwtFgDgPqDValNTUz/77LOODgTgXoWTCADg3oVbRgHA3dntdpvN1tFRANzDcBIBANy7sCAEALdmNBr3799fVFSUm5vb0bEA3JNwEgEA3NNwyygAAAAAAICbwhVCAAAAAAAAN4UFIQAAAAAAgJvCghAAAAAAAMBNYUEIAAAAAADgprAgBAAAAAAAcFNYEAIAAAAAALgpLAgBAAAAAADcFBaEAAAAAAAAbgoLQgAAAAAAADeFBSEAAAAAAICbwoIQAAAAAADATWFBCAAAAAAA4KawIAQAAAAAAHBTWBACAAAAAAC4KSwIAQAAAAAA3BQWhAAAAAAAAG4KC0IAAAAAAAA3hQUhAAAAAACAm8KCEAAAAAAAwE1hQQgAAAAAAOCmVB0dAADAPelq3a021tAv2K9dIgEAAABoNVwhBAAAAAAAcFNYEAIAAAAAALgpLAgBAAAAAADcFBaEAAAAAAAAbgoLQgAA6OxMJlNtbW1HRyHM6XRardY2VnIXOtiZcyhfQ0NDdXW1RIHGxsa7FgwAwP0B3zIKANBJNTY2btiwISwszNPTU6/Xz58/X61Wu+6sqqoqKytzPXzs2LEqVad4kd++ffuRI0fWrVvXusPr6+tTU1PLyspaXcMdtWXLlpkzZxJCrl+/npWVlZaWtmbNmoiICLaAw+FISUmJiooqKiqaO3eut7c3rwb5HdTr9bt27YqOjj558uQLL7yg0WjavYlOiJ0/tBfFxcXffvutWGGn03ns2LHRo0ffzQgBAO5puEIIANBJ/cd//EdycvKMGTOeffbZLl260Ms7rju/+uqrESNGxMXFhYWF/fWvf42Li4uLiysrK6uvr+/oHvyf2NjY119//bYOuXbt2tq1a+njoKCg8ePHe3p63oHQ2uqXX34ZPXq0QqEghPj7+//xj3+0Wq0eHv/2b+vq1avj4+PHjBkze/bsOXPm0J2t6KDVap00aVJycvKoUaNeffXVuLg4hmHat4lOghs54cwf2gvX5S5XQEDAlStXjEbjHY8SAOB+gQUhAEBn5HA4Tp8+7evrSzcTEhKCgoIEd/r6+vr5+RFCVCoV+47/8ccft1gsHRK5q+Dg4MjIyNs6pLS01MvL6w7F016cTmdaWlr//v3ppre3N10Zci/MOp3OL774gpbx9/evrq42mUykVR0sKCj4wx/+QFebXl5evXr1+u2339q3iU6CFzlv/vDW265mzJixYsWKOxUcAMB9p1PcTQQAADxKpTI8PHzfvn2TJ08mhPTs2ZPud935zjvvuB4eExPD21NdXb1mzZoePXpMnDhx4cKFYWFhqampJSUllZWVnp6e+/btW7ZsWVBQEC0WGhqamJi4a9euvXv3vvnmm1OmTKGVnD9//pdffnn44Yd//fXXmJiYbdu2/c///I/Vak1JSRk4cKDBYNBoNOPHj+e2W1tb+8UXX5hMpk8//VS6cta5c+dSUlIsFktISMjo0aPZGyPLy8t37ty5d+/ehQsXTps2jRAi3TQhRKyDvDyI1eN6OLfygoKCqVOnSo9jdXX1o48+ym4OHz78/Pnz/v7+MjvIZTabuZsPPvigTqeLiIhoryZoZrp27Tpy5MgLFy5kZWW98cYbTz75JH3WNUWCmbTb7V999VWvXr26du16/fp1sWGSmAm80bfb7ez8cU2v4MB5eno2NjZaLBbpa4kAAPB/GAAAuH1Xrje28b8Wm6ipqRk4cGBCQkJeXp70TurSpUvDhg2TqDAnJ2fKlClnz5612Wx6vd5ut2s0GofDwTBMYWHhkiVL2GKxsbG1tbUMw1gslsDAQKfTyTCMw+EYOHAgfVxfXz9hwgRaPiEhwWAw0MeLFi3S6XS8dvPz82fNmiVdOc/mzZtTU1PZTa1WO3bs2JqaGoZhmpub/fz86FHSTUt0kJsHsXrEDmetXLmyoqKCtzM+Pp67U6vVvvDCC+zmxx9/vHv3bvkd5Kqvr+/du7fNZqObr776ak5OTvs2kZeXFx8fT/fb7fbIyEg6UmIpcs3k9OnTy8rKaLGkpKTLly9LHCs2E3iRc+ePVqt9+eWX2afEJsCWLVsKCgoYAACQAbeMAgB0UqGhoefPn//LX/6SnJw8ceJEm80mtlMmb2/vhoaG6OholUrVs2dPpVKp1+vpDXhWq/Xq1atssaioqO7duxNCvLy8vL296bdoWq1WtVpN74r08/O7cuUKIaSystJisXTr1o0eGxERcfLkSV67fn5+7F2UYpW3aPDgwaGhoYQQtVodFBRkNptbbFqig9w8iNUjdjjr7Nmz7FEyKRQK5l8f/Guxg7wCgYGBO3bseOmll3bv3v3LL7/s3bt3wIAB7duEt7d3WFgYHWKlUjlv3rz09HQiPsq8TNbU1JSXl7NfqPP9998/8MADEsfKnAnc+cMlMQHCw8PLy8sFawMAAB7cMgoA0Hkplcrx48ePGzfu1VdfTUlJeeWVV8R2yqFQKB566CHunubm5p9++qlPnz7Nzc3sKoKuB1x5e3vHxsaePXv2kUce2bJly8cff0wI0ev1ZrP52LFjtExUVBSvCfLvH/oSq/y2eHh4MAwjp2mxDnJLStQjeDjLbDa3+Hm2oKAgg8HAbt64cWP48OEyO0gIKSsr27p1KyEkMDBw4cKFw4cPp4ebTKbw8HC6umtjE1y80QkLCzt//jwRTxEvkzqdbtCgQewmXcVJHNtikGyogvslBs7T09N1uQsAAIKwIAQA6IxMJlNjY2NYWBghRKFQLFiwICUlRXDnbVXLXQNYLJbExMRDhw55eHjwLqdwi9ntdnZzxowZvr6+eXl5U6ZMoR9LCwkJaWpquq1v+RerXILT6XQ6neymw+GQ07TMDorVI3E41bNnz+bmZt4H1Xjd6dWr1+nTp9nNgoKCN954Q2YHCSEPPPDA3//+d15hhmEWL16clpbWLk1I0Ov19LtqJFLN7a9Go9FqtbwCMo+VORMYhmF7IVFzQ0NDSEhIi7UBAADBt4wCAHROBoPh6NGj7OaZM2cmTZokuJPd5L5XFuRwOOx2O7tZUVERGhpKL7+UlJSwKwReMe479S+//HLQoEHDhw9nv6QkPDxcpVKdPXuWjYF+xSWvXTYwicq51Go191tSeUfRz9G12LTMDorVI3Y4a8SIEa4/ku5wOLgllUrliy++WFFRQQhpamry8/OjtzjK6aBrWgghFotl5cqVy5YtY79nqH2bYH+wweFwbNy4kU4wsRTx6oyIiFAqlXl5eWwNjY2NMo/lzgTXyNn5Y7fb2aMkJsDFixcHDx4smEAAAODBFUIAgM4oLCyssbExMzNTqVTW1NR07do1Nja2ubnZdSctf+HChV27dlVXVx8+fHjEiBHsT1OwTCbTgQMHtFptfn7+sGHDCCEDBgzo0qXLokWLYmJi4uPj16xZk5GR8eSTT2ZlZRUXF1+9erVfv345OTlNTU0HDx6cPHmyh4eHSqXq3bu3j4+PQqEYNGjQJ598MnDgwOzs7LfeekulUg0YMCA8PJy7RiWENDQ0pKenl5SUlJWVBQcHS1TOPWrcuHFTp04NCAiIjY319fXNyMg4d+7clStXwsPDjx8/3tjYmJGR8eyzz0o3LdZBXh4UCoVgPYKHT5gwga3/6aef3rFjB7vwKCoqqqysLC8v37Jly+jRo4cPH65Wqwkh77777oYNGx599NEzZ85s2bLltjqoVCq5PSouLr5x48Y777zDS1c7NqFSqfbs2VNRUZGbm7t161a68hdMkeuMUigUOTk5ixYt+u677x588MGIiAiaLsFjJWYCN3I/Pz92/gQGBmZkZJw/f76oqOjhhx8WGzhCSFZWlvxbqQEA3JzoR88BAEDC1bpbbayhX7Bfu0Ry19y6devIkSMTJ06kmxUVFYmJidybFd3QwoUL16xZ0+InCe8VWq02NTX1s88+6+hA2qSqqur48ePTp0/v6EAAAO4N98m/YQAAcKedPHmyS5cu7GZAQEBcXFwHxtMZrFixgn4P5/3Bbrff1vfWdk5paWlJSUkdHQUAwD0Dt4wCAIAsY8aM+cc//nH9+nV/f3+r1epwOFavXt3RQXWwrl27hoeHm81mHx+fjo6lrYxG4/79+4uKinJzc0eOHNnR4bTSpUuXXnrppXb5MlsAADeBW0YBAFrDDW8ZBQAAgPsPbhkFAAAAAABwU1gQAgAAAAAAuCncMgoAAAAAAOCmcIUQAAAAAADATWFBCAAAAAAA4KawIAQAAAAAAHBTWBACAAAAAAC4KSwIAQAAAAAA3BQWhAAAAAAAAG4KC0IAAAAAAAA3hQUhAAAAAACAm8KCEAAAAAAAwE1hQQgAAAAAAOCmsCAEAAAAAABwU1gQAgAAAAAAuCksCAEAAAAAANwUFoQAAAAAAABuCgtCAAAAAAAAN4UFIQAAAAAAgJvCghAAAABEVVVVbdiw4cCBA+1e+J5mMplqa2vv6OENDQ3V1dWtbqK9wrgnWK3Wffv2JSUl/e1vf5Mo1nn6KyeSzhOtfDIHoi3c50XmbsKCEAAA4D5nNpsPHz586N9duXJFzrFhYWH9+/c/ffp0uxe+m7Zv356cnNxetdXX16ekpCxfvrx1bUkfzpbZuHHjkiVL2hBmC+SE0QpZWVk9OeiSxuFwrF+/Pjs7e+3atRaLhS38ww8/5OXl/fjjj9waGhsbr1+/fluNvvbaawkJCT/88MPSpUvFytyh/raCzAnQSaK9LXIGoo067YvMPQ0LQgAAgPucj49PbGzsxIkTo6Oj4+Li4uLiRo4cuWnTJpmHq9VqtVp9JwrfNbGxsa+//np71RYUFDR+/HhPT8/WtSV9OLeMt7d3mwJ1ce3atbVr18oPoxXMZnM1R2hoKCFk9erV8fHxY8aMmT179pw5c2hJg8Fw48aN4cOHX7t2zWg0sjUcOnQoJCREfotNTU3V1dWenp4KhcLPz4/71F3orytuo4LkT4C7EG07khiI9iX2ItNi5jtcp40QC0IAAAC34OHhoVQq6WNfX987+o6tswkODo6MjLzn2vLwaOf3aaWlpV5eXu1bZ4ucTucXX3zRv39/Qoi/v391dbXJZCKEXLx4ccCAAYSQQYMGXbp0iRY+efLk008/fVv1V1VV9ezZU/CpDulvhzTaGUgMxN3R+TPfaSNU3nMXowEAAKAVVq9e/Ze//MXHx8disXh4eIwePZoQUlJScubMGZ1O9/nnnw8bNszHx4cWbmpqeuONN06cOFFdXa3T6ex2+8iRIwkhVqt1/fr1RqMxPz+/urr6gQcekChMCNHpdCUlJX369KGbDMNs3779iSeeGDZsWEREhMlk+vLLLy9fvvy73/3Ow8NDsHLXCKurq1euXPnrr79qNJrnn3/+l19+SUxMPHLkSGFhoU6nO3DgwCOPPMIufQkhtbW1H3300e7du59++ml6bHFxcffu3Tdt2rRkyRKNRsNbvzEMk5aWNmLEiGHDhvXv3//8+fOxsbFqtTo6OrqkpGTp0qW9e/f28PA4ffp0ZGRkamrqkiVL/P39o6KieG3R2ux2+8aNG/V6fU1NzalTp9higodz1dbWnjp1atKkSWJpJ4S49loiD+fOnVu7du3ly5dVKlVISIivr69YGIKzQk7qCCEXL17k7dTr9adOnXr++efZAhqNpnfv3hUVFXa7PSIi4rfffvPy8goLC7t582ZjY2NYWJjgBG5ubt64caPRaCwuLj5z5sxDDz1Eo/r222+zs7NDQkLMZjN3QSK/vxIZlhhEwUS5NtqWCSBWRnCAJIZe7BAWHdyCggKHw3HgwIFVq1b17t27b9++gueaYK5cB8K1mGttw4YNk1m/xIuM2HC71lNTU/P+++8XFRUFBgampKQsXrx46NChTqdz/fr17733XlNTU0xMjEQ2xOaJYJZanBsmk+nNN9+8du3aY4895nQ6t23b9swzzyxcuFCsQukp2lYMAAAAuAE/Pz+DwcAwzOHDhxsbGxmGsdvtGo3G4XAwDFNYWLhkyRK28JgxYy5evEgfb9my5R//+Ad9nJCQQCthGGbRokU6nU6iMMMwKSkpU6dO5UUyevTo0tJS+jgjI4Pd71q5WIQ5OTlTpkw5e/aszWbT6/VNTU1JSUn0qZ9//rm+vp7XYn5+/qxZs9hjY2Nja2trGYaxWCyBgYFOp9M1XY888si1a9cYhrHZbAkJCQcPHqT79+3bxzCMVqsdO3ZsTU0NwzDNzc1+fn5sJdy2GIaZPn16WVkZfZyUlHT58mXpw1larfbll1+WSLtrr1vMw+bNm1NTU7lNuIYhMSvkpO6HH35IS0tbt27dnDlzioqKaCsvvPACW+Djjz/evXs3wzB1dXXr169nGGb9+vU3btxgGIbu37FjR25u7s6dO3k1x8XF1dXV0cc7d+5MSUlhezFnzhxGiJz+SmSYJTiIYoniNdqWCSBYRrBd6aGXGFNWXl5efHw820RkZCQdaN65JpEr3kAIFnOtTWb9Ei8yLF7mBevJzs4eM2YMPbV1Ol1kZCR9LXI4HIGBgc3NzdLZEOs7rxcy50ZaWho7jc1mc3BwMPuU/LS3C9wyCgAA4C727Nlz6NChPXv20E2lUqnX6+l9iVar9erVq3R/ZWVlfX39oEGD6Cb9GBjdb7FYunXrRjcjIiJOnjwpVpiaO3fuzp07eWEsW7bsm2++oY/ZKxWClYtF6O3t3dDQEB0drVKpevbsqVQqjx49WlBQwDDMU089FRgYyGvRz89PpVKxx0ZFRXXv3p0Q4uXl5e3tbbVaXXO1cOHCrKwsQkhxcfHmzZs3btxICDEajQ8//DAtMHjwYNpZtVodFBRkNptd26qpqSkvL4+IiKCb33//Pft3fbHDXQlmhhDi2usW8+DKNQyxnMtMXVJS0nPPPffyyy+vW7cuPj6+vr6eV0ChUDAMQwjRaDQBAQFnzpwJCgoKCgrKzc1NSEi4ceOGwWAYMWJEbW0tvbOUqqioaGho0Gg0dDMuLm7x4sUt9k5Of4l4himxQZRIVIvHikUiJ1rBdqWHXk6o3t7eYWFhCoWClp83b156ejpxOdekc8USK8arTWb90i8ygsQC8PX1jY6Oph9SDQ0NNRgMdEQ8PDyGDBnCTjnBbEj03fUVqcWEE0KcTif3tnD2daPVaW81VctFAAAA4L6QmJjYrVu33r17s7eTNTc3//TTT3369KF/Gqc79Xr94MGDXQ/X6/Vms/nYsWN0Myoq6qGHHiorKxMsLOGpp56aPXv28uXL9Xo9vUdLrHKxCBUKBX2WUqvV+fn5CxYsKCws/Oijj2bOnMlrkfuui77Ja9HkyZOnT5/+pz/9qbGxMSQkpL6+3mw2l5SU8G5UY+tnY+O2pdPp2Hex5N/f8Ikd7kosM669bjEP0tgwBHNOZKeOUqvV06ZNO3HixEMPPWQwGNj99Itk6OM//vGPhJBHH320oaEhICDAy8ursLCQ3nEaGRl56dKloUOHskkIDw9nK/Hx8TEajVartS3fYMT2VyzDlMQgiiVKzrGCkciJVrDdFoe+xVB5gxsWFnb+/Hnicq5J56rFYrzaZNbfihcZiQC4xRwOB3fT6XSyUXH302xI9N21Xy0mnNscIYRXpnVpbzUsCAEAANwL+9bKYrEkJiYeOnTIw8OjvLycLaDRaHQ6neuBISEhTU1N9MOHLKPRKFhYgkqlSk5OzsvLYxiG/XihYOViERKX90/9+vVLT083Go0zZszo1atXbGysRADcY+12u+DbteDg4Fu3btXW1tKLCcnJyUeOHGH/Qu90Orlv5nhvK1kajUar1brul3M4wzC0jGBmKNde31YeBMOQyDlpKXVWq9Vms7HfV6RSqTw9PXv16sX9kYCCgoI33niDVy39lBS3foVCwY2tR48elZWV7KbFYvH396erQXpfn0Q3pftLJDNMxAdROlHSx8qZAGJlxNqVGHo5ofLo9Xr6PUDk3wddIlfcgZAo5jpYLdbfihcZOQHwIpH+o0z//v2l5wn3cJkJp7drSjzLPpZuuu1wyygAAIBbcH3TWVFRERoaSq9olZSUsAXo171UVVXRzZqaGvogPDxcpVKdPXuWbjIMYzKZxApTOp3uxIkTrsHMnTv3ww8/7NKlC7tHsHKxCB0Oh91uZ4+9cOFCYWEhISQwMDA5OZn7FFuefW/NO1ZsQUgIWbhw4SuvvEIvWI0bN27lypXszyHwKrHZbGwl3LYiIiKUSmVeXh77VGNjo/Th3MBoGcHMCPa6xTyo1WruzwAKhiGWczmpKy0tZe/kdDgcGRkZTz75pFKpfPHFFysqKgghTU1Nfn5+7LqaOnbsGPsdPIMGDaJvoMvKyuh3kFL9+vUjhDQ0NLCHrFixQjCq2+0vEc8wJTaIYoniNtqWCSBWRrBd6aGXGFMu9sc/HA7Hxo0b6Rca8cKQyBW3pFgx18GSU7/0iwyLm3k5ATAMw5ve3FFwzYbMvhN5c4MQ0rt3b/bOT94toPLT3i6wIAQAALjPWSyWQ4cO2Wy2HTt2cD/NMmDAgC5duixatGjbtm0jR468ceNGRkYGIUShUBw/fvyvf/3rBx988OOPP966dSsrKys7O1uhUGRnZ6ekpLz22muff/55enp6QECAWGHaRFZW1j//+U/XkOjHh7h3PQlWLhihyWQ6cOCAVqvNz8+nx/bo0SMvL+/48eNHjhwxGAzjxo3jttXQ0JCenl5SUlJWVmYymbKysoqLi2kecnJympqaDh48yL0Uw5owYUJ0dDS9eSwgIKBv3770monJZMrIyDh37tyVK1cIIcePH29sbMzIyHA4HNy2aKdycnI2bdq0YMGCtWvXZmZmdunSReJwtmmDwZCRkXH+/PmioiLBzAj2WjoPhJBx48Zt3759y5Yt9LcfBMPo37+/4KyQk7qoqKjY2NhTp04dO3ZszZo1hw8f9vX1JYS8++67e/fuzc3N/frrr7ds2cINyWg0ajQa9s5P+v2QZ86c8ff3Zz8xyM6l77777vjx41lZWdevX6ffx0gTVVJSkpub67qmktNfh8MhlmF2ZroOIhE/fbiNtnoCSJQRbFd66MVC5VGpVHv27Fm7du3MmTO3bt2q0WhczzWxXPEGQrCYa20y65d+kREcbrEA2Dlst9szMzMtFgtdiZ06daqqqiozM5NdhrlmQ6zvrr2QMzcIIaNGjaqpqZk5c+aXX34ZGRl58+bNzMxM+WlxHcFWU8i8yA4AAADQjnJyckaNGtXRUUAH432vBnQUrVabmpr62WefdXQgnYK7ZQNnIAAAANxt1dXVQ4YM6egooONhNdhJ2O12m83W0VF0Fu6WDZyEAAAAcJc4nc4+ffp88sknV65cCQoK6uhwAIAQQoxG4/79+4uKinJzczs6lo7nhtnALaMAAHD/s1qtmZmZmzdvHjBgwKpVq+5QK1VVVenp6f369UtISLhDTdwHbt26xTAM9+tkgMUwzI4dO3r16rVy5co///nPf/7znzs6IllMJpPFYpHz63B3lMQJ2EkibKOGhoZbt27Rz9+6blI3btzYtWtXly5dnnvuuY6IsR1SbTAY9uzZ4+Xl9fzzz9/WgWIToB1zcn9MJFe4QggAAJ2X2Ww+8i9Hjx69cOGC4Jd/tOi1115LSEj44Ycfli5d2u5BssLCwvr378/9hn1w5efn5z6rwe3btycnJ8svv2bNmkceeWTUqFG7du168MEH71xg7ai+vj4lJWX58uUdHYjoCdh5ImyL+vr6jRs3LlmyRHCT1a1bt+jo6KNHj971AP8vqranWqPRREdHHz58+HYPFJsA7ZWT+2MiCcKCEAAAOi8fH5/f//738fHxQ4YMGTNmTEBAwJQpU2pra2+rkqampurqak9PT4VCwf5C2h2iVqtdfyn72rVra9euvaPttt09EWQb3Z0+cluJjY19/fXXZR5os9m2bt1Kf8fcx8fn8ccfb0vT7aXFOoOCgsaPH+/p6dm+7baO4AnYqSLkkT9ktBfe3t6Cm1xqtVql6phfGm+vVHt6enp5ebXiQMEJQNqQE+4AdeaJ1EZYEAIAQKemUChUKpVSqVQoFL169Vq6dOmyZctuq4aqqireXVV3WWlpaeve3NxN90SQbXR3+shtJTg4mP6MoRxVVVVRUVHt1XR7cYeJ0YFuN7287+DBV/LcaW4y/5X35XVPAAC4n6xateovf/mLj48PIaSpqWnbtm0zZ86srq5euXLlr7/+qtFonn/++V9++SUxMdFqta5fv95oNObn51dXVz/wwAPV1dXffvttdnZ2SEiI2Wzu2bOnYBleVcOGDZNTOQ2vqanpjTfeOHHiRHV1tU6ns9vtI0eOZIM/d+7c2rVrL1++rFKp6G+sCdZTU1Pz/vvvFxUVBQYGpqSkLF68eOjQoU6nc/369e+9915TU1NMTAyNs6CgwOFwHDhwYNWqVb179+7bty9tSE6/EhMTS0pKzpw5o9PpPv/882HDhtGscoN0Op1Lly69du3aY4895nQ6t23b9swzz9DffJOZc0LIkSNHCgsLdTrdgQMHHnnkEaVSyR1Q1xjk1yxdDw3yzTffdI3fdSCam5s3btxoNBqLi4vPnDlDfxRRzkBItM5thRDy2Wef7d69m/3VdbvdvnHjRr1eX1NTc+rUKe7yT6/Xb9q0KS8vLyQk5MKFx35XbgAACvBJREFUCxcuXBg1atT8+fO9vb0Fk+OaYdcO0prb0iPXOgW7UFtbe/r06cjIyNTU1CVLlvj7+7Nda5fpwc694uLi7t27b9q0acmSJRqNhi62pU9ASiJCXnPe3t7btm2LiYl57LHHBgwYUFdX9+677zY1NUVGRtpsNokJKREhIURwsokNmVgSamtrT506RX8v3nWT29nc3FxfX9+DBw+uWLGiT58+9FVCcCwEd0r3hdUuk0GsEtq18vLyefPmKRSK7t27+/n5CdYgcwLwctKnT5/t27c/8cQTw4YNoz98/+WXX16+fPl3v/sdu8x2HaDb7R13egi+LomdDjJfz0l7YQAAADo3Pz8/g8FAH69evXrbtm30cU5OzpQpU86ePWuz2fR6PcMwCQkJbMlFixbpdDqGYbRa7Zw5c9jaBMu4ViWzcoZhxowZc/HiRfp4y5Yt//jHP3jxb968OTU1VToAhmGys7PHjBlz7do1hmF0Ol1kZGRpaSnDMA6HIzAwsLm5mWGYvLy8+Ph4p9PJMIzdbo+MjKytrZXfL7vdrtFoHA4HwzCFhYVLliwRDDItLS0lJYU+NpvNwcHBbDE5aWlqakpKSqJ7fv755/r6em42xGKQn3DpeiTi5w1EXFxcXV0dfbxz5072EDkDITOT+fn5s2bNYp+aPn16WVkZfZyUlHT58mVuj3gTdeDAgSaTSTA5YhnmdZDVlh7x6hTsglarHTt2bE1NDcMwzc3Nfn5+dIoy7Tc9aBJiY2PphLdYLIGBgbSVFk9AiQjFmnvyySfZOvfu3SvWF14rYhEy4pNNcMjEotJqtS+//DK3U9xN7v4JEyawHRw8eDDtuGD8Yp2S6Aur7ZNBohLatV9//ZV9VqwGmRNAMCejR4+m5wLDMBkZGa4H8gbodnvHJfi6JHY6yPx3qr3gQjMAANwDMjMzf/7555SUlKioqBkzZtCd3t7eDQ0N0dHRKpWqZ8+elZWVFoulW7du9NmIiIiTJ0/y6hErw6tKfuWVlZX19fX0c1+EkBa/fU4iSF9f3+jo6JCQEFqPwWCgf2P28PAYMmSIyWSiUYWFhSkUCkKIUqmcN29eenq6/H4plUq9Xk///m21Wq9evSoYJO+3wrmfvZGTFqVSefTo0YKCAoZhnnrqqcDAQG7lYjHc7mhK9EUiflZFRUVDQ4NGo6GbcXFxixcvlj8QMjPp5+fHtl5TU1NeXh4REUE3v//+e9drnlz0U6+CyZHOsKv26pFEFwYPHkwnv1qtDgoKMpvNRGRatm560CRERUV1796dEOLl5eXt7W21WuWfgIIRijX39ttvf/vtt4QQp9NJJ4mclxfBCInkZBMkc3ZJ6Nu3L/sq8corr6SnpwvGL9Epsb6w2mUySJ8Uly5d2rt3L/usWBdkTgDXnBBCli1b9s0339AC9DJsi+T3jneg4OuS4Okg/9+p9tIxHzkFAAC4LePHj9doNH/4wx+4OxUKBb3zitLr9Waz+dixY3QzKiqK+6x0GV5V8ivX6/WDBw+W3xGJINl3/5TD4eBu0q9X5ZUJCws7f/78bfWrubn5p59+6tOnD70uJBgk96tceWXkpEWtVufn5y9YsKCwsPCjjz6aOXMmr37BGFoxmmJ9kYifW394eDi76ePjYzQarVarWq2WMxASrXNx3//pdDr2bSsRWaaK4SWnxQy7Hs7dbHWPZHbBw8ODHt6O08O1F9TtnoC8CMWaGzdu3OzZs997772SkpKhQ4eK9YVXrWCERHKyiUUoZ3bJ1KtXr+LiYsH4y8rKZL4cuWqXyVBWViZWycmTJ+vq6j7//POFCxfSLwMTq6EVE4DmhBDy1FNPzZ49e/ny5Xq9nr13Wj7p3vEKC74uCZ4O8l/P2wsWhAAAcA8Qe3fCfasUEhLS1NQ0evRoXhl6S490GSK0cpBTuUaj0el08jrRQgDcFnnBSKxq+vfvL10t91iLxZKYmHjo0CEPD4/y8nKxIOlNRBLPttidfv36paenG43GGTNm9OrVKzY2Vk4McmqWWU+L76F79OhRWVnJrc3f3599g97iQMjMJJdGo9FqtRIFuBOVEELvGHRtmpLIsKB26ZFYF5xOJ/fNLrvgbN/pwQvebrczDCPzBBSLUKw5T0/PWbNmFRQU2O12+q2SLU5IsQhJS5PNlVhUDMPwFhWCP8PDG2K9Xh8RESEYf0NDg5yXI25fWO0yGYxGo1glMTExI0eO/PTTT997770PP/xQogY5E0AwJ4QQlUqVnJycl5fHMEyfPn1arOd2pzovBsHXJdfT4bb+nWoXuGUUAAA6O947Y+5+u93OboaHh6tUqrNnz9JNhmHovXDcYnLK3Fbl9NsIqqqq6P6amhrXONVqtcVika6H1yLDMNzLONwMGI1GdufGjRvpV0rI7FdFRUVoaCi9bFVSUsJtghtk79692fudeDc+yUnLhQsXCgsLCSGBgYHJycm8xIrFIDPhcvoiFj+3j/369SOENDQ00M1jx46tWLHCNRKxgZCZSYfDwb59jIiIUCqVeXl57FONjY0SubXZbOyg854SyzC3abGab7dH3DrFuiAWeTtOD9dW6BJFzgkoEaFEc/Pnz1+1alVwcDDdbHFCikVIJCeb4JCJRWW323n187JHOZ1OvV7PhrRhw4bJkycLxi/z5YgILQjbZTJIVEJ7PW3atP3791dXV0vUIGcCCOaEbs6dO/fDDz8U+3FU3gDdVu94VQm+LgmeDvL/nWovWBACAEDnZbFYDh06ZLPZduzYwfsgjclkOnDggFarzc/Pp3sUCkV2dnZKSsprr732+eefp6enBwQEGAyGjIyMkpKS3NxchmEEy7hWJbNyuv/48eN//etfP/jggx9//PHWrVtZWVnZ2dncUMeNG7d9+/YtW7ZUV1eL1WMymbKysoqLi69evWq32zMzMy0WC33TcOrUqaqqqszMTPpWQKVS7dmzZ+3atTNnzty6dSv9YJLMfg0YMKBLly6LFi3atm3byJEjb9y4kZGR4RrkqFGjampqZs6c+eWXX0ZGRt68eTMzM1N+Wnr06JGXl3f8+PEjR44YDIZx48ZxsyEYg/yES9dDnxKLn9tHQkhWVtZ33313/PjxrKys69ev02/8kzkQcjJ56dKl9PT0kpKSsrIy2qOcnJxNmzYtWLBg7dq1mZmZ3Peg7EQ9c+YMISQ3N7e+vj4jI8PhcLgmRyzDvA6yk7ktPeLNXtcumEymjIyMc+fOXblyhRBy/PjxxsZGGnl7TQ9eLwghOTk5TU1NBw8eZBimxRNQIkKJcRw0aFB9fT371ZotTkixCOlfBAQnm9iQCUZFZ8j58+eLiorYCcNusmprazdv3vzggw/u37//yy+/nDVr1nfffRcUFCQYv5yXI9e+sAlp+2SQruTSpUsKheLbb7998cUXS0pKxGpocQKI5YQ+Sz+MJ3YrJneAbrd3vKoEX5cETweZr+ftSHGHrjwCAABAu9NqtampqZ999llHBwIAcJ/IyckZNWpUR0fRkXCFEAAA4J5ht9ttNltHRwEAcJ+orq4eMmRIR0fRwbAgBAAAuDcYjcb9+/cXFRXl5uZ2dCwAAPcwp9PZp0+fTz755MqVK+zto24Lt4wCAAAAAIB7uXXrFsMwYl8n41awIAQAAAAAAHBTuGUUAAAAAADATWFBCAAAAAAA4KawIAQAAAAAAHBTWBACAAAAAAC4KSwIAQAAAAAA3BQWhAAAAAAAAG4KC0IAAAAAAAA39f8AmUCkxpi3VLMAAAAASUVORK5CYII=)

`figures/png/summary/Atlantic__Eastern_Central_mtc.png`

``` r

show_table(mtc$species[mtc$species$used, c("species_name", "match_type", "temp_pref_mean", "catch_pct",
                                           "weight_pct", "contribution_c")], n = 8)
```

|  | species_name | match_type | temp_pref_mean | catch_pct | weight_pct | contribution_c |
|:---|:---|:---|---:|---:|---:|---:|
| 1 | European pilchard(=Sardine) | exact | 10.3 | 19.84 | 32.67 | 3.37 |
| 5 | Round sardinella | exact | 18.8 | 6.19 | 10.19 | 1.92 |
| 6 | Atlantic chub mackerel | exact | 22.5 | 5.91 | 9.74 | 2.19 |
| 7 | Bonga shad | exact | 26.4 | 4.94 | 8.13 | 2.15 |
| 8 | Skipjack tuna | exact | 26.2 | 3.34 | 5.49 | 1.44 |
| 9 | European anchovy | exact | 10.8 | 2.90 | 4.78 | 0.52 |
| 10 | Yellowfin tuna | exact | 26.7 | 2.71 | 4.46 | 1.19 |
| 11 | Madeiran sardinella | exact | 21.0 | 2.65 | 4.37 | 0.92 |

`match =` takes a data frame or the path of a CSV file: the table saved
by
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md),
or the species table of an earlier run of
[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md),
after checking it. Items marked in `checked` count as checked; until all
items used are checked, the function says how much of the catch has been
checked, and the chart caption says that the values are automatic first
matches. Without `match =`, the default `version = "latest"` downloads
the tables it needs the first time (about 23 MB):

``` r

mtc <- mean_temperature_catch(FAO34$area_data)                         # newest FishBase release
mtc <- mean_temperature_catch(FAO34$area_data, version = "builtin")    # built-in table, no download
mtc <- mean_temperature_catch(FAO34$area_data, match = "tables/Atlantic__Eastern_Central_fishbase_match.csv")
```

The article [🌡️ Sea temperature and catch
temperature](https://linbojun99.github.io/faofishingreport/articles/sst-mtc.md)
explains the SST data, the trends, and the limitations of the preferred
temperatures.

## 📉 Use in the CMSY++ priors

CMSY++ needs a prior range for the intrinsic growth rate r of each
stock.
[`cmsy_priors()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_priors.md)
searches a second built-in table (`inst/extdata/cmsy_priors.csv`, made
by `data-raw/cmsy_priors.R`) with the resilience (table `stocks`) and
the prior r (table `estimate`) of the species-level items of the FAO
capture statistics in FishBase or SeaLifeBase 26.06. It works offline:

``` r

p34 <- cmsy_priors(c("PIL", "SAA", "VMA", "BOA"))
show_table(p34[, c("species_name", "matched_name", "match_type", "resilience", "prior_r",
                   "r_low", "r_high", "r_source")], digits = 3)
```

| species_name | matched_name | match_type | resilience | prior_r | r_low | r_high | r_source |
|:---|:---|:---|:---|---:|---:|---:|:---|
| Atlantic chub mackerel | Scomber colias | exact | Medium | 0.597 | 0.394 | 0.895 | FishBase prior r, 95% CL (Based on 6 data-limited stock assessments.) |
| Bonga shad | Ethmalosa fimbriata | exact | High | 0.228 | 0.151 | 0.342 | FishBase prior r, 95% CL (Based on 1 full stock assessment.) |
| European pilchard(=Sardine) | Sardina pilchardus | exact | Medium | 0.628 | 0.415 | 0.942 | FishBase prior r, 95% CL (Based on 26 full stock assessments.) |
| Round sardinella | Sardinella aurita | exact | High | 0.651 | 0.429 | 0.976 | FishBase prior r, 95% CL (Based on 1 full stock assessment.) |

The r range follows the CMSY++ user guide (Froese et al. 2021): the 95%
confidence limits of the FishBase prior r when the species has one
(Table 1); otherwise the range of its resilience (Table 2: High 0.6–1.5,
Medium 0.2–0.8, Low 0.05–0.5, Very low 0.015–0.1); otherwise none, and
the prior has to come from the user. This table is matched more strictly
than
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md):
fishes are looked up in FishBase first and other items in SeaLifeBase
first, as valid species names and then through the synonym table of the
database, but there is no WoRMS look-up, and ambiguous synonyms and
items above species level get no values (`match_type` “ambiguous
synonym”, “not a species” or “not found”). The corrections file of
[`fishbase_match()`](https://linbojun99.github.io/faofishingreport/reference/fishbase_match.md)
does not change it; priors of your own are given with `priors =` in
[`cmsy_input()`](https://linbojun99.github.io/faofishingreport/reference/cmsy_input.md).
See [📉 CMSY++ stock
assessment](https://linbojun99.github.io/faofishingreport/articles/cmsy.md),
step 3.

## 📝 In the Word report

The Word report of an area can include a section on the mean temperature
of the catch (`fao_report(34, mtc = TRUE)`). It uses the built-in
FishBase table by default (`mtc_version = "builtin"`), so writing the
report downloads nothing; give another release with `mtc_version`,
e.g. `mtc_version = "latest"`. The table of species used, with their
FishBase matches, is returned as `report$mtc_species`, and the report
cites the FishBase release used. See [📝 Word
report](https://linbojun99.github.io/faofishingreport/articles/report.md).

``` r

report <- fao_report(34, mtc = TRUE)
head(report$mtc_species[, c("species_name", "match_type", "temp_pref_mean", "used", "url")])
fao_report(34, mtc = TRUE, mtc_version = "latest")    # newest FishBase release
```

## 📖 References

Boettiger, C., Lang, D.T. and Wainwright, P.C. (2012). rfishbase:
exploring, manipulating and visualizing FishBase data from R. *Journal
of Fish Biology* 81(6): 2030–2039.
[doi:10.1111/j.1095-8649.2012.03464.x](https://doi.org/10.1111/j.1095-8649.2012.03464.x).
Source code: <https://github.com/ropensci/rfishbase>

Branch, T.A., Watson, R., Fulton, E.A., Jennings, S., McGilliard, C.R.,
Pablico, G.T., Ricard, D. and Tracey, S.R. (2010). The trophic
fingerprint of marine fisheries. *Nature* 468: 431–435.
[doi:10.1038/nature09528](https://doi.org/10.1038/nature09528)

Cheung, W.W.L., Watson, R. and Pauly, D. (2013). Signature of ocean
warming in global fisheries catch. *Nature* 497: 365–368.
[doi:10.1038/nature12156](https://doi.org/10.1038/nature12156)

FAO (2026). *Nomenclature, revision, and references of the ASFIS list
(version 2026)*. Rome: FAO.
<https://www.fao.org/fishery/static/ASFIS/ASFIS_References_2026_EN.pdf>

Froese, R. (2020). Personal communication: R code (PrefTempBatch_5.R) to
estimate preferred temperature from AquaMaps (version 10/2019). FishBase
reference 123201.
<https://www.fishbase.se/references/FBRefSummary.php?ID=123201>

Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). *User Guide for
CMSY++*. GEOMAR, Germany, 17
pp. <https://oceanrep.geomar.de/id/eprint/52147/>

Froese, R. and Pauly, D. (Eds.) (2000). *FishBase 2000: concepts, design
and data sources*. ICLARM, Los Baños, Philippines. Chapters “The
SYNONYMS table”
(<https://www.fishbase.se/manual/fishbasethe_synonyms_table.htm>) and
“The FOOD ITEMS table”, boxes 25 and 26
(<https://www.fishbase.se/manual/fishbasethe_food_items_table.htm>).

Froese, R. and Pauly, D. (Eds.) (2026). FishBase. World Wide Web
electronic publication. Version (06/2026). <https://www.fishbase.se>

Grenié, M., Berti, E., Carvajal-Quintero, J., Dädlow, G.M.L., Sagouis,
A. and Winter, M. (2023). Harmonizing taxon names in biodiversity data:
a review of tools, databases and best practices. *Methods in Ecology and
Evolution* 14: 12–25.
[doi:10.1111/2041-210X.13802](https://doi.org/10.1111/2041-210X.13802)

Liang, C. and Pauly, D. (2017). Fisheries impacts on China’s coastal
ecosystems: unmasking a pervasive ‘fishing down’ effect. *PLoS ONE* 12:
e0173296.
[doi:10.1371/journal.pone.0173296](https://doi.org/10.1371/journal.pone.0173296)

Liang, C., Xian, W. and Pauly, D. (2018). Impacts of ocean warming on
China’s fisheries catches: an application of “mean temperature of the
catch” concept. *Frontiers in Marine Science* 5: 26.
[doi:10.3389/fmars.2018.00026](https://doi.org/10.3389/fmars.2018.00026)

Palomares, M.L.D. and Pauly, D. (Eds.) (2026). SeaLifeBase. World Wide
Web electronic publication. Version (06/2026).
<https://www.sealifebase.se>

Pauly, D., Christensen, V., Dalsgaard, J., Froese, R. and Torres, F.
(1998). Fishing down marine food webs. *Science* 279: 860–863.
[doi:10.1126/science.279.5352.860](https://doi.org/10.1126/science.279.5352.860)

Tsikliras, A.C. and Stergiou, K.I. (2014). Mean temperature of the catch
increases quickly in the Mediterranean Sea. *Marine Ecology Progress
Series* 515: 281–284.
[doi:10.3354/meps11005](https://doi.org/10.3354/meps11005)

WoRMS Editorial Board. World Register of Marine Species. Flanders Marine
Institute (VLIZ). <https://www.marinespecies.org>,
[doi:10.14284/170](https://doi.org/10.14284/170)
