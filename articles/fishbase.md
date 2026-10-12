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

fb27 <- fishbase_match(FAO27$area_data, version = "builtin", corrections = FALSE)
#> FAO species items: 1029; matched 1001, failed 28, pending online check 0. Matched items cover 98.3% of the catch.
```

`corrections = FALSE` leaves out any corrections saved on this computer,
so the example gives the same result everywhere. The items are sorted by
catch, so the first rows cover most of it:

``` r

main <- head(fb27, 15)
main$page <- ifelse(is.na(main$url), "", paste0("[open](", main$url, ")"))
show_table(main[, c("species_name", "fao_scientific_name", "catch_pct", "match_type", "database",
                    "temp_pref_mean", "trophic_level", "page")], n = 15)
```

| species_name | fao_scientific_name | catch_pct | match_type | database | temp_pref_mean | trophic_level | page |
|:---|:---|---:|:---|:---|---:|---:|:---|
| Atlantic herring | Clupea harengus | 18.55 | exact | FishBase | 6.6 | 3.38 | [open](https://www.fishbase.se/summary/Clupea-harengus.html) |
| Atlantic cod | Gadus morhua | 13.94 | exact | FishBase | 6.6 | 4.09 | [open](https://www.fishbase.se/summary/Gadus-morhua.html) |
| Capelin | Mallotus villosus | 10.02 | exact | FishBase | 2.7 | 3.15 | [open](https://www.fishbase.se/summary/Mallotus-villosus.html) |
| Blue whiting(=Poutassou) | Micromesistius poutassou | 7.09 | exact | FishBase | 7.0 | 4.13 | [open](https://www.fishbase.se/summary/Micromesistius-poutassou.html) |
| Atlantic mackerel | Scomber scombrus | 6.19 | exact | FishBase | 10.2 | 3.63 | [open](https://www.fishbase.se/summary/Scomber-scombrus.html) |
| Sandeels(=Sandlances) nei | Ammodytes spp | 4.65 | genus | FishBase | NA | 3.16 | [open](https://www.fishbase.se/identification/SpeciesList.php?genus=Ammodytes) |
| European sprat | Sprattus sprattus | 3.95 | exact | FishBase | 9.5 | 3.01 | [open](https://www.fishbase.se/summary/Sprattus-sprattus.html) |
| Saithe(=Pollock) | Pollachius virens | 3.85 | exact | FishBase | 7.0 | 4.31 | [open](https://www.fishbase.se/summary/Pollachius-virens.html) |
| Haddock | Melanogrammus aeglefinus | 3.56 | exact | FishBase | 8.4 | 4.03 | [open](https://www.fishbase.se/summary/Melanogrammus-aeglefinus.html) |
| Norway pout | Trisopterus esmarkii | 2.11 | exact | FishBase | 7.8 | 3.68 | [open](https://www.fishbase.se/summary/Trisopterus-esmarkii.html) |
| Brown seaweeds | Phaeophyceae | 2.11 | plant (TL 1) | SeaLifeBase | NA | 1.00 | [open](https://www.sealifebase.se/Nomenclature/ScientificNameSearchList.php?Genus=Phaeophyceae) |
| European pilchard(=Sardine) | Sardina pilchardus | 1.67 | exact | FishBase | 10.3 | 3.06 | [open](https://www.fishbase.se/summary/Sardina-pilchardus.html) |
| Atlantic redfishes nei | Sebastes spp | 1.62 | genus | FishBase | NA | 3.70 | [open](https://www.fishbase.se/identification/SpeciesList.php?genus=Sebastes) |
| Atlantic horse mackerel | Trachurus trachurus | 1.54 | exact | FishBase | 13.3 | 3.71 | [open](https://www.fishbase.se/summary/Trachurus-trachurus.html) |
| European plaice | Pleuronectes platessa | 1.35 | exact | FishBase | 10.2 | 3.17 | [open](https://www.fishbase.se/summary/Pleuronectes-platessa.html) |

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
round(sort(tapply(fb27$catch_pct, fb27$match_type, sum), decreasing = TRUE), 1)
#>        exact        genus plant (TL 1)  not matched       family        order 
#>         86.3          7.8          3.0          1.7          0.8          0.2 
#>        class       phylum      synonym 
#>          0.1          0.0          0.0

# Share of the catch (%) of species-level items with a preferred temperature
species_level <- c("exact", "synonym", "WoRMS valid name (exact)", "WoRMS valid name (synonym)")
round(sum(fb27$catch_pct[fb27$match_type %in% species_level & !is.na(fb27$temp_pref_mean)]), 1)
#> [1] 86.2
```

In area 27, matched items make up 98% of the catch. Genus and family
items have a trophic level only; species-level items with a preferred
temperature make up 86%, and this is the part of the catch that the mean
temperature of the catch can use. The coverage also changes from year to
year;
[`mean_temperature_catch()`](https://linbojun99.github.io/faofishingreport/reference/mean_temperature_catch.md)
shows it for each year (see [below](#mtc)).

### Check and correct the matches

The matches are made automatically and are not checked by the package.
Names do not always agree between databases: FAO ASFIS names change
slowly, FishBase, SeaLifeBase and WoRMS follow different
classifications, and synonyms and misspellings are common (Grenié et
al. 2023). Before using the values:

1.  Open the table saved in `tables/` (here
    `tables/Atlantic__Northeast_fishbase_match.csv`). The items are
    sorted by catch, so the first rows matter most.
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

fri <- fb27[fb27$alpha3 == "FRI", ]
fix <- data.frame(alpha3 = "FRZ", species_en = "Frigate and bullet tunas",
                  trophic_level = fri$trophic_level, temp_pref_mean = fri$temp_pref_mean,
                  note = paste("Values of Auxis thazard,", fri$source, "SpecCode", fri$SpecCode))
write.csv(fix, "my_corrections.csv", row.names = FALSE)

fb27_fixed <- fishbase_match(FAO27$area_data, version = "builtin", corrections = "my_corrections.csv")
#> Applied 1 correction(s) from my_corrections.csv
#> FAO species items: 1029; matched 1002, failed 27, pending online check 0. Matched items cover 98.3% of the catch.
show_table(fb27_fixed[fb27_fixed$alpha3 == "FRZ", c("species_name", "status", "match_type", "trophic_level",
                                                    "temp_pref_mean", "rule_source")])
```

|  | species_name | status | match_type | trophic_level | temp_pref_mean | rule_source |
|:---|:---|:---|:---|---:|---:|:---|
| 172 | Frigate and bullet tunas | matched | manual | 4.37 | 22 | Manual correction (my_corrections.csv): Values of Auxis thazard, FishBase 26.06 (built-in table) SpecCode 94 |

With downloaded tables, a correction can instead name the record to use,
and all its values are read from it. The built-in table cannot look up a
`SpecCode`, so with `version = "builtin"` give the values themselves:

``` r

fix <- data.frame(alpha3 = "FRZ", database = "FishBase", SpecCode = 94,
                  note = "Record of Auxis thazard; give your source here")
write.csv(fix, "my_corrections.csv", row.names = FALSE)
fishbase_match(FAO27$area_data, version = "26.06", corrections = "my_corrections.csv")
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

mtc <- mean_temperature_catch(FAO27$area_data, match = fb27_fixed)
```

![Atlantic\_\_Northeast_mtc.png](data:image/png;base64,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Udm5c+e+/vrrLl26CCGOHTvWrVs3IUSHDh2+/fZbuUJeXl737t1193LbtGnTvn377du3/5RNAwAahIAQAJpHRUXFxo0bb775Zu2Npmuuueaaa64RQmRmZq5cufKLL7645ZZb/vWvf8ml+/bte+ONN4QQq1at2rlzpxCirq7urbfeevHFF7ds2fLiiy9qv9VWUlIyf/78vn373nXXXbpvuymKsmTJkk6dOj3zzDOlpaVlZWUzZ85MS0s7duxYZWXlokWLFi1alJOT884770ydOvXcuXMuN13vViSXO6IqLS19//33b7755l27ds2fP3/gwIGPPfaYLOfcuXNLly6VXwj8/e9/P3DgwLNnz9bW1r766qv/+te//va3v73yyivyBtfp06f/8Y9/CCH+9re/rVmzRgjhcjXnAg8cOOC8CXfZKysrZ8+efc0118yfP//AgQPOe+qyiZyLKi4ufu+9926++eaCgoK5c+f27Nlz9uzZdXV1S5cuveaaa8aOHSsb3HPjuCzZZYu57AJdVXNzcx966KFBgwbV1dU5HI73338/OTlZNmmDOkJr48aNN954o3q8yQmTySSnHQ7HV1991bNnT+dmnDx58ooVK5zTAQC+ovjGhQsXRowY4aPCAaAVOHDgQEJCwrvvvuu8qKqqqmPHjrt371YU5csvv0xISCgrK5OL5s6dm5CQUFdXJ2f/7//+74477pDTt99++1NPPaUoysqVKxMSEjZu3OhwOHJzcxMSEv75z3/qNlFXV9etWze5vqIot956a1JSks1mUxTlk08+sdvtiqK89dZbCQkJhYWFLjftzVY87Ihkt9sXLlyYkJBw8uRJRVHKy8uTkpIeeughRVEcDsdrr72WkJDw2muvnThxYvr06fn5+ZMnT37++edl3jvuuGPevHnayhQXF8uMLldzWaAupaCgwGV2h8ORlpZ2/fXXyyZ69913ExISCgoKdPurayKXRdXV1c2fPz8hIWHTpk0Oh2PXrl0JCQnz5s2z2Ww2m61r166PPfaYN43jspLOu+OuC3RVnTNnjjpbWFiYkJDw4YcfqnvhfUeoZsyY8e9//1tOnz179vbbb1cUZcWKFRs2bFAUZc2aNTab7fjx4ytXrlyyZElJSYma8cCBAz169FAAAO6lp6efPXu2sUrjDiEANA8PX98KDAzctGnTwIEDxaU3f7h800ZW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bLSqqqpDhw6pa8bGxnbo0EGdfeutt3bt2qXODhw40GKxyOmLFy8ePnxYTs+aNWvMmDE///nP1TWPHz9+/vx5dXbQoEEmk0mtzIIFC+QOCiHi4+MTExPVNY8dO1ZRUSFcqaioOHbsmDqbmJgYHx+vzh49elTdweLi4quuukpddP78+ePHj6uz7du3j4uLU2cPHz588eJFOW2xWAYOHKguKisry8rKkjsohJg2bZq2PgcPHqyurpbT/v7+2u7YuXPnu+++q/Zy586do6Oj1aUHDhyoqamR088+++ycOXPURUVFRadOnVJnk5KS2rZtq84++eSTsnEKCwuDgoJ69+6tLjp37tyZM2fU2a5du2qrunfvXrvdLqeDg4N79eqlLiooKMjJyVGbLiIiQjs69uzZo95jb9OmTY8ePdRFn3zyifbY6N69e1hYmDqrLpo1a1ZycvKDDz6oLsrLy8vLy1Nne/To0aZNGzlts9lmz54tLh0b4eHhycnJ6pq5ubnam/k5OTnqsaobcREREd26ddOuqR1W2hFns9n279+vrqkbcbqh2rdv34CAADmtjjhZ8tVXXy005IhTZ/v37+/n5yenCwoKXnjhBbXYmJiYTp06ucw4a9asefPmqd9w1o444TRU33zzTTmQZclXXXWV2fzjJ2WVlZVHjhxR19SOGiFEZmZmeXm5Ojt48GB1Wo44ten8/PwSEhLUpdqhajKZBg0apC46cuSI9thwN+JmzZoVGBj48ssvq4vUESd17NixXbt26uxLL7109uxZuYN+wApJ/AAAIABJREFUfn79+/dXF5WUlJw4cUKd/fbbb4cPH67OaodqQEBA37591UVFRUXaY0M34vbv32+z2eR0YGBgnz591EWFhYWnT59WZ7t27RoZGanO7tu3r66uTu5jYmLio48+qi6SI06d7datW0REhDr78MMP19bWyn0MCQnp2bOnuig/Pz83N1edPXr0qHY87t69W/30MzQ0NCUlRV2Ul5en3UftiFMUZffu3eqauhH3ww8/nD17Vp3t2bNnSEiInLbb7Xv37hWXjn9tkwohcnJytGdV7cmxoqLiySefVI9/DyNu1qxZTz31lHoA1NbWfv/99+qaunPcihUrduzYIS4d//369fP395eLqqurDx48qK4pHw5SeRiq8uSoNp3ZbNaOuKysrLKyMnVWe3I8ffq09vjX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ynVcHqjujt7g4GB5c9Jqtfr5+cmlgwYNqq6u9raNALRe9TwnAKA1UBTb2bPOybn3zLCdOCUUR/W+/bm/nN75809MTtde1pgY06WrBw+mTp26fPnywsLCmJgYmXL8+PFevXplZWWp69TW1i5dujQ5Obm4uLht27bjx48XQhw+fDg3N9fPz2/dunVPP/10ZGTk2bNnFyxYEBsbe+ONN65Zs2bt2rUPP/zwDTfcoN3coUOHli5dWl1dHRMTM3LkyLZt27osPD8///XXX4+JiZkwYYIs6s0334yJiXnvvfc+//zzm266aebMmXJz4eHhw4cPP3r06Gefffa73/1uxIgR7uos14+Li5s4ceKsWbMSExOXLVvmvBe6Snbv3v31118fOnTor371K4fDsWrVqjlz5hw/flwI4bJAl7ujVVNTs2zZsuTk5MrKyqqqqltvvVUWtXLlyj179nz44YedOnUaOHCgu+aS6SdOnFi9evXatWtnzZo1depUD92kVVdX99ZbbyUkJISHhxcWFsqMzi3gvFGXGT3UxIM6u1J0ocYp0XH/33dV1tqFEN9lFf32vT3LfzXsf59iFkKIdmEBZudUDYvF0rlz53Xr1k2ePFkIocZj7lrGZe+rtmzZUlxcHBISkpWVNWPGDH9//7Nnz86dO9f5YHA+Ep5//nnnDnW5OZdt67krvRlE3owgcekYdjmIXNbBy0Gkq2Ftba3LdnNXoMtNnzlz5ocffrjmmmuc+713797Lli274447PvroI216bW3t3//+9yVLlmgTU1NTb7rppj/+8Y9m848frDscji5dumzbtq2mpsY5iPW8VD3w5ERVVVVgYOCjjz4qZ00mk/o0fm1trTp+3R2oLhOffPJJ5y0OHjxYnVYUZeXKldOnT1+9evWECROKi4vnz5+fmpp64403uqwtgFaluW9RAvC9uroj8e0v71/VwYP1Fn/+/PnDhw+/8847L730kky5cOHC/v37V61a9eKLL6qrpaenFxcXy+nZs2efOXOmrq6ubdu2drtdUZTdu3c/9thjcunWrVvT0tIKCgoURamuro6IiHA49A8B/v3vf1+2bJmHwuX0119/PWrUKPlY1JkzZ1JSUrKyshRFsdvtERER8hmw77777rrrrpObqKurS0lJkZt2V+zWrVtvuOGGffv22Wy2vLw8d3uhq+SqVauWLl0qp6uqqqKjo9XVdAV62B3V2LFji4qK5PTq1avVkvfv35+RkeHcR7rm2r9//7XXXpufn68oSk1NTUhIiNrC9W76//2//5ednS2np02bdvz4cXctoNuoc0bPNfHgxLkLfZ7ccHn/LtbU1Vt+fn5+cnJyenr6d999pya6bBkPva8oysWLF6dNmyanv/zyy9LSUjnt7mBwPhJ0Hepucy7btt6u9GYQeTOCFPeDyF0dvBxEuhr+9EG0dOnSKVOmKE6Ki4uPHTsmW2/16tUycc2aNXKvg4ODdevL23dq+WVlZSdPnlQUZe7cuV9++aVuZc9LVSEhIbLAjRs3VldXa9PLysoURSktLc3IyPjhhx/URS4PVJeJqszMzKFDh7qrw4gRI2Q7KIqydu1aOfGPf/xj1apVixcvzsjI+P77793lBXDl4g4hYAAmU+jkSfpEW92FTz9VHI4fZ82mkJEjzWFhurUsYeHebMHhcEydOrV9+/a///3vLRbL119/nZ6efvjwYXWF3Nzc6urqqKgoOdulS5ft27dPnTo1Ly9PfsReW1t7+vRpuTQwMLBXr17t2rUTQgQEBAQGBtbW1np4csxl4R06dBBCBAcH9+/fX963jI2NLS4u7tq1qxDCbDb37t37/PnzMTExgYGBiYmJ8jN4i8UyY8aMjz/+OCMjw12xgYGB5eXl8lUT8gN4l3vh3ETqzQQhhPZFDroCPeyOlJOTU15ert4oGDt2bMeOHe+++24PHeSsR48e8s0Q/v7+kZGRVVVVwcHB9W46Pz//xIkTXbp0kbPvv/++xWIxmUz1toDLjB5q4rnyIQGW8X3idImVNXXfZhWpL+YwmUzX9mjnZ9HfDDSbPd0elGJjY48cOfLFF1/cd9998fHxH374YUFBgcuWsVgsHvbdYrF89dVXO3fuHDx48M9+9jM13d3B4Hxo6bjcnMu2rbcrddyt780IkjV3HkTjx493V4fmGkR33323u5GiKIoQ4u23305OTh49enRUVJR6X87k8ZayEGLv3r3XXnutEGLatGnz58/X9nW9S7U++uijDh06rF+/XrfO+vXrIyMj33rrrQULFiQkJKjpzgeqn5+fy0TP9Vc98cQTf/vb31544QWHw6H+hZk2bZqcyMjI6Ny586FDh5rxIWoAvkBACBiA2Zz4pou3OBY8+VTZe6uEvU74+wX27dth5XuXV7y8kAoJCbnlllu+/PLLcePGORwOk+l/XmKcl5dXVVX1zTffyNlevXr17dtXCFFTU7Nx48YOHTrIWw1yab2XXzruCncuym63a2fVb+loExMTE48cOeKhWJPJpJYvudwLHYcae19qMZWuQA+7o67QuXNndTYoKKisrKy2tvay34RpNptllerd9JkzZ7p3767Oqlfk9baAu4zuauJZu7DAV28Z6Jx+z9s79p0ps9U5LBbT5P6Jc6f0qbcodywWy/jx48eNG/fAAw8sXbp08ODB7lrGw777+/vv2LHj/vvv371798svv3zHHXfIdHcHg/Oh5cx5cy7btt6u1PFwtGtXczmCnFeTg8jzwGzeQeRM/Tu2atWqX/ziF59++qkMPtu1a1dZWakbXxUVFX5+fhEREXL22LFj6qL169fbbDZtDOZ5qdaNN94YFRXVsWNHbdwrhJg4cWJ4ePjgwYOHDx8un5JV6Q7UX//61+4SvTFu3Lhf/vKXf/jDHw4fPjxkyBDdUn9//6lTp27btu3666/3skAAVwQCQsC4Yp9/LuymKRXrPg5OTW3zs2t/eoG//e1vMzIy4uLiRo0apVsUExNz8eJF3e9PVFdX33jjjZ9//rnZbD5x4oR2kfZqr66uznOQ4LJw56J0hbgrMy8vLykpyXOx2rwe9kKXxcNeaBd53h0hRFxcXG5urrYCoaGh8mpVPrPnLqPK4XBor63Vq/x6N922bdv9+/frEr1pAZcZPdTk8rx515Bvs4r+c7x4XO/YgR0v8ybG+fPnL1y4kJiYKIQwmUz333//0qVLJ0yY4LJl6t33Tp06ffzxx2VlZbfccktCQkJaWprweDDo0nUd6nJzLtu23q70fv2GjiBxaRB5OTBFcwwiD0aNGpWcnPz+++/L+2ABAQFTp07dv3+/NkDauXPnzJkzZdhWVVU1ZMiQq666Si76xS9+sWvXLvVrip6XuiTD+6KioujoaG16TExMUFBQTk6OvMvq8kB1mej9vvv5+d155507d+6sq6uTUWttba3NZlN/LUP7ThoArQZvGQUMzGQKGjSo3dw5bcamiUvP7/0UKSkpFRUV//znP0NDQ8X/Xst27tzZarXu27dPziqKcv78+ZycnNjYWHlRdfjwYTUYsNvtdXV1arEuA0J/f3/1/XguC3cuSlEUbbwhv7Akp8vKytTEJUuWTJo0yUOxuuq52wtdJdu3b6/+pJj2t8WcC/SwO1KnTp2EEOrrEL/55pvnnnvOZVEua+K8ms1mk01R76a7dOlisVi+++47tZwLFy64awHtRl1m9FCTy2M2mVKTYx69vsdVnSIbeJv5v4qLi7/66it1du/evZMmTXLXMh56Xwhx9OjR3bt3i//f3r2HNXHmewB/Q8JdEAx4QytSFMVtsa2lasXTLihUqx63xXWPq7Y+vVH72FZr1z2rpz3VrfZytq5StbC4VbFej7ciBdGqCIhcxICCN1wNkoAaSSiGQC5z/nj3zDM7mRmGi4Lm+3n8IzN5531/7+99J+YlkwkhAQEBSUlJbE/FJoPz8MmZbIK5bXMoibyTSOYZRIROIpknpkQaefO28yeRVqs9ffo0acs333yzYsUKg8FANzdu3PjHP/6Rjcpqta5aterzzz+nm4WFhdyPHxMTE9PT09lN6Wd5XeBurlmzht3P/tHkzTffzM7Opo8FJ6rgTm61zP9/AVLM22+/vXr1anYtevXq1aVLl7KRZGZmsvfcAoBHBj4hBIBOMRqNe/fuvXnzpre3d1hY2H//93/TqxnLy8uPHDly586dU6dOxcTEKBSK3NzcP/zhDyqVKjw8PDQ09OWXX/b39+/Vq9eSJUvGjBkTHx+/du1a+m4jJyenoqLixo0bQ4YMycvLM5vNR44cmTZtGvcyqkmTJs2cOdPf3z82NnbAgAHOlRNCTCYTW1VISEh2drbFYjlz5sxzzz1XUlJSW1ubnZ09Z84cQohKpTp48GBNTU1BQcH27dvpl2cEYzaZTFlZWRqNpqioKDo6mhASHh7u3At6SRU3yAkTJvzlL3+ZO3fuuHHjZs6c+csvv2RnZ8fHxztXKNguL+05OTl///vfR48ebTabb9++vWjRIkKIwWDIzMysrKwsKCgYN24c9yo+biT099YuXLhw/fr10NDQ/Pz8pqamzMzMV155hX7/U6JphUKRl5e3ZMmSbdu2jRw5MiwsbMqUKWIZ4I2R84Emk0kikvsxXeUICQlpamrKzs5WKpV1dXW9e/emH+sJZkZi9Akh/fv33759u8VisVqtBoPhtddeo/sFJ8PYsWN5M8F5QMWac86tWMBcbZ5EMs8gepGq4EkkdmLKPIl4EXb+JMrJycnKytqzZw83DyaTae/evXV1dVOnTn3mmWcIIZ6engcOHGDX2EFBQbt37964cWNERATDMFeuXDl06BD9s1dZWVlqaqpCoaDfEqyvr7969er333//yiuvxMTEnD9/XuJZ9qM2i8WSl5dntVozMjIee+wxuvPChQvs/v/93/+dPn163759Z82alZiY6O/vn5iYKDhRW1paBGcvdfHixf379+v1+mPHjo0bN07wy7rDhw9vaGiIiIigm5GRkbGxsSUlJc3NzUVFRceOHWvzK74A8NBRdOZvsQAAjwCNRpOWlrZu3bruDgTgYYWTCADg4YVLRgHA1dlsNqvV2t1RADzEcBIBADy8sCAEAJdmNBoPHz5cXl5eUFDQ3bEAPJRwEgEAPNRwySgAAAAAAICLwieEAAAAAAAALgoLQgAAAAAAABeFBSEAAAAAAICLwoIQAAAAAADARWFBCAAAAAAA4KKwIAQAAAAAAHBRWBACAAAAAAC4KCwIAQAAAAAAXBQWhAAAAAAAAC4KC0IAAAAAAAAXhQUhAAAAAACAi8KCEAAAAAAAwEVhQQgAAAAAAOCisCAEAAAAAABwUVgQAgAAAAAAuCgsCAEAAAAAAFwUFoQAAAAAAAAuCgtCAAAAAAAAF4UFIQAAAAAAgIvCghAAAAAAAMBFqbo7AACAh9KNO/c6WcOQIN8uiQQAAACgw/AJIQAAAAAAgIvCghAAAAAAAMBFYUEIAAAAAADgorAgBAAAAAAAcFFYEAIAQE9nMpnq6+u7OwphDoejtbW1k5U8gA725BzK19jYqNfrJQo0NTU9sGAAAB4NuMsoAEAP1dTU9N1334WEhLi7u+t0urffftvDw8N5Z21tbXV1tfPhL7zwgkrVI17kd+3adeLEiY0bN3bs8IaGhrS0tOrq6g7XcF+lp6fPnTuXEHL79u2cnJydO3euXbs2LCyMLWC321NTUyMjI8vLy9944w0vLy9eDfI7qNPp9u/fHxUVdebMmddee02tVnd5Ez0QO39oLyoqKrZs2SJW2OFwnDp1KiYm5kFGCADwUMMnhAAAPdS///u/JyUlzZ49+5VXXunVqxf9eMd559/+9rdx48bFxcWFhIR8/PHHcXFxcXFx1dXVDQ0N3d2Df4qNjf3ggw/adcitW7eSk5Pp48DAwMmTJ7u7u9+H0Drr+PHjMTExCoWCEOLn5/e73/2utbXVze1f/m9ds2ZNfHz8xIkT58+fv2DBArqzAx1sbW19+eWXk5KSJkyY8N5778XFxTEM07VN9BDcyAln/tBeOC93ufz9/a9fv240Gu97lAAAjwosCAEAeiK73X727FkfHx+6mZCQEBgYKLjTx8fH19eXEKJSqdh3/M8884zFYumWyJ0FBQVFRES065CrV696enrep3i6isPh2Llz59ChQ+mml5cXXRlyP5h1OBzffvstLePn56fX600mE+lQB4uLi3/961/T1aanp+fAgQP/8Y9/dG0TPQQvct784a23nc2ePXvlypX3KzgAgEdOj7iaCAAAeJRKZWho6I8//jht2jRCyIABA+h+551/+tOfnA8fM2YMb49er1+7dm3//v2nTp26aNGikJCQtLS0ysrKmzdvuru7//jjjytWrAgMDKTF+vXrN2PGjP379x86dOijjz6aPn06raSqqur48eNPPvnk+fPnx4wZs2PHjv/5n/9pbW1NTU0dNmyYwWBQq9WTJ0/mtltfX//tt9+aTKa//vWv0pWzLly4kJqaarFYgoODY2Ji2Asjr127tm/fvkOHDi1atOjVV18lhEg3TQgR6yAvD2L1OB/Orby4uHjmzJnS46jX65966il2c+zYsVVVVX5+fjI7yNXc3MzdHDlypFarDQsL66omaGZ69+49fvz4ixcv5uTkLF68+Pnnn6fPOqdIMJM2m+1vf/vbwIEDe/fuffv2bbFhkpgJvNG32Wzs/HFOr+DAubu7NzU1WSwW6c8SAQDgnxgAAGi/67ebOvmvzSbq6uqGDRuWkJBQWFgovZO6fPlydHS0RIV5eXnTp08/d+6c1WrV6XQ2m02tVtvtdoZhSktLly1bxhaLjY2tr69nGMZisQQEBDgcDoZh7Hb7sGHD6OOGhoYpU6bQ8gkJCQaDgT5esmSJVqvltVtUVDRv3jzpynm2bt2alpbGbmo0mhdeeKGuro5hmJaWFl9fX3qUdNMSHeTmQawescNZq1atqqmp4e2Mj4/n7tRoNK+99hq7+fXXXx84cEB+B7kaGhoGDRpktVrp5nvvvZeXl9e1TRQWFsbHx9P9NpstIiKCjpRYipwzOWvWrOrqalosMTHxypUrEseKzQRe5Nz5o9Fo3nnnHfYpsQmQnp5eXFzMAACADLhkFACgh+rXr19VVdWHH36YlJQ0depUq9UqtlMmLy+vxsbGqKgolUo1YMAApVKp0+noBXitra03btxgi0VGRvbt25cQ4unp6eXl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)

``` r

show_table(mtc$species[mtc$species$used, c("species_name", "match_type", "temp_pref_mean", "catch_pct",
                                           "weight_pct", "contribution_c")], n = 8)
```

|  | species_name | match_type | temp_pref_mean | catch_pct | weight_pct | contribution_c |
|:---|:---|:---|---:|---:|---:|---:|
| 1 | Atlantic herring | exact | 6.6 | 18.55 | 21.52 | 1.42 |
| 2 | Atlantic cod | exact | 6.6 | 13.94 | 16.17 | 1.07 |
| 3 | Capelin | exact | 2.7 | 10.02 | 11.62 | 0.31 |
| 4 | Blue whiting(=Poutassou) | exact | 7.0 | 7.09 | 8.22 | 0.58 |
| 5 | Atlantic mackerel | exact | 10.2 | 6.19 | 7.18 | 0.73 |
| 7 | European sprat | exact | 9.5 | 3.95 | 4.58 | 0.44 |
| 8 | Saithe(=Pollock) | exact | 7.0 | 3.85 | 4.46 | 0.31 |
| 9 | Haddock | exact | 8.4 | 3.56 | 4.13 | 0.35 |

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

mtc <- mean_temperature_catch(FAO27$area_data)                         # newest FishBase release
mtc <- mean_temperature_catch(FAO27$area_data, version = "builtin")    # built-in table, no download
mtc <- mean_temperature_catch(FAO27$area_data, match = "tables/Atlantic__Northeast_fishbase_match.csv")
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

p27 <- cmsy_priors(c("HER", "WHB", "MAC", "COD"))
show_table(p27[, c("species_name", "matched_name", "match_type", "resilience", "prior_r",
                   "r_low", "r_high", "r_source")], digits = 3)
```

| species_name | matched_name | match_type | resilience | prior_r | r_low | r_high | r_source |
|:---|:---|:---|:---|---:|---:|---:|:---|
| Atlantic cod | Gadus morhua | exact | Medium | 0.509 | 0.336 | 0.764 | FishBase prior r, 95% CL (Based on 66 full stock assessments.) |
| Atlantic herring | Clupea harengus | exact | Medium | 0.436 | 0.288 | 0.654 | FishBase prior r, 95% CL (Based on 53 full stock assessments.) |
| Atlantic mackerel | Scomber scombrus | exact | Medium | 0.429 | 0.283 | 0.644 | FishBase prior r, 95% CL (Based on 10 full stock assessments.) |
| Blue whiting(=Poutassou) | Micromesistius poutassou | exact | Medium | 0.465 | 0.307 | 0.698 | FishBase prior r, 95% CL (Based on 9 full stock assessments.) |

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
of the catch (`fao_report(27, mtc = TRUE)`). It uses the built-in
FishBase table by default (`mtc_version = "builtin"`), so writing the
report downloads nothing; give another release with `mtc_version`,
e.g. `mtc_version = "latest"`. The table of species used, with their
FishBase matches, is returned as `report$mtc_species`, and the report
cites the FishBase release used. See [📝 Word
report](https://linbojun99.github.io/faofishingreport/articles/report.md).

``` r

report <- fao_report(27, mtc = TRUE)
head(report$mtc_species[, c("species_name", "match_type", "temp_pref_mean", "used", "url")])
fao_report(27, mtc = TRUE, mtc_version = "latest")    # newest FishBase release
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
