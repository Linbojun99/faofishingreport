# Grouping information for species, countries and fishing areas, from the FAO code lists:
#   species: 3-alpha code, scientific name, taxonomic level, major group (PISCES, ...) and ISSCAAP group
#   country: ISO3 code, continent, region and World Bank income group
#   area:    FAO code, inland/marine and ocean
# Chinese names come from the FAO code lists, or from inst/extdata/group_names_zh.csv where
# FAO gives none (ISSCAAP groups, major groups and income groups; unofficial translations).

# Taxonomic level of an FAO species item from its taxonomic code: each trailing "X" stands for
# an unidentified rank (no X = species). Codes of pooled items have a 3-digit suffix, ignored here.
fao_taxon_level <- function(taxonomic_code) {
  code <- substr(taxonomic_code, 1, 12)
  n_x <- nchar(code) - nchar(sub("X+$", "", code))
  level <- ifelse(n_x == 0, "species", ifelse(n_x <= 2, "genus", ifelse(n_x <= 5, "family", "higher")))
  level[is.na(code)] <- NA
  level
}

# Builds the grouping tables from the code lists of a Capture release.
fao_build_group_table <- function(capture, lookup = fao_lookup_tables()) {
  zh <- fao_extdata_csv("group_names_zh.csv")
  zh_of <- function(type, en) zh$name_zh[match(paste(type, en), paste(zh$type, zh$name_en))]

  sp <- capture$species
  species <- data.frame(
    name_en = fao_species_names(sp$`3A_Code`, sp, lookup),
    alpha3 = sp$`3A_Code`,
    scientific_name = sp$Scientific_Name,
    taxon_level = fao_taxon_level(sp$Taxonomic_Code),
    major_en = sp$Major_Group, major_zh = zh_of("major", sp$Major_Group),
    isscaap_en = sp$ISSCAAP_Group_En, isscaap_zh = zh_of("isscaap", sp$ISSCAAP_Group_En),
    stringsAsFactors = FALSE
  )
  ct <- capture$countries
  country <- data.frame(
    name_en = fao_country_names(ct$UN_Code, ct, lookup),
    iso3 = ct$ISO3_Code,
    continent_en = ct$Continent_Group_En, continent_zh = ct$Continent_Group_Cn,
    region_en = ct$GeoRegion_Group_En, region_zh = ct$GeoRegion_Group_Cn,
    income_en = ct$EcoClass_Group_En, income_zh = zh_of("income", ct$EcoClass_Group_En),
    stringsAsFactors = FALSE
  )
  ar <- capture$areas
  area <- data.frame(
    name_en = ar$Name_En, area_code = ar$Code,
    inland_marine_en = ar$InlandMarine_Group_En, inland_marine_zh = ar$InlandMarine_Group_Cn,
    ocean_en = ar$Ocean_Group_En, ocean_zh = ar$Ocean_Group_Cn,
    stringsAsFactors = FALSE
  )
  list(species = species[!duplicated(species$name_en), ],
       country = country[!duplicated(country$name_en), ],
       area = area[!duplicated(area$name_en), ])
}

# Grouping tables: the newer of the downloaded and bundled tables first, then the other.
fao_group_table <- function(data_dir = fao_data_dir()) {
  bundled <- new.env()
  load(system.file("extdata", "fao_groups.rda", package = "faofishingreport"), envir = bundled)
  groups <- bundled$fao_groups

  versions <- fao_local_versions(data_dir)
  if (length(versions) > 0) {
    newest <- versions[length(versions)]
    groups_file <- fao_groups_file(newest, data_dir)
    if (file.exists(groups_file)) {
      downloaded <- readRDS(groups_file)
      newer_first <- numeric_version(newest) > numeric_version(fao_bundled_release())
      groups <- lapply(names(groups), function(type) {
        # Tables saved by older versions of the package lack some columns (e.g. alpha3)
        columns <- union(names(groups[[type]]), names(downloaded[[type]]))
        fill <- function(x) {
          x[setdiff(columns, names(x))] <- NA_character_
          x[columns]
        }
        both <- if (newer_first) rbind(fill(downloaded[[type]]), fill(groups[[type]])) else
          rbind(fill(groups[[type]]), fill(downloaded[[type]]))
        both[!duplicated(both$name_en), ]
      })
      names(groups) <- names(bundled$fao_groups)
    }
  }
  groups
}

# Adds the grouping columns of species, countries and areas to FAO data (matched on English names).
fao_add_groups <- function(data, groups = fao_group_table()) {
  data <- as.data.frame(data)
  added <- list(
    groups$species[match(data$ASFIS.species..Name., groups$species$name_en), -1],
    groups$country[match(data$Country..Name., groups$country$name_en), -1],
    groups$area[match(data$FAO.major.fishing.area..Name., groups$area$name_en), -1]
  )
  for (part in added) {
    rownames(part) <- NULL
    data <- cbind(data, part)
  }
  data
}

# Group names in the requested language; missing Chinese names fall back to English.
fao_pick <- function(en, zh, lang) {
  zh <- ifelse(is.na(zh) | zh == "", en, zh)
  switch(lang,
         en = en,
         zh = zh,
         both = ifelse(zh == en, en, paste(zh, en)))
}

# Coastal countries of each FAO marine area (built by data-raw/build_coastal_countries.R)
fao_coastal_table <- function() {
  coastal <- new.env()
  load(system.file("extdata", "fao_coastal.rda", package = "faofishingreport"), envir = coastal)
  coastal$fao_coastal
}
