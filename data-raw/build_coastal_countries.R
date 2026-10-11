# Builds inst/extdata/fao_coastal.rda: the coastal countries of each FAO major marine fishing area,
# used by coastal_distant() to separate catches by coastal countries from distant-water fleets.
#
# A country is coastal to an area if its land (Natural Earth admin-0 map units, which keep
# overseas territories such as Reunion separate) lies within 20 km of the FAO area polygon
# (inst/extdata/FAO_AREAS_CWP.shp). Former countries take the coastline of their successors.
#
# Run from the package root:  Rscript data-raw/build_coastal_countries.R

library(sf)
sf_use_s2(TRUE)

areas <- st_read("inst/extdata/FAO_AREAS_CWP.shp", quiet = TRUE)
areas <- areas[areas$F_LEVEL == "MAJOR", ]
areas <- st_make_valid(areas)

units <- rnaturalearth::ne_countries(scale = "medium", type = "map_units", returnclass = "sf")
units <- st_make_valid(units)
units$iso3 <- ifelse(units$iso_a3 == "-99", units$adm0_a3, units$iso_a3)

near <- st_is_within_distance(units, areas, dist = 20000)
iso_area <- unique(data.frame(iso3 = rep(units$iso3, lengths(near)),
                              area_en = areas$NAME_EN[unlist(near)]))

load("inst/extdata/countries_name_data.rda")

# Former or merged countries reported by FAO, mapped to the countries now holding their coastline
successors <- list(
  "Un. Sov. Soc. Rep." = c("RUS", "UKR", "GEO", "EST", "LVA", "LTU"),
  "Yugoslavia SFR" = c("HRV", "SVN", "MNE", "BIH"),
  "Serbia and Montenegro" = "MNE",
  "Ethiopia PDR" = "ERI",
  "Sudan (former)" = "SDN",
  "Netherlands Antilles" = c("CUW", "SXM"),
  "United Republic of Tanzania, Zanzibar" = "TZA",
  "Gibraltar" = "ESP",
  "Palestine" = "ISR"
)

countries <- unique(c(name_mapping$Short.name, names(successors)))
countries <- countries[!is.na(countries) & countries != "Other nei"]
coastal <- do.call(rbind, lapply(countries, function(country) {
  iso3 <- if (country %in% names(successors)) successors[[country]] else
    name_mapping$ISO3[match(country, name_mapping$Short.name)]
  area_en <- unique(iso_area$area_en[iso_area$iso3 %in% iso3])
  if (length(area_en) == 0) return(NULL)
  data.frame(area_en = area_en, country_en = country, stringsAsFactors = FALSE)
}))
# Gibraltar and Palestine only border the Mediterranean
coastal <- coastal[!(coastal$country_en %in% c("Gibraltar", "Palestine") &
                     coastal$area_en != "Mediterranean and Black Sea"), ]

fao_coastal <- coastal[order(coastal$area_en, coastal$country_en), ]
rownames(fao_coastal) <- NULL
save(fao_coastal, file = "inst/extdata/fao_coastal.rda", compress = "xz")
message("Coastal countries per area: ",
        paste(names(table(fao_coastal$area_en)), table(fao_coastal$area_en), sep = " = ", collapse = "; "))
