# Builds the two ISSCAAP tables used by catch_trade(), load_fao_trade() and fao_capture_groups():
#   inst/extdata/isscaap_groups.csv      the ISSCAAP groups: code, English name, and whether the group
#                                        is an aquatic animal group (FALSE for the groups marked * in
#                                        the FAO list, which FAO leaves out of its summary tables of
#                                        aquatic animals: mammals, crocodiles, pearls, corals, sponges
#                                        and aquatic plants)
#   inst/extdata/isscaap_production.csv  world capture and aquaculture production of each ISSCAAP group
#                                        by year from 1976 (the first year of the FAO trade data),
#                                        tonnes live weight
#
# Sources, from the FAO data index (https://www.fao.org/fishery/static/Data/):
#   GlobalProduction_<version>.zip: Global_production_quantity.csv and CL_FI_SPECIES_GROUPS.csv
#   FI_Trade_<version>.zip:         ISSCAAP.pdf (the list of ISSCAAP groups)
#
# Needs pdftotext (poppler). Run from the package root:  Rscript data-raw/build_isscaap_production.R
# To use zips already downloaded, set FAO_ZIP_DIR to their folder (GlobalProduction.zip, FI_Trade.zip).

version <- "2026.1.0"
first_year <- 1976

zip_dir <- Sys.getenv("FAO_ZIP_DIR")
get_zip <- function(prefix) {
  if (zip_dir != "") return(file.path(zip_dir, paste0(prefix, ".zip")))
  file <- tempfile(fileext = ".zip")
  options(timeout = max(600, getOption("timeout")))
  utils::download.file(paste0("https://www.fao.org/fishery/static/Data/", prefix, "_", version, ".zip"), file,
                       mode = "wb")
  file
}
read <- function(zip_file, file) {
  utils::read.csv(unz(zip_file, file), colClasses = "character", encoding = "UTF-8", check.names = FALSE,
                  na.strings = "")
}

# The ISSCAAP groups, from the list in the trade release
pdf <- tempfile(fileext = ".pdf")
utils::unzip(get_zip("FI_Trade"), files = "ISSCAAP.pdf", exdir = dirname(pdf))
lines <- system2("pdftotext", c("-layout", shQuote(file.path(dirname(pdf), "ISSCAAP.pdf")), "-"), stdout = TRUE)
lines <- grep("^\\s*\\*?\\s*[0-9]{2}\\s+[A-Z]", lines, value = TRUE)
groups <- data.frame(
  isscaap_code = as.integer(sub("^\\s*\\*?\\s*([0-9]{2}).*$", "\\1", lines)),
  isscaap_en = sub("   +.*$", "", sub("^\\s*\\*?\\s*[0-9]{2}\\s+", "", lines)),
  aquatic_animal = !grepl("^\\s*\\*", lines),
  stringsAsFactors = FALSE
)
stopifnot(nrow(groups) == 50, !anyDuplicated(groups$isscaap_code))

# World capture and aquaculture production of each group
production_zip <- get_zip("GlobalProduction")
quantity <- read(production_zip, "Global_production_quantity.csv")
species <- read(production_zip, "CL_FI_SPECIES_GROUPS.csv")
quantity <- quantity[quantity$MEASURE == "Q_tlw" & as.integer(quantity$PERIOD) >= first_year, ]
group <- species$ISSCAAP_Group_En[match(quantity$SPECIES.ALPHA_3_CODE, species$`3A_Code`)]
stopifnot(all(group %in% groups$isscaap_en))
source <- ifelse(quantity$PRODUCTION_SOURCE_DET.CODE == "CAPTURE", "capture", "aquaculture")
tonnes <- tapply(as.numeric(quantity$VALUE), list(paste(group, quantity$PERIOD), source), sum)
tonnes[is.na(tonnes)] <- 0
keys <- strsplit(rownames(tonnes), " (?=[0-9]{4}$)", perl = TRUE)
production <- data.frame(
  isscaap_code = groups$isscaap_code[match(vapply(keys, `[`, "", 1), groups$isscaap_en)],
  year = as.integer(vapply(keys, `[`, "", 2)),
  capture_tonnes = round(tonnes[, "capture"]),
  aquaculture_tonnes = round(tonnes[, "aquaculture"])
)
production <- production[order(production$isscaap_code, production$year), ]

utils::write.csv(groups, "inst/extdata/isscaap_groups.csv", row.names = FALSE)
utils::write.csv(production, "inst/extdata/isscaap_production.csv", row.names = FALSE)
writeLines(paste0("FAO GlobalProduction release ", version, ": ", nrow(production), " rows, ",
                  length(unique(production$isscaap_code)), " ISSCAAP groups, ",
                  min(production$year), "-", max(production$year)))
