# Updates the data bundled with the package to the latest FAO Global Capture Production release:
#   inst/extdata/Global_Production_long.rda  catch data in the package's long format
#   inst/extdata/fao_names.rda               English -> Chinese names used when lang = "zh" or "both"
#   inst/extdata/fao_groups.rda              species, country and area groups (ISSCAAP, income, ...)
#   inst/extdata/bundled_release.txt         the FAO release these files come from (version, publication
#                                            date, years, URL, download date), shown by fao_data_info()
#
# Chinese names come, in order of priority, from:
#   1. FAO code lists published with the release
#      (CL_FI_COUNTRY_GROUPS, CL_FI_SPECIES_GROUPS, CL_FI_WATERAREA_GROUPS)
#   2. The latest FAO ASFIS species list
#   3. The tables already bundled with the package (countries_name_data.rda, ASFIS_sp.rda)
#
# Run from the package root:  Rscript data-raw/update_bundled_data.R

source("R/fao_data_update.R", encoding = "UTF-8")
source("R/fao_language.R", encoding = "UTF-8")
source("R/fao_groups.R", encoding = "UTF-8")
# Read the translation tables from the source tree instead of an installed package
fao_extdata_csv <- function(file) {
  utils::read.csv(file.path("inst/extdata", file), fileEncoding = "UTF-8",
                  colClasses = "character", check.names = FALSE)
}

lookup_env <- new.env()
load("inst/extdata/countries_name_data.rda", envir = lookup_env)
load("inst/extdata/ASFIS_sp.rda", envir = lookup_env)
lookup <- list(name_mapping = lookup_env$name_mapping, asfis = lookup_env$ASFIS_sp)

work_dir <- tempfile("fao_bundled_")
dir.create(work_dir)

release <- utils::tail(fao_list_releases(), 1)
capture_zip <- file.path(work_dir, basename(release$url))
utils::download.file(release$url, capture_zip, mode = "wb")
capture <- fao_read_capture(capture_zip, file.path(work_dir, "capture"))

asfis_zip <- file.path(work_dir, "ASFIS_sp.zip")
utils::download.file("https://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip", asfis_zip, mode = "wb")
asfis_csv <- grep("\\.csv$", utils::unzip(asfis_zip, exdir = file.path(work_dir, "asfis")), value = TRUE)
asfis <- utils::read.csv(asfis_csv, colClasses = "character", encoding = "UTF-8",
                         check.names = FALSE, na.strings = "")

Global_Production_long <- fao_convert_capture(capture, lookup)
attr(Global_Production_long, "fao_version") <- release$version
attr(Global_Production_long, "source_url") <- release$url
save(Global_Production_long, file = "inst/extdata/Global_Production_long.rda", compress = "xz")

fao_names <- fao_build_name_table(capture, lookup, asfis)
save(fao_names, file = "inst/extdata/fao_names.rda", compress = "xz")

fao_groups <- fao_build_group_table(capture, lookup)
save(fao_groups, file = "inst/extdata/fao_groups.rda", compress = "xz")

write.dcf(data.frame(Version = release$version, Released = release$date,
                     Years = paste(range(Global_Production_long$Year), collapse = "-"),
                     URL = release$url, Downloaded = as.character(Sys.Date())),
          "inst/extdata/bundled_release.txt")

message("Bundled data updated to Capture ", release$version, ": ", nrow(Global_Production_long), " rows, ",
        min(Global_Production_long$Year), "-", max(Global_Production_long$Year), ". Names from ",
        basename(asfis_csv), ": ", paste(names(table(fao_names$type)), table(fao_names$type), collapse = ", "))
