# Builds inst/extdata/fao_area_sst.csv and fao_division_sst.csv: the yearly sea surface temperature
# (SST) of each FAO major marine fishing area, and of the subareas and divisions of areas 34, 37,
# 47 and 51, from the default product, NOAA ERSST v5, bundled with the package and read by
# fao_area_sst() and mean_temperature_catch(). The other products (see fao_sst_products()) are
# not bundled: users download them with download_sst_data().
#
# The data are downloaded and the area means computed by download_sst_data(); see
# ?download_sst_data for the method. Files are kept in <fao_data_dir()>/sst/ersst5/, so running
# the script again only downloads the years that are missing.
#
# Needs the ncdf4 package.
# Run from the package root:  Rscript data-raw/build_fao_area_sst.R

pkgload::load_all()
sst <- download_sst_data("ersst5")
stopifnot(length(unique(sst$area_code)) == 19,
          all(sst$sst_mean > -2 & sst$sst_mean < 32),
          all(sst$sst_p10 <= sst$sst_p90),
          all(sst$sst_min_month <= sst$sst_mean & sst$sst_mean <= sst$sst_max_month))
readr::write_excel_csv(sst, "inst/extdata/fao_area_sst.csv")
message("Wrote inst/extdata/fao_area_sst.csv: ", nrow(sst), " rows, ", min(sst$year), "-", max(sst$year))

# Subareas and divisions of the areas with catch data by division (load_fao_divisions()), kept
# small; download_sst_data() and the user's own files give the table of all subareas and divisions
divisions <- read.csv(file.path(fao_data_dir(), "sst", "ersst5", "fao_division_sst.csv"), fileEncoding = "UTF-8-BOM",
                      colClasses = c(area_code = "character"))
divisions <- divisions[sub("\\..*$", "", divisions$area_code) %in% c("34", "37", "47", "51"), ]
stopifnot(all(divisions$sst_p10 <= divisions$sst_p90))
# The columns that are the same on every row (product, source, date) are added back when read
readr::write_excel_csv(divisions[, setdiff(names(divisions), c("product", "source", "downloaded"))],
                       "inst/extdata/fao_division_sst.csv")
message("Wrote inst/extdata/fao_division_sst.csv: ", length(unique(divisions$area_code)), " subareas and divisions, ",
        nrow(divisions), " rows")
