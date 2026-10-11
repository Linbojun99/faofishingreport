# Builds inst/extdata/fao_area_sst.csv: the yearly sea surface temperature (SST) of each FAO major
# marine fishing area from the default product, NOAA ERSST v5, bundled with the package and read
# by fao_area_sst() and mean_temperature_catch(). The other products (see fao_sst_products()) are
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
