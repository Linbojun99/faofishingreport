# Sea surface temperature (SST) of the FAO major marine fishing areas. The package comes with the
# yearly SST of the 19 areas from NOAA ERSST v5. Other SST data can be used from the path of
# NetCDF files (the user's own, or files downloaded with download_sst_data()): the area means are
# computed on the user's computer once and saved for later use.

#' Sea Surface Temperature of the FAO Major Fishing Areas
#'
#' This function returns the yearly sea surface temperature (SST) of the 19 FAO major marine
#' fishing areas: the area mean, the range within the area and the seasonal range, one row per
#' area and year, like the FAO catch statistics. It is used by `mean_temperature_catch()`. It
#' always says which SST data it uses.
#'
#' @section Which SST data:
#' \describe{
#'   \item{Built in (default): NOAA ERSST v5}{The package comes with the yearly SST of the 19
#'     areas from 1950, computed from NOAA Extended Reconstructed SST version 5 (Huang et al.
#'     2017): a monthly reconstruction on a 2° x 2° grid (about 220 km) from in situ observations
#'     only (ships and buoys of ICOADS, Argo floats from 1999). It is the smallest of the common
#'     SST products, needs no download, and is consistent over the whole period; its grid is
#'     coarse for small areas such as the Mediterranean and Black Sea (`n_cells` gives the number
#'     of grid cells of each area).}
#'   \item{Your own NetCDF files}{Give the path of a NetCDF file, or of a folder of NetCDF files,
#'     as `sst`, or once for the session with `fao_settings(sst = "path")`. The files must hold a
#'     gridded SST with longitude, latitude and time, monthly or daily (e.g. HadISST, COBE-SST2,
#'     OISST, ERSST, or a model output). The yearly area means are computed the first time, which
#'     can take a few minutes, and saved in the `sst` folder of `data_dir`; later calls with the
#'     same files read the saved table. Reading NetCDF files needs the ncdf4 package.}
#'   \item{Downloaded with download_sst_data()}{`download_sst_data()` downloads one of the products
#'     listed by `fao_sst_products()` (HadISST1, COBE-SST2, OISST v2.1, or the newest years of
#'     ERSST v5) and computes its table; it is then used by its name, e.g. `sst = "hadisst1"`.}
#' }
#'
#' @section Subareas and divisions:
#' With `level = "division"`, the table has one row per FAO subarea and division of the FAO map in
#' the package (`inst/extdata/FAO_AREAS_CWP.shp`): `area_code` is its code (e.g. "34.1.1" or the
#' subarea "34.2") and `area_en` its name. The built-in ERSST v5 table covers the subareas and
#' divisions of areas 34, 37, 47 and 51, those of the catch data by division
#' (`load_fao_divisions()`); tables from downloaded products or your own files cover all of them.
#' Many divisions are small for a 2° grid (see `n_cells`; e.g. the Sea of Azov or the Sea of
#' Marmara have none or a few cells), so a finer SST product, given as the path of its NetCDF
#' files, suits divisions better.
#'
#' @section How the area means are computed:
#' \itemize{
#'   \item Files: the SST variable is found by its name ("sst", "analysed_sst", "tos", ...)
#'     unless given as `variable`; longitudes may run from -180 to 180 or from 0 to 360; values in
#'     kelvin are converted to °C; if there are several depths, the first (the surface) is used.
#'     Values below -5 °C, which some products use to mark sea ice (-1000 in HadISST1), are set to
#'     -1.8 °C, the value other products give under sea ice. Daily values are averaged by month.
#'   \item Grids finer than 1° are first averaged to about 1° (mean of the cells with a value).
#'   \item Each grid cell counts for each FAO major area it overlaps (`inst/extdata/FAO_AREAS_CWP.shp`,
#'     the 19 marine areas), weighted by the area of the overlap computed on the sphere, so cells
#'     on a boundary are shared and coastal cells count for their sea part only.
#'   \item `sst_mean` is the mean of the 12 monthly area means; `sst_p10` and `sst_p90` are the
#'     weighted 10th and 90th percentiles of the yearly mean SST of the cells (the range within the
#'     area); `sst_min_month` and `sst_max_month` are the coldest and warmest monthly area means
#'     (the seasonal range; in areas on both sides of the equator the seasons of the two
#'     hemispheres partly cancel, so this range is narrow there). Only years with all 12 months
#'     are kept.
#' }
#' Values over the Arctic Sea and the Antarctic areas partly reflect sea ice rather than measured
#' water temperature. SST products use different observations and methods, so their values and
#' trends differ somewhat; repeating an analysis with a second product shows how much a result
#' depends on the product.
#'
#' @param area FAO area codes (e.g. "34") or names as in the FAO data (e.g. "Atlantic, Eastern
#'        Central"). Default is NULL, all areas.
#' @param sst The SST data: "ersst5" (built in), the name of a product downloaded with
#'        `download_sst_data()`, or the path of a NetCDF file or of a folder of NetCDF files.
#'        Default is the `sst` setting of `fao_settings()`, "ersst5" unless changed.
#' @param level "area" (default) for the 19 FAO major marine areas, or "division" for the FAO
#'        subareas and divisions (e.g. "37.1.1", Balearic), as in the catch data of
#'        `load_fao_divisions()` (see Subareas and divisions).
#' @param variable The name of the SST variable in your NetCDF files. Default is NULL, found from
#'        its name.
#' @param data_dir The directory where downloaded data and computed tables are stored. Default is
#'        `fao_data_dir()`.
#' @return A data frame with one row per area and year: `product`, `area_code`, `area_en`,
#'   `year`, `sst_mean`, `sst_p10`, `sst_p90`, `sst_min_month`, `sst_max_month` (all in °C),
#'   `n_cells` (grid cells in the area), `source` (the SST data used) and `downloaded` (date the
#'   data were downloaded or computed).
#' @inherit fao_sst_products references
#' @export
#' @examples
#' \dontrun{
#' sst34 <- fao_area_sst("34")                          # built in: NOAA ERSST v5
#' plot(sst_mean ~ year, data = sst34, type = "l", ylim = range(sst34$sst_p10, sst34$sst_p90))
#' lines(sst_p10 ~ year, data = sst34, lty = 2)
#' lines(sst_p90 ~ year, data = sst34, lty = 2)
#'
#' # Your own NetCDF file (e.g. HadISST_sst.nc downloaded from the Met Office), for one call ...
#' fao_area_sst("34", sst = "D:/data/HadISST_sst.nc")
#' # ... or for the whole session, including mean_temperature_catch()
#' fao_settings(sst = "D:/data/HadISST_sst.nc")
#'
#' # Subareas and divisions, e.g. those of the Mediterranean and Black Sea
#' div <- fao_area_sst(level = "division")
#' subset(div, startsWith(area_code, "37.") & year == 2024)
#' }
fao_area_sst <- function(area = NULL, sst = fao_setting("sst"), level = c("area", "division"), variable = NULL,
                         data_dir = fao_data_dir()) {
  level <- match.arg(level)
  table <- fao_sst_source(sst, variable, data_dir, level)
  if (!is.null(area)) {
    area <- as.character(area)
    unknown <- setdiff(area, c(table$area_code, table$area_en))
    if (length(unknown) > 0) {
      stop("Unknown FAO ", if (level == "area") "major marine fishing area" else "subarea or division", ": ",
           paste(unknown, collapse = ", "), ". Codes: ",
           paste(unique(paste(table$area_code, table$area_en)), collapse = "; "), call. = FALSE)
    }
    table <- table[table$area_code %in% area | table$area_en %in% area, ]
  }
  rownames(table) <- NULL
  table
}


#' Sea Surface Temperature Products
#'
#' This function lists the sea surface temperature (SST) products that the package knows: the
#' built-in NOAA ERSST v5, and the products that `download_sst_data()` can download. For each it
#' gives the provider, grid, first year, data sources, treatment of sea ice, licence, the
#' reference to cite and the approximate download size. Any other gridded SST in NetCDF files can
#' also be used by giving its path (see `fao_area_sst()`).
#'
#' \describe{
#'   \item{ersst5 (built in, default)}{NOAA Extended Reconstructed SST version 5 (Huang et al.
#'     2017): 2° x 2°, monthly, from 1854. In situ observations only (ships and buoys of ICOADS,
#'     Argo floats from 1999).}
#'   \item{hadisst1}{Met Office Hadley Centre HadISST1 (Rayner et al. 2003): 1° x 1°, monthly,
#'     from 1870. In situ observations (Met Office Marine Data Bank, with GTS reports from 1982;
#'     ICOADS monthly medians for 1871-1995), reconstructed by reduced-space optimal
#'     interpolation. The dataset description carries a warning (10 March 2016) that problems
#'     found in an analysis by D. Chelton and C. Risien may limit its use where high resolution or
#'     SST gradients matter (see References).}
#'   \item{cobe2}{Japan Meteorological Agency COBE-SST2 (Hirahara et al. 2014): 1° x 1°, monthly,
#'     from 1850. In situ SST and sea-ice observations; satellite observations are used to
#'     reconstruct variability in regions with few observations.}
#'   \item{oisst21}{NOAA Optimum Interpolation SST version 2.1 (Huang et al. 2021): daily 0.25° x
#'     0.25° analysis from September 1981 (monthly means from NOAA PSL; averaged to 1° by the
#'     package). Satellite (AVHRR), ship, buoy and Argo observations. Only from 1982.}
#' }
#' Only the table of ERSST v5 comes with the package, to keep it small; its data are also the
#' smallest to download (about 0.8 MB per year, against 3 to 11 MB for the others). The web page
#' of each product is given in the column `homepage`.
#'
#' @return A data frame with one row per product: `product` (the name used as `sst`), `name`,
#'   `provider`, `grid`, `first_year` (first complete year), `data_sources`, `sea_ice`, `licence`,
#'   `citation`, `doi`, `homepage` (web page of the product), `download` (server and dataset),
#'   `mb_per_year` (approximate download size) and `built_in`.
#' @references
#' Hirahara, S., Ishii, M. and Fukuda, Y. (2014). Centennial-scale sea surface temperature
#' analysis and its uncertainty. Journal of Climate 27: 57-75. \doi{10.1175/JCLI-D-12-00837.1}
#'
#' Huang, B., Thorne, P.W., Banzon, V.F., Boyer, T., Chepurin, G., Lawrimore, J.H., Menne, M.J.,
#' Smith, T.M., Vose, R.S. and Zhang, H.-M. (2017). Extended Reconstructed Sea Surface
#' Temperature, Version 5 (ERSSTv5): Upgrades, Validations, and Intercomparisons. Journal of
#' Climate 30: 8179-8205. \doi{10.1175/JCLI-D-16-0836.1}
#'
#' Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., Smith, T. and Zhang,
#' H.-M. (2021). Improvements of the Daily Optimum Interpolation Sea Surface Temperature (DOISST)
#' Version 2.1. Journal of Climate 34: 2923-2939. \doi{10.1175/JCLI-D-20-0166.1}
#'
#' Rayner, N.A., Parker, D.E., Horton, E.B., Folland, C.K., Alexander, L.V., Rowell, D.P., Kent,
#' E.C. and Kaplan, A. (2003). Global analyses of sea surface temperature, sea ice, and night
#' marine air temperature since the late nineteenth century. Journal of Geophysical Research 108
#' (D14): 4407. \doi{10.1029/2002JD002670}
#'
#' HadISST1 dataset description, with the warning of 10 March 2016 and a link to the analysis by
#' D. Chelton and C. Risien (\url{https://ir.library.oregonstate.edu/xmlui/handle/1957/58385}):
#' \url{https://coastwatch.pfeg.noaa.gov/erddap/info/erdHadISST/index.html}
#'
#' Web pages of the products: ERSST v5 \url{https://www.ncei.noaa.gov/products/extended-reconstructed-sst};
#' HadISST1 \url{https://www.metoffice.gov.uk/hadobs/hadisst/}; COBE-SST2
#' \url{https://psl.noaa.gov/data/gridded/data.cobe2.html}; OISST v2.1
#' \url{https://www.ncei.noaa.gov/products/optimum-interpolation-sst}
#' @export
#' @examples
#' fao_sst_products()[, c("product", "name", "grid", "first_year", "built_in")]
fao_sst_products <- function() {
  p <- fao_sst_registry
  data.frame(
    product = names(p),
    name = vapply(p, `[[`, character(1), "name"),
    provider = vapply(p, `[[`, character(1), "provider"),
    grid = vapply(p, `[[`, character(1), "grid"),
    first_year = vapply(p, `[[`, numeric(1), "first_year"),
    data_sources = vapply(p, `[[`, character(1), "data_sources"),
    sea_ice = vapply(p, `[[`, character(1), "sea_ice"),
    licence = vapply(p, `[[`, character(1), "licence"),
    citation = vapply(p, `[[`, character(1), "citation"),
    doi = vapply(p, `[[`, character(1), "doi"),
    homepage = vapply(p, `[[`, character(1), "homepage"),
    download = vapply(p, `[[`, character(1), "download"),
    mb_per_year = vapply(p, `[[`, numeric(1), "mb_per_year"),
    built_in = names(p) == "ersst5",
    row.names = NULL, stringsAsFactors = FALSE
  )
}


#' Download a Sea Surface Temperature Product
#'
#' This function downloads one of the sea surface temperature (SST) products listed by
#' `fao_sst_products()`, one year at a time, and computes the yearly SST of each FAO major marine
#' fishing area (see `fao_area_sst()` for the method). The product can then be used by its name,
#' e.g. `fao_area_sst(sst = "hadisst1")`, `mean_temperature_catch(..., sst = "hadisst1")` or
#' `fao_settings(sst = "hadisst1")`. Files are kept in the `sst` folder of `data_dir`, with
#' `files.csv` recording where and when each was downloaded; years already downloaded are skipped,
#' so an interrupted download can be resumed by running the function again. For ERSST v5, whose
#' table is built into the package, the function adds the years published since.
#'
#' SST data from other sources can be used without this function, by giving the path of the
#' NetCDF files (see `fao_area_sst()`). Reading the files needs the ncdf4 package.
#'
#' @param product The product: "ersst5", "hadisst1", "cobe2" or "oisst21"; see
#'        `fao_sst_products()`.
#' @param years The years to download. Default is NULL: from 1950 (1982 for OISST v2.1) to the
#'        last complete year.
#' @param data_dir The directory where downloaded data is stored. Default is `fao_data_dir()`.
#' @param overwrite A logical value. If TRUE, years already downloaded are downloaded again.
#'        Default is FALSE.
#' @param quiet A logical value. If TRUE, progress messages are not printed. Default is FALSE.
#' @return Invisibly, the area table (as returned by `fao_area_sst()`).
#' @inherit fao_sst_products references
#' @export
#' @examples
#' \dontrun{
#' fao_sst_products()
#' download_sst_data("hadisst1")          # about 240 MB
#' fao_area_sst("34", sst = "hadisst1")
#' }
download_sst_data <- function(product = "ersst5", years = NULL, data_dir = fao_data_dir(),
                              overwrite = FALSE, quiet = FALSE) {
  if (!is.character(product) || length(product) != 1 || !product %in% names(fao_sst_registry)) {
    stop("Unknown SST product: ", paste(product, collapse = ", "), ". Products: ",
         paste(names(fao_sst_registry), collapse = ", "), " (see fao_sst_products()).", call. = FALSE)
  }
  fao_sst_check_ncdf4()
  info <- fao_sst_registry[[product]]
  if (is.null(years)) years <- seq(max(1950, info$first_year), as.integer(format(Sys.Date(), "%Y")) - 1)
  dir <- file.path(data_dir, "sst", product)
  raw_dir <- file.path(dir, "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  todo <- years[overwrite | !file.exists(fao_sst_raw_file(raw_dir, product, years))]
  if (!quiet && length(todo) > 0) {
    message(info$name, ": downloading ", length(todo), " year(s), about ",
            round(length(todo) * info$mb_per_year), " MB, to ", raw_dir)
  }
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  bar <- if (!quiet && length(todo) > 1) utils::txtProgressBar(max = length(todo), style = 3) else NULL
  if (!is.null(bar)) on.exit(close(bar), add = TRUE)
  for (k in seq_along(todo)) {
    year <- todo[k]
    file <- fao_sst_raw_file(raw_dir, product, year)
    url <- info$url(year)
    if (!fao_sst_download(url, file)) {
      stop("Could not download ", info$name, " for ", year, ". Run the function again to resume.", call. = FALSE)
    }
    dates <- fao_sst_nc_meta(file, "sst")$dates
    if (length(unique(format(dates[format(dates, "%Y") == as.character(year)], "%m"))) < 12) {
      unlink(file)
      message(info$name, " ", year, " is not complete yet; stopping at ", year - 1, ".")
      break
    }
    fao_sst_update_manifest(dir, basename(file), url)
    if (!is.null(bar)) utils::setTxtProgressBar(bar, k)
  }

  files <- list.files(raw_dir, pattern = paste0("^", product, "_[0-9]{4}\\.nc$"), full.names = TRUE)
  if (length(files) == 0) stop("No downloaded files of ", info$name, " in ", raw_dir, call. = FALSE)
  manifest <- utils::read.csv(file.path(dir, "files.csv"), colClasses = "character")
  table <- fao_sst_compute(files, "sst", data_dir, product = product, source = info$name,
                           date = max(manifest$downloaded), quiet = quiet)
  fao_write_csv(table, file.path(dir, fao_sst_table_file("area")))
  divisions <- fao_sst_compute(files, "sst", data_dir, product = product, source = info$name,
                               date = max(manifest$downloaded), quiet = quiet, level = "division")
  fao_write_csv(divisions, file.path(dir, fao_sst_table_file("division")))
  if (!quiet) message(info$name, ": tables of the areas (", nrow(table), " rows) and of the subareas and divisions (",
                      nrow(divisions), " rows), ", min(table$year), "-", max(table$year), ", saved in ", dir)
  invisible(table)
}


# Products: description (see ?fao_sst_products) and the URL of one year
fao_sst_registry <- list(
  ersst5 = list(
    name = "NOAA ERSST v5", provider = "NOAA National Centers for Environmental Information (NCEI)",
    grid = "2 x 2 degrees, monthly", first_year = 1854,
    data_sources = "In situ only: ships and buoys (ICOADS), Argo floats from 1999",
    sea_ice = "Derived from sea-ice concentration (about -1.8 C under ice)",
    licence = "NOAA data: may be used and redistributed for free",
    citation = "Huang et al. (2017) Journal of Climate 30: 8179-8205", doi = "10.1175/JCLI-D-16-0836.1",
    homepage = "https://www.ncei.noaa.gov/products/extended-reconstructed-sst",
    download = "NOAA CoastWatch ERDDAP, dataset nceiErsstv5", mb_per_year = 0.78,
    url = function(year) {
      paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst%5B(", year, "-01-01):1:(",
             year, "-12-01)%5D%5B(0.0)%5D%5B(-88):1:(88)%5D%5B(0):1:(358)%5D")
    }
  ),
  hadisst1 = list(
    name = "Met Office HadISST1", provider = "Met Office Hadley Centre",
    grid = "1 x 1 degree, monthly", first_year = 1870,
    data_sources = paste("In situ: Met Office Marine Data Bank (with GTS reports from 1982) and ICOADS",
                         "monthly medians 1871-1995; reduced-space optimal interpolation"),
    sea_ice = "Estimated from sea-ice concentration; ice-covered cells (-1000) set to -1.8 C by this package",
    licence = "Crown copyright, Open Government Licence",
    citation = "Rayner et al. (2003) Journal of Geophysical Research 108(D14): 4407", doi = "10.1029/2002JD002670",
    homepage = "https://www.metoffice.gov.uk/hadobs/hadisst/",
    download = "NOAA CoastWatch ERDDAP, dataset erdHadISST", mb_per_year = 3.1,
    url = function(year) {
      paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdHadISST.nc?sst%5B(", year, "-01-15):1:(",
             year, "-12-15)%5D%5B(89.5):1:(-89.5)%5D%5B(-179.5):1:(179.5)%5D")
    }
  ),
  cobe2 = list(
    name = "JMA COBE-SST2", provider = "Japan Meteorological Agency; distributed by NOAA PSL",
    grid = "1 x 1 degree, monthly", first_year = 1850,
    data_sources = paste("In situ SST and sea-ice observations; satellite observations used to reconstruct",
                         "variability where observations are few"),
    sea_ice = "Derived from sea-ice concentration",
    licence = "Free; NOAA PSL asks users to acknowledge data provided by the NOAA PSL, Boulder, Colorado, USA",
    citation = "Hirahara et al. (2014) Journal of Climate 27: 57-75", doi = "10.1175/JCLI-D-12-00837.1",
    homepage = "https://psl.noaa.gov/data/gridded/data.cobe2.html",
    download = "NOAA PSL THREDDS, Datasets/COBE2/sst.mon.mean.nc", mb_per_year = 3.4,
    url = function(year) {
      paste0("https://psl.noaa.gov/thredds/ncss/grid/Datasets/COBE2/sst.mon.mean.nc?var=sst&time_start=",
             year, "-01-01T00:00:00Z&time_end=", year, "-12-31T00:00:00Z&accept=netcdf")
    }
  ),
  oisst21 = list(
    name = "NOAA OISST v2.1", provider = "NOAA NCEI; monthly means from NOAA PSL",
    grid = "0.25 x 0.25 degree, monthly means of daily analyses (averaged to 1 degree by the package)",
    first_year = 1982,
    data_sources = "Satellite (AVHRR), ships, buoys and Argo floats",
    sea_ice = "Proxy SST from sea-ice concentration in the marginal ice zone",
    licence = "NOAA data, free; NOAA PSL asks users to acknowledge data provided by the NOAA PSL, Boulder, Colorado, USA",
    citation = "Huang et al. (2021) Journal of Climate 34: 2923-2939", doi = "10.1175/JCLI-D-20-0166.1",
    homepage = "https://www.ncei.noaa.gov/products/optimum-interpolation-sst",
    download = "NOAA PSL THREDDS, Datasets/noaa.oisst.v2.highres/sst.mon.mean.nc", mb_per_year = 11,
    url = function(year) {
      paste0("https://psl.noaa.gov/thredds/ncss/grid/Datasets/noaa.oisst.v2.highres/sst.mon.mean.nc?var=sst",
             "&time_start=", year, "-01-01T00:00:00Z&time_end=", year, "-12-31T00:00:00Z&accept=netcdf")
    }
  )
)

fao_sst_check_ncdf4 <- function() {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Reading NetCDF files needs the ncdf4 package: install.packages(\"ncdf4\")", call. = FALSE)
  }
  invisible(TRUE)
}

# Checks the `sst` setting: a product name or existing NetCDF path(s)
fao_sst_check_setting <- function(sst) {
  if (!is.character(sst) || length(sst) == 0 || anyNA(sst)) {
    stop("sst must be \"ersst5\", the name of a downloaded product or the path of NetCDF files.", call. = FALSE)
  }
  if (length(sst) == 1 && sst %in% names(fao_sst_registry)) return(invisible(TRUE))
  missing <- sst[!file.exists(sst)]
  if (length(missing) > 0) {
    stop("SST data not found: ", paste(missing, collapse = ", "), ". Use \"ersst5\" (built in), a product of ",
         "fao_sst_products() or the path of a NetCDF file or folder.", call. = FALSE)
  }
  invisible(TRUE)
}

# The table (FAO major areas, or subareas and divisions) of the SST data `sst` (product name or
# NetCDF path), with a message saying which data are used
fao_sst_source <- function(sst, variable = NULL, data_dir = fao_data_dir(), level = "area") {
  fao_sst_check_setting(sst)
  units <- if (level == "area") "FAO major areas" else "FAO subareas and divisions"
  if (length(sst) == 1 && sst %in% names(fao_sst_registry)) {
    info <- fao_sst_registry[[sst]]
    local_file <- file.path(data_dir, "sst", sst, fao_sst_table_file(level))
    raw_dir <- file.path(data_dir, "sst", sst, "raw")
    # Products downloaded before the division tables existed: compute the table from the files
    if (!file.exists(local_file) && level == "division" && file.exists(file.path(data_dir, "sst", sst, "files.csv"))) {
      files <- list.files(raw_dir, pattern = paste0("^", sst, "_[0-9]{4}\\.nc$"), full.names = TRUE)
      if (length(files) > 0) {
        message("Computing the SST of the FAO subareas and divisions from the downloaded ", info$name, " files ...")
        manifest <- utils::read.csv(file.path(data_dir, "sst", sst, "files.csv"), colClasses = "character")
        fao_write_csv(fao_sst_compute(files, "sst", data_dir, product = sst, source = info$name,
                                      date = max(manifest$downloaded), level = "division"), local_file)
      }
    }
    table <- if (file.exists(local_file)) fao_sst_read_table(local_file) else NULL
    if (sst == "ersst5") {
      built_in <- fao_sst_read_table(system.file("extdata", fao_sst_table_file(level), package = "faofishingreport"))
      # The built-in division table leaves out the columns that are the same on every row
      if (!"source" %in% names(built_in)) {
        area_table <- fao_sst_read_table(system.file("extdata", "fao_area_sst.csv", package = "faofishingreport"))
        built_in <- cbind(product = "ersst5", built_in, source = info$name, downloaded = area_table$downloaded[1],
                          stringsAsFactors = FALSE)
      }
      if (is.null(table)) {
        message("SST: ", info$name, ", built into the package (", units,
                if (level == "division") " of areas 34, 37, 47 and 51", "; ", info$grid, "; ", info$data_sources,
                "; ", min(built_in$year), "-", max(built_in$year), "; ", info$citation, "). ",
                "See ?fao_area_sst to use other SST data.")
        return(built_in)
      }
      # the downloaded table replaces the same years (and adds the subareas and divisions the built-in
      # table lacks); the other years of the built-in table are kept
      columns <- intersect(names(built_in), names(table))
      keep <- !paste(built_in$area_code, built_in$year) %in% paste(table$area_code, table$year)
      table <- rbind(built_in[keep, columns], table[, columns])
      table <- table[order(table$area_code, table$year), ]
      rownames(table) <- NULL
      where <- "updated with download_sst_data()"
    } else if (is.null(table)) {
      stop(info$name, " has not been downloaded. Run download_sst_data(\"", sst, "\") first (about ",
           round(info$mb_per_year * (as.integer(format(Sys.Date(), "%Y")) - max(1950, info$first_year))),
           " MB), or give the path of your own NetCDF files (see ?fao_area_sst).", call. = FALSE)
    } else {
      where <- "downloaded with download_sst_data()"
    }
    message("SST: ", info$name, ", ", where, " (", units, "; ", info$grid, "; ", info$data_sources, "; ",
            min(table$year), "-", max(table$year), "; ", info$citation, "), in ", dirname(local_file), ".")
    return(table)
  }
  fao_sst_from_files(sst, variable, data_dir, level)
}

# File name of the table of a level: FAO major areas, or subareas and divisions
fao_sst_table_file <- function(level) if (level == "area") "fao_area_sst.csv" else "fao_division_sst.csv"

# Area table computed from the user's NetCDF files (a file, several files or a folder). The table
# is saved in <data_dir>/sst/files/, under a name that changes when the files change.
fao_sst_from_files <- function(path, variable, data_dir, level = "area") {
  fao_sst_check_ncdf4()
  files <- unlist(lapply(path, function(p) {
    if (dir.exists(p)) list.files(p, pattern = "\\.(nc|nc4|cdf|netcdf)$", full.names = TRUE, ignore.case = TRUE) else p
  }))
  if (length(files) == 0) stop("No NetCDF files (.nc) in ", paste(path, collapse = ", "), call. = FALSE)
  files <- sort(normalizePath(files))
  # "method 2": tables computed before the weights covered every cell and dates followed the calendar
  # (version 0.7.0) are computed again
  signature <- paste(c("method 2", files, file.size(files), as.numeric(file.mtime(files)),
                       if (is.null(variable)) "" else variable), collapse = "\n")
  signature_file <- tempfile()
  writeLines(signature, signature_file)
  key <- substr(unname(tools::md5sum(signature_file)), 1, 10)
  unlink(signature_file)
  label <- if (length(path) == 1) basename(path) else paste0(basename(files[1]), " and ", length(files) - 1, " more")
  if (length(path) == 1 && dir.exists(path)) label <- paste0(label, " (", length(files), " files)")
  dir <- file.path(data_dir, "sst", "files", paste0(gsub("[^A-Za-z0-9_.-]", "_", basename(path[1])), "_", key))
  table_file <- file.path(dir, fao_sst_table_file(level))
  units <- if (level == "area") "FAO areas" else "FAO subareas and divisions"

  if (file.exists(table_file)) {
    table <- fao_sst_read_table(table_file)
    info <- utils::read.csv(file.path(dir, "source.csv"), colClasses = "character")
  } else {
    meta <- fao_sst_nc_meta(files[1], variable)
    message("Computing the yearly SST of the ", units, " from ", length(files), " NetCDF file(s) (variable '",
            meta$variable, "'). This is done once; the result is saved in ", dir)
    table <- fao_sst_compute(files, meta$variable, data_dir, product = "user files", source = label,
                             date = as.character(Sys.Date()), quiet = FALSE, level = level)
    info <- data.frame(path = paste(path, collapse = "; "), files = length(files), variable = meta$variable,
                       title = meta$title, grid = meta$grid, years = paste(range(table$year), collapse = "-"),
                       computed = as.character(Sys.Date()), stringsAsFactors = FALSE)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    fao_write_csv(table, table_file)
    utils::write.csv(info, file.path(dir, "source.csv"), row.names = FALSE)
  }
  message("SST: your NetCDF file(s) ", info$path, " (variable '", info$variable, "'",
          if (!is.na(info$title) && info$title != "") paste0(", \"", info$title, "\""), "; ", info$grid, "; ",
          info$years, "), means of the ", units, " computed ", info$computed, " and saved in ", dir, ".")
  table
}

fao_sst_raw_file <- function(raw_dir, product, year) file.path(raw_dir, paste0(product, "_", year, ".nc"))

fao_sst_read_table <- function(file) {
  key <- paste("sst", file, file.mtime(file))
  if (is.null(.fao_cache[[key]])) {
    .fao_cache[[key]] <- utils::read.csv(file, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                                         colClasses = c(area_code = "character", year = "integer"))
  }
  .fao_cache[[key]]
}

# Downloads one file; ERDDAP answers "503 Service Unavailable" to quick successive requests, so
# failed downloads are retried after a pause
fao_sst_download <- function(url, file, attempts = 6) {
  part <- paste0(file, ".part")
  for (attempt in seq_len(attempts)) {
    ok <- tryCatch(utils::download.file(url, part, mode = "wb", quiet = TRUE) == 0,
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) {
      file.rename(part, file)
      Sys.sleep(1)
      return(TRUE)
    }
    unlink(part)
    if (attempt < attempts) Sys.sleep(10 * attempt)
  }
  FALSE
}

fao_sst_update_manifest <- function(dir, file, url) {
  manifest_file <- file.path(dir, "files.csv")
  new <- data.frame(file = file, url = url, size = file.size(file.path(dir, "raw", file)),
                    downloaded = as.character(Sys.Date()), stringsAsFactors = FALSE)
  if (file.exists(manifest_file)) {
    old <- utils::read.csv(manifest_file, colClasses = "character")
    new <- rbind(old[old$file != file, names(new)], new)
  }
  utils::write.csv(new[order(new$file), ], manifest_file, row.names = FALSE)
}

# Description of the SST variable of a NetCDF file: its name, the order of its dimensions, the
# longitudes and latitudes (and their order from -180 to 180 and south to north), the dates of its
# time steps and its units
fao_sst_nc_meta <- function(file, variable = NULL) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  lon_names <- c("lon", "longitude", "x", "nav_lon")
  lat_names <- c("lat", "latitude", "y", "nav_lat")
  dim_names <- function(v) vapply(nc$var[[v]]$dim, function(d) d$name, character(1))
  if (is.null(variable)) {
    gridded <- names(nc$var)[vapply(names(nc$var), function(v) {
      d <- tolower(dim_names(v))
      any(d %in% lon_names) && any(d %in% lat_names) && "time" %in% d
    }, logical(1))]
    preferred <- c("sst", "analysed_sst", "tos", "sea_surface_temperature", "temp", "thetao")
    named <- gridded[tolower(gridded) %in% preferred]
    named <- named[order(match(tolower(named), preferred))]
    variable <- if (length(named) > 0) named[1] else if (length(gridded) == 1) gridded else NA_character_
    if (is.na(variable)) {
      stop("Could not find the SST variable in ", basename(file), ". Variables: ",
           paste(names(nc$var), collapse = ", "), ". Give its name as `variable`.", call. = FALSE)
    }
  } else if (!variable %in% names(nc$var)) {
    stop("Variable '", variable, "' not found in ", basename(file), ". Variables: ",
         paste(names(nc$var), collapse = ", "), call. = FALSE)
  }
  dims <- dim_names(variable)
  lengths <- vapply(nc$var[[variable]]$dim, function(d) d$len, numeric(1))
  lon_name <- dims[tolower(dims) %in% lon_names][1]
  lat_name <- dims[tolower(dims) %in% lat_names][1]
  time_name <- dims[tolower(dims) == "time"][1]
  if (is.na(lon_name) || is.na(lat_name) || is.na(time_name)) {
    stop("The variable '", variable, "' of ", basename(file), " needs longitude, latitude and time dimensions; ",
         "it has: ", paste(dims, collapse = ", "), call. = FALSE)
  }
  lon <- as.vector(nc$dim[[lon_name]]$vals)
  lat <- as.vector(nc$dim[[lat_name]]$vals)
  lon180 <- ifelse(lon > 180, lon - 360, lon)
  units <- ncdf4::ncatt_get(nc, variable, "units")$value
  title <- ncdf4::ncatt_get(nc, 0, "title")$value
  list(file = file, variable = variable, dims = dims, lengths = lengths,
       lon_name = lon_name, lat_name = lat_name, time_name = time_name,
       lon = sort(lon180), lat = sort(lat), lon_order = order(lon180), lat_order = order(lat),
       dates = fao_sst_dates(nc$dim[[time_name]]$vals, nc$dim[[time_name]]$units,
                             fao_sst_calendar(nc, time_name)),
       kelvin = isTRUE(grepl("^(K|kelvin|degK|deg_K|degrees_K|degree_kelvin)$", units, ignore.case = TRUE)),
       title = if (is.character(title)) title else "",
       grid = paste0(format(abs(lon[2] - lon[1])), " x ", format(abs(lat[2] - lat[1])), " degrees"))
}

# Reads the time steps `steps` of the SST variable described by `meta`, as a longitude x latitude
# x step array in degrees C, with longitudes from -180 to 180 and latitudes increasing
fao_sst_nc_read <- function(meta, steps) {
  nc <- ncdf4::nc_open(meta$file)
  on.exit(ncdf4::nc_close(nc))
  time_pos <- match(meta$time_name, meta$dims)
  keep <- match(c(meta$lon_name, meta$lat_name, meta$time_name), meta$dims)
  start <- rep(1, length(meta$dims))
  count <- ifelse(seq_along(meta$dims) %in% keep[1:2], meta$lengths, 1)
  out <- array(NA_real_, c(length(meta$lon), length(meta$lat), length(steps)))
  # Contiguous runs of time steps are read in one go
  runs <- split(steps, cumsum(c(1, diff(steps) != 1)))
  k <- 0
  for (run in runs) {
    start[time_pos] <- run[1]
    count[time_pos] <- length(run)
    x <- ncdf4::ncvar_get(nc, meta$variable, start = start, count = count, collapse_degen = FALSE)
    dim(x) <- count
    x <- aperm(x, c(keep, setdiff(seq_along(meta$dims), keep)))
    dim(x) <- count[keep]
    out[, , k + seq_along(run)] <- x[meta$lon_order, meta$lat_order, , drop = FALSE]
    k <- k + length(run)
  }
  if (meta$kelvin) out <- out - 273.15
  # Sea-ice flags of some products (-1000 in HadISST1): the value other products give under ice
  out[!is.na(out) & out < -5] <- -1.8
  out
}

# Calendar of the time axis of a NetCDF file ("standard" when not given)
fao_sst_calendar <- function(nc, time_name) {
  att <- ncdf4::ncatt_get(nc, time_name, "calendar")
  if (isTRUE(att$hasatt)) as.character(att$value) else "standard"
}

# Dates of a NetCDF time axis ("seconds since 1970-01-01T00:00:00Z", "days since 1891-1-1",
# "hours since 2000-01-01 12:00:00", ...), with the time of the origin and the calendar of the
# file (CF conventions: standard, gregorian, proleptic_gregorian, noleap or 365_day, all_leap or
# 366_day, 360_day). Only the year and month of the dates are used, so in the 360-day calendar the
# 29th and 30th days are given as the 28th.
fao_sst_dates <- function(values, units, calendar = "standard") {
  unit <- tolower(sub(" since .*$", "", units))
  seconds <- c(seconds = 1, second = 1, s = 1, minutes = 60, minute = 60, hours = 3600, hour = 3600,
               days = 86400, day = 86400)[unit]
  if (is.na(seconds)) stop("Unsupported time units in the NetCDF file: ", units, call. = FALSE)
  origin <- regmatches(units, regexec(paste0(" since\\s+(-?[0-9]+)-([0-9]+)-([0-9]+)",
                                             "(?:[T ]+([0-9]+):([0-9]+)(?::([0-9.]+))?)?"), units, perl = TRUE))[[1]]
  if (length(origin) == 0) stop("Unsupported time units in the NetCDF file: ", units, call. = FALSE)
  part <- suppressWarnings(as.numeric(origin[-1]))
  part[is.na(part)] <- 0
  # days since the origin, then the date in the calendar of the file
  days <- (part[4] * 3600 + part[5] * 60 + part[6]) / 86400 + as.numeric(values) * unname(seconds) / 86400
  calendar <- tolower(trimws(calendar))
  if (calendar %in% c("standard", "gregorian", "proleptic_gregorian", "")) {
    origin_date <- as.Date(sprintf("%04d-%02d-%02d", part[1], part[2], part[3]))
    return(origin_date + floor(days + 1e-9))
  }
  month_days <- switch(calendar,
                       "360_day" = rep(30, 12),
                       noleap = , "365_day" = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                       all_leap = , "366_day" = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                       stop("Unsupported calendar in the NetCDF file: ", calendar,
                            " (supported: standard, gregorian, proleptic_gregorian, noleap, 365_day, all_leap, ",
                            "366_day, 360_day)", call. = FALSE))
  year_days <- sum(month_days)
  start <- c(0, cumsum(month_days))
  total <- part[1] * year_days + start[part[2]] + part[3] - 1 + floor(days + 1e-9)
  year <- total %/% year_days
  rest <- total - year * year_days
  month <- findInterval(rest, start[1:12])
  day <- rest - start[month] + 1
  as.Date(sprintf("%04d-%02d-%02d", year, month, pmin(day, 28)))
}

# Monthly means of one year from all files (daily or monthly steps): a longitude x latitude x 12
# array, or NULL when a month is missing
fao_sst_year_months <- function(metas, year) {
  sums <- NULL
  counts <- NULL
  for (meta in metas) {
    in_year <- which(format(meta$dates, "%Y") == as.character(year))
    if (length(in_year) == 0) next
    if (is.null(sums)) {
      sums <- array(0, c(length(meta$lon), length(meta$lat), 12))
      counts <- sums
    }
    # At most about a month of daily steps is read at a time
    for (chunk in split(in_year, ceiling(seq_along(in_year) / 31))) {
      x <- fao_sst_nc_read(meta, chunk)
      month <- as.integer(format(meta$dates[chunk], "%m"))
      for (k in seq_along(chunk)) {
        layer <- x[, , k]
        ok <- !is.na(layer)
        s <- sums[, , month[k]]
        s[ok] <- s[ok] + layer[ok]
        sums[, , month[k]] <- s
        cnt <- counts[, , month[k]]
        cnt[ok] <- cnt[ok] + 1
        counts[, , month[k]] <- cnt
      }
    }
  }
  if (is.null(sums) || !all(apply(counts > 0, 3, any))) return(NULL)
  means <- sums / counts
  means[counts == 0] <- NA
  means
}

# Area table (see ?fao_area_sst) of all complete years in the files
fao_sst_compute <- function(files, variable, data_dir, product, source, date, quiet = FALSE, level = "area") {
  fao_sst_check_ncdf4()
  metas <- lapply(files, fao_sst_nc_meta, variable = variable)
  first <- metas[[1]]
  same_grid <- vapply(metas, function(m) identical(m$lon, first$lon) && identical(m$lat, first$lat), logical(1))
  if (!all(same_grid)) {
    stop("The NetCDF files do not all have the same grid, e.g. ", basename(files[which(!same_grid)[1]]), call. = FALSE)
  }
  years <- sort(unique(as.integer(unlist(lapply(metas, function(m) format(m$dates, "%Y"))))))
  # Grids finer than 1 degree are averaged to about 1 degree first
  resolution <- abs(first$lon[2] - first$lon[1])
  n <- if (resolution < 0.75) round(1 / resolution) else 1
  if (n > 1 && (length(first$lon) %% n != 0 || length(first$lat) %% n != 0)) n <- 1
  if (n > 1 && !quiet) message("The ", format(resolution), " degree grid is averaged to ", format(n * resolution), " degree.")
  lon <- if (n > 1) colMeans(matrix(first$lon, nrow = n)) else first$lon
  lat <- if (n > 1) colMeans(matrix(first$lat, nrow = n)) else first$lat

  # The weights cover every cell that has values in any year so far: a cell without values in the
  # first years (e.g. under sea ice) is added in the year it has values
  weights <- NULL
  covered <- rep(FALSE, length(lon) * length(lat))
  bar <- if (!quiet && length(years) > 1) utils::txtProgressBar(min = 0, max = length(years), style = 3) else NULL
  if (!is.null(bar)) on.exit(close(bar), add = TRUE)
  rows <- list()
  for (k in seq_along(years)) {
    if (!is.null(bar)) utils::setTxtProgressBar(bar, k)
    months <- fao_sst_year_months(metas, years[k])
    if (is.null(months)) next
    if (n > 1) months <- fao_sst_block_mean(months, first$lon, first$lat, n)$sst
    sea <- as.vector(apply(!is.na(months), c(1, 2), any))
    if (is.null(weights) || any(sea & !covered)) {
      covered <- covered | sea
      weights <- fao_sst_weights(lon, lat, covered, data_dir, level)
    }
    rows[[length(rows) + 1]] <- fao_sst_area_stats(months, weights, years[k])
  }
  if (length(rows) == 0) stop("The NetCDF files have no complete year (12 months).", call. = FALSE)
  table <- cbind(product = product, do.call(rbind, rows), source = source, downloaded = date,
                 stringsAsFactors = FALSE)
  table <- table[order(as.integer(sub("\\..*$", "", table$area_code)), table$area_code, table$year), ]
  rownames(table) <- NULL
  table
}

# Averages blocks of n x n cells (mean of the cells with a value; NA when none has a value)
fao_sst_block_mean <- function(x, lon, lat, n) {
  months <- dim(x)[3]
  # Sums groups of n along the first dimension
  block_sum <- function(a) {
    d <- dim(a)
    array(colSums(matrix(a, nrow = n)), c(d[1] / n, d[-1]))
  }
  both <- function(a) aperm(block_sum(aperm(block_sum(a), c(2, 1, 3))), c(2, 1, 3))
  values <- x
  values[is.na(values)] <- 0
  total <- both(values)
  count <- both(array(as.numeric(!is.na(x)), dim(x)))
  y <- total / count
  y[count == 0] <- NA
  dim(y) <- c(length(lon) / n, length(lat) / n, months)
  list(sst = y, lon = colMeans(matrix(lon, nrow = n)), lat = colMeans(matrix(lat, nrow = n)))
}

# Weight of each grid cell in each FAO major area (or in each subarea and division): the area of
# their overlap in km2, computed on the sphere (planar areas in degrees are wrong for cells that
# cross the 180 degree meridian). `sea` marks the cells with values. The weights of a cell do not
# depend on the other cells, so they are saved by grid (all its longitudes and latitudes) in the sst
# folder of data_dir with the cells computed so far, and cells new to a grid are added.
fao_sst_weights <- function(lon, lat, sea, data_dir, level = "area") {
  key <- paste(c("km2v2", if (level == "division") "division", length(lon), length(lat), rlang::hash(list(lon, lat))),
               collapse = "_")
  file <- file.path(data_dir, "sst", "weights", paste0(key, ".rds"))
  saved <- if (file.exists(file)) readRDS(file) else NULL
  done <- if (is.null(saved)) integer(0) else attr(saved, "cells")
  cells <- which(sea)
  new <- setdiff(cells, done)
  if (length(new) > 0) {
    weights <- rbind(saved, fao_sst_cell_weights(lon, lat, new, level))
    attr(weights, "cells") <- sort(union(done, new))
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(weights, file)
    saved <- weights
  }
  out <- saved[saved$cell %in% cells, ]
  rownames(out) <- NULL
  out
}

# Overlap (km2) of the grid cells `cells` with the FAO areas (or subareas and divisions)
fao_sst_cell_weights <- function(lon, lat, cells, level) {
  half_lon <- abs(lon[2] - lon[1]) / 2
  half_lat <- abs(lat[2] - lat[1]) / 2
  i <- (cells - 1) %% length(lon) + 1
  j <- (cells - 1) %/% length(lon) + 1
  boxes <- lapply(seq_along(cells), function(k) {
    x <- lon[i[k]]
    y <- lat[j[k]]
    sf::st_polygon(list(rbind(c(x - half_lon, y - half_lat), c(x + half_lon, y - half_lat),
                              c(x + half_lon, y + half_lat), c(x - half_lon, y + half_lat),
                              c(x - half_lon, y - half_lat))))
  })
  grid <- sf::st_sf(cell = cells, geometry = sf::st_sfc(boxes, crs = 4326))
  areas <- sf::st_read(system.file("extdata", "FAO_AREAS_CWP.shp", package = "faofishingreport"), quiet = TRUE)
  levels <- if (level == "area") "MAJOR" else c("SUBAREA", "DIVISION")
  areas <- sf::st_make_valid(areas[areas$F_LEVEL %in% levels, c("F_CODE", "NAME_EN")])
  use_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(TRUE))
  on.exit(suppressMessages(sf::sf_use_s2(use_s2)))
  overlap <- sf::st_make_valid(suppressWarnings(sf::st_intersection(grid, areas)))
  weight <- as.numeric(sf::st_area(overlap)) / 1e6
  weights <- data.frame(cell = overlap$cell, area_code = as.character(overlap$F_CODE), area_en = overlap$NAME_EN,
                        weight = weight, stringsAsFactors = FALSE)
  weights[weights$weight > 0, ]
}

# Weighted quantile: the smallest value whose cumulative weight reaches the probability
fao_weighted_quantile <- function(x, w, p) {
  o <- order(x)
  x <- x[o]
  cumulative <- cumsum(w[o]) / sum(w)
  vapply(p, function(q) x[which(cumulative >= q - 1e-12)[1]], numeric(1))
}

# Yearly mean and ranges of each area from one year of SST (longitude x latitude x 12 months)
fao_sst_area_stats <- function(sst, weights, year) {
  values <- matrix(sst, ncol = dim(sst)[3])
  do.call(rbind, lapply(split(weights, weights$area_code), function(w) {
    v <- values[w$cell, , drop = FALSE]
    monthly <- vapply(seq_len(ncol(v)), function(m) {
      ok <- !is.na(v[, m])
      sum(v[ok, m] * w$weight[ok]) / sum(w$weight[ok])
    }, numeric(1))
    cell_mean <- rowMeans(v)
    ok <- !is.na(cell_mean)
    range <- fao_weighted_quantile(cell_mean[ok], w$weight[ok], c(0.1, 0.9))
    data.frame(area_code = w$area_code[1], area_en = w$area_en[1], year = as.integer(year),
               sst_mean = round(mean(monthly), 3), sst_p10 = round(range[1], 3), sst_p90 = round(range[2], 3),
               sst_min_month = round(min(monthly), 3), sst_max_month = round(max(monthly), 3),
               n_cells = sum(ok), stringsAsFactors = FALSE)
  }))
}
