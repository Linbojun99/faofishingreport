# Sea surface temperature (SST) of the FAO major marine fishing areas from gridded SST products:
# the products, their download, and the yearly area means. The table of the default product (NOAA
# ERSST v5) is bundled with the package; the others are downloaded and computed on the user's
# computer with download_sst_data().

#' Sea Surface Temperature Products
#'
#' This function lists the sea surface temperature (SST) products that `fao_area_sst()`,
#' `download_sst_data()` and `mean_temperature_catch()` can use: their provider, grid, years,
#' data sources, treatment of sea ice, licence and the reference to cite.
#'
#' \describe{
#'   \item{ersst5 (default)}{NOAA Extended Reconstructed SST version 5 (Huang et al. 2017): 2° x
#'     2°, monthly, from 1854. In situ observations only (ships and buoys of ICOADS, Argo floats
#'     from 1999), so the record is consistent over the whole period. Coarse for small areas such
#'     as the Mediterranean and Black Sea. Its table is bundled with the package.}
#'   \item{hadisst1}{Met Office Hadley Centre HadISST1 (Rayner et al. 2003): 1° x 1°, monthly,
#'     from 1870. In situ observations (Met Office Marine Data Bank, with GTS reports from 1982;
#'     ICOADS monthly medians for 1871-1995), reconstructed by reduced-space optimal
#'     interpolation. The Met Office has warned (2016) that problems found in the dataset may limit
#'     its use where high resolution or SST gradients matter.}
#'   \item{cobe2}{Japan Meteorological Agency COBE-SST2 (Hirahara et al. 2014): 1° x 1°, monthly,
#'     from 1850. In situ SST and sea-ice observations; satellite observations are used to
#'     reconstruct variability in regions with few observations.}
#'   \item{oisst21}{NOAA Optimum Interpolation SST version 2.1 (Huang et al. 2021): daily 0.25° x
#'     0.25° analysis from September 1981 (monthly means from NOAA PSL; averaged to 1° by this
#'     package). Satellite (AVHRR), ship, buoy and Argo observations. The best resolution, but
#'     only from 1982, so it cannot show earlier changes.}
#' }
#'
#' All products are spatially complete reconstructions (interpolated where there are no
#' observations), and all estimate SST under or near sea ice from sea-ice data: ERSST v5,
#' COBE-SST2 and OISST v2.1 give values near -1.8 °C under ice; HadISST1 marks ice-covered cells
#' with -1000, which this package sets to -1.8 °C. Values over the Arctic Sea and the Antarctic
#' areas therefore partly reflect sea ice rather than measured water temperature. The products use
#' different observations and methods, so their values and trends differ somewhat; repeating an
#' analysis with a second product shows how much a result depends on the product.
#'
#' @return A data frame with one row per product: `product` (the name used in the functions),
#'   `name`, `provider`, `grid`, `first_year` (first complete year), `data_sources`, `sea_ice`,
#'   `licence`, `citation`, `doi`, `download` (server and dataset), `mb_per_year` (approximate
#'   download size) and `bundled` (whether the area table comes with the package).
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
#' @export
#' @examples
#' fao_sst_products()[, c("product", "name", "grid", "first_year", "bundled")]
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
    download = vapply(p, `[[`, character(1), "download"),
    mb_per_year = vapply(p, `[[`, numeric(1), "mb_per_year"),
    bundled = names(p) == "ersst5",
    row.names = NULL, stringsAsFactors = FALSE
  )
}


#' Download a Sea Surface Temperature Product and Compute the Area Means
#'
#' This function downloads a sea surface temperature (SST) product (see `fao_sst_products()`)
#' one year at a time, and computes the yearly SST of each FAO major marine fishing area, which
#' `fao_area_sst()` and `mean_temperature_catch()` then read. Files are kept in the `sst` folder
#' of `data_dir`, with `files.csv` recording where and when each was downloaded; years already
#' downloaded are skipped, so an interrupted download can be resumed by running the function
#' again. The table of the default product, ERSST v5, is bundled with the package; running the
#' function for it adds years published since.
#'
#' The area means are computed as follows. Each grid cell counts for each FAO major area it
#' overlaps (`inst/extdata/FAO_AREAS_CWP.shp`, the 19 marine areas), weighted by the area of the
#' overlap (computed on the sphere), so cells on a boundary are shared and coastal cells count for
#' their sea part only (OISST v2.1 is first averaged to 1°). For each year:
#' \itemize{
#'   \item `sst_mean`: the mean of the 12 monthly area means;
#'   \item `sst_p10`, `sst_p90`: the weighted 10th and 90th percentiles of the yearly mean SST of
#'     the cells of the area, i.e. the range of temperature within the area;
#'   \item `sst_min_month`, `sst_max_month`: the coldest and warmest monthly area means, i.e. the
#'     seasonal range. In areas on both sides of the equator the seasons of the two hemispheres
#'     partly cancel, so this range is narrow there.
#' }
#' Only complete years (12 months) are kept. Reading the files needs the ncdf4 package.
#'
#' @param product The SST product: "ersst5" (default), "hadisst1", "cobe2" or "oisst21"; see
#'        `fao_sst_products()`.
#' @param years The years to download. Default is NULL: from 1950 (1982 for OISST v2.1) to the
#'        last complete year available.
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
#' fao_area_sst("34", product = "hadisst1")
#' }
download_sst_data <- function(product = "ersst5", years = NULL, data_dir = fao_data_dir(),
                              overwrite = FALSE, quiet = FALSE) {
  product <- fao_sst_check_product(product)
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Reading SST files needs the ncdf4 package: install.packages(\"ncdf4\")", call. = FALSE)
  }
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
    if (length(fao_sst_read_year(file, product, year)$months) < 12) {
      unlink(file)
      message(info$name, " ", year, " is not complete yet; stopping at ", year - 1, ".")
      break
    }
    fao_sst_update_manifest(dir, basename(file), url)
    if (!is.null(bar)) utils::setTxtProgressBar(bar, k)
  }

  table <- fao_sst_build_table(product, raw_dir, data_dir, quiet)
  fao_write_csv(table, file.path(dir, "fao_area_sst.csv"))
  if (!quiet) message(info$name, ": area table with ", nrow(table), " rows, ", min(table$year), "-",
                      max(table$year), ", saved in ", dir)
  invisible(table)
}


#' Sea Surface Temperature of the FAO Major Fishing Areas
#'
#' This function returns the yearly sea surface temperature (SST) of the 19 FAO major marine
#' fishing areas from a gridded SST product (see `fao_sst_products()`): the mean, the range
#' within the area (10th to 90th percentile of the cells) and the seasonal range (coldest and
#' warmest month). It is used by `mean_temperature_catch()`; `download_sst_data()` describes how
#' the values are computed.
#'
#' The default product, NOAA ERSST v5, is bundled with the package (from 1950 to the year before
#' the package was built); if it has been updated with `download_sst_data()`, the newer table is
#' used. The other products must first be downloaded with `download_sst_data()`.
#'
#' @param area FAO area codes (e.g. "34") or names as in the FAO data (e.g. "Atlantic, Eastern
#'        Central"). Default is NULL, all areas.
#' @param product The SST product: "ersst5" (default), "hadisst1", "cobe2" or "oisst21".
#' @param data_dir The directory where downloaded data is stored. Default is `fao_data_dir()`.
#' @return A data frame with one row per area and year: `product`, `area_code`, `area_en`,
#'   `year`, `sst_mean`, `sst_p10`, `sst_p90`, `sst_min_month`, `sst_max_month` (all in °C),
#'   `n_cells` (grid cells in the area), `source` (product name) and `downloaded` (date of the
#'   last download).
#' @inherit fao_sst_products references
#' @export
#' @examples
#' \dontrun{
#' sst34 <- fao_area_sst("34")
#' plot(sst_mean ~ year, data = sst34, type = "l", ylim = range(sst34$sst_p10, sst34$sst_p90))
#' lines(sst_p10 ~ year, data = sst34, lty = 2)
#' lines(sst_p90 ~ year, data = sst34, lty = 2)
#' }
fao_area_sst <- function(area = NULL, product = "ersst5", data_dir = fao_data_dir()) {
  product <- fao_sst_check_product(product)
  local_file <- file.path(data_dir, "sst", product, "fao_area_sst.csv")
  sst <- if (file.exists(local_file)) fao_sst_read_table(local_file) else NULL
  if (product == "ersst5") {
    bundled <- fao_sst_read_table(system.file("extdata", "fao_area_sst.csv", package = "faofishingreport"))
    if (is.null(sst) || max(sst$year) <= max(bundled$year)) sst <- bundled
  } else if (is.null(sst)) {
    info <- fao_sst_registry[[product]]
    stop(info$name, " has not been downloaded. Run download_sst_data(\"", product, "\") first (about ",
         round(info$mb_per_year * (as.integer(format(Sys.Date(), "%Y")) - max(1950, info$first_year))), " MB).",
         call. = FALSE)
  }
  if (!is.null(area)) {
    area <- as.character(area)
    unknown <- setdiff(area, c(sst$area_code, sst$area_en))
    if (length(unknown) > 0) {
      stop("Unknown FAO major marine fishing area: ", paste(unknown, collapse = ", "), ". Areas: ",
           paste(unique(paste(sst$area_code, sst$area_en)), collapse = "; "), call. = FALSE)
    }
    sst <- sst[sst$area_code %in% area | sst$area_en %in% area, ]
  }
  rownames(sst) <- NULL
  sst
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
    download = "NOAA PSL THREDDS, Datasets/COBE2/sst.mon.mean.nc", mb_per_year = 3.4,
    url = function(year) {
      paste0("https://psl.noaa.gov/thredds/ncss/grid/Datasets/COBE2/sst.mon.mean.nc?var=sst&time_start=",
             year, "-01-01T00:00:00Z&time_end=", year, "-12-31T00:00:00Z&accept=netcdf")
    }
  ),
  oisst21 = list(
    name = "NOAA OISST v2.1", provider = "NOAA NCEI; monthly means from NOAA PSL",
    grid = "0.25 x 0.25 degree, monthly means of daily analyses (averaged to 1 degree by this package)",
    first_year = 1982,
    data_sources = "Satellite (AVHRR), ships, buoys and Argo floats",
    sea_ice = "Proxy SST from sea-ice concentration in the marginal ice zone",
    licence = "NOAA data, free; NOAA PSL asks users to acknowledge data provided by the NOAA PSL, Boulder, Colorado, USA",
    citation = "Huang et al. (2021) Journal of Climate 34: 2923-2939", doi = "10.1175/JCLI-D-20-0166.1",
    download = "NOAA PSL THREDDS, Datasets/noaa.oisst.v2.highres/sst.mon.mean.nc", mb_per_year = 11,
    url = function(year) {
      paste0("https://psl.noaa.gov/thredds/ncss/grid/Datasets/noaa.oisst.v2.highres/sst.mon.mean.nc?var=sst",
             "&time_start=", year, "-01-01T00:00:00Z&time_end=", year, "-12-31T00:00:00Z&accept=netcdf")
    }
  )
)

fao_sst_check_product <- function(product) {
  if (!is.character(product) || length(product) != 1 || !product %in% names(fao_sst_registry)) {
    stop("Unknown SST product: ", paste(product, collapse = ", "), ". Products: ",
         paste(names(fao_sst_registry), collapse = ", "), " (see fao_sst_products()).", call. = FALSE)
  }
  product
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

# Reads the 12 months of one year of a downloaded file: SST as a longitude x latitude x month
# array, with longitudes from -180 to 180 and latitudes increasing
fao_sst_read_year <- function(file, product, year) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  dims <- vapply(nc$var$sst$dim, function(d) d$name, character(1))
  lengths <- vapply(nc$var$sst$dim, function(d) d$len, numeric(1))
  x <- ncdf4::ncvar_get(nc, "sst", collapse_degen = FALSE)
  dim(x) <- lengths[lengths > 1 | dims %in% c("time")]
  dims <- dims[lengths > 1 | dims %in% c("time")]
  lon_name <- intersect(c("longitude", "lon"), dims)
  lat_name <- intersect(c("latitude", "lat"), dims)
  x <- aperm(x, match(c(lon_name, lat_name, "time"), dims))
  lon <- as.vector(nc$dim[[lon_name]]$vals)
  lat <- as.vector(nc$dim[[lat_name]]$vals)
  dates <- fao_sst_dates(nc$dim$time$vals, nc$dim$time$units)

  keep <- which(as.integer(format(dates, "%Y")) == year)
  keep <- keep[!duplicated(format(dates[keep], "%m"))]
  x <- x[, , keep, drop = FALSE]
  if (product == "hadisst1") x[!is.na(x) & x <= -999] <- -1.8
  lon <- ifelse(lon > 180, lon - 360, lon)
  x <- x[order(lon), order(lat), , drop = FALSE]
  lon <- sort(lon)
  lat <- sort(lat)
  if (product == "oisst21") {
    block <- fao_sst_block_mean(x, lon, lat, 4)
    x <- block$sst
    lon <- block$lon
    lat <- block$lat
  }
  list(sst = x, lon = lon, lat = lat, months = format(dates[keep], "%m"))
}

# Dates of a NetCDF time axis ("seconds since 1970-01-01T00:00:00Z", "days since 1891-1-1", ...)
fao_sst_dates <- function(values, units) {
  unit <- tolower(sub(" since .*$", "", units))
  origin <- as.POSIXct(sub("^.* since ([0-9]+-[0-9]+-[0-9]+).*$", "\\1", units), format = "%Y-%m-%d", tz = "UTC")
  seconds <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)[[unit]]
  as.Date(origin + values * seconds)
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

# Weight of each grid cell in each FAO major area: the area of their overlap in km2, computed on
# the sphere (planar areas in degrees are wrong for cells that cross the 180 degree meridian).
# `sea` marks the cells with values. Saved in the sst folder of data_dir.
fao_sst_weights <- function(lon, lat, sea, data_dir) {
  key <- paste("km2", length(lon), length(lat), lon[1], lat[1], sum(sea), sep = "_")
  file <- file.path(data_dir, "sst", "weights", paste0(gsub("[^A-Za-z0-9_]", "_", key), ".rds"))
  if (file.exists(file)) return(readRDS(file))
  half_lon <- abs(lon[2] - lon[1]) / 2
  half_lat <- abs(lat[2] - lat[1]) / 2
  cells <- which(sea)
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
  areas <- sf::st_make_valid(areas[areas$F_LEVEL == "MAJOR", c("F_CODE", "NAME_EN")])
  use_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(TRUE))
  on.exit(suppressMessages(sf::sf_use_s2(use_s2)))
  overlap <- sf::st_make_valid(suppressWarnings(sf::st_intersection(grid, areas)))
  weight <- as.numeric(sf::st_area(overlap)) / 1e6
  weights <- data.frame(cell = overlap$cell, area_code = as.character(overlap$F_CODE), area_en = overlap$NAME_EN,
                        weight = weight, stringsAsFactors = FALSE)
  weights <- weights[weights$weight > 0, ]
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(weights, file)
  weights
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

# Area table of all complete years downloaded for a product
fao_sst_build_table <- function(product, raw_dir, data_dir, quiet) {
  files <- list.files(raw_dir, pattern = paste0("^", product, "_[0-9]{4}\\.nc$"), full.names = TRUE)
  if (length(files) == 0) stop("No downloaded files of ", product, " in ", raw_dir, call. = FALSE)
  years <- sort(as.integer(sub("^.*_([0-9]{4})\\.nc$", "\\1", files)))
  first <- fao_sst_read_year(fao_sst_raw_file(raw_dir, product, years[1]), product, years[1])
  sea <- as.vector(apply(!is.na(first$sst), c(1, 2), any))
  if (!quiet) message("Computing the area means (the grid weights are computed once and saved) ...")
  weights <- fao_sst_weights(first$lon, first$lat, sea, data_dir)
  table <- do.call(rbind, lapply(years, function(year) {
    y <- fao_sst_read_year(fao_sst_raw_file(raw_dir, product, year), product, year)
    if (length(y$months) < 12) return(NULL)
    if (!identical(y$lon, first$lon) || !identical(y$lat, first$lat)) {
      stop("The grid of ", product, " ", year, " differs from that of ", years[1], call. = FALSE)
    }
    fao_sst_area_stats(y$sst, weights, year)
  }))
  manifest <- utils::read.csv(file.path(dirname(raw_dir), "files.csv"), colClasses = "character")
  table <- cbind(product = product, table, source = fao_sst_registry[[product]]$name,
                 downloaded = max(manifest$downloaded), stringsAsFactors = FALSE)
  table <- table[order(as.integer(table$area_code), table$year), ]
  rownames(table) <- NULL
  table
}
