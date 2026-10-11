#' Catch by Division: FAO Data of the Regional Fishery Bodies
#'
#' This function returns the capture production by division (parts of an FAO major fishing area)
#' that FAO publishes for four regions, in the format of `load_fao_data()` with three more columns:
#' `Division` (code, e.g. "37.1.1"), `Division..Name.` (code and name, e.g. "37.1.1 Balearic") and
#' `Region`. It can be given to `pelagic_demersal()` and `mean_temperature_catch()` with
#' `by = "division"`, or to any other analysis as catch data.
#' \describe{
#'   \item{GFCM}{General Fisheries Commission for the Mediterranean: area 37 (Mediterranean and
#'     Black Sea), divisions such as 37.1.1 Balearic, 37.2.1 Adriatic, 37.4.2 Black Sea.}
#'   \item{CECAF}{Fishery Committee for the Eastern Central Atlantic: area 34.}
#'   \item{SEATL}{Southeast Atlantic: area 47.}
#'   \item{RECOFI}{Regional Commission for Fisheries: part of area 51 (the Gulf, the Sea of Oman and the southwestern Arabian Sea).}
#' }
#' In release 2026.1.0, GFCM and CECAF start in 1970, SEATL in 1975 and RECOFI in 1986. Some codes
#' are not places: "Tunas" (e.g. 34.0.0, tunas reported for the whole area) and "Not known"
#' (e.g. 37.9.0, or 34.1.9 for a subarea whose division is not known).
#'
#' The divisions of GFCM, CECAF and SEATL cover the whole FAO area: summed over the divisions, the
#' catch of a country, species and year equals that of the area in `load_fao_data()` for nearly
#' all records found in both (99.7 to 100% within 1% in release 2026.1.0), and the records found
#' in one dataset only hold less than 2.2% of the catch. RECOFI covers only the Gulf, the Sea of
#' Oman and the southwestern Arabian Sea, a part of area 51.
#'
#' The data come from the file FI_Regional of the FAO data index
#' (\url{https://www.fao.org/fishery/static/Data/}). When the computer is online, the newest
#' release is used, downloaded and converted the first time (about 5 MB) and saved in `data_dir`;
#' offline, or with `fao_settings(online = FALSE)`, the newest saved release is used.
#'
#' @param region The regions: any of "GFCM", "CECAF", "SEATL" and "RECOFI". Default is NULL, all.
#' @param version The release, e.g. "2026.1.0". Default is "latest".
#' @param data_dir The directory where the data is saved. Default is `fao_data_dir()`.
#' @return A data frame with the columns of `load_fao_data()` and `Division`, `Division..Name.`
#'   and `Region`, with the attributes `fao_version` and `source_url`.
#' @references
#' FAO. Fishery and Aquaculture Statistics: capture production by regional fishery bodies
#' (FI_Regional). \url{https://www.fao.org/fishery/statistics-query/en}
#' @export
#' @examples
#' \dontrun{
#' med <- load_fao_divisions("GFCM")
#' pelagic_demersal(med, by = "division")
#' mean_temperature_catch(med, by = "division")
#' }
load_fao_divisions <- function(region = NULL, version = "latest", data_dir = fao_data_dir()) {
  regions <- c("GFCM", "CECAF", "SEATL", "RECOFI")
  if (!is.null(region)) {
    unknown <- setdiff(region, regions)
    if (length(unknown) > 0) {
      stop("Unknown region: ", paste(unknown, collapse = ", "), ". Regions: ", paste(regions, collapse = ", "),
           call. = FALSE)
    }
  }
  local <- fao_division_versions(data_dir)
  online <- if (isTRUE(fao_setting("online"))) {
    tryCatch(fao_list_releases("FI_Regional"), error = function(e) {
      message("Could not read the FAO data index (", conditionMessage(e), ").")
      NULL
    })
  }
  if (identical(version, "latest")) {
    version <- if (!is.null(online)) online$version[nrow(online)] else if (length(local) > 0) local[length(local)] else
      stop("The FAO data index cannot be reached and no regional data has been downloaded.", call. = FALSE)
  }
  file <- fao_division_file(version, data_dir)
  if (!file.exists(file)) {
    if (is.null(online)) {
      stop("Regional data release ", version, " has not been downloaded and the FAO data index cannot be reached.",
           call. = FALSE)
    }
    release <- online[online$version == version, ]
    if (nrow(release) == 0) {
      stop("Regional data release ", version, " is not available. Releases: ", paste(online$version, collapse = ", "),
           call. = FALSE)
    }
    zip_file <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_file), add = TRUE)
    old <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    message("Downloading ", release$url, " (", release$size, ") ...")
    utils::download.file(release$url, zip_file, mode = "wb", quiet = TRUE)
    data <- fao_convert_divisions(zip_file)
    attr(data, "fao_version") <- version
    attr(data, "source_url") <- release$url
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(data, file)
  }
  data <- readRDS(file)
  if (!is.null(region)) {
    keep <- data$Region %in% region
    data <- structure(data[keep, ], fao_version = attr(data, "fao_version"), source_url = attr(data, "source_url"))
  }
  rownames(data) <- NULL
  years <- tapply(data$Year, data$Region, range)
  message("Using FAO regional capture data (FI_Regional) release ", version, ": ",
          paste0(names(years), " ", vapply(years, paste, character(1), collapse = "-"), collapse = ", "), ".")
  data
}


fao_division_file <- function(version, data_dir) {
  file.path(data_dir, paste0("FAO_Regional_", version, ".rds"))
}

fao_division_versions <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^FAO_Regional_[0-9.]+\\.rds$")
  versions <- sub("^FAO_Regional_([0-9.]+)\\.rds$", "\\1", files)
  versions[order(numeric_version(versions))]
}

# Converts an FI_Regional zip to the package format with the division columns
fao_convert_divisions <- function(zip_file, lookup = fao_lookup_tables()) {
  listed <- utils::unzip(zip_file, list = TRUE)$Name
  read <- function(file) {
    utils::read.csv(unz(zip_file, file), colClasses = "character", encoding = "UTF-8", check.names = FALSE,
                    na.strings = "")
  }
  countries <- read("CL_FI_COUNTRY_GROUPS.csv")
  species <- read("CL_FI_SPECIES_GROUPS.csv")
  divisions <- read("CL_FI_WATERAREA_DIVISION.csv")
  quantity_files <- grep("_Capture_Quantity\\.csv$", listed, value = TRUE)
  quantity <- do.call(rbind, lapply(quantity_files, function(f) {
    x <- read(f)
    x$Region <- sub("_Capture_Quantity\\.csv$", "", f)
    x[x$MEASURE == "Q_tlw", c("COUNTRY.UN_CODE", "SPECIES.ALPHA_3_CODE", "DIVISION.CODE", "PERIOD", "VALUE",
                              "STATUS", "Region")]
  }))
  areas <- fao_group_table()$area
  area_code <- sub("\\..*$", "", quantity$DIVISION.CODE)
  row <- match(quantity$DIVISION.CODE, divisions$Code)
  division_name <- ifelse(is.na(divisions$Name_En[row]), divisions$Local_Name[row], divisions$Name_En[row])
  data.frame(
    Country..Name. = fao_country_names(quantity$COUNTRY.UN_CODE, countries, lookup),
    ASFIS.species..Name. = fao_species_names(quantity$SPECIES.ALPHA_3_CODE, species, lookup),
    FAO.major.fishing.area..Name. = areas$name_en[match(area_code, areas$area_code)],
    Unit..Name. = "Tonnes - live weight",
    Unit = "TLW, Number",
    Year = as.integer(quantity$PERIOD),
    Production = as.numeric(quantity$VALUE),
    Production.Type = fao_production_type(quantity$STATUS),
    Division = quantity$DIVISION.CODE,
    Division..Name. = ifelse(is.na(division_name), quantity$DIVISION.CODE, paste(quantity$DIVISION.CODE, division_name)),
    Region = quantity$Region,
    stringsAsFactors = FALSE
  )
}

# Production types of the package data, from FAO status symbols
fao_production_type <- function(status) {
  names <- c(A = "actual", E = "estimate", I = "estimate", N = "neligible", P = "provisional", Q = "suppressed",
             X = "international organization")
  out <- unname(names[status])
  out[is.na(out)] <- status[is.na(out)]
  out
}

# The spatial units of an analysis: FAO major areas, or divisions (data from load_fao_divisions())
fao_spatial_units <- function(data, by) {
  if (by == "area") {
    areas <- fao_group_table()$area
    return(list(name = data$FAO.major.fishing.area..Name.,
                code = areas$area_code[match(data$FAO.major.fishing.area..Name., areas$name_en)]))
  }
  if (!all(c("Division", "Division..Name.") %in% names(data))) {
    stop("by = \"division\" needs catch data by division, e.g. load_fao_divisions(\"GFCM\").", call. = FALSE)
  }
  list(name = data$Division..Name., code = data$Division)
}

# The unit of an SST table used for each division code: the division itself, else the subarea
# (e.g. 34.2.0 -> 34.2, 34.1.9 "not known" -> 34.1, 51.3.1 -> 51.3); NA for codes that are not
# places (34.0.0 tunas, 34.9.0 not known)
fao_division_sst_unit <- function(code, available) {
  short <- sub("\\.0$", "", code)
  subarea <- sub("^([^.]+\\.[^.]+)\\..*$", "\\1", code)
  ifelse(code %in% available, code, ifelse(short %in% available, short, ifelse(subarea %in% available, subarea, NA)))
}
