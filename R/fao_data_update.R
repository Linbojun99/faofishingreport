#' Check for the Latest FAO Global Capture Production Release
#'
#' This function reads the FAO fishery data index, lists the Global Capture
#' Production releases published online, and compares the newest release with
#' the data bundled with the package and the data downloaded on this computer.
#'
#' @param data_dir The directory where downloaded releases are stored. Default is `fao_data_dir()`.
#' @param quiet A logical value. If TRUE, no message is printed. Default is FALSE.
#' @return Invisibly, a list with `latest` (newest online version), `bundled`
#'   (version bundled with the package), `local` (newest downloaded version, or NA
#'   if none), `update_available` (logical) and `releases` (a data frame of all
#'   online releases).
#' @family FAO data
#' @export
#' @examples
#' \dontrun{
#' check_fao_update()
#' }
check_fao_update <- function(data_dir = fao_data_dir(), quiet = FALSE) {
  releases <- fao_list_releases()
  latest <- releases[nrow(releases), ]

  bundled <- fao_bundled_release()
  local_versions <- fao_local_versions(data_dir)
  local <- if (length(local_versions) > 0) local_versions[length(local_versions)] else NA_character_
  current <- max(numeric_version(c(bundled, local[!is.na(local)])))
  update_available <- numeric_version(latest$version) > current

  if (!quiet) {
    message("FAO Global Capture Production")
    message("  Latest online release: ", latest$version, " (published ", latest$date, ")")
    message("  Bundled with package:  ", bundled)
    if (is.na(local)) {
      message("  Downloaded locally:    none")
    } else {
      message("  Downloaded locally:    ", local, " (", data_dir, ")")
    }
    if (update_available) {
      message("  A newer release is available. Run download_fao_data() to download it.")
    } else {
      message("  Your data is up to date.")
    }
  }

  invisible(list(latest = latest$version, bundled = bundled, local = local,
                 update_available = update_available, releases = releases))
}


#' Download an FAO Global Capture Production Release
#'
#' This function downloads a Global Capture Production release from FAO,
#' converts it to the same long format as the data bundled with the package
#' (`Global_Production_long`), and saves it as an `.rds` file in `data_dir`.
#'
#' Country and species names are matched through their FAO codes to the names
#' used by the bundled data, so the result works with the existing analysis
#' functions and Chinese name tables. Only quantities in tonnes live weight are
#' kept, as in the bundled data.
#'
#' @param version The release to download, e.g. "2026.1.0". Default is "latest".
#' @param data_dir The directory where the converted data is saved. Default is `fao_data_dir()`.
#' @param overwrite A logical value. If FALSE (default), a release that was already
#'   downloaded is read from disk instead of being downloaded again.
#' @param quiet A logical value. If TRUE, progress messages are not printed. Default is FALSE.
#' @return Invisibly, a data frame with the columns of `Global_Production_long`.
#' @family FAO data
#' @export
#' @examples
#' \dontrun{
#' check_fao_update()
#' new_data <- download_fao_data()
#' }
download_fao_data <- function(version = "latest", data_dir = fao_data_dir(),
                              overwrite = FALSE, quiet = FALSE) {
  releases <- fao_list_releases()
  if (identical(version, "latest")) {
    version <- releases$version[nrow(releases)]
  }
  if (!version %in% releases$version) {
    stop("Release ", version, " is not available online. Available releases: ",
         paste(releases$version, collapse = ", "), call. = FALSE)
  }
  release <- releases[releases$version == version, ]

  out_file <- fao_data_file(version, data_dir)
  if (file.exists(out_file) && !overwrite) {
    if (!quiet) message("Release ", version, " is already downloaded: ", out_file)
    return(invisible(readRDS(out_file)))
  }

  zip_file <- tempfile(fileext = ".zip")
  exdir <- tempfile("fao_capture_")
  on.exit(unlink(c(zip_file, exdir), recursive = TRUE), add = TRUE)

  old_options <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old_options), add = TRUE)

  if (!quiet) message("Downloading ", release$url, " (", release$size, ") ...")
  utils::download.file(release$url, zip_file, mode = "wb", quiet = quiet)

  if (!quiet) message("Converting to the package data format ...")
  capture <- fao_read_capture(zip_file, exdir)
  lookup <- fao_lookup_tables()
  data <- fao_convert_capture(capture, lookup)
  attr(data, "fao_version") <- version
  attr(data, "release_date") <- release$date
  attr(data, "source_url") <- release$url
  attr(data, "download_date") <- Sys.Date()

  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, out_file)
  # Official Chinese names and species/country/area groups of this release
  saveRDS(fao_build_name_table(capture, lookup), fao_names_file(version, data_dir))
  saveRDS(fao_build_group_table(capture, lookup), fao_groups_file(version, data_dir))

  if (!quiet) {
    message("Saved release ", version, ": ", nrow(data), " rows, years ",
            min(data$Year), "-", max(data$Year))
    message("File: ", out_file)
  }
  invisible(data)
}


#' Load FAO Global Capture Production Data
#'
#' This function loads a release downloaded with `download_fao_data()`, or the
#' data bundled with the package.
#'
#' @param version The release to load, e.g. "2026.1.0". Default is "latest", the
#'   newest of the downloaded releases and the bundled data. Use "bundled" for the
#'   data bundled with the package.
#' @param data_dir The directory where downloaded releases are stored. Default is `fao_data_dir()`.
#' @return A data frame with the columns of `Global_Production_long`.
#' @family FAO data
#' @export
#' @examples
#' \dontrun{
#' data <- load_fao_data()
#' FAO34 <- subset(data, FAO.major.fishing.area..Name. == "Atlantic, Eastern Central")
#' }
load_fao_data <- function(version = "latest", data_dir = fao_data_dir()) {
  version <- fao_resolve_version(version, data_dir)
  bundled_release <- fao_bundled_release()

  if (identical(version, "bundled")) {
    bundled <- new.env()
    load(system.file("extdata", "Global_Production_long.rda", package = "faofishingreport"),
         envir = bundled)
    data <- bundled$Global_Production_long
    message("Using FAO Global Capture Production release ", bundled_release,
            " (bundled with the package, ", min(data$Year), "-", max(data$Year), ")")
    return(data)
  }

  data <- readRDS(fao_data_file(version, data_dir))
  message("Using FAO Global Capture Production release ", version,
          " (", min(data$Year), "-", max(data$Year), ")")
  data
}


#' Information on the FAO Data Used
#'
#' Shows which FAO Global Capture Production release the analyses use (the release that
#' `load_fao_data()` loads), when FAO published it, the years it covers, where it was
#' downloaded from, its licence and a citation, e.g. for the data section of a report.
#'
#' @inheritParams load_fao_data
#' @return A list of class `fao_data_info` with `release`, `release_date` (publication date
#'   on the FAO website), `years`, `source` ("bundled" with the package or "downloaded"), `url`,
#'   `download_date`, `licence`, `terms` and `citation`. Printing it shows a summary.
#' @family FAO data
#' @export
#' @examples
#' \dontrun{
#' fao_data_info()
#' fao_data_info()$citation
#' }
fao_data_info <- function(version = "latest", data_dir = fao_data_dir()) {
  version <- fao_resolve_version(version, data_dir)
  if (identical(version, "bundled")) {
    meta <- fao_bundled_meta()
    info <- list(release = meta[["Version"]], release_date = meta[["Released"]],
                 years = as.integer(strsplit(meta[["Years"]], "-")[[1]]), source = "bundled",
                 url = meta[["URL"]], download_date = meta[["Downloaded"]])
  } else {
    data <- readRDS(fao_data_file(version, data_dir))
    attribute <- function(name, default = NA_character_) {
      value <- attr(data, name)
      if (is.null(value)) default else as.character(value)
    }
    info <- list(release = version, release_date = attribute("release_date"),
                 years = range(data$Year), source = "downloaded",
                 url = attribute("source_url", paste0(fao_base_url, "Capture_", version, ".zip")),
                 download_date = attribute("download_date"))
  }
  info$licence <- "CC BY 4.0"
  info$terms <- "https://www.fao.org/contact-us/terms/db-terms-of-use/en"
  info$citation <- fao_data_citation(info)
  structure(info, class = "fao_data_info")
}

#' @export
print.fao_data_info <- function(x, ...) {
  released <- if (is.na(x$release_date)) "" else paste0(" (published by FAO on ", x$release_date, ")")
  obtained <- if (x$source == "bundled") "bundled with faofishingreport" else "with download_fao_data()"
  if (!is.na(x$download_date)) obtained <- paste0(obtained, ", downloaded from FAO on ", x$download_date)
  cat("FAO Global Capture Production\n",
      "  Release:   ", x$release, released, "\n",
      "  Years:     ", x$years[1], "-", x$years[2], "\n",
      "  File:      ", x$url, "\n",
      "  Obtained:  ", obtained, "\n",
      "  Licence:   ", x$licence, " (", x$terms, ")\n",
      "  Citation:  ", x$citation, "\n", sep = "")
  invisible(x)
}

# Citation of a release, following the FAO "Cite this content as" format of the Global
# Capture Production page, with the years, the release and the date the data was obtained
fao_data_citation <- function(info) {
  year <- if (!is.na(info$release_date)) substr(info$release_date, 1, 4) else sub("[.].*", "", info$release)
  cited <- ""
  if (!is.na(info$download_date)) {
    date <- as.Date(info$download_date)
    # English month names whatever the locale of R
    cited <- paste0(" [Cited ", as.integer(format(date, "%d")), " ",
                    month.name[as.integer(format(date, "%m"))], " ", format(date, "%Y"), "].")
  }
  paste0("FAO. ", year, ". Global Capture Production ", info$years[1], "-", info$years[2],
         ", release ", info$release, ". In: Fisheries and Aquaculture [online]. Rome.", cited,
         " https://www.fao.org/fishery/collection/capture/en. Licence: CC BY 4.0.")
}


#' Directory for Downloaded FAO Data
#'
#' Returns the directory where `download_fao_data()` saves releases and where all functions
#' look for them: the folder set with `fao_settings(data_dir = ...)`, or otherwise the user data
#' folder of R (`tools::R_user_dir("faofishingreport", "data")`).
#'
#' @return A character string with the directory path.
#' @family FAO data
#' @export
#' @examples
#' \dontrun{
#' fao_data_dir()
#' fao_settings(data_dir = "D:/shared/fao_data")   # e.g. a folder shared by a team
#' }
fao_data_dir <- function() {
  dir <- getOption("faofishingreport.data_dir")
  if (is.null(dir)) tools::R_user_dir("faofishingreport", which = "data") else path.expand(dir)
}


fao_base_url <- "https://www.fao.org/fishery/static/Data/"

fao_data_file <- function(version, data_dir) {
  file.path(data_dir, paste0("Global_Production_long_", version, ".rds"))
}

fao_names_file <- function(version, data_dir) {
  file.path(data_dir, paste0("fao_names_", version, ".rds"))
}

fao_groups_file <- function(version, data_dir) {
  file.path(data_dir, paste0("fao_groups_", version, ".rds"))
}

# The FAO release of the data bundled with the package, its publication date, years, source and
# download date (written by data-raw/update_bundled_data.R)
fao_bundled_meta <- function() {
  read.dcf(system.file("extdata", "bundled_release.txt", package = "faofishingreport"))[1, ]
}

fao_bundled_release <- function() {
  fao_bundled_meta()[["Version"]]
}

# The release used for version = "latest" (the newest of the downloaded releases and the
# bundled data), "bundled" or a release number; "bundled" when the bundled release is asked for
fao_resolve_version <- function(version, data_dir) {
  local_versions <- fao_local_versions(data_dir)
  bundled_release <- fao_bundled_release()
  if (identical(version, "latest")) {
    newest_local <- local_versions[length(local_versions)]
    use_local <- length(local_versions) > 0 &&
      numeric_version(newest_local) > numeric_version(bundled_release)
    version <- if (use_local) newest_local else "bundled"
  }
  if (identical(version, bundled_release) && !version %in% local_versions) {
    version <- "bundled"
  }
  if (!identical(version, "bundled") && !version %in% local_versions) {
    stop("Release ", version, " has not been downloaded. Downloaded releases: ",
         if (length(local_versions) > 0) paste(local_versions, collapse = ", ") else "none",
         call. = FALSE)
  }
  version
}

fao_local_versions <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^Global_Production_long_[0-9.]+\\.rds$")
  versions <- sub("^Global_Production_long_([0-9.]+)\\.rds$", "\\1", files)
  versions[order(numeric_version(versions))]
}

# Lists the Capture_<version>.zip files on the FAO data index, oldest first.
fao_list_releases <- function() {
  index <- tryCatch(
    readLines(fao_base_url, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      stop("Could not read the FAO data index (", fao_base_url, "): ",
           conditionMessage(e), call. = FALSE)
    }
  )

  pattern <- paste0('href="(Capture_([0-9]+\\.[0-9]+\\.[0-9]+)\\.zip)".*?',
                    '([0-9]{4}-[0-9]{2}-[0-9]{2}) [0-9:]+.*?<td align="right">([^<]*)</td>')
  rows <- regmatches(index, regexec(pattern, index, perl = TRUE))
  rows <- do.call(rbind, rows[lengths(rows) == 5])
  if (is.null(rows)) {
    stop("No Capture releases were found on the FAO data index (", fao_base_url, ").",
         call. = FALSE)
  }

  releases <- data.frame(version = rows[, 3], date = rows[, 4], size = trimws(rows[, 5]),
                         url = paste0(fao_base_url, rows[, 2]), stringsAsFactors = FALSE)
  releases <- releases[!duplicated(releases$version), ]
  releases <- releases[order(numeric_version(releases$version)), ]
  rownames(releases) <- NULL
  releases
}

# Reads the quantity file and code lists of a Capture release zip.
fao_read_capture <- function(zip_file, exdir) {
  files <- c(quantity = "Capture_Quantity.csv", countries = "CL_FI_COUNTRY_GROUPS.csv",
             species = "CL_FI_SPECIES_GROUPS.csv", areas = "CL_FI_WATERAREA_GROUPS.csv")
  utils::unzip(zip_file, files = files, exdir = exdir)
  lapply(files, function(file) {
    utils::read.csv(file.path(exdir, file), colClasses = "character", encoding = "UTF-8",
                    check.names = FALSE, na.strings = "")
  })
}

# Name tables bundled with the package, used so names match the bundled data
fao_lookup_tables <- function() {
  lookup <- new.env()
  load(system.file("extdata", "countries_name_data.rda", package = "faofishingreport"), envir = lookup)
  load(system.file("extdata", "ASFIS_sp.rda", package = "faofishingreport"), envir = lookup)
  list(name_mapping = lookup$name_mapping, asfis = lookup$ASFIS_sp)
}

# Country names as used in the bundled data, from FAO UN codes
fao_country_names <- function(un_code, countries, lookup) {
  name <- lookup$name_mapping$Short.name[match(as.integer(un_code), lookup$name_mapping$UN)]
  missing <- is.na(name)
  name[missing] <- countries$Name_En[match(un_code[missing], countries$UN_Code)]
  name
}

# Species names as used in the bundled data, from FAO 3-alpha codes
fao_species_names <- function(alpha3, species, lookup) {
  name <- lookup$asfis$English_name[match(alpha3, lookup$asfis$Alpha3_Code)]
  missing <- is.na(name) | name == ""
  name[missing] <- sub(" NEI$", " nei", species$Name_En[match(alpha3[missing], species$`3A_Code`)])
  # Species without an English name are shown as [Scientific name], as in the bundled data
  missing <- is.na(name)
  name[missing] <- paste0("[", species$Scientific_Name[match(alpha3[missing], species$`3A_Code`)], "]")
  name
}

# Converts a Capture release to the Global_Production_long format.
fao_convert_capture <- function(capture, lookup = fao_lookup_tables()) {
  # The bundled data only contains tonnes live weight
  quantity <- capture$quantity[capture$quantity$MEASURE == "Q_tlw", ]

  country <- fao_country_names(quantity$COUNTRY.UN_CODE, capture$countries, lookup)
  species_name <- fao_species_names(quantity$SPECIES.ALPHA_3_CODE, capture$species, lookup)
  area <- capture$areas$Name_En[match(quantity$AREA.CODE, capture$areas$Code)]

  status_names <- c(A = "actual", E = "estimate", I = "estimate", N = "neligible",
                    P = "provisional", Q = "suppressed", X = "international organization")
  production_type <- unname(status_names[quantity$STATUS])
  production_type[is.na(production_type)] <- quantity$STATUS[is.na(production_type)]

  data.frame(
    Country..Name. = country,
    ASFIS.species..Name. = species_name,
    FAO.major.fishing.area..Name. = area,
    Unit..Name. = "Tonnes - live weight",
    Unit = "TLW, Number",
    Year = as.integer(quantity$PERIOD),
    Production = as.numeric(quantity$VALUE),
    Production.Type = production_type,
    stringsAsFactors = FALSE
  )
}
