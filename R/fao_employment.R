#' Employment in Fisheries and Aquaculture: FAO Data
#'
#' This function returns the FAO statistics of the number of people employed in the primary sector
#' of fisheries and aquaculture ("Global employment in the primary sector of fisheries and
#' aquaculture", FI_Employment), by country, work area, working time, sex and year, from 1995,
#' with FAO's notes on individual countries.
#'
#' @section What the data are:
#' FAO collects the data from countries with an annual questionnaire (jointly with the OECD for 53
#' countries) and estimates the figures of countries that do not report (FAO metadata of release
#' 2026.1.0; FAO first disseminated this dataset in 2026):
#' \itemize{
#'   \item 223 countries, 1995-2024; 65.4 million people in 2024 (SOFIA 2026 gives 65.3 million).
#'   \item Work areas: aquaculture, marine coastal fishing, marine deep-sea fishing, marine fishing
#'     not specified (NEI), inland waters fishing, and "Unspecified" (9 percent of the people in
#'     2024). Processing and subsistence are not published, their coverage being too small.
#'   \item Working time, as defined by the Coordinating Working Party on Fishery Statistics (CWP):
#'     full time (at least 90 percent of working time or livelihood), part time (30 to 90 percent),
#'     occasional (less than 30 percent), or unspecified.
#'   \item The numbers are people, not full-time equivalents: an occasional worker counts as one.
#'   \item Working time is unspecified for 38 percent and sex for 41 percent of the data (FAO
#'     metadata); FAO's imputations make up about 30 percent of the data and half of the people in
#'     2024. `Number.Type` "estimate" marks them and the estimates of countries.
#'   \item National definitions differ, and the level of detail can change over the years; read
#'     the notes of each country (the attribute `country_notes`, also shown by
#'     `employment_summary()` and `catch_per_fisher()`). For example, FAO notes that China reports
#'     its part-time and occasional workers of fishing and aquaculture together as "Unspecified",
#'     and that its "Marine fishing, NEI" includes inland fishing for 1995-2006 and 2014-2020.
#'   \item FAO revises the whole series between releases, so releases should not be compared.
#' }
#'
#' When the computer is online, the newest release is used, downloaded and converted the first time
#' (a file under 1 MB) and saved in `data_dir`; offline, or with `fao_settings(online = FALSE)`, the
#' newest saved release is used.
#'
#' @param version The release, e.g. "2026.1.0". Default is "latest".
#' @param data_dir The directory where the data is saved. Default is `fao_data_dir()`.
#' @return A data frame with one row per country, work area, working time, sex and year:
#'   `Country..Name.` (as in `load_fao_data()`), `Work.area..Code.` (A, C, D, I, M or U),
#'   `Work.area`, `Working.time`, `Sex`, `Year`, `Number` (people) and `Number.Type` ("actual",
#'   "estimate", ...). The attribute `country_notes` is a data frame of FAO's notes on individual
#'   countries (`country_en`, `note`), and `fao_version` and `source_url` give the release.
#' @references
#' FAO. Global employment in the primary sector of fisheries and aquaculture (FI_Employment),
#' metadata and country notes of the release. \url{https://www.fao.org/fishery/static/Data/}
#'
#' Coordinating Working Party on Fishery Statistics (CWP). Handbook of Fishery Statistics: fishers.
#' \url{https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/}
#'
#' FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome: FAO. \doi{10.4060/cd8357en}
#' @export
#' @examples
#' \dontrun{
#' employment <- load_fao_employment()
#' subset(employment, Country..Name. == "Norway" & Year == 2024)
#' notes <- attr(employment, "country_notes")
#' notes$note[notes$country_en == "China"]
#' }
load_fao_employment <- function(version = "latest", data_dir = fao_data_dir()) {
  local <- fao_employment_versions(data_dir)
  online <- if (isTRUE(fao_setting("online"))) {
    tryCatch(fao_list_releases("FI_Employment"), error = function(e) {
      message("Could not read the FAO data index (", conditionMessage(e), ").")
      NULL
    })
  }
  if (identical(version, "latest")) {
    version <- if (!is.null(online)) online$version[nrow(online)] else if (length(local) > 0) local[length(local)] else
      stop("The FAO data index cannot be reached and no employment data has been downloaded.", call. = FALSE)
  }
  file <- fao_employment_file(version, data_dir)
  if (!file.exists(file)) {
    if (is.null(online)) {
      stop("Employment data release ", version, " has not been downloaded and the FAO data index cannot be reached.",
           call. = FALSE)
    }
    release <- online[online$version == version, ]
    if (nrow(release) == 0) {
      stop("Employment data release ", version, " is not available. Releases: ",
           paste(online$version, collapse = ", "), call. = FALSE)
    }
    zip_file <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_file), add = TRUE)
    old <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    message("Downloading ", release$url, " (", release$size, ") ...")
    utils::download.file(release$url, zip_file, mode = "wb", quiet = TRUE)
    data <- fao_convert_employment(zip_file)
    attr(data, "fao_version") <- version
    attr(data, "source_url") <- release$url
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(data, file)
  }
  data <- readRDS(file)
  message("Using FAO employment data (FI_Employment) release ", version, ": ", min(data$Year), "-", max(data$Year),
          ", number of people (not full-time equivalents).")
  data
}


fao_employment_file <- function(version, data_dir) {
  file.path(data_dir, paste0("FAO_Employment_", version, ".rds"))
}

fao_employment_versions <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^FAO_Employment_[0-9.]+\\.rds$")
  versions <- sub("^FAO_Employment_([0-9.]+)\\.rds$", "\\1", files)
  versions[order(numeric_version(versions))]
}

# Converts an FI_Employment zip to one row per country, work area, working time, sex and year
fao_convert_employment <- function(zip_file, lookup = fao_lookup_tables()) {
  read <- function(file) {
    utils::read.csv(unz(zip_file, file), colClasses = "character", encoding = "UTF-8", check.names = FALSE,
                    na.strings = "")
  }
  countries <- read("CL_FI_COUNTRY_GROUPS.csv")
  areas <- read("CL_FI_WORK_AREA.csv")
  occupation <- read("CL_FI_OCCUPATION.csv")
  gender <- read("CL_FI_GENDER.csv")
  x <- read("EMPLOY_NUMBER.csv")
  data <- data.frame(
    Country..Name. = fao_country_names(x$COUNTRY.UN_CODE, countries, lookup),
    Work.area..Code. = x$WORKAREA.CODE,
    Work.area = areas$Name_En[match(x$WORKAREA.CODE, areas$Code)],
    Working.time = occupation$Name_En[match(x$OCCUPATION.CODE, occupation$Code)],
    Sex = gender$Name_En[match(x$GENDER.CODE, gender$Code)],
    Year = as.integer(x$PERIOD),
    Number = as.numeric(x$VALUE),
    Number.Type = fao_trade_status(x$STATUS, trade = FALSE),
    stringsAsFactors = FALSE
  )
  notes_file <- grep("^NOTES_EMPLOY_COUNTRY_En\\.html$", utils::unzip(zip_file, list = TRUE)$Name, value = TRUE)
  attr(data, "country_notes") <- if (length(notes_file) == 1) {
    con <- unz(zip_file, notes_file, encoding = "UTF-8")
    on.exit(close(con))
    fao_employment_notes(paste(readLines(con, warn = FALSE), collapse = "\n"), countries, lookup)
  }
  data
}

# FAO's notes on individual countries, from the HTML file of the release
fao_employment_notes <- function(html, countries, lookup) {
  blocks <- regmatches(html, gregexpr("(?s)<div id=\"[^\"]+\">.*?</div>", html, perl = TRUE))[[1]]
  if (length(blocks) == 0) return(data.frame(country_en = character(), note = character()))
  name <- sub("(?s)^<div id=\"([^\"]+)\">.*$", "\\1", blocks, perl = TRUE)
  text <- sub("(?s)^.*?</h4>", "", blocks, perl = TRUE)
  text <- gsub("<p></p>", "\n", text, fixed = TRUE)
  text <- gsub("<[^>]+>", "", text)
  entities <- c("&amp;" = "&", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#39;" = "'", "&nbsp;" = " ")
  for (e in names(entities)) text <- gsub(e, entities[[e]], text, fixed = TRUE)
  text <- gsub("[ \t]*\n[ \t]*", "\n", gsub("[ \t\r]+", " ", text))
  text <- gsub("([^\n])\n(?=[a-z])", "\\1 ", trimws(text), perl = TRUE)     # lines broken inside a sentence
  un <- countries$UN_Code[match(toupper(name), toupper(countries$Name_En))]
  country <- ifelse(is.na(un), name, fao_country_names(un, countries, lookup))
  data.frame(country_en = country, note = text, stringsAsFactors = FALSE)
}

# FAO's note on a country, if any
fao_employment_note <- function(employment, country) {
  notes <- attr(employment, "country_notes")
  if (is.null(notes)) return(character())
  notes$note[notes$country_en == country]
}
