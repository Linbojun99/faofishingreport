#' Trade of Aquatic Products: FAO Data
#'
#' This function returns the FAO statistics of the international trade of aquatic products
#' ("Global aquatic trade - by species/commodity", FI_Trade): the quantity and value of the exports,
#' imports and re-exports of each country by commodity and year, from 1976. Each commodity of the
#' International Standard Statistical Classification of Fishery Commodities (ISSCFC) belongs to one
#' ISSCAAP species group, so the trade can be compared with the catch of the same groups (see
#' `catch_trade()`).
#'
#' @section What the data are:
#' FAO compiles the data from the customs statistics of the United Nations Statistics Division and
#' Eurostat, mapped to the ISSCFC; countries that do not report are filled with the reports of their
#' trade partners (FAO metadata of the release). In release 2026.1.0:
#' \itemize{
#'   \item 225 reporting countries, 1976-2024, about 1,200 commodities in use.
#'   \item The quantity is the net **product weight** in tonnes (the weight of fillets, frozen
#'     fish, fishmeal, ...), not the live weight of the catch; FAO publishes no conversion factors
#'     with the data.
#'   \item The value is in thousand US dollars, exports generally free on board (FOB) and imports
#'     with cost, insurance and freight (CIF).
#'   \item Catch of a country's vessels landed in foreign ports counts as an export of that country.
#'   \item Commodities do not tell whether the product was caught or farmed.
#'   \item Few countries report re-exports separately (26 in 2024, not China), so most re-exports
#'     are counted as exports.
#'   \item The status shows "partner data" for values of non-reporting countries taken from the
#'     reports of their partners (from 2019; such values are shown as "estimate" before 2019).
#' }
#' Trade between countries (by partner) is published only from 2019, in a separate file
#' (FI_Trade_Partners), not read by this function.
#'
#' When the computer is online, the newest release is used, downloaded and converted the first time
#' (a 16 MB file; converting takes a minute) and saved in `data_dir`; offline, or with
#' `fao_settings(online = FALSE)`, the newest saved release is used.
#'
#' @param version The release, e.g. "2026.1.0". Default is "latest".
#' @param data_dir The directory where the data is saved. Default is `fao_data_dir()`.
#' @return A data frame with one row per country, commodity, flow and year: `Country..Name.` (as in
#'   `load_fao_data()`), `Commodity..Code.` (ISSCFC), `Commodity..Name.`, `ISSCAAP.group..Code.`,
#'   `ISSCAAP.group..Name.` (as in the species groups of the catch data), `Trade.flow` ("Exports",
#'   "Imports" or "Re-exports"), `Year`, `Quantity` (tonnes, product weight), `Value` (thousand USD),
#'   `Quantity.Type` and `Value.Type` ("actual", "estimate", "partner data", ...). The attributes
#'   `fao_version` and `source_url` give the release.
#' @references
#' FAO. Global aquatic trade statistics (FI_Trade), metadata of the release, and the ISSCFC and
#' ISSCAAP classifications. \url{https://www.fao.org/fishery/static/Data/}
#'
#' Coordinating Working Party on Fishery Statistics (CWP). Handbook of Fishery Statistics:
#' fishery commodities classification (ISSCFC).
#' \url{https://www.fao.org/cwp-on-fishery-statistics/handbook/socio-economic-dimension/fishery-commodities-classification/en/}
#' @export
#' @examples
#' \dontrun{
#' trade <- load_fao_trade()
#' norway <- subset(trade, Country..Name. == "Norway" & Trade.flow == "Exports" & Year == 2024)
#' head(norway[order(-norway$Value), c("Commodity..Name.", "Quantity", "Value")])
#' catch_trade("Norway", trade = trade)
#' }
load_fao_trade <- function(version = "latest", data_dir = fao_data_dir()) {
  local <- fao_trade_versions(data_dir)
  online <- if (isTRUE(fao_setting("online"))) {
    tryCatch(fao_list_releases("FI_Trade"), error = function(e) {
      message("Could not read the FAO data index (", conditionMessage(e), ").")
      NULL
    })
  }
  if (identical(version, "latest")) {
    version <- if (!is.null(online)) online$version[nrow(online)] else if (length(local) > 0) local[length(local)] else
      stop("The FAO data index cannot be reached and no trade data has been downloaded.", call. = FALSE)
  }
  file <- fao_trade_file(version, data_dir)
  if (!file.exists(file)) {
    if (is.null(online)) {
      stop("Trade data release ", version, " has not been downloaded and the FAO data index cannot be reached.",
           call. = FALSE)
    }
    release <- online[online$version == version, ]
    if (nrow(release) == 0) {
      stop("Trade data release ", version, " is not available. Releases: ", paste(online$version, collapse = ", "),
           call. = FALSE)
    }
    zip_file <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_file), add = TRUE)
    old <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    message("Downloading ", release$url, " (", release$size, ") ...")
    utils::download.file(release$url, zip_file, mode = "wb", quiet = TRUE)
    message("Converting the trade data ...")
    data <- fao_convert_trade(zip_file)
    attr(data, "fao_version") <- version
    attr(data, "source_url") <- release$url
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(data, file)
  }
  data <- readRDS(file)
  message("Using FAO trade data (FI_Trade) release ", version, ": ", min(data$Year), "-", max(data$Year),
          ", quantities in tonnes product weight, values in thousand USD.")
  data
}


fao_trade_file <- function(version, data_dir) {
  file.path(data_dir, paste0("FAO_Trade_", version, ".rds"))
}

fao_trade_versions <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^FAO_Trade_[0-9.]+\\.rds$")
  versions <- sub("^FAO_Trade_([0-9.]+)\\.rds$", "\\1", files)
  versions[order(numeric_version(versions))]
}

# Converts an FI_Trade zip to one row per country, commodity, flow and year, with quantity and value
fao_convert_trade <- function(zip_file, lookup = fao_lookup_tables()) {
  read <- function(file) {
    utils::read.csv(unz(zip_file, file), colClasses = "character", encoding = "UTF-8", check.names = FALSE,
                    na.strings = "")
  }
  countries <- read("CL_FI_COUNTRY_GROUPS.csv")
  commodities <- read("CL_FI_COMMODITY_ISSCFC.csv")
  quantity <- read("TRADE_QUANTITY.csv")
  value <- read("TRADE_VALUE.csv")
  key <- function(x) paste(x$TRADE_FLOW.ALPHA_CODE, x$COUNTRY_REPORTER.UN_CODE, x$COMMODITY.FAO_CODE, x$PERIOD)
  quantity_key <- key(quantity)
  value_key <- key(value)
  only_value <- !value_key %in% quantity_key
  rows <- rbind(quantity[, c("TRADE_FLOW.ALPHA_CODE", "COUNTRY_REPORTER.UN_CODE", "COMMODITY.FAO_CODE", "PERIOD")],
                value[only_value, c("TRADE_FLOW.ALPHA_CODE", "COUNTRY_REPORTER.UN_CODE", "COMMODITY.FAO_CODE", "PERIOD")])
  row_key <- c(quantity_key, value_key[only_value])
  q <- match(row_key, quantity_key)
  v <- match(row_key, value_key)
  commodity <- match(rows$COMMODITY.FAO_CODE, commodities$Code)
  isscaap <- as.integer(commodities$ISSCAAP[commodity])
  groups <- fao_isscaap_groups()
  flows <- c(E = "Exports", I = "Imports", R = "Re-exports")
  data.frame(
    Country..Name. = fao_country_names(rows$COUNTRY_REPORTER.UN_CODE, countries, lookup),
    Commodity..Code. = rows$COMMODITY.FAO_CODE,
    Commodity..Name. = commodities$Name_En[commodity],
    ISSCAAP.group..Code. = isscaap,
    ISSCAAP.group..Name. = groups$isscaap_en[match(isscaap, groups$isscaap_code)],
    Trade.flow = unname(flows[rows$TRADE_FLOW.ALPHA_CODE]),
    Year = as.integer(rows$PERIOD),
    Quantity = as.numeric(quantity$VALUE[q]),
    Value = as.numeric(value$VALUE[v]),
    Quantity.Type = fao_trade_status(quantity$STATUS[q]),
    Value.Type = fao_trade_status(value$STATUS[v]),
    stringsAsFactors = FALSE
  )
}

# Status of trade and employment values: as the production types, with series breaks and missing
# values named; trade values flagged X are taken from the reports of trade partners (mirror data,
# FAO metadata of FI_Trade), employment values flagged X come from an international organization
fao_trade_status <- function(status, trade = TRUE) {
  out <- fao_production_type(status)
  if (trade) out[status %in% "X"] <- "partner data"
  out[status %in% "B"] <- "series break"
  out[status %in% c("L", "M", "O")] <- "missing"
  out
}

# The ISSCAAP groups (built by data-raw/build_isscaap_production.R from the FAO list)
fao_isscaap_groups <- function() {
  groups <- utils::read.csv(system.file("extdata", "isscaap_groups.csv", package = "faofishingreport"),
                            stringsAsFactors = FALSE)
  zh <- fao_extdata_csv("group_names_zh.csv")
  groups$isscaap_zh <- zh$name_zh[match(paste("isscaap", groups$isscaap_en), paste(zh$type, zh$name_en))]
  groups
}


#' Species Groups Mostly Caught, Not Farmed
#'
#' This function gives, for each ISSCAAP species group of aquatic animals, the share of capture in
#' the world production (capture and aquaculture): the lowest share over `years` and its year, and
#' the shares in the first and last year, and whether the group counts as
#' mostly caught: capture made up at least `min_capture_pct` percent of its world production in
#' every year of `years`. `catch_trade()` compares trade with catch only for these groups, because
#' trade statistics do not tell whether a product was caught or farmed.
#'
#' The shares come from the FAO global production data (capture and aquaculture, tonnes live
#' weight), release 2026.1.0, built into the package from 1976 (the first year of the trade data).
#' The groups FAO leaves out of its summary tables of aquatic animals (mammals, crocodiles, pearls,
#' corals, sponges and aquatic plants) are not counted.
#'
#' With the default 90 percent and the years 1976 to 2024, the groups counted are the cods,
#' herrings and sardines, tunas, miscellaneous pelagic and demersal fishes, sharks and rays, shads,
#' lobsters, king crabs, krill, miscellaneous marine crustaceans, squids and octopuses, and
#' horseshoe crabs. Groups once mostly caught but now partly or largely farmed are not counted:
#' shrimps (capture below 90 percent from 1986), crabs (from 2000), flounders (from 2004),
#' miscellaneous coastal fishes (from 2005) and salmons (81 percent caught in 1976, 12 percent in
#' 2024). "Marine fishes not identified" is not counted because capture made up less than 90
#' percent of its production in each year from 2022 to 2024 (89.65 to 89.97 percent).
#'
#' @param min_capture_pct The lowest share of capture (percent of world production) in every year
#'        for a group to count. Default is 90.
#' @param years The years. Default is NULL, all years of the table (1976 to 2024).
#' @return A data frame with one row per ISSCAAP group of aquatic animals with production:
#'   `isscaap_code`, `isscaap_en`, `isscaap_zh`, `capture_pct_min` and `year_min` (the lowest share
#'   and its year), `capture_pct_first` and `capture_pct_last` (the shares in the first and last
#'   year), and `counted` (TRUE for the groups mostly caught), sorted by code.
#' @references
#' FAO. Global production statistics (capture and aquaculture), release 2026.1.0.
#' \url{https://www.fao.org/fishery/static/Data/}
#' @export
#' @examples
#' fao_capture_groups()
#' fao_capture_groups(years = 1976:1990)      # shrimps were then mostly caught
#' fao_capture_groups(min_capture_pct = 80)
fao_capture_groups <- function(min_capture_pct = 90, years = NULL) {
  production <- utils::read.csv(system.file("extdata", "isscaap_production.csv", package = "faofishingreport"))
  covered <- range(production$year)
  if (!is.null(years)) {
    production <- production[production$year %in% years, ]
    if (nrow(production) == 0) {
      stop("No production data for these years; the table covers ", covered[1], "-", covered[2], ".", call. = FALSE)
    }
  }
  groups <- fao_isscaap_groups()
  groups <- groups[groups$aquatic_animal, ]
  production <- production[production$isscaap_code %in% groups$isscaap_code, ]
  total <- production$capture_tonnes + production$aquaculture_tonnes
  production$capture_pct <- ifelse(total > 0, 100 * production$capture_tonnes / total, NA_real_)
  out <- do.call(rbind, lapply(split(production, production$isscaap_code), function(p) {
    p <- p[order(p$year), ]
    shares <- p$capture_pct[!is.na(p$capture_pct)]
    lowest <- if (length(shares) > 0) which.min(p$capture_pct) else NA
    data.frame(isscaap_code = p$isscaap_code[1],
               capture_pct_min = if (length(shares) > 0) min(shares) else NA_real_,
               year_min = if (length(shares) > 0) p$year[lowest] else NA_integer_,
               capture_pct_first = p$capture_pct[1], capture_pct_last = p$capture_pct[nrow(p)])
  }))
  out <- cbind(groups[match(out$isscaap_code, groups$isscaap_code), c("isscaap_code", "isscaap_en", "isscaap_zh")],
               out[, -1])
  out$counted <- !is.na(out$capture_pct_min) & out$capture_pct_min >= min_capture_pct
  rownames(out) <- NULL
  out
}
