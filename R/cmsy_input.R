# FAO catch data -> the two CMSY++ input files (catch file and ID file), one stock per FAO species
# item and fishing area (or country), with the priors and their sources.

#' Prepare FAO Catch Data for CMSY++
#'
#' This function turns FAO catch data into the two input files of CMSY++ (Froese et al. 2021,
#' 2023): the catch file (`Stock`, `yr`, `ct`, `bt`) and the ID file, one row per stock, with
#' the priors and settings. Each stock gets its resilience and r prior from the built-in
#' FishBase and SeaLifeBase table (see [cmsy_priors()]), with the source of each value.
#'
#' The FAO statistics contain catches only, with no abundance index (CPUE or survey biomass), so
#' every stock is assessed with the catch-only part of CMSY++ (CMSY: `btype = "None"`, `bt` empty)
#' and the Bayesian Schaefer model (BSM) is not run. The results then depend mostly on the
#' priors for relative biomass (B/k), which CMSY++ sets with its neural network unless they are
#' given in `priors`; they are indicative and do not replace stock assessments.
#'
#' A stock is the yearly catch of one FAO species item in one fishing area, summed over all
#' countries (`by = "area"`, as in [stock_status()]), or of one country (`by = "country"`). Only
#' items identified to species and measured in tonnes are used. Following the user guide, years
#' must be consecutive and gaps are not allowed: a stock starts with its first and ends with its
#' last year with catch, and missing or zero catches in between are filled by linear
#' interpolation between the adjacent years (the number of filled years is reported). Stocks are
#' left out when they have fewer than `min_years` years (CMSY++ uses the first and the last ten
#' years for its default priors), less than `min_catch` tonnes in total, more than
#' `max_filled` of their years filled, or no prior for r.
#'
#' Every stock keeps the links to the sources of its data and priors: `url` (the FishBase or
#' SeaLifeBase page of the species), `data_url` (the FishBase or SeaLifeBase table the prior was
#' read from), `rule_url` (the CMSY++ user guide) and `fao_data_url` (the FAO data release).
#'
#' @inheritParams summary_params
#' @param species Optional species to keep, by English or Chinese FAO name, both together (as
#'        shown with `lang = "both"`), scientific name or FAO 3-alpha code, e.g.
#'        `c("European pilchard(=Sardine)", "Sardinella aurita", "BOG")` or Chinese names.
#'        Default is NULL, all species.
#' @param by "area" (default): one stock per species item and fishing area, summed over
#'        countries; or "country": one stock per species item, fishing area and country.
#' @param priors Optional data frame of priors and settings that replace the defaults, one row
#'        per stock or species. It identifies the rows by a `Stock`, `alpha3`, `species_en` or
#'        `species` column (`species`: any name, as in the `species` argument) and can have any of the ID file columns `Resilience`, `r.low`, `r.hi`,
#'        `stb.low`, `stb.hi`, `int.yr`, `intb.low`, `intb.hi`, `endb.low`, `endb.hi`,
#'        `StartYear`, `EndYear` and `Comment`. Values given here are recorded as "user".
#' @param default_resilience Resilience ("High", "Medium", "Low" or "Very low") for stocks
#'        whose species has no resilience or prior r in FishBase or SeaLifeBase. Default is NULL:
#'        such stocks are left out, since the user guide gives no default.
#' @param min_years Minimum number of years from the first to the last year with catch. Default is 10.
#' @param min_catch Minimum total catch of a stock, in tonnes. Default is 1000.
#' @param max_filled Maximum share of the years of a stock that may be filled by interpolation.
#'        Default is 0.2.
#' @return Invisibly, a list of class `cmsy_input` with `catch` and `id` (the two CMSY++ input
#'         tables), `stocks` (one row per stock with its species, area, years, filled years, catch
#'         and the source of its r prior) and `excluded` (stocks left out, with the reason).
#'         When `table` is TRUE the four tables are saved in `tables/cmsy/`.
#' @references
#' Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for CMSY++. GEOMAR,
#' Germany, 17 pp. \url{https://oceanrep.geomar.de/id/eprint/52147/}
#'
#' Froese, R., Winker, H., Coro, G., et al. (2023). New developments in the analysis of catch
#' time series as the basis for fish stock assessments: the CMSY++ method. Acta Ichthyologica et
#' Piscatoria 53: 173-189. \doi{10.3897/aiep.53.105910}
#' @export
#' @examples
#' \dontrun{
#' FAO34 <- fishing_area_data("Atlantic, Eastern Central", plot = FALSE)
#' input <- cmsy_input(FAO34$area_data)
#' head(input$stocks)
#'
#' # Some species only, named in English, by scientific name or by FAO code
#' input <- cmsy_input(FAO34$area_data, species = c("European pilchard(=Sardine)", "Sardinella aurita", "BOG"))
#'
#' # Expert priors for one species (user guide Tables 1 and 3)
#' input <- cmsy_input(FAO34$area_data,
#'                     priors = data.frame(alpha3 = "PIL", endb.low = 0.2, endb.hi = 0.6))
#' }
cmsy_input <- function(area_data, species = NULL, by = c("area", "country"), priors = NULL, default_resilience = NULL,
                       min_years = 10, min_catch = 1000, max_filled = 0.2, table = TRUE,
                       lang = NULL, label = NULL) {
  by <- match.arg(by)
  lang <- fao_lang(lang)
  if (!is.null(default_resilience) && !default_resilience %in% cmsy_resilience_r$resilience) {
    stop("`default_resilience` must be one of ", paste0("\"", cmsy_resilience_r$resilience, "\"", collapse = ", "),
         call. = FALSE)
  }
  name <- fao_summary_name(fao_summary_label(area_data, label), "cmsy")
  fao_version <- attr(area_data, "fao_version")
  if (is.null(fao_version)) fao_version <- cmsy_fao_version()

  data <- fao_add_groups(area_data)
  data <- data[!is.na(data$Production) & data$taxon_level %in% "species" &
                 grepl("^Tonnes", data$Unit..Name.), ]
  if (nrow(data) == 0) stop("The data have no catch in tonnes of items identified to species.", call. = FALSE)
  if (!is.null(species)) {
    data <- data[data$alpha3 %in% cmsy_match_species(species, among = unique(data$alpha3)), ]
  }
  keys <- c("ASFIS.species..Name.", "FAO.major.fishing.area..Name.", if (by == "country") "Country..Name.")
  series <- stats::aggregate(list(Tonnes = data$Production), by = data[c(keys, "Year")], FUN = sum)
  info <- data[!duplicated(data[keys]), c(keys, "alpha3", "scientific_name", "isscaap_en", "area_code",
                                          "ocean_en", if (by == "country") "iso3")]

  stocks <- lapply(split(series, series[keys], drop = TRUE), function(s) cmsy_series(s, min_years, min_catch, max_filled))
  stocks <- stocks[!vapply(stocks, is.null, logical(1))]
  if (length(stocks) == 0) stop("No catch series in the data.", call. = FALSE)
  meta <- do.call(rbind, lapply(stocks, function(s) s$meta))
  meta <- merge(meta, info, by = keys, sort = FALSE)
  meta$Stock <- cmsy_stock_id(meta$alpha3, meta$area_code, if (by == "country") meta$iso3)

  # Priors: built-in FishBase/SeaLifeBase table, then the user's values
  table_priors <- cmsy_priors_table()
  hit <- match(meta$alpha3, table_priors$alpha3)
  meta$SpecCode <- table_priors$SpecCode[hit]
  meta$database <- table_priors$database[hit]
  meta$url <- table_priors$url[hit]
  meta$data_url <- table_priors$data_url[hit]
  meta$rule_url <- table_priors$rule_url[hit]
  meta$Resilience <- table_priors$resilience[hit]
  from_r <- !is.na(table_priors$prior_r_lcl[hit])
  meta$r.low <- ifelse(from_r, table_priors$r_low[hit], NA_real_)
  meta$r.hi <- ifelse(from_r, table_priors$r_high[hit], NA_real_)
  meta$r_source <- table_priors$r_source[hit]
  meta$resilience_source <- ifelse(is.na(meta$Resilience), NA_character_, paste(meta$database, "resilience"))
  # CMSY++ stops unless Resilience is one of its four categories, even when r.low and r.hi are
  # given (they replace the category's range); such species get the category containing prior r
  no_category <- is.na(meta$Resilience) & !is.na(meta$r.low)
  if (any(no_category)) {
    prior_r <- table_priors$prior_r[hit][no_category]
    meta$Resilience[no_category] <- cmsy_category_of_r(prior_r)
    meta$resilience_source[no_category] <- "category containing FishBase/SeaLifeBase prior r (input check only; r.low and r.hi are used)"
  }
  for (col in c("stb.low", "stb.hi", "int.yr", "intb.low", "intb.hi", "endb.low", "endb.hi")) meta[[col]] <- NA_real_
  meta$StartYear <- meta$first_year
  meta$EndYear <- meta$last_year
  meta$Comment <- paste0("FAO catch only, ", ifelse(by == "country", "one country", "all countries"), ", ",
                         meta$filled_years, " years interpolated")
  meta$prior_source <- "FishBase/SeaLifeBase; B/k priors from the CMSY++ neural network"
  if (!is.null(priors)) meta <- cmsy_apply_priors(meta, priors)
  if (!is.null(default_resilience)) {
    use <- is.na(meta$Resilience) & is.na(meta$r.low)
    meta$Resilience[use] <- default_resilience
    meta$r_source[use] <- paste0("default_resilience = \"", default_resilience, "\" set by the user (CMSY++ user guide Table 2)")
    meta$resilience_source[use] <- "user (default_resilience)"
  }

  meta$status <- "ready"
  meta$reason <- ""
  no_prior <- is.na(meta$Resilience) | !meta$Resilience %in% cmsy_resilience_r$resilience
  meta$reason[no_prior] <- "no resilience or r prior: give it in `priors` or set `default_resilience`"
  meta$reason[meta$n_years < min_years] <- paste0("fewer than ", min_years, " years")
  meta$reason[meta$filled_share > max_filled] <- paste0("more than ", 100 * max_filled, "% of years filled")
  meta$reason[meta$total_tonnes < min_catch] <- paste0("total catch below ", min_catch, " t")
  meta$status[nzchar(meta$reason)] <- "excluded"
  meta <- meta[order(meta$status != "ready", -meta$total_tonnes), ]

  ready <- meta[meta$status == "ready", ]
  catch <- do.call(rbind, lapply(stocks, function(s) s$catch))
  catch <- merge(catch, meta[c(keys, "Stock")], by = keys)
  catch <- catch[catch$Stock %in% ready$Stock, ]
  catch <- data.frame(Stock = catch$Stock, yr = catch$Year, ct = round(catch$Tonnes, 3), bt = NA_real_,
                      stringsAsFactors = FALSE)
  catch <- catch[order(catch$Stock, catch$yr), ]

  source_text <- paste0("FAO Global Capture Production ", fao_version)
  id <- data.frame(
    Continent = cmsy_ascii(ifelse(is.na(ready$ocean_en), "", ready$ocean_en)),
    Region = cmsy_ascii(ready$FAO.major.fishing.area..Name.),
    Subregion = cmsy_ascii(if (by == "country") ready$Country..Name. else "All countries"),
    Stock = ready$Stock, Group = cmsy_ascii(ready$isscaap_en), Name = cmsy_ascii(ready$ASFIS.species..Name.),
    ScientificName = cmsy_ascii(ready$scientific_name), SpecCode = ready$SpecCode, Source = source_text,
    MinOfYear = ready$first_year, MaxOfYear = ready$last_year,
    StartYear = ready$StartYear, EndYear = ready$EndYear,
    Flim = NA, Fpa = NA, Blim = NA, Bpa = NA, Bmsy = NA, MSYBtrigger = NA, Fmsy = NA, last_F = NA,
    Resilience = ready$Resilience, r.low = ready$r.low, r.hi = ready$r.hi,
    stb.low = ready$stb.low, stb.hi = ready$stb.hi, int.yr = ready$int.yr,
    intb.low = ready$intb.low, intb.hi = ready$intb.hi, endb.low = ready$endb.low, endb.hi = ready$endb.hi,
    btype = "None", e.creep = NA, force.cmsy = FALSE, Comment = cmsy_ascii(ready$Comment),
    stringsAsFactors = FALSE
  )
  if (nrow(ready) == 0) warning("No stock is ready for CMSY++; see `excluded`.", call. = FALSE)

  stock_table <- data.frame(
    Stock = meta$Stock,
    species_name = fao_translate(meta$ASFIS.species..Name., "species", lang),
    species_en = meta$ASFIS.species..Name.,
    species_zh = cmsy_species_lookup()$species_zh[match(meta$alpha3, cmsy_species_lookup()$alpha3)],
    alpha3 = meta$alpha3, scientific_name = meta$scientific_name,
    area = meta$FAO.major.fishing.area..Name.,
    country = if (by == "country") meta$Country..Name. else NA_character_,
    first_year = meta$first_year, last_year = meta$last_year, n_years = meta$n_years,
    filled_years = meta$filled_years, total_tonnes = meta$total_tonnes, max_tonnes = meta$max_tonnes,
    Resilience = meta$Resilience, r.low = meta$r.low, r.hi = meta$r.hi, r_source = meta$r_source,
    resilience_source = meta$resilience_source, prior_source = meta$prior_source,
    url = meta$url, data_url = meta$data_url, rule_url = meta$rule_url,
    fao_data_url = paste0(fao_base_url, "Capture_", fao_version, ".zip"),
    status = meta$status, reason = meta$reason, stringsAsFactors = FALSE
  )
  rownames(stock_table) <- NULL
  result <- structure(list(
    catch = catch, id = id,
    stocks = stock_table[stock_table$status == "ready", ],
    excluded = stock_table[stock_table$status != "ready", ],
    by = by, name = name, fao_version = fao_version, created = Sys.time()
  ), class = "cmsy_input")

  if (table) {
    fao_ensure_dirs(file.path("tables", "cmsy"))
    utils::write.csv(catch, file.path("tables", "cmsy", paste0(name, "_catch.csv")), row.names = FALSE, na = "NA")
    utils::write.csv(id, file.path("tables", "cmsy", paste0(name, "_id.csv")), row.names = FALSE, na = "NA")
    fao_write_csv(result$stocks, file.path("tables", "cmsy", paste0(name, "_stocks.csv")))
    fao_write_csv(result$excluded, file.path("tables", "cmsy", paste0(name, "_excluded.csv")))
  }
  message(nrow(result$stocks), " stocks ready for CMSY++ (catch only, no abundance data); ",
          nrow(result$excluded), " left out (see $excluded).")
  invisible(result)
}

#' @export
print.cmsy_input <- function(x, ...) {
  cat("CMSY++ input from FAO capture ", x$fao_version, ": ", nrow(x$stocks), " stocks ready, ",
      nrow(x$excluded), " left out\n", sep = "")
  if (nrow(x$stocks) > 0) {
    print(utils::head(x$stocks[, c("Stock", "species_en", "area", "first_year", "last_year",
                                   "Resilience", "r.low", "r.hi")], 10), row.names = FALSE)
  }
  invisible(x)
}

# One catch series: consecutive years from the first to the last year with catch, with missing
# or zero years in between filled by linear interpolation (user guide: no gaps, no zeros)
cmsy_series <- function(s, min_years, min_catch, max_filled) {
  positive <- s$Year[s$Tonnes > 0]
  if (length(positive) == 0) return(NULL)
  years <- seq(min(positive), max(positive))
  catch <- s$Tonnes[match(years, s$Year)]
  gap <- is.na(catch) | catch <= 0
  if (any(gap)) catch[gap] <- stats::approx(years[!gap], catch[!gap], xout = years[gap])$y
  keys <- s[1, setdiff(names(s), c("Year", "Tonnes")), drop = FALSE]
  meta <- cbind(keys, first_year = min(years), last_year = max(years), n_years = length(years),
                filled_years = sum(gap), filled_share = sum(gap) / length(years),
                total_tonnes = sum(catch), max_tonnes = max(catch))
  catch_rows <- cbind(keys[rep(1, length(years)), , drop = FALSE], Year = years, Tonnes = catch)
  list(meta = meta, catch = catch_rows)
}

# Stock identifiers without spaces or the characters the user guide warns about
cmsy_stock_id <- function(alpha3, area_code, iso3 = NULL) {
  id <- paste0(alpha3, "_", area_code)
  if (!is.null(iso3)) id <- paste0(id, "_", ifelse(is.na(iso3), "XXX", iso3))
  id
}

# Text without non-ASCII characters, "/" or ":" (user guide: they may cause errors)
cmsy_ascii <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  gsub("[/:]", "-", x)
}

# Resilience category whose r range (user guide Table 2) contains r; the closest geometric mean
# when several do
cmsy_category_of_r <- function(r) {
  vapply(r, function(value) {
    inside <- which(value >= cmsy_resilience_r$r_low & value <= cmsy_resilience_r$r_high)
    if (length(inside) == 0) inside <- seq_len(nrow(cmsy_resilience_r))
    centre <- sqrt(cmsy_resilience_r$r_low[inside] * cmsy_resilience_r$r_high[inside])
    cmsy_resilience_r$resilience[inside][which.min(abs(log(centre) - log(value)))]
  }, character(1))
}

# User priors replace the defaults, matched by Stock, then alpha3, then species_en
cmsy_apply_priors <- function(meta, priors) {
  priors <- as.data.frame(priors, stringsAsFactors = FALSE)
  if ("species" %in% names(priors)) {
    priors$alpha3 <- vapply(priors$species, function(s) {
      if (is.na(s)) NA_character_ else cmsy_match_species(s, among = unique(meta$alpha3))[1]
    }, character(1))
    priors$species <- NULL
  }
  id_cols <- intersect(c("Stock", "alpha3", "species_en"), names(priors))
  if (length(id_cols) == 0) stop("`priors` needs a Stock, alpha3, species_en or species column.", call. = FALSE)
  value_cols <- intersect(c("Resilience", "r.low", "r.hi", "stb.low", "stb.hi", "int.yr", "intb.low", "intb.hi",
                            "endb.low", "endb.hi", "StartYear", "EndYear", "Comment"), names(priors))
  unknown <- setdiff(names(priors), c(id_cols, value_cols))
  if (length(unknown) > 0) warning("Unknown columns in `priors` are ignored: ", paste(unknown, collapse = ", "), call. = FALSE)
  if ("Resilience" %in% value_cols) {
    bad <- !is.na(priors$Resilience) & !priors$Resilience %in% cmsy_resilience_r$resilience
    if (any(bad)) stop("Resilience must be one of ", paste0("\"", cmsy_resilience_r$resilience, "\"", collapse = ", "),
                       " (CMSY++ only accepts \"Very low\" with a small l).", call. = FALSE)
  }
  meta_key <- c(Stock = "Stock", alpha3 = "alpha3", species_en = "ASFIS.species..Name.")
  for (i in seq_len(nrow(priors))) {
    rows <- rep(FALSE, nrow(meta))
    for (col in id_cols) {
      if (!is.na(priors[[col]][i])) rows <- rows | meta[[meta_key[[col]]]] == priors[[col]][i]
    }
    if (!any(rows)) {
      warning("No stock matches row ", i, " of `priors`.", call. = FALSE)
      next
    }
    set <- character(0)
    for (col in value_cols) {
      value <- priors[[col]][i]
      if (is.na(value)) next
      meta[[col]][rows] <- value
      set <- c(set, col)
    }
    if (any(c("Resilience", "r.low", "r.hi") %in% set)) {
      meta$r_source[rows] <- "user (priors)"
      if ("Resilience" %in% set) meta$resilience_source[rows] <- "user (priors)"
    }
    if (any(grepl("^(stb|int|endb)", set))) {
      meta$prior_source[rows] <- paste0("user B/k priors (", paste(grep("^(stb|int|endb)", set, value = TRUE), collapse = ", "),
                                        "); others from FishBase/SeaLifeBase and the CMSY++ neural network")
    }
  }
  meta
}

# FAO release that load_fao_data() uses by default, without loading it
cmsy_fao_version <- function(data_dir = fao_data_dir()) {
  tryCatch({
    local <- fao_local_versions(data_dir)
    bundled <- fao_bundled_release()
    newest <- if (length(local) > 0) local[length(local)] else NA
    if (!is.na(newest) && numeric_version(newest) > numeric_version(bundled)) newest else bundled
  }, error = function(e) NA_character_)
}
