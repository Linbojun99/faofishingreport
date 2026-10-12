#' Employment in Fisheries and Aquaculture by Year
#'
#' This function summarises the FAO employment statistics (`load_fao_employment()`) of one or more
#' countries, or of the world: the number of people in aquaculture, marine fishing, inland fishing
#' and work areas not specified, by year, and how many are women and how many work full time, as
#' FAO does in The State of World Fisheries and Aquaculture (FAO 2024, 2026).
#'
#' The shares of women and of full-time workers are calculated among the people whose sex or
#' working time is known, and the share of people with unknown sex or working time is given next to
#' them, as FAO does: they are unknown for a third to a half of the people (FAO 2024, 2026), and the
#' detail available can change from year to year (FAO metadata), so a change in a share can come
#' from a change in reporting. `estimated_pct` is the share of the people in estimated values
#' (estimated by FAO or by the country).
#' The FAO notes on each country are shown and returned (there are none for the world as a
#' whole); read them before comparing years or countries.
#'
#' @inheritParams summary_params
#' @param label The name used in the output file names. Default is the country, "selected_countries"
#'        for several countries, or "World".
#' @param country_name One or more countries, as named in the FAO data (e.g. "Norway"). Default is
#'        NULL, the world (all countries summed).
#' @param employment Employment data from `load_fao_employment()`. Default is NULL, which loads it.
#' @return Invisibly, a list of two data frames:
#'   \describe{
#'     \item{yearly}{one row per country (or the world) and year: people in aquaculture, marine
#'       coastal, deep-sea and not specified fishing, all marine fishing, inland fishing, work area
#'       unspecified and in total; women, men and sex unknown; full time, part time, occasional and
#'       working time unknown; the share of women and of each working time among those known, the
#'       share with sex or working time unknown, and the share in estimated values; NA in the
#'       years without any record (not reported), between the first and last years reported;}
#'     \item{notes}{FAO's notes on the countries.}
#'   }
#'   Charts are saved in `figures/png/summary/`: `<label>_employment.png` (people by work area,
#'   stacked) and `<label>_employment_composition.png` (shares of women and full-time workers, and
#'   of unknown sex and working time). The table `<label>_employment.csv` is saved in `tables/`.
#' @references
#' FAO (2024). The State of World Fisheries and Aquaculture 2024. Blue Transformation in action.
#' Rome: FAO. \doi{10.4060/cd0683en}
#'
#' FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome: FAO. \doi{10.4060/cd8357en}
#'
#' FAO. Global employment in the primary sector of fisheries and aquaculture (FI_Employment),
#' metadata and country notes of the release. \url{https://www.fao.org/fishery/static/Data/}
#'
#' Coordinating Working Party on Fishery Statistics (CWP). Handbook of Fishery Statistics: fishers.
#' \url{https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/}
#' @export
#' @examples
#' \dontrun{
#' employment <- load_fao_employment()
#' employment_summary(employment = employment, lang = "zh")              # the world
#' employment_summary(c("Norway", "Viet Nam", "Peru"), employment = employment)
#' }
employment_summary <- function(country_name = NULL, employment = NULL, plot = TRUE, table = TRUE, lang = NULL,
                               pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), label = NULL) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  lang <- fao_lang(lang)
  if (is.null(employment)) employment <- load_fao_employment()
  world <- is.null(country_name)
  data <- if (world) employment else employment[employment$Country..Name. %in% country_name, ]
  if (!world) fao_trade_check_countries(country_name, data$Country..Name., "employment")
  data <- data[!is.na(data$Number), ]
  units <- if (world) "World" else country_name
  data$unit <- if (world) rep("World", nrow(data)) else data$Country..Name.
  name <- fao_summary_name(if (!is.null(label)) label else if (length(units) == 1) units else "selected_countries",
                           "employment")

  yearly <- do.call(rbind, lapply(units, function(u) fao_employment_yearly(data[data$unit == u, ], u)))
  unit_names <- stats::setNames(if (world) fao_label("world", lang) else fao_translate(units, "country", lang), units)
  yearly <- cbind(country = unname(unit_names[yearly$country_en]), yearly, stringsAsFactors = FALSE)
  rownames(yearly) <- NULL
  notes <- if (world) data.frame(country_en = character(), note = character()) else {
    n <- attr(employment, "country_notes")
    if (is.null(n)) data.frame(country_en = character(), note = character()) else n[n$country_en %in% units, ]
  }

  for (u in units) {
    x <- yearly[yearly$country_en == u, ]
    last <- x[nrow(x), ]
    pct <- function(v) if (is.na(v)) "-" else paste0(round(v), "%")
    message(u, ", ", last$year, ": ", format(round(last$total), big.mark = ","), " people; aquaculture ",
            pct(100 * last$aquaculture / last$total), ", marine fishing ", pct(100 * last$marine_fishing / last$total),
            ", inland fishing ", pct(100 * last$inland / last$total), ", work area unspecified ",
            pct(100 * last$unspecified / last$total), ". Women ", pct(last$female_pct), " of those with known sex (sex unknown for ",
            pct(last$sex_unknown_pct), "); full time ", pct(last$full_time_pct), " of those with known working time (unknown for ",
            pct(last$time_unknown_pct), "); estimated values ", pct(last$estimated_pct), ".")
    for (note in notes$note[notes$country_en == u]) message("FAO note on ", u, ": ", note)
  }

  if (plot) fao_employment_plots(yearly, name, lang, pptx)
  if (table) fao_save_table(yearly, name)
  fao_run_result(list(yearly = yearly, notes = notes), run, fao_run_title("employment_summary", what = country_name),
                 main = fao_cols(yearly, c("country", "year", "total", "marine_fishing", "inland", "aquaculture", "female_pct")),
                 note = "People employed, last years:", from = "bottom")
}

# People by work area, sex and working time of one country (or the world) in each year
fao_employment_yearly <- function(x, unit) {
  years <- seq(min(x$Year), max(x$Year))
  # Years without any record were not reported: NA, not zero. In a reported year, a work area,
  # sex or working time without a record has no people
  reported <- years %in% x$Year
  by <- function(keep) {
    s <- tapply(x$Number[keep], factor(x$Year[keep], levels = years), sum)
    s[is.na(s)] <- 0
    s <- as.numeric(s)
    s[!reported] <- NA_real_
    s
  }
  area <- x$Work.area..Code.
  share <- function(part, whole) ifelse(whole > 0, 100 * part / whole, NA_real_)
  out <- data.frame(country_en = unit, year = years, aquaculture = by(area == "A"), marine_coastal = by(area == "C"),
                    marine_deep_sea = by(area == "D"), marine_nei = by(area == "M"), stringsAsFactors = FALSE)
  out$marine_fishing <- out$marine_coastal + out$marine_deep_sea + out$marine_nei
  out$inland <- by(area == "I")
  out$unspecified <- by(area == "U")
  out$total <- by(rep(TRUE, nrow(x)))
  out$female <- by(x$Sex == "Female")
  out$male <- by(x$Sex == "Male")
  out$sex_unknown <- by(x$Sex == "Unknown gender")
  out$full_time <- by(x$Working.time == "Full time")
  out$part_time <- by(x$Working.time == "Part time")
  out$occasional <- by(x$Working.time == "Occasional")
  out$time_unknown <- by(x$Working.time == "Status unspecified")
  known_time <- out$full_time + out$part_time + out$occasional
  out$female_pct <- share(out$female, out$female + out$male)
  out$sex_unknown_pct <- share(out$sex_unknown, out$total)
  out$full_time_pct <- share(out$full_time, known_time)
  out$part_time_pct <- share(out$part_time, known_time)
  out$occasional_pct <- share(out$occasional, known_time)
  out$time_unknown_pct <- share(out$time_unknown, out$total)
  out$estimated_pct <- share(by(x$Number.Type == "estimate"), out$total)
  out
}

# Charts: people by work area (stacked), and the shares of women, full-time workers and unknowns
fao_employment_plots <- function(yearly, name, lang, pptx) {
  units <- unique(yearly$country)
  sectors <- c(aquaculture = fao_label("emp_aquaculture", lang), marine_fishing = fao_label("emp_marine", lang),
               inland = fao_label("emp_inland", lang), unspecified = fao_label("emp_unspecified", lang))
  million <- max(yearly$total, na.rm = TRUE) > 5e6
  # stacked bars by year: the years that were not reported stay empty
  long <- do.call(rbind, lapply(names(sectors), function(s) {
    data.frame(country = yearly$country, year = yearly$year, sector = sectors[[s]],
               people = yearly[[s]] / if (million) 1e6 else 1e3, stringsAsFactors = FALSE)
  }))
  long <- long[!is.na(long$people), ]
  long$country <- factor(long$country, levels = units)
  long$sector <- factor(long$sector, levels = sectors)
  ncol <- min(4, length(units))
  facets <- function(p) {
    if (length(units) == 1) return(p)
    p + ggplot2::facet_wrap(~country, ncol = ncol, scales = "free_y")
  }
  size <- function(file, p) {
    fao_save_plot(p, file.path("summary", file), width = if (length(units) == 1) 12 else 4 * ncol,
                  height = if (length(units) == 1) 6.5 else 2.8 * ceiling(length(units) / ncol) + 1.4, pptx = pptx)
  }
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$people, fill = .data$sector)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::scale_fill_manual(values = stats::setNames(c("#1A9641", "#2C7BB6", "#ABD9E9", "grey70"), sectors), name = NULL) +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(long$year), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label(if (million) "million_people" else "thousand_people", lang),
                  caption = fao_label("employment_note", lang)) +
    fao_theme()
  size(name, facets(p))

  shares <- c(female_pct = fao_label("emp_women_pct", lang), full_time_pct = fao_label("emp_fulltime_pct", lang),
              sex_unknown_pct = fao_label("emp_sex_unknown_pct", lang),
              time_unknown_pct = fao_label("emp_time_unknown_pct", lang))
  comp <- do.call(rbind, lapply(names(shares), function(s) {
    data.frame(country = yearly$country, year = yearly$year, measure = shares[[s]], pct = yearly[[s]],
               stringsAsFactors = FALSE)
  }))
  comp$country <- factor(comp$country, levels = units)
  comp$measure <- factor(comp$measure, levels = shares)
  q <- ggplot2::ggplot(comp[!is.na(comp$pct), ],
                       ggplot2::aes(x = .data$year, y = .data$pct, colour = .data$measure, linetype = .data$measure)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = stats::setNames(c("#D7191C", "#2C7BB6", "#D7191C", "#2C7BB6"), shares), name = NULL) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "solid", "dashed", "dashed"), shares), name = NULL) +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(comp$year), expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label("percent", lang)) +
    fao_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2), linetype = ggplot2::guide_legend(ncol = 2))
  size(paste0(name, "_composition"), if (length(units) == 1) q else q + ggplot2::facet_wrap(~country, ncol = ncol))
  invisible(NULL)
}


#' Catch per Fisher
#'
#' This function divides the catch of a country by its number of fishers, by year: the marine catch
#' (FAO marine areas) by the marine fishers (coastal, deep-sea and marine not specified), the
#' inland catch by the inland fishers, and the whole catch by all fishers, from the FAO capture
#' and employment statistics (`load_fao_data()` and `load_fao_employment()`). FAO reports
#' production per fisher by region in The State of World Fisheries and Aquaculture (FAO 2012,
#' table 9) and in its studies of fisher numbers (FAO 1999); Scherrer et al. (2024) followed marine
#' catch per fisher worldwide since 1950.
#'
#' @section Counting fishers:
#' FAO counts people, not full-time equivalents: an occasional fisher counts as one, like a
#' full-time fisher. With `weights = "fte"`, fishers are counted as full-time equivalents with the
#' weights of Scherrer et al. (2024; code on Zenodo): part time 60/95, occasional 15/95, and full
#' time and unknown working time 1. These are the middle of the CWP ranges of working time (30 to 90
#' percent, below 30 percent) relative to the middle of the full-time range (90 to 100 percent).
#' Where working time is unknown, as for many countries, the two counts are equal.
#'
#' People whose work area is not specified are not counted as fishers: they can be fishers or
#' fish farmers (in China, the part-time and occasional workers of both; FAO country note), so
#' catch per fisher is too high where many are unspecified. `unspecified_pct` gives their share of
#' all workers. Fish farmers are not counted.
#'
#' @section Reading the results:
#' \itemize{
#'   \item Catch is recorded by the flag of the vessel and fishers as the crews of national vessels
#'     (CWP handbook), but some countries leave out foreign crews (e.g. Norway; FAO country note),
#'     and the catch of distant-water fleets can come with few national fishers.
#'   \item The marine and inland split follows each country's reporting; for example, FAO notes that
#'     China's marine fishers not specified include inland fishers in 1995-2006 and 2014-2020, so
#'     use the catch per fisher of all fishing for China.
#'   \item Estimated values (by FAO or the country) are common (`fishers_estimated_pct`), and FAO's
#'     are often carried forward unchanged (FAO 2024, table T.39): `fishers_unchanged` marks years
#'     with the same number of fishers as the year before.
#'   \item Surveys can find more or fewer fishers than FAO statistics (FAO, Duke University and
#'     WorldFish 2023, Annex B).
#' }
#'
#' @inheritParams summary_params
#' @param label The name used in the output file names. Default is the country, or
#'        "selected_countries" for several countries.
#' @param country_name One or more countries, as named in the FAO data (e.g. "Norway").
#' @param weights "headcount" (default) counts fishers as people; "fte" as full-time equivalents.
#' @param data Catch data in the format of `load_fao_data()`. Default is NULL, which loads
#'        `load_fao_data(data_version)`.
#' @param employment Employment data from `load_fao_employment()`. Default is NULL, which loads it.
#' @param data_version The FAO data release of the catch data; see `load_fao_data()`. Default is
#'        "latest".
#' @return Invisibly, a list with `yearly` (one row per country and year: marine and inland catch,
#'   marine, inland and all fishers, catch per fisher of each, the number of fishers as people,
#'   fish farmers, workers of unspecified work area and their share of all workers, the share of
#'   fishers in estimated values or with unknown working time, and whether the number of fishers is
#'   unchanged from the year before; NA in the years in which the country reported no catch or no
#'   workers) and `notes` (FAO's notes on the countries). The chart
#'   `<label>_catch_per_fisher.png` is saved in `figures/png/summary/` and the table
#'   `<label>_catch_per_fisher.csv` in `tables/`.
#' @references
#' Coordinating Working Party on Fishery Statistics (CWP). Handbook of Fishery Statistics: fishers.
#' \url{https://www.fao.org/cwp-on-fishery-statistics/handbook/capture-fisheries-statistics/fishers/en/}
#'
#' FAO (1999). Numbers of fishers 1970-1996. FAO Fisheries Circular 929, Revision 1. Rome: FAO.
#' \url{https://www.fao.org/3/x1168t/x1168t.pdf}
#'
#' FAO (2012). The State of World Fisheries and Aquaculture 2012. Rome: FAO.
#' \url{https://www.fao.org/4/i2727e/i2727e.pdf}
#'
#' FAO (2024). Fishery and Aquaculture Statistics - Yearbook 2021. Rome: FAO.
#' \doi{10.4060/cc9523en}
#'
#' FAO, Duke University and WorldFish (2023). Illuminating Hidden Harvests: the contributions of
#' small-scale fisheries to sustainable development. Rome: FAO. \doi{10.4060/cc4576en}
#'
#' Scherrer, K.J.N., Rousseau, Y., Teh, L.C.L., Sumaila, U.R. and Galbraith, E.D. (2024).
#' Diminishing returns on labour in the global marine food system. Nature Sustainability 7: 45-52.
#' \doi{10.1038/s41893-023-01249-8}. Code: \doi{10.5281/zenodo.7215108}
#' @export
#' @examples
#' \dontrun{
#' employment <- load_fao_employment()
#' catch_per_fisher(c("Norway", "Iceland", "Peru", "Indonesia"), employment = employment)
#' catch_per_fisher("India", weights = "fte", employment = employment, lang = "zh")
#' }
catch_per_fisher <- function(country_name, weights = c("headcount", "fte"), data = NULL, employment = NULL,
                             plot = TRUE, table = TRUE, lang = NULL, pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"),
                             label = NULL, data_version = "latest") {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  weights <- match.arg(weights)
  lang <- fao_lang(lang)
  name <- fao_summary_name(if (!is.null(label)) label else if (length(country_name) == 1) country_name else
    "selected_countries", "catch_per_fisher")
  if (is.null(data)) data <- load_fao_data(data_version)
  catch <- data[data$Country..Name. %in% country_name & !is.na(data$Production), ]
  fao_trade_check_countries(country_name, catch$Country..Name., "catch")
  if (is.null(employment)) employment <- load_fao_employment()
  workers <- employment[employment$Country..Name. %in% country_name & !is.na(employment$Number), ]
  fao_trade_check_countries(country_name, workers$Country..Name., "employment")
  years <- intersect(seq(min(data$Year), max(data$Year)), seq(min(employment$Year), max(employment$Year)))
  if (length(years) == 0) stop("No year has both catch and employment data.", call. = FALSE)
  catch <- fao_add_groups(catch[catch$Year %in% years, ])
  workers <- workers[workers$Year %in% years, ]
  w <- if (weights == "fte") c("Full time" = 1, "Part time" = 60 / 95, "Occasional" = 15 / 95, "Status unspecified" = 1) else
    c("Full time" = 1, "Part time" = 1, "Occasional" = 1, "Status unspecified" = 1)
  workers$weighted <- workers$Number * unname(w[workers$Working.time])

  yearly <- do.call(rbind, lapply(country_name, function(country) {
    fao_fisher_yearly(catch[catch$Country..Name. == country, ], workers[workers$Country..Name. == country, ],
                      country, years)
  }))
  countries <- stats::setNames(fao_translate(country_name, "country", lang), country_name)
  yearly <- cbind(country = unname(countries[yearly$country_en]), yearly, stringsAsFactors = FALSE)
  rownames(yearly) <- NULL
  n <- attr(employment, "country_notes")
  notes <- if (is.null(n)) data.frame(country_en = character(), note = character()) else n[n$country_en %in% country_name, ]

  unit <- if (weights == "fte") "full-time equivalent fisher" else "fisher"
  for (country in country_name) {
    x <- yearly[yearly$country_en == country & !is.na(yearly$fishers) & yearly$fishers > 0, ]
    if (nrow(x) == 0) {
      message(country, ": no fishers reported.")
      next
    }
    last <- x[nrow(x), ]
    value <- function(v, digits = 1) if (is.na(v)) "-" else format(round(v, digits), big.mark = ",")
    message(country, ", ", last$year, ": ", value(last$fishers_headcount, 0), " fishers; catch per ", unit, " ",
            value(last$catch_per_fisher), " t (marine ", value(last$marine_catch_per_fisher), " t, inland ",
            value(last$inland_catch_per_fisher), " t); estimated values ", value(last$fishers_estimated_pct, 0),
            "% of the fishers; the number of fishers is unchanged from the year before in ", sum(x$fishers_unchanged),
            " of ", nrow(x), " years.")
    if (isTRUE(last$unspecified_pct > 5)) {
      message(country, ": ", round(last$unspecified_pct), "% of the workers (", value(last$unspecified, 0),
              ") are in an unspecified work area and not counted as fishers; catch per fisher is too high if they include fishers.")
    }
    for (note in notes$note[notes$country_en == country]) message("FAO note on ", country, ": ", note)
  }

  if (plot) fao_fisher_plot(yearly, name, weights, lang, pptx)
  if (table) fao_save_table(yearly, name)
  fao_run_result(list(yearly = yearly, notes = notes), run, fao_run_title("catch_per_fisher", what = country_name),
                 main = fao_cols(yearly, c("country", "year", "fishers", "catch_per_fisher", "marine_catch_per_fisher",
                                           "inland_catch_per_fisher")),
                 note = "Catch per fisher (t), last years:", from = "bottom")
}

# Catch, fishers and catch per fisher of one country in each year
fao_fisher_yearly <- function(catch, workers, country, years) {
  # Years in which the country reported no catch, or no workers, are NA, not zero
  by <- function(values, year, keep) {
    s <- tapply(values[keep], factor(year[keep], levels = years), sum)
    s[is.na(s)] <- 0
    s <- as.numeric(s)
    s[!years %in% year] <- NA_real_
    s
  }
  marine <- catch$inland_marine_en %in% "Marine areas"
  area <- workers$Work.area..Code.
  fisher <- area %in% c("C", "D", "M", "I")
  per <- function(tonnes, people) ifelse(people > 0, tonnes / people, NA_real_)
  out <- data.frame(country_en = country, year = years,
                    marine_catch_tonnes = by(catch$Production, catch$Year, marine),
                    inland_catch_tonnes = by(catch$Production, catch$Year, !marine),
                    marine_fishers = by(workers$weighted, workers$Year, area %in% c("C", "D", "M")),
                    inland_fishers = by(workers$weighted, workers$Year, area == "I"), stringsAsFactors = FALSE)
  out$fishers <- out$marine_fishers + out$inland_fishers
  out$marine_catch_per_fisher <- per(out$marine_catch_tonnes, out$marine_fishers)
  out$inland_catch_per_fisher <- per(out$inland_catch_tonnes, out$inland_fishers)
  out$catch_per_fisher <- per(out$marine_catch_tonnes + out$inland_catch_tonnes, out$fishers)
  out$fishers_headcount <- by(workers$Number, workers$Year, fisher)
  out$fish_farmers <- by(workers$Number, workers$Year, area == "A")
  out$unspecified <- by(workers$Number, workers$Year, area == "U")
  all_workers <- by(workers$Number, workers$Year, rep(TRUE, nrow(workers)))
  share <- function(part, whole) ifelse(whole > 0, 100 * part / whole, NA_real_)
  out$unspecified_pct <- share(out$unspecified, all_workers)
  out$fishers_estimated_pct <- share(by(workers$Number, workers$Year, fisher & workers$Number.Type == "estimate"),
                                     out$fishers_headcount)
  out$fishers_time_unknown_pct <- share(by(workers$Number, workers$Year, fisher & workers$Working.time == "Status unspecified"),
                                        out$fishers_headcount)
  out$fishers_unchanged <- !is.na(out$fishers_headcount) & out$fishers_headcount > 0 &
    c(FALSE, diff(out$fishers_headcount) %in% 0)
  out
}

# Chart: catch per fisher by year (marine, inland and all fishing), one panel per country
fao_fisher_plot <- function(yearly, name, weights, lang, pptx) {
  series <- c(marine_catch_per_fisher = fao_label("cpf_marine", lang), inland_catch_per_fisher = fao_label("cpf_inland", lang),
              catch_per_fisher = fao_label("cpf_all", lang))
  long <- do.call(rbind, lapply(names(series), function(s) {
    data.frame(country = yearly$country, year = yearly$year, series = series[[s]], tonnes = yearly[[s]],
               stringsAsFactors = FALSE)
  }))
  long <- long[!is.na(long$tonnes), ]
  if (nrow(long) == 0) return(invisible(NULL))
  units <- unique(yearly$country)
  long$country <- factor(long$country, levels = units)
  long$series <- factor(long$series, levels = series)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$tonnes, colour = .data$series, linetype = .data$series)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = stats::setNames(c("#2C7BB6", "#1A9641", "black"), series), name = NULL) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "solid", "dashed"), series), name = NULL) +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(long$year), expand = c(0.01, 0)) +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label(if (weights == "fte") "cpf_fte" else "cpf_headcount", lang),
                  caption = fao_label(if (weights == "fte") "cpf_note_fte" else "cpf_note_headcount", lang)) +
    fao_theme()
  ncol <- min(4, length(units))
  if (length(units) > 1) p <- p + ggplot2::facet_wrap(~country, ncol = ncol, scales = "free_y")
  fao_save_plot(p, file.path("summary", name), width = if (length(units) == 1) 12 else 4 * ncol,
                height = if (length(units) == 1) 6.5 else 2.8 * ceiling(length(units) / ncol) + 1.4, pptx = pptx)
  invisible(p)
}
