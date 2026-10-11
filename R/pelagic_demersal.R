#' Pelagic/Demersal Catch Ratio
#'
#' This function calculates the ratio of pelagic to demersal catch (P/D) of each FAO marine
#' fishing area in the data, by year and by decade. Caddy (1993) proposed that nutrient enrichment
#' of a sea favours short-lived plankton-feeding pelagic fishes over demersal fishes, and de Leiva
#' Moreno et al. (2000) used the ratio of pelagic to demersal landings as a proxy for the nutrient
#' level of the semi-enclosed seas around Europe.
#'
#' For each area and year, \eqn{P/D = \sum P / \sum D}{P/D = sum(P) / sum(D)}, the catch (tonnes) of
#' the groups counted as pelagic divided by the catch of the groups counted as demersal. The
#' ratio of a decade is the mean of its yearly ratios (as Hondorp et al. 2010), with the lowest and
#' highest yearly ratio, as de Leiva Moreno et al. (2000) reported mean values and ranges.
#'
#' @section Which species are pelagic and which demersal:
#' The FAO data give each species item an ISSCAAP species group, so the function counts groups
#' (see `fao_pd_groups()`). Two published definitions are available:
#' \describe{
#'   \item{"de_leiva_moreno" (default)}{Small pelagic fishes against demersal fishes and
#'     crustaceans, as de Leiva Moreno et al. (2000): pelagic = herrings, sardines, anchovies;
#'     demersal = flounders, cods and hakes, miscellaneous coastal and demersal fishes, and the
#'     marine crustacean groups (crabs, lobsters, king crabs, shrimps, miscellaneous marine
#'     crustaceans). As in that study, tunas and other large pelagics, the miscellaneous pelagic
#'     fishes (mainly medium pelagics such as horse mackerels and mackerels), unidentified fishes,
#'     cephalopods and other molluscs, and diadromous fishes are left out; sharks and rays (about
#'     which the study says nothing) and krill are left out too.}
#'   \item{"grainger_garcia"}{All pelagic against all demersal fishes, as Grainger and Garcia
#'     (1996) for the oceans: pelagic = herrings, sardines, anchovies, tunas and billfishes, and
#'     miscellaneous pelagic fishes; demersal = flounders, cods and hakes, miscellaneous coastal
#'     and demersal fishes, sharks and rays, and unidentified marine fishes. Invertebrates are left
#'     out.}
#' }
#' Other groups can be given as `pelagic` and `demersal`.
#'
#' The published studies classed each species (by habitat or diet), not ISSCAAP groups, and FAO
#' revised the ISSCAAP fish groups 33, 34 and 37 in 2001, so their values are only roughly
#' comparable with those of this function. Some groups mix pelagic and demersal species: for
#' example, the miscellaneous coastal fishes (group 33) include sandeels and mullets, which some
#' studies count as pelagic.
#'
#' @section Interpretation:
#' de Leiva Moreno et al. (2000) found P/D below 1 in oligotrophic seas (e.g. Levantine and Ionian
#' seas), around 1 to 3.5 in mesotrophic seas (e.g. North Sea, Adriatic) and around 6 to 20 or
#' more in eutrophic seas (e.g. Black Sea, Sea of Azov, Baltic), and a positive relation between
#' ln(P/D) and chlorophyll; they described these classes as subjective and overlapping. The ratio
#' also changes for reasons other than nutrients:
#' \itemize{
#'   \item Fishing and markets: demersal fishes are usually in higher demand, so their overfishing
#'     alone raises P/D (de Leiva Moreno et al. 2000; Caddy et al. 1998); rules on gears such as
#'     purse seines change it too (Hondorp et al. 2010).
#'   \item Oxygen: under hypoxia demersal and benthic stocks decline while pelagic fishes are less
#'     affected, so P/D can rise with hypoxia rather than with nutrients alone (de Leiva Moreno et
#'     al. 2000; Hondorp et al. 2010).
#'   \item Natural variability: small pelagic stocks fluctuate strongly with climate, and a few
#'     species can drive the ratio (Grainger and Garcia 1996; Hondorp et al. 2010).
#'   \item Reporting: catch not identified to species ("Marine fishes not identified", shown as
#'     `unidentified_pct`) is probably mostly demersal (Grainger and Garcia 1996), and its share
#'     changes over time. The FAO capture data contain neither aquaculture nor discards.
#' }
#' With `by = "division"`, the ratio is calculated for each division of the catch data of
#' `load_fao_divisions()`. Codes that are not places, such as "37.0.0 Tunas" or "37.9.0 Not
#' known", are kept as their own units.
#'
#' Read the ratio together with the catch of each group (the group table and the composition
#' chart) before attributing a change to nutrients. Where no pelagic group is caught (e.g. the
#' Antarctic areas, where krill is not counted), P/D is 0; the chart, on a log scale, leaves such
#' years out.
#'
#' @inheritParams summary_params
#' @param definition The groups counted as pelagic and demersal: "de_leiva_moreno" (default) or
#'        "grainger_garcia" (see above and `fao_pd_groups()`).
#' @param by "area" (default) for the FAO major areas, or "division" for the divisions of the
#'        catch data of `load_fao_divisions()` (e.g. the GFCM divisions of the Mediterranean and
#'        Black Sea, the units of de Leiva Moreno et al. 2000).
#' @param pelagic,demersal ISSCAAP groups (English names as in the FAO data, e.g. "Herrings,
#'        sardines, anchovies") counted as pelagic and demersal instead of those of `definition`.
#'        Default is NULL.
#' @return Invisibly, a list of three data frames:
#'   \describe{
#'     \item{yearly}{one row per area and year: pelagic, demersal, other and total catch (tonnes),
#'       P/D, the pelagic share of the pelagic and demersal catch, the share of the catch counted
#'       as pelagic or demersal, and the share of unidentified marine fishes;}
#'     \item{decades}{one row per area and decade: mean, lowest and highest yearly P/D, and the
#'       catch of the decade;}
#'     \item{groups}{the ISSCAAP groups of the data, how each is counted and its catch.}
#'   }
#'   With `by = "division"`, `area` and `area_en` hold the divisions, `area_code` their codes, and
#'   the file names end in `_pelagic_demersal_divisions`.
#'   Charts are saved in `figures/png/summary/`: `<label>_pelagic_demersal_ratio.png` (P/D by
#'   year, one panel per area), and `<label>_pelagic_demersal.png` and `..._share.png` (pelagic,
#'   demersal and other catch). Tables are saved in `tables/`: `<label>_pelagic_demersal.csv`,
#'   `..._decades.csv` and `..._groups.csv`.
#' @references
#' Caddy, J.F. (1993). Toward a comparative evaluation of human impacts on fishery ecosystems of
#' enclosed and semi-enclosed seas. Reviews in Fisheries Science 1: 57-95.
#' \doi{10.1080/10641269309388535}
#'
#' Caddy, J.F., Carocci, F. and Coppola, S. (1998). Have peak fishery production levels been
#' passed in continental shelf area? Some perspectives arising from historical trends in
#' production per shelf area. Journal of Northwest Atlantic Fishery Science 23: 191-219.
#' \doi{10.2960/J.v23.a13}
#'
#' de Leiva Moreno, J.I., Agostini, V.N., Caddy, J.F. and Carocci, F. (2000). Is the
#' pelagic-demersal ratio from fishery landings a useful proxy for nutrient availability? A
#' preliminary data exploration for the semi-enclosed seas around Europe. ICES Journal of Marine
#' Science 57: 1091-1102. \doi{10.1006/jmsc.2000.0705}
#'
#' Grainger, R.J.R. and Garcia, S.M. (1996). Chronicles of marine fishery landings (1950-1994):
#' trend analysis and fisheries potential. FAO Fisheries Technical Paper 359. Rome: FAO.
#' \url{https://www.fao.org/4/w3244e/w3244e00.htm}
#'
#' Hondorp, D.W., Breitburg, D.L. and Davias, L.A. (2010). Eutrophication and fisheries: separating
#' the effects of nitrogen loads and hypoxia on the pelagic-to-demersal ratio and other measures
#' of landings composition. Marine and Coastal Fisheries 2: 339-361. \doi{10.1577/C09-020.1}
#' @export
#' @examples
#' \dontrun{
#' FAO37 <- fishing_area_data("Mediterranean and Black Sea", plot = FALSE)
#' pd <- pelagic_demersal(FAO37$area_data, lang = "zh")
#' pd$decades
#' pd$groups                                   # how each ISSCAAP group is counted
#'
#' # All pelagic and demersal fishes (Grainger and Garcia 1996)
#' pelagic_demersal(FAO37$area_data, definition = "grainger_garcia", label = "FAO37_all_fishes")
#'
#' # Your own groups, e.g. small and medium pelagics
#' pelagic_demersal(FAO37$area_data,
#'                  pelagic = c("Herrings, sardines, anchovies", "Miscellaneous pelagic fishes"),
#'                  demersal = fao_pd_groups()$isscaap_en[fao_pd_groups()$de_leiva_moreno == "demersal"],
#'                  label = "FAO37_small_medium_pelagic")
#'
#' # The GFCM divisions of the Mediterranean and Black Sea
#' pelagic_demersal(load_fao_divisions("GFCM"), by = "division")
#'
#' # All FAO major marine fishing areas, one panel per area
#' marine <- subset(load_fao_data(), FAO.major.fishing.area..Name. %in% fao_area_sst()$area_en)
#' pelagic_demersal(marine, label = "marine_areas")
#' }
pelagic_demersal <- function(area_data, definition = c("de_leiva_moreno", "grainger_garcia"),
                             pelagic = NULL, demersal = NULL, by = c("area", "division"), plot = TRUE, table = TRUE,
                             lang = NULL, pptx = getOption("faofishingreport.pptx", FALSE), label = NULL) {
  definition <- match.arg(definition)
  by <- match.arg(by)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label),
                           if (by == "division") "pelagic_demersal_divisions" else "pelagic_demersal")
  definitions <- fao_pd_groups()
  custom <- !is.null(pelagic) || !is.null(demersal)
  if (is.null(pelagic)) pelagic <- definitions$isscaap_en[definitions[[definition]] == "pelagic"]
  if (is.null(demersal)) demersal <- definitions$isscaap_en[definitions[[definition]] == "demersal"]
  fao_pd_check_groups(pelagic, demersal, fao_group_table()$species$isscaap_en)
  definition_name <- if (custom) "your own groups" else
    c(de_leiva_moreno = "de Leiva Moreno et al. 2000", grainger_garcia = "Grainger and Garcia 1996")[[definition]]
  message("P/D (", definition_name, "): pelagic = ", paste(pelagic, collapse = "; "), ". Demersal = ",
          paste(demersal, collapse = "; "), ".")

  data <- fao_add_groups(area_data)
  inland <- !data$inland_marine_en %in% "Marine areas"
  if (any(inland)) {
    message("Inland fishing areas are left out of the pelagic/demersal ratio.")
    data <- data[!inland, ]
  }
  if (nrow(data) == 0) stop("The data contains no marine fishing area.", call. = FALSE)
  data$class <- ifelse(data$isscaap_en %in% pelagic, "pelagic",
                       ifelse(data$isscaap_en %in% demersal, "demersal", "other"))
  # The spatial units: FAO major areas (sorted by catch) or divisions (sorted by code)
  units <- fao_spatial_units(data, by)
  data$unit <- units$name
  data$unit_code <- units$code
  areas <- if (by == "area") {
    names(sort(tapply(data$Production, data$unit, sum, na.rm = TRUE), decreasing = TRUE))
  } else {
    unique(data$unit[order(data$unit_code)])
  }
  years <- seq(min(data$Year), max(data$Year))
  yearly <- do.call(rbind, lapply(areas, function(a) {
    x <- data[data$unit == a, ]
    cbind(area_code = x$unit_code[1], fao_pd_yearly(x, a, years), stringsAsFactors = FALSE)
  }))
  decades <- do.call(rbind, lapply(areas, function(a) fao_pd_periods(yearly[yearly$area_en == a, ])))
  decades <- cbind(area_code = yearly$area_code[match(decades$area_en, yearly$area_en)], decades, stringsAsFactors = FALSE)
  group_table <- fao_pd_group_table(data, pelagic, demersal, lang)

  area_names <- stats::setNames(if (by == "area") fao_translate(areas, "area", lang) else areas, areas)
  yearly <- cbind(area = unname(area_names[yearly$area_en]), yearly)
  decades <- cbind(area = unname(area_names[decades$area_en]), decades)
  rownames(yearly) <- rownames(decades) <- NULL

  for (a in areas) {
    x <- yearly[yearly$area_en == a, ]
    d <- decades[decades$area_en == a & !is.na(decades$pd_mean), ]
    message(a, ": pelagic and demersal groups make up ",
            round(100 * sum(x$pelagic_tonnes + x$demersal_tonnes) / sum(x$total_tonnes)),
            "% of the catch, unidentified marine fishes ", round(100 * sum(x$unidentified_tonnes) / sum(x$total_tonnes)),
            "%; ", if (nrow(d) == 0) "no P/D (no demersal catch counted)" else
              paste0("mean P/D ", paste0(d$period, " ", format(round(d$pd_mean, 2), nsmall = 2), collapse = ", ")))
  }

  # A ratio of 0 cannot be drawn on the log scale of the chart
  zero <- areas[vapply(areas, function(a) all(yearly$pd_ratio[yearly$area_en == a] %in% c(0, NA)), logical(1))]
  if (length(zero) > 0) {
    message("No pelagic catch counted (P/D = 0 or missing in every year), so not on the P/D chart: ",
            paste(zero, collapse = ", "), ".")
  }

  if (plot) fao_pd_plots(data, yearly, name, lang, pptx, definition_name)
  if (table) {
    fao_save_table(yearly, name)
    fao_save_table(decades, paste0(name, "_decades"))
    fao_save_table(group_table, paste0(name, "_groups"))
  }
  invisible(list(yearly = yearly, decades = decades, groups = group_table))
}


#' Pelagic and Demersal Species Groups
#'
#' This function lists the ISSCAAP species groups of marine fishes and crustaceans, and how each
#' is counted by the two definitions of `pelagic_demersal()`: "pelagic", "demersal" or "not
#' counted". ISSCAAP groups not listed (molluscs, diadromous and freshwater fishes, mammals,
#' plants, other invertebrates) are not counted by either definition.
#'
#' @return A data frame with one row per group: `isscaap_code`, `isscaap_en` (the name used in
#'   the FAO data), `de_leiva_moreno` and `grainger_garcia`.
#' @inherit pelagic_demersal references
#' @export
#' @examples
#' fao_pd_groups()
fao_pd_groups <- function() {
  groups <- data.frame(
    isscaap_code = c(31, 32, 33, 34, 35, 36, 37, 38, 39, 42, 43, 44, 45, 46, 47),
    isscaap_en = c("Flounders, halibuts, soles", "Cods, hakes, haddocks", "Miscellaneous coastal fishes",
                   "Miscellaneous demersal fishes", "Herrings, sardines, anchovies", "Tunas, bonitos, billfishes",
                   "Miscellaneous pelagic fishes", "Sharks, rays, chimaeras", "Marine fishes not identified",
                   "Crabs, sea-spiders", "Lobsters, spiny-rock lobsters", "King crabs, squat-lobsters",
                   "Shrimps, prawns", "Krill, planktonic crustaceans", "Miscellaneous marine crustaceans"),
    de_leiva_moreno = c("demersal", "demersal", "demersal", "demersal", "pelagic", "not counted", "not counted",
                        "not counted", "not counted", "demersal", "demersal", "demersal", "demersal",
                        "not counted", "demersal"),
    grainger_garcia = c("demersal", "demersal", "demersal", "demersal", "pelagic", "pelagic", "pelagic",
                        "demersal", "demersal", "not counted", "not counted", "not counted", "not counted",
                        "not counted", "not counted"),
    stringsAsFactors = FALSE
  )
  groups
}

fao_pd_check_groups <- function(pelagic, demersal, known) {
  unknown <- setdiff(c(pelagic, demersal), known)
  if (length(unknown) > 0) {
    stop("Unknown ISSCAAP group: ", paste(unknown, collapse = ", "), ". Groups: ",
         paste(sort(unique(stats::na.omit(known))), collapse = "; "), call. = FALSE)
  }
  both <- intersect(pelagic, demersal)
  if (length(both) > 0) stop("A group cannot be both pelagic and demersal: ", paste(both, collapse = ", "), call. = FALSE)
  if (length(pelagic) == 0 || length(demersal) == 0) stop("Give at least one pelagic and one demersal group.", call. = FALSE)
  invisible(TRUE)
}

# Pelagic, demersal and other catch of one area in each year, the P/D ratio, the pelagic share of
# the pelagic and demersal catch, the share of the catch counted, and the share of unidentified
# marine fishes
fao_pd_yearly <- function(x, area_en, years) {
  by_year <- function(keep) {
    out <- tapply(x$Production[keep], factor(x$Year[keep], levels = years), sum, na.rm = TRUE)
    out[is.na(out)] <- 0
    as.numeric(out)
  }
  pelagic <- by_year(x$class == "pelagic")
  demersal <- by_year(x$class == "demersal")
  unidentified <- by_year(x$isscaap_en %in% "Marine fishes not identified")
  total <- by_year(rep(TRUE, nrow(x)))
  data.frame(area_en = area_en, year = years, pelagic_tonnes = pelagic, demersal_tonnes = demersal,
             other_tonnes = total - pelagic - demersal, total_tonnes = total,
             pd_ratio = ifelse(demersal > 0, pelagic / demersal, NA_real_),
             pelagic_pct = ifelse(pelagic + demersal > 0, 100 * pelagic / (pelagic + demersal), NA_real_),
             counted_pct = ifelse(total > 0, 100 * (pelagic + demersal) / total, NA_real_),
             unidentified_tonnes = unidentified,
             unidentified_pct = ifelse(total > 0, 100 * unidentified / total, NA_real_),
             stringsAsFactors = FALSE)
}

# P/D of each decade (the latest, unfinished decade included): mean, lowest and highest of the
# yearly ratios, and the catch of the decade
fao_pd_periods <- function(x) {
  period <- fao_decade(x$year)
  do.call(rbind, lapply(split(x, factor(period, levels = unique(period))), function(p) {
    ratio <- p$pd_ratio[!is.na(p$pd_ratio)]
    data.frame(area_en = p$area_en[1], period = fao_decade(p$year[1]), first_year = min(p$year),
               last_year = max(p$year), n_years = length(ratio),
               pd_mean = if (length(ratio) > 0) mean(ratio) else NA_real_,
               pd_min = if (length(ratio) > 0) min(ratio) else NA_real_,
               pd_max = if (length(ratio) > 0) max(ratio) else NA_real_,
               pelagic_tonnes = sum(p$pelagic_tonnes), demersal_tonnes = sum(p$demersal_tonnes),
               total_tonnes = sum(p$total_tonnes),
               counted_pct = if (sum(p$total_tonnes) > 0) 100 * sum(p$pelagic_tonnes + p$demersal_tonnes) /
                 sum(p$total_tonnes) else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

# Catch of each ISSCAAP group in the data and how it is counted
fao_pd_group_table <- function(data, pelagic, demersal, lang) {
  catch <- tapply(data$Production, data$isscaap_en, sum, na.rm = TRUE)
  catch <- sort(catch, decreasing = TRUE)
  zh <- data$isscaap_zh[match(names(catch), data$isscaap_en)]
  counted <- ifelse(names(catch) %in% pelagic, "pelagic", ifelse(names(catch) %in% demersal, "demersal", "not counted"))
  data.frame(group = fao_pick(names(catch), zh, lang), isscaap_en = names(catch), counted_as = counted,
             catch_tonnes = as.numeric(catch), catch_pct = 100 * as.numeric(catch) / sum(catch),
             stringsAsFactors = FALSE)
}

# Charts: P/D by year (one panel per area), and the pelagic, demersal and other catch
fao_pd_plots <- function(data, yearly, name, lang, pptx, definition_name) {
  areas <- unique(yearly$area)
  yearly$area <- factor(yearly$area, levels = areas)
  ncol <- min(4, length(areas))
  nrow <- ceiling(length(areas) / ncol)
  caption <- paste0(fao_label("pd_ratio_note", lang), " (", definition_name, ")")
  p <- ggplot2::ggplot(yearly[!is.na(yearly$pd_ratio) & yearly$pd_ratio > 0, ],
                       ggplot2::aes(x = .data$year, y = .data$pd_ratio)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_line(colour = "#2C7BB6", linewidth = 0.6) +
    ggplot2::geom_point(colour = "#2C7BB6", size = 1.1) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_continuous(breaks = fao_year_breaks(yearly$year, by = if (length(areas) == 1) 10 else 20),
                                expand = c(0.01, 0)) +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label("pd_ratio", lang), caption = caption) +
    fao_theme()
  if (length(areas) > 1) {
    p <- p + ggplot2::facet_wrap(~area, ncol = ncol, scales = "free_y") +
      ggplot2::theme(panel.spacing.x = ggplot2::unit(1.5, "lines"))
  }
  fao_save_plot(p, file.path("summary", paste0(name, "_ratio")),
                width = if (length(areas) == 1) 12 else 4 * ncol,
                height = if (length(areas) == 1) 6 else 2.6 * nrow + 1.2, pptx = pptx)

  class_labels <- c(pelagic = fao_label("pelagic", lang), demersal = fao_label("demersal", lang),
                    other = fao_label("other_catch", lang))
  data$category <- factor(class_labels[data$class], levels = class_labels)
  composition <- fao_yearly_by(data, "category")
  fao_stack_charts(composition, fao_label("pd_class", lang), file.path("summary", name), lang, pptx,
                   colors = stats::setNames(c("#2C7BB6", "#A6611A", "grey75"), class_labels))
  invisible(p)
}
