#' Catch and Trade of the Species Groups Mostly Caught
#'
#' This function compares the catch of one or more countries with their exports and imports of the
#' same ISSCAAP species groups, by year, from the FAO capture and trade statistics
#' (`load_fao_data()` and `load_fao_trade()`). It shows how much of a country's catch leaves as
#' exports, how much of the same groups it imports, and the value of this trade.
#'
#' @section Which species groups are compared:
#' Trade statistics do not tell whether a product was caught or farmed, so the trade of a group
#' that is largely farmed (salmons, shrimps, carps, oysters, ...) cannot be compared with its catch
#' (Golden et al. 2017; Guillen et al. 2019). The function therefore compares only the groups
#' mostly caught: capture made up at least `min_capture_pct` percent of the world production of
#' the group in every year analysed (see `fao_capture_groups()`). With the default 90 percent and
#' 1976-2024, these are the cods and hakes, herrings and sardines, tunas, miscellaneous pelagic and
#' demersal fishes, sharks and rays, shads, lobsters, king crabs, krill, miscellaneous marine
#' crustaceans, squids and octopuses, and horseshoe crabs, which held about a third of the value of
#' world exports in 2024. The table `groups` lists all groups of each country with their catch and
#' exports, so the groups left out can be seen.
#'
#' The catch is the country's whole catch (all FAO areas, inland and marine), because trade is
#' recorded for the country as a whole. Catch is recorded by the flag of the vessel, and catch landed
#' abroad by a country's vessels counts as an export of that country, so both refer to the same
#' fleet.
#'
#' @section Product weight and live weight:
#' Trade quantities are product weight: a tonne of fillets comes from two to three tonnes of fish,
#' a tonne of fishmeal from four to five. FAO publishes no conversion factors with the trade data;
#' they are collected from countries and published in FAO Fisheries Circular 847 (FAO 2000), and
#' factors for highly processed products are very uncertain (CWP handbook). Published studies use
#' factors of the European Market Observatory for Fisheries and Aquaculture Products (EUMOFA) or
#' others (Gephart et al. 2024; Zhao et al. 2024). Without `conversion`, the function compares
#' product weight of trade with live weight of catch: `export_catch_pct` is then the exports in
#' product weight as a percentage of the catch in live weight, usually lower than the share of the
#' catch exported. With `conversion`, a table of factors by commodity, the trade is converted to
#' live weight equivalent (commodities without a factor stay in product weight; `converted_pct`
#' gives the share of the exports, imports and re-exports with a factor), and the apparent supply, catch + imports - exports - re-exports, is
#' calculated as in food balance sheets (CWP handbook; Zhao et al. 2024).
#'
#' @section Fishmeal and fish oil:
#' FAO assigns fishmeal and fish oil to a species group when the trade records give the species
#' (e.g. anchoveta meal, herring oil), but most are traded as "Fishmeals, nei" or "Fish body oils,
#' nei" and classed as "Marine fishes not identified", which is not compared. They are made mostly
#' from small pelagic fishes and from fish waste, so the catch of a reduction fishery (e.g. the
#' Peruvian anchoveta) appears without its main export. Fishmeal, fish oil, solubles and silages
#' (ISSCFC 081, 411 and 035.01) not assigned to a group compared are reported separately as
#' `meal_oil_export_tonnes` and `meal_oil_import_tonnes` (product weight).
#'
#' @section Reading the results:
#' \itemize{
#'   \item Exports above the catch of a group (`exports_exceed_catch`) mean that the country
#'     processes and re-exports imported fish, that re-exports are not reported separately (26
#'     countries reported them in 2024, not China), that the product is classed in another group
#'     than the catch (e.g. crustaceans traded as "Crustaceans nei", group 47), or that the data
#'     disagree (Asche et al. 2022; Dent and Clarke 2015).
#'   \item Exports and imports of the same group are common: different species, sizes or products
#'     within a group, and processing of imported raw material.
#'   \item Unit values (`export_unit_value`, USD per kg product weight) are in current US dollars,
#'     not adjusted for inflation, and change with the mix of products.
#'   \item `estimated_export_value_pct` is the share of the export value estimated (by FAO or the
#'     country) or taken from the reports of trade partners.
#' }
#'
#' @inheritParams summary_params
#' @param country_name One or more countries, as named in the FAO data (e.g. "Norway").
#' @param min_capture_pct A group is compared when capture made up at least this percentage of its
#'        world production in every year analysed. Default is 90.
#' @param years The years. Default is NULL, all years with both catch and trade data (from 1976).
#' @param conversion NULL (default), or a data frame of conversion factors to live weight with the
#'        columns `commodity_code` (ISSCFC code, as `Commodity..Code.` of `load_fao_trade()`) and
#'        `factor` (live weight per product weight).
#' @param top_n The number of groups shown in the chart by group. Default is 8.
#' @param data Catch data in the format of `load_fao_data()`. Default is NULL, which loads
#'        `load_fao_data(data_version)`.
#' @param trade Trade data from `load_fao_trade()`. Default is NULL, which loads it.
#' @param data_version The FAO data release of the catch data; see `load_fao_data()`.
#' @return Invisibly, a list of three data frames:
#'   \describe{
#'     \item{yearly}{one row per country and year, for the groups compared: catch (tonnes live
#'       weight), exports, imports and re-exports (tonnes product weight), export and import value
#'       (thousand USD), unit values (USD per kg), exports as a percentage of the catch, net
#'       exports, the share of the country's catch and export value in the groups compared, the
#'       share of the export value estimated, and the trade of fishmeal and fish oil not assigned
#'       to a group compared. With `conversion`, also the trade in live weight equivalent, the
#'       share converted and the apparent supply;}
#'     \item{by_group}{the same by country, group and year;}
#'     \item{groups}{one row per country and ISSCAAP group with catch or trade: whether it is
#'       compared, the lowest share of capture in its world production, and the mean yearly
#'       catch, exports and export value of the last five years.}
#'   }
#'   Charts are saved in `figures/png/summary/`: `<label>_catch_trade.png` (catch, exports and
#'   imports by year, one panel per country) and `<label>_catch_trade_groups.png` (the same for the
#'   `top_n` groups; with several countries, one file per country ending in the country name).
#'   Tables are saved in `tables/`: `<label>_catch_trade.csv`, `..._by_group.csv` and
#'   `..._groups.csv`.
#' @references
#' Asche, F., Yang, B., Gephart, J.A., Smith, M.D., Anderson, J.L., Camp, E.V., Garlock, T.M.,
#' Love, D.C., Oglend, A. and Straume, H.-M. (2022). China's seafood imports - Not for domestic
#' consumption? Science 375: 386-388. \doi{10.1126/science.abl4756}
#'
#' Coordinating Working Party on Fishery Statistics (CWP). Handbook of Fishery Statistics:
#' conversion factors; food balance sheets.
#' \url{https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/conversionfactors/en/}
#'
#' Dent, F. and Clarke, S. (2015). State of the global market for shark products. FAO Fisheries and
#' Aquaculture Technical Paper 590. Rome: FAO. \url{https://www.fao.org/3/i4795e/i4795e.pdf}
#'
#' FAO (2000). Conversion factors - landed weight to live weight. FAO Fisheries Circular 847,
#' Revision 1. Rome: FAO. \url{https://www.fao.org/3/x9144t/x9144t.pdf}
#'
#' FAO (2021). FAO Yearbook. Fishery and Aquaculture Statistics 2019. Rome: FAO.
#' \doi{10.4060/cb7874t}
#'
#' Gephart, J.A., Agrawal Bejarano, R., Gorospe, K., Godwin, A., Golden, C.D., Naylor, R.L., et al.
#' (2024). Globalization of wild capture and farmed aquatic foods. Nature Communications 15: 8026.
#' \doi{10.1038/s41467-024-51965-8}
#'
#' Golden, C.D., Seto, K.L., Dey, M.M., Chen, O.L., Gephart, J.A., Myers, S.S., Smith, M., et al.
#' (2017). Does aquaculture support the needs of nutritionally vulnerable nations? Frontiers in
#' Marine Science 4: 159. \doi{10.3389/fmars.2017.00159}
#'
#' Guillen, J., Natale, F., Carvalho, N., Casey, J., Hofherr, J., Druon, J.-N., Fiore, G.,
#' Gibin, M., Zanzi, A. and Martinsohn, J.T. (2019). Global seafood consumption footprint. Ambio
#' 48: 111-122. \doi{10.1007/s13280-018-1060-9}
#'
#' Zhao, K., Gaines, S.D., García Molinos, J., Zhang, M. and Xu, J. (2024). Effect of trade on
#' global aquatic food consumption patterns. Nature Communications 15: 1412.
#' \doi{10.1038/s41467-024-45556-w}
#' @export
#' @examples
#' \dontrun{
#' trade <- load_fao_trade()
#' norway <- catch_trade("Norway", trade = trade, lang = "zh")
#' norway$yearly
#' norway$groups                                   # the groups compared and those left out
#'
#' # Several countries, one panel each
#' catch_trade(c("Peru", "Chile", "Morocco"), trade = trade, label = "fishmeal_countries")
#'
#' # Groups at least 80 % caught (adds flounders, crabs, unidentified marine fishes, ...)
#' catch_trade("Iceland", min_capture_pct = 80, trade = trade)
#'
#' # With your own conversion factors to live weight (e.g. from EUMOFA or FAO Circular 847)
#' factors <- data.frame(commodity_code = c("034.1.5.2.19", "034.4.1.5.2.19"), factor = c(1.17, 2.6))
#' catch_trade("Norway", trade = trade, conversion = factors)
#' }
catch_trade <- function(country_name, min_capture_pct = 90, years = NULL, conversion = NULL, top_n = 8,
                        data = NULL, trade = NULL, plot = TRUE, table = TRUE, lang = NULL,
                        pptx = getOption("faofishingreport.pptx", FALSE), label = NULL, data_version = "latest") {
  lang <- fao_lang(lang)
  name <- fao_summary_name(if (!is.null(label)) label else if (length(country_name) == 1) country_name else
    "selected_countries", "catch_trade")
  if (is.null(data)) data <- load_fao_data(data_version)
  catch <- data[data$Country..Name. %in% country_name & !is.na(data$Production), ]
  fao_trade_check_countries(country_name, catch$Country..Name., "catch")
  if (is.null(trade)) trade <- load_fao_trade()
  trade <- trade[trade$Country..Name. %in% country_name, ]
  fao_trade_check_countries(country_name, trade$Country..Name., "trade")

  span <- intersect(seq(min(data$Year), max(data$Year)), seq(min(trade$Year), max(trade$Year)))
  if (!is.null(years)) span <- intersect(span, years)
  if (length(span) == 0) stop("No year has both catch and trade data.", call. = FALSE)
  span <- sort(span)
  catch <- fao_add_groups(catch[catch$Year %in% span, ])
  trade <- trade[trade$Year %in% span, ]

  capture <- fao_capture_groups(min_capture_pct, span)
  counted <- capture$isscaap_en[capture$counted]
  if (length(counted) == 0) stop("No species group has capture above ", min_capture_pct, "% of world production in every year.",
                                 call. = FALSE)
  message("Groups compared (capture at least ", min_capture_pct, "% of world production in every year ", min(span), "-",
          max(span), "): ", paste(counted, collapse = "; "), ".")
  near <- capture[!capture$counted & !is.na(capture$capture_pct_min) & capture$capture_pct_min >= min_capture_pct - 15, ]
  if (nrow(near) > 0) {
    message("Not compared, capture fell below ", min_capture_pct, "%: ",
            paste0(near$isscaap_en, " (", format(round(near$capture_pct_min, 1), nsmall = 1), "% in ", near$year_min, ")",
                   collapse = "; "), ".")
  }

  lw <- fao_trade_live_weight(trade, conversion)
  if (!is.null(lw)) {
    trade$quantity_lw <- lw$quantity
    trade$converted <- lw$converted
  }
  animal <- fao_isscaap_groups()
  trade$meal_oil <- grepl("^(081|411|035\\.01)(\\.|$)", trade$Commodity..Code.) &
    !trade$ISSCAAP.group..Name. %in% counted &
    trade$ISSCAAP.group..Code. %in% animal$isscaap_code[animal$aquatic_animal]

  grid <- expand.grid(year = span, isscaap_en = counted, country_en = country_name, stringsAsFactors = FALSE)[, 3:1]
  by_group <- fao_trade_sums(grid, catch, trade, counted, by_group = TRUE)
  yearly <- fao_trade_sums(unique(grid[, c("country_en", "year")]), catch, trade, counted, by_group = FALSE)
  groups <- fao_trade_group_table(catch, trade, capture, country_name, span, lang)

  countries <- stats::setNames(fao_translate(country_name, "country", lang), country_name)
  zh <- animal$isscaap_zh[match(by_group$isscaap_en, animal$isscaap_en)]
  by_group <- cbind(country = unname(countries[by_group$country_en]), by_group[1],
                    group = fao_pick(by_group$isscaap_en, zh, lang), by_group[-1], stringsAsFactors = FALSE)
  yearly <- cbind(country = unname(countries[yearly$country_en]), yearly, stringsAsFactors = FALSE)
  rownames(by_group) <- rownames(yearly) <- NULL

  for (country in country_name) fao_trade_messages(yearly[yearly$country_en == country, ],
                                                    by_group[by_group$country_en == country, ], country, !is.null(lw))

  if (plot) fao_trade_plots(yearly, by_group, name, top_n, min_capture_pct, !is.null(lw), lang, pptx)
  if (table) {
    fao_save_table(yearly, name)
    fao_save_table(by_group, paste0(name, "_by_group"))
    fao_save_table(groups, paste0(name, "_groups"))
  }
  invisible(list(yearly = yearly, by_group = by_group, groups = groups))
}


fao_trade_check_countries <- function(country_name, found, what) {
  missing <- setdiff(country_name, found)
  if (length(missing) > 0) {
    stop("No ", what, " data found for: ", paste(missing, collapse = ", "),
         ". Use the country names of the FAO data, e.g. \"Korea, Republic of\".", call. = FALSE)
  }
  invisible(TRUE)
}

# Trade quantities in live weight equivalent: product weight times the factor of the commodity,
# or product weight where no factor is given
fao_trade_live_weight <- function(trade, conversion) {
  if (is.null(conversion)) return(NULL)
  if (!is.data.frame(conversion) || !all(c("commodity_code", "factor") %in% names(conversion))) {
    stop("conversion must be a data frame with the columns commodity_code (ISSCFC code, as Commodity..Code. of ",
         "load_fao_trade()) and factor (live weight per product weight).", call. = FALSE)
  }
  if (any(!is.finite(conversion$factor) | conversion$factor <= 0)) {
    stop("Conversion factors must be positive numbers.", call. = FALSE)
  }
  if (anyDuplicated(conversion$commodity_code)) stop("Each commodity can have one conversion factor only.", call. = FALSE)
  factor <- conversion$factor[match(trade$Commodity..Code., conversion$commodity_code)]
  list(quantity = trade$Quantity * ifelse(is.na(factor), 1, factor), converted = !is.na(factor))
}

# Catch and trade summed over the rows of `target` (country and year, with the ISSCAAP group if
# `by_group`), for the groups compared; country totals add the shares and the fishmeal and fish oil
fao_trade_sums <- function(target, catch, trade, counted, by_group) {
  key <- function(country, year, group = NULL) if (by_group) paste(country, group, year, sep = "\r") else
    paste(country, year, sep = "\r")
  target_key <- key(target$country_en, target$year, target$isscaap_en)
  catch_key <- key(catch$Country..Name., catch$Year, catch$isscaap_en)
  trade_key <- key(trade$Country..Name., trade$Year, trade$ISSCAAP.group..Name.)
  sum_by <- function(values, keys, keep) {
    s <- tapply(values[keep], keys[keep], sum, na.rm = TRUE)
    out <- as.numeric(s[target_key])
    out[is.na(out)] <- 0
    out
  }
  in_group <- trade$ISSCAAP.group..Name. %in% counted
  priced <- !is.na(trade$Quantity) & trade$Quantity > 0 & !is.na(trade$Value)
  flow <- function(f, column, keep = TRUE) sum_by(trade[[column]], trade_key, in_group & trade$Trade.flow == f & keep)
  unit_value <- function(f) {
    tonnes <- flow(f, "Quantity", priced)
    ifelse(tonnes > 0, flow(f, "Value", priced) / tonnes, NA_real_)
  }
  out <- target
  out$catch_tonnes <- sum_by(catch$Production, catch_key, catch$isscaap_en %in% counted)
  out$export_tonnes <- flow("Exports", "Quantity")
  out$import_tonnes <- flow("Imports", "Quantity")
  out$reexport_tonnes <- flow("Re-exports", "Quantity")
  out$export_value <- flow("Exports", "Value")
  out$import_value <- flow("Imports", "Value")
  out$export_unit_value <- unit_value("Exports")
  out$import_unit_value <- unit_value("Imports")
  live <- "quantity_lw" %in% names(trade)
  exported <- out$export_tonnes
  if (live) {
    out$export_tonnes_lw <- flow("Exports", "quantity_lw")
    out$import_tonnes_lw <- flow("Imports", "quantity_lw")
    out$reexport_tonnes_lw <- flow("Re-exports", "quantity_lw")
    all_tonnes <- sum_by(trade$Quantity, trade_key, in_group)
    out$converted_pct <- ifelse(all_tonnes > 0, 100 * sum_by(trade$Quantity, trade_key, in_group & trade$converted) /
                                  all_tonnes, NA_real_)
    out$apparent_supply_tonnes <- out$catch_tonnes + out$import_tonnes_lw - out$export_tonnes_lw - out$reexport_tonnes_lw
    exported <- out$export_tonnes_lw
  }
  out$export_catch_pct <- ifelse(out$catch_tonnes > 0, 100 * exported / out$catch_tonnes, NA_real_)
  out$net_export_tonnes <- if (live) out$export_tonnes_lw - out$import_tonnes_lw else out$export_tonnes - out$import_tonnes
  out$exports_exceed_catch <- exported > out$catch_tonnes
  if (by_group) return(out)

  # Country totals: shares of the whole catch and export value, estimated values, fishmeal and fish oil
  total_catch <- sum_by(catch$Production, key(catch$Country..Name., catch$Year), rep(TRUE, nrow(catch)))
  exports <- trade$Trade.flow == "Exports"
  total_value <- sum_by(trade$Value, trade_key, exports)
  estimated <- trade$Value.Type %in% c("estimate", "partner data")
  out$counted_catch_pct <- ifelse(total_catch > 0, 100 * out$catch_tonnes / total_catch, NA_real_)
  out$counted_export_value_pct <- ifelse(total_value > 0, 100 * out$export_value / total_value, NA_real_)
  out$estimated_export_value_pct <- ifelse(out$export_value > 0, 100 * flow("Exports", "Value", estimated) / out$export_value,
                                           NA_real_)
  out$meal_oil_export_tonnes <- sum_by(trade$Quantity, trade_key, exports & trade$meal_oil)
  out$meal_oil_import_tonnes <- sum_by(trade$Quantity, trade_key, trade$Trade.flow == "Imports" & trade$meal_oil)
  out$meal_oil_export_value <- sum_by(trade$Value, trade_key, exports & trade$meal_oil)
  out
}

# All ISSCAAP groups of each country with catch or trade: whether compared, and the catch and
# exports of the last five years
fao_trade_group_table <- function(catch, trade, capture, country_name, span, lang) {
  recent <- span[span > max(span) - 5]
  groups <- fao_isscaap_groups()
  do.call(rbind, lapply(country_name, function(country) {
    x <- catch[catch$Country..Name. == country & catch$Year %in% recent, ]
    t <- trade[trade$Country..Name. == country & trade$Year %in% recent & trade$Trade.flow == "Exports", ]
    names <- union(unique(x$isscaap_en), unique(t$ISSCAAP.group..Name.))
    names <- names[!is.na(names)]
    mean_of <- function(values, group_of) {
      s <- tapply(values, group_of, sum, na.rm = TRUE)[names] / length(recent)
      s[is.na(s)] <- 0
      as.numeric(s)
    }
    out <- data.frame(country = fao_translate(country, "country", lang), country_en = country,
                      group = fao_pick(names, groups$isscaap_zh[match(names, groups$isscaap_en)], lang),
                      isscaap_en = names, compared = names %in% capture$isscaap_en[capture$counted],
                      world_capture_pct_min = capture$capture_pct_min[match(names, capture$isscaap_en)],
                      catch_tonnes = mean_of(x$Production, x$isscaap_en),
                      export_tonnes = mean_of(t$Quantity, t$ISSCAAP.group..Name.),
                      export_value = mean_of(t$Value, t$ISSCAAP.group..Name.), stringsAsFactors = FALSE)
    out$export_value_pct <- if (sum(out$export_value) > 0) 100 * out$export_value / sum(out$export_value) else NA_real_
    names(out)[7:9] <- paste0(names(out)[7:9], "_", min(recent), "_", max(recent))
    out[order(!out$compared, -out[[9]], -out[[7]]), ]
  }))
}

fao_trade_messages <- function(yearly, by_group, country, live) {
  last <- yearly[nrow(yearly), ]
  weight <- if (live) "live weight equivalent" else "product weight"
  pct <- function(x) if (is.na(x)) "-" else paste0(round(x), "%")
  message(country, ", ", last$year, ": the groups compared hold ", pct(last$counted_catch_pct), " of the catch and ",
          pct(last$counted_export_value_pct), " of the export value; exports (", weight, ") equal ",
          pct(last$export_catch_pct), " of their catch; ", pct(last$estimated_export_value_pct),
          " of their export value is estimated or from partner reports.")
  if (any(yearly$meal_oil_export_tonnes > 0)) {
    message(country, ": fishmeal and fish oil not assigned to a group compared, exported in ", last$year, ": ",
            format(round(last$meal_oil_export_tonnes), big.mark = ","), " tonnes product weight (see meal_oil_export_tonnes).")
  }
  exceed <- by_group[by_group$exports_exceed_catch, ]
  if (nrow(exceed) > 0) {
    counts <- sort(table(exceed$isscaap_en), decreasing = TRUE)
    message(country, ": exports exceed the catch in ",
            paste0(names(counts), " (", counts, ifelse(counts == 1, " year)", " years)"), collapse = "; "),
            "; processing of imported fish, re-exports, products classed in another group, or data errors.")
  }
}

# Charts: catch, exports and imports by year, one panel per country, and by group
fao_trade_plots <- function(yearly, by_group, name, top_n, min_capture_pct, live, lang, pptx) {
  series <- c(catch = fao_label("trade_catch", lang),
              exports = fao_label(if (live) "trade_exports_lw" else "trade_exports", lang),
              imports = fao_label(if (live) "trade_imports_lw" else "trade_imports", lang))
  colors <- stats::setNames(c("#2C7BB6", "#D7191C", "#1A9641"), series)
  long <- function(x, panel) {
    exports <- if (live) x$export_tonnes_lw else x$export_tonnes
    imports <- if (live) x$import_tonnes_lw else x$import_tonnes
    data.frame(panel = rep(x[[panel]], 3), year = rep(x$year, 3),
               series = factor(rep(series, each = nrow(x)), levels = series),
               kt = c(x$catch_tonnes, exports, imports) / 1000, stringsAsFactors = FALSE)
  }
  chart <- function(d, panels) {
    d$panel <- factor(d$panel, levels = panels)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$kt, colour = .data$series)) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = colors, name = NULL) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(d$year, by = if (length(panels) == 1) 5 else 10),
                                  expand = c(0.01, 0)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("thousand_tonnes", lang),
                    caption = fao_label("catch_trade_note", lang, n = min_capture_pct)) +
      fao_theme()
    if (length(panels) > 1) p <- p + ggplot2::facet_wrap(~panel, ncol = min(4, length(panels)), scales = "free_y")
    p
  }
  save <- function(p, file, n) {
    ncol <- min(4, n)
    fao_save_plot(p, file.path("summary", file), width = if (n == 1) 12 else 4 * ncol,
                  height = if (n == 1) 6.5 else 2.8 * ceiling(n / ncol) + 1.4, pptx = pptx)
  }
  countries <- unique(yearly$country)
  save(chart(long(yearly, "country"), countries), name, length(countries))
  for (country in unique(by_group$country_en)) {
    x <- by_group[by_group$country_en == country, ]
    size <- tapply(x$catch_tonnes + x$export_tonnes + x$import_tonnes, x$group, sum)
    top <- names(sort(size[size > 0], decreasing = TRUE))[seq_len(min(top_n, sum(size > 0)))]
    if (length(top) == 0) next
    file <- if (length(countries) == 1) paste0(name, "_groups") else paste0(name, "_groups_", fao_file_name(country))
    save(chart(long(x[x$group %in% top, ], "group"), top), file, length(top))
  }
  invisible(NULL)
}
