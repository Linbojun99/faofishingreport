#' Mean Temperature of the Catch
#'
#' This function calculates the mean temperature of the catch (MTC) of each FAO fishing area in
#' the data: the average preferred temperature of the species caught, weighted by their catch
#' (Cheung et al. 2013), and compares it with the sea surface temperature (SST) of the area from
#' a chosen SST product (see `fao_area_sst()` and `fao_sst_products()`). A rising MTC means that
#' species of warmer waters make up a larger part of the catch.
#'
#' For each year \eqn{t},
#' \deqn{MTC_t = \frac{\sum_i T_i C_{i,t}}{\sum_i C_{i,t}}}{MTC_t = sum_i(T_i * C_i,t) / sum_i(C_i,t)}
#' where \eqn{T_i} is the preferred temperature of the FAO species item \eqn{i} and
#' \eqn{C_{i,t}} its catch in year \eqn{t}; both sums are over the items that have a preferred
#' temperature. The coverage of a year is the share of its catch made up by these items.
#'
#' @section What the package does and what the user must check:
#' The package computes the MTC, its trends and its comparison with SST. The preferred
#' temperature of each species is \strong{not} checked by the package: it is a first, automatic
#' match of the FAO scientific name to FishBase or SeaLifeBase (`fishbase_match()`), given with
#' the links needed to check it. Whether a match and its temperature are right must be judged by
#' the user, from these pages and the literature, before the results are used:
#' \enumerate{
#'   \item Run the function, then open the species table (`tables/<label>_mtc_species.csv`). Items
#'     are sorted by catch, so the first rows matter most.
#'   \item For each item used, open `url` (the FishBase or SeaLifeBase page of the matched
#'     species), `search_url` and `worms_url` (the World Register of Marine Species). Check that
#'     the matched species is the FAO item, and that its preferred temperature is plausible for
#'     the stock in the area.
#'   \item Write who checked it, or the date, in the column `checked`. Correct a wrong value in
#'     `temp_pref_mean` and give the source in `note` (to use a value for an item that is not
#'     used, such as a genus, also set its `match_type` to "manual"); or record the correction in
#'     the corrections file of `fishbase_match()`, which every later run applies.
#'   \item Run the function again with `match = "<path of the checked table>"`.
#' }
#' Until every item used has been marked as checked, the function says so in its messages and in
#' the caption of the charts.
#'
#' Checking is needed because names do not always match between databases: FAO ASFIS names
#' change slowly and conservatively, FishBase, SeaLifeBase and WoRMS follow different
#' classifications, and synonyms and misspellings are common (Grenié et al. 2023).
#' The preferred temperature itself is also uncertain (next section).
#'
#' @section Preferred temperatures:
#' The preferred temperature used is `TempPrefMean` of the FishBase or SeaLifeBase `estimate`
#' table. FishBase gives it as an estimate from AquaMaps distribution maps (version 10/2019),
#' computed with an unpublished R script (FishBase reference 123201: Froese 2020, pers. comm.);
#' how the map cells are chosen and whether surface or bottom temperature is used is not
#' documented. For some species it is well below the temperatures of the area where most of their
#' catch comes from; for example, it is 10.3 °C for European pilchard (Sardina pilchardus), much
#' of whose catch is taken off north-west Africa. Cheung et al. (2013) used instead the median of
#' a temperature preference profile computed from modelled species distributions, and most
#' regional studies used their values (e.g. Tsikliras and Stergiou 2014; Liang et al. 2018);
#' Dimarchopoulou et al. (2022) and Lavin et al. (2023) used the FishBase mean, as here. The
#' level of the MTC therefore depends on the source of the values, and the MTC of this function
#' is not directly comparable with published values.
#'
#' Only matched items whose `match_type` is in `use_match` are used: by default, items whose
#' FAO scientific name is a valid species name ("exact"), a synonym in the synonym table of
#' FishBase or SeaLifeBase ("synonym"), either of these after looking up the valid name in WoRMS
#' ("WoRMS valid name (...)"), or set by the user ("manual"). Items above species level (e.g.
#' "Sardinella spp" or "Marine fishes nei") are not used, as in the studies above, since the
#' species that make up their catch are not known; some studies give genera the mean of the
#' species of the genus found in the area (Dimarchopoulou et al. 2022).
#'
#' @section Coverage:
#' The items used never cover the whole catch, and published studies report coverage from below
#' 30% to over 90% (e.g. Liang et al. 2018; Tsikliras et al. 2015). No published study sets a
#' minimum coverage: `min_coverage` (default 50%) is a convention of this package, and should be
#' set for each analysis. The coverage changes over time because more catch is reported by species
#' instead of groups, without the earlier years being revised (Garibaldi 2012); such changes, and
#' catches moved between areas or reported as "nei" (FAO 2024 reports 1.3 million tonnes of
#' Chinese distant-water catch recorded as "nei" in Area 61), change the MTC without any change in
#' temperature. Look at the coverage chart before reading a trend.
#'
#' @section Trends and comparison with SST:
#' Trends are calculated over all years and over the years from `trend_from`, using only years
#' whose coverage is at least `min_coverage`: the slope of a linear regression (°C per decade)
#' and a Mann-Kendall test (Kendall's tau between year and MTC) with its variance corrected for
#' autocorrelation (Yue and Wang 2004; `kendall_p_iid` is the p value of the usual test), as in
#' `peak_trend()`. The MTC is
#' compared with the SST of the area by their slopes and by the Pearson correlation of the two
#' series and of their year-to-year differences.
#'
#' These are simple descriptive statistics. Cheung et al. (2013) estimated the rate of change of
#' each large marine ecosystem with models that also included fishing effort, a large-scale ocean
#' index and autocorrelation, and related the rates of MTC and SST across ecosystems; regional
#' studies used lagged correlations, regressions and models (Tsikliras and Stergiou 2014; Liang
#' et al. 2018; Kamberi et al. 2025). Both series are autocorrelated: the Mann-Kendall test takes
#' this into account, but the p-values of the regression slope and of the correlations treat years
#' as independent and are too small (Yue et al. 2002; Pyper and Peterman 1998).
#' Two series that both rise over time are correlated even when they are not related; the
#' correlation of differences avoids this, but can miss a relation that acts only over decades
#' (Pyper and Peterman 1998). In the Adriatic Sea, the relation of MTC with SST was no longer
#' significant once autocorrelation was accounted for (Kamberi et al. 2025).
#'
#' @section Divisions:
#' With `by = "division"` and catch data by division (`load_fao_divisions()`, e.g. the GFCM
#' divisions of the Mediterranean and Black Sea; Tsikliras and Stergiou 2014 used GFCM data of the
#' western, central and eastern Mediterranean), the MTC
#' is calculated for each division and compared with the SST of the division
#' (`fao_area_sst(level = "division")`). Divisions without a boundary on the FAO map use the SST of
#' their subarea (e.g. 34.1.9 "Northern coastal, not known" uses 34.1, 51.3.1 Oman Sea uses 51.3);
#' the column `sst_unit` gives the unit used. Codes that are not places (e.g. 37.0.0 Tunas, 37.9.0
#' Not known) have no SST. On the 2° grid of the built-in ERSST v5, some divisions have only a few
#' cells (the function says which); a finer SST product, given as the path of its NetCDF files,
#' suits them better.
#'
#' @section Interpretation:
#' A change in the MTC can come from a warmer or cooler ocean, but also from changes in the
#' species fished, fishing grounds, gear, markets, management and reporting. For example, in the
#' Arabian Gulf the MTC fell because of export demand for largehead hairtail (Alajmi et al. 2024),
#' and in New Zealand fleets moving to deeper, colder waters lowered it (Lavin et al. 2023). Check
#' which items drive a change with the species table (`weight_pct`, `contribution_c`) before
#' attributing it to temperature. A preferred temperature describes the whole range of a species,
#' not the stock in the area.
#'
#' @inheritParams summary_params
#' @param version The FishBase and SeaLifeBase release, e.g. "26.06"; see `fishbase_match()`.
#'        Default is "latest".
#' @param min_coverage The minimum share of the catch (in percent) made up by items with a
#'        preferred temperature. Years below it are faded in the charts, marked in the table and
#'        left out of the trends and correlations. Default is 50, a convention of this package
#'        (see Coverage); use 0 to keep all years.
#' @param trend_from The first year of the second trend period. Default is 1970, the start of
#'        the period studied by Cheung et al. (2013).
#' @param use_match The kinds of match (`match_type` of `fishbase_match()`) whose preferred
#'        temperature is used. Default is exact names, synonyms, valid names found in WoRMS and
#'        manual values.
#' @param match The match table to use instead of running `fishbase_match()`: a data frame, or
#'        the path of a CSV file such as the species table of a previous run after checking it
#'        (see What the user must check). It needs the columns `species_en`, `match_type` and
#'        `temp_pref_mean`, and may have `status`, `checked` and `note`. Default is NULL.
#' @param sst The SST to compare with (see `fao_area_sst()`): "ersst5" (NOAA ERSST v5, built
#'        into the package), the name of a product downloaded with `download_sst_data()`, the path
#'        of your own NetCDF file or folder of NetCDF files, FALSE for none, or a data frame with
#'        the columns `area_code` (or `area_en`), `year` and `sst_mean` (and optionally `sst_p10`
#'        and `sst_p90`). Default is the `sst` setting of `fao_settings()`, "ersst5" unless
#'        changed. The SST data used is printed, shown in the chart legend and given in the
#'        column `sst_source` of the yearly table.
#' @param by "area" (default) for the FAO major areas, or "division" for the divisions of the
#'        catch data of `load_fao_divisions()` (see Divisions).
#' @param data_dir The directory where downloaded data is stored. Default is `fao_data_dir()`.
#' @return Invisibly, a list of three data frames:
#'   \describe{
#'     \item{yearly}{one row per area (or division) and year: MTC, coverage (percent), whether the
#'       coverage is below `min_coverage`, SST (mean and 10th-90th percentile range in the area),
#'       its product and the unit of the SST table used (`sst_unit`), number of items used, total
#'       catch and catch of the items used;}
#'     \item{trend}{one row per area and period: slopes of the MTC and SST (°C per decade), the
#'       Mann-Kendall test of the MTC, and the correlation of MTC and SST for the series and
#'       for their year-to-year differences;}
#'     \item{species}{one row per area and FAO species item: preferred temperature, catch, weight
#'       in and contribution (°C) to the MTC of the whole period, match, links, and the columns
#'       `checked` and `note` for the user's checks.}
#'   }
#'   Charts are saved in `figures/png/summary/`: `<label>_mtc.png` (MTC and SST with its range
#'   in the area; for a single area with the coverage below) and, for several areas,
#'   `<label>_mtc_coverage.png`. Tables are saved in `tables/`: `<label>_mtc.csv`,
#'   `<label>_mtc_trend.csv` and `<label>_mtc_species.csv`. With `by = "division"`, `_mtc` in the
#'   file names becomes `_mtc_divisions`.
#' @references
#' Alajmi, A., Ben-Hasan, A., Alrushaid, T., Vahabnezhad, A. and Pauly, D. (2024). Mean
#' temperature of the catch index can be masked by changes in catch composition unrelated to
#' ocean warming. Fisheries Management and Ecology 31: e12659. \doi{10.1111/fme.12659}
#'
#' Cheung, W.W.L., Watson, R. and Pauly, D. (2013). Signature of ocean warming in global
#' fisheries catch. Nature 497: 365-368. \doi{10.1038/nature12156}
#'
#' Dimarchopoulou, D., Makino, M., Prayoga, M.R., Zeller, D., Vianna, G.M.S. and Humphries, A.T.
#' (2022). Responses in fisheries catch data to a warming ocean along a latitudinal gradient in
#' the western Pacific Ocean. Environmental Biology of Fishes 105: 1347-1362.
#' \doi{10.1007/s10641-021-01162-z}
#'
#' FAO (2024). The State of World Fisheries and Aquaculture 2024. Rome: FAO. \doi{10.4060/cd0683en}
#'
#' Froese, R. (2020). Personal communication: R code (PrefTempBatch_5.R) to estimate preferred
#' temperature from AquaMaps (version 10/2019). FishBase reference 123201.
#' \url{https://www.fishbase.se/references/FBRefSummary.php?ID=123201}
#'
#' Garibaldi, L. (2012). The FAO global capture production database: a six-decade effort to catch
#' the trend. Marine Policy 36: 760-768. \doi{10.1016/j.marpol.2011.10.024}
#'
#' Grenié, M., Berti, E., Carvajal-Quintero, J., Dädlow, G.M.L., Sagouis, A. and Winter, M.
#' (2023). Harmonizing taxon names in biodiversity data: a review of tools, databases and best
#' practices. Methods in Ecology and Evolution 14: 12-25. \doi{10.1111/2041-210X.13802}
#'
#' Kamberi, E., Hala, E. and Möllmann, C. (2025). Impact of large-scale oceanic variability on
#' Adriatic fisheries evidenced through the 'mean temperature of the catch' approach. Frontiers
#' in Marine Science 12: 1719738. \doi{10.3389/fmars.2025.1719738}
#'
#' Lavin, C.P., Pauly, D., Dimarchopoulou, D., Liang, C. and Costello, M.J. (2023). Fishery catch
#' is affected by geographic expansion, fishing down food webs and climate change in Aotearoa, New
#' Zealand. PeerJ 11: e16070. \doi{10.7717/peerj.16070}
#'
#' Liang, C., Xian, W. and Pauly, D. (2018). Impacts of ocean warming on China's fisheries
#' catches: an application of "mean temperature of the catch" concept. Frontiers in Marine
#' Science 5: 26. \doi{10.3389/fmars.2018.00026}
#'
#' Pyper, B.J. and Peterman, R.M. (1998). Comparison of methods to account for autocorrelation in
#' correlation analyses of fish data. Canadian Journal of Fisheries and Aquatic Sciences 55:
#' 2127-2140. \doi{10.1139/f98-104}
#'
#' Tsikliras, A.C. and Stergiou, K.I. (2014). Mean temperature of the catch increases quickly in
#' the Mediterranean Sea. Marine Ecology Progress Series 515: 281-284. \doi{10.3354/meps11005}
#'
#' Tsikliras, A.C., Peristeraki, P., Tserpes, G. and Stergiou, K.I. (2015). Mean temperature of
#' the catch (MTC) in the Greek Seas based on landings and survey data. Frontiers in Marine
#' Science 2: 23. \doi{10.3389/fmars.2015.00023}
#'
#' Yue, S. and Wang, C.Y. (2004). The Mann-Kendall test modified by effective sample size to
#' detect trend in serially correlated hydrological series. Water Resources Management 18:
#' 201-218. \doi{10.1023/B:WARM.0000043140.61082.60}
#'
#' Yue, S., Pilon, P., Phinney, B. and Cavadias, G. (2002). The influence of autocorrelation on
#' the ability to detect trend in hydrological series. Hydrological Processes 16: 1807-1829.
#' \doi{10.1002/hyp.1095}
#'
#' Froese, R. and Pauly, D. (Eds.) FishBase. \url{https://www.fishbase.se}; Palomares, M.L.D. and
#' Pauly, D. (Eds.) SeaLifeBase. \url{https://www.sealifebase.se}; WoRMS Editorial Board. World
#' Register of Marine Species. \url{https://www.marinespecies.org}
#' @export
#' @examples
#' \dontrun{
#' FAO27 <- fishing_area_data("Atlantic, Northeast", plot = FALSE)
#' mtc <- mean_temperature_catch(FAO27$area_data, lang = "both")
#' mtc$trend
#' # The items with the largest catch: check their match and preferred temperature with the links
#' head(mtc$species[, c("species_name", "match_type", "temp_pref_mean", "catch_pct", "used", "url")], 20)
#'
#' # After checking tables/Atlantic__Eastern_Central_mtc_species.csv (column `checked`, and
#' # corrected values with their source in `note`), run again with the checked table
#' mean_temperature_catch(FAO27$area_data, match = "tables/Atlantic__Northeast_mtc_species.csv")
#'
#' # Other SST data: your own NetCDF file (or folder), or a product downloaded once
#' mean_temperature_catch(FAO27$area_data, sst = "D:/data/HadISST_sst.nc", label = "FAO27_hadisst")
#' download_sst_data("cobe2")
#' mean_temperature_catch(FAO27$area_data, sst = "cobe2", label = "FAO27_cobe2")
#'
#' # The GFCM divisions of the Mediterranean and Black Sea
#' mean_temperature_catch(load_fao_divisions("GFCM"), by = "division")
#'
#' # The period studied by Cheung et al. (2013)
#' mean_temperature_catch(subset(FAO27$area_data, Year >= 1970 & Year <= 2006),
#'                        label = "FAO27_1970_2006")
#'
#' # All FAO major marine fishing areas, one chart panel per area
#' marine <- subset(load_fao_data(), FAO.major.fishing.area..Name. %in% fao_area_sst()$area_en)
#' mean_temperature_catch(marine, label = "marine_areas", lang = "zh")
#' }
mean_temperature_catch <- function(area_data, version = "latest", min_coverage = 50, trend_from = 1970,
                                   use_match = c("exact", "synonym", "WoRMS valid name (exact)",
                                                 "WoRMS valid name (synonym)", "manual"),
                                   match = NULL, sst = fao_setting("sst"), by = c("area", "division"),
                                   plot = TRUE, table = TRUE, lang = NULL,
                                   pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), label = NULL,
                                   data_dir = fao_data_dir()) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  by <- match.arg(by)
  lang <- fao_lang(lang)
  name <- fao_summary_name(fao_summary_label(area_data, label), if (by == "division") "mtc_divisions" else "mtc")
  # The spatial units: FAO major areas, or divisions (catch data of load_fao_divisions())
  units <- fao_spatial_units(area_data, by)
  matched <- if (is.null(match)) {
    fishbase_match(area_data, version = version, table = FALSE, lang = "en", label = label, data_dir = data_dir)
  } else fao_mtc_match_table(match)

  catch <- stats::aggregate(list(Tonnes = area_data$Production),
                            by = list(area_en = units$name, species_en = area_data$ASFIS.species..Name.,
                                      Year = area_data$Year),
                            FUN = sum, na.rm = TRUE)
  unit_codes <- stats::setNames(units$code, units$name)
  row <- match(catch$species_en, matched$species_en)
  catch$temp <- matched$temp_pref_mean[row]
  matched_ok <- if ("status" %in% names(matched)) matched$status %in% "matched" else rep(TRUE, nrow(matched))
  catch$used <- !is.na(row) & matched$match_type[row] %in% use_match & matched_ok[row] & !is.na(catch$temp)
  # Items whose match the user has marked as checked (the `checked` column of a checked table)
  catch$checked <- catch$used & !is.na(row) & fao_mtc_is_checked(matched$checked, nrow(matched))[row]

  areas <- if (by == "area") names(sort(tapply(catch$Tonnes, catch$area_en, sum), decreasing = TRUE)) else
    unique(catch$area_en)[order(unit_codes[unique(catch$area_en)])]
  years <- seq(min(catch$Year), max(catch$Year))
  yearly <- do.call(rbind, lapply(areas, function(a) fao_mtc_yearly(catch[catch$area_en == a, ], years)))
  yearly$area_code <- unname(unit_codes[yearly$area_en])
  yearly$low_coverage <- is.na(yearly$coverage_pct) | yearly$coverage_pct < min_coverage
  sst_table <- fao_mtc_sst(sst, data_dir, if (by == "division") "division" else "area")
  sst_name <- if (is.null(sst_table)) NULL else if ("source" %in% names(sst_table)) sst_table$source[1] else "user table"
  yearly$sst_source <- if (is.null(sst_name)) NA_character_ else sst_name
  # The unit of the SST table used: the area or division itself, else (divisions without a boundary
  # on the FAO map) its subarea; NA for codes that are not places (e.g. 37.0.0 Tunas)
  yearly$sst_unit <- if (is.null(sst_table) || !"area_code" %in% names(sst_table)) NA_character_ else
    if (by == "division") fao_division_sst_unit(yearly$area_code, unique(sst_table$area_code)) else yearly$area_code
  sst_row <- if (is.null(sst_table)) rep(NA_integer_, nrow(yearly)) else if ("area_code" %in% names(sst_table)) {
    match(paste(yearly$sst_unit, yearly$year), paste(sst_table$area_code, sst_table$year))
  } else {
    # tables without codes: division names in the catch data start with the code ("37.1.1 Balearic")
    names_en <- if (by == "division") sub("^\\S+ ", "", yearly$area_en) else yearly$area_en
    match(paste(names_en, yearly$year), paste(sst_table$area_en, sst_table$year))
  }
  if (!is.null(sst_table) && all(is.na(sst_row))) {
    message("No SST matches the ", if (by == "division") "divisions" else "areas", " of the data; give an SST table ",
            "with the column area_code (or area_en) of the ", if (by == "division") "divisions" else "areas", ".")
  }
  if (by == "division" && !is.null(sst_table) && "n_cells" %in% names(sst_table)) {
    cells <- sst_table$n_cells[match(yearly$sst_unit, sst_table$area_code)]
    few <- unique(yearly$area_en[!is.na(cells) & cells < 5])
    if (length(few) > 0) {
      message("The SST of ", paste(few, collapse = ", "), " rests on fewer than 5 grid cells of ", sst_name,
              "; a finer SST product suits small divisions better (see ?fao_area_sst).")
    }
  }
  for (column in c("sst_mean", "sst_p10", "sst_p90")) {
    yearly[[column]] <- if (column %in% names(sst_table)) as.numeric(sst_table[[column]][sst_row]) else NA_real_
  }

  trend <- do.call(rbind, lapply(areas, function(a) fao_mtc_trend(yearly[yearly$area_en == a, ], trend_from)))
  species <- do.call(rbind, lapply(areas, function(a) fao_mtc_species(catch[catch$area_en == a, ], matched, use_match)))

  area_names <- stats::setNames(if (by == "area") fao_translate(areas, "area", lang) else areas, areas)
  yearly$area <- unname(area_names[yearly$area_en])
  yearly <- yearly[, c("area", "area_en", "area_code", "year", "mtc", "coverage_pct", "low_coverage",
                       "sst_mean", "sst_p10", "sst_p90", "sst_source", "sst_unit", "n_items", "catch_tonnes",
                       "catch_used_tonnes")]
  trend <- cbind(area = unname(area_names[trend$area_en]), trend)
  species <- cbind(area = unname(area_names[species$area_en]),
                   species_name = fao_translate(species$species_en, "species", lang), species)
  rownames(yearly) <- rownames(trend) <- rownames(species) <- NULL

  checked_pct <- vapply(areas, function(a) {
    x <- catch[catch$area_en == a & catch$used, ]
    if (sum(x$Tonnes) > 0) 100 * sum(x$Tonnes[x$checked]) / sum(x$Tonnes) else 0
  }, numeric(1))
  for (a in areas) {
    x <- yearly[yearly$area_en == a & !is.na(yearly$coverage_pct), ]
    t <- trend[trend$area_en == a, ]
    t <- t[nrow(t), ]
    message(a, ": ", round(100 * sum(x$catch_used_tonnes) / sum(x$catch_tonnes)), "% of the catch ",
            min(x$year), "-", max(x$year), " has a preferred temperature (", round(min(x$coverage_pct)), "-",
            round(max(x$coverage_pct)), "% by year), of which ", round(checked_pct[[a]]), "% checked by the user; ",
            "MTC trend ", t$period_start, "-", t$period_end, ": ",
            if (is.na(t$mtc_slope_per_decade)) "not enough years" else
              paste0(sprintf("%+.2f", t$mtc_slope_per_decade), " \u00b0C per decade"))
  }
  if (any(checked_pct < 100)) {
    message("The preferred temperatures are automatic first matches, not checked values. Check each item ",
            "with the links of the species table", if (table) paste0(" (tables/", name, "_species.csv)"),
            ", mark it in the column 'checked', correct wrong values, and run the function again with ",
            "match = <the checked table>. See ?mean_temperature_catch.")
  }

  if (plot) fao_mtc_plots(yearly, min_coverage, name, lang, pptx, sst_name = sst_name, checked = all(checked_pct >= 100))
  if (table) {
    fao_save_table(yearly, name)
    fao_save_table(trend, paste0(name, "_trend"))
    fao_save_table(species, paste0(name, "_species"))
  }
  fao_run_result(list(yearly = yearly, trend = trend, species = species), run, fao_run_title("mean_temperature_catch", area_data, label),
                 main = fao_cols(trend, c("area", "period_start", "period_end", "mtc_slope_per_decade", "trend",
                                          "sst_slope_per_decade", "cor_mtc_sst")),
                 note = "Trend of the mean temperature of the catch (MTC, degrees C per decade):")
}


# MTC, coverage and number of items used of one area in each year. `x` is the yearly catch of
# each item in the area, with its preferred temperature `temp` and whether it is `used`.
fao_mtc_yearly <- function(x, years) {
  by_year <- function(values) {
    out <- tapply(values, factor(x$Year, levels = years), sum)
    out[is.na(out)] <- 0
    as.numeric(out)
  }
  total <- by_year(x$Tonnes)
  used <- by_year(ifelse(x$used, x$Tonnes, 0))
  weighted <- by_year(ifelse(x$used, x$Tonnes * x$temp, 0))
  data.frame(area_en = x$area_en[1], year = years,
             mtc = ifelse(used > 0, weighted / used, NA_real_),
             coverage_pct = ifelse(total > 0, 100 * used / total, NA_real_),
             n_items = by_year(x$used & x$Tonnes > 0),
             catch_tonnes = total, catch_used_tonnes = used, stringsAsFactors = FALSE)
}

# The match table given by the user: a data frame, or the path of a CSV file saved by
# fishbase_match() or mean_temperature_catch() (e.g. after checking it). Items listed more than
# once (a species table of several areas) keep a row marked as checked, else their first row.
fao_mtc_match_table <- function(match) {
  if (is.character(match) && length(match) == 1) {
    if (!file.exists(match)) stop("Match table not found: ", match, call. = FALSE)
    match <- fao_read_csv_utf8(match, colClasses = "character")
  }
  if (!is.data.frame(match)) stop("match must be a data frame or the path of a CSV file.", call. = FALSE)
  needed <- setdiff(c("species_en", "match_type", "temp_pref_mean"), names(match))
  if (length(needed) > 0) {
    stop("match needs the columns of fishbase_match(); missing: ", paste(needed, collapse = ", "), call. = FALSE)
  }
  match$temp_pref_mean <- suppressWarnings(as.numeric(match$temp_pref_mean))
  match <- match[order(!fao_mtc_is_checked(match$checked, nrow(match))), ]
  match[!duplicated(match$species_en), ]
}

# TRUE for items marked in the `checked` column (any text); all FALSE when there is no such column
fao_mtc_is_checked <- function(checked, n) {
  if (is.null(checked)) return(rep(FALSE, n))
  !is.na(checked) & trimws(as.character(checked)) != ""
}

# SST to compare with: a product (see fao_sst_products()), none, or a table given by the user
fao_mtc_sst <- function(sst, data_dir, level = "area") {
  if (isFALSE(sst)) return(NULL)
  if (is.null(sst)) sst <- "ersst5"
  if (is.character(sst)) return(fao_area_sst(sst = sst, level = level, data_dir = data_dir))
  if (!is.data.frame(sst) || !all(c("year", "sst_mean") %in% names(sst)) ||
      !any(c("area_code", "area_en") %in% names(sst))) {
    stop("sst must be \"ersst5\" (built in), a product downloaded with download_sst_data(), the path of ",
         "NetCDF files, FALSE, or a data frame with the columns area_code (or area_en), year and sst_mean.",
         call. = FALSE)
  }
  if ("area_code" %in% names(sst)) sst$area_code <- as.character(sst$area_code)
  sst
}

# Trends of one area over all years and from `trend_from`: slopes of MTC and SST, Mann-Kendall
# test of the MTC, and the correlation of MTC and SST (series and year-to-year differences).
# Only years whose coverage is at least min_coverage are used.
fao_mtc_trend <- function(x, trend_from) {
  x <- x[order(x$year), ]
  ok <- !x$low_coverage & !is.na(x$mtc)
  first <- if (any(ok)) min(x$year[ok]) else min(x$year)
  last <- max(x$year)
  periods <- list(c(first, last))
  if (trend_from > first && trend_from < last) periods[[2]] <- c(trend_from, last)

  slope <- function(y, year) if (sum(!is.na(y)) < 3) NA_real_ else 10 * unname(stats::coef(stats::lm(y ~ year))[2])
  test <- function(a, b, method) {
    keep <- !is.na(a) & !is.na(b)
    if (sum(keep) < 3) return(c(estimate = NA_real_, p = NA_real_))
    t <- suppressWarnings(stats::cor.test(a[keep], b[keep], method = method, exact = FALSE))
    c(estimate = unname(t$estimate), p = t$p.value)
  }
  do.call(rbind, lapply(periods, function(p) {
    sel <- x$year >= p[1] & x$year <= p[2]
    mtc <- ifelse(ok, x$mtc, NA)[sel]
    sst <- ifelse(ok, x$sst_mean, NA)[sel]
    year <- x$year[sel]
    # A constant series gives an "essentially perfect fit" warning; its slope is 0
    fit <- if (sum(!is.na(mtc)) >= 3) suppressWarnings(summary(stats::lm(mtc ~ year)))$coefficients else NULL
    kendall <- fao_mann_kendall(year, mtc)
    series <- test(mtc, sst, "pearson")
    differences <- test(diff(mtc), diff(sst), "pearson")
    data.frame(
      area_en = x$area_en[1], period_start = p[1], period_end = p[2], n_years = sum(!is.na(mtc)),
      mtc_slope_per_decade = if (is.null(fit)) NA_real_ else 10 * fit["year", "Estimate"],
      mtc_slope_p = if (is.null(fit)) NA_real_ else fit["year", "Pr(>|t|)"],
      kendall_tau = kendall[["tau"]], kendall_p = kendall[["p"]], kendall_p_iid = kendall[["p_iid"]],
      trend = ifelse(!is.na(kendall[["p"]]) & kendall[["p"]] < 0.05,
                     ifelse(kendall[["tau"]] > 0, "up", "down"), "none"),
      sst_slope_per_decade = slope(ifelse(is.na(mtc), NA, sst), year),
      cor_mtc_sst = series[["estimate"]], cor_mtc_sst_p = series[["p"]],
      cor_differences = differences[["estimate"]], cor_differences_p = differences[["p"]],
      stringsAsFactors = FALSE
    )
  }))
}

# Items of one area with their catch over the whole period, preferred temperature, weight in and
# contribution to the MTC of the period, match and links, sorted by catch
fao_mtc_species <- function(x, matched, use_match) {
  catch <- tapply(x$Tonnes, x$species_en, sum)
  catch <- sort(catch, decreasing = TRUE)
  out <- data.frame(area_en = x$area_en[1], species_en = names(catch), stringsAsFactors = FALSE)
  row <- match(out$species_en, matched$species_en)
  info <- c("alpha3", "fao_scientific_name", "taxon_level", "major_group", "database", "match_type",
            "matched_name", "SpecCode", "temp_pref_mean")
  for (column in info) out[[column]] <- if (column %in% names(matched)) matched[[column]][row] else NA
  out$used <- x$used[match(out$species_en, x$species_en)]
  out$reason <- ifelse(out$used, "",
                       ifelse(is.na(row), "not in the match table",
                              ifelse(!out$match_type %in% use_match,
                                     paste0("match type \"", out$match_type, "\" not used"),
                                     "no preferred temperature in FishBase or SeaLifeBase")))
  out$catch_tonnes <- as.numeric(catch)
  out$catch_pct <- 100 * out$catch_tonnes / sum(out$catch_tonnes)
  out$cumulative_pct <- cumsum(out$catch_pct)
  used_catch <- sum(out$catch_tonnes[out$used])
  out$weight_pct <- ifelse(out$used & used_catch > 0, 100 * out$catch_tonnes / used_catch, NA_real_)
  out$contribution_c <- out$weight_pct / 100 * ifelse(out$used, out$temp_pref_mean, NA_real_)
  column <- function(name) if (name %in% names(matched)) matched[[name]][row] else rep(NA_character_, nrow(out))
  out$url <- column("url")
  out$search_url <- column("search_url")
  # The WoRMS record found by fishbase_match(), else a WoRMS search for the FAO name
  out$worms_url <- ifelse(is.na(column("worms_url")), fao_worms_url(out$fao_scientific_name), column("worms_url"))
  out$rule_source <- column("rule_source")
  out$match_note <- column("match_note")
  out$source <- column("source")
  out$status <- column("status")
  # For the user's checks: whether the match and preferred temperature were checked, and how
  out$checked <- ifelse(is.na(column("checked")), "", column("checked"))
  out$note <- ifelse(is.na(column("note")), "", column("note"))
  out
}

# Search of the World Register of Marine Species for an FAO scientific name: the species name,
# or the first name of a group ("Sardinella spp" -> "Sardinella", "Sepiidae, Sepiolidae" ->
# "Sepiidae"). Names in brackets (subgenera, former names) are left out.
fao_worms_url <- function(scientific_name) {
  vapply(scientific_name, function(s) {
    if (is.na(s) || trimws(s) == "") return(NA_character_)
    first <- strsplit(gsub("\\([^)]*\\)", " ", s), ",|;| or ")[[1]][1]
    words <- strsplit(trimws(gsub("\\s+", " ", first)), " ")[[1]]
    is_species <- length(words) >= 2 && grepl("^[a-z]", words[2]) && !words[2] %in% c("spp", "spp.", "sp", "sp.", "nei")
    name <- if (is_species) paste(words[1], words[2]) else words[1]
    paste0("https://www.marinespecies.org/aphia.php?p=taxlist&tName=", utils::URLencode(name, reserved = TRUE))
  }, character(1), USE.NAMES = FALSE)
}

# Charts: MTC and SST (with the SST range within the area), and the coverage (below them for a
# single area, in a second chart for several areas). Years with a coverage below min_coverage
# are faded. `checked` tells whether all preferred temperatures used were checked by the user.
fao_mtc_plots <- function(yearly, min_coverage, name, lang, pptx, sst_name = NULL, checked = FALSE) {
  areas <- unique(yearly$area)
  yearly$area <- factor(yearly$area, levels = areas)
  sst_label <- fao_label("sst", lang)
  range_label <- fao_label("sst_range", lang)
  if (!is.null(sst_name)) sst_label <- paste0(sst_label, ": ", sst_name)
  series <- c(fao_label("mtc", lang), sst_label)
  temperature <- rbind(
    data.frame(area = yearly$area, Year = yearly$year, value = yearly$mtc, series = series[1],
               faded = yearly$low_coverage, stringsAsFactors = FALSE),
    data.frame(area = yearly$area, Year = yearly$year, value = yearly$sst_mean, series = series[2],
               faded = FALSE, stringsAsFactors = FALSE)
  )
  temperature$series <- factor(temperature$series, levels = series)
  # Lines join only the years shown in full
  temperature$line <- ifelse(temperature$faded, NA, temperature$value)
  sst_range <- data.frame(area = yearly$area, Year = yearly$year, low = yearly$sst_p10, high = yearly$sst_p90,
                          range = range_label)
  sst_range <- sst_range[!is.na(sst_range$low) & !is.na(sst_range$high), ]
  coverage <- data.frame(area = yearly$area, Year = yearly$year, value = yearly$coverage_pct,
                         faded = yearly$low_coverage)
  breaks <- fao_year_breaks(yearly$year, by = if (length(areas) == 1) 10 else 20)
  colors <- stats::setNames(c("#D7191C", "#2C7BB6"), series)
  note <- fao_label("mtc_faded_note", lang, n = min_coverage)
  if (!checked) note <- paste0(note, "\n", fao_label("mtc_unchecked_note", lang))

  temperature_layers <- function(data, range) list(
    if (nrow(range) > 0) {
      ggplot2::geom_ribbon(data = range, ggplot2::aes(ymin = .data$low, ymax = .data$high, fill = .data$range),
                           alpha = 0.15)
    },
    ggplot2::geom_line(data = data, ggplot2::aes(y = .data$line, colour = .data$series), linewidth = 0.6, na.rm = TRUE),
    ggplot2::geom_point(data = data, ggplot2::aes(y = value, colour = .data$series, alpha = .data$faded),
                        size = 1.2, na.rm = TRUE),
    ggplot2::scale_colour_manual(values = colors, drop = FALSE),
    ggplot2::scale_fill_manual(values = stats::setNames("#2C7BB6", range_label)),
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.25), guide = "none")
  )
  coverage_layers <- function(data) {
    # The threshold line goes in the coverage panel only (all panels of the coverage chart)
    threshold <- data.frame(y = min_coverage)
    if ("panel" %in% names(data)) threshold$panel <- data$panel[1]
    list(
      ggplot2::geom_col(data = data[!data$faded, ], ggplot2::aes(y = value), fill = "grey55", width = 0.8, na.rm = TRUE),
      ggplot2::geom_col(data = data[data$faded, ], ggplot2::aes(y = value), fill = "grey85", width = 0.8, na.rm = TRUE),
      ggplot2::geom_hline(data = threshold, ggplot2::aes(yintercept = .data$y), linetype = "dashed",
                          colour = "grey30", linewidth = 0.4)
    )
  }
  x_scale <- ggplot2::scale_x_continuous(breaks = breaks, expand = c(0.01, 0))
  # The two legends one above the other, so that long bilingual labels fit
  legends <- list(ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2)),
                  ggplot2::theme(legend.box = "vertical", legend.spacing.y = ggplot2::unit(0, "pt")))

  if (length(areas) == 1) {
    panels <- c(fao_label("temperature_c", lang), fao_label("mtc_coverage", lang))
    temperature$panel <- factor(panels[1], levels = panels)
    sst_range$panel <- factor(rep(panels[1], nrow(sst_range)), levels = panels)
    coverage$panel <- factor(panels[2], levels = panels)
    p <- ggplot2::ggplot(mapping = ggplot2::aes(x = Year)) +
      coverage_layers(coverage) + temperature_layers(temperature, sst_range) +
      ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
      x_scale +
      ggplot2::labs(x = fao_label("year", lang), y = NULL, colour = NULL, fill = NULL, caption = note) +
      fao_theme() + legends
    fao_save_plot(p, file.path("summary", name), width = 12, height = 8, pptx = pptx)
    return(invisible(p))
  }

  ncol <- min(4, length(areas))
  nrow <- ceiling(length(areas) / ncol)
  # Room between panels, so the last year of one panel and the first of the next stay apart
  spacing <- ggplot2::theme(panel.spacing.x = ggplot2::unit(1.5, "lines"))
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = Year)) +
    temperature_layers(temperature, sst_range) +
    ggplot2::facet_wrap(~area, ncol = ncol, scales = "free_y") +
    x_scale +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label("temperature_c", lang), colour = NULL, fill = NULL,
                  caption = note) +
    fao_theme() + legends +
    spacing
  fao_save_plot(p, file.path("summary", name), width = 4 * ncol, height = 2.6 * nrow + 1.4, pptx = pptx)
  p_coverage <- ggplot2::ggplot(mapping = ggplot2::aes(x = Year)) +
    coverage_layers(coverage) +
    ggplot2::facet_wrap(~area, ncol = ncol) +
    x_scale +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::labs(x = fao_label("year", lang), y = fao_label("mtc_coverage", lang)) +
    fao_theme() +
    spacing
  fao_save_plot(p_coverage, file.path("summary", paste0(name, "_coverage")), width = 4 * ncol,
                height = 2.4 * nrow + 1, pptx = pptx)
  invisible(list(temperature = p, coverage = p_coverage))
}
