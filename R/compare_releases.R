#' Compare Two FAO Data Releases
#'
#' This function compares two releases of the FAO Global Capture Production data record by record
#' (country, species item, fishing area and year) and lists the historical data that were
#' revised between them: records whose catch changed, records added for years the older release
#' already covered, and records removed. It summarises the revisions by year and by country and
#' shows how the total catch of each year changed.
#'
#' FAO revises earlier years when countries send corrected or more complete data (Garibaldi 2012).
#' Zeller and Pauly (2018) call "presentist bias" the practice of applying improvements in
#' reporting to recent years only, without revising the earlier years, which makes the time series
#' show an increase that did not happen. Comparing releases shows which years and countries were
#' revised and how far back the revisions reach. No published study has compared successive
#' FishStat releases record by record in this way; the comparison is a tool of this package, and
#' the two papers above give the background.
#'
#' Releases are compared on the names used by the package (matched through FAO codes when a
#' release is downloaded). A record removed under one species item and added under another with
#' the same country, area, year and catch (e.g. when FAO renames or splits a species item) is
#' marked "reclassified" and is not counted as a revision of the catch. Download older releases
#' first with `download_fao_data(version = ...)`; `check_fao_update()` lists them.
#'
#' @inheritParams summary_params
#' @param old,new The releases to compare, e.g. "2025.1.0" and "2026.1.0", or "bundled" for the
#'        data bundled with the package. Default is the newest release available on this computer
#'        for `new`, and the one before it for `old`.
#' @param area_name,country_name Optional FAO fishing areas and countries to compare. Default is
#'        NULL, all.
#' @param tolerance Changes of at most this many tonnes are ignored. Default is 0.5 (rounding).
#' @param top_n The number of countries with the largest revisions in the country chart. Default is 10.
#' @param data_dir The directory where downloaded releases are stored. Default is `fao_data_dir()`.
#' @return Invisibly, a list with `by_year` (catch in both releases and the revisions of each
#'         year), `by_country` (revisions by country, without reclassified records), `records`
#'         (every revised, added, removed or reclassified record) and `releases` (the two releases
#'         compared).
#' @references
#' Garibaldi, L. (2012). The FAO global capture production database: a six-decade effort to catch
#' the trend. Marine Policy 36: 760-768. \doi{10.1016/j.marpol.2011.10.024}
#'
#' Zeller, D. and Pauly, D. (2018). The 'presentist bias' in time-series data: implications for
#' fisheries science and policy. Marine Policy 90: 14-19. \doi{10.1016/j.marpol.2018.01.015}
#' @export
#' @examples
#' \dontrun{
#' download_fao_data(version = "2025.1.0")
#' compare_releases("2025.1.0", "2026.1.0", lang = "zh")
#' compare_releases("2025.1.0", "2026.1.0", country_name = "China")
#' }
compare_releases <- function(old = NULL, new = NULL, area_name = NULL, country_name = NULL, tolerance = 0.5,
                             top_n = 10, plot = TRUE, table = TRUE, lang = NULL,
                             pptx = getOption("faofishingreport.pptx", FALSE), plot_show = fao_setting("plot_show"), label = NULL,
                             data_dir = fao_data_dir()) {
  run <- fao_run_begin(plot_show)
  on.exit(fao_run_end(run), add = TRUE)
  lang <- fao_lang(lang)
  available <- unique(c(fao_bundled_release(), fao_local_versions(data_dir)))
  available <- available[order(numeric_version(available))]
  if (is.null(new)) new <- available[length(available)]
  if (is.null(old)) {
    earlier <- available[numeric_version(available) < numeric_version(if (new == "bundled") fao_bundled_release() else new)]
    if (length(earlier) == 0) {
      stop("Only one FAO release is available on this computer (", paste(available, collapse = ", "),
           "). Download an earlier one with download_fao_data(version = ...); check_fao_update() lists them.",
           call. = FALSE)
    }
    old <- earlier[length(earlier)]
  }
  old_data <- load_fao_data(old, data_dir)
  new_data <- load_fao_data(new, data_dir)
  pick <- function(d) {
    keep <- rep(TRUE, nrow(d))
    if (!is.null(area_name)) keep <- keep & d$FAO.major.fishing.area..Name. %in% area_name
    if (!is.null(country_name)) keep <- keep & d$Country..Name. %in% country_name
    d[keep, ]
  }
  old_data <- pick(old_data)
  new_data <- pick(new_data)
  if (nrow(old_data) == 0 || nrow(new_data) == 0) stop("No data for the chosen areas and countries.", call. = FALSE)

  old_label <- if (old == "bundled") fao_bundled_release() else old
  new_label <- if (new == "bundled") fao_bundled_release() else new
  if (is.null(label)) label <- paste(c(area_name, country_name), collapse = "_")
  name <- paste0(fao_file_name(if (label == "") "fao" else label), "_releases_", fao_file_name(old_label), "_vs_",
                 fao_file_name(new_label))

  comparison <- fao_compare_data(old_data, new_data, tolerance)
  records <- comparison$records
  by_year <- comparison$by_year
  changed <- records[records$change %in% c("revised", "added", "removed", "reclassified"), ]
  revisions <- changed[changed$change != "reclassified", ]
  by_country <- if (nrow(revisions) == 0) {
    data.frame(country_en = character(0), net_change_tonnes = numeric(0), abs_change_tonnes = numeric(0),
               n_records = integer(0), first_year = integer(0), last_year = integer(0))
  } else {
    do.call(rbind, lapply(split(revisions, revisions$country), function(x) {
      data.frame(country_en = x$country[1], net_change_tonnes = sum(x$difference),
                 abs_change_tonnes = sum(abs(x$difference)), n_records = nrow(x),
                 first_year = min(x$Year), last_year = max(x$Year), stringsAsFactors = FALSE)
    }))
  }
  by_country <- by_country[order(-by_country$abs_change_tonnes), ]
  by_country <- cbind(country = fao_translate(by_country$country_en, "country", lang), by_country)
  rownames(by_country) <- NULL
  changed <- cbind(country_name = fao_translate(changed$country, "country", lang),
                   species_name = fao_translate(changed$species, "species", lang), changed)

  n <- base::table(factor(records$change, levels = c("revised", "added", "removed", "reclassified", "new year", "unchanged")))
  message("Release ", old_label, " vs ", new_label, ": ", paste(names(n), n, sep = " ", collapse = ", "), " records.")

  if (plot) {
    release_labels <- c(fao_label("release", lang, n = old_label), fao_label("release", lang, n = new_label))
    totals <- rbind(
      data.frame(Year = by_year$Year, tonnes = by_year$old_tonnes, release = release_labels[1]),
      data.frame(Year = comparison$new_totals$Year, tonnes = comparison$new_totals$tonnes, release = release_labels[2])
    )
    totals <- totals[!is.na(totals$tonnes), ]
    totals$release <- factor(totals$release, levels = release_labels)
    p_total <- ggplot2::ggplot(totals, ggplot2::aes(x = Year, y = tonnes / 1e6, colour = release, linetype = release)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_colour_manual(values = stats::setNames(c("grey45", "#D7191C"), release_labels)) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(totals$Year)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("million_tonnes", lang), colour = NULL, linetype = NULL) +
      fao_theme()
    fao_save_plot(p_total, file.path("summary", name), width = 12, height = 6, pptx = pptx)

    by_year$direction <- ifelse(by_year$change_tonnes >= 0, "up", "down")
    p_change <- ggplot2::ggplot(by_year, ggplot2::aes(x = Year, y = change_tonnes / 1e3, fill = direction)) +
      ggplot2::geom_col(width = 0.85, show.legend = FALSE) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
      ggplot2::scale_fill_manual(values = c(up = "#1A9641", down = "#D7191C")) +
      ggplot2::scale_x_continuous(breaks = fao_year_breaks(by_year$Year)) +
      ggplot2::labs(x = fao_label("year", lang), y = fao_label("revision_kt", lang),
                    caption = fao_label("revision_note", lang, n = paste(new_label, "-", old_label))) +
      fao_theme()
    fao_save_plot(p_change, file.path("summary", paste0(name, "_by_year")), width = 12, height = 6, pptx = pptx)

    if (nrow(by_country) > 0) {
      top <- utils::head(by_country, top_n)
      top$country <- factor(top$country, levels = rev(top$country))
      top$direction <- ifelse(top$net_change_tonnes >= 0, "up", "down")
      p_country <- ggplot2::ggplot(top, ggplot2::aes(x = net_change_tonnes / 1e3, y = country, fill = direction)) +
        ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
        ggplot2::geom_vline(xintercept = 0, colour = "grey30") +
        ggplot2::scale_fill_manual(values = c(up = "#1A9641", down = "#D7191C")) +
        ggplot2::labs(x = fao_label("revision_kt", lang), y = NULL) +
        fao_theme()
      fao_save_plot(p_country, file.path("summary", paste0(name, "_countries")), width = 10,
                    height = max(4, 0.45 * nrow(top) + 2), pptx = pptx)
    }
  }
  if (table) {
    fao_save_table(by_year[, setdiff(names(by_year), "direction")], paste0(name, "_by_year"))
    fao_save_table(by_country, paste0(name, "_countries"))
    fao_save_table(changed, paste0(name, "_records"))
  }
  fao_run_result(list(by_year = by_year[, setdiff(names(by_year), "direction")], by_country = by_country,
                      records = changed, releases = c(old = old_label, new = new_label)), run,
                 fao_run_title("compare_releases", what = c(area_name, country_name)),
                 main = by_year[, setdiff(names(by_year), "direction")],
                 note = paste0("Catch in release ", new_label, " against ", old_label, ", last years:"), from = "bottom")
}

# Record-by-record comparison of two data frames in the package format
fao_compare_data <- function(old_data, new_data, tolerance = 0.5) {
  collapse <- function(d) {
    out <- stats::aggregate(list(tonnes = d$Production),
                            by = list(country = d$Country..Name., species = d$ASFIS.species..Name.,
                                      area = d$FAO.major.fishing.area..Name., Year = d$Year),
                            FUN = sum, na.rm = TRUE)
    status <- stats::aggregate(list(status = d$Production.Type),
                               by = list(country = d$Country..Name., species = d$ASFIS.species..Name.,
                                         area = d$FAO.major.fishing.area..Name., Year = d$Year),
                               FUN = function(x) paste(sort(unique(x)), collapse = "/"))
    merge(out, status, by = c("country", "species", "area", "Year"))
  }
  old <- collapse(old_data)
  new <- collapse(new_data)
  last_old <- max(old$Year)
  records <- merge(old, new, by = c("country", "species", "area", "Year"), all = TRUE, suffixes = c("_old", "_new"))
  in_old <- !is.na(records$tonnes_old)
  in_new <- !is.na(records$tonnes_new)
  records$difference <- ifelse(in_old & in_new, records$tonnes_new - records$tonnes_old,
                               ifelse(in_new, records$tonnes_new, -records$tonnes_old))
  records$change <- ifelse(in_old & in_new,
                           ifelse(abs(records$difference) > tolerance, "revised", "unchanged"),
                           ifelse(in_new, ifelse(records$Year > last_old, "new year", "added"), "removed"))
  records$change[records$change %in% c("added", "removed") & abs(records$difference) <= tolerance] <- "unchanged"
  # A record removed under one species and added under another with the same country, area, year
  # and catch is a change of species item (e.g. a renamed or split species), not a revision
  moved <- which(records$change %in% c("added", "removed"))
  key <- paste(records$country[moved], records$area[moved], records$Year[moved], round(abs(records$difference[moved]), 1))
  for (rows in split(moved, key)) {
    added <- rows[records$change[rows] == "added"]
    removed <- rows[records$change[rows] == "removed"]
    n_pairs <- min(length(added), length(removed))
    if (n_pairs > 0) records$change[c(added[seq_len(n_pairs)], removed[seq_len(n_pairs)])] <- "reclassified"
  }
  records$status_changed <- in_old & in_new & records$status_old != records$status_new
  records <- records[order(records$country, records$species, records$area, records$Year), ]
  rownames(records) <- NULL

  years <- seq(min(old$Year), last_old)
  common <- records[records$Year <= last_old, ]
  by_year <- data.frame(Year = years)
  by_year$old_tonnes <- vapply(years, function(y) sum(common$tonnes_old[common$Year == y], na.rm = TRUE), numeric(1))
  by_year$new_tonnes <- vapply(years, function(y) sum(common$tonnes_new[common$Year == y], na.rm = TRUE), numeric(1))
  by_year$change_tonnes <- by_year$new_tonnes - by_year$old_tonnes
  by_year$change_pct <- ifelse(by_year$old_tonnes > 0, 100 * by_year$change_tonnes / by_year$old_tonnes, NA_real_)
  count <- function(type) vapply(years, function(y) sum(common$change[common$Year == y] == type), integer(1))
  by_year$n_revised <- count("revised")
  by_year$n_added <- count("added")
  by_year$n_removed <- count("removed")
  by_year$n_reclassified <- count("reclassified")
  by_year$n_status_changed <- vapply(years, function(y) sum(common$status_changed[common$Year == y]), integer(1))

  new_totals <- stats::aggregate(list(tonnes = new$tonnes), by = list(Year = new$Year), FUN = sum)
  list(records = records, by_year = by_year, new_totals = new_totals)
}
