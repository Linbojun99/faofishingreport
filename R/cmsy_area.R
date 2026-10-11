# One call for a whole fishing area: CMSY++ for its main species, with charts, projections and
# tables saved in one folder per area.

#' Run CMSY++ for the Main Species of a Fishing Area
#'
#' This function does in one call what [cmsy_input()], [cmsy_run()], [cmsy_plot()],
#' [cmsy_forecast()] and [cmsy_tables()] do step by step, for the species with the largest catch in
#' an FAO fishing area (or for chosen species): it prepares the catch-only input, runs CMSY++ on
#' every stock, draws the charts, projects the stocks under fishing scenarios and writes the
#' tables. Progress bars follow each step.
#'
#' The results of each area are saved in their own folder, `out_dir/<area>/`:
#' \describe{
#'   \item{input/}{the CMSY++ catch and ID files, the stocks used, and the main species left out
#'     with the reason (e.g. no r prior: see `priors` and `default_resilience` of [cmsy_input()]);}
#'   \item{runs/}{the CMSY++ run of each stock (its folder has the original CMSY++ charts,
#'     output and log), the changed CMSY++ code and the list of changes;}
#'   \item{figures/}{one folder of charts per stock and a `summary/` folder for all stocks (PNG,
#'     and PowerPoint in `figures_pptx/` when `pptx` is TRUE);}
#'   \item{tables/}{results, yearly series, the priors and results tables for reports,
#'     projections, the CMSY++ settings used (`settings_used.csv`) and the sources of everything
#'     used (`sources.csv`).}
#' }
#'
#' As in [cmsy_input()], FAO data have catches only (no abundance index), so all stocks are
#' assessed with the catch-only part of CMSY++ and the results are indicative.
#'
#' @inheritParams cmsy_run
#' @inheritParams cmsy_plot
#' @param auto_start As in [cmsy_run()], but TRUE by default here: a stock whose start biomass is
#'        ambiguous starts in the first year with at least 40% of the maximum catch, as suggested
#'        by CMSY++.
#' @param seed The random seed of the runs, so that the results can be repeated. Default is 1.
#' @param charts The charts to draw (see [cmsy_plot()]). Default is all of them.
#' @param pptx A logical value. If TRUE, the charts are also saved as editable PowerPoint files in
#'        `<out_dir>/<area>/figures_pptx/`. Default is FALSE, which can be changed for all functions
#'        with `fao_settings(pptx = TRUE)`.
#' @param area FAO major fishing area name(s), as in [fishing_area_data()] (e.g.
#'        "Atlantic, Eastern Central"), or FAO catch data such as `fishing_area_data()$area_data`.
#' @param top Number of species with the largest catch to assess. Default is 10.
#' @param species Optional species to assess instead of the `top` ones, by English or Chinese
#'        name, scientific name or FAO 3-alpha code.
#' @param recent_years Catch is ranked by its mean over the last `recent_years` years of the
#'        data. Default is 10.
#' @param out_dir The folder in which a folder per area is made. Default is "cmsy_areas".
#' @param forecast A logical value. If TRUE (default), the stocks are also projected under the
#'        default fishing scenarios of [cmsy_forecast()].
#' @param ... Further arguments passed to [cmsy_input()] (e.g. `priors`, `default_resilience`).
#' @return Invisibly, the result of [cmsy_run()] with the elements `input`, `forecast`, `tables`
#'         and `dir` (the folder of the area); a list of these, named by area, for several areas.
#' @references
#' Froese, R., Winker, H., Coro, G., Palomares, M.L.D., Tsikliras, A.C., Dimarchopoulou, D.,
#' Touloumis, K., Demirel, N., Vianna, G.M.S., Scarcella, G., Schijns, R., Liang, C. and Pauly,
#' D. (2023). New developments in the analysis of catch time series as the basis for fish stock
#' assessments: the CMSY++ method. Acta Ichthyologica et Piscatoria 53: 173-189.
#' \doi{10.3897/aiep.53.105910}
#'
#' Froese, R., Demirel, N., Coro, G. and Winker, H. (2021). User Guide for CMSY++. GEOMAR,
#' Germany, 17 pp. \url{https://oceanrep.geomar.de/id/eprint/52147/}
#'
#' FAO (2026). The State of World Fisheries and Aquaculture 2026. Rome, FAO. Section "The status
#' of fishery resources", Box 1.3 (assessment tiers and status classes).
#' \url{https://openknowledge.fao.org/server/api/core/bitstreams/09d03e2f-7097-4d59-bf94-db65e95f44fd/content/sofia-2026/the-status-fishery-resources.html}
#' @export
#' @examples
#' \dontrun{
#' cmsy_setup()                     # once: JAGS and the R packages CMSY++ needs
#'
#' # The ten species with the largest catch of the last ten years in FAO area 34, in Chinese
#' area34 <- cmsy_area("Atlantic, Eastern Central", top = 10, lang = "zh")
#' area34$summary[, c("Stock", "species_name", "B_Bmsy", "F_Fmsy", "status")]
#' area34$dir                       # folder with input/, runs/, figures/ and tables/
#'
#' # Chosen species (scientific name, FAO code, ...), all annotations, five status classes
#' cmsy_area("Atlantic, Eastern Central", species = c("Brachydeuterus auritus", "PIL"),
#'           annotations = TRUE, classes = "palomares")
#'
#' # Species without a FishBase prior get a resilience (passed on to cmsy_input())
#' cmsy_area("Atlantic, Eastern Central", top = 20, default_resilience = "Medium")
#'
#' # Several areas, quietly, with longer MCMC chains (see cmsy_defaults())
#' cmsy_area(c("Atlantic, Eastern Central", "Pacific, Northwest"), top = 5, silent = TRUE,
#'           settings = list(n.iter = 100000, n.burnin = 50000))
#' }
cmsy_area <- function(area, top = 10, species = NULL, recent_years = 10, out_dir = "cmsy_areas",
                      auto_start = TRUE, seed = 1, parallel = NULL, settings = list(),
                      charts = c(cmsy_stock_charts, cmsy_summary_charts), classes = c("fao", "palomares"),
                      forecast = TRUE, annotations = getOption("faofishingreport.cmsy_annotations", FALSE),
                      lang = NULL, pptx = getOption("faofishingreport.pptx", FALSE),
                      silent = fao_setting("silent"), ...) {
  if (is.character(area) && length(area) > 1) {
    out <- lapply(seq_along(area), function(i) {
      if (!silent) message("\n== ", i, "/", length(area), ": ", area[i], " ==")
      cmsy_area(area[i], top = top, species = species, recent_years = recent_years, out_dir = out_dir,
                auto_start = auto_start, seed = seed, parallel = parallel, settings = settings, charts = charts,
                classes = classes,
                forecast = forecast, annotations = annotations, lang = lang, pptx = pptx, silent = silent, ...)
    })
    return(invisible(stats::setNames(out, area)))
  }
  lang <- fao_lang(lang)
  classes <- match.arg(classes)
  step <- function(i, text) if (!silent) message("[", i, "/5] ", text)

  step(1, "Preparing the catch-only input")
  area_data <- if (is.data.frame(area)) area else {
    cmsy_quietly(fishing_area_data(area, plot = FALSE, timeseries_analysis = FALSE)$area_data, TRUE)
  }
  label <- fao_summary_label(area_data, NULL)
  if (is.null(attr(area_data, "fao_version")) && !silent) cmsy_assumed_release(cmsy_fao_version())
  # The input and the stocks are checked before the results of an earlier run are touched
  input <- cmsy_input(area_data, species = species, table = FALSE, lang = lang, silent = TRUE, ...)
  if (is.null(species)) {
    # The species with the largest recent catch, whether or not CMSY++ can assess them
    data <- fao_add_groups(area_data)
    data <- data[!is.na(data$Production) & data$taxon_level %in% "species" & grepl("^Tonnes", data$Unit..Name.), ]
    data <- data[data$Year > max(data$Year) - recent_years, ]
    ranking <- stats::aggregate(list(catch = data$Production), by = list(alpha3 = data$alpha3), FUN = sum)
    top_species <- utils::head(ranking$alpha3[order(-ranking$catch)], top)
    chosen <- input$stocks$Stock[input$stocks$alpha3 %in% top_species]
    skipped <- input$excluded[input$excluded$alpha3 %in% top_species, ]
  } else {
    chosen <- input$stocks$Stock
    skipped <- input$excluded
  }
  if (length(chosen) == 0) {
    if (nrow(skipped) > 0) {
      message("Left out: ", paste0(skipped$species_name, " (", skipped$reason, ")", collapse = "; "))
    }
    stop("No stock of ", label, " is ready for CMSY++.", call. = FALSE)
  }

  # The results of an earlier run in the same folder are moved aside, and put back if this run
  # fails; they are deleted only when this run has finished
  dir <- file.path(out_dir, fao_file_name(label))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir)
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  outputs <- c("input", "runs", "figures", "figures_pptx", "tables")
  previous <- outputs[file.exists(outputs)]
  backup <- ".previous_run"
  unlink(backup, recursive = TRUE)
  if (length(previous) > 0) {
    dir.create(backup)
    file.rename(previous, file.path(backup, previous))
  }
  finished <- FALSE
  on.exit({
    if (!finished) {
      unlink(outputs, recursive = TRUE)
      if (length(previous) > 0) {
        file.rename(file.path(backup, previous), previous)
        message("CMSY++ did not finish; the results of the earlier run in ", dir, " were kept.")
      }
    }
    unlink(backup, recursive = TRUE)
  }, add = TRUE, after = FALSE)

  input$catch <- input$catch[input$catch$Stock %in% chosen, ]
  input$id <- input$id[input$id$Stock %in% chosen, ]
  input$stocks <- input$stocks[input$stocks$Stock %in% chosen, ]
  input$name <- ""
  fao_ensure_dirs("input")
  utils::write.csv(input$catch, file.path("input", "catch.csv"), row.names = FALSE, na = "NA")
  utils::write.csv(input$id, file.path("input", "id.csv"), row.names = FALSE, na = "NA")
  fao_write_csv(input$stocks, file.path("input", "stocks.csv"))
  fao_write_csv(skipped, file.path("input", "excluded.csv"))
  if (!silent) {
    message("      ", nrow(input$stocks), if (nrow(input$stocks) == 1) " stock" else " stocks", " of ", label, " to assess",
            if (nrow(skipped) > 0) paste0("; left out: ", paste0(skipped$species_name, " (", skipped$reason, ")",
                                                                 collapse = "; ")))
  }
  input$excluded <- skipped

  step(2, "Running CMSY++")
  result <- cmsy_run(input, out_dir = "runs", parallel = parallel, plots = "analysis", seed = seed,
                     auto_start = auto_start, settings = settings, table = FALSE, lang = lang, silent = silent)

  step(3, "Drawing the charts")
  cmsy_plot(result, charts = charts, classes = classes, annotations = annotations, lang = lang, pptx = pptx,
            silent = silent)
  projection <- NULL
  if (forecast) {
    step(4, "Projecting under fishing scenarios")
    projection <- cmsy_forecast(result, seed = seed, table = FALSE, annotations = annotations, lang = lang,
                                pptx = pptx, silent = silent)
  } else {
    step(4, "Projections skipped (forecast = FALSE)")
  }

  step(5, "Writing the tables")
  tables <- cmsy_tables(result, table = FALSE, lang = lang, silent = TRUE)
  fao_ensure_dirs("tables")
  fao_write_csv(result$summary, file.path("tables", "results.csv"))
  if (!is.null(result$series)) fao_write_csv(result$series, file.path("tables", "series.csv"))
  fao_write_csv(tables$priors, file.path("tables", "priors_table.csv"))
  fao_write_csv(tables$results, file.path("tables", "results_table.csv"))
  fao_write_csv(result$sources, file.path("tables", "sources.csv"))
  fao_write_csv(result$settings, file.path("tables", "settings_used.csv"))
  if (!is.null(projection)) {
    fao_write_csv(projection$projection, file.path("tables", "forecast.csv"))
    fao_write_csv(projection$share, file.path("tables", "forecast_share.csv"))
  }
  # figures/png/cmsy/... and figures/pptx/cmsy/... become figures/ and figures_pptx/
  cmsy_move_dir(file.path("figures", "pptx", "cmsy"), "figures_pptx")
  cmsy_move_dir(file.path("figures", "png", "cmsy"), "figures")

  result$input <- input
  result$forecast <- projection
  result$tables <- tables
  result$dir <- dir
  finished <- TRUE
  if (!silent) {
    ok <- cmsy_has_results(result$summary$run_status)
    message("Done: ", sum(ok), " of ", nrow(result$summary), " stocks; ",
            sum(result$summary$status[ok] == "overfished", na.rm = TRUE), " overfished (B/Bmsy < 0.8). Results in ", dir)
  }
  invisible(result)
}

# Moves the contents of `from` to `to` (replacing `to`), e.g. figures/png/cmsy to figures
cmsy_move_dir <- function(from, to) {
  if (!dir.exists(from)) return(invisible(FALSE))
  staging <- paste0(to, "_moving")
  unlink(staging, recursive = TRUE)
  file.rename(from, staging)
  unlink(to, recursive = TRUE)
  file.rename(staging, to)
}
