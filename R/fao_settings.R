#' Settings for Charts, Maps and Output
#'
#' This function sets, for the rest of the R session, how all functions of the package draw
#' and save charts and maps. Call it without arguments to see the current settings, or with
#' `reset = TRUE` to go back to the defaults. Each setting is stored as the R option
#' `faofishingreport.<name>`, so it can also be set with `options()`.
#'
#' @param ... Settings to change, as `name = value`:
#' \describe{
#'   \item{lang}{Default language of charts: "en" (default), "zh" or "both". The `lang`
#'     argument of a function still takes precedence.}
#'   \item{theme}{Chart theme: "bw" (default), "classic", "minimal", "light", "linedraw",
#'     "gray" or "dark", or a ggplot2 theme object such as `ggplot2::theme_bw(base_size = 14)`.
#'     Maps and pie charts keep a minimal theme.}
#'   \item{base_size}{Base font size of the chart theme. Default is 12.}
#'   \item{font}{Font family for chart text; see `fao_font()`.}
#'   \item{palette}{Colours of categories (countries, species, groups): a palette of the
#'     ggsci package ("npg" (default), "lancet", "nejm", "jama", "aaas", "jco", "d3", ...),
#'     of RColorBrewer ("Set1", "Set2", "Dark2", "Paired", ...), of base R
#'     (`grDevices::palette.pals()`, e.g. "Okabe-Ito" or "Tableau 10"; or `grDevices::hcl.pals()`,
#'     e.g. "viridis" or "Dark 3"), or a vector of colours.}
#'   \item{map_palette}{Colours of map values: a Material Design palette of ggsci
#'     ("deep-orange" (default), "blue", "teal", "red", ...), an RColorBrewer palette ("YlOrRd",
#'     "Blues", ...), a viridis option ("viridis", "magma", "plasma", "inferno", "cividis",
#'     "mako", "rocket", "turbo"), a `grDevices::hcl.pals()` name, or a vector of colours.}
#'   \item{map_center}{Central longitude of maps: 0 (default) for longitudes -180 to 180
#'     (Atlantic in the centre), or 180 for longitudes 0 to 360 (Pacific in the centre). The
#'     strings "-180-180" and "0-360" are also accepted.}
#'   \item{map_projection}{Map projection: "longlat" (default, plain longitude-latitude),
#'     "robinson", "mollweide", "equal_earth", "winkel_tripel" or "mercator", or any PROJ string
#'     or EPSG code (e.g. "EPSG:3857"; then `map_center` is not applied).}
#'   \item{map_land}{Fill colour of countries without data on maps. Default is "grey90".}
#'   \item{map_ocean}{Fill colour of the ocean on maps. Default is NA (no fill).}
#'   \item{grid}{Whether charts have background grid lines (and maps graticules). Default is FALSE.}
#'   \item{dpi}{Resolution of the PNG files. Default is 600.}
#'   \item{pptx}{Whether charts are also saved as editable PowerPoint files (needs the eoffice
#'     package). Default is FALSE.}
#'   \item{data_dir}{Folder where `download_fao_data()` saves FAO releases and where all
#'     functions look for them, e.g. a folder shared by a team. Default is NULL, the user data
#'     folder of R; see `fao_data_dir()`.}
#'   \item{online}{Whether functions may connect to the internet to look for the newest FishBase
#'     release (see `fishbase_table()`). Default is TRUE; use FALSE to work offline with the
#'     downloaded copy, e.g. on a slow connection.}
#'   \item{sst}{The sea surface temperature (SST) data used by `fao_area_sst()` and
#'     `mean_temperature_catch()`: "ersst5" (default, NOAA ERSST v5, built into the package), a
#'     product downloaded with `download_sst_data()` (e.g. "hadisst1"), or the path of your own
#'     NetCDF file, or of a folder of NetCDF files (see `fao_area_sst()`).}
#'   \item{silent}{Whether the CMSY++ functions (`cmsy_run()`, `cmsy_plot()`, ...) work
#'     silently, without progress bars and messages. Default is FALSE. Their `silent` argument
#'     still takes precedence.}
#' }
#' @param reset A logical value. If TRUE, all settings go back to their defaults first.
#' @return Invisibly, a list of the current settings.
#' @family settings and language
#' @export
#' @examples
#' \dontrun{
#' fao_settings()                                   # show the current settings
#' fao_settings(lang = "zh", palette = "lancet", theme = "classic")
#' fao_settings(map_center = 180, map_projection = "robinson", map_palette = "YlOrRd")
#' fao_settings(reset = TRUE)                       # back to the defaults
#' }
fao_settings <- function(..., reset = FALSE) {
  if (reset) {
    options(stats::setNames(rep(list(NULL), length(fao_defaults)), paste0("faofishingreport.", names(fao_defaults))))
  }
  new <- list(...)
  if (length(new) > 0 && (is.null(names(new)) || any(names(new) == ""))) {
    stop("Settings must be given as name = value.", call. = FALSE)
  }
  if ("font" %in% names(new)) {
    fao_font(new$font)
    new$font <- NULL
  }
  unknown <- setdiff(names(new), names(fao_defaults))
  if (length(unknown) > 0) {
    stop("Unknown setting: ", paste(unknown, collapse = ", "), ". Available settings: ",
         paste(c(names(fao_defaults), "font"), collapse = ", "), call. = FALSE)
  }
  # Check the values now, so a mistake shows here rather than when a chart is drawn
  if (!is.null(new$lang)) fao_lang(new$lang)
  if (!is.null(new$map_center)) fao_map_center(new$map_center)
  if (!is.null(new$palette)) fao_palette(3, new$palette)
  if (!is.null(new$map_palette)) fao_scale_fill_map(new$map_palette)
  if (is.character(new$theme)) {
    old_theme <- options(faofishingreport.theme = new$theme)
    tryCatch(fao_base_theme(), finally = options(old_theme))
  }
  if (!is.null(new$map_projection) && !new$map_projection %in% names(fao_named_projections) &&
      is.na(suppressWarnings(tryCatch(sf::st_crs(new$map_projection), error = function(e) sf::NA_crs_)))) {
    stop("Unknown map projection \"", new$map_projection, "\". Use longlat, robinson, mollweide, ",
         "equal_earth, winkel_tripel, mercator, a PROJ string or an EPSG code.", call. = FALSE)
  }
  if (isTRUE(new$pptx)) fao_check_eoffice()
  if (!is.null(new$online) && !(is.logical(new$online) && length(new$online) == 1 && !is.na(new$online))) {
    stop("online must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(new$sst)) fao_sst_check_setting(new$sst)
  if (!is.null(new$silent) && !(is.logical(new$silent) && length(new$silent) == 1 && !is.na(new$silent))) {
    stop("silent must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(new) > 0) options(stats::setNames(new, paste0("faofishingreport.", names(new))))

  current <- c(lapply(stats::setNames(names(fao_defaults), names(fao_defaults)), fao_setting),
               list(font = fao_font()))
  if (length(new) == 0 && !reset) {
    for (name in names(current)) {
      value <- current[[name]]
      shown <- if (inherits(value, "theme")) "<ggplot2 theme>" else if (name == "data_dir") fao_data_dir() else paste(format(value), collapse = ", ")
      cat(sprintf("  %-15s %s\n", name, shown))
    }
  }
  invisible(current)
}

fao_defaults <- list(
  lang = "en", theme = "bw", base_size = 12, palette = "npg", map_palette = "deep-orange",
  map_center = 0, map_projection = "longlat", map_land = "grey90", map_ocean = NA,
  grid = FALSE, dpi = 600, pptx = FALSE, data_dir = NULL, online = TRUE, sst = "ersst5", silent = FALSE
)

fao_setting <- function(name) {
  getOption(paste0("faofishingreport.", name), fao_defaults[[name]])
}

# Complete ggplot2 theme of the charts, with the chosen font
fao_base_theme <- function() {
  theme <- fao_setting("theme")
  if (inherits(theme, "theme")) return(theme + ggplot2::theme(text = ggplot2::element_text(family = fao_font())))
  theme_function <- switch(
    theme,
    bw = ggplot2::theme_bw, classic = ggplot2::theme_classic, minimal = ggplot2::theme_minimal,
    light = ggplot2::theme_light, linedraw = ggplot2::theme_linedraw, gray = ggplot2::theme_gray,
    grey = ggplot2::theme_gray, dark = ggplot2::theme_dark,
    stop("Unknown theme \"", theme, "\". Use bw, classic, minimal, light, linedraw, gray, dark ",
         "or a ggplot2 theme object.", call. = FALSE)
  )
  theme_function(base_size = fao_setting("base_size"), base_family = fao_font())
}

# `n` colours of the chosen category palette; more colours than the palette has are interpolated
fao_palette <- function(n, palette = fao_setting("palette")) {
  if (n == 0) return(character(0))
  if (length(palette) > 1) {
    colors <- palette
  } else if (exists(paste0("pal_", palette), envir = asNamespace("ggsci"), inherits = FALSE)) {
    colors <- suppressWarnings(get(paste0("pal_", palette), envir = asNamespace("ggsci"))()(100))
    colors <- colors[!is.na(colors)]
  } else if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    colors <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palette, "maxcolors"], palette)
  } else if (palette %in% grDevices::palette.pals()) {
    colors <- grDevices::palette.colors(palette = palette)
  } else {
    colors <- tryCatch(grDevices::hcl.colors(max(n, 2), palette), error = function(e) {
      stop("Unknown palette \"", palette, "\"; see ?fao_settings.", call. = FALSE)
    })
  }
  colors <- unname(colors)
  if (n > length(colors)) grDevices::colorRampPalette(colors)(n) else colors[seq_len(n)]
}

# Discrete colour or fill scale using the chosen palette
fao_scale_discrete <- function(aesthetic = "colour") {
  ggplot2::discrete_scale(aesthetics = aesthetic, palette = function(n) fao_palette(n))
}

# Continuous fill scale for maps using the chosen map palette
fao_scale_fill_map <- function(palette = NULL, na.value = fao_setting("map_land")) {
  if (is.null(palette)) palette <- fao_setting("map_palette")
  # a colour bar long enough that its labels do not overlap when the legend is at the bottom
  guide <- ggplot2::guide_colourbar(barwidth = grid::unit(15, "lines"), barheight = grid::unit(0.8, "lines"))
  material <- c("red", "pink", "purple", "deep-purple", "indigo", "blue", "light-blue", "cyan", "teal",
                "green", "light-green", "lime", "yellow", "amber", "orange", "deep-orange", "brown",
                "grey", "blue-grey")
  viridis <- c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")
  if (length(palette) > 1) {
    ggplot2::scale_fill_gradientn(colours = palette, na.value = na.value, guide = guide)
  } else if (palette %in% material) {
    ggsci::scale_fill_material(palette, na.value = na.value, guide = guide)
  } else if (palette %in% viridis) {
    ggplot2::scale_fill_viridis_c(option = palette, direction = -1, na.value = na.value, guide = guide)
  } else if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    ggplot2::scale_fill_distiller(palette = palette, direction = 1, na.value = na.value, guide = guide)
  } else {
    colors <- tryCatch(grDevices::hcl.colors(9, palette, rev = TRUE), error = function(e) {
      stop("Unknown map palette \"", palette, "\"; see ?fao_settings.", call. = FALSE)
    })
    ggplot2::scale_fill_gradientn(colours = colors, na.value = na.value, guide = guide)
  }
}
