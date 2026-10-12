# The FAO major fishing areas (table and world map) and maps of one area with its subareas,
# divisions or subdivisions.

# Official FAO maps of the major fishing areas (https://www.fao.org/fishery/docs/maps/Default.htm)
fao_area_map_base <- "https://www.fao.org/fishery/docs/maps/"
fao_area_map_files <- c(
  "01" = "fig_h1_1", "02" = "fig_h1_2", "03" = "fig_h1_3", "04" = "fig_h1_4_1", "05" = "fig_h1_5_1",
  "06" = "fig_h1_6", "08" = "fig_h1_8", "18" = "fig_h2_18_0", "21" = "fig_h2_21_0", "27" = "fig_h2_27_0",
  "31" = "fig_h2_31_0", "34" = "fig_h2_34_0", "37" = "fig_h2_37_0", "41" = "fig_h2_41_0",
  "47" = "fig_h2_47_0", "48" = "fig_h5_48_0", "51" = "fig_h3_51_0", "57" = "fig_h3_57_0",
  "58" = "fig_h5_58_0", "61" = "fig_h4_61_0", "67" = "fig_h4_67_0", "71" = "fig_h4_71_0",
  "77" = "fig_h4_77_0", "81" = "fig_h4_81_0", "87" = "fig_h4_87_0", "88" = "fig_h5_88_0"
)

#' FAO Major Fishing Areas
#'
#' This function lists the FAO major fishing areas for statistical purposes, defined by the
#' Coordinating Working Party on Fishery Statistics (CWP): the 19 marine areas and the inland
#' waters of each continent. For each area it gives its code, name, ocean, catch in the last year
#' of the data and the link to its official FAO map. With `plot = TRUE`, a world map of the marine
#' areas labelled with their codes is saved as `figures/png/area/fao_major_areas.png`.
#'
#' The official maps are the GIF images of the FAO fishing area maps page
#' (<https://www.fao.org/fishery/docs/maps/Default.htm>); the package links to them and does not
#' copy them. The world map is drawn from the area boundaries bundled with the package
#' (`inst/extdata/FAO_AREAS_CWP.shp`, FAO, CC BY 4.0), in the projection and central longitude
#' set with `fao_settings()`. [fishing_area_map()] maps one area with its subareas.
#'
#' @param data FAO catch data as returned by `load_fao_data()`, used for the catch of the last
#'        year. Default is NULL, the latest release (`load_fao_data()`).
#' @param plot A logical value. If TRUE (default), the world map is saved in `figures/png/area/`.
#' @param table A logical value. If TRUE (default), the table is saved in `tables/`.
#' @param lang The language of the map text and names: "en" (English), "zh" (Chinese) or "both"
#'        (Chinese and English). Default is NULL, which uses the language set with
#'        `fao_settings()` ("en" unless changed).
#' @param pptx A logical value. If TRUE, the map is also saved as an editable PowerPoint file in
#'        `figures/pptx/area/`. Default is the value set with `fao_settings()` (FALSE unless changed).
#' @return Invisibly, a data frame with one row per area: `area_code`, `name_en`, `name` (in the
#'         chosen language), `type` ("marine" or "inland"), `ocean`, `catch_last_year` (tonnes),
#'         `share_pct` (percentage of the world catch of the year, marine and inland) and
#'         `fao_map` (link to the official FAO map). The map is in the attribute "plot".
#' @references
#' CWP (Coordinating Working Party on Fishery Statistics). *CWP Handbook of Fishery Statistics*:
#' Main water areas (FAO major fishing areas). Rome: FAO.
#' <https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/main-water-areas/en/>
#'
#' FAO. Fishing area maps. In: FAO Fisheries and Aquaculture Division \[online\]. Rome.
#' <https://www.fao.org/fishery/docs/maps/Default.htm>
#'
#' FAO (2024). FAO Major Fishing Areas for Statistical Purposes. In: FAO Fisheries and
#' Aquaculture Division \[online\]. Licence: CC-BY 4.0.
#' <https://www.fao.org/fishery/geonetwork?uuid=ac02a460-da52-11dc-9d70-0017f293bd28>
#' @seealso [fishing_area_map()] for the map of one area and its subareas.
#' @export
#' @examples
#' \dontrun{
#' areas <- fao_areas(lang = "zh")
#' areas[areas$type == "marine", c("area_code", "name", "catch_last_year")]
#' }
fao_areas <- function(data = NULL, plot = TRUE, table = TRUE, lang = NULL, pptx = fao_setting("pptx")) {
  lang <- fao_lang(lang)
  if (is.null(data)) data <- load_fao_data()
  areas <- fao_group_table()$area
  areas <- areas[grepl("^[0-9]{2}$", areas$area_code) & !areas$area_code %in% c("98", "99"), ]
  areas <- areas[order(areas$area_code), ]
  last <- data[data$Year == max(data$Year), ]
  catch <- tapply(last$Production, last$FAO.major.fishing.area..Name., sum, na.rm = TRUE)
  marine <- areas$inland_marine_en == "Marine areas"
  result <- data.frame(
    area_code = areas$area_code, name_en = areas$name_en,
    name = fao_translate(areas$name_en, "area", lang),
    type = ifelse(marine, "marine", "inland"),
    ocean = if (lang == "en") areas$ocean_en else areas$ocean_zh,
    catch_last_year = as.numeric(catch[areas$name_en]),
    stringsAsFactors = FALSE
  )
  result$share_pct <- 100 * result$catch_last_year / sum(last$Production, na.rm = TRUE)
  files <- fao_area_map_files[result$area_code]
  result$fao_map <- ifelse(is.na(files), NA_character_, paste0(fao_area_map_base, files, ".gif"))
  rownames(result) <- NULL

  if (plot) {
    shapes <- fao_area_map()
    shapes$area_code <- sprintf("%02d", as.integer(shapes$F_CODE))
    # oceans in the order Atlantic, Indian, Pacific, Southern (those of areas 21, 51, 61 and 48)
    oceans <- result$ocean[match(c("21", "51", "61", "48"), result$area_code)]
    shapes$ocean <- factor(result$ocean[match(shapes$area_code, result$area_code)], levels = oceans)
    old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    points <- suppressWarnings(sf::st_point_on_surface(shapes))
    labels <- data.frame(sf::st_coordinates(points), code = as.integer(shapes$area_code))
    p <- ggplot2::ggplot() +
      fao_map_outline() +
      ggplot2::geom_sf(data = shapes, ggplot2::aes(fill = ocean), colour = "white", linewidth = 0.3) +
      fao_land_layer(fill = fao_setting("map_land")) +
      ggplot2::geom_label(data = labels, ggplot2::aes(x = X, y = Y, label = code), size = 3.2,
                          family = fao_font(), label.size = 0, label.padding = ggplot2::unit(0.12, "lines"),
                          fill = "white", alpha = 0.85) +
      ggplot2::scale_fill_manual(values = stats::setNames(c("#0072B2", "#E69F00", "#56B4E9", "#009E73"), oceans)) +
      ggplot2::labs(x = NULL, y = NULL, fill = fao_label("ocean", lang),
                    caption = fao_label("fao_areas_source", lang)) +
      fao_minimal_theme() +
      ggplot2::theme(legend.position = "bottom") +
      fao_map_coord()
    fao_save_plot(p, file.path("area", "fao_major_areas"), width = 12, height = 7, pptx = pptx)
    attr(result, "plot") <- p
  }
  if (table) fao_save_table(result, "fao_major_areas")
  invisible(result)
}

#' Map of an FAO Major Fishing Area and Its Subareas
#'
#' This function maps one FAO major marine fishing area with its subareas, divisions or
#' subdivisions, labelled with their codes (e.g. 27.4, the North Sea), from the boundaries
#' bundled with the package (`inst/extdata/FAO_AREAS_CWP.shp`, FAO, CC BY 4.0). When the
#' ggOceanMaps package (Vihtakari 2026, <https://github.com/MikkoVihtakari/ggOceanMaps>) is
#' installed, the map is drawn on its bathymetric base map: polar stereographic for areas reaching
#' beyond 60 degrees of latitude (e.g. area 27), else in longitude and latitude, with the depth
#' from the low-resolution ETOPO 2022 relief model bundled with ggOceanMaps (no download needed).
#' Without ggOceanMaps, the land of Natural Earth is drawn in a Lambert azimuthal equal-area
#' projection centred on the area, without depth.
#'
#' The official FAO map of the area is linked in the result (`fao_map`); see [fao_areas()].
#'
#' @param area The FAO major marine fishing area: its code (e.g. 27) or English name (e.g.
#'        "Atlantic, Northeast").
#' @param level The units drawn inside the area: "subarea" (default), "division",
#'        "subdivision" or "none" (the outline of the area only). Areas without units at the level
#'        asked for are drawn with their outline only.
#' @param bathymetry A logical value. If TRUE (default), the depth is shown (ggOceanMaps only).
#' @param labels A logical value. If TRUE (default), the units are labelled with their codes.
#' @param engine How the map is drawn: "auto" (default, ggOceanMaps when installed, else "sf"),
#'        "ggOceanMaps" or "sf".
#' @param plot A logical value. If TRUE (default), the map is saved in `figures/png/area/`.
#' @param table A logical value. If TRUE (default), the table of units is saved in `tables/`.
#' @inheritParams fao_areas
#' @return Invisibly, a list with `map` (the ggplot object), `units` (one row per unit: `code`,
#'         `level`, `name_en`), `engine`, `fao_map` (link to the official FAO map of the area) and
#'         `sources` (the boundaries, base map and depth data used, to cite with the map).
#' @references
#' FAO (2024). FAO Major Fishing Areas for Statistical Purposes. In: FAO Fisheries and
#' Aquaculture Division \[online\]. Licence: CC-BY 4.0.
#' <https://www.fao.org/fishery/geonetwork?uuid=ac02a460-da52-11dc-9d70-0017f293bd28>
#'
#' NOAA National Centers for Environmental Information (2022). ETOPO 2022 15 Arc-Second Global
#' Relief Model. [doi:10.25921/fd45-gt74](https://doi.org/10.25921/fd45-gt74)
#'
#' Vihtakari, M. (2026). *ggOceanMaps: Plot Data on Oceanographic Maps using 'ggplot2'*. R package
#' version 3.0.0. [doi:10.32614/CRAN.package.ggOceanMaps](https://doi.org/10.32614/CRAN.package.ggOceanMaps),
#' <https://github.com/MikkoVihtakari/ggOceanMaps>
#' @seealso [fao_areas()] for all areas.
#' @export
#' @examples
#' \dontrun{
#' # The Northeast Atlantic and its subareas (27.1 Barents Sea, ..., 27.14 East Greenland)
#' ne_atlantic <- fishing_area_map(27, lang = "zh")
#' ne_atlantic$units
#'
#' # Its divisions, without depth
#' fishing_area_map("Atlantic, Northeast", level = "division", bathymetry = FALSE)
#' }
fishing_area_map <- function(area, level = c("subarea", "division", "subdivision", "none"), bathymetry = TRUE,
                             labels = TRUE, engine = c("auto", "ggOceanMaps", "sf"), plot = TRUE, table = TRUE,
                             lang = NULL, pptx = fao_setting("pptx")) {
  level <- match.arg(level)
  engine <- match.arg(engine)
  lang <- fao_lang(lang)
  area <- fao_report_area(area, "en")
  if (!area$marine) stop("Maps are drawn for marine fishing areas only; ", area$name, " is an inland area.", call. = FALSE)
  if (engine == "auto") engine <- if (requireNamespace("ggOceanMaps", quietly = TRUE)) "ggOceanMaps" else "sf"
  if (engine == "ggOceanMaps" && !requireNamespace("ggOceanMaps", quietly = TRUE)) {
    stop("The ggOceanMaps package is needed for engine = \"ggOceanMaps\": install.packages(\"ggOceanMaps\")",
         call. = FALSE)
  }

  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  shapes <- fao_area_units_shapes()
  major <- shapes[shapes$F_LEVEL == "MAJOR" & shapes$F_CODE == area$code, ]
  if (nrow(major) == 0) {
    codes <- sort(as.integer(shapes$F_CODE[shapes$F_LEVEL == "MAJOR"]))
    stop("Unknown FAO major fishing area for maps: ", area$name, ". Marine areas: ", paste(codes, collapse = ", "),
         call. = FALSE)
  }
  units <- if (level == "none") shapes[0, ] else shapes[shapes$F_AREA == area$code & shapes$F_LEVEL == toupper(level), ]
  units <- units[fao_area_code_order(units$F_CODE), ]

  base <- if (engine == "ggOceanMaps") fao_ocean_basemap(major, bathymetry) else fao_sf_basemap(major)
  crs <- base$crs
  # The boundaries follow the coast in great detail: about 1 km is kept, and islands and ice rises
  # smaller than 0.02% of the area are left to the land layer, which is drawn over the boundaries
  simplify <- function(x) {
    x <- suppressWarnings(sf::st_simplify(sf::st_transform(x, crs), dTolerance = 1000, preserveTopology = TRUE))
    fao_drop_small_rings(x, 2e-4 * as.numeric(sf::st_area(sf::st_transform(major, crs))))
  }
  units_map <- simplify(units)
  major_map <- simplify(major)
  p <- base$plot +
    ggplot2::geom_sf(data = units_map, fill = NA, colour = "#D7191C", linewidth = 0.35, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = major_map, fill = NA, colour = "black", linewidth = 0.6, inherit.aes = FALSE)
  # the ocean in light blue where no depth is shown
  ocean <- fao_setting("map_ocean")
  if (is.na(ocean)) ocean <- "#EAF4FA"
  if (engine == "ggOceanMaps") {
    p <- ggOceanMaps::reorder_layers(p)
    fill <- p$scales$get_scales("fill")
    if (bathymetry && !is.null(fill)) fill$name <- fao_label("depth_m", lang)
    # ggOceanMaps draws the panel over the map (for its grid lines), so the ocean is a first layer
    if (!bathymetry) {
      p$layers <- c(ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = ocean), p$layers)
    }
  } else {
    p <- p + base$land + ggplot2::coord_sf(crs = crs, xlim = base$xlim, ylim = base$ylim, expand = FALSE) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = ocean, colour = NA))
  }
  p <- p + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  if (labels && nrow(units_map) > 0) {
    points <- suppressWarnings(sf::st_point_on_surface(units_map))
    text <- data.frame(sf::st_coordinates(points), code = units_map$F_CODE)
    p <- p + ggrepel::geom_label_repel(data = text, ggplot2::aes(x = X, y = Y, label = code), inherit.aes = FALSE,
                                       size = 3, family = fao_font(), label.size = 0, fill = "white", alpha = 0.85,
                                       label.padding = ggplot2::unit(0.12, "lines"), min.segment.length = 0,
                                       seed = 1, max.overlaps = Inf)
  }
  sources <- fao_area_map_sources(engine, bathymetry)
  p <- p + ggplot2::labs(x = NULL, y = NULL, caption = paste(sources$caption[[lang]], collapse = "; ")) +
    ggplot2::theme(text = ggplot2::element_text(family = fao_font()),
                   plot.caption = ggplot2::element_text(size = 8, colour = "grey40"))

  result_units <- data.frame(code = units$F_CODE, level = tolower(units$F_LEVEL), name_en = units$NAME_EN,
                             stringsAsFactors = FALSE)
  name <- paste0(fao_file_name(area$name), "_", if (level == "none") "outline" else level, "_map")
  if (plot) {
    # The raster of depths of ggOceanMaps is drawn with geom_raster, which warns about the uneven
    # spacing of its projected cells; the map is right
    withCallingHandlers(fao_save_plot(p, file.path("area", name), width = 10, height = 8, pptx = pptx),
                        warning = function(w) if (grepl("uneven", conditionMessage(w))) invokeRestart("muffleWarning"))
  }
  if (table && nrow(result_units) > 0) fao_save_table(result_units, name)
  invisible(list(map = p, units = result_units, engine = engine,
                 fao_map = paste0(fao_area_map_base, fao_area_map_files[[sprintf("%02d", as.integer(area$code))]], ".gif"),
                 sources = sources$table))
}

# All units of the bundled FAO area layer (major areas, subareas, divisions, subdivisions,
# subunits) in longitude-latitude, kept for the session
fao_area_units_shapes <- function() {
  if (is.null(.fao_cache$fao_area_units)) {
    old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    x <- sf::st_read(system.file("extdata", "FAO_AREAS_CWP.shp", package = "faofishingreport"), quiet = TRUE)
    x <- sf::st_make_valid(x[, c("F_CODE", "F_LEVEL", "F_AREA", "NAME_EN")])
    .fao_cache$fao_area_units <- sf::st_set_crs(x, 4326)
  }
  .fao_cache$fao_area_units
}

# Order of unit codes such as 27.2, 27.10, 27.3.d.28.1: numbers by value, letters alphabetically
fao_area_code_order <- function(code) {
  parts <- strsplit(code, ".", fixed = TRUE)
  depth <- max(lengths(parts), 1)
  keys <- lapply(seq_len(depth), function(i) {
    part <- vapply(parts, function(p) if (length(p) >= i) p[i] else "", character(1))
    number <- suppressWarnings(as.numeric(part))
    ifelse(is.na(number), part, formatC(number, width = 6, flag = "0"))
  })
  do.call(order, keys)
}

# Longitude and latitude limits of an area; for areas across the 180th meridian (e.g. 61, 71, 81)
# the western limit is east of the eastern one, as ggOceanMaps expects
fao_area_limits <- function(shape) {
  bb <- sf::st_bbox(shape)
  lon <- c(bb[["xmin"]], bb[["xmax"]])
  if (diff(lon) > 300) {
    shifted <- sf::st_bbox(sf::st_shift_longitude(shape))
    lon <- c(shifted[["xmin"]], shifted[["xmax"]] - 360)
  }
  c(lon, bb[["ymin"]], bb[["ymax"]])
}

# Base map of ggOceanMaps for an area: limits from the area itself (rotated so that its central
# meridian points up), else from its longitude and latitude limits
fao_ocean_basemap <- function(major, bathymetry) {
  # basemap() finds its bundled land and depth data (dd_land, dd_rbathy) on the search path, so
  # ggOceanMaps is attached while the base map is made, and detached again if it was not attached
  if (!"package:ggOceanMaps" %in% search()) {
    suppressPackageStartupMessages(attachNamespace("ggOceanMaps"))
    on.exit(detach("package:ggOceanMaps", character.only = TRUE), add = TRUE)
  }
  build <- function(...) suppressMessages(suppressWarnings(ggOceanMaps::basemap(..., bathymetry = bathymetry,
                                                                                 rotate = TRUE, land.col = "grey80",
                                                                                 land.border.col = "grey50")))
  plot <- tryCatch(build(data = major), error = function(e) NULL)
  if (is.null(plot)) plot <- build(limits = fao_area_limits(major))
  list(plot = plot, crs = sf::st_crs(attributes(plot)$crs))
}

# Base map without ggOceanMaps: land of Natural Earth in a Lambert azimuthal equal-area
# projection centred on the area, ocean in the colour set with fao_settings()
fao_sf_basemap <- function(major) {
  limits <- fao_area_limits(major)
  across <- limits[1] > limits[2]
  east <- if (across) limits[2] + 360 else limits[2]
  center <- c(lon = (limits[1] + east) / 2, lat = mean(limits[3:4]))
  if (center[["lon"]] > 180) center[["lon"]] <- center[["lon"]] - 360
  crs <- sf::st_crs(sprintf("+proj=laea +lat_0=%.3f +lon_0=%.3f +datum=WGS84 +units=m +no_defs",
                            center[["lat"]], center[["lon"]]))
  if (is.null(.fao_cache$world_lonlat)) {
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    .fao_cache$world_lonlat <- sf::st_make_valid(sf::st_geometry(world))
  }
  # only the land near the area is projected, so that the far side of the globe is left out; an
  # area across the 180th meridian takes the land on both sides of it
  crop <- function(west, east) {
    box <- c(xmin = max(west, -180), xmax = min(east, 180), ymin = max(limits[3] - 15, -90), ymax = min(limits[4] + 15, 90))
    suppressMessages(suppressWarnings(sf::st_crop(.fao_cache$world_lonlat, sf::st_bbox(box, crs = sf::st_crs(4326)))))
  }
  land <- if (across) c(crop(limits[1] - 20, 180), crop(-180, limits[2] + 20)) else crop(limits[1] - 20, limits[2] + 20)
  bb <- sf::st_bbox(sf::st_transform(major, crs))
  pad <- 0.04 * max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
  # the land is added after the boundaries, so that it covers them along the coast
  list(plot = ggplot2::ggplot() + fao_minimal_theme(), crs = crs,
       land = ggplot2::geom_sf(data = sf::st_transform(land, crs), fill = "grey80", colour = "grey50", linewidth = 0.1),
       xlim = c(bb[["xmin"]] - pad, bb[["xmax"]] + pad),
       ylim = c(bb[["ymin"]] - pad, bb[["ymax"]] + pad))
}

# The data and software of an area map, to cite with it: a short caption in each language and a
# table with the full references
fao_area_map_sources <- function(engine, bathymetry) {
  rows <- list(
    c("boundaries", "FAO (2024). FAO Major Fishing Areas for Statistical Purposes. Licence: CC-BY 4.0.",
      "https://www.fao.org/fishery/geonetwork?uuid=ac02a460-da52-11dc-9d70-0017f293bd28")
  )
  caption <- list(en = "Boundaries: FAO (CC BY 4.0)", zh = "\u8fb9\u754c\uff1aFAO\uff08CC BY 4.0\uff09")
  if (engine == "ggOceanMaps") {
    rows <- c(rows, list(
      c("base map", "Vihtakari, M. (2026). ggOceanMaps: Plot Data on Oceanographic Maps using 'ggplot2'. R package version 3.0.0. doi:10.32614/CRAN.package.ggOceanMaps",
        "https://github.com/MikkoVihtakari/ggOceanMaps"),
      c("land", "Natural Earth. 1:10m Land and Minor Islands (public domain), as bundled with ggOceanMaps.",
        "https://www.naturalearthdata.com/downloads/10m-physical-vectors/")))
    caption$en <- c(caption$en, "map: ggOceanMaps")
    caption$zh <- c(caption$zh, "\u5e95\u56fe\uff1aggOceanMaps")
    if (bathymetry) {
      rows <- c(rows, list(c("depth", "NOAA National Centers for Environmental Information (2022). ETOPO 2022 15 Arc-Second Global Relief Model. doi:10.25921/fd45-gt74",
                             "https://doi.org/10.25921/fd45-gt74")))
      caption$en <- c(caption$en, "depth: NOAA ETOPO 2022")
      caption$zh <- c(caption$zh, "\u6c34\u6df1\uff1aNOAA ETOPO 2022")
    }
  } else {
    rows <- c(rows, list(c("land", "Natural Earth. Admin 0 - Countries, 1:50m (public domain), via the rnaturalearth package.",
                           "https://www.naturalearthdata.com/")))
    caption$en <- c(caption$en, "land: Natural Earth")
    caption$zh <- c(caption$zh, "\u9646\u5730\uff1aNatural Earth")
  }
  caption$both <- caption$zh
  table <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(table) <- c("layer", "reference", "url")
  list(caption = caption, table = table)
}

# Removes the holes (islands, ice rises) and parts of polygons smaller than `min_area` (in the
# units of the projected layer), keeping the largest part of each feature
fao_drop_small_rings <- function(x, min_area) {
  ring_area <- function(ring) {
    n <- nrow(ring)
    abs(sum(ring[-1, 1] * ring[-n, 2] - ring[-n, 1] * ring[-1, 2])) / 2
  }
  geometry <- lapply(sf::st_geometry(x), function(g) {
    parts <- if (inherits(g, "POLYGON")) list(unclass(g)) else if (inherits(g, "MULTIPOLYGON")) unclass(g) else return(g)
    parts <- lapply(parts, function(rings) {
      if (length(rings) < 2) return(rings)
      c(rings[1], rings[-1][vapply(rings[-1], ring_area, numeric(1)) >= min_area])
    })
    size <- vapply(parts, function(rings) ring_area(rings[[1]]), numeric(1))
    keep <- size >= min_area
    keep[which.max(size)] <- TRUE
    sf::st_multipolygon(parts[keep])
  })
  sf::st_set_geometry(x, sf::st_sfc(geometry, crs = sf::st_crs(x)))
}
