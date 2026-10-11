# Map helpers: projection, central longitude and base layers set with fao_settings().

# Central longitude of maps: 0 (longitudes -180 to 180) or 180 (longitudes 0 to 360)
fao_map_center <- function(value = fao_setting("map_center")) {
  center <- if (identical(value, "-180-180")) 0 else if (identical(value, "0-360")) 180 else suppressWarnings(as.numeric(value))
  if (length(center) != 1 || !center %in% c(0, 180, -180, 360)) {
    stop("map_center must be 0 (longitudes -180 to 180) or 180 (longitudes 0 to 360).", call. = FALSE)
  }
  if (center %in% c(0, 360)) 0 else 180
}

# Whether the map projection is one of the named projections (so map_center applies)
fao_named_projections <- c(longlat = "+proj=eqc", robinson = "+proj=robin", mollweide = "+proj=moll",
                           equal_earth = "+proj=eqearth", winkel_tripel = "+proj=wintri",
                           mercator = "+proj=merc")

# Coordinate reference system of maps
fao_map_crs <- function() {
  projection <- fao_setting("map_projection")
  center <- fao_map_center()
  if (!projection %in% names(fao_named_projections)) return(sf::st_crs(projection))
  if (projection == "longlat" && center == 0) return(sf::st_crs(4326))
  sf::st_crs(paste0(fao_named_projections[[projection]], " +lon_0=", center, " +datum=WGS84 +units=m +no_defs"))
}

# Prepares an sf layer for the map and projects it. For maps centred on 180, polygons are cut at
# the new edge of the map (0 degrees) and the parts that FAO and Natural Earth split at the
# dateline are joined again, so no line runs through the middle of the Pacific.
fao_map_prepare <- function(x) {
  # Cutting is done on planar longitude-latitude, so geometries are repaired the same way first
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  x <- sf::st_make_valid(sf::st_transform(x, 4326))
  if (fao_map_center() == 180 && fao_setting("map_projection") %in% names(fao_named_projections)) {
    # Cutting at 0 degrees turns a feature with parts on both sides into several rows; after
    # shifting to longitudes 0-360 the rows of each feature are joined again, which also removes
    # the dateline split of the data (e.g. Russia, the Pacific fishing areas)
    x$fao_map_id <- seq_len(nrow(x))
    x <- sf::st_shift_longitude(suppressWarnings(sf::st_break_antimeridian(x, lon_0 = 180)))
    split_ids <- as.integer(names(which(table(x$fao_map_id) > 1)))
    first <- x[!duplicated(x$fao_map_id), ]
    for (id in split_ids) {
      joined <- suppressWarnings(sf::st_union(sf::st_geometry(x)[x$fao_map_id == id]))
      sf::st_geometry(first)[first$fao_map_id == id] <- joined
    }
    x <- first
    x$fao_map_id <- NULL
  }
  if (fao_setting("map_projection") == "mercator") {
    x <- suppressWarnings(sf::st_crop(x, xmin = -180, xmax = 180, ymin = -80, ymax = 84))
  }
  sf::st_transform(x, fao_map_crs())
}

# Outline of the world in the map projection, filled with the ocean colour
fao_map_outline <- function() {
  projection <- fao_setting("map_projection")
  center <- if (projection %in% names(fao_named_projections)) fao_map_center() else 0
  lat_max <- if (projection == "mercator") c(-80, 84) else c(-90, 90)
  west <- center - 180 + 1e-6
  east <- center + 180 - 1e-6
  lons <- seq(west, east, length.out = 361)
  lats <- seq(lat_max[1], lat_max[2], length.out = 181)
  ring <- rbind(cbind(lons, lat_max[2]), cbind(east, rev(lats)), cbind(rev(lons), lat_max[1]),
                cbind(west, lats), c(west, lat_max[2]))
  outline <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  ggplot2::geom_sf(data = sf::st_transform(outline, fao_map_crs()), fill = fao_setting("map_ocean"),
                   colour = "grey60", linewidth = 0.3)
}

fao_map_coord <- function() {
  # Meridians meet at the poles in the Mollweide projection, so only latitudes are labelled
  label_axes <- if (identical(fao_setting("map_projection"), "mollweide")) "---N" else ggplot2::waiver()
  ggplot2::coord_sf(crs = fao_map_crs(), expand = FALSE, label_axes = label_axes)
}

# Base layers prepared for the current projection and central longitude, kept for the session
fao_cached_layer <- function(name, build) {
  key <- paste(name, fao_setting("map_projection"), fao_map_center())
  if (is.null(.fao_cache[[key]])) .fao_cache[[key]] <- fao_map_prepare(build())
  .fao_cache[[key]]
}

# World countries (Natural Earth admin-0 map units, which keep overseas territories such as
# Reunion separate) with ISO3 codes, prepared for the map
fao_world_map <- function(antarctica = TRUE) {
  world <- fao_cached_layer("world", function() {
    world <- rnaturalearth::ne_countries(scale = "medium", type = "map_units", returnclass = "sf")
    world$iso3 <- ifelse(world$iso_a3 == "-99", world$adm0_a3, world$iso_a3)
    world[, c("iso3", "name", "continent")]
  })
  if (!antarctica) world <- world[world$continent != "Antarctica", ]
  world
}

# FAO major fishing areas (inst/extdata/FAO_AREAS_CWP.shp), prepared for the map
fao_area_map <- function() {
  fao_cached_layer("fao_areas", fao_area_shapes)
}

# FAO major fishing areas in longitude-latitude, simplified for world maps (kept for the session)
fao_area_shapes <- function() {
  if (is.null(.fao_cache$fao_area_shapes)) {
    areas <- sf::st_read(system.file("extdata", "FAO_AREAS_CWP.shp", package = "faofishingreport"), quiet = TRUE)
    areas <- areas[areas$F_LEVEL == "MAJOR", c("F_CODE", "NAME_EN")]
    old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    # The coastlines of the shapefile are far more detailed than a world map needs; about 2 km is
    # kept. Straight boundaries then get a vertex every degree again, so that neighbouring areas
    # follow the same curve in projections such as Robinson and no gaps open between them.
    areas <- suppressWarnings(sf::st_simplify(sf::st_make_valid(areas), dTolerance = 0.02, preserveTopology = TRUE))
    # Segmentize in plain coordinates (degrees); with a geographic CRS the length would be in metres
    areas <- sf::st_segmentize(sf::st_set_crs(sf::st_make_valid(areas), NA), dfMaxLength = 1)
    .fao_cache$fao_area_shapes <- sf::st_set_crs(areas, 4326)
  }
  .fao_cache$fao_area_shapes
}

# Land as background of fishing area maps; with borders = FALSE the country borders have the
# fill colour, so no borders show
fao_land_layer <- function(fill = "grey80", borders = FALSE, antarctica = TRUE) {
  ggplot2::geom_sf(data = fao_world_map(antarctica), fill = fill,
                   colour = if (borders) "white" else fill, linewidth = 0.1)
}

# ISO3 codes of FAO country names (as used in the data)
fao_country_iso3 <- function(country) {
  groups <- fao_group_table()
  groups$country$iso3[match(country, groups$country$name_en)]
}

# Minimal theme of maps and pie charts, with the chosen font
fao_minimal_theme <- function() {
  ggplot2::theme_minimal(base_size = fao_setting("base_size"), base_family = fao_font())
}

# World map of a value by country (FAO country names, matched on ISO3 codes). Returns the plot and
# the map data.
fao_country_value_map <- function(country, value, fill_label, polygon_color = "grey90", palette = NULL) {
  values <- stats::aggregate(list(value = value), by = list(iso3 = fao_country_iso3(country)),
                             FUN = sum, na.rm = TRUE)
  world <- dplyr::left_join(fao_world_map(antarctica = FALSE), values, by = "iso3")
  plot <- ggplot2::ggplot() +
    fao_map_outline() +
    ggplot2::geom_sf(data = world, ggplot2::aes(fill = value), colour = polygon_color, linewidth = 0.1) +
    fao_scale_fill_map(palette) +
    fao_map_coord() +
    ggplot2::labs(fill = fill_label) +
    fao_minimal_theme()
  list(plot = plot, data = world)
}

# Map of a value by FAO major fishing area. Land is a background without country borders;
# countries given by ISO3 code in `highlight` are drawn in red. Returns the plot and the map data.
fao_area_value_map <- function(area, value, fill_label, highlight = NULL, palette = NULL) {
  values <- stats::aggregate(list(value = value), by = list(NAME_EN = area), FUN = sum, na.rm = TRUE)
  areas <- dplyr::left_join(fao_area_map(), values, by = "NAME_EN")
  no_catch <- if (is.na(fao_setting("map_ocean"))) "white" else fao_setting("map_ocean")
  plot <- ggplot2::ggplot() +
    fao_map_outline() +
    ggplot2::geom_sf(data = areas, ggplot2::aes(fill = value), colour = NA) +
    ggplot2::geom_sf(data = areas, fill = NA, colour = "grey60", linewidth = 0.2) +
    fao_land_layer() +
    fao_scale_fill_map(palette, na.value = no_catch) +
    ggplot2::labs(fill = fill_label) +
    fao_minimal_theme()
  if (length(highlight) > 0) {
    world <- fao_world_map()
    plot <- plot + ggplot2::geom_sf(data = world[world$iso3 %in% highlight, ], fill = "red",
                                    colour = "black", linewidth = 0.2)
  }
  list(plot = plot + fao_map_coord(), data = areas)
}
