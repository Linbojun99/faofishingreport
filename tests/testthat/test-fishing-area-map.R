test_that("fao_areas() lists the marine and inland areas with links to the FAO maps", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  areas <- fao_areas(plot = FALSE, table = FALSE, lang = "en")
  expect_equal(sum(areas$type == "marine"), 19)
  expect_true(all(c("27", "34", "88") %in% areas$area_code))
  expect_equal(areas$fao_map[areas$area_code == "27"], "https://www.fao.org/fishery/docs/maps/fig_h2_27_0.gif")
  expect_true(all(!is.na(areas$fao_map[areas$type == "marine"])))
  expect_equal(sum(areas$share_pct, na.rm = TRUE), 100, tolerance = 1e-6)
  zh <- fao_areas(plot = FALSE, table = FALSE, lang = "zh")
  expect_equal(zh$name[zh$area_code == "27"], fao_translate("Atlantic, Northeast", "area", "zh"))
})

test_that("fishing_area_map() draws the subareas of an area, in the order of their codes", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  map <- fishing_area_map(27, engine = "sf", lang = "en")
  expect_equal(map$units$code, paste0("27.", 1:14)[-c(11, 13)])
  expect_true(file.exists(file.path("figures", "png", "area", "Atlantic__Northeast_subarea_map.png")))
  expect_true(file.exists(file.path("tables", "Atlantic__Northeast_subarea_map.csv")))
  expect_s3_class(map$map, "ggplot")
  expect_equal(map$fao_map, "https://www.fao.org/fishery/docs/maps/fig_h2_27_0.gif")
  expect_true("land" %in% map$sources$layer)
  divisions <- fishing_area_map("Atlantic, Northeast", level = "division", engine = "sf", plot = FALSE, table = FALSE)
  expect_true(all(grepl("^27[.][0-9]+[.][a-z]", divisions$units$code)))
  outline <- fishing_area_map(34, level = "none", engine = "sf", plot = FALSE, table = FALSE)
  expect_equal(nrow(outline$units), 0)
  expect_error(fishing_area_map("Africa - Inland waters", plot = FALSE), "marine fishing areas only")
  expect_error(fishing_area_map(99, plot = FALSE), "Unknown FAO major fishing area")
})

test_that("fishing_area_map() uses ggOceanMaps when it is installed, and cites its data", {
  skip_if_not_installed("ggOceanMaps")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  map <- fishing_area_map(27, lang = "zh", table = FALSE)
  expect_equal(map$engine, "ggOceanMaps")
  expect_true(all(c("base map", "depth") %in% map$sources$layer))
  expect_match(paste(map$sources$url, collapse = " "), "github.com/MikkoVihtakari/ggOceanMaps")
  expect_equal(map$map$scales$get_scales("fill")$name, fao_label("depth_m", "zh"))
  # ggOceanMaps is attached only while the base map is made
  expect_false("package:ggOceanMaps" %in% search())
})

test_that("areas across the 180th meridian get limits that cross it", {
  ring <- function(x) sf::st_polygon(list(rbind(c(x[1], 10), c(x[2], 10), c(x[2], 20), c(x[1], 20), c(x[1], 10))))
  shape <- sf::st_sfc(sf::st_multipolygon(list(ring(c(150, 180)), ring(c(-180, -175)))), crs = 4326)
  expect_equal(fao_area_limits(shape), c(150, -175, 10, 20))
  expect_equal(fao_area_limits(sf::st_sfc(ring(c(-40, 10)), crs = 4326)), c(-40, 10, 10, 20))
})

test_that("unit codes are ordered by number, then letter", {
  codes <- c("27.2", "27.14", "27.10", "27.1", "27.3.d.28.2", "27.3.d.28.1", "27.3.a")
  expect_equal(codes[fao_area_code_order(codes)],
               c("27.1", "27.2", "27.3.a", "27.3.d.28.1", "27.3.d.28.2", "27.10", "27.14"))
})

test_that("small islands and parts are removed from the boundaries, the main part is kept", {
  square <- function(x0, size) rbind(c(x0, x0), c(x0 + size, x0), c(x0 + size, x0 + size), c(x0, x0 + size), c(x0, x0))
  shape <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_multipolygon(list(
    list(square(0, 100), square(10, 1), square(50, 20)),   # a big part with a small and a large hole
    list(square(200, 2))                                   # a small part
  ))))
  out <- sf::st_geometry(fao_drop_small_rings(shape, min_area = 10))[[1]]
  expect_length(out, 1)
  expect_length(out[[1]], 2)
  expect_equal(as.numeric(sf::st_area(out)), 100^2 - 20^2)
})

test_that("area maps are saved with the shape of the map, without empty bands above and below", {
  box <- function(w, h) {
    sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(w, 0), c(w, h), c(0, h), c(0, 0)))), crs = 3857))
  }
  map <- function(w, h) ggplot2::ggplot() + ggplot2::geom_sf(data = box(w, h)) + ggplot2::coord_sf(expand = FALSE)
  expect_equal(fao_area_map_size(map(100, 50)), c(10, 9.2 * 0.5 + 1))
  expect_equal(fao_area_map_size(map(100, 50), legend = TRUE), c(10, 7.6 * 0.5 + 1))
  expect_equal(fao_area_map_size(map(100, 5))[2], 4)      # very wide: at least 4 inches
  expect_equal(fao_area_map_size(map(10, 100))[2], 14)    # very tall: at most 14 inches
  expect_equal(fao_area_map_size(ggplot2::ggplot()), c(10, 8))
})
