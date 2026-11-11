# population_plot() and shared_limits() read `tiles_3857` (and, for
# shared_limits, the window/extent globals) from the global environment, as the
# report does. local_globals() (helper-fixtures.R) sets and restores them.

# ---- shared_limits ---------------------------------------------------------

local_shared_limit_globals <- function(.env = parent.frame()) {
  local_globals(
    tiles_3857  = make_tiles_3857(),
    first_ds    = as.Date("2025-01-07"), first_hour  = "0800",
    latest_ds   = as.Date("2025-01-08"), latest_hour = "1600",
    lon_limits  = c(-119, -117),
    lat_limits  = c(33.5, 35),
    .env = .env
  )
}

test_that("shared_limits returns NULL for a column that does not exist", {
  local_shared_limit_globals()
  expect_null(shared_limits("not_a_column"))
})

test_that("shared_limits returns the range across the first and latest windows", {
  local_shared_limit_globals()
  # n_difference over both windows: -10, 5, 30, -40, 20, -5, NA, -60
  expect_equal(shared_limits("Difference between baseline and crisis"), c(-60, 30))
  expect_equal(shared_limits("# Users During Crisis"), c(20, 200))
})

test_that("shared_limits symmetric = TRUE centres the range on zero", {
  local_shared_limit_globals()
  expect_equal(shared_limits("Difference between baseline and crisis", symmetric = TRUE), c(-60, 60))
  expect_equal(shared_limits("# Users During Crisis", symmetric = TRUE), c(-200, 200))
})

test_that("shared_limits only uses the requested windows", {
  local_shared_limit_globals()
  expect_equal(
    shared_limits("Difference between baseline and crisis",
                  ds1 = "2025-01-07", hour1 = "0800",
                  ds2 = "2025-01-07", hour2 = "0800"),
    c(-40, 30)
  )
})

test_that("shared_limits only uses tiles inside lon_limits / lat_limits", {
  local_shared_limit_globals()
  local_globals(lon_limits = c(-118.6, -118.4), lat_limits = c(33.9, 34.1))
  # only the tile at (-118.5, 34.0): values -10 (window 1) and 20 (window 2)
  expect_equal(shared_limits("Difference between baseline and crisis"), c(-10, 20))
})

test_that("shared_limits returns NULL when there are no finite values", {
  local_shared_limit_globals()
  local_globals(lon_limits = c(0, 1), lat_limits = c(0, 1))
  expect_null(shared_limits("z_score"))
})

# ---- population_plot -------------------------------------------------------

test_that("population_plot returns a ggplot titled with the formatted window", {
  withr::local_locale(c(LC_TIME = "C"))
  local_globals(tiles_3857 = make_tiles_3857())

  p <- population_plot("2025-01-07", "0800", tzone = "America/Los_Angeles")
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "January 7, 2025 8am")
  expect_true("GeomSf" %in% layer_geoms(p))
})

test_that("population_plot only draws the requested window", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot("2025-01-08", "1600", tzone = "UTC")
  dat <- p$layers[[1]]$data
  expect_equal(nrow(dat), 4)
  expect_true(all(dat$hour == "1600"))
})

test_that("population_plot fails loudly for a window that is not in the data", {
  local_globals(tiles_3857 = make_tiles_3857())
  expect_error(
    population_plot("2030-01-01", "0000", tzone = "UTC"),
    "no data for ds=2030-01-01, hour=0000.*Available windows: 2025-01-07 0800, 2025-01-08 1600"
  )
})

test_that("population_plot rejects unknown metric / highway_detail / disaster_type", {
  local_globals(tiles_3857 = make_tiles_3857())
  expect_error(population_plot("2025-01-07", "0800", tzone = "UTC", metric = "bogus"))
  expect_error(population_plot("2025-01-07", "0800", tzone = "UTC", highway_detail = "bogus"))
  expect_error(population_plot("2025-01-07", "0800", tzone = "UTC", disaster_type = "flood"))
})

test_that("population_plot metric = 'difference' uses a diverging scale on the difference column", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot("2025-01-07", "0800", tzone = "UTC", metric = "difference")
  fill <- p$scales$get_scales("fill")
  expect_identical(fill$name, "Users (crisis - baseline)")
  expect_match(rlang::as_label(p$layers[[1]]$mapping$fill), "Difference between baseline and crisis")
  # without lon/lat limits the fill limits span the whole dataset
  expect_equal(fill$limits, c(-60, 30))
})

test_that("population_plot metric = 'crisis' uses a log10 user-count scale", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot("2025-01-07", "0800", tzone = "UTC", metric = "crisis")
  fill <- p$scales$get_scales("fill")
  expect_identical(fill$name, "Users")
  expect_match(rlang::as_label(p$layers[[1]]$mapping$fill), "# Users During Crisis", fixed = TRUE)
  expect_match(fill$get_transformation()$name, "log-10")
})

test_that("population_plot metric = 'zscore' errors without a z_score column", {
  tiles <- make_tiles_3857()
  tiles$z_score <- NULL
  local_globals(tiles_3857 = tiles)
  expect_error(
    population_plot("2025-01-07", "0800", tzone = "UTC", metric = "zscore"),
    "column `z_score` not in tiles_3857"
  )
})

test_that("population_plot metric = 'zscore' uses a fixed -4..4 scale", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot("2025-01-07", "0800", tzone = "UTC", metric = "zscore")
  fill <- p$scales$get_scales("fill")
  expect_identical(fill$name, "z-score (crisis vs. baseline)")
  expect_equal(fill$limits, c(-4, 4))
})

test_that("population_plot fill_limits overrides the data-driven limits", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot("2025-01-07", "0800", tzone = "UTC", fill_limits = c(-100, 100))
  expect_equal(p$scales$get_scales("fill")$limits, c(-100, 100))
})

test_that("population_plot title = FALSE drops the title and plot_title overrides it", {
  local_globals(tiles_3857 = make_tiles_3857())
  p_none <- population_plot("2025-01-07", "0800", tzone = "UTC", title = FALSE)
  expect_null(p_none$labels$title)

  p_custom <- population_plot("2025-01-07", "0800", tzone = "UTC", plot_title = "My map")
  expect_identical(p_custom$labels$title, "My map")
})

test_that("population_plot without lon/lat limits does not fetch a basemap or highways", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mocked_bindings(get_tiles = function(...) stop("should not download tiles"))
  local_mocked_bindings(opq = function(...) stop("should not query OSM"), .package = "osmdata")
  p <- population_plot("2025-01-07", "0800", tzone = "UTC", highway_detail = "major")
  expect_null(p$coordinates$limits$x)
  expect_null(p$coordinates$limits$y)
})

test_that("population_plot with lon/lat limits crops data, rescales fill and sets 3857 limits", {
  local_globals(tiles_3857 = make_tiles_3857())
  tile_args <- NULL
  local_mocked_bindings(get_tiles = function(x, ...) {
    tile_args <<- list(x = x, ...)
    NULL
  })

  lon <- c(-118.6, -118.4)
  lat <- c(33.9, 34.1)
  p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                       lon_limits = lon, lat_limits = lat, zoom = 7)

  # basemap requested for the extent at the given zoom
  expect_identical(tile_args$provider, "CartoDB.Voyager")
  expect_identical(tile_args$zoom, 7)
  expect_equal(nrow(tile_args$x), 2)

  # only the one tile inside the box is drawn; limits come from it
  expect_equal(nrow(p$layers[[1]]$data), 1)
  expect_equal(p$scales$get_scales("fill")$limits, c(-10, -10))

  bb <- sf::st_bbox(sf::st_transform(
    sf::st_as_sfc(sf::st_bbox(c(xmin = lon[1], xmax = lon[2], ymin = lat[1], ymax = lat[2]),
                              crs = sf::st_crs(4326))),
    3857
  ))
  expect_equal(unname(p$coordinates$limits$x), unname(c(bb["xmin"], bb["xmax"])))
  expect_equal(unname(p$coordinates$limits$y), unname(c(bb["ymin"], bb["ymax"])))
})

test_that("population_plot keeps whole-data limits when the box contains no tiles", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mocked_bindings(get_tiles = function(...) NULL)
  p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                       lon_limits = c(0, 1), lat_limits = c(0, 1))
  expect_equal(nrow(p$layers[[1]]$data), 0)
  expect_equal(p$scales$get_scales("fill")$limits, c(-60, 30))
})

test_that("population_plot warns and continues when the OSM highway download fails", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mocked_bindings(get_tiles = function(...) NULL)
  local_mocked_bindings(osmdata_sf = function(...) stop("offline"), .package = "osmdata")

  expect_warning(
    p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                         lon_limits = c(-119, -117), lat_limits = c(33.5, 35),
                         highway_detail = "major"),
    "Could not download highway data from OpenStreetMap: offline"
  )
  expect_s3_class(p, "ggplot")
  expect_identical(sum(layer_geoms(p) == "GeomSf"), 1L)
})

test_that("population_plot queries the OSM highway classes for each detail level", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mocked_bindings(get_tiles = function(...) NULL)
  requested <- NULL
  local_mocked_bindings(
    osmdata_sf = function(q, ...) {
      requested <<- q$features
      list(osm_lines = sf::st_sf(
        highway = "motorway",
        geometry = sf::st_sfc(sf::st_linestring(rbind(c(-118.5, 34), c(-118, 34.5))), crs = 4326)
      ))
    },
    .package = "osmdata"
  )

  p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                       lon_limits = c(-119, -117), lat_limits = c(33.5, 35),
                       highway_detail = "major")
  expect_match(paste(requested, collapse = " "), "motorway")
  expect_no_match(paste(requested, collapse = " "), "secondary")
  # population polygons + highway lines
  expect_identical(sum(layer_geoms(p) == "GeomSf"), 2L)
  expect_equal(sf::st_crs(p$layers[[2]]$data)$epsg, 3857)

  population_plot("2025-01-07", "0800", tzone = "UTC",
                  lon_limits = c(-119, -117), lat_limits = c(33.5, 35),
                  highway_detail = "all")
  expect_match(paste(requested, collapse = " "), "residential")
})

test_that("population_plot adds labelled arrows for city labels", {
  local_globals(tiles_3857 = make_tiles_3857())
  labels <- sf::st_transform(sf::st_sf(
    label = c("Los Angeles", "Pasadena"),
    geometry = sf::st_sfc(sf::st_point(c(-118.24, 34.05)), sf::st_point(c(-118.14, 34.15)), crs = 4326)
  ), 3857)

  expect_no_warning(
    p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                         labels = labels, label_angles = c("Pasadena" = 90))
  )
  expect_true(all(c("GeomSegment", "GeomLabel") %in% layer_geoms(p)))

  lab <- p$layers[[which(layer_geoms(p) == "GeomLabel")]]$data
  coords <- unname(sf::st_coordinates(labels))
  # Los Angeles has no angle -> default 0 (label straight above the point)
  expect_equal(lab$angle, c(0, 90))
  expect_equal(lab$label_x[1], coords[1, 1])
  expect_equal(lab$label_y[1], coords[1, 2] + 80000)
  # Pasadena at 90 degrees -> label to the right
  expect_equal(lab$label_x[2], coords[2, 1] + 80000)
  expect_equal(lab$label_y[2], coords[2, 2], tolerance = 1e-6)
})

test_that("population_plot delegates to the fire / earthquake layer helpers", {
  local_globals(tiles_3857 = make_tiles_3857())
  called <- character()
  local_mocked_bindings(
    add_fire_layer = function(p1, ...) { called <<- c(called, "fire"); p1 },
    add_earthquake_layer = function(p1, ...) { called <<- c(called, "earthquake"); p1 }
  )
  population_plot("2025-01-07", "0800", tzone = "UTC", disaster_type = "fire")
  population_plot("2025-01-07", "0800", tzone = "UTC", disaster_type = "earthquake")
  population_plot("2025-01-07", "0800", tzone = "UTC")
  expect_identical(called, c("fire", "earthquake"))
})

# ---- wrappers --------------------------------------------------------------

test_that("population_plot_n_difference wraps population_plot with a minimal theme", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot_n_difference("2025-01-07", "0800", tzone = "UTC")
  expect_s3_class(p, "ggplot")
  expect_identical(p$scales$get_scales("fill")$name, "Users (crisis - baseline)")
  expect_identical(p$theme$legend.position, "bottom")
  expect_s3_class(p$theme$axis.text, "element_blank")
})

test_that("population_plot_n_crisis wraps population_plot with the crisis metric", {
  local_globals(tiles_3857 = make_tiles_3857())
  p <- population_plot_n_crisis("2025-01-07", "0800", tzone = "UTC", title = FALSE)
  expect_identical(p$scales$get_scales("fill")$name, "Users")
  expect_identical(p$theme$legend.position, "bottom")
  expect_null(p$labels$title)
})

test_that("population_plot_n_* refuse a conflicting metric argument", {
  local_globals(tiles_3857 = make_tiles_3857())
  expect_error(population_plot_n_crisis("2025-01-07", "0800", tzone = "UTC", metric = "zscore"))
})
