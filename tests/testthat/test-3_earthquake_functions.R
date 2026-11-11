# The USGS feed is never hit: sf::st_read is mocked to return fixture features.

local_mock_usgs <- function(result = make_earthquakes_4326(), .env = parent.frame()) {
  calls <- new.env()
  calls$urls <- character()
  local_mocked_bindings(
    st_read = function(dsn, ...) {
      calls$urls <- c(calls$urls, dsn)
      result
    },
    .package = "sf",
    .env = .env
  )
  calls
}

# ---- fetch_earthquakes -----------------------------------------------------

test_that("fetch_earthquakes builds the USGS query from its arguments", {
  calls <- local_mock_usgs()
  fetch_earthquakes(c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31", 3)

  url <- calls$urls
  expect_length(url, 1)
  expect_match(url, "^https://earthquake.usgs.gov/fdsnws/event/1/query\\?format=geojson")
  expect_match(url, "&starttime=2025-01-01", fixed = TRUE)
  expect_match(url, "&endtime=2025-01-31", fixed = TRUE)
  expect_match(url, "&minlatitude=33.5&maxlatitude=35", fixed = TRUE)
  expect_match(url, "&minlongitude=-119&maxlongitude=-117", fixed = TRUE)
  expect_match(url, "&minmagnitude=3$")
})

test_that("fetch_earthquakes returns events in EPSG:3857, in time order, lettered A, B, C", {
  local_mock_usgs()
  eq <- fetch_earthquakes(c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31", 2.5)

  expect_s3_class(eq, "sf")
  expect_equal(sf::st_crs(eq)$epsg, 3857)
  expect_false(is.unsorted(eq$datetime))
  expect_identical(eq$eq_order, c("A", "B", "C"))
  # fixture is out of order: 08:00 (2.7), 12:00 (4.1), 20:00 (3.3)
  expect_equal(eq$mag, c(2.7, 4.1, 3.3))
})

test_that("fetch_earthquakes converts epoch-ms to UTC and Pacific datetimes", {
  local_mock_usgs()
  eq <- fetch_earthquakes(c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31", 2.5)

  expect_s3_class(eq$datetime, "POSIXct")
  expect_identical(attr(eq$datetime, "tzone"), "UTC")
  expect_equal(eq$datetime[1], as.POSIXct("2025-01-07 08:00:00", tz = "UTC"))
  # 08:00 UTC = 00:00 PST
  expect_identical(eq$datetime_pst[1], "2025-01-07 00:00:00")
  expect_identical(eq$datetime_pst[2], "2025-01-07 04:00:00")
})

test_that("fetch_earthquakes returns an empty result untouched", {
  empty <- make_earthquakes_4326()[0, ]
  local_mock_usgs(empty)
  eq <- fetch_earthquakes(c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31", 9)
  expect_equal(nrow(eq), 0)
  expect_false("eq_order" %in% names(eq))
})

# ---- disaster_limits -------------------------------------------------------

test_that("disaster_limits returns the magnitude range of fetched earthquakes", {
  local_mock_usgs()
  expect_equal(disaster_limits(c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31"), c(2.7, 4.1))
})

test_that("disaster_limits passes its arguments (and default magnitude) through", {
  seen <- NULL
  local_mocked_bindings(fetch_earthquakes = function(...) {
    seen <<- list(...)
    data.frame(mag = c(3, NA, 5))
  })
  expect_equal(disaster_limits(c(1, 2), c(3, 4), "a", "b"), c(3, 5))
  expect_identical(seen, list(c(1, 2), c(3, 4), "a", "b", 2.5))
})

# ---- add_earthquake_layer (internal) ---------------------------------------

test_that("add_earthquake_layer adds points, letter labels and a magnitude scale", {
  local_mock_usgs()
  base <- ggplot2::ggplot()
  p <- disasterMaps:::add_earthquake_layer(
    base, c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31",
    min_magnitude = 2.5, disaster_limits = NULL, xlim = NULL, ylim = NULL
  )

  expect_identical(layer_geoms(p), c("GeomSf", "GeomText"))
  col <- p$scales$get_scales("colour")
  expect_identical(col$name, "Magnitude")
  expect_equal(col$limits, c(2.7, 4.1))
  expect_equal(sf::st_crs(p$coordinates$crs)$epsg, 3857)
})

test_that("add_earthquake_layer uses supplied disaster_limits and map limits", {
  local_mock_usgs()
  p <- disasterMaps:::add_earthquake_layer(
    ggplot2::ggplot(), c(-119, -117), c(33.5, 35), "2025-01-01", "2025-01-31",
    min_magnitude = 2.5, disaster_limits = c(2, 6), xlim = c(1, 2), ylim = c(3, 4)
  )
  expect_equal(p$scales$get_scales("colour")$limits, c(2, 6))
  expect_equal(p$coordinates$limits$x, c(1, 2))
  expect_equal(p$coordinates$limits$y, c(3, 4))
})

test_that("population_plot(disaster_type = 'earthquake') draws the earthquake layer", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mocked_bindings(get_tiles = function(...) NULL)
  local_mock_usgs()
  p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                       lon_limits = c(-119, -117), lat_limits = c(33.5, 35),
                       disaster_type = "earthquake",
                       start_date = "2025-01-01", end_date = "2025-01-31")
  expect_true("GeomText" %in% layer_geoms(p))
  expect_identical(p$scales$get_scales("colour")$name, "Magnitude")
})
