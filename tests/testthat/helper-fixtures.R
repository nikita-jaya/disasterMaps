# Shared fixtures for the test suite. testthat sources every helper-*.R file
# before running tests, so these are available in all test files.

# Several plotting functions read globals (tiles_3857, lon_limits, first_ds, ...)
# that the report / notebook normally assigns in the global environment. This
# helper sets them in .GlobalEnv for the duration of a test and restores
# whatever was there before.
local_globals <- function(..., .env = parent.frame()) {
  vals <- list(...)
  ge <- globalenv()
  for (nm in names(vals)) {
    local({
      name <- nm
      had <- exists(name, envir = ge, inherits = FALSE)
      old <- if (had) get(name, envir = ge, inherits = FALSE) else NULL
      assign(name, vals[[name]], envir = ge)
      withr::defer(
        if (had) assign(name, old, envir = ge) else rm(list = name, envir = ge),
        envir = .env
      )
    })
  }
  invisible()
}

# Four tiles around Los Angeles, observed in two time windows.
make_pop_data <- function() {
  base <- data.frame(
    quadkey   = c("0231", "0232", "0233", "0230"),
    longitude = c(-118.5, -118.0, -118.5, -118.0),
    latitude  = c(34.0, 34.0, 34.5, 34.5)
  )
  w1 <- base
  w1$ds <- as.Date("2025-01-07")
  w1$hour <- "0800"
  w1$n_crisis <- c(100, 200, 50, 20)
  w1$n_difference <- c(-10, 5, 30, -40)
  w1$z_score <- c(-1, 0.5, 2, -3)

  w2 <- base
  w2$ds <- as.Date("2025-01-08")
  w2$hour <- "1600"
  w2$n_crisis <- c(120, 180, 60, NA)
  w2$n_difference <- c(20, -5, NA, -60)
  w2$z_score <- c(1, -0.5, NA, -5)

  rbind(w1, w2)
}

# The plotting-ready object load_population() produces: sf polygons in EPSG:3857
# with the two user-facing column renames.
make_tiles_3857 <- function(pop = make_pop_data(), half_width = 0.2) {
  polys <- lapply(seq_len(nrow(pop)), function(k) {
    x <- pop$longitude[k]
    y <- pop$latitude[k]
    d <- half_width
    sf::st_polygon(list(rbind(
      c(x - d, y - d), c(x + d, y - d), c(x + d, y + d),
      c(x - d, y + d), c(x - d, y - d)
    )))
  })
  tiles <- sf::st_sf(pop, geometry = sf::st_sfc(polys, crs = 4326))
  tiles <- sf::st_transform(tiles, 3857)
  dplyr::rename(
    tiles,
    `Difference between baseline and crisis` = n_difference,
    `# Users During Crisis` = n_crisis
  )
}

# Earthquake features shaped like the USGS GeoJSON feed (time in epoch ms),
# deliberately out of chronological order.
make_earthquakes_4326 <- function() {
  times <- as.POSIXct(
    c("2025-01-07 12:00:00", "2025-01-07 08:00:00", "2025-01-07 20:00:00"),
    tz = "UTC"
  )
  sf::st_sf(
    mag  = c(4.1, 2.7, 3.3),
    time = as.numeric(times) * 1000,
    geometry = sf::st_sfc(
      sf::st_point(c(-118.2, 34.1)),
      sf::st_point(c(-118.4, 34.2)),
      sf::st_point(c(-118.1, 34.3)),
      crs = 4326
    )
  )
}

make_fires_4326 <- function() {
  sf::st_sf(
    poly_IncidentName = c("Alpha", "Bravo"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-118.6, 34.0), c(-118.5, 34.0), c(-118.5, 34.1), c(-118.6, 34.0)))),
      sf::st_polygon(list(rbind(c(-118.2, 34.2), c(-118.1, 34.2), c(-118.1, 34.3), c(-118.2, 34.2)))),
      crs = 4326
    )
  )
}

# Classes of the geoms in a ggplot's layers, e.g. "GeomSf", "GeomLabel".
layer_geoms <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
}
