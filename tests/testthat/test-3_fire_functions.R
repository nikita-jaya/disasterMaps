# The WFIGS service is never hit: the package's imported st_read is mocked.

local_mock_wfigs <- function(result = make_fires_4326(), .env = parent.frame()) {
  calls <- new.env()
  calls$urls <- character()
  local_mocked_bindings(
    st_read = function(dsn, ...) {
      calls$urls <- c(calls$urls, dsn)
      if (inherits(result, "error")) stop(result)
      result
    },
    .env = .env
  )
  calls
}

fire_args <- list(
  start_date = "2025-01-07",
  end_date   = "2025-01-31",
  lon_limits = c(-119, -117),
  lat_limits = c(33.5, 35)
)

call_fetch_fires <- function(dir, ...) {
  args <- utils::modifyList(fire_args, list(cache_dir = dir, ...))
  do.call(fetch_fires, args)
}

# ---- input validation ------------------------------------------------------

test_that("fetch_fires validates lon/lat limits", {
  dir <- withr::local_tempdir()
  local_mock_wfigs()
  expect_error(call_fetch_fires(dir, lon_limits = -118), "exactly two values")
  expect_error(call_fetch_fires(dir, lat_limits = c(1, 2, 3)), "exactly two values")
  expect_error(call_fetch_fires(dir, lon_limits = c(-117, -119)), "lon_limits must be c\\(min_longitude")
  expect_error(call_fetch_fires(dir, lat_limits = c(35, 35)), "lat_limits must be c\\(min_latitude")
})

test_that("fetch_fires validates dates", {
  dir <- withr::local_tempdir()
  local_mock_wfigs()
  expect_error(call_fetch_fires(dir, start_date = NA_character_), "must be valid dates")
  expect_error(call_fetch_fires(dir, start_date = "2025-02-01", end_date = "2025-01-01"),
               "start_date must be before end_date")
})

# ---- download --------------------------------------------------------------

test_that("fetch_fires builds an ArcGIS query with the date filter and bounding box", {
  dir <- withr::local_tempdir()
  calls <- local_mock_wfigs()
  suppressMessages(call_fetch_fires(dir))

  url <- utils::URLdecode(calls$urls)
  expect_length(url, 1)
  expect_match(url, "^https://services3.arcgis.com/.*/WFIGS_Interagency_Perimeters/FeatureServer/0/query\\?")
  expect_match(url, "where=attr_FireDiscoveryDateTime >= DATE '2025-01-07 00:00:00' AND attr_FireDiscoveryDateTime <= DATE '2025-01-31 00:00:00'", fixed = TRUE)
  expect_match(url, "&geometry=-119,33.5,-117,35&", fixed = TRUE)
  expect_match(url, "&inSR=4326", fixed = TRUE)
  expect_match(url, "&f=geojson$")
  # the raw URL is percent-encoded (no literal spaces or quotes)
  expect_no_match(calls$urls, "[ ']")
})

test_that("fetch_fires honours custom date_field and layer_url", {
  dir <- withr::local_tempdir()
  calls <- local_mock_wfigs()
  suppressMessages(call_fetch_fires(dir, date_field = "my_date", layer_url = "https://example.test/query"))
  url <- utils::URLdecode(calls$urls)
  expect_match(url, "^https://example.test/query\\?where=my_date >= DATE")
})

test_that("fetch_fires returns perimeters in EPSG:3857 with query metadata and caches them", {
  dir <- withr::local_tempdir()
  local_mock_wfigs()
  expect_message(f <- call_fetch_fires(dir), "Downloaded 2 fire perimeter")

  expect_s3_class(f, "sf")
  expect_equal(sf::st_crs(f)$epsg, 3857)
  expect_equal(nrow(f), 2)
  expect_s3_class(attr(f, "fetched_at"), "POSIXct")
  expect_equal(attr(f, "query_start"), as.POSIXct("2025-01-07", tz = "UTC"))
  expect_equal(attr(f, "query_end"), as.POSIXct("2025-01-31", tz = "UTC"))
  expect_identical(attr(f, "query_lon_limits"), c(-119, -117))
  expect_identical(attr(f, "query_lat_limits"), c(33.5, 35))
  expect_identical(attr(f, "query_date_field"), "attr_FireDiscoveryDateTime")
  expect_match(attr(f, "query_url"), "^https://")

  expect_true(file.exists(file.path(dir, "fires.Rds")))
  expect_equal(nrow(readRDS(file.path(dir, "fires.Rds"))), 2)
})

# ---- caching / fallback ----------------------------------------------------

test_that("fetch_fires returns the cache without downloading or validating", {
  dir <- withr::local_tempdir()
  saveRDS(make_fires_4326(), file.path(dir, "fires.Rds"))
  calls <- local_mock_wfigs()

  # even invalid limits are fine: the cache short-circuits everything
  expect_message(f <- call_fetch_fires(dir, lon_limits = 1), "Loading cached")
  expect_equal(nrow(f), 2)
  expect_length(calls$urls, 0)
})

test_that("fetch_fires re_run = TRUE re-downloads despite a cache", {
  dir <- withr::local_tempdir()
  saveRDS("stale", file.path(dir, "fires.Rds"))
  calls <- local_mock_wfigs()
  f <- suppressMessages(call_fetch_fires(dir, re_run = TRUE))
  expect_length(calls$urls, 1)
  expect_s3_class(f, "sf")
  expect_s3_class(readRDS(file.path(dir, "fires.Rds")), "sf")
})

test_that("fetch_fires falls back to the cache with a warning when the download fails", {
  dir <- withr::local_tempdir()
  saveRDS(make_fires_4326(), file.path(dir, "fires.Rds"))
  local_mock_wfigs(simpleError("timeout"))

  expect_warning(
    f <- suppressMessages(call_fetch_fires(dir, re_run = TRUE)),
    "Fire-perimeter download failed \\(timeout\\); using cached"
  )
  expect_equal(nrow(f), 2)
})

test_that("fetch_fires returns an empty EPSG:3857 sf with a warning when download fails and no cache exists", {
  dir <- withr::local_tempdir()
  local_mock_wfigs(simpleError("timeout"))

  expect_warning(
    f <- suppressMessages(call_fetch_fires(dir)),
    "no cache exists; continuing with no fire perimeters"
  )
  expect_s3_class(f, "sf")
  expect_equal(nrow(f), 0)
  expect_equal(sf::st_crs(f)$epsg, 3857)
  expect_false(file.exists(file.path(dir, "fires.Rds")))
})

# ---- add_fire_layer (internal) ---------------------------------------------

test_that("add_fire_layer always re-fetches and adds an outlined perimeter layer", {
  seen <- NULL
  local_mocked_bindings(fetch_fires = function(...) {
    seen <<- list(...)
    sf::st_transform(make_fires_4326(), 3857)
  })
  p <- disasterMaps:::add_fire_layer(ggplot2::ggplot(), "2025-01-07", "2025-01-31",
                                     c(-119, -117), c(33.5, 35))

  expect_true(seen$re_run)
  expect_identical(seen$start_date, "2025-01-07")
  expect_identical(seen$lon_limits, c(-119, -117))
  expect_identical(layer_geoms(p), "GeomSf")
  expect_true(is.na(p$layers[[1]]$aes_params$fill))
  expect_identical(rlang::as_label(p$layers[[1]]$mapping$colour), "\"Fire Perimeter\"")
})

test_that("population_plot(disaster_type = 'fire') draws fire perimeters over the population", {
  local_globals(tiles_3857 = make_tiles_3857())
  local_mocked_bindings(
    get_tiles = function(...) NULL,
    fetch_fires = function(...) sf::st_transform(make_fires_4326(), 3857)
  )
  p <- population_plot("2025-01-07", "0800", tzone = "UTC",
                       lon_limits = c(-119, -117), lat_limits = c(33.5, 35),
                       disaster_type = "fire",
                       start_date = "2025-01-07", end_date = "2025-01-31")
  expect_identical(sum(layer_geoms(p) == "GeomSf"), 2L)
  expect_true("poly_IncidentName" %in% names(p$layers[[2]]$data))
})
