# ---- aggregate_csvs --------------------------------------------------------

# Write CSVs into a temp folder and point here::here() at it.
local_csv_dir <- function(files, .env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .env)
  dir.create(file.path(root, "data"))
  for (nm in names(files)) {
    writeLines(files[[nm]], file.path(root, "data", nm))
  }
  local_mocked_bindings(
    here = function(...) file.path(root, ...),
    .package = "here",
    .env = .env
  )
  root
}

test_that("aggregate_csvs stops when the directory is missing", {
  local_csv_dir(list())
  expect_error(aggregate_csvs("does_not_exist"), "Directory does not exist")
})

test_that("aggregate_csvs stops when the directory has no CSVs", {
  local_csv_dir(list("notes.txt" = "hello"))
  expect_error(aggregate_csvs("data"), "No CSV files found")
})

test_that("aggregate_csvs combines files and parses ds / hour from file names", {
  local_csv_dir(list(
    "LA_2025-01-07_0800.csv" = c("quadkey,n_crisis", "0231,10", "0232,20"),
    "LA_2025-01-08_1600.csv" = c("quadkey,n_crisis", "0231,15")
  ))
  out <- aggregate_csvs("data")

  expect_equal(nrow(out), 3)
  expect_setequal(out$source_file, c("LA_2025-01-07_0800.csv", "LA_2025-01-08_1600.csv"))
  expect_s3_class(out$ds, "Date")
  expect_setequal(as.character(out$ds), c("2025-01-07", "2025-01-08"))
  expect_setequal(out$hour, c("0800", "1600"))
  expect_identical(out$hour[out$source_file == "LA_2025-01-08_1600.csv"], "1600")
})

test_that("aggregate_csvs reads quadkeys as character, keeping leading zeros", {
  local_csv_dir(list(
    "LA_2025-01-07_0800.csv" = c(
      "quadkey,start_quadkey,end_quadkey,n",
      "0231,0012,0300,1"
    )
  ))
  out <- aggregate_csvs("data")
  expect_type(out$quadkey, "character")
  expect_identical(out$quadkey, "0231")
  expect_identical(out$start_quadkey, "0012")
  expect_identical(out$end_quadkey, "0300")
})

test_that("aggregate_csvs reads population-style CSVs without warnings", {
  local_csv_dir(list(
    "LA_2025-01-07_0800.csv" = c("quadkey,n_crisis", "0231,10")
  ))
  expect_no_warning(out <- aggregate_csvs("data"))
  expect_identical(out$quadkey, "0231")
})

test_that("aggregate_csvs reads movement-style CSVs without warnings", {
  local_csv_dir(list(
    "LA_2025-01-07_0800.csv" = c("start_quadkey,end_quadkey,n_crisis", "0012,0300,5")
  ))
  expect_no_warning(out <- aggregate_csvs("data"))
  expect_identical(out$start_quadkey, "0012")
  expect_identical(out$end_quadkey, "0300")
  expect_false("quadkey" %in% names(out))
})

test_that("aggregate_csvs reads the bundled synthetic data without warnings", {
  for (type in c("fire", "earthquake")) {
    for (kind in c("population", "movement")) {
      dir <- system.file("extdata", type, kind, package = "disasterMaps")
      expect_no_warning(aggregate_csvs(dir), message = paste(type, kind))
    }
  }
})

test_that("aggregate_csvs treats \\N and blanks as NA so numeric columns stay numeric", {
  local_csv_dir(list(
    "LA_2025-01-07_0800.csv" = c("quadkey,n_crisis,n_difference", "0231,\\N,5", "0232,7,")
  ))
  out <- aggregate_csvs("data")
  expect_type(out$n_crisis, "double")
  expect_type(out$n_difference, "double")
  expect_identical(out$n_crisis, c(NA, 7))
  expect_identical(out$n_difference, c(5, NA))
})

test_that("aggregate_csvs dedupes exact duplicate rows only when asked", {
  files <- list(
    "LA_2025-01-07_0800.csv" = c("quadkey,n", "0231,1", "0231,1", "0232,2")
  )
  local_csv_dir(files)
  expect_equal(nrow(aggregate_csvs("data")), 2)
  expect_equal(nrow(aggregate_csvs("data", dedupe = FALSE)), 3)
})

test_that("aggregate_csvs passes multi-part paths to here::here", {
  root <- local_csv_dir(list())
  dir.create(file.path(root, "a", "b"), recursive = TRUE)
  writeLines(c("quadkey,n", "0231,1"), file.path(root, "a", "b", "X_2025-01-07_0000.csv"))
  out <- aggregate_csvs(c("a", "b"))
  expect_equal(nrow(out), 1)
  expect_identical(out$hour, "0000")
})

# ---- analyze_dataset -------------------------------------------------------

test_that("analyze_dataset reports missingness, numeric summaries and shape", {
  df <- data.frame(
    a = c(1, 2, NA, 5),
    b = c("x", NA, NA, "y"),
    c = c(10, 10, 10, 10)
  )
  expect_no_warning(res <- analyze_dataset(df))

  expect_named(res, c("missing", "numeric_summary", "n_rows", "n_cols"))
  expect_equal(res$n_rows, 4)
  expect_equal(res$n_cols, 3)

  expect_equal(res$missing$a, 0.25)
  expect_equal(res$missing$b, 0.5)
  expect_equal(res$missing$c, 0)

  expect_setequal(names(res$numeric_summary), c("a_mean", "a_sd", "c_mean", "c_sd"))
  expect_equal(res$numeric_summary$a_mean, mean(c(1, 2, 5)))
  expect_equal(res$numeric_summary$a_sd, sd(c(1, 2, 5)))
  expect_equal(res$numeric_summary$c_sd, 0)
})

# ---- cache_path (internal) -------------------------------------------------

test_that("cache_path builds the path and creates the directory", {
  root <- withr::local_tempdir()
  dir <- file.path(root, "nested", "cache")
  expect_false(dir.exists(dir))
  p <- disasterMaps:::cache_path("x.Rds", dir)
  expect_identical(p, file.path(dir, "x.Rds"))
  expect_true(dir.exists(dir))
})

# ---- load_population -------------------------------------------------------

test_that("load_population reads an existing cache without rebuilding", {
  dir <- withr::local_tempdir()
  cached <- data.frame(quadkey = "0231", n = 1)
  saveRDS(cached, file.path(dir, "tiles.Rds"))
  local_mocked_bindings(aggregate_csvs = function(...) stop("should not rebuild"))

  expect_message(out <- load_population(cache_dir = dir), "Loading cached")
  expect_identical(out, cached)
})

test_that("load_population reads the population folder by default", {
  dir <- withr::local_tempdir()
  seen <- NULL
  local_mocked_bindings(aggregate_csvs = function(path_parts, ...) {
    seen <<- path_parts
    make_pop_data()
  })

  suppressMessages(load_population(cache_dir = dir))
  expect_identical(seen, "fb_pop_crisis_bing_tiles")
})

test_that("load_population polygonises quadkeys into an EPSG:3857 sf with renamed columns", {
  dir <- withr::local_tempdir()
  pop <- make_pop_data()
  local_mocked_bindings(aggregate_csvs = function(...) pop)

  expect_message(
    tiles <- load_population(cache_dir = dir),
    "4 distinct quadkeys \\(8 rows total\\)"
  )

  expect_s3_class(tiles, "sf")
  expect_equal(sf::st_crs(tiles)$epsg, 3857)
  expect_equal(nrow(tiles), nrow(pop))
  expect_true(all(sf::st_geometry_type(tiles) == "POLYGON"))
  expect_true(all(c("Difference between baseline and crisis", "# Users During Crisis") %in% names(tiles)))
  expect_false(any(c("n_difference", "n_crisis") %in% names(tiles)))
  expect_setequal(tiles$quadkey, unique(pop$quadkey))
})

test_that("load_population drops rows with missing or empty quadkeys", {
  dir <- withr::local_tempdir()
  pop <- make_pop_data()
  pop$quadkey[1] <- NA
  pop$quadkey[2] <- ""
  local_mocked_bindings(aggregate_csvs = function(...) pop)

  tiles <- suppressMessages(load_population(cache_dir = dir))
  expect_false(any(is.na(tiles$quadkey) | tiles$quadkey == ""))
  expect_equal(nrow(tiles), nrow(pop) - 2)
})

test_that("load_population caches the finished tiles and reuses them", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(aggregate_csvs = function(...) make_pop_data())
  first <- suppressMessages(load_population(cache_dir = dir))
  expect_identical(readRDS(file.path(dir, "tiles.Rds")), first)

  local_mocked_bindings(aggregate_csvs = function(...) stop("should not rebuild"))
  expect_message(second <- load_population(cache_dir = dir), "Loading cached")
  expect_identical(second, first)
  expect_equal(sf::st_crs(second)$epsg, 3857)
})

test_that("load_population re_run = TRUE ignores an existing cache", {
  dir <- withr::local_tempdir()
  saveRDS(data.frame(old = 1), file.path(dir, "tiles.Rds"))
  local_mocked_bindings(aggregate_csvs = function(...) make_pop_data())

  out <- suppressMessages(load_population(re_run = TRUE, cache_dir = dir))
  expect_s3_class(out, "sf")
  expect_s3_class(readRDS(file.path(dir, "tiles.Rds")), "sf")
})

# ---- load_movement ---------------------------------------------------------

make_movement <- function() {
  data.frame(
    start_longitude = c(-118, -118, -118, -117),
    end_longitude   = c(-118, -117, -118, -117),
    start_latitude  = c(34, 34, 34, 35),
    end_latitude    = c(34, 34, 35, 35),
    n_difference    = c(1, 2, NA, 4),
    n_crisis        = c(10, 20, 30, 40)
  )
}

test_that("load_movement reads the movement folder by default", {
  dir <- withr::local_tempdir()
  seen <- NULL
  local_mocked_bindings(aggregate_csvs = function(path_parts, ...) {
    seen <<- path_parts
    make_movement()
  })

  expect_message(load_movement(cache_dir = dir), "Building movement data")
  expect_identical(seen, "fb_move_crisis_bing_tiles")
})

test_that("load_movement keeps moves where EITHER coordinate changed", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(aggregate_csvs = function(...) make_movement())
  out <- suppressMessages(load_movement(cache_dir = dir))

  # rows 1 and 4 (no change) dropped; E/W-only and N/S-only kept
  expect_equal(nrow(out), 2)
  expect_equal(out$end_longitude, c(-117, -118))
  expect_equal(out$end_latitude, c(34, 35))
})

test_that("load_movement renames the metric columns and keeps explicit NA", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(aggregate_csvs = function(...) make_movement())
  out <- suppressMessages(load_movement(cache_dir = dir))

  expect_true(all(c("Difference between baseline and crisis", "# Users During Crisis") %in% names(out)))
  expect_false(any(c("n_difference", "n_crisis") %in% names(out)))
  expect_true(anyNA(out$`Difference between baseline and crisis`))
})

test_that("load_movement caches its result and reuses it", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(aggregate_csvs = function(...) make_movement())
  first <- suppressMessages(load_movement(cache_dir = dir))
  expect_identical(readRDS(file.path(dir, "moved.Rds")), first)

  local_mocked_bindings(aggregate_csvs = function(...) stop("should not rebuild"))
  expect_message(second <- load_movement(cache_dir = dir), "Loading cached")
  expect_identical(second, first)
})

test_that("load_movement re_run = TRUE ignores an existing cache", {
  dir <- withr::local_tempdir()
  saveRDS(data.frame(old = 1), file.path(dir, "moved.Rds"))
  local_mocked_bindings(aggregate_csvs = function(...) make_movement())

  out <- suppressMessages(load_movement(re_run = TRUE, cache_dir = dir))
  expect_equal(nrow(out), 2)
})

test_that("load_* honour a custom cache file name", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(aggregate_csvs = function(...) make_movement())
  suppressMessages(load_movement(cache_dir = dir, cache = "custom.Rds"))
  expect_true(file.exists(file.path(dir, "custom.Rds")))
})

# ---- fit_zoom --------------------------------------------------------------

test_that("fit_zoom keeps the requested zoom for a small extent", {
  expect_silent(z <- fit_zoom(c(-118.5, -118.0), c(34.0, 34.5), zoom = 9))
  expect_identical(z, 9)
})

test_that("fit_zoom lowers the zoom for a large extent and says so", {
  lon <- c(-125, -114)
  lat <- c(32, 42)
  expect_message(z <- fit_zoom(lon, lat, zoom = 12), "reduced basemap zoom 12")
  expect_lt(z, 12)

  tiles_at <- function(z) {
    deg <- 360 / 2^z
    max(1, ceiling(diff(lon) / deg)) * max(1, ceiling(diff(lat) / deg))
  }
  expect_lte(tiles_at(z), 250)
  # and it is the highest zoom that fits the budget
  expect_gt(tiles_at(z + 1), 250)
})

test_that("fit_zoom respects a custom tile budget and never goes below 1", {
  z_small <- suppressMessages(fit_zoom(c(-120, -110), c(30, 40), zoom = 10, max_tiles = 4))
  z_big   <- suppressMessages(fit_zoom(c(-120, -110), c(30, 40), zoom = 10, max_tiles = 1000))
  expect_lt(z_small, z_big)

  expect_identical(suppressMessages(fit_zoom(c(-180, 180), c(-85, 85), zoom = 5, max_tiles = 1)), 1)
})

test_that("fit_zoom is order-independent for lon/lat inputs", {
  expect_identical(
    suppressMessages(fit_zoom(c(-110, -125), c(42, 32), zoom = 12)),
    suppressMessages(fit_zoom(c(-125, -110), c(32, 42), zoom = 12))
  )
})

# ---- format_time -----------------------------------------------------------

test_that("format_time formats Pacific time without leading zeros", {
  withr::local_locale(c(LC_TIME = "C"))
  expect_identical(format_time("2025-01-07", "0800", tzone = "America/Los_Angeles"),
                   "January 7, 2025 8am")
  expect_identical(format_time("2025-01-07", "1600", tzone = "America/Los_Angeles"),
                   "January 7, 2025 4pm")
})

test_that("format_time converts from Pacific to the requested time zone", {
  withr::local_locale(c(LC_TIME = "C"))
  # 08:00 PST = 16:00 UTC
  expect_identical(format_time("2025-01-07", "0800", tzone = "UTC"), "January 7, 2025 4pm")
  # 20:00 PST rolls over to the next day in UTC
  expect_identical(format_time("2025-01-07", "2000", tzone = "UTC"), "January 8, 2025 4am")
})

test_that("format_time handles midnight, two-digit days and vectors", {
  withr::local_locale(c(LC_TIME = "C"))
  expect_identical(format_time("2025-01-15", "0000", tzone = "America/Los_Angeles"),
                   "January 15, 2025 12am")
  expect_identical(
    format_time(c("2025-01-07", "2025-01-10"), c("0800", "1200"), tzone = "America/Los_Angeles"),
    c("January 7, 2025 8am", "January 10, 2025 12pm")
  )
})

test_that("format_time accepts a Date for ds", {
  withr::local_locale(c(LC_TIME = "C"))
  expect_identical(format_time(as.Date("2025-01-07"), "0800", tzone = "America/Los_Angeles"),
                   "January 7, 2025 8am")
})

# ---- compute_windows -------------------------------------------------------

test_that("compute_windows returns the earliest and latest windows with labels", {
  withr::local_locale(c(LC_TIME = "C"))
  pop <- make_pop_data()
  w <- compute_windows(pop[nrow(pop):1, ], tzone = "America/Los_Angeles")  # unsorted input

  expect_identical(w$first_ds, "2025-01-07")
  expect_identical(w$first_hour, "0800")
  expect_identical(w$latest_ds, "2025-01-08")
  expect_identical(w$latest_hour, "1600")
  expect_identical(w$first_label, "January 7, 2025 8am")
  expect_identical(w$latest_label, "January 8, 2025 4pm")
})

test_that("compute_windows orders hours within the same day", {
  pop <- data.frame(ds = as.Date("2025-01-07"), hour = c("1600", "0000", "0800"))
  w <- compute_windows(pop, tzone = "UTC")
  expect_identical(w$first_hour, "0000")
  expect_identical(w$latest_hour, "1600")
})

test_that("compute_windows accepts the sf from load_population", {
  dir <- withr::local_tempdir()
  pop <- make_pop_data()
  local_mocked_bindings(aggregate_csvs = function(...) pop)
  tiles <- suppressMessages(load_population(cache_dir = dir))

  expect_identical(compute_windows(tiles, tzone = "UTC"),
                   compute_windows(pop, tzone = "UTC"))
})

test_that("compute_windows fails loudly on empty data", {
  empty <- make_pop_data()[0, ]
  expect_error(compute_windows(empty, tzone = "UTC"), "No population data loaded")
})
