disasters <- c(fire = "US", earthquake = "TR")

synthetic_dir <- function(type, kind) {
  system.file("extdata", type, kind, package = "disasterMaps")
}

read_synthetic <- function(type, kind = "population") {
  suppressWarnings(aggregate_csvs(synthetic_dir(type, kind)))
}

pop_cols <- c("latitude", "longitude", "quadkey", "country", "date_time",
              "n_baseline", "n_crisis", "n_difference", "density_baseline",
              "density_crisis", "percent_change", "z_score", "ds")
move_cols <- c("start_latitude", "start_longitude", "end_latitude",
               "end_longitude", "length_km", "start_quadkey", "end_quadkey",
               "country", "date_time", "n_baseline", "n_crisis",
               "n_difference", "percent_change", "z_score", "ds")

expect_meta_types <- function(df, cols) {
  types <- lapply(df[cols], class)
  for (col in cols) {
    expected <- if (grepl("quadkey|country", col)) "character"
      else if (col == "date_time") c("POSIXct", "POSIXt")
      else if (col == "ds") "Date"
      else "numeric"
    expect_equal(types[[col]], expected, info = col)
  }
}

# Every point should fall inside the tile its quadkey names
expect_centroids_in_tiles <- function(quadkey, lat, lon) {
  qk <- unique(data.frame(quadkey = quadkey, latitude = lat, longitude = lon))
  expect_equal(nrow(qk), length(unique(qk$quadkey)))
  expect_true(all(nchar(qk$quadkey) == 13))
  tiles <- quadkeyr::quadkey_df_to_polygon(qk["quadkey"])
  tiles <- tiles[match(qk$quadkey, tiles$quadkey), ]
  pts <- sf::st_as_sf(qk, coords = c("longitude", "latitude"), crs = 4326)
  expect_true(all(diag(sf::st_within(pts, tiles, sparse = FALSE))))
}

for (type in names(disasters)) {

  for (kind in c("population", "movement")) {
    test_that(paste(type, kind, "has one day at 0000 and 1600"), {
      files <- list.files(synthetic_dir(type, kind), pattern = "\\.csv$")
      expect_length(files, 2)
      expect_match(files, "_\\d{4}-\\d{2}-\\d{2}_(0000|1600)\\.csv$")

      df <- read_synthetic(type, kind)
      expect_setequal(df$hour, c("0000", "1600"))
      expect_length(unique(df$ds), 1)
      expect_equal(unique(df$country), disasters[[type]])
      expect_equal(df$n_difference, df$n_crisis - df$n_baseline)
      # date_time is the window start, which the file name gives in Pacific time
      expect_equal(format(df$date_time, "%H%M", tz = "America/Los_Angeles"),
                   df$hour)
    })
  }

  test_that(paste(type, "population matches the Meta column types"), {
    fb <- read_synthetic(type)
    expect_true(all(pop_cols %in% names(fb)))
    expect_meta_types(fb, pop_cols)
  })

  test_that(paste(type, "movement matches the Meta column types"), {
    mp <- read_synthetic(type, "movement")
    expect_true(all(move_cols %in% names(mp)))
    expect_meta_types(mp, move_cols)
  })

  test_that(paste(type, "population latitude/longitude are tile centroids"), {
    fb <- read_synthetic(type)
    expect_centroids_in_tiles(fb$quadkey, fb$latitude, fb$longitude)
  })

  test_that(paste(type, "movement matches population in time and place"), {
    fb <- read_synthetic(type)
    mp <- read_synthetic(type, "movement")
    expect_setequal(paste(mp$ds, mp$hour), paste(fb$ds, fb$hour))
    expect_true(all(c(mp$start_quadkey, mp$end_quadkey) %in% fb$quadkey))

    # Start/end coordinates are the same centroids as in the population data
    cen <- unique(fb[c("quadkey", "latitude", "longitude")])
    s <- match(mp$start_quadkey, cen$quadkey)
    e <- match(mp$end_quadkey, cen$quadkey)
    expect_equal(mp$start_latitude, cen$latitude[s])
    expect_equal(mp$start_longitude, cen$longitude[s])
    expect_equal(mp$end_latitude, cen$latitude[e])
    expect_equal(mp$end_longitude, cen$longitude[e])

    expect_true(all(mp$length_km >= 0))
    expect_true(all(mp$length_km[mp$start_quadkey == mp$end_quadkey] == 0))
  })

  test_that(paste(type, "data works with the cleaning pipeline"), {
    fb <- read_synthetic(type)
    w <- compute_windows(fb, "America/Los_Angeles")
    expect_equal(w$first_hour, "0000")
    expect_equal(w$latest_hour, "1600")
    tiles <- suppressMessages(load_population(synthetic_dir(type, "population"),
                                              re_run = TRUE,
                                              cache_dir = withr::local_tempdir()))
    expect_s3_class(tiles, "sf")
    expect_equal(nrow(tiles), nrow(fb))
    expect_identical(compute_windows(tiles, "America/Los_Angeles"), w)

    moved <- suppressMessages(load_movement(synthetic_dir(type, "movement"),
                                            re_run = TRUE,
                                            cache_dir = withr::local_tempdir()))
    expect_gt(nrow(moved), 0)
    expect_true(all(moved$start_quadkey != moved$end_quadkey))
  })
}
