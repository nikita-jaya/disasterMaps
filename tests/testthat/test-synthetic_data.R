disasters <- c(fire = "US", earthquake = "TR")

read_synthetic <- function(type) {
  dir <- system.file("extdata", type, package = "disasterMaps")
  suppressWarnings(aggregate_csvs(dir))
}

for (type in names(disasters)) {

  test_that(paste(type, "data has one day at 0000, 0800 and 1600"), {
    dir <- system.file("extdata", type, package = "disasterMaps")
    files <- list.files(dir, pattern = "\\.csv$")
    expect_length(files, 3)
    expect_match(files, "_\\d{4}-\\d{2}-\\d{2}_(0000|0800|1600)\\.csv$")

    fb <- read_synthetic(type)
    expect_setequal(fb$hour, c("0000", "0800", "1600"))
    expect_length(unique(fb$ds), 1)
  })

  test_that(paste(type, "data matches the Meta column types"), {
    fb <- read_synthetic(type)
    meta_cols <- c("latitude", "longitude", "quadkey", "country", "date_time",
                   "n_baseline", "n_crisis", "n_difference", "density_baseline",
                   "density_crisis", "percent_change", "z_score", "ds")
    expect_true(all(meta_cols %in% names(fb)))
    types <- lapply(fb[meta_cols], class)
    expect_equal(types$quadkey, "character")
    expect_equal(types$country, "character")
    expect_equal(types$date_time, c("POSIXct", "POSIXt"))
    expect_equal(types$ds, "Date")
    num <- c("latitude", "longitude", "n_baseline", "n_crisis", "n_difference",
             "density_baseline", "density_crisis", "percent_change", "z_score")
    for (col in num) expect_equal(types[[col]], "numeric", info = col)
    expect_equal(unique(fb$country), disasters[[type]])
    expect_equal(fb$n_difference, fb$n_crisis - fb$n_baseline)
    # date_time is the window start, which the file name gives in Pacific time
    expect_equal(format(fb$date_time, "%H%M", tz = "America/Los_Angeles"),
                 fb$hour)
  })

  test_that(paste(type, "latitude/longitude are the centroid of each quadkey"), {
    fb <- read_synthetic(type)
    qk <- unique(fb[c("quadkey", "latitude", "longitude")])
    expect_equal(nrow(qk), length(unique(fb$quadkey)))
    expect_true(all(nchar(qk$quadkey) == 13))
    tiles <- quadkeyr::quadkey_df_to_polygon(qk["quadkey"])
    tiles <- tiles[match(qk$quadkey, tiles$quadkey), ]
    pts <- sf::st_as_sf(qk, coords = c("longitude", "latitude"), crs = 4326)
    expect_true(all(diag(sf::st_within(pts, tiles, sparse = FALSE))))
  })

  test_that(paste(type, "data works with build_tiles() and compute_windows()"), {
    fb <- read_synthetic(type)
    w <- compute_windows(fb, "America/Los_Angeles")
    expect_equal(w$first_hour, "0000")
    expect_equal(w$latest_hour, "1600")
    tiles <- suppressMessages(build_tiles(fb, re_run = TRUE,
                                          cache_dir = withr::local_tempdir()))
    expect_s3_class(tiles, "sf")
    expect_equal(nrow(tiles), nrow(fb))
  })
}
