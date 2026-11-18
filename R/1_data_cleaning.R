###### ------- Data Cleaning Functions --------#######


###### ----- Aggregate CSVs ------ ######

# Reads every .csv in one folder and stacks them into a single data frame.
# `path_parts` is passed to here::here(), so it can be one path or several
# parts (c("data", "fire")). Adds `source_file`, plus `ds` (Date) and `hour`
# ("HHMM") parsed from each file name. Exact duplicate rows are dropped
# unless dedupe = FALSE.

#' Title
#'
#' @param path_parts
#' @param output_name
#' @param dedupe
#' @param lat_col
#' @param lon_col
#' @param prefix
#' @param calculate_county_change
#' @param county_group_cols
#'
#' @returns
#' @keywords internal
#' @noRd
aggregate_csvs <- function(path_parts,
                           output_name = "aggregated.csv",
                           dedupe = TRUE,
                           lat_col = NULL,
                           lon_col = NULL,
                           prefix = "",
                           calculate_county_change = FALSE,
                           county_group_cols = c("county_geoid", "county_name", "county_state", "ds")) {

  data_dir <- do.call(here::here, as.list(path_parts))

  if (!dir.exists(data_dir)) {
    stop("Directory does not exist: ", data_dir)
  }

  csv_files <- list.files(
    path = data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", data_dir)
  }

  # Quadkeys are read as character so leading zeros survive. Only the quadkey
  # columns a file actually has are named, because readr warns about missing
  # ones: population CSVs have `quadkey`, movement CSVs have `start_quadkey`
  # and `end_quadkey`.
  quadkey_cols <- c("quadkey", "start_quadkey", "end_quadkey")

  combined_data <- lapply(csv_files, function(f) {
    header <- names(read_csv(f, n_max = 0, show_col_types = FALSE))
    present <- intersect(quadkey_cols, header)
    col_spec <- do.call(
      cols,
      c(stats::setNames(rep(list(col_character()), length(present)), present),
        list(.default = col_guess()))
    )
    # Meta writes nulls as "\N". Treat it (and blanks) as NA, otherwise a single
    # "\N" turns a numeric column into character.
    read_csv(
      f,
      col_types = col_spec,
      show_col_types = FALSE,
      na = c("", "NA", "\\N")
    ) |>
      mutate(source_file = basename(f))
  }) |>
    bind_rows()

  if (dedupe) {
    combined_data <- combined_data |> distinct()
  }

  # File names look like <prefix>_YYYY-MM-DD_HHMM.csv
  combined_data <- combined_data |>
    mutate(
      ds = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", source_file)),
      hour = sub(".*_(\\d{4}-\\d{2}-\\d{2})_(\\d{4}).*", "\\2", source_file)
    )

  return(combined_data)
}


# Quick health check of a data frame. Returns a list with the share of NAs
# per column, the mean and sd of every numeric column (ignoring NAs), and the
# number of rows and columns. An sf (such as load_population()'s result) is
# checked without its geometry column.

#' Title
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
analyze_dataset <- function(df) {
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  list(
    missing = df |>
      summarise(across(everything(), ~mean(is.na(.)))),

    numeric_summary = df |>
      summarise(across(where(is.numeric),
                       list(mean = \(x) mean(x, na.rm = TRUE),
                            sd   = \(x) sd(x, na.rm = TRUE)))),

    n_rows = nrow(df),
    n_cols = ncol(df)
  )
}


###### ----- Cached Pipeline Steps ------ ######

# load_population() and load_movement() each read their cached .Rds if it
# exists, and otherwise build from the raw CSVs and save the cache. Pass
# re_run = TRUE to force a rebuild, e.g. after adding new CSVs. The plot
# functions expect the results in the globals `tiles_3857` and `moved`.

# Folder for the .Rds caches. To build a fresh set without overwriting the
# current one, pass another folder as `cache_dir`:
#   run_dir    <- format(Sys.time(), "rds_%Y%m%d_%H%M%S")
#   tiles_3857 <- load_population(re_run = TRUE, cache_dir = run_dir)
rds_cache_dir <- "."

# Returns <dir>/<file>, creating <dir> if needed.

#' Title
#'
#' @param file
#' @param dir
#'
#' @returns
#' @keywords internal
#' @noRd
cache_path <- function(file, dir = rds_cache_dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, file)
}

# Population tiles as EPSG:3857 polygons, one row per quadkey and time window,
# with the metric columns renamed for the plots.

#' Title
#'
#' @param path_parts
#' @param re_run
#' @param cache_dir
#' @param cache
#'
#' @returns
#' @export
#'
#' @examples
load_population <- function(path_parts = "fb_pop_crisis_bing_tiles",
                            re_run = FALSE, cache_dir = rds_cache_dir,
                            cache = "tiles.Rds") {
  path <- cache_path(cache, cache_dir)
  if (!re_run && file.exists(path)) {
    message("Loading cached ", path)
    return(readRDS(path))
  }

  # NA in n_difference is kept on purpose: it means fewer than 11 users.
  fb <- aggregate_csvs(path_parts)

  # A tile's polygon depends only on its quadkey, not the time window, so each
  # distinct quadkey is polygonised once and joined back to every row. This is
  # far faster than polygonising every row of a multi-window dataset.
  qk <- dplyr::distinct(fb, quadkey)
  qk <- qk[!is.na(qk$quadkey) & nzchar(qk$quadkey), , drop = FALSE]
  message("Building tile polygons for ", nrow(qk), " distinct quadkeys ",
          "(", nrow(fb), " rows total) -> ", path)

  geom  <- quadkey_df_to_polygon(qk)["quadkey"]   # keep only quadkey + geometry
  tiles <- sf::st_as_sf(dplyr::left_join(geom, fb, by = "quadkey")) |>
    sf::st_transform(3857) |>
    dplyr::rename(`Difference between baseline and crisis` = n_difference,
                  `# Users During Crisis` = n_crisis)

  saveRDS(tiles, path)
  tiles
}

# Movement between tiles, keeping only rows where the start and end differ,
# with the metric columns renamed for the plots.

#' Title
#'
#' @param path_parts
#' @param re_run
#' @param cache_dir
#' @param cache
#'
#' @returns
#' @export
#'
#' @examples
load_movement <- function(path_parts = "fb_move_crisis_bing_tiles",
                          re_run = FALSE, cache_dir = rds_cache_dir,
                          cache = "moved.Rds") {
  path <- cache_path(cache, cache_dir)
  if (!re_run && file.exists(path)) {
    message("Loading cached ", path)
    return(readRDS(path))
  }

  message("Building movement data from CSVs -> ", path)
  moved <- aggregate_csvs(path_parts) |>
    # Keep a move if EITHER coordinate changed, so pure east/west or
    # north/south moves are not dropped.
    filter(start_longitude != end_longitude | start_latitude != end_latitude) |>
    # NA in n_difference is kept on purpose: it means fewer than 11 users.
    rename(`Difference between baseline and crisis` = n_difference,
           `# Users During Crisis` = n_crisis)

  saveRDS(moved, path)
  moved
}


###### ----- Basemap ------ ######

# Local cache for basemap tiles, so repeated renders reuse them and a warmed
# cache lets the report render offline.
tile_cache_dir <- "maptiles_cache"

# Lowers the basemap zoom until the extent fits in roughly `max_tiles` tiles.
# A wide extent at a high zoom means thousands of tiles, which stitch into a
# huge raster and can crash R. Returns a zoom no higher than the one requested.

#' Title
#'
#' @param lon
#' @param lat
#' @param zoom
#' @param max_tiles
#'
#' @returns
#' @keywords internal
#' @noRd
fit_zoom <- function(lon, lat, zoom, max_tiles = 250) {
  dlon <- abs(diff(range(lon))); dlat <- abs(diff(range(lat)))
  z <- zoom
  while (z > 1) {
    deg_per_tile <- 360 / (2^z)
    tx <- max(1, ceiling(dlon / deg_per_tile))
    ty <- max(1, ceiling(dlat / deg_per_tile))   # rough but conservative
    if (tx * ty <= max_tiles) break
    z <- z - 1
  }
  if (z < zoom)
    message("fit_zoom(): large extent (~", round(dlon, 2), " x ", round(dlat, 2),
            " deg); reduced basemap zoom ", zoom, " -> ", z, " to stay within ~",
            max_tiles, " tiles.")
  z
}


###### ----- Time Windows ------ ######

# Formats a Meta timestamp (Pacific time) in `tzone`, e.g. "January 7, 2025 8am".

#' Title
#'
#' @param ds
#' @param hour
#' @param tzone
#'
#' @returns
#' @keywords internal
#' @noRd
format_time <- function(ds, hour, tzone) {
  datetime <- ymd_hm(
    paste(ds, hour),
    tz = "America/Los_Angeles"
  )

  datetime_local <- with_tz(
    datetime,
    tzone = tzone
  )

  formatted <- format(
    datetime_local,
    "%B %d, %Y %I%p"
  )

  # Drop leading zeros and lower-case am/pm
  formatted <- gsub(" 0", " ", formatted)
  formatted <- gsub("AM", "am", formatted)
  formatted <- gsub("PM", "pm", formatted)

  formatted
}

# Earliest and latest time windows in the data, with readable labels. Accepts
# a plain data frame or the sf returned by load_population().

#' Title
#'
#' @param fb_data_bing
#' @param tzone
#'
#' @returns
#' @export
#'
#' @examples
compute_windows <- function(fb_data_bing, tzone) {
  if (inherits(fb_data_bing, "sf")) fb_data_bing <- sf::st_drop_geometry(fb_data_bing)
  aw <- fb_data_bing |>
    dplyr::distinct(ds, hour) |>
    dplyr::arrange(ds, hour)
  stopifnot("No population data loaded." = nrow(aw) > 0)
  fw <- dplyr::slice(aw, 1)
  lw <- dplyr::slice(aw, dplyr::n())
  list(
    first_ds     = as.character(fw$ds), first_hour  = fw$hour,
    latest_ds    = as.character(lw$ds), latest_hour = lw$hour,
    first_label  = format_time(as.character(fw$ds), fw$hour, tzone = tzone),
    latest_label = format_time(as.character(lw$ds), lw$hour, tzone = tzone)
  )
}
