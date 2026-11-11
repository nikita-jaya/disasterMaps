######------- Synthetic Facebook Population & Movement During Crisis data --------#######

# Builds the synthetic CSVs shipped in inst/extdata/: per disaster type, one
# day of population and movement data at the 0000 and 1600 windows:
#
#   inst/extdata/fire/population/la_wildfire_2025-01-08_{0000,1600}.csv
#   inst/extdata/fire/movement/la_wildfire_movement_2025-01-08_{...}.csv
#   inst/extdata/earthquake/population/turkey_earthquake_2023-02-06_{...}.csv
#   inst/extdata/earthquake/movement/turkey_earthquake_movement_2023-02-06_{...}.csv
#
# Meta's Data for Good crisis datasets are limited-access, so these stand in
# for them in tests and vignettes. They mimic the real files: same columns
# (13 for population, 15 for movement), quadkeys as text, nulls written as \N,
# and the date and window start time only in the file name (aggregate_csvs()
# turns those into `ds` and `hour`). Movement uses the same tiles, day, windows
# and country as population, and its flows come from the same displacement.
#
# Re-run from the package root to regenerate them:
#   source("data-raw/synthetic_crisis_data.R")
#
# Needs rnaturalearth + rnaturalearthdata (here only, not package imports),
# used to keep tiles on land and inside the right country.
#
# Assumptions about the real format:
#   * tiles are Bing tiles at zoom 13; latitude/longitude are the tile centroid
#   * the HHMM in the file name is Pacific time (format_time() assumes this)
#   * country is an ISO 3166-1 alpha-2 code
#   * counts under 10 are suppressed (\N); tiles where both counts are
#     suppressed are left out
#   * n_difference   = n_crisis - n_baseline
#   * percent_change = 100 * n_difference / n_baseline
#   * z_score        = n_difference / baseline standard deviation
#   * density_*      = tile count / total count over all tiles in that window
#   * movement rows are tile-to-tile journeys between consecutive windows,
#     including start == end rows for people who stayed put; length_km is the
#     great-circle distance between the two tile centroids; a pair with no
#     baseline (e.g. a new evacuation route) has n_baseline = \N

tz_files <- "America/Los_Angeles"

# ---------------------------------------------------------------------------
# Bing tile (Web Mercator) helpers, vectorised
# ---------------------------------------------------------------------------

# lon/lat -> tile X/Y at a zoom level
lonlat_to_tile <- function(lon, lat, zoom) {
  n <- 2^zoom
  lat_rad <- lat * pi / 180
  list(
    x = floor((lon + 180) / 360 * n),
    y = floor((1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * n)
  )
}

# tile X/Y -> lon/lat of the tile centre
tile_centroid <- function(x, y, zoom) {
  n <- 2^zoom
  list(
    lon = (x + 0.5) / n * 360 - 180,
    lat = atan(sinh(pi * (1 - 2 * (y + 0.5) / n))) * 180 / pi
  )
}

# tile X/Y -> quadkey string
tile_to_quadkey <- function(x, y, zoom) {
  vapply(seq_along(x), function(i) {
    digits <- vapply(zoom:1, function(z) {
      mask <- bitwShiftL(1L, z - 1L)
      as.character((bitwAnd(x[i], mask) != 0) + 2 * (bitwAnd(y[i], mask) != 0))
    }, character(1))
    paste(digits, collapse = "")
  }, character(1))
}

# great-circle distance in km between points
haversine_km <- function(lon1, lat1, lon2, lat2) {
  to_rad <- pi / 180
  a <- sin((lat2 - lat1) * to_rad / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin((lon2 - lon1) * to_rad / 2)^2
  2 * 6371.0088 * asin(sqrt(pmin(1, a)))
}

# approximate distance in km between points (fine at city scale)
km <- function(lon1, lat1, lon2, lat2) {
  dx <- (lon1 - lon2) * 111.32 * cos(lat1 * pi / 180)
  dy <- (lat1 - lat2) * 110.57
  sqrt(dx^2 + dy^2)
}

# ---------------------------------------------------------------------------
# Generator
# ---------------------------------------------------------------------------
#  name       file-name prefix
#  out_dir    folder to write to; population/ and movement/ go inside it
#  ds         the day ("YYYY-MM-DD")
#  hours      window start times, "HHMM", Pacific time
#  lon_limits, lat_limits  bounding box
#  country    ISO2 code; tiles whose centroid is outside it (sea or another
#             country) are dropped
#  centres    data frame lon, lat, peak (users per tile), sd_km: where people live
#  floor      users per tile away from the centres
#  sparse     optional function(lon, lat) -> logical, tiles to thin out
#  impacts    data frame lon, lat, radius_km, start (POSIXct), loss (fraction
#             who leave at the peak), ramp_hours (how long leaving takes)
#  absorb     share of displaced people who stay in the area, in a ring
#             around each impact zone (the rest leave the map)
#  trip_km    typical trip length for movement (distance decay), and
#  max_trip_km  the longest trip between tiles in the movement data
simulate_crisis <- function(name, out_dir, ds, hours = c("0000", "1600"),
                            lon_limits, lat_limits, country, centres,
                            floor = 300, sparse = NULL, impacts, absorb = 0.6,
                            trip_km = 8, max_trip_km = 40,
                            zoom = 13, min_users = 10, seed = 1) {
  set.seed(seed)

  # Tile grid over the bounding box, kept only on land in `country`
  nw <- lonlat_to_tile(lon_limits[1], lat_limits[2], zoom)
  se <- lonlat_to_tile(lon_limits[2], lat_limits[1], zoom)
  grid <- expand.grid(x = nw$x:se$x, y = nw$y:se$y)
  cen <- tile_centroid(grid$x, grid$y, zoom)
  grid$longitude <- cen$lon
  grid$latitude <- cen$lat

  land <- rnaturalearth::ne_countries(scale = "medium", country = NULL,
                                      returnclass = "sf")
  land <- land[land$iso_a2_eh == country, ]
  pts <- sf::st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326,
                      remove = FALSE)
  on_land <- lengths(sf::st_intersects(pts, sf::st_union(land))) > 0
  grid <- grid[on_land, , drop = FALSE]
  grid$quadkey <- tile_to_quadkey(grid$x, grid$y, zoom)

  # Baseline: urban centres on top of a low floor
  dens <- rep(floor, nrow(grid))
  for (i in seq_len(nrow(centres))) {
    d <- km(grid$longitude, grid$latitude, centres$lon[i], centres$lat[i])
    dens <- dens + centres$peak[i] * exp(-(d / centres$sd_km[i])^2 / 2)
  }
  if (!is.null(sparse)) {
    thin <- sparse(grid$longitude, grid$latitude)
    dens[thin] <- dens[thin] * 0.08
  }
  base_tile <- dens * stats::rlnorm(nrow(grid), 0, 0.35)

  impact_dist <- vapply(seq_len(nrow(impacts)), function(i)
    km(grid$longitude, grid$latitude, impacts$lon[i], impacts$lat[i]),
    numeric(nrow(grid)))
  impact_dist <- matrix(impact_dist, nrow = nrow(grid))

  # People are home at night and spread out in the day
  hour_factor <- c("0000" = 1.00, "0800" = 0.93, "1600" = 0.97)

  # Displacement at a moment in time: ramps up after each impact starts, then
  # slowly eases. Tiles inside an impact zone lose people (`frac` of them); a
  # ring around it gains some (`gained`).
  displacement <- function(date_time, base_mean) {
    frac_total <- rep(0, nrow(grid))
    ring_weight <- rep(0, nrow(grid))
    for (i in seq_len(nrow(impacts))) {
      hrs <- as.numeric(difftime(date_time, impacts$start[i], units = "hours"))
      if (hrs <= 0) next
      intensity <- min(1, hrs / impacts$ramp_hours[i]) *
        exp(-max(0, hrs - 48) / 120)
      d <- impact_dist[, i]
      r <- impacts$radius_km[i]
      inside <- exp(-(d / r)^4)  # flat top, sharp edge
      frac_total <- frac_total + impacts$loss[i] * intensity * inside
      ring_weight <- ring_weight +
        exp(-((d - 3 * r) / (2 * r))^2) * (1 - inside) * base_mean
    }
    lost <- base_mean * frac_total
    displaced <- sum(lost)
    gained <- if (sum(ring_weight) > 0)
      absorb * displaced * ring_weight / sum(ring_weight) else rep(0, nrow(grid))
    list(frac = frac_total, lost = lost, gained = gained)
  }

  pop_dir <- file.path(out_dir, "population")
  for (dir in c(pop_dir, file.path(out_dir, "movement"))) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    file.remove(list.files(dir, pattern = "\\.csv$", full.names = TRUE))
  }

  for (hr in hours) {
    date_time <- lubridate::ymd_hm(paste(ds, hr), tz = tz_files)
    base_mean <- base_tile * hour_factor[[hr]]
    n_baseline <- round(base_mean)
    sd_baseline <- pmax(1, sqrt(base_mean) * 1.8 + 0.04 * base_mean)

    disp <- displacement(date_time, base_mean)
    lost <- disp$lost
    gained <- disp$gained

    crisis_mean <- pmax(0, base_mean - lost + gained) *
      stats::rnorm(nrow(grid), 1, 0.04)
    n_crisis <- as.numeric(stats::rpois(nrow(grid), crisis_mean))

    n_baseline[n_baseline < min_users] <- NA
    n_crisis[n_crisis < min_users] <- NA
    keep <- !(is.na(n_baseline) & is.na(n_crisis))

    # Rounded so the CSVs stay small (readr writes 15+ digits otherwise)
    n_difference <- n_crisis - n_baseline
    df <- data.frame(
      latitude         = round(grid$latitude, 6),
      longitude        = round(grid$longitude, 6),
      quadkey          = grid$quadkey,
      country          = country,
      date_time        = date_time,
      n_baseline       = n_baseline,
      n_crisis         = n_crisis,
      n_difference     = n_difference,
      density_baseline = signif(n_baseline / sum(n_baseline, na.rm = TRUE), 6),
      density_crisis   = signif(n_crisis / sum(n_crisis, na.rm = TRUE), 6),
      percent_change   = round(100 * n_difference / n_baseline, 4),
      z_score          = round(n_difference / sd_baseline, 4),
      ds               = as.Date(ds),
      stringsAsFactors = FALSE
    )[keep, ]

    f <- file.path(pop_dir, sprintf("%s_%s_%s.csv", name, ds, hr))
    readr::write_csv(df, f, na = "\\N")
    message("Wrote ", nrow(df), " rows to ", f)
  }

  simulate_movement(
    grid = grid, base_tile = base_tile, displacement = displacement,
    name = name, out_dir = file.path(out_dir, "movement"), ds = ds,
    hours = hours, country = country, trip_km = trip_km,
    max_trip_km = max_trip_km, min_users = min_users, seed = seed + 1
  )
}

# ---------------------------------------------------------------------------
# Movement: tile-to-tile journeys during each 8-hour window
# ---------------------------------------------------------------------------
# Baseline journeys follow a gravity model: each tile sends a share of its
# people to other tiles, weighted by the destination's population and
# exp(-distance / trip_km). Most people stay put (start == end rows).
# During the crisis:
#   * fewer trips start in, or go to, evacuated tiles
#   * people who leave an impact zone in this window (the rise in displacement
#     since the previous window) travel to the tiles that gain people in the
#     population data, nearer ones first
simulate_movement <- function(grid, base_tile, displacement, name, out_dir, ds,
                              hours, country, trip_km, max_trip_km, min_users,
                              seed) {
  set.seed(seed)
  n <- nrow(grid)
  dist <- outer(seq_len(n), seq_len(n), function(i, j)
    haversine_km(grid$longitude[i], grid$latitude[i],
                 grid$longitude[j], grid$latitude[j]))
  near <- dist <= max_trip_km

  # Share of people making a trip to another tile, by window (overnight
  # 12am-8am is quiet, daytime is busy)
  trip_rate <- c("0000" = 0.06, "0800" = 0.18, "1600" = 0.15)
  hour_factor <- c("0000" = 1.00, "0800" = 0.93, "1600" = 0.97)

  for (hr in hours) {
    date_time <- lubridate::ymd_hm(paste(ds, hr), tz = tz_files)
    base_mean <- base_tile * hour_factor[[hr]]

    # Baseline flows (matrix, rows = start tile, cols = end tile)
    w <- (rep(1, n) %o% base_mean) * exp(-dist / trip_km) * near
    diag(w) <- 0
    w <- w / pmax(rowSums(w), 1e-12)
    base_flow <- trip_rate[[hr]] * base_mean * w
    diag(base_flow) <- (1 - trip_rate[[hr]]) * base_mean

    # Crisis flows
    now  <- displacement(date_time, base_mean)
    prev <- displacement(date_time - 8 * 3600, base_mean)
    keep_start <- pmax(0, 1 - now$frac)
    keep_end   <- pmax(0, 1 - 0.7 * now$frac)
    crisis_flow <- base_flow * keep_start * rep(keep_end, each = n)
    diag(crisis_flow) <- diag(base_flow) * keep_start

    leaving <- pmax(0, now$frac - prev$frac) * base_mean
    if (sum(leaving) > 0 && sum(now$gained) > 0) {
      to <- (rep(1, n) %o% now$gained) * exp(-dist / (2 * trip_km))
      diag(to) <- 0
      # Evacuees follow a few routes: keep each start tile's top 5 destinations
      origins <- which(leaving > 0)
      for (i in origins) {
        cut <- sort(to[i, ], decreasing = TRUE)[5]
        to[i, to[i, ] < cut] <- 0
      }
      to <- to / pmax(rowSums(to), 1e-12)
      share <- sum(now$gained) / sum(now$lost)  # same absorb share as population
      crisis_flow <- crisis_flow + share * leaving * to
    }

    # Pairs worth reporting: expected count near the suppression threshold
    idx <- which(base_flow >= min_users / 2 | crisis_flow >= min_users / 2,
                 arr.ind = TRUE)
    b <- base_flow[idx]
    cm <- crisis_flow[idx] * stats::rnorm(nrow(idx), 1, 0.05)
    n_baseline <- round(b)
    n_crisis <- as.numeric(stats::rpois(nrow(idx), pmax(0, cm)))
    sd_baseline <- pmax(1, sqrt(b) * 1.8 + 0.04 * b)

    n_baseline[n_baseline < min_users] <- NA
    n_crisis[n_crisis < min_users] <- NA
    keep <- !(is.na(n_baseline) & is.na(n_crisis))

    s <- idx[, 1]
    e <- idx[, 2]
    n_difference <- n_crisis - n_baseline
    df <- data.frame(
      start_latitude  = round(grid$latitude[s], 6),
      start_longitude = round(grid$longitude[s], 6),
      end_latitude    = round(grid$latitude[e], 6),
      end_longitude   = round(grid$longitude[e], 6),
      length_km       = round(dist[idx], 4),
      start_quadkey   = grid$quadkey[s],
      end_quadkey     = grid$quadkey[e],
      country         = country,
      date_time       = date_time,
      n_baseline      = n_baseline,
      n_crisis        = n_crisis,
      n_difference    = n_difference,
      percent_change  = round(100 * n_difference / n_baseline, 4),
      z_score         = round(n_difference / sd_baseline, 4),
      ds              = as.Date(ds),
      stringsAsFactors = FALSE
    )[keep, ]
    df <- df[order(df$start_quadkey, df$end_quadkey), ]

    f <- file.path(out_dir, sprintf("%s_movement_%s_%s.csv", name, ds, hr))
    readr::write_csv(df, f, na = "\\N")
    message("Wrote ", nrow(df), " rows to ", f)
  }
}

pst <- function(x) as.POSIXct(x, tz = tz_files)

# ---------------------------------------------------------------------------
# Fire: Los Angeles wildfires, January 8, 2025
# ---------------------------------------------------------------------------
# Palisades fire (started Jan 7, 10:30am) and Eaton fire (Jan 7, 6:18pm).
# Evacuation from each burn area ramps up over ~16 hours.
simulate_crisis(
  name       = "la_wildfire",
  out_dir    = "inst/extdata/fire",
  trip_km    = 5,
  max_trip_km = 30,
  ds         = "2025-01-08",
  lon_limits = c(-118.75, -117.95),
  lat_limits = c(33.85, 34.35),
  country    = "US",
  centres    = data.frame(
    lon   = c(-118.24, -118.14, -118.49, -118.19, -118.35, -118.45, -118.30, -118.03),
    lat   = c( 34.05,   34.15,   34.02,   33.77,   34.09,   34.20,   33.92,   34.07),
    peak  = c( 9000,    4500,    4000,    5000,    6000,    4500,    5500,    3500),
    sd_km = c(   10,       6,       5,       8,       7,       9,       9,       8)
  ),
  floor      = 400,
  # San Gabriel and Santa Monica mountains
  sparse     = function(lon, lat) lat > 34.22 | (lat > 34.08 & lon < -118.55),
  impacts    = data.frame(
    lon        = c(-118.545, -118.120),
    lat        = c(  34.065,   34.200),
    radius_km  = c(     6.5,      5.5),
    start      = pst(c("2025-01-07 10:30", "2025-01-07 18:18")),
    loss       = c(    0.85,     0.80),
    ramp_hours = c(      16,       16)
  ),
  absorb     = 0.6,
  seed       = 2025
)

# ---------------------------------------------------------------------------
# Earthquake: Kahramanmaras earthquakes, Turkey, February 6, 2023
# ---------------------------------------------------------------------------
# M7.8 near Pazarcik (Feb 6, 04:17 local = Feb 5, 5:17pm Pacific) and M7.5
# near Elbistan (Feb 6, 13:24 local = 2:24am Pacific). People leave damaged
# cities over ~30 hours; many go beyond the map (e.g. to Adana or Mersin).
simulate_crisis(
  name       = "turkey_earthquake",
  out_dir    = "inst/extdata/earthquake",
  trip_km    = 10,
  max_trip_km = 80,
  ds         = "2023-02-06",
  lon_limits = c(35.95, 38.55),
  lat_limits = c(35.85, 38.45),
  country    = "TR",
  centres    = data.frame(
    #          Gaziantep Kahramanmaras Antakya Iskenderun Adiyaman Osmaniye Kilis Malatya Elbistan Islahiye Pazarcik Nizip
    lon   = c(37.38,     36.93,        36.16,  36.17,     38.28,   36.25,   37.12, 38.31,  37.20,   36.63,   37.30,   37.80),
    lat   = c(37.07,     37.58,        36.20,  36.59,     37.76,   37.07,   36.72, 38.35,  38.20,   37.03,   37.49,   37.01),
    peak  = c( 7000,      4500,         3500,   3000,      3000,    2500,    1500,  4000,   1800,    1000,     900,   1200),
    sd_km = c(    8,         6,            5,      5,         5,       5,       4,     6,      4,       3,       3,      3)
  ),
  floor      = 25,
  impacts    = data.frame(
    #               Antakya Kahramanmaras Adiyaman Islahiye Pazarcik Iskenderun Malatya Elbistan Osmaniye
    lon        = c(36.16,   36.93,        38.28,   36.63,   37.30,   36.17,     38.31,  37.20,   36.25),
    lat        = c(36.21,   37.58,        37.76,   37.03,   37.49,   36.59,     38.35,  38.20,   37.07),
    radius_km  = c(    7,       6,            5,       4,       4,       4,         5,      4,       4),
    start      = pst(c(rep("2023-02-05 17:17", 7), "2023-02-06 02:24",
                       "2023-02-05 17:17")),
    loss       = c( 0.75,    0.60,         0.60,    0.55,    0.55,    0.40,      0.45,   0.50,    0.30),
    ramp_hours = rep(30, 9)
  ),
  absorb     = 0.35,
  seed       = 2023
)
