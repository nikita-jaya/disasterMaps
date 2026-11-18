# Disaster-agnostic map context layers for population_plot(): shaded-relief
# terrain, OpenStreetMap rivers and roads, U.S. state borders and city labels.
# Nothing here knows about hurricanes, fires or earthquakes; disaster-specific
# layers are added through add_map_layers()'s `under_pop` / `over_pop` hooks.
#
# Every `map_bbox` below is a named lon/lat vector
# c(xmin = , ymin = , xmax = , ymax = ) in EPSG:4326.


# Downloads OpenStreetMap line features (e.g. rivers, roads) with `key` equal
# to any of `values` inside map_bbox, and returns them as an sf in EPSG:3857
# with only the `key` column. Results are cached in cache_dir as
# "<tag>_<bbox>.rds". Tries three Overpass servers in turn (three rounds);
# returns NULL, with a message, if none of them responds. Returns NULL straight
# away when map_bbox is NULL.

#' Title
#'
#' @param map_bbox
#' @param key
#' @param values
#' @param tag
#' @param cache_dir
#'
#' @returns
#' @export
#'
#' @examples
osm_lines_cached <- function(map_bbox, key, values, tag, cache_dir) {
  if (is.null(map_bbox)) return(NULL)

  # ------------------------------------------------------------
  # Cache
  # ------------------------------------------------------------

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  bb_tag <- paste(round(unname(map_bbox), 2), collapse = "_")
  f <- file.path(cache_dir, paste0(tag, "_", bb_tag, ".rds"))

  if (file.exists(f)) return(readRDS(f))


  # ------------------------------------------------------------
  # Download, failing over between Overpass servers
  # ------------------------------------------------------------

  servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://overpass.private.coffee/api/interpreter"
  )

  out <- NULL

  for (srv in rep(servers, 3)) {
    if (!is.null(out)) break

    message("Downloading OSM ", tag, " from ",
            sub("https://([^/]+)/.*", "\\1", srv), " ...")

    res <- tryCatch({
      osmdata::set_overpass_url(srv)

      q <- osmdata::opq(bbox = unname(map_bbox), timeout = 180) |>
        osmdata::add_osm_feature(key = key, value = values)

      x <- osmdata::osmdata_sf(q)$osm_lines

      if (is.null(x) || nrow(x) == 0) {
        "empty"
      } else {
        sf::st_transform(x[, key, drop = FALSE], 3857)
      }
    }, error = function(e) {
      message("   failed: ", substr(conditionMessage(e), 1, 120))
      NULL
    })

    if (inherits(res, "sf")) { out <- res; break }
    if (identical(res, "empty")) message("   server returned 0 features")

    Sys.sleep(3)
  }


  # ------------------------------------------------------------
  # Save
  # ------------------------------------------------------------

  if (is.null(out)) {
    message("*** ", tag, " could NOT be downloaded; layer omitted. Re-run later.")
  } else {
    saveRDS(out, f)
  }

  out
}


# Shaded relief for map_bbox. Elevation is downloaded once (AWS tiles at
# `zoom`, via elevatr) and cached in cache_dir as a GeoTIFF, then reprojected
# to EPSG:3857 and hill-shaded with the sun at shade_angle (degrees above the
# horizon) and shade_direction (degrees from north). Large rasters are
# aggregated to about 200,000 cells to keep plotting fast. Returns a list of
# `elevation` and `shade` (terra rasters) and `shade_df` (x, y, shade data
# frame for geom_raster()), or NULL when map_bbox is NULL.

#' Title
#'
#' @param map_bbox
#' @param zoom
#' @param cache_dir
#' @param shade_angle
#' @param shade_direction
#'
#' @returns
#' @export
#'
#' @examples
terrain_hillshade <- function(map_bbox, zoom, cache_dir,
                              shade_angle = 40, shade_direction = 315) {
  if (is.null(map_bbox)) return(NULL)

  # ------------------------------------------------------------
  # Elevation (downloaded once, then cached)
  # ------------------------------------------------------------

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  bb_tag <- paste(round(unname(map_bbox), 2), collapse = "_")
  elevation_file <- file.path(cache_dir,
                              paste0("elevation_z", zoom, "_", bb_tag, ".tif"))

  if (!file.exists(elevation_file)) {
    message("Downloading elevation (first time only) ...")

    loc <- data.frame(
      x = unname(c(map_bbox["xmin"], map_bbox["xmax"])),
      y = unname(c(map_bbox["ymin"], map_bbox["ymax"]))
    )

    r0 <- terra::rast(
      elevatr::get_elev_raster(loc, prj = 4326, z = zoom, src = "aws",
                               clip = "bbox", verbose = FALSE)
    )
    names(r0) <- "elevation"
    terra::writeRaster(r0, elevation_file, overwrite = TRUE)
  }

  elev <- terra::project(terra::rast(elevation_file), "EPSG:3857")
  names(elev) <- "elevation"

  # Keep plotting fast for large extents
  if (terra::ncell(elev) > 2e5) {
    elev <- terra::aggregate(elev, fact = ceiling(sqrt(terra::ncell(elev) / 2e5)))
  }


  # ------------------------------------------------------------
  # Hillshade
  # ------------------------------------------------------------

  slope  <- terra::terrain(elev, v = "slope",  unit = "radians")
  aspect <- terra::terrain(elev, v = "aspect", unit = "radians")
  shade  <- terra::shade(slope, aspect, angle = shade_angle,
                         direction = shade_direction)
  names(shade) <- "shade"

  list(
    elevation = elev,
    shade     = shade,
    shade_df  = terra::as.data.frame(shade, xy = TRUE, na.rm = TRUE)
  )
}


# Borders between U.S. states (from the `maps` package) as sf lines in
# EPSG:3857, cropped to map_bbox when it is given. Returns NULL with a warning
# if `maps` is not installed or the borders cannot be built.

#' Title
#'
#' @param map_bbox
#'
#' @returns
#' @export
#'
#' @examples
state_borders <- function(map_bbox = NULL) {
  if (!requireNamespace("maps", quietly = TRUE)) {
    warning("Package `maps` is required for state borders; layer omitted.",
            call. = FALSE)
    return(NULL)
  }

  tryCatch({

    # ------------------------------------------------------------
    # Split the maps polylines into one linestring per segment
    # ------------------------------------------------------------

    states_map <- maps::map("state", plot = FALSE, fill = FALSE,
                            boundary = FALSE, interior = TRUE)

    xy <- data.frame(x = states_map$x, y = states_map$y)

    # NA rows separate the segments
    separator  <- is.na(xy$x) | is.na(xy$y)
    segment_id <- cumsum(separator)
    xy         <- xy[!separator, , drop = FALSE]
    segment_id <- segment_id[!separator]

    split_xy <- split(xy, segment_id)
    split_xy <- split_xy[vapply(split_xy, nrow, integer(1)) >= 2]

    state_lines <- sf::st_sfc(
      lapply(split_xy, function(d) {
        sf::st_linestring(as.matrix(d[, c("x", "y"), drop = FALSE]))
      }),
      crs = 4326
    )

    states_sf <- sf::st_sf(geometry = state_lines)


    # ------------------------------------------------------------
    # Crop in lon/lat first, then reproject for plotting
    # ------------------------------------------------------------

    if (!is.null(map_bbox)) {
      crop_box <- sf::st_bbox(
        c(xmin = unname(map_bbox["xmin"]), ymin = unname(map_bbox["ymin"]),
          xmax = unname(map_bbox["xmax"]), ymax = unname(map_bbox["ymax"])),
        crs = sf::st_crs(4326)
      )
      states_sf <- suppressWarnings(sf::st_crop(states_sf, crop_box))
    }

    sf::st_transform(states_sf, 3857)
  }, error = function(e) {
    warning("State borders could not be built: ", conditionMessage(e),
            call. = FALSE)
    NULL
  })
}


# Adds city markers and arrowed pointer labels to the map `p`. `cities` is a
# data frame with columns place, lon and lat, and optionally label_dx_m and
# label_dy_m: the label's offset from the city in metres (EPSG:3857), which
# default to default_dx_m / default_dy_m. show_points = FALSE drops the
# markers. Returns `p` unchanged when `cities` is NULL or empty.

#' Title
#'
#' @param p
#' @param cities
#' @param default_dx_m
#' @param default_dy_m
#' @param show_points
#' @param label_size
#'
#' @returns
#' @export
#'
#' @examples
city_label_layers <- function(p, cities, default_dx_m = 25000,
                              default_dy_m = 20000, show_points = TRUE,
                              label_size = 3) {
  if (is.null(cities) || !nrow(cities)) return(p)

  # ------------------------------------------------------------
  # Check columns and fill in default offsets
  # ------------------------------------------------------------

  missing_cols <- setdiff(c("place", "lon", "lat"), names(cities))
  if (length(missing_cols)) {
    stop("`cities` is missing: ", paste(missing_cols, collapse = ", "))
  }

  if (!"label_dx_m" %in% names(cities)) cities$label_dx_m <- default_dx_m
  if (!"label_dy_m" %in% names(cities)) cities$label_dy_m <- default_dy_m


  # ------------------------------------------------------------
  # City points and label positions in EPSG:3857
  # ------------------------------------------------------------

  pts <- sf::st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(3857)

  xy  <- sf::st_coordinates(pts)
  lab <- sf::st_drop_geometry(pts)
  lab$x <- xy[, 1]
  lab$y <- xy[, 2]
  lab$label_x <- lab$x + lab$label_dx_m
  lab$label_y <- lab$y + lab$label_dy_m


  # ------------------------------------------------------------
  # Draw markers, arrows and labels
  # ------------------------------------------------------------

  if (show_points) {
    p <- p + ggplot2::geom_sf(
      data = pts, shape = 21, fill = "black", colour = "white",
      size = 2, stroke = 0.5, inherit.aes = FALSE
    )
  }

  p +
    ggplot2::geom_segment(
      data = lab,
      ggplot2::aes(x = label_x, y = label_y, xend = x, yend = y),
      colour = "black", linewidth = 0.5,
      arrow = grid::arrow(length = grid::unit(0.15, "cm"), type = "closed"),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = lab,
      ggplot2::aes(x = label_x, y = label_y, label = place),
      size = label_size, fontface = "bold", fill = "white",
      colour = "black", linewidth = 0.25, inherit.aes = FALSE
    )
}


# Adds all context layers to a population map in one call, in this order:
#   basemap -> terrain -> rivers -> under_pop() -> population tiles
#   -> state borders -> roads -> over_pop() -> cities
# `p1` is a ggplot whose last layer is the population polygons, as built by
# population_plot(). Pass `osm` (non-NULL) when p1's first layer is a basemap,
# so terrain goes on top of it, and `pop_sf` (non-NULL) to lift the
# population layer so context layers can go underneath it.
# `under_pop` and `over_pop` are functions(p) returning p, for disaster
# layers: under_pop draws below the population tiles (e.g. forecast cone,
# flood extent), over_pop above tiles and roads but below city labels (e.g.
# storm track, fire perimeter, epicentre).
# `config` is a list of optional settings: cache_dir, terrain_zoom,
# shade_angle, shade_direction, shade_alpha, pop_alpha, roads ("major",
# "secondary" or "none"), rivers ("major", "all" or "none"), show_terrain,
# show_state_borders, state_border_colour, state_border_linewidth, cities,
# show_cities, show_city_points and city_label_size.

#' Title
#'
#' @param p1
#' @param map_bbox
#' @param osm
#' @param pop_sf
#' @param config
#' @param under_pop
#' @param over_pop
#'
#' @returns
#' @export
#'
#' @examples
add_map_layers <- function(p1, map_bbox, osm = NULL, pop_sf = NULL,
                           config = list(), under_pop = NULL, over_pop = NULL) {

  # ------------------------------------------------------------
  # Settings
  # ------------------------------------------------------------

  cfg <- function(name, default = NULL) {
    if (!is.null(config[[name]])) config[[name]] else default
  }

  cache_dir       <- cfg("cache_dir", "map_cache")
  terrain_zoom    <- cfg("terrain_zoom", 9)
  shade_angle     <- cfg("shade_angle", 40)
  shade_direction <- cfg("shade_direction", 315)
  shade_alpha     <- cfg("shade_alpha", 0.50)
  pop_alpha       <- cfg("pop_alpha", 0.82)
  roads           <- cfg("roads", "major")
  rivers          <- cfg("rivers", "major")
  show_terrain    <- cfg("show_terrain", TRUE)


  # ------------------------------------------------------------
  # Pull the population layer off the stack so context goes underneath
  # ------------------------------------------------------------

  pop_layer <- NULL
  if (!is.null(pop_sf) && length(p1$layers) > 0) {
    pop_layer <- p1$layers[[length(p1$layers)]]
    pop_layer$aes_params$alpha     <- pop_alpha
    pop_layer$aes_params$colour    <- "grey85"
    pop_layer$aes_params$linewidth <- 0.05
    p1$layers <- p1$layers[-length(p1$layers)]
  }


  # ------------------------------------------------------------
  # Terrain (just above the basemap)
  # ------------------------------------------------------------

  if (!is.null(map_bbox) && show_terrain) {
    terrain <- tryCatch(
      terrain_hillshade(map_bbox, terrain_zoom, cache_dir,
                        shade_angle, shade_direction),
      error = function(e) {
        warning("Terrain failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    if (!is.null(terrain)) {
      shade_layer <- ggplot2::geom_raster(
        data = terrain$shade_df,
        ggplot2::aes(x = x, y = y, alpha = shade),
        fill = "grey15", inherit.aes = FALSE
      )
      p1$layers <- append(p1$layers, list(shade_layer),
                          after = if (!is.null(osm)) 1L else 0L)
      p1 <- p1 + ggplot2::scale_alpha_continuous(
        range = c(0.03, shade_alpha), guide = "none"
      )
    }
  }


  # ------------------------------------------------------------
  # Rivers
  # ------------------------------------------------------------

  if (!is.null(map_bbox) && rivers != "none") {
    water <- osm_lines_cached(
      map_bbox, key = "waterway",
      values = if (rivers == "all") c("river", "stream") else "river",
      tag = paste0("water_", rivers), cache_dir = cache_dir
    )

    if (!is.null(water)) {
      r <- water[water$waterway == "river", , drop = FALSE]
      if (nrow(r) > 0) {
        p1 <- p1 + ggplot2::geom_sf(data = r, colour = "dodgerblue3",
                                    linewidth = 0.3, alpha = 0.55,
                                    inherit.aes = FALSE, show.legend = FALSE)
      }
      if (rivers == "all") {
        s <- water[water$waterway == "stream", , drop = FALSE]
        if (nrow(s) > 0) {
          p1 <- p1 + ggplot2::geom_sf(data = s, colour = "deepskyblue3",
                                      linewidth = 0.12, alpha = 0.45,
                                      inherit.aes = FALSE, show.legend = FALSE)
        }
      }
    }
  }


  # ------------------------------------------------------------
  # Roads (downloaded now, drawn after the population tiles)
  # ------------------------------------------------------------

  road_major_sf <- road_secondary_sf <- NULL

  if (!is.null(map_bbox) && roads != "none") {
    major <- c("motorway", "motorway_link", "trunk", "trunk_link",
               "primary", "primary_link")
    secondary <- c("secondary", "secondary_link")

    road_sf <- osm_lines_cached(
      map_bbox, key = "highway",
      values = if (roads == "secondary") c(major, secondary) else major,
      tag = paste0("roads_", roads), cache_dir = cache_dir
    )

    if (!is.null(road_sf)) {
      road_major_sf <- road_sf[road_sf$highway %in% major, , drop = FALSE]
      if (roads == "secondary") {
        road_secondary_sf <- road_sf[road_sf$highway %in% secondary, , drop = FALSE]
      }
    }
  }


  # ------------------------------------------------------------
  # Disaster layers below the population tiles
  # ------------------------------------------------------------

  if (is.function(under_pop)) p1 <- under_pop(p1)


  # ------------------------------------------------------------
  # Restore population tiles
  # ------------------------------------------------------------

  if (!is.null(pop_layer)) {
    p1$layers <- append(p1$layers, list(pop_layer), after = length(p1$layers))
  }


  # ------------------------------------------------------------
  # State borders
  # ------------------------------------------------------------

  if (isTRUE(cfg("show_state_borders", FALSE)) && !is.null(map_bbox)) {
    sb <- state_borders(map_bbox)
    if (!is.null(sb) && nrow(sb) > 0) {
      p1 <- p1 + ggplot2::geom_sf(
        data = sb, colour = cfg("state_border_colour", "grey15"),
        linewidth = cfg("state_border_linewidth", 0.45), alpha = 0.9,
        inherit.aes = FALSE, show.legend = FALSE
      )
    }
  }


  # ------------------------------------------------------------
  # Roads on top of the population tiles
  # ------------------------------------------------------------

  if (!is.null(road_major_sf) && nrow(road_major_sf) > 0) {
    p1 <- p1 + ggplot2::geom_sf(data = road_major_sf, colour = "black",
                                linewidth = 0.45, alpha = 0.82,
                                inherit.aes = FALSE)
  }
  if (!is.null(road_secondary_sf) && nrow(road_secondary_sf) > 0) {
    p1 <- p1 + ggplot2::geom_sf(data = road_secondary_sf, colour = "grey35",
                                linewidth = 0.25, alpha = 0.72,
                                inherit.aes = FALSE)
  }


  # ------------------------------------------------------------
  # Disaster layers above the tiles and roads
  # ------------------------------------------------------------

  if (is.function(over_pop)) p1 <- over_pop(p1)


  # ------------------------------------------------------------
  # City labels last
  # ------------------------------------------------------------

  if (isTRUE(cfg("show_cities", TRUE))) {
    p1 <- city_label_layers(
      p1, cities = cfg("cities", NULL),
      show_points = cfg("show_city_points", TRUE),
      label_size  = cfg("city_label_size", 3)
    )
  }

  p1
}
