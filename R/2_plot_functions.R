# Range of column `col` across two time windows, so maps of both windows can
# share one colour scale. Only tiles inside lon_limits / lat_limits count.
# With symmetric = TRUE the range is centred on zero (c(-m, m)), which suits
# diverging scales. Returns NULL if the column is missing or has no finite
# values. Reads the globals `tiles_3857`, `lon_limits` and `lat_limits`, and
# by default the window globals that attach(compute_windows(...)) creates.

#' Title
#'
#' @param col
#' @param symmetric
#' @param ds1
#' @param hour1
#' @param ds2
#' @param hour2
#'
#' @returns
#' @export
#'
#' @examples
shared_limits <- function(col, symmetric = FALSE,
                          ds1 = first_ds, hour1 = first_hour,
                          ds2 = latest_ds, hour2 = latest_hour) {
  if (!col %in% names(tiles_3857)) return(NULL)
  v <- tiles_3857 |>
    dplyr::filter((ds == ds1 & hour == hour1) | (ds == ds2 & hour == hour2),
                  dplyr::between(longitude, lon_limits[1], lon_limits[2]),
                  dplyr::between(latitude,  lat_limits[1], lat_limits[2])) |>
    dplyr::pull(.data[[col]])
  v <- v[is.finite(v)]
  if (!length(v)) return(NULL)
  r <- range(v, na.rm = TRUE)
  if (symmetric) { m <- max(abs(r)); c(-m, m) } else r
}

# Map of one population time window (plot_ds + plot_hour) from the global
# `tiles_3857`, returned as a ggplot. `metric` picks the fill: change from
# baseline, users during the crisis, or z-score. When lon_limits and
# lat_limits are given, the map is cropped to them and gets a CartoDB basemap
# (at `zoom`, lowered by fit_zoom() for wide extents) and, optionally,
# OpenStreetMap roads (`highway_detail`), rivers (`river_detail`),
# shaded-relief terrain (`show_terrain`, drawn under slightly see-through
# population tiles) and U.S. state borders (`show_state_borders`).
# `disaster_type` overlays fire perimeters or earthquake epicentres for
# start_date..end_date.
# `labels` (sf points in EPSG:3857 with a `label` column) adds arrowed city
# labels, angled per city with `label_angles`.

#' Title
#'
#' @param plot_ds
#' @param plot_hour
#' @param tzone
#' @param metric
#' @param title
#' @param plot_title
#' @param lon_limits
#' @param lat_limits
#' @param zoom
#' @param fill_limits
#' @param labels
#' @param label_angles
#' @param highway_detail
#' @param river_detail
#' @param show_terrain
#' @param show_state_borders
#' @param disaster_type
#' @param disaster_limits
#' @param start_date
#' @param end_date
#' @param min_magnitude
#'
#' @returns
#' @export
#'
#' @examples
population_plot <- function(
    plot_ds,
    plot_hour,
    tzone,
    metric = c("difference", "crisis", "zscore"),
    title = TRUE,
    plot_title = NULL,
    lon_limits = NULL,   # c(min_lon, max_lon)
    lat_limits = NULL,   # c(min_lat, max_lat)
    zoom = 9,             # basemap detail
    fill_limits = NULL,  # force the fill scale limits
    labels = NULL,
    label_angles = NULL,
    highway_detail = c("none", "major", "secondary", "all"),
    river_detail = c("none", "major", "all"),
    show_terrain = FALSE,
    show_state_borders = FALSE,
    disaster_type = NULL, # "fire", "earthquake"
    disaster_limits = NULL,
    start_date = NULL,    # disaster query window (fire / earthquake)
    end_date = NULL,
    min_magnitude = 2.5   # earthquakes only
) {
  
  metric <- match.arg(metric)
  highway_detail <- match.arg(highway_detail)
  river_detail <- match.arg(river_detail)
  # If using match.arg inside, handle NULL safely like this:
  if (!is.null(disaster_type)) {
    disaster_type <- match.arg(disaster_type, c("fire", "earthquake"))
  }
  # ------------------------------------------------------------
  # Select coloured column
  # ------------------------------------------------------------
  
  fill_col <- switch(
    metric,
    difference = "Difference between baseline and crisis",
    crisis     = "# Users During Crisis",
    zscore     = "z_score"
  )
  
  # ------------------------------------------------------------
  # Defensive check for z_score
  # ------------------------------------------------------------
  
  if (metric == "zscore" && !("z_score" %in% names(tiles_3857))) {
    stop(
      "population_plot(metric='zscore'): column `z_score` not in tiles_3857. ",
      "Rebuild with load_population(re_run = TRUE); if still missing, ",
      "quadkey_df_to_polygon dropped it - join z_score back from the raw CSVs by quadkey."
    )
  }
  
  # ------------------------------------------------------------
  # Get requested population window
  # ------------------------------------------------------------
  
  first_time_pop <- tiles_3857 |>
    dplyr::filter(
      ds == plot_ds,
      hour == plot_hour
    )
  
  # Fail loudly if requested window does not exist
  if (nrow(first_time_pop) == 0) {
    stop(
      "population_plot(metric='", metric,
      "'): no data for ds=", plot_ds,
      ", hour=", plot_hour,
      ". Available windows: ",
      paste(
        sort(unique(paste(tiles_3857$ds, tiles_3857$hour))),
        collapse = ", "
      )
    )
  }
  
  # ------------------------------------------------------------
  # Fill scale limits
  # ------------------------------------------------------------
  
  lims <- range(
    tiles_3857[[fill_col]],
    na.rm = TRUE
  )
  
  # ------------------------------------------------------------
  # Default coordinate limits
  # ------------------------------------------------------------
  
  xlim <- NULL
  ylim <- NULL
  
  # ------------------------------------------------------------
  # Spatial filtering and basemap
  # ------------------------------------------------------------
  
  osm <- NULL
  
  if (!is.null(lon_limits) && !is.null(lat_limits)) {
    
    # Restrict population data to requested bounding box
    first_time_pop <- first_time_pop |>
      dplyr::filter(
        longitude >= lon_limits[1],
        longitude <= lon_limits[2],
        latitude >= lat_limits[1],
        latitude <= lat_limits[2]
      )
    
    # Recalculate limits from visible data
    if (nrow(first_time_pop) > 0) {
      lims <- range(
        first_time_pop[[fill_col]],
        na.rm = TRUE
      )
    }
    
    # ----------------------------------------------------------
    # Basemap bounding box
    # ----------------------------------------------------------
    
    pts <- rbind(
      data.frame(
        lon = lon_limits[1],
        lat = lat_limits[1]
      ),
      data.frame(
        lon = lon_limits[2],
        lat = lat_limits[2]
      )
    )
    
    pts_sf <- sf::st_as_sf(
      pts,
      coords = c("lon", "lat"),
      crs = 4326
    )
    
    # Download OpenStreetMap / CartoDB basemap, lowering the zoom first if the
    # extent would need too many tiles
    zoom <- fit_zoom(lon_limits, lat_limits, zoom)
    osm <- get_tiles(
      pts_sf,
      provider = "CartoDB.Voyager",
      crop = TRUE,
      zoom = zoom
    )
    
    # ----------------------------------------------------------
    # Convert bounding box to EPSG:3857
    # ----------------------------------------------------------
    
    bbox_ll <- sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = lon_limits[1],
          xmax = lon_limits[2],
          ymin = lat_limits[1],
          ymax = lat_limits[2]
        ),
        crs = sf::st_crs(4326)
      )
    )
    
    bbox_3857 <- sf::st_transform(
      bbox_ll,
      3857
    ) |>
      sf::st_bbox()
    
    xlim <- c(
      bbox_3857["xmin"],
      bbox_3857["xmax"]
    )
    
    ylim <- c(
      bbox_3857["ymin"],
      bbox_3857["ymax"]
    )
    
    # Bounding box used by OSM highway query
    map_bbox <- c(
      xmin = lon_limits[1],
      ymin = lat_limits[1],
      xmax = lon_limits[2],
      ymax = lat_limits[2]
    )
    
  } else {
    
    # ----------------------------------------------------------
    # No explicit spatial limits
    #
    # We don't download a highway layer in this case because
    # querying the entire tiles_3857 extent through OSM can be
    # unnecessarily expensive.
    # ----------------------------------------------------------
    
    map_bbox <- NULL
  }
  
  # ------------------------------------------------------------
  # Highway layer
  # ------------------------------------------------------------
  
  highway_sf <- NULL
  
  if (
    highway_detail != "none" &&
    !is.null(map_bbox)
  ) {
    
    # Define OSM highway classes by desired granularity
    highway_classes <- switch(
      highway_detail,
      
      # Major highways:
      # Freeways, trunks, primary roads
      major = c(
        "motorway",
        "motorway_link",
        "trunk",
        "trunk_link",
        "primary",
        "primary_link"
      ),
      
      # Add secondary roads
      secondary = c(
        "motorway",
        "motorway_link",
        "trunk",
        "trunk_link",
        "primary",
        "primary_link",
        "secondary",
        "secondary_link"
      ),
      
      # Include tertiary and local/unclassified roads
      all = c(
        "motorway",
        "motorway_link",
        "trunk",
        "trunk_link",
        "primary",
        "primary_link",
        "secondary",
        "secondary_link",
        "tertiary",
        "tertiary_link",
        "unclassified",
        "residential"
      )
    )
    
    # Query OpenStreetMap, failing over between Overpass servers and
    # caching the result for this session. Returns NULL (with a message)
    # if every server fails. Result is already in EPSG:3857.
    highway_sf <- osm_lines_cached(
      map_bbox,
      key = "highway",
      values = highway_classes,
      tag = paste0("roads_", highway_detail),
      cache_dir = tempdir()
    )

    if (
      !is.null(highway_sf) &&
      nrow(highway_sf) > 0
    ) {

      # Remove empty geometries
      highway_sf <- highway_sf[
        !sf::st_is_empty(highway_sf),
        ,
        drop = FALSE
      ]
    }
  }

  # ------------------------------------------------------------
  # River layer
  # ------------------------------------------------------------

  river_sf <- NULL

  if (
    river_detail != "none" &&
    !is.null(map_bbox)
  ) {

    # Major: rivers only. All: rivers and streams
    river_sf <- osm_lines_cached(
      map_bbox,
      key = "waterway",
      values = if (river_detail == "all") c("river", "stream") else "river",
      tag = paste0("water_", river_detail),
      cache_dir = tempdir()
    )
  }

  # ------------------------------------------------------------
  # Shared-scale override
  # ------------------------------------------------------------
  
  if (!is.null(fill_limits)) {
    lims <- fill_limits
  }
  
  # ------------------------------------------------------------
  # Fill scale
  # ------------------------------------------------------------
  
  fill_scale <- if (metric == "difference") {
    
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "grey",
      high = "red",
      limits = lims,
      midpoint = 0,
      name = "Users (crisis - baseline)"
    )
    
  } else if (metric == "crisis") {
    
    ggplot2::scale_fill_gradient(
      trans = "log10",
      low = "blue",
      high = "red",
      limits = lims,
      labels = scales::label_comma(),
      name = "Users"
    )
    
  } else {
    
    # z-score
    ggplot2::scale_fill_distiller(
      palette = "RdBu",
      direction = -1,
      limits = c(-4, 4),
      oob = scales::squish,
      na.value = "grey60",
      name = "z-score (crisis vs. baseline)"
    )
  }
  
  # ------------------------------------------------------------
  # Base population map
  # ------------------------------------------------------------
  
  p1 <- ggplot2::ggplot()
  
  # Basemap
  if (!is.null(osm)) {

    p1 <- p1 +
      layer_spatial(osm)
  }

  # ------------------------------------------------------------
  # Terrain
  #
  # Shaded relief between the basemap and the population tiles. The
  # tiles are made slightly see-through below so the relief shows.
  # ------------------------------------------------------------

  terrain <- NULL

  if (show_terrain && !is.null(map_bbox)) {

    terrain <- tryCatch(
      terrain_hillshade(map_bbox, zoom = zoom, cache_dir = tempdir()),
      error = function(e) {
        warning("Could not build terrain: ", conditionMessage(e))
        NULL
      }
    )
  }

  if (!is.null(terrain)) {

    p1 <- p1 +
      ggplot2::geom_raster(
        data = terrain$shade_df,
        ggplot2::aes(
          x = x,
          y = y,
          alpha = shade
        ),
        fill = "grey15",
        inherit.aes = FALSE
      ) +
      ggplot2::scale_alpha_continuous(
        range = c(0.03, 0.5),
        guide = "none"
      )
  }

  # Population polygons
  p1 <- p1 +
    ggplot2::geom_sf(
      data = first_time_pop,
      ggplot2::aes(
        fill = .data[[fill_col]]
      ),
      color = if (is.null(terrain)) "white" else "grey85",
      linewidth = if (is.null(terrain)) 0.1 else 0.05,
      alpha = if (is.null(terrain)) 1 else 0.82
    )
  # ------------------------------------------------------------
  # Disaster-specific layer
  #
  # Added disaster-specific data (e.g. fire polygons or earthquake epicenters)
  # ------------------------------------------------------------
  
  
  if (!is.null(disaster_type)) {

    p1 <- switch(
      disaster_type,
      fire = add_fire_layer(
        p1, start_date, end_date, lon_limits, lat_limits
      ),
      earthquake = add_earthquake_layer(
        p1, lon_limits, lat_limits, start_date, end_date,
        min_magnitude, disaster_limits, xlim, ylim
      )
    )
  }
  
  # ------------------------------------------------------------
  # State borders
  #
  # Added AFTER population polygons so borders appear on top.
  # ------------------------------------------------------------

  if (show_state_borders && !is.null(map_bbox)) {

    border_sf <- state_borders(map_bbox)

    if (!is.null(border_sf) && nrow(border_sf) > 0) {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = border_sf,
          color = "grey15",
          linewidth = 0.45,
          alpha = 0.9,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  # ------------------------------------------------------------
  # River layer
  #
  # Added AFTER population polygons so rivers appear on top, but
  # before highways so roads cross over them.
  # ------------------------------------------------------------

  if (
    !is.null(river_sf) &&
    nrow(river_sf) > 0
  ) {

    rivers <- river_sf[river_sf$waterway == "river", , drop = FALSE]
    streams <- river_sf[river_sf$waterway == "stream", , drop = FALSE]

    if (nrow(rivers) > 0) {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = rivers,
          color = "dodgerblue3",
          linewidth = 0.3,
          alpha = 0.55,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }

    if (nrow(streams) > 0) {
      p1 <- p1 +
        ggplot2::geom_sf(
          data = streams,
          color = "deepskyblue3",
          linewidth = 0.12,
          alpha = 0.45,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  # ------------------------------------------------------------
  # Highway layer
  #
  # Added AFTER population polygons so highways appear on top.
  # ------------------------------------------------------------
  
  if (
    !is.null(highway_sf) &&
    nrow(highway_sf) > 0
  ) {
    
    # Different visual hierarchy depending on road type
    p1 <- p1 +
      ggplot2::geom_sf(
        data = highway_sf,
        ggplot2::aes(
          linewidth = highway
        ),
        color = "black",
        alpha = 0.85,
        inherit.aes = FALSE
      ) +
      
      ggplot2::scale_linewidth_manual(
        values = c(
          motorway       = 1.2,
          motorway_link  = 0.7,
          trunk          = 1.0,
          trunk_link     = 0.6,
          primary        = 0.8,
          primary_link   = 0.5,
          secondary      = 0.6,
          secondary_link = 0.4,
          tertiary       = 0.35,
          tertiary_link  = 0.25,
          unclassified   = 0.25,
          residential    = 0.20
        ),
        guide = "none"
      )
  }
  
  # ------------------------------------------------------------
  # Fill scale + coordinate system
  # ------------------------------------------------------------
  
  p1 <- p1 +
    fill_scale +
    
    ggplot2::coord_sf(
      crs = sf::st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    )
  
  # ------------------------------------------------------------
  # Legends
  # ------------------------------------------------------------
  
  p1 <- p1 +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        direction = "horizontal",
        order = 1,
        title.position = "top"
      ),
      color = ggplot2::guide_legend(
        direction = "horizontal",
        order = 2
      )
    )
  
  # ------------------------------------------------------------
  # City labels and arrows
  # ------------------------------------------------------------
  
  if (!is.null(labels)) {
    
    # `labels` is already in EPSG:3857
    coords <- sf::st_coordinates(labels)
    
    label_xy <- labels |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        x = coords[, 1],
        y = coords[, 2]
      )
    
    # ----------------------------------------------------------
    # City-specific arrow angles
    #
    # Example:
    #
    # label_angles <- c(
    #   "Los Angeles" = 20,
    #   "San Diego" = -35,
    #   "San Francisco" = 90
    # )
    #
    # Convention:
    #   0   = straight up
    #   90  = right
    #   -90 = left
    #   180 = straight down
    # ----------------------------------------------------------
    
    if (is.null(label_angles)) {
      
      label_angles <- stats::setNames(
        rep(0, nrow(label_xy)),
        label_xy$label
      )
    }
    
    # Match angle to each city
    label_xy$angle <- unname(
      label_angles[label_xy$label]
    )
    
    # Default to straight up
    label_xy$angle[
      is.na(label_xy$angle)
    ] <- 0
    
    # ----------------------------------------------------------
    # Arrow/label distance
    # ----------------------------------------------------------
    
    # 15% of the map's height, so labels stay on small maps; 80 km when
    # the map has no limits
    arrow_length <- if (!is.null(ylim)) {
      0.15 * unname(diff(ylim))
    } else {
      80000
    }
    
    # Degrees -> radians
    angle_rad <- label_xy$angle * pi / 180
    
    # Calculate label position
    label_xy <- label_xy |>
      dplyr::mutate(
        label_x = x + arrow_length * sin(angle_rad),
        label_y = y + arrow_length * cos(angle_rad)
      )
    
    # ----------------------------------------------------------
    # Arrows
    # ----------------------------------------------------------
    
    p1 <- p1 +
      
      ggplot2::geom_segment(
        data = label_xy,
        ggplot2::aes(
          x = label_x,
          y = label_y,
          xend = x,
          yend = y
        ),
        color = "black",
        linewidth = 1.2,
        arrow = grid::arrow(
          length = grid::unit(
            0.35,
            "cm"
          ),
          type = "closed"
        ),
        inherit.aes = FALSE
      ) +
      
      # --------------------------------------------------------
    # City labels
    # --------------------------------------------------------
    
    ggplot2::geom_label(
      data = label_xy,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label
      ),
      color = "black",
      fill = "white",
      size = 4,
      fontface = "bold",
      linewidth = 0.3,
      inherit.aes = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # Title
  # ------------------------------------------------------------
  
  if (title) {
    
    if (is.null(plot_title)) {
      
      p1 +
        ggplot2::ggtitle(
          format_time(
            plot_ds,
            plot_hour,
            tzone = tzone
          )
        )
      
    } else {
      
      p1 +
        ggplot2::ggtitle(
          plot_title
        )
    }
    
  } else {
    
    p1
  }
}

# Downloads OpenStreetMap line features (e.g. roads, rivers) with `key` equal
# to any of `values` inside map_bbox, and returns them as an sf in EPSG:3857
# with only the `key` column. Used by population_plot(highway_detail = ,
# river_detail = ). `map_bbox` is a named lon/lat vector
# c(xmin = , ymin = , xmax = , ymax = ) in EPSG:4326. Results are cached in
# cache_dir as "<tag>_<bbox>.rds". Tries three Overpass servers in turn
# (three rounds); returns NULL, with a message, if none of them responds.
# Returns NULL straight away when map_bbox is NULL.

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


# Shaded relief for population_plot(show_terrain = TRUE). `map_bbox` is a
# named lon/lat vector c(xmin = , ymin = , xmax = , ymax = ) in EPSG:4326.
# Elevation is downloaded once (AWS tiles at `zoom`, via elevatr) and cached
# in cache_dir as a GeoTIFF, then reprojected to EPSG:3857 and hill-shaded
# with the sun at shade_angle (degrees above the horizon) and shade_direction
# (degrees from north). Large rasters are aggregated to about 200,000 cells to
# keep plotting fast. Returns a list of `elevation` and `shade` (terra
# rasters) and `shade_df` (x, y, shade data frame for geom_raster()), or NULL
# when map_bbox is NULL.

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
# EPSG:3857, for population_plot(show_state_borders = TRUE). Cropped to
# map_bbox (named lon/lat vector c(xmin = , ymin = , xmax = , ymax = )) when
# it is given. Returns NULL with a warning if `maps` is not installed or the
# borders cannot be built.

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

# Change-from-baseline map: population_plot(metric = "difference") with a
# clean theme (no axes) and the legend along the bottom. Any other
# population_plot() argument can be passed through `...`.

#' Title
#'
#' @param plot_ds
#' @param plot_hour
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
population_plot_n_difference <- function(plot_ds, plot_hour, ...) {
  population_plot(plot_ds, plot_hour, metric = "difference", ...) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",         # stack the fill colorbar and the outline legend
      legend.direction = "horizontal", # each legend laid out horizontally
      legend.key.width = unit(1.4, "cm")
    )
}

# Users-during-crisis map: population_plot(metric = "crisis") with the same
# theme as population_plot_n_difference().

#' Title
#'
#' @param plot_ds
#' @param plot_hour
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
population_plot_n_crisis <- function(plot_ds, plot_hour, ...) {
  population_plot(plot_ds, plot_hour, metric = "crisis", ...) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",         # stack the fill colorbar and the outline legend
      legend.direction = "horizontal", # each legend laid out horizontally
      legend.key.width = unit(1.4, "cm")
    )
}
