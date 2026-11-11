# Downloads earthquakes of at least `min_magnitude` inside the lon/lat box
# between start_date and end_date from the USGS API. Returns an sf of points
# in EPSG:3857, sorted by time, with a UTC `datetime`, a Pacific-time
# `datetime_pst` and an `eq_order` letter (A, B, ...) used to label them on
# maps. An empty result is returned unchanged.

#' Title
#'
#' @param lon_limits
#' @param lat_limits
#' @param start_date
#' @param end_date
#' @param min_magnitude
#'
#' @returns
#' @export
#'
#' @examples
fetch_earthquakes <- function(
    lon_limits,
    lat_limits,
    start_date,
    end_date,
    min_magnitude
) {
  
  usgs_url <- paste0(
    "https://earthquake.usgs.gov/fdsnws/event/1/query?",
    "format=geojson",
    "&starttime=", start_date,
    "&endtime=", end_date,
    "&minlatitude=", lat_limits[1],
    "&maxlatitude=", lat_limits[2],
    "&minlongitude=", lon_limits[1],
    "&maxlongitude=", lon_limits[2],
    "&minmagnitude=", min_magnitude
  )
  
  eq <- sf::st_read(usgs_url, quiet = TRUE)
  
  # Process only if rows are returned
  if (nrow(eq) > 0) {
    eq <- eq |>
      sf::st_transform(3857) |>
      dplyr::mutate(
        # USGS gives time in milliseconds since 1970
        datetime = as.POSIXct(
          time / 1000,
          origin = "1970-01-01",
          tz = "UTC"
        ),
        datetime_pst = format(
          datetime,
          tz = "America/Los_Angeles"
        )
      ) |>
      dplyr::arrange(datetime) |>
      dplyr::mutate(eq_order = LETTERS[row_number()])
  }
  return (eq)
}

# Magnitude range of the earthquakes fetch_earthquakes() finds for these
# limits and dates. Pass it to population_plot(disaster_limits = ...) so
# several earthquake maps share one colour scale.

#' Title
#'
#' @param lon_limits
#' @param lat_limits
#' @param start_date
#' @param end_date
#' @param min_magnitude
#'
#' @returns
#' @export
#'
#' @examples
disaster_limits <- function(
    lon_limits,
    lat_limits,
    start_date,
    end_date,
    min_magnitude = 2.5
) {
  earthquakes <- fetch_earthquakes(lon_limits,
                                   lat_limits,
                                   start_date,
                                   end_date,
                                   min_magnitude)
  
  range(earthquakes$mag, na.rm = TRUE)
}

# Adds earthquake epicentres to the map `p1`, for
# population_plot(disaster_type = "earthquake"). Points are coloured by
# magnitude and labelled with their `eq_order` letter. Without
# `disaster_limits` the colour scale covers this map's earthquakes only.
# coord_sf() is set again so the new layers keep the map's crop.

#' Title
#'
#' @param p1
#' @param lon_limits
#' @param lat_limits
#' @param start_date
#' @param end_date
#' @param min_magnitude
#' @param disaster_limits
#' @param xlim
#' @param ylim
#'
#' @returns
#' @keywords internal
#' @noRd
add_earthquake_layer <- function(p1,
                                 lon_limits,
                                 lat_limits,
                                 start_date,
                                 end_date,
                                 min_magnitude,
                                 disaster_limits,
                                 xlim,
                                 ylim) {

  earthquakes <- fetch_earthquakes(lon_limits,
                                   lat_limits,
                                   start_date,
                                   end_date,
                                   min_magnitude)
  if (is.null(disaster_limits)){
    disaster_limits <- range(earthquakes$mag, na.rm = TRUE)
  }

  p1 +
    geom_sf(
      data = earthquakes,
      aes(color = mag),
      size = 5,
      alpha = 0.8
    ) +
    geom_sf_text(
      data = earthquakes,
      aes(label = eq_order),
      color = "black",
      fontface = "bold",
      size = 3
    ) +
    scale_color_gradient(
      low = "gold",
      high = "red",
      limits = disaster_limits,
      breaks = pretty(disaster_limits, n = 5),
      oob = scales::squish,
      name = "Magnitude"
    ) +
    coord_sf(
      crs = st_crs(3857),
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    )
}