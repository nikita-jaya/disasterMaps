# Title

Title

## Usage

``` r
fetch_fires(
  start_date,
  end_date,
  lon_limits,
  lat_limits,
  re_run = FALSE,
  cache_dir = rds_cache_dir,
  cache = "fires.Rds",
  date_field = "attr_FireDiscoveryDateTime",
  layer_url = paste0("https://services3.arcgis.com/T4QMspbfLg3qTGWY/",
    "ArcGIS/rest/services/", "WFIGS_Interagency_Perimeters/", "FeatureServer/0/query")
)
```

## Arguments

- layer_url:
