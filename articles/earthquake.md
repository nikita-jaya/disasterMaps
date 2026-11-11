# Earthquakes

``` r

library(disasterMaps)

tzone <- "Europe/Istanbul"
```

## Data

``` r

data_dir <- system.file("extdata", "earthquake", "population", package = "disasterMaps")
list.files(data_dir)
#> [1] "turkey_earthquake_2023-02-06_0000.csv"
#> [2] "turkey_earthquake_2023-02-06_1600.csv"

tiles_3857 <- load_population(data_dir, re_run = TRUE, cache_dir = tempdir())
head(tiles_3857)
#> Simple feature collection with 6 features and 15 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 4001631 ymin: 4642479 xmax: 4016307 ymax: 4647371
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>         quadkey latitude longitude country           date_time n_baseline
#> 1 1221102112030 38.46219  35.96924      TR 2023-02-06 08:00:00         24
#> 2 1221102112030 38.46219  35.96924      TR 2023-02-07 00:00:00         24
#> 3 1221102112031 38.46219  36.01318      TR 2023-02-06 08:00:00         18
#> 4 1221102112031 38.46219  36.01318      TR 2023-02-07 00:00:00         17
#> 5 1221102112120 38.46219  36.05713      TR 2023-02-06 08:00:00         13
#> 6 1221102112120 38.46219  36.05713      TR 2023-02-07 00:00:00         13
#>   # Users During Crisis Difference between baseline and crisis density_baseline
#> 1                    19                                     -5      4.24588e-05
#> 2                    33                                      9      4.37687e-05
#> 3                    19                                      1      3.18441e-05
#> 4                    21                                      4      3.10028e-05
#> 5                    NA                                     NA      2.29985e-05
#> 6                    13                                      0      2.37080e-05
#>   density_crisis percent_change z_score         ds
#> 1    3.40766e-05       -20.8333 -0.5081 2023-02-06
#> 2    6.30559e-05        37.5000  0.9300 2023-02-06
#> 3    3.40766e-05         5.5556  0.1207 2023-02-06
#> 4    4.01265e-05        23.5294  0.4907 2023-02-06
#> 5             NA             NA      NA 2023-02-06
#> 6    2.48402e-05         0.0000  0.0000 2023-02-06
#>                             source_file hour                       geometry
#> 1 turkey_earthquake_2023-02-06_0000.csv 0000 POLYGON ((4001631 4642479, ...
#> 2 turkey_earthquake_2023-02-06_1600.csv 1600 POLYGON ((4001631 4642479, ...
#> 3 turkey_earthquake_2023-02-06_0000.csv 0000 POLYGON ((4006523 4642479, ...
#> 4 turkey_earthquake_2023-02-06_1600.csv 1600 POLYGON ((4006523 4642479, ...
#> 5 turkey_earthquake_2023-02-06_0000.csv 0000 POLYGON ((4011415 4642479, ...
#> 6 turkey_earthquake_2023-02-06_1600.csv 1600 POLYGON ((4011415 4642479, ...
```

``` r

analyze_dataset(tiles_3857)
#> $missing
#>   quadkey latitude longitude country date_time   n_baseline
#> 1       0        0         0       0         0 0.0007784524
#>   # Users During Crisis Difference between baseline and crisis density_baseline
#> 1            0.01043126                             0.01120972     0.0007784524
#>   density_crisis percent_change    z_score ds source_file hour
#> 1     0.01043126     0.01120972 0.01120972  0           0    0
#> 
#> $numeric_summary
#>   latitude_mean latitude_sd longitude_mean longitude_sd n_baseline_mean
#> 1      37.51884   0.5978687       37.17482    0.7710179        173.5106
#>   n_baseline_sd # Users During Crisis_mean # Users During Crisis_sd
#> 1      620.8509                   170.0617                 597.8652
#>   Difference between baseline and crisis_mean
#> 1                                   -5.004881
#>   Difference between baseline and crisis_sd density_baseline_mean
#> 1                                  90.99679          0.0003116236
#>   density_baseline_sd density_crisis_mean density_crisis_sd percent_change_mean
#> 1         0.001114903        0.0003146633       0.001105383            1.594948
#>   percent_change_sd z_score_mean z_score_sd
#> 1           19.2276   0.02898885  0.6441174
#> 
#> $n_rows
#> [1] 6423
#> 
#> $n_cols
#> [1] 15
```

## Movement data

``` r

move_dir <- system.file("extdata", "earthquake", "movement", package = "disasterMaps")
list.files(move_dir)
#> [1] "turkey_earthquake_movement_2023-02-06_0000.csv"
#> [2] "turkey_earthquake_movement_2023-02-06_1600.csv"

moved <- load_movement(move_dir, re_run = TRUE, cache_dir = tempdir())
head(moved)
#> # A tibble: 6 × 17
#>   start_latitude start_longitude end_latitude end_longitude length_km
#>            <dbl>           <dbl>        <dbl>         <dbl>     <dbl>
#> 1           37.1            36.2         37.1          36.1      5.51
#> 2           37.1            36.2         37.1          36.2      3.90
#> 3           37.1            36.2         37.1          36.2      3.90
#> 4           37.1            36.2         37.1          36.2      5.51
#> 5           37.1            36.2         37.1          36.3      8.72
#> 6           37.1            36.2         37.1          36.2      3.90
#> # ℹ 12 more variables: start_quadkey <chr>, end_quadkey <chr>, country <chr>,
#> #   date_time <dttm>, n_baseline <dbl>, `# Users During Crisis` <dbl>,
#> #   `Difference between baseline and crisis` <dbl>, percent_change <dbl>,
#> #   z_score <dbl>, ds <date>, source_file <chr>, hour <chr>
```

## Time windows and map extent

``` r

w <- compute_windows(tiles_3857, tzone)
w
#> $first_ds
#> [1] "2023-02-06"
#> 
#> $first_hour
#> [1] "0000"
#> 
#> $latest_ds
#> [1] "2023-02-06"
#> 
#> $latest_hour
#> [1] "1600"
#> 
#> $first_label
#> [1] "February 6, 2023 11am"
#> 
#> $latest_label
#> [1] "February 7, 2023 3am"

lon_limits <- range(tiles_3857$longitude)
lat_limits <- range(tiles_3857$latitude)
zoom <- 10 # lowered automatically if the extent is too wide

# One colour scale for both windows
diff_limits <- shared_limits(
  "Difference between baseline and crisis", symmetric = TRUE,
  ds1 = w$first_ds, hour1 = w$first_hour,
  ds2 = w$latest_ds, hour2 = w$latest_hour
)
```

## Population during the crisis

``` r
population_plot_n_crisis(
  plot_ds = w$first_ds, plot_hour = w$first_hour, tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = zoom
)
#> 
|---------|---------|---------|---------|
=========================================
                                          
```

![](earthquake_files/figure-html/crisis-map-1.png)

## Change from baseline

### First window

``` r

population_plot_n_difference(
  plot_ds = w$first_ds, plot_hour = "0000", tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = zoom,
  fill_limits = diff_limits
)
```

![](earthquake_files/figure-html/difference-0000-1.png)

### Second window

``` r

population_plot_n_difference(
  plot_ds = w$first_ds, plot_hour = "1600", tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = zoom,
  fill_limits = diff_limits
)
```

![](earthquake_files/figure-html/difference-1600-1.png)

## Z-scores

``` r

population_plot(
  plot_ds = w$latest_ds, plot_hour = w$latest_hour, tzone = tzone,
  metric = "zscore",
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = zoom
)
```

![](earthquake_files/figure-html/zscore-1.png)

## Earthquake epicentres

``` r

start_date <- as.Date(w$first_ds) - 1
end_date <- as.Date(w$latest_ds) + 1
min_magnitude <- 6

population_plot_n_difference(
  plot_ds = w$latest_ds, plot_hour = w$latest_hour, tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = zoom,
  fill_limits = diff_limits,
  disaster_type = "earthquake",
  start_date = start_date,
  end_date = end_date,
  min_magnitude = min_magnitude
)
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
```

![](earthquake_files/figure-html/epicentres-1.png)

## Animation

``` r

animation <- population_plot_animate(
  metric = "difference", tzone = tzone,
  lon_limits = lon_limits, lat_limits = lat_limits, zoom = zoom,
  fill_limits = diff_limits, fps = 1
)
#> `nframes` and `fps` adjusted to match transition
animation
```

![](earthquake_files/figure-html/animation-1.gif)
