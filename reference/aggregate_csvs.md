# Title

Title

## Usage

``` r
aggregate_csvs(
  path_parts,
  output_name = "aggregated.csv",
  dedupe = TRUE,
  lat_col = NULL,
  lon_col = NULL,
  prefix = "",
  calculate_county_change = FALSE,
  county_group_cols = c("county_geoid", "county_name", "county_state", "ds")
)
```

## Arguments

- county_group_cols:
