# americas <img src="man/figures/logo.png" align="right" height="120" alt="" />

**Easy interactive maps of United States survey data.**

`americas` turns any data frame of geographic points (with longitude and
latitude columns) into an interactive Shiny dashboard: a dynamic Leaflet map
and a static ggplot2/Plotly map, drawn over US state boundaries. Colour the
points by any categorical variable, size them by any numeric variable, shrink
the markers with a slider, or show locations only.

## Package Installation


Installation of `americas` from GitHub requires the `devtools` package and can be done in the following way.

```r
devtools::install_github("isharaWijayaratne/americas")
```

## Use

```r
library(americas)

# Example dataset bundled with the package
df <- read.csv(system.file("extdata", "us_survey.csv", package = "americas"))

americas(
  data         = df,
  latitude     = "Latitude",
  longitude    = "Longitude",
  qualitative  = c("State", "Rice_Variety", "Soil_Type", "Stage_of_Maturity"),
  quantitative = c("Disease_Incidence", "Mean_gall_count")
)
```

That call opens the dashboard in your browser. The `qualitative` variables
populate the "colour by" menu and the `quantitative` variables populate the
"size by" menu.

## The function

```r
americas(data, latitude, longitude, qualitative, quantitative,
         title = "Spatial Mapping", radius_range = c(2, 25, 20))
```

- `data` — a data frame of points.
- `latitude`, `longitude` — coordinate column names (strings).
- `qualitative` — categorical column names to colour by.
- `quantitative` — numeric column names to size by.
- `title` — dashboard header text.
- `radius_range` — `c(min, max, default)` for the marker-size slider, in pixels.

## Notes

- Boundaries are the contiguous US states from the **spData** package.
- Colours are generated automatically for any number of categories.

## Developing - I'am open to suggestions on improving this package.

Note to myself: After editing the `#'` documentation comments, regenerate the help files and
`NAMESPACE`, then check the package:

```r
devtools::document()
devtools::check()
devtools::install()
```
