# ggcognigen

{ggcognigen} provides geoms, themes, styles, and helper functions for {ggplot2} at Cognigen Corporation.

This package has not been released for production use. When it is available for production use, load it with:
```r
library(ggcognigen)
```

Install the development version with:
```r
remotes::install_gitlab(repo = "r/ggcognigen", host = "gitlab.cognigencorp.com")
```

# Functionality

## Themes

Cognigen themes are based on `ggplot2::theme_bw()`. Additional `theme()` inputs can be passed to `...` for further customization.

- `theme_cognigen()`
- `theme_cognigen_grid()`

## Geoms
- `geom_boxplot2()`
- `geom_boxcount()`
- `geom_crossbar2()`

## Style and scale functions for mapping aesthetics

These control aesthetics that are mapped to data such as color, fill, linetype, shape, and size.

- `cognigen_style()`
- `scale_discrete_cognigen()`
- `set_default_style()`
- `read_style_theme()`

## Miscellaneous helper functions
- `format_continuous_cognigen()`
  - Pass to the `labels` argument of a `scale_` function to take care of formatting
- `get_device_size_pixel()`

# Pending Discussions
- How should versioning and licensing be organized/automated?
- Should we use custom names for internal geoms/functions or use the same names as {ggplot2} to override defaults?

# TODO
- Vignettes
  - Styles and Scales to display the default mappings
  - Examples of each custom geom
- Find and document methods of controlling overlaying scales
  - Experiment with `inherit.aes` arg
- Function to split by a grouping variable to create file for each group
  - Could treat like a `facet_` function
- Experiment with `plot.margin` in themes
- Merge concept of `setaxis` macro with `format_continuous_cognigen()`
  - Might require custom `scale_` functions
- Geoms
  - histograms
  - density plots
  - bar plots
