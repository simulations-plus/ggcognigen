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

- `theme_cognigen()`: simple theme for Cognigen plots (without grid lines).
- `theme_cognigen_grid()`: similar to `theme_cognigen()` but displaying grid lines in the background.

## Geoms

New geom functions for the creation of box-and-whisker plots.

- `geom_boxplot2()`: a variant on `ggplot2::geom_boxplot()` allowing users to set whisker limits based upon a confidence interval rather than a multiple of the IQR, display outliers with jitter, and providing a slightly different graphical styles for grouping/coloring.
- `geom_boxcount()`: a utility function working in combination with `geom_boxplot2()` allowing the display of the number of data points used for the calculation of statistics which are graphically represented by each box and whiskers

## Style and scale functions for mapping aesthetics

These control aesthetics that are mapped to data such as color, fill, linetype, shape, and size.

- `cognigen_style()`
- `scale_discrete_cognigen()`
- `set_default_style()`
- `read_style_theme()`

## Miscellaneous helper functions
- `format_continuous_cognigen()`: a utility function be passed to the `labels` argument of a `scale_` function in order to automatically format axis ticks numerical labels in default or scientific notation (beyond pre-defined value limits)
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
