# ggcognigen

`ggcognigen` provides geoms, themes, styles, and helper functions for {ggplot2} at the Clinical Pharmacology and Pharmacometrics (CPP) business unit of Simulations Plus, Inc.

Load it with:
```r
library(ggcognigen)
```

# Functionality

## Themes

CPP themes are based on `ggplot2::theme_bw()`. Additional `theme()` inputs can be passed to `...` for further customization.

- `theme_cognigen()`: simple theme for CPP plots (without grid lines).
- `theme_cognigen_grid()`: similar to `theme_cognigen()` but displaying grid lines in the background.

## Geoms

Additional geom functions:

- `geom_barcount()`: a utility function working in combination with `geom_bar()` and allowing the display of the (possibly cumulative) count, normalized count, or percentages of the data represented by each bar.
- `geom_boxplot2()`: a variant on `ggplot2::geom_boxplot()` allowing users to set whisker limits based upon a confidence interval rather than a multiple of the IQR, display outliers with jitter, and providing a slightly different graphical styles for grouping/coloring.
- `geom_boxcount()`: a utility function working in combination with `geom_boxplot2()` allowing the display of the number of data points used for the calculation of statistics which are graphically represented by each box and whiskers.
- `geom_histcount()`: a utility function working in combination with `geom_histogram()` and allowing the display of the cumulative count, density, or percentage of data in each histogram bar.

## Stats

Additional stat functions:

- `stat_barcount()`: associated with `geom_barcount`.
- `stat_bin2()`: a variant of `ggplot2::bin()` which exports the `percent` variable in addition to the default `count` and `density` variables. Mostly intended to be used with `ggplot2::geom_histogram()` and `stat_histcount()` when percentages are required.
- `stat_count2()`: a variant of `ggplot2::count()` which exports the `percent` variable in addition to the default `count` and `prop` variables. Mostly intended to be used with `ggplot2::geom_bar()` and `stat_barcount()` when percentages are required.
- `stat_boxplot2()`: a variant of `ggplot2::stat_boxplot()` which supports `geom_boxplot2()`.
- `stat_boxcount()`: associated with `geom_boxcount()`.
- `stat_histcount()`: associated with `geom_histcount()`.

## Positions

New position function:

- `position_fillpercent()`: a variant of `ggplot2::position_fill()` which normalizes the data to 100 instead of 1. Mostly intended for use with `ggplot2::geom_bar()` and `geom_barcount()` for display of data in percentages.

## Style and scale functions for mapping aesthetics

These control aesthetics that are mapped to data such as color, fill, linetype, shape, and size.

- `cognigen_style()`
- `scale_discrete_cognigen()`
- `set_default_style()`
- `read_style()`

## Forest plots

Additional functions for creation of forest plots:

- `make_gmr_data()`: a function to calculate and report geometric means (GM) and geometric mean ratios (GMR).
- `make_gmr_table()`: a function to format and write GMR table (typically created by `make_gmr_data()`) to html, docx, or tex files.
- `make_forestplot()`: a function to create a forest plot as a `ggplot2` object and based upon the GRM table created by `make_gmr_data()`.

## Miscellaneous helper functions
- `ggsave_multiple()`: extension of `ggplot2::ggsave` to save multiple plots and multi-page plots like those created using `ggforce::facet_grid_paginate()` or `ggforce::facet_wrap_paginate()`.
- `get_device_size()`: a utility function returning default device dimensions based upon a number of plots/panels per page.
- `format_continuous_cognigen()`: a utility function be passed to the `labels` argument of a `scale_` function in order to automatically format axis ticks numerical labels in default or scientific notation (beyond pre-defined value limits)
