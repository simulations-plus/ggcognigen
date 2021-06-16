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


# TODO
- Release date scheduled: 2021-08-01
  - Need risk assessment
  - Need RVal meeting to define timeline and scope
  - Potential Testing / Qualification
    - Could start with same data; make plots in validated environments:
      - SAS, kiwiExplore, kiwiExploreLive
    - Could start with a project containing kiwiExplore plots
      - Added benefits of reference, comparison, training
    - Need to define testing platform
    - Kevin would need to sign off on any testing
- Vignettes
  - Styles and Scales to display the default mappings
    - Figures like https://portal.kiwipharm.org/kiwiDoc/0721_Exploratory_Display_Styles.html for each style
    - Include directions on accessing elements from styles manually
  - Examples of each custom geom
  - Example of moving legend
- Function to split by a grouping variable to create file for each group
  - Could treat like a `facet_` function
  - Can `ggsave` save multiple plots with regex like base devices?
  - {ggforce} has this. Maybe we can just document how to use it in a vignette
- Categorical variable formatting. i.e., represent a factor SEXF == 0 as "Male"
- Experiment with `plot.margin` in themes
- Experiment with `legend.text` and other text sizes
- Merge concept of `setaxis` macro with `format_continuous_cognigen()`
  - Might require custom `scale_` functions
- Standards for footnotes (captions)
- Geoms
  - geom_smooth?
    - lattice uses a different smoother
    - Email from Aksana on 2019-08-15
  - histograms
  - density plots
  - bar plots
