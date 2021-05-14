###############################################################################
# Name: GitLab: R/shared-code/themes/ggplot-theme.R
#
# Copyright 2020, Cognigen Corporation.  The contents of this program are
# confidential and cannot be used - in any form - for anything outside the
# drug and specific project for which the file is provided.
###############################################################################
#
# PURPOSE:
# Create a ggplot2 theme; Consider different styles currently used across
# the company.
# This is a work in progress
###############################################################################

# all available components are described in the documentation
?ggplot2::theme

# Sections needed beyond theme --------------------------------------------

# This file contains a {ggplot2} theme
# The theme controls non-data components of the plot
# We will also need to select color palettes for different aes elements
# as well as shapes, line types, etc
# see "ExploreLive styles functions" section in this file
# The legend is not adjusted because most legends require specific adjustments


# Cognigen theme ----------------------------------------------------------

# Arguments passed to `...` further control the theme

theme_cognigen <- function(...) {

  if(!"ggplot2" %in% loadedNamespaces()) library(ggplot2)

  # font is set to "" which will default to "sans"
  font <- ""

  # start with a standard ggplot theme
  theme_bw() %+replace%

    theme(

      # grid elements
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # text elements
      plot.title = element_text(
        family = font,
        size = 14,
        face = "bold",
        vjust = 1
      ),

      plot.subtitle = element_text(
        family = font,
        size = 11
      ),

      plot.caption = element_text(
        family = font,
        size = 9
      ),

      axis.title = element_text(
        family = font,
        face = "bold",
        size = 11
      ),

      axis.text = element_text(
        family = font,
        size = 10
      ),

      # user defined elements
      ...

    )

}

theme_cognigen_grid <- function(
  major.x = TRUE,
  major.y = TRUE,
  minor.x = FALSE,
  minor.y = FALSE,
  ...
){

  if ( !is.logical(major.x) ) major.x <- TRUE
  if ( !is.logical(major.y) ) major.y <- TRUE
  if ( !is.logical(minor.x) ) minor.x <- TRUE
  if ( !is.logical(minor.y) ) minor.y <- TRUE

  theme_cognigen(...) %+replace%

    theme(
      panel.grid.major.x = if (major.x[1]) {
        element_line(colour = '#dddddd', size = 0.25)
      } else {
        element_blank()
      },
      panel.grid.major.y = if (major.y[1]) {
        element_line(colour = '#dddddd', size = 0.25)
      } else {
        element_blank()
      },
      panel.grid.minor.x = if (minor.x[1]) {
        element_line(colour = '#dddddd', size = 0.25)
      } else {
        element_blank()
      },
      panel.grid.minor.y = if (minor.y[1]) {
        element_line(colour = '#dddddd', size = 0.25)
      } else {
        element_blank()
      }
    )

}
