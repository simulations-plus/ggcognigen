# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Discrete scale constructor
#'
#' @param n number of values to be mapped
#' @param geom name of the geom; either 'point', 'line', or 'bar'
#' @param style \code{list} of style elements
#' @param grayscale \code{logical} indicating whether the plot should be grayscale
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO cleanup examples
#'
#' xydata <- read.csv(
#'   file = './resources/xyplot_data.csv',
#'   header = TRUE,
#'   as.is = TRUE,
#'   stringsAsFactors = FALSE
#' )
#' xydata$DOSE <- as.factor(xydata$DOSE)
#'
#' # scatter plot
#' ggplot(data = xydata) +
#'   aes(x = TIME, y = CONCENTRATION, colour = DOSE, fill = DOSE, shape = DOSE) +
#'   geom_point() +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(n = 10, geom = 'point')
#'
#' ggplot(data = xydata) +
#'   aes(x = TIME, y = CONCENTRATION) +
#'   geom_point() +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(n = 10, geom = 'point')
#'
#' # Lineplot
#' linedata <- subset(xydata, REP == 1)
#' ggplot(data = linedata) +
#'   aes(x = TIME, y = CONCENTRATION, group = DOSE) +
#'   geom_line() +
#'   theme_cognigen()
#'
#' # Barchart
#' bardata <-  read.csv(
#'   file = './resources/bar_data.csv',
#'   header = TRUE,
#'   as.is = TRUE,
#'   stringsAsFactors = FALSE
#' )
#' bardata <- subset(bardata, GROUP > 0)
#' bardata$GROUP <- as.factor(bardata$GROUP)
#'
#' ggplot(data = bardata) +
#'   aes(x = STUDY, y = COUNT, fill = GROUP) +
#'   geom_bar(stat = 'identity', position = 'stack', alpha = 1) +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(n = 10, geom = 'bar')
#'
#' # Boxplot
#' boxdata <- read.csv(
#'   file = './resources/box_data.csv',
#'   header = TRUE,
#'   as.is = TRUE,
#'   stringsAsFactors = FALSE
#' )
#' boxdata$GROUP <- as.factor(boxdata$GROUP)
#' boxdata$CATEGORICAL <- as.factor(boxdata$CATEGORICAL)
#'
#' ggplot(data = boxdata) +
#'   aes(x = GROUP, y = CONTINUOUS, colour = CATEGORICAL) +
#'   geom_cognigenBoxplot(
#'     notch = TRUE,
#'     coef = 90,
#'     fill = 'white',
#'     #outlier.colour = NA,
#'     outlier.jitter = TRUE,
#'     outlier.size = 3,
#'     position = position_dodge(width = 0.9),
#'     na.rm = TRUE
#'   ) +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(10)
#'
#' # Histogram
#' histdata <- read.csv(
#'   file = './resources/hist_data.csv',
#'   header = TRUE,
#'   as.is = TRUE,
#'   stringsAsFactors = FALSE
#' )
#' histdata$GROUP <- as.factor(histdata$GROUP)
#' }

scale_discrete_cognigen <- function(
  n = 10,
  geom = 'point',
  style = cognigen_style(),
  grayscale = FALSE
) {

  # Input check
  if ( ! (is.integer(n) & n > 0) ){
    n <- 10
  }

  if ( ! (geom %in% c('point', 'line', 'bar', 'boxplot'))){
    stop('Invalid geom argument')
  }

  default <- cognigen_style()

  if ( !identical(get.structure(style), get.structure(default)) ){
    message('Invalid settings style. Reverting to default style.')
    style <- default
  }

  if ( !is.logical(grayscale) ){
    stop('Invalid grayscale argument')
  }
  n <- n[1]
  geom <- geom[1]
  grayscale <- grayscale[1]

  # Utility functions
  scales_col <- function(n, geom, style, grayscale){

    grayscale <- ifelse(grayscale, 'grayscale', 'color')

    aes_setting <- switch(
      geom,
      'point' = 'col',
      'line' = 'col',
      'bar' = 'col',
      'boxplot' = 'bwdotcol'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'bar' = -1,
      'boxplot' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'bar' = 'bar',
      'boxplot' = 'box.sym'
    )


    if ( n != 1 ){
      cols <- style[[geom]][[grayscale]][, aes_setting][aes_value]
    }

    rep(
      cols,
      times = n%/%10 + ifelse(n%%10>0, 1, 0),
      length.out = n
    )

  }

  scales_fill <- function(n, geom, style, grayscale){

    grayscale <- ifelse(grayscale, 'grayscale', 'color')

    aes_setting <- switch(
      geom,
      'point' = 'fill',
      'line' = 'fill',
      'bar' = 'fill',
      'boxplot' = 'bwdotfill'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'bar' = -1,
      'boxplot' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'bar' = 'bar',
      'boxplot' = 'box.sym'
    )

    if ( n != 1 ){
      fills <- style[[geom]][[grayscale]][, aes_setting][aes_value]
    }

    rep(
      fills,
      times = n%/%10 + ifelse(n%%10>0, 1, 0),
      length.out = n
    )

  }

  scales_pch <- function(n, geom, style, grayscale){

    grayscale <- ifelse(grayscale, 'grayscale', 'color')

    aes_setting <- switch(
      geom,
      'point' = 'pch',
      'line' = 'pch',
      'boxplot' = 'bwdotpch'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'boxplot' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'boxplot' = 'box.sym'
    )

    if ( n != 1 ){
      pchs <- as.integer(style[[geom]][[grayscale]][, aes_setting][aes_value])
    }

    if ( n > 10 ) {
      rep(
        pchs,
        each = 10,
        length.out = n
      )
    } else {
      pchs[1:n]
    }

  }

  scales_lty <- function(n, geom, style, grayscale){

    grayscale <- ifelse(grayscale, 'grayscale', 'color')

    aes_setting <- switch(
      geom,
      'point' = 'pch',
      'line' = 'pch',
      'boxplot' = 'value'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'boxplot' = 6
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'bar' = 'bar',
      'boxplot' = 'box.rec'
    )

    if ( n != 1 ){
      ltys <- style[[geom]][[grayscale]][, aes_setting][aes_value]
    }

    if ( n > 10 ){
      rep(
        ltys,
        each = 10,
        length.out = n
      )
    } else {
      ltys[1:n]
    }

  }

  # Function body
  if ( geom %in% c('point', 'line') ){
    list(
      ggplot2::scale_colour_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_fill_manual(values = scales_fill(n, geom, style, grayscale)),
      ggplot2::scale_shape_manual(values = scales_pch(n, geom, style, grayscale)),
      ggplot2::scale_linetype_manual(values = scales_lty(n, geom, style, grayscale))
    )
  } else if ( geom %in% 'bar') {
    list(
      ggplot2::scale_colour_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_fill_manual(values = scales_col(n, geom, style, grayscale))
    )
  } else if ( geom %in% 'boxplot') {
    list(
      ggplot2::scale_colour_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_fill_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_shape_manual(values = scales_pch(n, geom, style, grayscale)),
      ggplot2::scale_linetype_manual(values = scales_lty(n, geom, style, grayscale))
    )
  }

}
