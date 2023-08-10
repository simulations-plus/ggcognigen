#' Discrete scale constructor
#'
#' @param n number of values to be mapped
#' @param geom name of the geom; either 'point', 'line', 'bar', 'boxplot', or
#'   'histogram'
#' @param style \code{list} of style elements
#' @param grayscale \code{logical} indicating whether the plot should be
#'   grayscale
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use the xydata dataset provided in the ggcognigen package
#'
#' xydata$DOSE <- as.factor(xydata$DOSE)
#'
#' # scatter plot
#' ggplot(data = xydata) +
#'   aes(x = TIME, y = CONCENTRATION, colour = DOSE, fill = DOSE, shape = DOSE) +
#'   geom_point() +
#'   scale_discrete_cognigen(n = 10, geom = 'point')
#'
#' ggplot(data = xydata) +
#'   aes(x = TIME, y = CONCENTRATION) +
#'   geom_point() +
#'   scale_discrete_cognigen(n = 10, geom = 'point')
#'
#' ggplot(data = xydata) +
#'   aes(x = TIME, y = CONCENTRATION, colour = DOSE, fill = DOSE, shape = DOSE) +
#'   geom_point() +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(style = cognigen_purple_style(), n = 10, geom = 'point')
#'
#' # Lineplot
#' linedata <- subset(xydata, REP == 1)
#' ggplot(data = linedata) +
#'   aes(x = TIME, y = CONCENTRATION, group = DOSE) +
#'   geom_line()
#'
#' # Barchart
#' ggplot(data = bardata) +
#'   aes(x = STUDY, y = COUNT, fill = GROUP) +
#'   geom_bar(stat = 'identity', position = 'stack', alpha = 1) +
#'   scale_discrete_cognigen(n = 10, geom = 'bar')
#'
#' ggplot(data = bardata) +
#'   aes(x = STUDY, y = COUNT, fill = GROUP) +
#'   geom_bar(stat = 'identity', position = 'stack', alpha = 1) +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(style = cognigen_purple_style(), n = 10, geom = 'bar')
#'
#' # Boxplot
#' ggplot(data = boxdata) +
#'   aes(x = GROUP, y = CONTINUOUS, colour = CATEGORICAL) +
#'   geom_boxplot2(
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
#'   scale_discrete_cognigen(
#'     style = cognigen_purple_style(),
#'     n = 10,
#'     geom = 'boxplot'
#'   )
#'
#' ggplot(data = boxdata) +
#'   aes(x = GROUP, y = CONTINUOUS, colour = CATEGORICAL) +
#'   geom_boxplot2(
#'     notch = TRUE,
#'     coef = 90,
#'     fill = 'white',
#'     outlier.jitter = TRUE,
#'     outlier.size = 3,
#'     position = position_dodge(width = 0.9),
#'     na.rm = TRUE
#'   ) +
#'   theme_cognigen() +
#'   scale_discrete_cognigen(10)
#'
#' # Histogram
#'
#' }

scale_discrete_cognigen <- function(
  n = 10,
  geom = 'point',
  style = cognigen_style(),
  grayscale = FALSE
) {

  # Input check
  if( is.numeric(n) & !is.integer(n) ) {
    n <- as.integer(ceiling(n))
  }

  if ( ! (is.integer(n) & n > 0) ){
    n <- 10
  }

  if ( ! (geom %in% c('point', 'line', 'bar', 'boxplot', 'histogram'))){
    stop('Invalid geom argument')
  }

  default <- cognigen_style()

  if ( is.character(style) ){
    if ( file.exists(style) ){
      style <- read_style(path = style)
    } else {
      stop('Invalid path to style file')
    }
  }

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
      'boxplot' = 'bwdotcol',
      'histogram' = 'col'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'bar' = -1,
      'boxplot' = -1,
      'histogram' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'bar' = 'bar',
      'boxplot' = 'box.sym',
      'histogram' = 'hist.dens'
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
      'boxplot' = 'bwdotfill',
      'histogram' = 'fill'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'bar' = -1,
      'boxplot' = -1,
      'histogram' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'bar' = 'bar',
      'boxplot' = 'box.sym',
      'histogram' = 'hist.dens'
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
      'boxplot' = 'bwdotpch',
      'histogram' = 'pch'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'boxplot' = -1,
      'histogram' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'boxplot' = 'box.sym',
      'histogram' = 'hist.dens'
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
      'point' = 'lty',
      'line' = 'lty',
      'boxplot' = 'value',
      'histogram' = 'hidlty'
    )
    aes_value <- switch(
      geom,
      'point' = -1,
      'line' = -1,
      'boxplot' = 6,
      'histogram' = -1
    )
    geom <- switch(
      geom,
      'point' = 'scatter',
      'line' = 'scatter',
      'bar' = 'bar',
      'boxplot' = 'box.rec',
      'histogram' = 'hist.dens'
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
  } else if ( geom %in% 'bar' ) {
    list(
      ggplot2::scale_colour_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_fill_manual(values = scales_col(n, geom, style, grayscale))
    )
  } else if ( geom %in% 'boxplot' ) {
    list(
      ggplot2::scale_colour_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_fill_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_shape_manual(values = scales_pch(n, geom, style, grayscale)),
      ggplot2::scale_linetype_manual(values = scales_lty(n, geom, style, grayscale))
    )
  } else if ( geom %in% 'histogram' ) {
    list(
      ggplot2::scale_colour_manual(values = scales_col(n, geom, style, grayscale)),
      ggplot2::scale_fill_manual(values = scales_fill(n, geom, style, grayscale)),
      ggplot2::scale_shape_manual(values = scales_pch(n, geom, style, grayscale)),
      ggplot2::scale_linetype_manual(values = scales_lty(n, geom, style, grayscale))
    )
  }

}


#' Continuous scale for color and fill aesthetics with a purple gradient
#'
#' @inheritParams ggplot2::scale_color_gradient
#' @inheritDotParams ggplot2::continuous_scale scale_name name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   value = seq(1, 100),
#'   x = runif(100),
#'   y = runif(100),
#'   z = rnorm(100)
#' )
#'
#' # color
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_point(aes(color = z), pch = 19) +
#'   scale_continuous_cognigen()
#'
#' # fill
#' ggplot(df, aes(x = value, y = y)) +
#'   geom_bar(aes(fill = z), stat = "identity") +
#'   scale_continuous_cognigen()
#' }
scale_continuous_cognigen <- function(..., na.value = "grey50", guide = "colourbar") {

  ggplot2::scale_color_gradient(
    ...,
    low = "#E79BFF",
    high = "#3F184A",
    na.value = na.value,
    guide = guide,
    aesthetics = c("colour", "fill")
  )

}
