# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Data count for histograms
#'
#' This function is intended to work in combination with [geom_histogram()] and
#' to display the sum of the values represented by each bar.
#'
#' The same non-default arguments used in the the [geom_histogram()] call should
#' also be used in the `geom_histcount()` call.
#'
#' Density/percentage can be displayed by setting the `y` aesthetic to
#' `..density..` / `..percent..` (which are provided by the `bin2` stat) and the
#' `label` aesthetic to `..density_label..` / `..percent_label..` (which are
#' provided by the `histcount` stat).
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @inheritParams ggplot2::geom_histogram
#' @param geom,stat Use to override the default connection between
#'   `geom_histcount` and `stat_histcount`.
#'
#' @seealso [geom_bar()]
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(diamonds)
#'
#' # Histogram for continuous variable count
#' p + aes(x = price) + geom_histogram() + geom_histcount()
#'
#' # Map class to y instead to flip the orientation
#' p + aes(y = price) + geom_histogram() + geom_histcount()
#'
#' # Histogram with a fill aesthetic
#' p + aes(x = price, fill = clarity) + geom_histogram() + geom_histcount()
#'
#' # Histogram for continuous variable density
#' p +
#'   aes(x = price) +
#'   geom_histogram(aes(y = ..density..), stat = 'bin2', bins = 15) +
#'   geom_histcount(aes(y = ..density.., label = ..density_label..), bins = 15)
#'
#' # Histogram for continuous variable percentage using the bin2 stat
#' p +
#'   aes(x = price) +
#'   geom_histogram(aes(y = ..percent..), stat = 'bin2', bins = 15) +
#'   geom_histcount(aes(y = ..percent.., label = ..percent_label..), bins = 15) +
#'   ylab('percent (%)')
#' }

geom_histcount <- function(
  mapping = NULL,
  data = NULL,
  stat = "histcount",
  position = "stack",
  ...,
  binwidth = NULL,
  bins = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {

  position <- ggplot2:::check_subclass(position, 'Position', env = parent.frame())

  if ( !inherits(position, 'PositionStack') ) {
    rlang::abort("stat_histcount() is only compatible with position_stack().")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHistcount,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomHistcount <- ggplot2::ggproto(
  "GeomHistcount",
  GeomBarcount,
  draw_panel = function(data, panel_params, coord) {
    data$colour <- 'black'
    ggplot2:::ggname(
      "geom_barcount",
      GeomText$draw_panel(data, panel_params, coord)
    )
  }

)
