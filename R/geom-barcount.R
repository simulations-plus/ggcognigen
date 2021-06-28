# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Data count for barcounts
#'
#' This function is intended to work in combination with [geom_bar()] and
#' to display the sum of the values represented by each bar.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param geom,stat Use to override the default connection between
#'   `geom_barcount` and `stat_barcount`.
#'
#' @seealso [geom_bar()]
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(mpg)
#' p + aes(x = class) + geom_bar() + geom_barcount()
#'
#' # Map class to y instead to flip the orientation
#' p + aes(y = class) + geom_bar() + geom_barcount()
#'
#' # For stacked position
#' p <- ggplot(diamonds, aes(color, fill = cut))
#' p + geom_bar(position = 'stack') + geom_barcount()
#'
#' # For dodged position
#' p + geom_bar(position = 'dodge') + geom_barcount(position = position_dodge(width = 0.9))
#' }

geom_barcount <- function(
  mapping = NULL,
  data = NULL,
  stat = "barcount",
  position = "stack",
  ...,
  width = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE) {

  position <- ggplot2:::check_subclass(position, 'Position', env = parent.frame())

  if ( inherits(position, 'PositionFill') ) {
    rlang::abort("stat_boxcount() is not compatible with position_fill().")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarcount,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      position = position,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomBarcount <- ggplot2::ggproto(
  "GeomBarcount",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = "black", size = 3, angle = 0, hjust = 0.5,
    vjust = 0.5 , alpha = NA, family = "", fontface = 1, lineheight = 1.2,
  ),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = FALSE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  draw_panel = function(data, panel_params, coord) {
    data$colour <- 'black'
    ggplot2:::ggname(
      "geom_barcount",
      GeomText$draw_panel(data, panel_params, coord)
    )
  }

)
