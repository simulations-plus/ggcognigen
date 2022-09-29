#' Data count for barcounts
#'
#' This function is intended to work in combination with
#' \code{\link[ggplot2]{geom_bar}} to display the sum of the values represented
#' by each bar. Like \code{\link[ggplot2]{geom_bar}}, this function works if the
#' \code{x} and/or \code{y} aesthetics are provided.
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param overall.stack Defines whether an overall count is displayed for
#'   stacked bars or if the counts of each individual component of the
#'   stacked bars should be displayed.
#' @param digits Integer indicating the number of significant digits to be used.
#'   Recognized values are \code{0..22}. Use \code{digits = 0} to display as
#'   integers.
#' @param geom,stat Use to override the default connection between
#'   \code{geom_barcount} and \code{stat_barcount}.
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}
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
#' p + geom_bar(position = 'stack') + geom_barcount(overall.stack = FALSE)
#'
#' # For dodged position
#' p + geom_bar(position = 'dodge') + geom_barcount(position = position_dodge(width = 0.9))
#'
#' # For fill position
#' p + geom_bar(position = 'fill') + geom_barcount(position = position_fill())
#'
#' # For fillpercent position
#' p + geom_bar(position = 'fillpercent') + geom_barcount(position = position_fillpercent()) + ylab('count (%)')
#'
#' }

geom_barcount <- function(
  mapping = NULL,
  data = NULL,
  stat = "barcount",
  position = "stack",
  ...,
  overall.stack = TRUE,
  digits = 3,
  width = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarcount,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      overall.stack = overall.stack,
      digits = digits,
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
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = "black", size = 3, angle = 0, hjust = 0.5,
    vjust = 0.5 , alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = FALSE)
    params
  },

  extra_params = c("overall.stack", "digits", "na.rm", "orientation"),

  draw_panel = function(data, panel_params, coord) {
    if ( !is.null(data$fill) ){
      data$colour <- ifelse(contrast(data$fill) > 150, 'black', 'white')
    } else {
      data$colour <- 'black'
    }
    ggplot2:::ggname(
      "geom_barcount",
      GeomText$draw_panel(data, panel_params, coord)
    )
  }

)
