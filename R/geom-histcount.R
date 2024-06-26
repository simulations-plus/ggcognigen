#' Data count for histograms
#'
#' @description
#'
#' This function is intended to work in combination with
#' \code{\link[ggplot2]{geom_histogram}} to display the sum of the values
#' represented by each bar.
#'
#' The same non-default arguments used in the the
#' \code{\link[ggplot2]{geom_histogram}} call should also be used in the
#' \code{\link{geom_histcount}} call.
#'
#' Density/percentage can be displayed by setting the \code{y} aesthetic to
#' \code{after_stat(density} / \code{after_stat(percent)} (which are provided by the
#' \code{bin2} stat) and the \code{label} aesthetic to \code{after_stat(density_label)}
#' / \code{after_stat(percent_label)} (which are provided by the \code{histcount} stat).
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @eval ggplot2:::rd_aesthetics("geom", "histcount")
#'
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams geom_barcount
#' @param geom,stat Use to override the default connection between
#'   \code{geom_histcount} and \code{stat_histcount}.
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}
#' @export
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(diamonds)
#'
#' # Histogram for continuous variable count
#' p +
#'   aes(x = price) +
#'   geom_histogram() +
#'   geom_histcount()
#'
#' # Map class to y instead to flip the orientation
#' p +
#'   aes(y = price) +
#'   geom_histogram() +
#'   geom_histcount()
#'
#' # Histogram with a fill aesthetic
#' p +
#'   aes(x = price, fill = clarity) +
#'   geom_histogram() +
#'   geom_histcount()
#'
#' # Histogram for continuous variable density
#' p +
#'   aes(x = price) +
#'   geom_histogram(aes(y = after_stat(density)), stat = 'bin2', bins = 15) +
#'   geom_histcount(aes(y = after_stat(density), label = after_stat(density_label)), bins = 15)
#'
#' # Histogram for continuous variable percentage using the bin2 stat
#' p +
#'   aes(x = price) +
#'   geom_histogram(aes(y = after_stat(percent)), stat = 'bin2', bins = 15) +
#'   geom_histcount(aes(y = after_stat(percent), label = after_stat(percent_label)), bins = 15) +
#'   ylab('percent (%)')

geom_histcount <- function(
  mapping = NULL,
  data = NULL,
  stat = "histcount",
  position = "stack",
  ...,
  digits = 3,
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
      digits = digits,
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
  extra_params = c("digits", "na.rm"),
  draw_panel = function(data, panel_params, coord) {
    data$colour <- 'black'
    ggplot2:::ggname(
      "geom_barcount",
      GeomText$draw_panel(data, panel_params, coord)
    )
  }

)
