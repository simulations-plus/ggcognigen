# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Data count for box and whiskers plot
#'
#' This function is intended to work in combination with [geom_boxplot2()] and
#' to display the number of data points used for the calculation of statistics
#' which are graphically represented by each box and whiskers.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param geom,stat Use to override the default connection between
#'   `geom_boxcount` and `stat_boxcount`.
#' @param outlier.position
#'   This controls the placement of the counts above the boxes, depending on
#'   whether outliers were hidden (`outlier.position = NULL`) or
#'   displayed (`outlier.position = 'identity'` or `outlier.position = 'jitter'`)
#'   when the call to [geom_boxplot2()] was made. By default, outliers are
#'   assumed to be displayed .
#'
#' @seealso [geom_boxplot2()]
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(mpg, aes(class, hwy))
#' p + geom_boxplot2() + geom_boxcount()
#'
#' # For display on log axis scale, use the scale_y_continuous function
#' # Using coord_trans(y ='log10') would display the counts at the wrong place
#' p + geom_boxplot2() + geom_boxcount() + scale_y_continuous(trans = 'log10')
#' }

geom_boxcount <- function(
  mapping = NULL,
  data = NULL,
  stat = "boxcount",
  position = "dodge2",
  ...,
  outlier.position = 'jitter',
  na.rm = FALSE,
  orientation = NA,
  show.legend = FALSE,
  inherit.aes = TRUE) {

  #position <- ggplot2:::check_subclass(position, "Position", env = parent.frame())

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxcount,
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      outlier.position = outlier.position,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxcount <- ggplot2::ggproto(
  "GeomBoxcount",
  ggplot2::Geom,
  required_aes = c("x|y"),
  default_aes = ggplot2::aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
  ),

  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width", "orientation"),

  setup_data = function(data, params) {
    data$xmin <- data$xmax <- data$x
    data$ymin <- data$ymax <- data$y
    data$width <- 0
    data
  },

  draw_group = function(
    data, panel_params, coord,
    outlier.position = 'jitter',
    flipped_aes = FALSE
  ) {

    data <- ggplot2::flip_data(data, flipped_aes)
    # this may occur when using geom_count(stat = "identity")
    if (nrow(data) != 1) {
      rlang::abort("Can't draw more than one boxplot per group. Did you forget aes(group = ...)?")
    }

    data$y <- ifelse(
      length(outlier.position) > 0,
      data$y,
      data$ymax
    )
    data$colour <- 'black'

    ggplot2:::ggname(
      "geom_boxcount",
      GeomText$draw_panel(data, panel_params, coord)
    )
  }

)
