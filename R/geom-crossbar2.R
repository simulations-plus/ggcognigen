# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Vertical intervals: crossbars
#'
#' Represent a vertical interval defined by \code{x},
#' \code{ymin} and \code{ymax}. Each case draws a single graphical object.
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @eval ggplot2:::rd_aesthetics("geom", "crossbar2")
#' @param fatten A multiplicative factor used to increase the size of the
#'   middle bar in \code{\link[ggplot2]{geom_crossbar}} and the middle point in
#'   \code{\link[ggplot2]{geom_pointrange}}.
#' @seealso
#'  \code{\link[ggplot2]{stat_summary}} for examples of these guys in use,
#'
#'  \code{\link[ggplot2]{geom_smooth}} for continuous analogue,
#'
#'  \code{\link[ggplot2]{geom_errorbarh}} for a horizontal error bar.
#' @export
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @examples
#' \dontrun{
#' # Create a simple example dataset
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#'
#' p <- ggplot(df, aes(trt, resp, colour = group))
#' p + geom_crossbar2(aes(ymin = lower, ymax = upper), width = 0.2)
#' }
geom_crossbar2 <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  fatten = 2.5,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCrossbar2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fatten = fatten,
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
GeomCrossbar2 <- ggplot2::ggproto(
  "GeomCrossbar2",
  ggplot2::Geom,
  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(colour = "black", fill = NA, size = 0.5, linetype = 1, shape = 19,
                             alpha = NA, stroke = 0.5),
  extra_params = c("na.rm", "orientation"),

  setup_params = function(data, params) {
    ggplot2::GeomErrorbar$setup_params(data, params)
  },
  setup_data = function(data, params) {
    ggplot2::GeomErrorbar$setup_data(data, params)
  },

  draw_panel = function(data, panel_params, coord, fatten = 2.5, width = NULL, flipped_aes = FALSE) {
    data <- ggplot2::flip_data(data, flipped_aes)

    middle <- transform(data, x = 0.5*(xmax+xmin), size = size * fatten, alpha = NA)

    has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
      !is.na(data$ynotchlower) && !is.na(data$ynotchupper)

    if (has_notch) {
      if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
        message("notch went outside hinges. Try setting notch=FALSE.")

      notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

      box <- ggplot2:::new_data_frame(list(
        x = c(
          data$xmin, data$xmin, data$xmin + notchindent, data$xmin, data$xmin,
          data$xmax, data$xmax, data$xmax - notchindent, data$xmax, data$xmax,
          data$xmin
        ),
        y = c(
          data$ymax, data$ynotchupper, data$y, data$ynotchlower, data$ymin,
          data$ymin, data$ynotchlower, data$y, data$ynotchupper, data$ymax,
          data$ymax
        ),
        alpha = rep(data$alpha, 11),
        colour = rep(data$colour, 11),
        size = rep(data$size, 11),
        linetype = rep(data$linetype, 11),
        fill = rep('white', nrow(data) * 11), #data$fill, 11),
        group = rep(seq_len(nrow(data)), 11)
      ))
    } else {
      # No notch
      box <- ggplot2:::new_data_frame(list(
        x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
        y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
        alpha = rep(data$alpha, 5),
        colour = rep(data$colour, 5),
        size = rep(data$size, 5),
        linetype = rep(data$linetype, 5),
        fill = rep('white', nrow(data) * 5), #rep(data$fill, 5),
        group = rep(seq_len(nrow(data)), 5) # each bar forms it's own group
      ))
    }
    box <- ggplot2::flip_data(box, flipped_aes)
    middle <- ggplot2::flip_data(middle, flipped_aes)

    ggplot2:::ggname(
      "geom_crossbar2",
      grid::gTree(
        children = grid::gList(
          GeomPolygon$draw_panel(box, panel_params, coord),
          GeomPoint$draw_panel(middle, panel_params, coord)
        )
      )
    )
  },

  draw_key = function(data, params, size) {
    grid::grobTree(
      grid::rectGrob(height = 0.5, width = 0.75),
      grid::linesGrob(c(0.125, 0.875), 0.5),
      gp = grid::gpar(
        col = data$colour %||% "grey20",
        fill = alpha("white", data$alpha),
        lwd = (data$size %||% 0.5) * .pt,
        lty = data$linetype %||% 1
      )
    )
  }

)
