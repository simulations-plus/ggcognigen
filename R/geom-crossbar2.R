# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' @export
#' @rdname geom_linerange
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
  layer(
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
GeomCrossbar2 <- ggproto(
  "GeomCrossbar2",
  Geom,
  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(colour = "black", fill = NA, size = 0.5, linetype = 1, shape = 19,
                    alpha = NA, stroke = 0.5),
  extra_params = c("na.rm", "orientation"),

  setup_params = function(data, params) {
    GeomErrorbar$setup_params(data, params)
  },
  setup_data = function(data, params) {
    GeomErrorbar$setup_data(data, params)
  },

  draw_panel = function(data, panel_params, coord, fatten = 2.5, width = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)

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
        fill = rep('white', 11), #data$fill, 11),
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
        fill = rep('white', 5), #rep(data$fill, 5),
        group = rep(seq_len(nrow(data)), 5) # each bar forms it's own group
      ))
    }
    box <- flip_data(box, flipped_aes)
    middle <- flip_data(middle, flipped_aes)

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

  draw_key = draw_key_crossbar2

)
