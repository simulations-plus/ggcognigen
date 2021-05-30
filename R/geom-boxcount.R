
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

  layer(
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
GeomBoxcount <- ggproto(
  "GeomBoxcount",
  ggplot2::Geom,
  required_aes = c("x|y"),
  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width", "orientation"),

  draw_group = function(
    data, panel_params, coord,
    outlier.position = 'jitter',
    flipped_aes = FALSE
  ) {

    data <- ggplot2::flip_data(data, flipped_aes)
    # this may occur when using geom_count(stat = "identity")
    if (nrow(data) != 1) {
      abort("Can't draw more than one boxplot per group. Did you forget aes(group = ...)?")
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
