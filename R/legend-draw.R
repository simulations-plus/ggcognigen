#' @export
#' @rdname draw_key
draw_key_boxplot2 <- function(data, params, size) {

  library(rlang, include.only = '%||%')
  
  # Copy of draw_key_point
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- ggplot2::translate_shape_string(data$shape)
  }

  grid::pointsGrob(
    0.5, 0.5,
    pch = data$shape,
    gp = grid::gpar(
      col = alpha(data$colour %||% "black", data$alpha),
      fill = alpha(data$fill %||% "black", data$alpha),
      fontsize = 2.5 * (data$size %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke / 2,
      lwd = (data$stroke %||% 0.5) * .stroke / 2
    )
  )
}


#' @export
#' @rdname draw_key
draw_key_crossbar2 <- function(data, params, size) {
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
