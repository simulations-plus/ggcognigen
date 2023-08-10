#' Variant of ggplot2::position_fill
#'
#' This function performs the same data transformation as
#' \code{\link[ggplot2]{position_fill}}, but is normalized to 100 instead of 1.
#'
#' @inheritParams ggplot2::position_stack
#' @seealso \code{\link[ggplot2]{position_fill}}
#' @export
#' @rdname position_fillpercent
position_fillpercent <- function(vjust = 1, reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionFillpercent, vjust = vjust, reverse = reverse)
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionFillpercent <- ggplot2::ggproto(
  "PositionFillpercent",
  ggplot2::PositionStack,
  fill = TRUE,
  compute_panel = function(data, params, scales) {
    # Get data normalized to 1
    fun <- ggplot2:::fetch_ggproto(ggplot2::PositionFill, 'compute_panel')
    data <- fun(data = data, params = params, scales = scales)
    # Transform to percentages
    data <- flip_data(data, params$flipped_aes)
    data$y <- data$y * 100
    data$ymin <- data$ymin * 100
    data$ymax <- data$ymax * 100
    flip_data(data, params$flipped_aes)
  }
)
