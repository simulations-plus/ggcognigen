# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' @rdname geom_barcount
#' @inheritParams ggplot2::stat_identity
#' @export

stat_barcount <- function(
  mapping = NULL,
  data = NULL,
  geom = "barcount",
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
    stat = StatBarcount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      position = position,
      width = width,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

StatBarcount <- ggplot2::ggproto(
  "StatBarcount",
  ggplot2::Stat,
  required_aes = "x|y",
  #default_aes = aes(x = after_stat(barcount), y = after_stat(barcount), weight = 1),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      abort("stat_count() requires an x or y aesthetic.")
    }

    params
  },

  extra_params = c("na.rm", "orientation", "position"),

  compute_panel = function(data, scales, position, width = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    data$flipped_aes <- flipped_aes

    data <- data[order(data$x, data $group), ]

    if ( is.null(data$y) ){
      data$y <- data$weight %||% rep(1, length(data$x))
    }
    data <- data[!is.na(data$x) & !is.na(data$y), ]
    x <- data$x
    y <- data$y
    data$width <- width %||% (resolution(x) * 0.9)

    if ( all(data$group < 0) ){
      counts <- data[!duplicated(data$x), ]
      count <- as.numeric(unlist(tapply(y, x, sum)))
      counts$y <- count + 0.05 * max(count)
    } else {
      if ( inherits(position, 'PositionStack') ){
        counts <- data[!duplicated(data$x), ]
        count <- as.numeric(unlist(tapply(y, x, sum)))
        counts$y <- count + 0.05 * max(count)
      } else if ( inherits(position, 'PositionDodge') | inherits(position, 'PositionDodge2')) {
        counts <- data[!duplicated(data[, c('x', 'group')]), ]
        count <- aggregate(y, list(x, data$group), sum, na.rm = TRUE)
        count <- as.numeric(count[order(count[, 1], count[, 2]), 3])
        counts$y <- count + 0.05 * max(count)
        counts$xmin <- counts$xmin <- counts$x
      } else {
        counts$y <- NA
        count <- NA
      }

    }
    counts$label <- count
    flip_data(counts, flipped_aes)
  }

)

