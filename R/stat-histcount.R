# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' @rdname geom_histcount
#' @inheritParams ggplot2::stat_identity
#' @export

stat_histcount <- function(
  mapping = NULL,
  data = NULL,
  geom = "histcount",
  position = "stack",
  ...,
  binwidth = NULL,
  bins = NULL,
  center = NULL,
  boundary = NULL,
  breaks = NULL,
  closed = c("right", "left"),
  pad = FALSE,
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
    stat = StatHistcount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
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

StatHistcount <- ggplot2::ggproto(
  "StatHistcount",
  StatBin2,

  default_aes = ggplot2::aes(x = after_stat(count), y = after_stat(count), weight = 1),

  setup_params = function(data, params){

    local_params <- params
    local_params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    tmp <- ggplot2::flipped_names(local_params$flipped_aes)
    if ('x' %in% names(tmp) && is.integer(data[[tmp$x]])){
      x <- tmp$x
      rlang::abort(
        glue::glue(
          "StatHistcount requires a continuous {x} variable: the {x} variable is discrete. ",
          "Perhaps you want stat=\"barcount\"?"))
    }
    StatBin2$setup_params(data = data, params = params)

  },

  compute_panel = function(
    data,
    scales,
    binwidth = NULL,
    bins = NULL,
    center = NULL,
    boundary = NULL,
    closed = c("right", "left"),
    pad = FALSE,
    breaks = NULL,
    flipped_aes = FALSE,
    origin = NULL,
    right = NULL,
    drop = NULL,
    width = NULL
  ) {

    fun <- ggplot2:::fetch_ggproto(ggplot2::StatBin, 'compute_group')
    bins <- fun(
      data = data, scales = scales, binwidth = binwidth, bins = bins,
      center = center, boundary = boundary,
      closed = closed, pad = pad,
      breaks = breaks, flipped_aes = flipped_aes,
      origin = origin, right = right, drop = drop,
      width = width
    )
    nrows <- data$nrow_total_[1]
    bins$percent <- 100 * bins$count/nrows

    # Add PANEL and group
    bins$PANEL <- unique(data$PANEL)
    bins$group <- -1

    # Store labels
    bins$label <- bins$count
    bins$percent_label <- sprintf('%s %%', signif(bins$percent, 3))
    bins$density_label <- signif(bins$density, 3)
    bins$ncount_label <- signif(bins$ncount, 3)
    bins$ndensity_label <- signif(bins$ndensity, 3)

    # Add margins
    bins$count <- bins$count + 0.05 * max(bins$count)
    bins$percent <- bins$percent + 0.05 * max(bins$percent)
    bins$density <- bins$density + 0.05 * max(bins$density)
    bins$ncount <- bins$ncount + 0.05 * max(bins$ncount)
    bins$ndensity <- bins$ndensity + 0.05 * max(bins$ndensity)

    bins
  }

)

