#' Extension of ggplot2::StatBin
#'
#' Adds \code{percent} to the list of variables computed by
#' \code{\link[ggplot2]{stat_bin}} from the ggplot2 package; \code{percent}
#' represents the percentage of data in the overall data set, regardless of
#' aesthetics or group.
#'
#' @inheritParams ggplot2::stat_bin
#' @seealso \code{\link[ggplot2]{stat_bin}}
#' @examples
#' \dontrun{
#' # Count
#' ggplot(diamonds, aes(carat)) +
#'   geom_histogram()
#' # Percent
#' ggplot(diamonds, aes(carat)) +
#'   geom_histogram(aes(y=..percent..), stat = 'bin2')
#' }
#' @export
#' @rdname stat_bin2

stat_bin2 <- function(
  mapping = NULL,
  data = NULL,
  geom = "bar",
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

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBin2,
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

StatBin2 <- ggplot2::ggproto(
  "StatBin2",
  ggplot2::StatBin,
  setup_params = function(data, params){
    local_params <- params
    local_params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    tmp <- ggplot2::flipped_names(local_params$flipped_aes)
    if ('x' %in% names(tmp) && is.integer(data[[tmp$x]])){
      x <- tmp$x
      rlang::abort(
        glue::glue(
          "StatBin2 requires a continuous {x} variable: the {x} variable is discrete. ",
          "Perhaps you want stat=\"count2\"?"))
    }
    fun <- ggplot2:::fetch_ggproto(ggplot2::StatBin, 'setup_params')
    fun(data = data, params = params)
  },
  setup_data = function(data, params){
    data <- flip_data(data, params$flipped_aes)
    data['nrow_total_'] <- nrow(data)
    flip_data(data, params$flipped_aes)
  },
  compute_group = function(
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
    bins$percent <- 100 * bins$count / data$nrow_total_[1]
    bins
  }
)

#' Extension of ggplot2::StatCount
#'
#' Adds \code{percent} to the list of variables computed by \code{\link[ggplot2]{stat_count}}
#' from the ggplot2 package
#'
#' @inheritParams ggplot2::stat_count
#' @seealso \code{\link[ggplot2]{stat_count}}
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#'   \item{percent}{overall percentage of points in bin}
#' }
#' @seealso \code{\link{stat_bin2}}, which bins data in ranges and counts the
#'   cases in each range. It differs from \code{stat_count2}, which counts the
#'   number of cases at each \code{x} position (without binning into ranges).
#'   \code{\link{stat_bin2}} requires continuous \code{x} data, whereas
#'   \code{stat_count2} can be used for both discrete and continuous \code{x} data.
#'
#' @examples
#' \dontrun{
#' # Count
#' ggplot(diamonds, aes(clarity)) +
#'   geom_bar()
#' # Percent
#' ggplot(diamonds, aes(clarity)) +
#'   geom_bar(aes(y=..percent..), stat = 'count2')
#' }
#'
#' @export
#' @rdname stat_count2

stat_count2 <- function(
  mapping = NULL,
  data = NULL,
  geom = "bar",
  position = "stack",
  ...,
  width = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {

  params <- list(
    na.rm = na.rm,
    orientation = orientation,
    width = width,
    ...
  )
  if (!is.null(params$y)) {
    rlang::abort("stat_count2() must not be used with a y aesthetic.")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatCount2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

StatCount2 <- ggplot2::ggproto(
  "StatCount2",
  ggplot2::StatCount,
  setup_data = function(data, params){
    data <- flip_data(data, params$flipped_aes)
    data['nrow_total_'] <- nrow(data)
    flip_data(data, params$flipped_aes)
  },
  compute_group = function(self, data, scales, width = NULL, flipped_aes = FALSE){
    fun <- ggplot2:::fetch_ggproto(ggplot2::StatCount, 'compute_group')
    bars <- fun(self = self, data = data, scales = scales, width = width, flipped_aes = flipped_aes)
    bars$percent <- 100 * bars$count/data$nrow_total_[1]
    bars
  }
)
