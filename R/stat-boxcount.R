#' @rdname geom_boxcount
#' @param coef Length of the whiskers as multiple of IQR (if lower than 50) or a
#'   confidence interval (if greater than or equal to 50). Defaults to 1.5.
#' @inheritParams ggplot2::stat_identity
#' @export

stat_boxcount <- function(
  mapping = NULL,
  data = NULL,
  geom = "boxcount",
  position = "dodge2",
  ...,
  coef = 1.5,
  na.rm = FALSE,
  orientation = NA,
  show.legend = FALSE,
  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBoxcount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      coef = coef,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBoxcount <- ggplot2::ggproto(
  "StatBoxcount",
  ggplot2::Stat,
  required_aes = c("y|x"),

  setup_data = function(data, params) {
    data <- flip_data(data, params$flipped_aes)
    data$x <- data$x %||% 0
    data <- remove_missing(
      data,
      na.rm = params$na.rm,
      vars = "x",
      name = "stat_boxcount"
    )
    data$y <- data$y + 0.05 * diff(range(data$y, na.rm = TRUE))
    ggplot2::flip_data(data, params$flipped_aes)
  },

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = TRUE,
                                          group_has_equal = TRUE,
                                          main_is_optional = TRUE)
    data <- flip_data(data, params$flipped_aes)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      rlang::abort("stat_boxcount() requires an x or y aesthetic.")
    }

    if (is.double(data$x) && !ggplot2::has_groups(data) && any(data$x != data$x[1L])) {
      warn(glue("Continuous {flipped_names(params$flipped_aes)$x} aesthetic -- did you forget aes(group=...)?"))
    }

    params
  },

  extra_params = c("na.rm", "orientation", "coef"),

  compute_group = function(data, scales, na.rm = FALSE, coef = 1.5, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)

    qs <- c(0, 0.25, 0.5, 0.75, 1)

    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])

    ci <- stats::quantile(
      data$y,
      probs = c((100-coef)/200, 1-((100-coef)/200)),
      na.rm = na.rm
    )

    if (coef < 50){
      outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
      if (any(outliers)) {
        stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
      }
    } else {
      stats[c(1,5)] <- ci
    }

    df <- data.frame(
      x = if (is.factor(data$x)) data$x[1] else mean(range(data$x)),
      y = max(data$y, na.rm = TRUE),
      ymax = stats[5],
      label = nrow(data[which(!is.na(data$x) & !is.na(data$y)), ])
    )

    df$flipped_aes <- flipped_aes
    flip_data(df, flipped_aes)
  }
)
