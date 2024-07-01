#' A variant on box and whiskers plot (in the style of Tukey, or not...)
#'
#' The boxplot compactly displays the distribution of a continuous variable. It
#' displays five summary statistics (the median, two hinges and two whiskers),
#' and all "outlying" points individually. \code{geom_boxplot2()} is a variant
#' on the \code{\link[ggplot2]{geom_boxplot}} function from the ggplot2 package.
#' It allows users to set whisker limits based upon a confidence interval rather
#' than a multiple of the IQR, allows to display outliers with jitter, and
#' provides slightly different graphical styles when grouping/coloring is used.
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @section Summary statistics:
#' The lower and upper hinges correspond to the first and third quartiles (the
#' 25th and 75th percentiles). This differs slightly from the method used by the
#' \code{\link[graphics]{boxplot}} function, and may be apparent with small
#' samples. See \code{\link[grDevices]{boxplot.stats}} for for more information
#' on how hinge positions are calculated for \code{\link[graphics]{boxplot}}.
#'
#' By default, the upper whisker extends from the hinge to the largest value no
#' further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range,
#' or distance between the first and third quartiles). The lower whisker extends
#' from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data
#' beyond the end of the whiskers are called "outlying" points and are plotted
#' individually. If a \code{coef} argument is provided to the function call, the
#' whiskers may extend to alternative limits. If \code{coef} is set to a value
#' lower than 50, the value is used a multiplier to the IQR (default is 1.5 as
#' explained above). If \code{coef} is set to value greater than or equal to 50,
#' the whiskers extend to the limit of the \code{coef} confidence interval.
#'
#' In a notched box plot, the notches extend \code{1.58 * IQR / sqrt(n)}. This
#' gives a roughly 95% confidence interval for comparing medians. See McGill et
#' al. (1978) for more details.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "boxplot2")
#'
#'
#' @seealso \code{\link[ggplot2]{geom_boxplot}} for original ggplot2 geom
#'   function.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @inheritParams geom_crossbar2
#' @param geom,stat Use to override the default connection between
#'   \code{geom_boxplot2} and \code{stat_boxplot2}.
#' @param outlier.position
#'   By default, outliers are displayed with a small degree of jitter. Sometimes
#'   it can be useful to hide the outliers, for example when overlaying the raw
#'   data points on top of the boxplot. Hiding the outliers can be achieved by
#'   setting \code{outlier.position = NULL}. Importantly, this does not remove
#'   the outliers, it only hides them, so the range calculated for the y-axis
#'   will be the same with outliers shown and outliers hidden. If needed,
#'   outliers can be displayed without jitter by setting \code{outlier.position
#'   = 'identity'}.
#' @param outlier.colour,outlier.color,outlier.fill,outlier.shape,outlier.size,outlier.stroke,outlier.alpha
#'   Aesthetics for outliers inherited from the original
#'   \code{\link[ggplot2]{geom_boxplot}} function but that are not used in
#'   \code{geom_boxplot2}. Instead, outliers inherits colors, shapes, sizes from
#'   the box aesthetics. These aesthetics were included to maintain code
#'   compatibility with call to \code{\link[ggplot2]{geom_boxplot}}.
#' @param whisker.cap If \code{FALSE} (default), the whiskers are simple
#'   segments. If \code{TRUE}, the end of the whiskers are delineated by
#'   orthogonal segments.
#' @param notch If \code{FALSE} (default) make a standard box plot. If
#'   \code{TRUE}, make a notched box plot. Notches are used to compare groups;
#'   if the notches of two boxes do not overlap, this suggests that the medians
#'   are significantly different.
#' @param notchwidth For a notched box plot, width of the notch relative to
#'   the body (defaults to \code{notchwidth = 0.5}).
#' @param varwidth If \code{FALSE} (default) make a standard box plot. If
#'   \code{TRUE}, boxes are drawn with widths proportional to the
#'   square-roots of the number of observations in the groups (possibly
#'   weighted, using the \code{weight} aesthetic).
#' @importFrom rlang `%||%`
#' @export
#' @references McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(class, hwy))
#' p + geom_boxplot2()
#' p + geom_boxplot2(outlier.position = 'identity', coef = 90)
#' # Orientation follows the discrete axis
#' ggplot(mpg, aes(hwy, class)) + geom_boxplot2()
#'
#' p + geom_boxplot2(notch = TRUE)
#' p + geom_boxplot2(whisker.cap = TRUE)
#' p + geom_boxplot2(varwidth = TRUE)
#' p + geom_boxplot2(fill = "white", colour = "#3366FF")
#'
#' # Boxplots are automatically dodged when any aesthetic is a factor
#' p + geom_boxplot2(aes(colour = factor(drv)))
#'
#' # Use the median_symbol argument to control how the median is drawn
#' p + geom_boxplot2(aes(colour = factor(drv)), median_symbol = FALSE)
#'
#' # You can also use boxplots with continuous x, as long as you supply
#' # a grouping variable. cut_width is particularly useful
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot2()
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot2(aes(group = cut_width(carat, 0.25)))
#' # Adjust the transparency of outliers using outlier.alpha
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot2(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)
#'
#' # It's possible to draw a boxplot with your own computations if you
#' # use stat = "identity":
#' y <- rnorm(100)
#' df <- data.frame(
#'   x = 1,
#'   y0 = min(y),
#'   y25 = quantile(y, 0.25),
#'   y50 = median(y),
#'   y75 = quantile(y, 0.75),
#'   y100 = max(y)
#' )
#' ggplot(df, aes(x)) +
#'   geom_boxplot2(
#'    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
#'    stat = "identity"
#'  )
geom_boxplot2 <- function(
  mapping = NULL,
  data = NULL,
  stat = "boxplot2",
  position = "dodge2",
  ...,
  median_symbol = TRUE,
  outlier.position = 'jitter',
  outlier.colour = NULL,
  outlier.color = NULL,
  outlier.fill = NULL,
  outlier.shape = 21,
  outlier.size = 1.5,
  outlier.stroke = 0.5,
  outlier.alpha = NULL,
  whisker.cap = FALSE,
  notch = FALSE,
  notchwidth = 0.5,
  varwidth = FALSE,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- ggplot2::position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      rlang::warn("Can't preserve total widths when varwidth = TRUE.")
      position$preserve <- "single"
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      median_symbol = median_symbol,
      outlier.position = outlier.position,
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      whisker.cap = whisker.cap,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
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
GeomBoxplot2 <- ggplot2::ggproto(
  "GeomBoxplot2",
  ggplot2::Geom,
  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    weight = 1, colour = "grey20", fill = "transparent", size = 0.5,
    alpha = NA, shape = 21, linetype = "solid", linewidth = 0.5, stroke = 0.5
  ),

  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width", "orientation"),

  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params)

    show_warning <- !is.null(params$outlier.colour) |
      !is.null(params$outlier.color) |
      !is.null(params$outlier.fill) |
      params$outlier.shape != 21 |
      params$outlier.size != 1.5 |
      params$outlier.stroke != 0.5 |
      !is.null(params$outlier.alpha)

    if (show_warning){
      message(
        paste(
          'The following arguments are not used in geom_boxplot2:',
          '  outlier.colour, outlier.color, outlier.fill, outlier.shape,',
          '  outlier.size, outlier.stroke, and outlier.alpha',
          sep = '\n'
        )
      )
    }

    params
  },

  setup_data = ggplot2:::fetch_ggproto(ggplot2::GeomBoxplot, 'setup_data'),

  draw_group = function(
    data, panel_params, coord, fatten = 2.5,
    median_symbol = TRUE,
    outlier.position = 'jitter',
    outlier.colour = NULL, outlier.fill = NULL,
    outlier.shape = 21,
    outlier.size = 1.5, outlier.stroke = 0.5,
    outlier.alpha = NULL,
    whisker.cap = FALSE,
    notch = FALSE, notchwidth = 0.5, varwidth = FALSE, flipped_aes = FALSE
  ) {

    data <- ggplot2::flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot2(stat = "identity")
    if (nrow(data) != 1) {
      rlang::abort("Can't draw more than one boxplot per group. Did you forget aes(group = ...)?")
    }

    common <- list(
      colour = data$colour,
      size = data$size,
      linewidth = data$linewidth,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stroke= data$stroke,
      shape = data$shape,
      alpha = data$alpha
    )

    whiskers <- vctrs::data_frame(
      !!!c(
        list(
          x = c(
            data$x,
            data$x,
            data$x - ifelse(whisker.cap, 0.25, 0)*(data$xmax - data$xmin),
            data$x - ifelse(whisker.cap, 0.25, 0)*(data$xmax - data$xmin)
          ),
          xend = c(
            data$x,
            data$x,
            data$x + ifelse(whisker.cap, 0.25, 0)*(data$xmax - data$xmin),
            data$x + ifelse(whisker.cap, 0.25, 0)*(data$xmax - data$xmin)
          ),
          y = c(
            data$upper,
            data$lower,
            data$ymax,
            data$ymin
          ),
          yend = c(
            data$ymax,
            data$ymin,
            data$ymax,
            data$ymin
          ),
          alpha = c(NA_real_, NA_real_, NA_real_, NA_real_)
        ),
        common
      ),
      .size = 4L,
      .name_repair = "minimal"
    )
    whiskers <- ggplot2::flip_data(whiskers, flipped_aes)

    box <- vctrs::data_frame(
      !!!c(
        list(
          xmin = data$xmin,
          xmax = data$xmax,
          ymin = data$lower,
          y = data$middle,
          ymax = data$upper,
          ynotchlower = ifelse(notch, data$notchlower, NA),
          ynotchupper = ifelse(notch, data$notchupper, NA),
          notchwidth = notchwidth,
          alpha = data$alpha
        ),
        common
      ),
      .name_repair = "minimal"
    )
    box <- ggplot2::flip_data(box, flipped_aes)

    coords <- coord$transform(data, panel_params)

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1 && !is.null(outlier.position) ) {

      outliers <- vctrs::data_frame(
        !!!list(
          y = data$outliers[[1]],
          x = if (all(outlier.position == 'jitter')){
            jitter(
              x = rep(data$x, length.out = length(data$outliers[[1]])),
              amount = 0.25*(max(data$xmax, na.rm = TRUE) - min(data$xmin, na.rm = TRUE))
            )
          } else {
            data$x[1]
          },
          colour = coords$colour[1],
          fill = coords$fill[1],
          shape = coords$shape[1],
          size = coords$size[1]*fatten,
          stroke = coords$stroke[1],
          alpha = coords$alpha[1]
        ),
        .size = length(data$outliers[[1]]),
        .name_repair = "minimal"
      )
      outliers <- ggplot2::flip_data(outliers, flipped_aes)
      outliers_grob <- ggplot2::GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    # if there is a color aesthetic and median_symbol is TRUE, represent the
    # median with the corresponding symbol and color. otherwise, represent the
    # median with a line segment.
    if ( length(data$ncolours) > 0 ){
      ggplot2:::ggname(
        "geom_boxplot2",
        grid::grobTree(
          outliers_grob,
          ggplot2::GeomSegment$draw_panel(whiskers, panel_params, coord),
          GeomCrossbar2$draw_panel(
            box,
            median_symbol = median_symbol,
            fatten = fatten,
            panel_params,
            coord,
            flipped_aes = flipped_aes
          )
        )
      )
    } else {
      ggplot2:::ggname(
        "geom_boxplot2",
        grid::grobTree(
          outliers_grob,
          ggplot2::GeomSegment$draw_panel(whiskers, panel_params, coord),
          ggplot2::GeomCrossbar$draw_panel(
            box,
            fatten = fatten,
            panel_params,
            coord,
            flipped_aes = flipped_aes
          )
        )
      )
    }
  },

  draw_key = function(data, params, size) {
    grob <- ggplot2::draw_key_point(data, params, size)
    grob$gp$fontsize <- grob$gp$fontsize * 1.75
    grob
  }

)
