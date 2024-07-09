#' CPP ggplot2 themes. Run by default at startup of \code{ggcognigen}
#'
#' @param smaller \code{logical} indicating whether to use smaller font size
#' @param ... additional theme elements to override defaults. See
#'   \code{\link[ggplot2]{theme}}.
#'
#' @export
#'
#' @importFrom ggplot2 `%+replace%`
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(class, hwy)) +
#'   geom_point()
#'
#' p + theme_cognigen()
#'
#' p + theme_cognigen_grid()
theme_cognigen <- function(smaller = FALSE, ...) {

  # font is set to "" which will default to "sans"
  font <- ""

  # start with a standard ggplot theme
  ggplot2::theme_bw() %+replace%

    ggplot2::theme(

      # grid elements
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # text elements
      plot.title = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 11, 12),
        face = "bold",
        vjust = 1
      ),

      plot.subtitle = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 10, 11)
      ),

      plot.caption = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 8, 9),
        hjust = 0
      ),

      axis.title = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 9, 10),
        face = "bold"
      ),

      axis.text = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 8, 9)
      ),

      legend.title = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 8, 9)
      ),

      legend.text = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 8, 9)
      ),

      strip.text = ggplot2::element_text(
        family = font,
        size = ifelse(smaller, 8, 9)
      ),

      # facet elements
      strip.background = ggplot2::element_rect(),
      strip.background.x = ggplot2::element_rect(),
      strip.background.y = ggplot2::element_rect()

    ) %+replace%

    # user defined elements
    ggplot2::theme(...)

}


#' @rdname theme_cognigen
#'
#' @param major.x,major.y,minor.x,minor.y \code{logical} indicating whether to
#' draw major/minor grid lines on the respective axis
#' @param smaller \code{logical} indicating whether to use smaller font size
#'
#' @export
theme_cognigen_grid <- function(
  major.x = TRUE,
  major.y = TRUE,
  minor.x = FALSE,
  minor.y = FALSE,
  smaller = FALSE,
  ...
){

  if ( !is.logical(major.x) ) major.x <- TRUE
  if ( !is.logical(major.y) ) major.y <- TRUE
  if ( !is.logical(minor.x) ) minor.x <- TRUE
  if ( !is.logical(minor.y) ) minor.y <- TRUE

  theme_cognigen(smaller = smaller) %+replace%

    ggplot2::theme(
      panel.grid.major.x = if (major.x[1]) {
        ggplot2::element_line(colour = '#dddddd', linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.major.y = if (major.y[1]) {
        ggplot2::element_line(colour = '#dddddd', linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor.x = if (minor.x[1]) {
        ggplot2::element_line(colour = '#dddddd', linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor.y = if (minor.y[1]) {
        ggplot2::element_line(colour = '#dddddd', linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      }
    ) %+replace%

    # user defined elements
    ggplot2::theme(...)

}
