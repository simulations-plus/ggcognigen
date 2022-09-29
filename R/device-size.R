#' Generate width and height of a device based upon a given number of plots per
#' page
#'
#' @param nplots Number of plots per page.
#' @param layout Either 'portrait' or 'landscape'.
#' @param units One of ('in', 'cm', 'mm', 'px'). If not supplied, defaults to
#'   'in'.
#' @param dpi Plot resolution in dot per inch.
#'
#' @return A named \code{numeric} vector of width and height
#' @export
#' @examples
#' \dontrun{
#' dims <- get_device_size(nplots = 4,
#'                         units = 'in',
#'                         dpi = 300)
#'
#' plot <- ggplot(mpg, aes(class, hwy)) +
#'   geom_point()
#'
#' ggsave(filename = 'plot.png',
#'        plot = plot,
#'        path = tempdir(),
#'        width = dims['width'],
#'        height = dims['height'],
#'        units = 'in',
#'        dpi = 300)
#' }
get_device_size <- function(
  nplots = 1,
  layout = 'portrait',
  units = c('in', 'cm', 'mm', 'px'),
  dpi = 300
){

  if ( !is.numeric(nplots) )
    stop('nplots must be an integer.')

  nplots <- as.integer(nplots[1])

  if ( nplots <= 0 )
    stop('nplots must be greater than 0.')

  if ( nplots > 8 ) nplots <- 8

  if ( ! (is.character(layout) && layout[1] %in% c('portrait', 'landscape')) )
    stop('layout must be \'portait\' or \'landscape\'.')

  layout <- layout[1]

  if ( ! (is.numeric(dpi) && dpi[1] > 0) )
    stop('dpi must be a positive integer.')

  dpi <- as.integer(dpi[1])

  # Assume letter-size page and typical Cognigen template margins (in inches)
  width <- 8.5 - 2.5
  height <- 11 - 3.32


  # Creates the vector of sizes
  sizes <- switch(
    nplots,
    c(width, height*0.5),
    c(width, height),
    c(width, height),
    c(width, height*0.5),
    c(width, height*0.75),
    c(width, height*0.75),
    c(width, height),
    c(width, height)
  )

  if ( layout == 'landscape' ){
    sizes <- c(sizes[2], sizes[1])
  }

  # Convert to requested units
  units <- match.arg(units)
  from_inches <- function(x) {
    x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10, px = dpi)[units]
  }

  sizes <- from_inches(sizes)

  names(sizes) <- c('width', 'height')

  return(structure(sizes, units = units))

}

#' Generate width and height in pixels of a device based upon a given number of plots per page
#'
#' @return A named \code{numeric} vector of width and height
#' @export
#' @rdname get_device_size
#'
get_device_size_pixel <- function(
  nplots = 1,
  layout = 'portrait',
  dpi = 300
){

  get_device_size(
    nplots = nplots,
    layout = layout,
    units = 'px',
    dpi = dpi
  )
}
