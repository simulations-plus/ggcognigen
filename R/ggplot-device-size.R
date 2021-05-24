#' Generate width and height of a device in pixels
#'
#' @param nplots number of plots per page
#' @param layout either 'portrait' or 'landscape'
#' @param res plot resolution
#'
#' @return a named \code{numeric} vector of width and height
#' @export
get_device_size_pixel <- function(
  nplots = 1,
  layout = 'portrait',
  res = 300
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

  if ( ! (is.numeric(res) && res[1] > 0) )
    stop('res must be a positive integer.')

  res <- as.integer(res[1])

  # Assume letter-size page and typical Cognigen template margins (in inches)
  width <- 8 - 2.5
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

  if ( layout == 'landscape'){
    size <- c(sizes[2], sizes[1])
  }

  names(sizes) <- c('width', 'height')

  return(sizes*res)

}
