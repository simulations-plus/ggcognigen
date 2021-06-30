# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Save multiple ggplots
#'
#' `ggsave_multiple()` extends the capabilities of `ggplot2::ggsave()` by
#' allowing #' the saving of multiple ggplots to multiple image files. In
#' particular, it is suitable for saving multi-page plots created using
#' `ggforce::facet_grid_paginate()` or `ggforce::facet_wrap_paginate()`.
#'
#' ggplots are to saved in separate files. Therefore, the `filenames` and
#' `plots` arguments must have the same length.
#'
#' For multi-page ggplots, the filenames can be explicitly provided using
#' a C integer format expression, such as `figure%03d.png`, to produce
#' successive filenames `figure001.png`, `figure002.png`, etc. If the
#' that is not the case, `ggsave_multiple()` will automatically detect
#' which ggplot is a multi-page plot and amend the filename using the
#' format mentioned above. The number of digits used for page identification
#' will be depend on the number of pages to be created and will be 2
#' at the minimum.
#'
#' @param filenames A vector of file names to create on disk.
#' @param plots A list of plots to save.
#' @param device Device to use. Can either be a device function
#'   (e.g. [png()]), or one of "eps", "ps", "tex" (pictex),
#'   "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param path Path of the directory to save plot to: `path` and `filename`
#'   are combined to create the fully qualified file name. Defaults to the
#'   working directory.
#' @param scale Multiplicative scaling factor.
#' @param width,height,units Plot size in `units` ("in", "cm", or "mm").
#'   If not supplied, uses the size of current graphics device.
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320),
#'   "print" (300), or "screen" (72). Applies only to raster output types.
#' @param limitsize When `TRUE` (the default), `ggsave` will not
#'   save images larger than 50x50 inches, to prevent the common error of
#'   specifying dimensions in pixels.
#' @param ... Other arguments passed on to the graphics device function,
#'   as specified by `device`.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggforce)
#'
#' g1 <- ggplot(data = diamonds) +
#'   aes(x = carat, y = price) +
#'     geom_point()
#' g2 <- ggplot(data = diamonds) +
#'   aes(x = carat, y = price) +
#'   geom_point() +
#'   facet_wrap_paginate(vars(clarity), nrow = 2, ncol = 2, page = NULL)
#' g3 <- ggplot(diamonds) +
#'   aes(carat, price) +
#'   geom_point(alpha = 0.2) +
#'   facet_grid_paginate(color ~ cut:clarity, ncol = 3, nrow = 3, page = NULL)
#' ggsave_multiple(
#'   filenames = c('plot_g1.png', 'plot_g2.png', 'plot_g3.png'),
#'   plots = list(g1, g2, g3),
#'   path = '~/workspace_gitlab/tmp/'
#' }

ggsave_multiple <- function(
  filenames,
  plots = NULL,
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE,
  ...
){

  force(filenames)

  if ( length(plots) == 0 ){
    stop('plots argument is empty.')
  }

  if ( !all(class(plots) == 'list') ){
    plots <- list(plots)
  }

  if ( !all(sapply(plots, is.ggplot)) ){
    stop('plots must contain ggplot objets')
  }

  if ( length(plots) != length(filenames) ){
    stop('Lengths of filenames and plots arguments must be identical.')
  }

  # Find if plots are paginated and how many pages there are
  is_paginated <- sapply(
    plots,
    function(x){
      class(x$facet)[1] %in% c('FacetWrapPaginate', 'FacetGridPaginate')
    }
  )

  npages <- sapply(
    plots,
    function(x){
      pages <- ggplot_build(x)$layout$layout$page
      ifelse( !is.null(pages), max(pages, na.rm = TRUE), 0L)
    }
  )

  # Process each plot i
  for ( iplot in 1:length(plots) ){

    filename <- filenames[[iplot]]

    if ( is_paginated[iplot] ){

      # Add class for S3 print methods to be used=
      class(plots[[iplot]]) <- c('ggcognigen', class(plots[[iplot]]))

      # Update the filename if multiple pages required for the chosen devie
      if ( npages[iplot] > 1 ){
        ext <- tolower(tools::file_ext(filename))

        # Poor man check for C notation in the filname
        if ( ext != 'pdf' & !grepl('%', filename) ){
          filename <- sub(
            sprintf('.%s', ext),
            sprintf('-%%0%dd.%s', min(2, ceiling(log10(npages[iplot])) + 1), ext),
            filename
          )
        }
      }

    }

    # Save image
    ggsave(
      plot = plots[[iplot]],
      filename = filename,
      device = device,
      path = path,
      scale = scale,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      limitsize = limitsize,
      ...
    )

  }


}

# Adapted from https://github.com/tidyverse/ggplot2/issues/86
# Author of reference function: Benjamin Guiastrennec

print.ggcognigen <- function(x, ...) {

  # Get pages
  pages <- 1:ggforce::n_pages(repair_facet(x))

  # Prevent issue with repair_facet when page = NULL
  x$facet$params$page <- pages

  # Print all pages
  for (p in seq_along(pages)) {
    x$facet$params$page <- pages[p]
    ggplot2:::print.ggplot(x = repair_facet(x), ...)

  }

  # Prevent ggforce from dropping multiple pages value
  x$facet$params$page <- pages
}

# From https://github.com/tidyverse/ggplot2/issues/86
# Original author: Benjamin Guiastrennec

repair_facet <- function(x) {
  if (class(x$facet)[1] == 'FacetWrapPaginate' &&
      !'nrow' %in% names(x$facet$params)) {
    x$facet$params$nrow <- x$facet$params$max_row
  }
  x
}
