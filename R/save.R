# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Save multiple ggplots
#'
#' `ggsave_multiple()` extends `ggplot2::ggsave()` to save multiple ggplots to
#' multiple files. In particular, it is suitable for saving multi-page plots
#' created using `ggforce::facet_grid_paginate()` or
#' `ggforce::facet_wrap_paginate()`.
#'
#' ggplots are saved in separate files. Therefore, the `filenames` and `plots`
#' arguments must have the same length.
#'
#' For multi-page ggplots, the filenames can be explicitly provided using a C
#' integer format expression, such as `figure%03d.png`, to produce successive
#' filenames `figure001.png`, `figure002.png`, etc. If that is not the case,
#' `ggsave_multiple()` will automatically detect multi-page plots and amend
#' filenames using the format mentioned above. The number of digits used for
#' page identification will depend on the number of pages to be created and will
#' be 2 at the minimum.
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
#'
#' g2 <- ggplot(data = diamonds) +
#'   aes(x = carat, y = price) +
#'   geom_point() +
#'   facet_wrap_paginate(vars(clarity), nrow = 2, ncol = 2, page = NULL)
#'
#' g3 <- ggplot(diamonds) +
#'   aes(carat, price) +
#'   geom_point(alpha = 0.2) +
#'   facet_grid_paginate(color ~ cut, ncol = 3, nrow = 3, page = NULL)
#'
#' g4 <- ggplot(data = diamonds) +
#'   aes(x = carat, y = price, color = clarity, alpha = 0.2) +
#'   geom_point()
#'
#' g5 <- ggplot(data = diamonds) +
#'   aes(x = carat, y = price, color = cut, alpha = 0.2) +
#'   geom_point()
#'
#' gs <- ggpubr::ggarrange(g1, g4, g5, nrow = 2, ncol = 2)
#'
#' ggsave_multiple(
#'   filenames = c('plot_g1.png', 'plot_g2.png', 'plot_g3.png', 'plot_gs.png'),
#'   plots = list(g1, g2, g3, gs),
#'   path = tempdir()
#' )
#'
#' }
#'

ggsave_multiple <- function(
  filenames,
  plots,
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

  if ( missing(plots) ){
    stop('plots argument is missing with any default.')
  }

  if ( length(filenames) == 0 ){
    stop('filenames argument is empty.')
  }

  if ( length(plots) == 0 ){
    stop('plots argument is empty.')
  }

  if ( !all(class(plots) == 'list') ){
    plots <- list(plots)
  }

  if ( !all(sapply(plots, ggplot2::is.ggplot)) ){
    stop('plots must contain `ggplot` objects')
  }

  if ( length(plots) != length(filenames) ){
    stop('Lengths of filenames and plots arguments must be identical.')
  }

  nplots <- length(plots)

  build_plots <- lapply(
    seq_along(plots),
    function(iplot, plots){
      tryCatch(
        {
          ggplot2::ggplot_build(plots[[iplot]])
        },
        error = function(e) {
          message(glue::glue('Error occured while building plot #{iplot}'))
          stop(e)
        }
      )
    },
    plots
  )

  # Find if plots are paginated and how many pages there are
  is_paginated <- sapply(
    plots,
    function(x){
      class(x$facet)[1] %in% c('FacetWrapPaginate', 'FacetGridPaginate')
    }
  )

  npages <- sapply(
    build_plots,
    function(x){
      pages <- x$layout$layout$page
      ifelse( !is.null(pages), max(pages, na.rm = TRUE), 0L)
    }
  )

  # Process each plot i
  for ( iplot in 1:length(plots) ){

    # Get number of plots per page
    if ( is_paginated[iplot] ){
      tmp <- subset(build_plots[[iplot]]$layout$layout, page == 1)
      nrow_panels <- max(tmp$ROW)
      ncol_panels <- max(tmp$COL)
    } else if ( 'ggarrange' %in% class(plots[[iplot]]) ){
      # Need to extract the number of rows and columns from the dimension of the first slot
      # in the built ggarrange object
      tmp <- build_plots[[iplot]]$data[[1]]
      nrow_panels <- 1/(tmp$ymax - tmp$ymin)
      ncol_panels <- 1/(tmp$xmax - tmp$xmin)
    } else {
      nrow_panels <- max(build_plots[[iplot]]$layout$layout$ROW)
      ncol_panels <- max(build_plots[[iplot]]$layout$layout$COL)
    }
    n_panels <- nrow_panels*ncol_panels

    # Check if width / height are defined
    # if no, call get_device_size using number of plots per page
    # if yes, use them
    if ( missing(width) | missing(height) | any(is.na(width)) | any(is.na(height)) ){
      if ( !glue::glue('{nrow_panels}x{ncol_panels}') %in%
           c('1x1', '1x2', '2x1', '1x3', '3x1', '2x2', '2x3', '3x2', '2x4', '4x2')
      ) {
        warning(
          glue::glue(
            'A non-standard plot layout was applied in plot #{iplot}: {nrow_panels}x{ncol_panels}. ',
            'Default plot dimensions may not be suitable for this case. Consider providing width and height.',
          )
        )
      }
      dim <- get_device_size(
        nplots = n_panels,
        layout = ifelse(ncol_panels > nrow_panels, 'landscape', 'portrait'),
        units = match.arg(units),
        dpi = dpi
      )
    } else {
      dim <- c(width, height)
    }

    filename <- filenames[[iplot]]

    if ( is_paginated[iplot] ){

      # Add class for S3 print methods to be used
      class(plots[[iplot]]) <- c('ggcognigen', class(plots[[iplot]]))

      # Update the filename if multiple pages required for the chosen device
      if ( npages[iplot] > 1 ){
        ext <- tolower(tools::file_ext(filename))

        # Poor man check for C notation in the filename
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
    full_path <- Reduce(file.path, c(path, filename))
    msg <- glue::glue("Saving {dim[1]} x {dim[2]} {match.arg(units)} image to '{full_path}'")

    if ( nplots > 1L ){
      msg <- glue::glue("[{iplot}/{nplots}] {msg}")
    }

    if ( npages[iplot] >= 1L ){
      msg <- glue::glue('{msg} ({npages[iplot]} pages)')
    }

    message(msg)

    suppressMessages({
      ggplot2::ggsave(
        plot = plots[[iplot]],
        filename = filename,
        device = device,
        path = path,
        scale = scale,
        width = dim[1],
        height = dim[2],
        units = units,
        dpi = dpi,
        limitsize = limitsize,
        ...
      )
    })

  }

}


# intercept print method
#' @export
print.gg <- function(x, ...) {

  if(inherits(x$facet, c('FacetWrapPaginate', 'FacetGridPaginate'))) {
    class(x) <- c('ggcognigen', class(x))
    print(x)
  } else {
    NextMethod()
  }

}

# Adapted from https://github.com/tidyverse/ggplot2/issues/86
# Author of reference function: Benjamin Guiastrennec
#' @export
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
