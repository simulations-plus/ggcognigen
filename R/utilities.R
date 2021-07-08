# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

contrast <- function(color){
  is.color <- sapply(
    color,
    function(x) {
      tryCatch(
        is.matrix(grDevices::col2rgb(x)),
        error = function(e) FALSE
      )
    }
  )
  if ( any(!is.color) ){
    rlang::abort( sprintf('Invalid color(s): %s', paste(color[!is.color], collapse = ', ')) )
  }
  color <- grDevices::col2rgb(color)
  color[1,]*0.299 + color[2,]*0.587 + color[3,]*0.114
}
