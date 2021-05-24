#' Format continuous values
#'
#' @param x breaks
#'
#' @return either a \code{numeric} or \code{expression} vector the same length as \code{x}
#' @export
#'
#' @examples
#' \dontrun{
#' min <- -5000
#' max <- 5000
#' set.seed(123)
#'
#' data <- data.frame(
#'   x = runif(1000, min, max),
#'   y = runif(1000, min, max)
#' )
#'
#' ggplot(data, aes(x = x, y = y)) +
#'   geom_point() +
#'   theme_cognigen() +
#'   scale_y_continuous(labels = format_continuous_cognigen)
#'
#' min <- 0
#' max.x <- 1000
#' max.y <- 3
#' set.seed(123)
#'
#' data <- data.frame(
#'   x = runif(1000, min, max.x),
#'   y = rlnorm(1000, min, max.y)
#' )
#'
#' ggplot(data, aes(x = x, y = y)) +
#'   geom_point() +
#'   theme_cognigen_grid(minor.x = TRUE, minor.y = TRUE) +
#'   scale_y_log10(minor_breaks = minor_breaks_log, labels = format_continuous_cognigen)
#'}
format_continuous_cognigen <- function(x){

  sciNot.expression <- function(x, digits = 5){

    exponent <- floor(log10(x))
    base <- signif(x / 10^exponent, digits)
    res <- vector(mode = 'expression', length = length(x))

    for (i in seq_along(x)){

      if ( x[i] < 0 ) {
        neg <- TRUE
      } else {
        neg <- FALSE
      }

      if ( neg ){
        if (base[i] == 1){
          res[i] <- as.expression(
            substitute(
              -10^exponent,
              list(exponent = exponent[i])
            )
          )
        } else {
          res[i] <- as.expression(
            substitute(
              -base%.%10^exponent,
              list(base = base[i], exponent = exponent[i])
            )
          )
        }
      } else {
        if (base[i] == 1){
          res[i] <- as.expression(
            substitute(
              10^exponent,
              list(exponent = exponent[i])
            )
          )
        } else {
          res[i] <- as.expression(
            substitute(
              base%.%10^exponent,
              list(base = base[i], exponent = exponent[i])
            )
          )
        }
      }
    }

    return(res)

  }

  sapply(
    x,
    function(xx){
      if ( is.na(xx) ){
        NA
      } else if ( abs(xx) < 999 ){
        return(xx)
      } else {
        return(sciNot.expression(xx))
      }
    }
  )

}

minor_breaks_log <- function(x){

  exponents <- min(floor(log10(x)), na.rm = TRUE) : max(ceiling(log10(x)), na.rm = TRUE)
  rep(1:9, length(exponents))*(10^rep(exponents, each = 9))

}
