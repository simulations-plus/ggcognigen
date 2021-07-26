# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

# get.structure
# utility function for style checks
get.structure <- function(x){
  if ( !is.data.frame(x) ){
    if ( is.list(x) ){
      lapply(x, get.structure)
    } else {
      stop('x argument is not a list.')
    }
  } else {
    sort(names(x))
  }
}

#' Cognigen ggplot2 style (default graphical style)
#'
#' @return \code{list} of style elements
#' @export
cognigen_style <- function(){

  list(
    scatter = list(
      color = data.frame(
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        col = c('#5B5B5B', '#FF0000', '#0000FF', '#008000', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                '#888888', '#50E050', '#8F308F'),
        fill = c('#5B5B5B', '#FF0000', '#0000FF', '#008000', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                 '#888888', '#50E050', '#8F308F'),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5,
                0.45, 0.5, 0.5, 0.45, 0.5),
        lty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                'dotdash'),
        lwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        col = c('#5B5B5B', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                '#888888', '#BEBEBE', '#4C4C4C'),
        fill = c('#5B5B5B', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                 '#888888', '#BEBEBE', '#4C4C4C'),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        lty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                'dotdash'),
        lwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      )
    ),
    ramp = list(
      color = data.frame(
        pch = c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        col = c('#9E0142', '#D53E4F', '#F46D43', '#FDAE61', '#FEE08B', '#E6F598', '#ABDDA4', '#66C2A5',
                '#3288BD', '#5E4FA2'),
        fill = c('transparent', NA, NA, NA, NA, NA, NA, NA, NA, NA),
        cex = c(0.5, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        pch = c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        col = c('#D8D8D8', '#C0C0C0', '#A8A8A8', '#909090', '#787878', '#606060', '#484848', '#303030',
                '#181818', '#000000'),
        fill = c('transparent', NA, NA, NA, NA, NA, NA, NA, NA, NA),
        cex = c(0.5, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE
      )
    ),
    bar = list(
      color = data.frame(
        col = c('#FFFFFF', '#FF7F7F', '#7F7FFF', '#7FBF7F', '#FFCF7F', '#FF7FCF', '#7FEEFF', '#E3B485',
                '#C3C3C3', '#A7EFA7', '#C797C7'),
        border = rep('#000000', 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        col = c('#FFFFFF', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                '#888888', '#BEBEBE', '#4C4C4C'),
        border = rep('#000000', 11),
        stringsAsFactors = FALSE
      )
    ),
    box.sym = list(
      color = data.frame(
        bwdotpch = c('|', '3', '0', '1', '2', '4', '5', '19', '15', '8', '17'),
        bwdotcol = c('#000000', '#FF0000', '#0000FF', '#008080', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                     '#888888', '#50E050', '#8F308F'),
        bwdotfill = c('#000000', '#FF0000', '#0000FF', '#008080', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                      '#888888', '#50E050', '#8F308F'),
        bwdotcex = rep(0.45, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        bwdotpch = c('|', '3', '0', '1', '2', '4', '5', '19', '22', '8', '24'),
        bwdotcol = c('#000000', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                     '#888888', '#BEBEBE', '#4C4C4C'),
        bwdotfill = c('#000000', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                      '#888888', '#BEBEBE', '#4C4C4C'),
        bwdotcex = rep(0.45, 11),
        stringsAsFactors = FALSE
      )
    ),
    box.rec = list(
      color = data.frame(
        value = c('#000000', 'solid', '1', '#000000', 'transparent', 'solid', '1', '21', '#000000', '0.45'),
        row.names = c('Whisker color',
                      'Whisker line type', 'Whisker line width', 'Box border color', 'Box fill',
                      'Box border type', 'Box border width', 'Outlier symbol', 'Outlier color',
                      'Outlier scaling'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#000000', 'solid', '1', '#000000', 'transparent', 'solid', '1', '21', '#000000', '0.45'),
        row.names = c('Whisker color',
                      'Whisker line type', 'Whisker line width', 'Box border color', 'Box fill',
                      'Box border type', 'Box border width', 'Outlier symbol', 'Outlier color',
                      'Outlier scaling'),
        stringsAsFactors = FALSE
      )
    ),
    hist = list(
      color = data.frame(
        value = c('#FFFFFF', '#000000', 'solid', '1', '#FF0000', 'dashed', '1.5'),
        row.names = c('Histogram fill color', 'Histogram border color', ' Histogram border type',
                      'Histogram border width', 'Normal distribution color', 'Normal distribution type',
                      'Normal distribution width'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#FFFFFF', '#000000', 'solid', '1', '#363636', 'dashed', '1.5'),
        row.names = c('Histogram fill color', 'Histogram border color', ' Histogram border type',
                      'Histogram border width', 'Normal distribution color', 'Normal distribution type',
                      'Normal distribution width'),
        stringsAsFactors = FALSE
      )
    ),
    hist.dens = list(
      color = data.frame(
        col = c('#000000', '#FF0000', '#0000FF', '#008000', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                '#888888', '#50E050', '#8F308F'),
        fill = c('#FFFFFF', '#FF7F7F', '#7F7FFF', '#7FBF7F', '#FFCF7F', '#FF7FCF', '#7FEEFF', '#E3B485',
                 '#C3C3C3', '#A7EFA7', '#C797C7'),
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        hidcol = c('#0066FF', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        hidlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                   'dotdash'),
        hidlwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        col = c('#000000', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                '#888888', '#BEBEBE', '#4C4C4C'),
        fill = c('#FFFFFF', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                 '#888888', '#BEBEBE', '#4C4C4C'),
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5,
                0.45, 0.5, 0.5, 0.45, 0.5),
        hidcol = c('#5D5D5D', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        hidlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                   'dotdash'),
        hidlwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      )
    ),
    vpc = list(
      color = data.frame(
        value = c(
          # Observed data
          '1', '#000000', '0.5',
          'solid', '#FF0000', '1.5', 'solid', '#FF0000', '1.5', 'solid', '#FF0000', '1.5',
          # Simulated data
          '#90ee90', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5',
          # PI
          '#0000FF', '#0000FF', '#0000FF'
        ),
        value2 = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#FF0000', '1.5', 'dashed', '#FF0000', '1.5', 'dashed', '#FF0000', '1.5',
          # Simulated data
          '#90ee90', 'solid', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5',
          # PI
          '#FF0000', '#0000FF', '#0000FF'
        ),
        value3 = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#FF0000', '1.5', 'solid', '#0000FF', '1.5', 'solid', '#0000FF', '1.5',
          # Simulated data
          '#90ee90', 'solid', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5',
          # PI
          '#FF0000', '#0000FF', '#0000FF'
        ),
        row.names = c(
          # Observed data
          'Symbol', 'Symbol color', 'Symbol scale',
          'Median line type', 'Median line color', 'Median line width',
          'CI upper limit line type', 'CI upper limit line color', 'CI upper limit line width',
          'CI lower limit line type', 'CI lower limit line color', 'CI lower limit line width',
          # Simulated data
          'PI area color',
          'Predicted median line type', 'Predicted median line color', 'Predicted median line width',
          'PI upper limit line type', 'PI upper limit line color', 'PI upper limit line width',
          'PI lower limit line type', 'PI lower limit line color', 'PI lower limit line width',
          # PI CI
          'CI around median - Area color', 'CI around PI upper limit - Area color', 'CI around PI lower limit - Area color'
        ),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#000000', '1.5', 'solid', '#000000', '1.5', 'solid', '#000000', '1.5',
          # Simulated data
          '#D1D1D1', 'dashed', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5',
          # PI
          '#5F5F5F', '#5F5F5F', '#5F5F5F'
        ),
        value2 = c(
          # Observed data
          '3', '#000000', '0.5',
          'dotted', '#000000', '3', 'F8', '#000000', '1.5', 'F8', '#000000', '1.5',
          # Simulated data
          '#D1D1D1', 'solid', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5',
          # PI
          '#5F5F5F', '#5F5F5F', '#5F5F5F'
        ),
        value3 = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#000000', '1.5', 'F8', '#000000', '1.5', 'F8', '#000000', '1.5',
          # Simulated data
          '#90ee90', 'solid', '#000000', '1.5', 'dashed', '#000000', '1.5', 'dashed', '#000000', '1.5',
          # PI
          '#111111', '#666666', '#666666'
        ),
        row.names = c(
          # Observed data
          'Symbol', 'Symbol color', 'Symbol scale',
          'Median line type', 'Median line color', 'Median line width',
          'CI upper limit line type', 'CI upper limit line color', 'CI upper limit line width',
          'CI lower limit line type', 'CI lower limit line color', 'CI lower limit line width',
          # Simulated data
          'PI area color',
          'Predicted median line type', 'Predicted median line color', 'Predicted median line width',
          'PI upper limit line type', 'PI upper limit line color', 'PI upper limit line width',
          'PI lower limit line type', 'PI lower limit line color', 'PI lower limit line width',
          # PI CI
          'CI around median - Area color', 'CI around PI upper limit - Area color', 'CI around PI lower limit - Area color'
        ),
        stringsAsFactors = FALSE
      )
    ),
    vpc.style = data.frame(
      style = 1:22,
      type = c('p', 'p', 'p', 'p', 'p', 'p', 'p', 'n', 'n', 'n', 'p', 'p', 'p',
               'p', 'p', 'p', 'p', 'n', 'n', 'n', 'p', 'n'),
      PI.real = c(NA, NA, NA, 'lines', 'lines', 'lines', 'lines', 'lines',
                  'lines', 'lines', NA, NA, NA, 'lines', 'lines', 'lines', 'lines',
                  'lines', 'lines', 'lines', 'lines', 'lines'),
      PI = c('lines', 'lines', 'area', 'lines', 'lines', 'area', NA, 'lines',
             'lines', 'area', 'lines', 'lines', 'area', 'lines', 'lines', 'area',
             NA, 'lines', 'lines', 'area', 'lines', NA),
      PI.ci = c(NA, 'area', NA, NA, 'area', NA, 'area', NA, 'area', NA, NA,
                'area', NA, NA, 'area', NA, 'area', NA, 'area', NA, 'area', 'area'),
      stringsAsFactors = FALSE
    ),
    vpc.tte.style = data.frame(
      style = 1:24,
      real.ci = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                  TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                  TRUE, TRUE, TRUE, TRUE),
      median.line = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
                      FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE,
                      FALSE, FALSE, TRUE, TRUE, TRUE),
      ci.area = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
                  FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
                  TRUE, FALSE, TRUE),
      ci.lines = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
                   FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
                   TRUE, FALSE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    spline = list(
      color = data.frame(
        smcol1 = c('#0066FF', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        smcol2 = c('#7F7F7F', '#FF7F7F', '#7F7FFF', '#7FBF7F', '#FFCF7F', '#FF7FCF', '#7FEEFF', '#E3B485',
                   '#C3C3C3', '#A7EFA7', '#C797C7'),
        smlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                  'dotdash'),
        smlwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        smcol1 = c('#B2B2B2', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        smcol2 = c('#7F7F7F', '#9A9A9A', '#898989', '#ADADAD', '#D3D3D3', '#A0A0A0', '#D8D8D8', '#BABABA',
                   '#C3C3C3', '#DEDEDE', '#A5A5A5'),
        smlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                  'dotdash'),
        smlwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      )
    ),
    hline = list(
      color = data.frame(
        hlinecol1 = rep('#FF0000', 10),
        hlinecol2 = rep('#000000', 10),
        hlinelty = rep('solid', 10),
        hlinelwd = rep(1, 10),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        hlinecol1 = rep('#363636', 10),
        hlinecol2 = rep('#000000', 10),
        hlinelty = rep('solid', 10),
        hlinelwd = rep(1, 10),
        stringsAsFactors = FALSE
      )
    ),
    vline = list(
      color = data.frame(
        vlinecol1 = rep('#FF0000', 10),
        vlinecol2 = rep('#000000', 10),
        vlinelty = rep('solid', 10),
        vlinelwd = rep(1, 10),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        vlinecol1 = rep('#363636', 10),
        vlinecol2 = rep('#000000', 10),
        vlinelty = rep('solid', 10),
        vlinelwd = rep(1, 10),
        stringsAsFactors = FALSE
      )
    ),
    abline = list(
      color = data.frame(
        value = c('#FF0000', '#000000', 'solid', '1'),
        row.names = c('Line color - no grouping', 'Line color - grouping', 'Line type', 'Line width'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#363636', '#000000', 'solid', '1'),
        row.names = c('Line color - no grouping', 'Line color - grouping', 'Line type', 'Line width'),
        stringsAsFactors = FALSE
      )
    ),
    error = list(
      color = data.frame(
        errpch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        errcol1 = c('#0066FF', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        errcol2 = c('#000000', '#FF0000', '#0000FF', '#008000', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                    '#888888', '#50E050', '#8F308F'),
        errcol3 = c('#7F7F7F', '#FF7F7F', '#7F7FFF', '#7FBF7F', '#FFCF7F', '#FF7FCF', '#7FEEFF', '#E3B485',
                    '#C3C3C3', '#A7EFA7', '#C797C7'),
        errfill = c('#000000', '#FF0000', '#0000FF', '#008000', '#FFA000', '#FF00A0', '#00DDFF', '#C76A0C',
                    '#888888', '#50E050', '#8F308F'),
        errcex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        errlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                   'dotdash'),
        errlwd = rep(1.5, 11),
        erralpha = rep(1, 11),
        erralpha.area = rep(0.25, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        errpch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        errcol1 = c('#B2B2B2', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        errcol2 = c('#000000', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                    '#888888', '#BEBEBE', '#4C4C4C'),
        errcol3 = c('#7F7F7F', '#9A9A9A', '#898989', '#ADADAD', '#D3D3D3', '#A0A0A0', '#D8D8D8', '#BABABA',
                    '#C3C3C3', '#DEDEDE', '#A5A5A5'),
        errfill = c('#000000', '#363636', '#141414', '#5B5B5B', '#A7A7A7', '#424242', '#B1B1B1', '#767676',
                    '#888888', '#BEBEBE', '#4C4C4C'),
        errcex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        errlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                   'dotdash'),
        errlwd = rep(1.5, 11),
        erralpha = rep(1, 11),
        erralpha.area = rep(0.25, 11),
        stringsAsFactors = FALSE
      )
    ),
    background = list(
      color = data.frame(
        value = c('#FFFFFF', '#DDDDDD', '#FFFFFF', '#000000'),
        row.names = c('Panel background color', 'Grid line color',
                      'Strip background color', 'Strip text color'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#FFFFFF', '#DDDDDD', '#FFFFFF', '#000000'),
        row.names = c('Panel background color', 'Grid line color',
                      'Strip background color', 'Strip text color'),
        stringsAsFactors = FALSE
      )
    )
  )

}

#' An alternative Cognigen ggplot2 style using a purple-hue ramp. It is
#' primarily intended to be used for order
#'
#' @inheritParams scale_discrete_cognigen
#' @param gray.first \code{logical} indicating whether the first group
#'   of the discrete scale should be represented using a gray color
#' @return \code{list} of style elements
#' @export
cognigen_purple_style <- function(n = 10, gray.first = FALSE){

  if ( !is.logical(gray.first) ){
    rlang::abort('gray.first argument must be logical')
  }

  n_max <- ifelse(all(gray.first), 9L, 10L)

  if ( !(is.numeric(n)) ){
    rlang::abort(
      glue::glue('n argument ({n}) must be an integer')
    )
  }

  if ( !(n >= 1 && n <= n_max) ){
    old_n <- n
    n <- min(n_max, max(1, n))
    rlang::warn(
      glue::glue('n argument ({old_n}) was coerced to {n}.')
    )
  }

  n <- as.integer(n)

  find_light_shade <- function(col){
    rgbdec <- grDevices::col2rgb(col)
    rgbdec <- 255 - 0.5*(255-rgbdec)
    apply(rgbdec, 2, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))
  }

  col2gray <- function(col) {
    rgbdec <- col2rgb(col)
    graydec <- apply(
      rgbdec,
      2,
      function(x){
        if ( all(x[1]==x) ){
          x[1]
        } else {
          0.21*x[1]+0.71*x[2]+0.08*x[3]
        }
      }
    )
    apply(graydec, 2, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))
  }

  if ( all(gray.first) ){
    new_col <- grDevices::colorRampPalette(c('#E79BFF', '#3F184A'))(9)
    new_light_col <- find_light_shade(new_col)
    new_col_grayscale <- grDevices::colorRampPalette(c('#B3B3B3', '#242424'))(9)
    new_light_col_grayscale <- find_light_shade(new_col_grayscale)
    new_col <- c('#5B5B5B', new_col)
    new_light_col <- c('#7F7F7F', new_light_col)
    new_fill <- new_col
    new_col_grayscale <- c('#5B5B5B', new_col_grayscale)
    new_light_col_grayscale <- c('#7F7F7F', new_light_col_grayscale)
    new_fill_grayscale <- new_col_grayscale
  } else {
    new_col <- grDevices::colorRampPalette(c('#E79BFF', '#3F184A'))(10)
    new_light_col <- find_light_shade(new_col)
    new_fill <- new_col
    new_col_grayscale <- grDevices::colorRampPalette(c('#B3B3B3', '#242424'))(10)
    new_light_col_grayscale <- find_light_shade(new_col_grayscale)
    new_fill_grayscale <- new_col_grayscale
  }

  utils::modifyList(
    cognigen_style(),
    list(
      scatter = list(
        color = list(
          col = c('#5B5B5B', new_col),
          fill = c('#5B5B5B', new_fill)
        ),
        grayscale = list(
          col = c('#5B5B5B', new_col_grayscale),
          fill = c('#5B5B5B', new_fill_grayscale)
        )
      ),
      ramp = list(
        color = list(col = new_col),
        grayscale = list(col = new_col_grayscale)
      ),
      bar = list(
        color = list( col = c('#FFFFFF', new_col) ),
        grayscale = list( col = c('#FFFFFF', new_col_grayscale) )
      ),
      box.sym = list(
        color = list(
          bwdotcol = c('#000000', new_col),
          bwdotfill = c('#000000', new_fill)
        ),
        grayscale = list(
          bwdotcol = c('#000000', new_col_grayscale),
          bwdotfill = c('#000000', new_fill_grayscale)
        )
      ),
      hist.dens = list(
        color = list(
          col = c('#000000', new_col),
          fill = c('#FFFFFF', new_fill)
        ),
        grayscale = list(
          col = c('#000000', new_col_grayscale),
          fill = c('#FFFFFF', new_fill_grayscale)
        )
      ),
      spline = list(
        color = list( smcol2 = c('#7F7F7F', new_light_col) ),
        grayscale = list( smcol2 = c('#7F7F7F', new_light_col_grayscale) )
      ),
      error = list(
        color = list(
          errcol2 = c('#000000', new_col),
          errcol3 = c('#7F7F7F', new_light_col),
          errfill = c('#000000', new_fill)
        ),
        grayscale = list(
          errcol2 = c('#000000', new_col_grayscale),
          errcol3 = c('#7F7F7F', new_light_col_grayscale),
          errfill = c('#000000', new_fill_grayscale)
        )
      )
    )
  )
}


# Alternative ggplot2 style
new_cognigen_style <- function(){

  list(
    scatter = list(
      color = data.frame(
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        col = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                '#F781BF', '#F38400', '#41B6C4'),
        fill = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                 '#F781BF', '#F38400', '#41B6C4'),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        lty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                'dotdash'),
        lwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        col = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        fill = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                 '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        lty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed',
                'F8', 'dotdash'),
        lwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      )
    ),
    ramp = list(
      color = data.frame(
        pch = c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        col = c('#9E0142', '#D53E4F', '#F46D43', '#FDAE61', '#FEE08B', '#E6F598', '#ABDDA4', '#66C2A5',
                '#3288BD', '#5E4FA2'),
        fill = c('transparent', NA, NA, NA, NA, NA, NA, NA, NA, NA),
        cex = c(0.5, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        pch = c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        col = c('#D8D8D8', '#C0C0C0', '#A8A8A8', '#909090', '#787878', '#606060', '#484848',
                '#303030', '#181818', '#000000'),
        fill = c('transparent', NA, NA, NA, NA, NA, NA, NA, NA, NA),
        cex = c(0.5, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE
      )
    ),
    bar = list(
      color = data.frame(
        col = c('#FFFFFF', '#F18C8D', '#9ABEDB', '#A5D7A4', '#FED78A', '#CBA6D1', '#C3C3C3', '#D2AA93',
                '#FBC0DF', '#FBC17F', '#9FDAE1'),
        border = rep('#000000', 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        col = c('#FFFFFF', '#454545',
                '#747474', '#929292', '#B3B3B3', '#646464', '#888888', '#636363', '#9F9F9F', '#919191', '#CFCFCF'),
        border = rep('#000000', 11),
        stringsAsFactors = FALSE
      )
    ),
    box.sym = list(
      color = data.frame(
        bwdotpch = c('|', '3', '0', '1', '2', '4', '5', '19', '15', '8', '17'),
        bwdotcol = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                     '#F781BF', '#F38400', '#41B6C4'),
        bwdotfill = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                      '#F781BF', '#F38400', '#41B6C4'),
        bwdotcex = c(0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        bwdotpch = c('|', '3', '0', '1', '2', '4',
                     '5', '19', '22', '8', '24'),
        bwdotcol = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                     '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        bwdotfill = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                      '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        bwdotcex = c(0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45),
        stringsAsFactors = FALSE
      )
    ),
    box.rec = list(
      color = data.frame(
        value = c('#000000', 'solid', '1', '#000000', 'transparent', 'solid', '1', '21', '#000000', '0.45'),
        row.names = c('Whisker color', 'Whisker line type', 'Whisker line width', 'Box border color',
                      'Box fill', 'Box border type', 'Box border width', 'Outlier symbol',
                      'Outlier color', 'Outlier scaling'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#000000', 'solid', '1', '#000000', 'transparent', 'solid', '1', '21', '#000000', '0.45'),
        row.names = c('Whisker color', 'Whisker line type', 'Whisker line width', 'Box border color',
                      'Box fill', 'Box border type', 'Box border width', 'Outlier symbol',
                      'Outlier color', 'Outlier scaling'),
        stringsAsFactors = FALSE
      )
    ),
    hist = list(
      color = data.frame(
        value = c('#FFFFFF', '#000000', 'solid', '1', '#FF0000', 'dashed', '1.5'),
        row.names = c('Histogram fill color', 'Histogram border color', 'Histogram border type',
                      'Histogram border width', 'Normal distribution color', 'Normal distribution type',
                      'Normal distribution width'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#FFFFFF', '#000000', 'solid', '1', '#363636', 'dashed', '1.5'),
        row.names = c('Histogram fill color', 'Histogram border color', 'Histogram border type',
                      'Histogram border width', 'Normal distribution color', 'Normal distribution type',
                      'Normal distribution width'),
        stringsAsFactors = FALSE
      )
    ),
    hist.dens = list(
      color = data.frame(
        col = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                      '#F781BF', '#F38400', '#41B6C4'),
        fill = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                 '#F781BF', '#F38400', '#41B6C4'),
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        hidcol = c('#0066FF', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        hidlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                   'dotdash'),
        hidlwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        col = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        fill = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                 '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        pch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        cex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        hidcol = c('#5D5D5D', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        hidlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed',
                   'F8', 'dotdash'),
        hidlwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      )
    ),
    vpc = list(
      color = data.frame(
        value = c(
          # Observed data
          '1', '#000000', '0.5',
          'solid', '#FF0000', '1.5', 'solid', '#FF0000', '1.5', 'solid', '#FF0000', '1.5',
          # Simulated data
          '#90ee90', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5',
          # PI
          '#0000FF', '#0000FF', '#0000FF'
        ),
        value2 = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#FF0000', '1.5', 'dashed', '#FF0000', '1.5', 'dashed', '#FF0000', '1.5',
          # Simulated data
          '#90ee90', 'solid', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5',
          # PI
          '#FF0000', '#0000FF', '#0000FF'
        ),
        value3 = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#FF0000', '1.5', 'solid', '#0000FF', '1.5', 'solid', '#0000FF', '1.5',
          # Simulated data
          '#90ee90', 'solid', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5', 'dashed', '#0000FF', '1.5',
          # PI
          '#FF0000', '#0000FF', '#0000FF'
        ),
        row.names = c(
          # Observed data
          'Symbol', 'Symbol color', 'Symbol scale',
          'Median line type', 'Median line color', 'Median line width',
          'CI upper limit line type', 'CI upper limit line color', 'CI upper limit line width',
          'CI lower limit line type', 'CI lower limit line color', 'CI lower limit line width',
          # Simulated data
          'PI area color',
          'Predicted median line type', 'Predicted median line color', 'Predicted median line width',
          'PI upper limit line type', 'PI upper limit line color', 'PI upper limit line width',
          'PI lower limit line type', 'PI lower limit line color', 'PI lower limit line width',
          # PI CI
          'CI around median - Area color', 'CI around PI upper limit - Area color', 'CI around PI lower limit - Area color'
        ),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#000000', '1.5', 'solid', '#000000', '1.5', 'solid', '#000000', '1.5',
          # Simulated data
          '#D1D1D1', 'dashed', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5',
          # PI
          '#5F5F5F', '#5F5F5F', '#5F5F5F'
        ),
        value2 = c(
          # Observed data
          '3', '#000000', '0.5',
          'dotted', '#000000', '3', 'F8', '#000000', '1.5', 'F8', '#000000', '1.5',
          # Simulated data
          '#D1D1D1', 'solid', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5', 'dashed', '#5F5F5F', '1.5',
          # PI
          '#5F5F5F', '#5F5F5F', '#5F5F5F'
        ),
        value3 = c(
          # Observed data
          '3', '#000000', '0.5',
          'solid', '#000000', '1.5', 'F8', '#000000', '1.5', 'F8', '#000000', '1.5',
          # Simulated data
          '#90ee90', 'solid', '#000000', '1.5', 'dashed', '#000000', '1.5', 'dashed', '#000000', '1.5',
          # PI
          '#111111', '#666666', '#666666'
        ),
        row.names = c(
          # Observed data
          'Symbol', 'Symbol color', 'Symbol scale',
          'Median line type', 'Median line color', 'Median line width',
          'CI upper limit line type', 'CI upper limit line color', 'CI upper limit line width',
          'CI lower limit line type', 'CI lower limit line color', 'CI lower limit line width',
          # Simulated data
          'PI area color',
          'Predicted median line type', 'Predicted median line color', 'Predicted median line width',
          'PI upper limit line type', 'PI upper limit line color', 'PI upper limit line width',
          'PI lower limit line type', 'PI lower limit line color', 'PI lower limit line width',
          # PI CI
          'CI around median - Area color', 'CI around PI upper limit - Area color', 'CI around PI lower limit - Area color'
        ),
        stringsAsFactors = FALSE
      )
    ),
    vpc.style = data.frame(
      style = 1:22,
      type = c('p', 'p', 'p', 'p', 'p', 'p', 'p', 'n', 'n', 'n', 'p', 'p', 'p',
               'p', 'p', 'p', 'p', 'n', 'n', 'n', 'p', 'n'),
      PI.real = c(NA, NA, NA, 'lines', 'lines', 'lines', 'lines', 'lines',
                  'lines', 'lines', NA, NA, NA, 'lines', 'lines', 'lines', 'lines',
                  'lines', 'lines', 'lines', 'lines', 'lines'),
      PI = c('lines', 'lines', 'area', 'lines', 'lines', 'area', NA, 'lines',
             'lines', 'area', 'lines', 'lines', 'area', 'lines', 'lines', 'area',
             NA, 'lines', 'lines', 'area', 'lines', NA),
      PI.ci = c(NA, 'area', NA, NA, 'area', NA, 'area', NA, 'area', NA, NA,
                'area', NA, NA, 'area', NA, 'area', NA, 'area', NA, 'area', 'area'),
      stringsAsFactors = FALSE
    ),
    vpc.tte.style = data.frame(
      style = 1:24,
      real.ci = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                  TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                  TRUE, TRUE, TRUE, TRUE),
      median.line = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
                      FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE,
                      FALSE, FALSE, TRUE, TRUE, TRUE),
      ci.area = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
                  FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
                  TRUE, FALSE, TRUE),
      ci.lines = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
                   FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
                   TRUE, FALSE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    spline = list(
      color = data.frame(
        smcol1 = c('#0066FF', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        smcol2 = c('#7F7F7F', '#F18C8D', '#9BBEDB', '#A6D7A4', '#FED78A', '#CBA6D1', '#C3C3C3', '#D2AA93',
                   '#FBC0DF', '#F9C17F', '#A0DAE1'),
        smlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313',
                  'solid', 'dashed', 'F8', 'dotdash'),
        smlwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        smcol1 = c('#B2B2B2', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        smcol2 = c('#7F7F7F', '#A2A2A2', '#B9B9B9', '#C8C8C8', '#D9D9D9', '#B1B1B1', '#C3C3C3',
                   '#B1B1B1', '#CFCFCF', '#C8C8C8', '#CFCFCF'),
        smlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313',
                  'solid', 'dashed', 'F8', 'dotdash'),
        smlwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        stringsAsFactors = FALSE
      )
    ),
    hline = list(
      color = data.frame(
        hlinecol1 = rep('#FF0000', 10),
        hlinecol2 = rep('#000000', 10),
        hlinelty = rep('solid', 10),
        hlinelwd = rep(1L, 10),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        hlinecol1 = rep('#363636', 10),
        hlinecol2 = rep('#000000', 10),
        hlinelty = rep('solid', 10),
        hlinelwd = rep(1L, 10),
        stringsAsFactors = FALSE
      )
    ),
    vline = list(
      color = data.frame(
        vlinecol1 = rep('#FF0000', 10),
        vlinecol2 = rep('#000000', 10),
        vlinelty = rep('solid', 10),
        vlinelwd = rep(1L, 10),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        vlinecol1 = rep('#363636', 10),
        vlinecol2 = rep('#000000', 10),
        vlinelty = rep('solid', 10),
        vlinelwd = rep(1L, 10),
        stringsAsFactors = FALSE
      )
    ),
    abline = list(
      color = data.frame(
        value = c('#FF0000', '#000000', 'solid', '1'),
        row.names = c('Line color - no grouping', 'Line color - grouping', 'Line type', 'Line width'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#363636', '#000000', 'solid', '1'),
        row.names = c('Line color - no grouping', 'Line color - grouping', 'Line type', 'Line width'),
        stringsAsFactors = FALSE
      )
    ),
    error = list(
      color = data.frame(
        errpch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        errcol1 = c('#0066FF', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        errcol2 = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                    '#F781BF', '#F38400', '#41B6C4'),
        errcol3 = c('#7F7F7F', '#F18C8D', '#9BBEDB', '#A6D7A4', '#FED78A', '#CBA6D1', '#C3C3C3', '#D2AA93',
                    '#FBC0DF', '#F9C17F', '#A0DAE1'),
        errfill = c('#000000', '#E41A1C', '#377EB8', '#4DAF4A', '#FEAF16', '#984EA3', '#888888', '#A65628',
                    '#F781BF', '#F38400', '#41B6C4'),
        errcex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        errlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed', 'F8',
                   'dotdash'),
        errlwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        erralpha = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
        erralpha.area = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        errpch = c(1L, 3L, 0L, 1L, 2L, 4L, 5L, 19L, 15L, 8L, 17L),
        errcol1 = c('#B2B2B2', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        errcol2 = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                    '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        errcol3 = c('#7F7F7F', '#A2A2A2', '#B9B9B9', '#C8C8C8', '#D9D9D9', '#B1B1B1', '#C3C3C3',
                    '#B1B1B1', '#CFCFCF', '#C8C8C8', '#CFCFCF'),
        errfill = c('#000000', '#454545', '#747474', '#929292', '#B3B3B3', '#646464', '#888888',
                    '#636363', '#9F9F9F', '#919191', '#9F9F9F'),
        errcex = c(0.45, 0.5, 0.5, 0.5, 0.45, 0.5, 0.45, 0.5, 0.5, 0.45, 0.5),
        errlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 'solid', 'dashed',
                   'F8', 'dotdash'),
        errlwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
        erralpha = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
        erralpha.area = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
        stringsAsFactors = FALSE
      )
    ),
    background = list(
      color = data.frame(
        value = c('#FFFFFF', '#DDDDDD', '#FFFFFF', '#000000'),
        row.names = c('Panel background color', 'Grid line color',
                      'Strip background color', 'Strip text color'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#FFFFFF', '#DDDDDD', '#FFFFFF', '#000000'),
        row.names = c('Panel background color', 'Grid line color',
                      'Strip background color', 'Strip text color'),
        stringsAsFactors = FALSE
      )
    )
  )
}

#' Set default ggplot2 style when no aesthetics are used. Run by default at startup of
#' `ggcognigen`
#'
#' @param style \code{list} of style elements, liked one return by \code{\link{cognigen_style}}
#' or \code{\link{read_style}}. Alternatively, \code{style} can be set to \code{'ggplot2'} to revert to the
#' defaults of the ggplot2 package.
#'
#' @return updates elements of default ggplot2 style then invisibly returns \code{NULL}
#' @export
#'
#' @examples
#' \dontrun{
#' set_default_style()
#'
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_point()
#'
#' set_default_style(style = 'ggplot2')
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_point()
#'
#' }
set_default_style <- function(style = cognigen_style()){

  default <- cognigen_style()

  if ( is.character(style) && all(style == 'ggplot2') ){
    style <- list(
      scatter = list(
        color = list(
          pch = 19,
          col = 'black',
          fill = NA,
          lty = 1,
          cex = 0.5
        )
      ),
      line = list(
        col = 'black',
        lty = 1,
        lwd = 1
      ),
      bar = list(
        border = NA,
        col = 'grey35'
      )
    )
    bar_size <- 0.5
  } else {
    if ( !identical(get.structure(style), get.structure(default)) ){
      message('Invalid settings style. Reverting to default style.')
      style <- default
    }
    bar_size <- 0.25
  }

  ggplot2::update_geom_defaults(
    'point',
    list(
      shape = style$scatter$color$pch[1],
      colour = style$scatter$color$col[1],
      fill = style$scatter$color$fill[1],
      lty = style$scatter$color$lty[1],
      size = 3*style$scatter$color$cex[1]
    )
  )

  ggplot2::update_geom_defaults(
    'line',
    list(
      colour = style$scatter$color$col[1],
      lty = style$scatter$color$lty[1],
      size = 0.5*style$scatter$color$lwd[1]
    )
  )

  ggplot2::update_geom_defaults(
    'bar',
    list(
      colour = style$bar$color$border[1],
      fill = style$bar$color$col[1],
      size = bar_size
    )
  )

  ggplot2::update_geom_defaults(
    'col',
    list(
      colour = style$bar$color$border[1],
      fill = style$bar$color$col[1]
    )
  )

  return(invisible(NULL))
}

#' Read graphical style file created with the theme_maker Shiny app
#'
#' @param path Relative or absolute path to the graphical style file
#'
#' @return \code{list} of style elements
#' @export
read_style <- function(path){

  if ( !file.exists(path) ){
    stop('Invalid file path')
  }

  json <- try(
    jsonlite::read_json(path, simplifyVector = TRUE, flatten = TRUE),
    silent = TRUE
  )

  # Check json

  invalid <- FALSE
  default <- cognigen_style()

  if ( class(json) == 'try-error') {
    invalid <- TRUE
  }

  if ( !invalid ){
    invalid <- !identical(get.structure(json), get.structure(default))
  }

  if ( invalid ){
    message('Invalid settings style file. Reverting to default style.')
    json <- default
  }

  return(json)

}
