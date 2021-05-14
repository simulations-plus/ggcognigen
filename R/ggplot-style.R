

# get.structure
# utility function for style checks
get.structure <- function(x){
  if (!(is.data.frame(x))){
    lapply(x, get.structure)
  } else {
    sort(names(x))
  }
}

# cognigen_style
# default graphical style

cognigen_style <- function(){
  
  list(
    scatter = list(
      color = data.frame(
        pch = as.integer(c(21, 3, 2, 4, 22, 25, 21, 23, 22, 8, 24)),
        col = c('#000000', '#FF0000', '#0000FF', '#008000', '#FF00FF', '#FFA000', 
                '#00DDFF', '#A05000', '#80AD88', '#3883A8', '#6F306F'),
        fill = c('transparent', '#FF0000', 'transparent', 'transparent', 
                 'transparent', '#FFA000', '#00DDFF', 'transparent', '#80AD88', 
                 'transparent', '#6F306F'),
        cex = c(0.45, 0.5, 0.45, 0.5, 0.55, 0.45, 0.5, 0.45, 0.5, 0.5, 0.5),
        lty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                'solid', 'dashed', 'F8', 'dotdash'),
        lwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        pch = as.integer(c(21, 3, 2, 4, 22, 25, 21, 23, 22, 8, 24)),
        col = c('#000000', '#363636', '#141414', '#5B5B5B', '#4A4A4A', '#A7A7A7', 
                '#B1B1B1', '#5A5A5A', '#A1A1A1', '#767676', '#424242'),
        fill = c('transparent', '#363636', 'transparent', 'transparent', 
                 'transparent', '#A7A7A7', '#B1B1B1', 'transparent', '#A1A1A1', 
                 'transparent', '#424242'),
        cex = c(0.45, 0.5, 0.45, 0.5, 0.55, 0.45, 0.5, 0.45, 0.5, 0.5, 0.5),
        lty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                'solid', 'dashed', 'F8', 'dotdash'),
        lwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      )
    ),
    ramp = list(
      color = data.frame(
        pch = c(16, rep(NA, 9)),
        col = c('#9E0142', '#D53E4F', '#F46D43', '#FDAE61', '#FEE08B', 
                '#E6F598', '#ABDDA4', '#66C2A5', '#3288BD', '#5E4FA2'),
        fill = c('transparent', rep(NA, 9)),
        cex = c(0.5, rep(NA, 9)),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        pch = c(16, rep(NA, 9)),
        col = c('#D8D8D8', '#C0C0C0', '#A8A8A8', '#909090', '#787878', 
                '#606060', '#484848', '#303030', '#181818', '#000000'),
        fill = c('transparent', rep(NA, 9)),
        cex = c(0.5, rep(NA, 9)),
        stringsAsFactors = FALSE
      )
    ),
    bar = list(
      color = data.frame(
        col = c('#FFFFFF', '#FF8080', '#8080FF', '#66DD66', '#FF80FF', '#FFCF80',
                '#80EFFF', '#FFA851', '#C5D7C4', '#95C5DB', '#CA86CA'),
        border = rep('#000000', 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        col = c('#FFFFFF', '#9B9B9B', '#8A8A8A', '#BABABA', '#A5A5A5', '#D3D3D3', 
                '#D9D9D9', '#B3B3B3', '#D2D2D2', '#BDBDBD', '#9A9A9A'),
        border = rep('#000000', 11),
        stringsAsFactors = FALSE
      )
    ),
    box.sym = list(
      color = data.frame(
        bwdotpch = c('|', 3, 2, 4, 22, 25, 21, 23, 22, 8, 24),
        bwdotcol = c('#000000', '#FF0000', '#0000FF', '#008000', '#FF00FF', '#FFA000', 
                     '#00DDFF', '#A05000', '#80AD88', '#3883A8', '#6F306F'),
        bwdotfill = c('transparent', '#FF0000', 'transparent', 'transparent', 
                      'transparent', '#FFA000', '#00DDFF', 'transparent', '#80AD88', 
                      'transparent', '#6F306F'),
        bwdotcex = rep(0.45, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        bwdotpch = c('|', 3, 2, 4, 22, 25, 21, 23, 22, 8, 24),
        bwdotcol = c('#000000', '#363636', '#141414', '#5B5B5B', '#4A4A4A', '#A7A7A7', 
                     '#B1B1B1', '#5A5A5A', '#A1A1A1', '#767676', '#424242'),
        bwdotfill = c('transparent', '#363636', 'transparent', 'transparent', 
                      'transparent', '#A7A7A7', '#B1B1B1', 'transparent', '#A1A1A1', 
                      'transparent', '#424242'),
        bwdotcex = rep(0.45, 11),
        stringsAsFactors = FALSE
      )
    ),
    box.rec = list(
      color = data.frame(
        value = c('#000000', 'solid', '1', '#000000', 'transparent', 'solid', 1, 21,
                  '#000000', 0.45),
        row.names = c('Whisker color',
                      'Whisker line type', 'Whisker line width', 'Box border color', 'Box fill',
                      'Box border type', 'Box border width', 'Outlier symbol', 'Outlier color',
                      'Outlier scaling'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#000000', 'solid', '1', '#000000', 'transparent', 'solid', 1, 21,
                  '#000000', 0.45),
        row.names = c('Whisker color',
                      'Whisker line type', 'Whisker line width', 'Box border color', 'Box fill',
                      'Box border type', 'Box border width', 'Outlier symbol', 'Outlier color',
                      'Outlier scaling'),
        stringsAsFactors = FALSE
      )
    ),
    hist = list(
      color = data.frame(
        value = c('#FFFFFF', '#000000', 'solid', '1', '#FF0000', 'dashed', 1.5),
        row.names = c('Histogram fill color', 'Histogram border color', ' Histogram border type', 
                      'Histogram border width', 'Normal distribution color', 'Normal distribution type', 
                      'Normal distribution width'),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        value = c('#FFFFFF', '#000000', 'solid', '1', '#FF0000', 'dashed', 1.5),
        row.names = c('Histogram fill color', 'Histogram border color', ' Histogram border type', 
                      'Histogram border width', 'Normal distribution color', 'Normal distribution type', 
                      'Normal distribution width'),
        stringsAsFactors = FALSE
      )
    ),
    hist.dens = list(
      color = data.frame(
        col = c('#000000', '#FF0000', '#0000FF', '#008000', '#FF00FF', '#FFA000', 
                '#00DDFF', '#A05000', '#80AD88', '#3883A8', '#6F306F'
        ),
        fill = c('transparent', '#FF0000', 'transparent', 'transparent', 
                 'transparent', '#FFA000', '#00DDFF', 'transparent', '#80AD88', 
                 'transparent', '#6F306F'),
        pch = as.integer(c(21, 3, 2, 4, 22, 25, 21, 23, 22, 8, 24)),
        cex = c(0.45, 0.5, 0.45, 0.5, 0.55, 0.45, 0.5, 0.45, 0.5, 0.5, 0.5),
        hidcol = c('#0066FF', rep(NA, 10)),
        hidlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                   'solid', 'dashed', 'F8', 'dotdash'),
        hidlwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        col = c('#000000', '#FF0000', '#0000FF', '#008000', '#FF00FF', '#FFA000', 
                '#00DDFF', '#A05000', '#80AD88', '#3883A8', '#6F306F'
        ),
        fill = c('transparent', '#363636', 'transparent', 'transparent', 
                 'transparent', '#A7A7A7', '#B1B1B1', 'transparent', '#A1A1A1', 
                 'transparent', '#424242'),
        pch = as.integer(c(21, 3, 2, 4, 22, 25, 21, 23, 22, 8, 24)),
        cex = c(0.45, 0.5, 0.45, 0.5, 0.55, 0.45, 0.5, 0.45, 0.5, 0.5, 0.5),
        hidcol = c('#0066FF', rep(NA, 10)),
        hidlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                   'solid', 'dashed', 'F8', 'dotdash'),
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
      type = c('p', 'p', 'p', 'p', 'p',  'p', 'p', 'n', 'n', 'n', 'p', 'p', 'p', 
               'p', 'p', 'p', 'p', 'n',  'n', 'n', 'p', 'n'),
      PI.real = c(NA, NA, NA, 'lines', 'lines', 'lines',  'lines', 'lines', 
                  'lines', 'lines', NA, NA, NA, 'lines', 'lines',  'lines', 'lines', 
                  'lines', 'lines', 'lines', 'lines', 'lines'),
      PI = c('lines', 'lines', 'area', 'lines',  'lines', 'area', NA, 'lines', 
             'lines', 'area', 'lines', 'lines',  'area', 'lines', 'lines', 'area', 
             NA, 'lines', 'lines', 'area',  'lines', NA),
      PI.ci = c(NA, 'area', NA, NA, 'area', NA, 'area', NA,  'area', NA, NA, 
                'area', NA, NA, 'area', NA, 'area', NA, 'area',  NA, 'area', 'area'),
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
        smcol1 = c('#0066FF', rep(NA, 10)),
        smcol2 = c('#7F7F7F', '#FF8080', '#9999FF', '#00CC00', '#FF99FF', 
                   '#FFD999', '#99F1FF', '#DF7000', '#CCDECF', '#69ACCD', '#A045A0'),
        smlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                  'solid', 'dashed', 'F8', 'dotdash'),
        smlwd = rep(1.5, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        smcol1 = c('#B2B2B2', rep(NA, 10)),
        smcol2 = c('#7F7F7F', '#9B9B9B', '#A1A1A1', '#919191', '#B7B7B7', 
                   '#DCDCDC', '#E0E0E0', '#7E7E7E', '#D9D9D9', '#A1A1A1', '#5F5F5F'),
        smlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                  'solid', 'dashed', 'F8', 'dotdash'),
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
        errpch = as.integer(c(21, 3, 2, 4, 22, 25, 21, 23, 22, 8, 24)),
        errcol1 = c('#0066FF', rep(NA, 10)),
        errcol2 = c('#000000', '#FF0000', '#0000FF', '#008000', '#FF00FF', 
                    '#FFA000', '#00DDFF', '#A05000', '#80AD88', '#3883A8', '#6F306F'),
        errcol3 = c('#7F7F7F', '#FF8080', '#9999FF', '#00CC00', '#FF99FF', 
                    '#FFD999', '#99F1FF', '#DF7000', '#CCDECF', '#69ACCD', '#A045A0'),
        errfill = c('transparent', '#FF0000', 'transparent', 'transparent', 
                    'transparent', '#FFA000', '#00DDFF', 'transparent', '#80AD88', 
                    'transparent', '#6F306F'),
        errcex = c(0.45, 0.5, 0.45, 0.5, 0.55, 0.45, 0.5, 0.45, 0.5, 0.5, 0.5),
        errlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                   'solid', 'dashed', 'F8', 'dotdash'),
        errlwd = rep(1.5, 11),
        erralpha = rep(1, 11),
        erralpha.area = rep(0.25, 11),
        stringsAsFactors = FALSE
      ),
      grayscale = data.frame(
        errpch = as.integer(c(21, 3, 2, 4, 22, 25, 21, 23, 22, 8, 24)),
        errcol1 = c('#B2B2B2', rep(NA, 10)),
        errcol2 = c('#000000', '#363636', '#141414', '#5B5B5B', '#4A4A4A', 
                    '#A7A7A7', '#B1B1B1', '#5A5A5A', '#A1A1A1', '#767676', '#424242'),
        errcol3 = c('#7F7F7F', '#9B9B9B', '#A1A1A1', '#919191', '#B7B7B7', 
                    '#DCDCDC', '#E0E0E0', '#7E7E7E', '#D9D9D9', '#A1A1A1', '#5F5F5F'),
        errfill = c('transparent', '#363636', 'transparent', 'transparent', 
                    'transparent', '#A7A7A7', '#B1B1B1', 'transparent', '#A1A1A1', 
                    'transparent', '#424242'),
        errcex = c(0.45, 0.5, 0.45, 0.5, 0.55, 0.45, 0.5, 0.45, 0.5, 0.5, 0.5),
        errlty = c('solid', 'solid', 'dashed', 'F8', 'dotdash', '22848222', 'F313', 
                   'solid', 'dashed', 'F8', 'dotdash'),
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

# set_default_style
# set default ggplot2 style when no aesthetics are used
set_default_style <- function(style = cognigen_style()){
  
  default <- cognigen_style()
  
  if ( !identical(get.structure(style), get.structure(default)) ){
    message('Invalid settings style. Reverting to default style.')
    style <- default
  }
  
  update_geom_defaults(
    'point', 
    list(
      shape = style$scatter$color$pch[1], 
      colour = style$scatter$color$col[1],
      fill = style$scatter$color$fill[1],
      lty = style$scatter$color$lty[1],
      size = 3*style$scatter$color$cex[1]
    )
  )
  
  update_geom_defaults(
    'line', 
    list(
      colour = style$scatter$color$col[1],
      lty = style$scatter$color$lty[1],
      size = 0.5*style$scatter$color$lwd[1]
    )
  )
  
  update_geom_defaults(
    'bar', 
    list(
      colour = style$bar$color$border[1],
      fill = style$bar$color$col[1]
    )
  )
  
  update_geom_defaults(
    'col', 
    list(
      colour = style$bar$color$border[1],
      fill = style$bar$color$col[1]
    )
  )
}

# read_style_theme
# Read graphical style file; file must be a file created with the theme_maker Shiny app

read_style_theme <- function(file){
  
  if ( !file.exists(file) ){
    stop('Invalid file path')
  }
  
  json <- try(
    jsonlite::read_json(input$openInput$datapath, simplifyVector = TRUE, flatten = TRUE),
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