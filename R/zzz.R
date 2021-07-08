# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' @importFrom utils packageVersion
.onLoad <- function(libname, pkgname) {

  packageStartupMessage(
    sprintf(
      'Loading ggcognigen Version %s',
      packageVersion('ggcognigen')
    )
  )
  license <- system.file('LICENSE', package='ggcognigen')
  if (length(options()$show_license) > 0){
    show <- options()$show_license
  } else {
    show <- TRUE
  }
  if (length(license) > 0 & license != '' & show){
    packageStartupMessage(
      sprintf(
        paste(
          '********************************************************************************',
          '',
          'The use, copy, and re-distribution of the ggcognigen package are subject to the',
          'terms specified in the license provided in:',
          '  %s',
          'Your use of the ggcognigen package is deemed acceptance of such license terms.',
          'If you do not agree to such license terms, do not load or use the ggcognigen',
          'package.',
          '',
          'Copyright 2020-%s Cognigen Corporation. All rights reserved.',
          '',
          '********************************************************************************',
          sep = '\n'
        ),
        license,
        format(Sys.Date(), '%Y')
      )
    )
  }

  # Apply default Cognigen styling and theme for ggplot2
  set_default_style()

  # Apply default Cognigen theme
  ggplot2::theme_set(theme_cognigen())

  packageStartupMessage(
    '\nDefault style set to `cognigen_style()`\n',
    'Default theme set to `theme_cognigen()`'
  )

}

