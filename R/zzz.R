# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

.onLoad <- function(libname, pkgname) {

  packageStartupMessage('Loading ggcognigen Version $version$')
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

}

