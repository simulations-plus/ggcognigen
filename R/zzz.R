#' @importFrom utils packageVersion
.onLoad <- function(libname, pkgname) {

  packageStartupMessage(
    sprintf(
      'Loading ggcognigen Version %s',
      packageVersion('ggcognigen')
    )
  )

  # Apply default styling and theme for ggplot2
  set_default_style()

  # Apply default theme
  ggplot2::theme_set(theme_cognigen())

  packageStartupMessage(
    '\nDefault style set to `cognigen_style()`\n',
    'Default theme set to `theme_cognigen()`'
  )

}

