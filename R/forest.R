# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' Calculate and report geometric means (GM) and geometric mean ratios (GMR)
#'
#' @description
#'
#' This function is intended to calculate and output geometric means (GM) and
#' geometric mean ratios (GMR) into a data.frame suitable for plotting using
#' the \code{make_forestplot} function.
#'
#' @param data a data.frame of data provided in wide-form and expected to contain
#' one record per individual
#' @param x_var the variable(s) in \code{data} for which the GM and GMRs must be
#' calculated
#' @param id_var the variable in \code{data} differentiating individual data
#' @param by a stratification variable in \code{data}
#' @param covariates a vector of variables in \code{data}. They are expected to
#' be categorical variables (either logical, character, or factor) and define
#' the group for which the GM and GMRs of \code{x_var} are calculated
#' @param labels a vector of character or expression which defines the labels for
#' the variables set in \code{covariates}. Expected to have the same length as
#' \code{covariates}
#' @param ref_levels a vector of integer defining the reference level to be used
#' in the calculation of GMRs for the variables set in \code{covariates}. Expected
#' to have the same length as \code{covariates}
#' @param paired a logical value indicating if the results to be compared in the
#' calculation of GMRs are paired (TRUE) or not (FALSE, default)
#' @param ci a numerical value between 0 and 1 defining the width of the
#' confidence interval around the calculated GMRs
#' @param sep the character that separates counts from GMR + confidence interval
#' in the \code{gmr_n_label} output variable
#' @param digits the number of significant digits to apply when creating result
#' labels (see Results)
#' @param silent a logicial value indicating whether information messages should
#' be shown (FALSE) or hidden (TRUE)
#'
#' @export
#'
#' @return This function returns a data.frame with one row per variable in
#' \code{covariates} and level in these variables. The variables in this
#' data.frame are: \describe{
#' \item{x_var}{one of the variables defined in \code{x_var},}
#' \item{y_var}{one of the variables defined in \code{covariates},}
#' \item{y_label}{the label associated with each variables defined in
#' \code{covariates}; one of the label defined in \code{labels}, or, if this
#' argument was not defined, either the name of the variables and its 'label'
#' attributes in \code{data},}
#' \item{by}{one of the level of the \code{by} variable,}
#' \item{value}{one of the level of the \code{covariates} variables}
#' \item{n}{the number of individuals included in this particular level of
#' \code{y_var} and \code{by}; records with missing value of \code{x_var} are
#' excluded}
#' \item{reference}{a logical variable defining whether this particular
#' level of \code{y_var} is the reference for the calculation of GMRs}
#' \item{gm}{the GM value for this particular level of \code{y_var} and \code{by}}
#' \item{gm_lo}{the lower limit of the GM confidence interval}
#' \item{gm_hi}{the upper limit of the GM confidence interval}
#' \item{gm_label}{the GM label; structured as X1 [X2, X3], where X1
#' is \code{gm}, X2 is \code{gm_lo}, X3 is \code{gm_hi}, and
#' X1, X2, and X3 are formatted to \code{digits} significant digits}
#' \item{gmr}{the GMR value for this particular level of \code{y_var} and \code{by}
#' with respect to the reference level}
#' \item{gmr_lo}{the lower limit of the GMR confidence interval}
#' \item{gmr_hi}{the upper limit of the GMR confidence interval}
#' \item{gmr_label}{the GMR label; structured as X1 [X2, X3], where X1
#' is \code{gmr}, X2 is \code{gmr_lo}, X3 is \code{gmr_hi}, and
#' X1, X2, and X3 are formatted to \code{digits} significant digits}
#' \item{gmr_n_label}{the GMR label with n; structured as X1 [X2, X3] n = X4, where X1
#' is \code{gmr}, X2 is \code{gmr_lo}, X3 is \code{gmr_hi}, X4 is \code{n}, and
#' X1, X2, and X3 are formatted to \code{digits} significant digits}
#' }
#'
#' \code{y_var}, \code{y_label}, and \code{value} are coerced to factors to
#' facilitate plotting using \code{make_forestplot}.
#'
#' @examples
#' \dontrun{
#' # Use expo dataset provided in the ggcognigen package
#'
#' make_gmr_data(
#'   x_var = c('CMAXSS', 'AUCSS', 'CMINSS'),
#'   data = expo,
#'   id_var = 'ID',
#'   by = NULL,
#'   covariates = c('AGE', 'WTKG', 'BMI', 'SEXF', 'RFCAT', 'CPCAT'),
#'   labels = c(
#'     expression('Age'~'(y)'),
#'     expression('Body'~'Weight'~'(kg)'),
#'     expression('Body'~'Mass'~'Index'~'(kg/'*m^2*')'),
#'     expression('Sex'),
#'     expression('Renal'~'Function'),
#'     expression('Hepatic'~'Function')
#'   ),
#'   ref_levels = c(2, 3, 2, 1, 1, 1),
#'   digits = 3,
#'   silent = TRUE
#' )
#' }

make_gmr_data <- function(
  data,
  x_var,
  id_var,
  by = NULL,
  covariates,
  labels,
  ref_levels,
  paired = FALSE,
  ci = 0.9,
  sep = '\t',
  digits = 3L,
  silent = FALSE
){

  if ( missing(x_var) ){
    stop('x_var argument is missing')
  }
  if ( !is.character(x_var) ){
    stop('x_var is not a character value')
  }
  if ( length(x_var) > 1){
    tmp <- lapply(
      x_var,
      function(x, ...){
        make_gmr_data(x, ...)
      },
      data = data,
      id_var = id_var,
      by = by,
      covariates = covariates,
      labels = labels,
      ref_levels = ref_levels,
      paired = paired,
      ci = ci,
      digits = digits,
      silent = silent
    )

    res <- do.call( 'rbind', tmp )
    attr(res$y_label, 'expression') <- attr(tmp[[1]]$y_label, 'expression')

    return(res)

  }

  # Functions
  gm <- function(x){
    suppressWarnings(exp(mean(log(x))))
  }
  gm_ci <- function(x, ci = 0.9){
    suppressWarnings(
      exp(
        stats::quantile(
          log(x),
          probs = c(0.5*(1-ci), 1 - 0.5*(1-ci))
        )
      )
    )
  }
  gmr_ci <- function(test, ref, paired = FALSE, ci = 0.9){
    exp(
      stats::t.test(
        y = log(ref),
        x = log(test),
        paired = paired,
        mu = 0,
        conf.level = ci
      )$conf.int
    )
  }

  # Validate arguments
  if ( missing(data) ){
    stop('data argument is missing')
  }

  if ( !is.data.frame(data) ){
    stop('data argument must be a data.frame')
  }

  if ( missing (id_var) ){
    stop('id_var argument is missing')
  }
  if ( !is.character(id_var) ){
    stop('id_var is not a character value')
  }
  if ( length(id_var) > 1 ){
    warning('Only the first element of id_var will be used')
  }
  id_var <- id_var[1]

  if ( length(by) >0 ){
    if ( !(is.character(by)) ){
      stop('by is not a character value')
    }

    if ( length(by) > 1 ){
      warning('Only the first element of by will be used')
    }
  }

  if ( missing(covariates) ){
    stop('covariates argument is missing')
  }
  if ( !is.character(covariates) ){
    stop('covariates is not a character vector')
  }

  if ( missing(labels)){
    # Use label attributes if present, else use the variable names
    labels <- sapply(
      covariates,
      function(x, data) {
        label <- attr(data[, x], 'label')
        if (length(label) == 0){
          x
        } else {
          label
        }
      },
      data
    )
  } else {
    if ( !(is.character(labels) | is.expression(labels) ) ){
      stop('labels is not a character or expression vector')
    }
  }
  label_expressions <- is.expression(labels)
  labels <- rep(labels, length.out = length(covariates))

  if ( missing(ref_levels) ){
    ref_levels <- rep(1, length(covariates))
  } else {
    if ( !is.numeric(ref_levels) ){
      stop('ref_levels is not a numeric vector')
    } else {
      ref_levels <- rep(ref_levels, length.out = length(covariates))
    }
    if ( any(is.na(ref_levels)) ){
      ref_levels[is.na(ref_levels)] <- 1
    }
  }

  if ( !is.logical(paired) ){
    stop('paired is not a logical value')
  }
  paired <- paired[1]

  if ( !is.numeric(ci) ){
    stop('ci is not a numeric value')
  }
  ci <- ci[1]
  if ( ci < 0 | ci > 1 ){
    stop('ci must be between 0 and 1')
  }


  if ( !is.character(sep) ){
    stop('sep is not a character value')
  }
  sep <- sep[1]

  if ( !is.numeric(digits) ){
    stop('digits is not a numeric value')
  }
  digits <- max(c(1, as.integer(digits)[1]), na.rm = TRUE)

  if ( !is.logical(silent) ){
    stop('silent is not a logical value')
  }
  silent <- silent[1]

  # Report dimension of data
  nrows <- nrow(data)
  ncols <- ncol(data)
  if (nrows == 0 | ncols == 0){
    stop('Data file is empty')
  } else {
    if ( !silent ){
      message( glue::glue('Data file contains {nrows} rows and {ncols} columns') )
    }
  }

  # Check existence of variables
  if ( !id_var %in% names(data) ){
    stop( glue::glue('{id_var} not present in data file') )
  }
  if ( !x_var %in% names(data) ){
    stop( glue::glue('{x_var} not present in data file') )
  }
  if ( !all(covariates %in% names(data)) ){
    invalid <- covariates[
      which( ! covariates %in% names(data) )
    ]
    stop( glue::glue('{invalid} not present in data file') )
  }

  # Check that all covariates are categorical
  is_categorical <- sapply(
    covariates,
    function(x, data){
      all(class(data[, x]) == 'character') |
        all(class(data[, x]) == 'logical') |
        inherits(data[, x], 'factor')
    },
    data
  )
  if ( !all(is_categorical) ){
    invalid <- paste(
      covariates[!is_categorical],
      collapse = ', '
    )
    stop( glue::glue('Non-categorical variables: {invalid}') )
  }

  # Set default by variables
  oby <- by
  if ( length(by) == 0 ){
    by <- '__default__'
    data[[by]] <- 1
  }

  # Filter out records with missing id_var and x_var
  invalid_rows <- which( is.na(data[, id_var]) | is.na(data[, x_var]) )

  if ( length(invalid_rows) > 0 ) {
    data <- data[ !invalid_rows, ]
    nrows <- nrow(data)
    if ( !silent ){
      message(
        glue::glue('... {nrows} rows left after subset to rows with missing {id_var} and {x_var} values')
      )
    }
  }

  # Filter out records with missing by
  invalid_rows <- which( is.na(data[, by]) )

  if ( length(invalid_rows) > 0 ) {
    data <- data[ !invalid_rows, ]
    nrows <- nrow(data)
    if ( !silent ){
      message(
        glue::glue('... {nrows} rows left after subset to rows with missing {oby} values')
      )
    }
  }

  # Filter to first record per ID if required
  data <- data[!duplicated(data[, c(id_var, by)]), ]

  if (nrows != nrow(data)){
    nrows <- nrow(data)
    if ( !silent ){
      if ( length(oby) == 0 ){
        message(
          glue::glue(
            '... {nrows} rows left after subset to first record per individual'
          )
        )
      } else {
        message(
          glue::glue(
            '... {nrows} rows left after subset to first record per individual and {oby}'
          )
        )
      }
    }
  }

  # Filter to records with positive x_var values
  invalid_rows <- which( data[, x_var] <= 0 )

  if ( length(invalid_rows) > 0 ) {
    data <- data[ !invalid_rows, ]
    nrows <- nrow(data)
    if ( !silent ){
      message(
        glue::glue('... {nrows} rows left after subset to rows with positive {x_var} values')
      )
    }
  }

  # Setup results data.frame
  res <- data.frame(
    x_var = character(),
    y_var = character(),
    y_label = character(),
    value = character(),
    n = integer(),
    reference = logical(),
    gm = numeric(),
    gm_lo = numeric(),
    gm_hi = numeric(),
    gm_label = character(),
    gmr = numeric(),
    gmr_lo = numeric(),
    gmr_hi = numeric(),
    gmr_label = character()
  )
  if ( length(oby) > 0 ){
    res$by <- character()
  }

  # Get by levels
  tmp_by <- data[, by]
  if ( !inherits(tmp_by, 'factor') ){
    tmp_by <- factor(tmp_by)
  }
  by_levels <- levels(tmp_by)
  tmp_by <- as.integer(tmp_by)

  # Calculate GMR and CI
  if ( !silent ){
    message( glue::glue('\n\nProcessing {x_var}'))
  }
  for ( ivar in seq_along(covariates) ){

    var <- covariates[ivar]
    if ( !silent ){
      message( glue::glue('\n\nProcessing {var} - Frequency table'))
      if ( length(oby) == 0 ){
        print( table(data[, var]) )
      } else {
        print( table(data[, by], data[, var]) )
      }
      cat('\n')
    }

    tmp <- data[, var]
    if ( !inherits(tmp, 'factor') ){
      tmp <- factor(tmp)
    }

    # Re-order levels in tmp
    tmp <- factor(
      tmp,
      levels = c(
        levels(tmp)[ref_levels[ivar]],
        levels(tmp)[-ref_levels[ivar]]
      )
    )
    tmp_levels <- levels(tmp)

    # Convert to an integer vector
    tmp <- as.integer(tmp)

    for (iby in seq_along(by_levels)){

      # Check that the reference group as non-missing values
      ref <- data[tmp == 1L  & tmp_by == iby & !is.na(tmp), x_var]
      ref_level <- tmp_levels[1]

      if ( length(ref) == 0 ){
        stop(
          glue::glue('No row available for the reference group (ref_level}) for variable {var} ')
        )
      }

      for ( ilevel in seq_along(tmp_levels) ){

        test <- data[tmp == ilevel & tmp_by == iby & !is.na(tmp), x_var]
        test_level <- tmp_levels[ilevel]
        by_level <- by_levels[iby]
        if ( !silent ){
          if ( length(oby) == 0){
            message( glue::glue('... processing group: {test_level}'))
          } else {
            message( glue::glue('... processing group: {test_level} ({by_level})'))
          }
        }

        if ( length(test) == 0 ){
          if ( !silent ){
            message(
              glue::glue('Warning message:\n  No row available for the test group ({test_level}) for variable {var} ')
            )
          }

          tmp_res <-data.frame(
            x_var = x_var,
            y_var = var,
            y_label = var,
            value = test_level,
            n = 0,
            reference = ilevel == 1,
            gm = NA,
            gm_lo = NA,
            gm_hi = NA,
            gm_label = NA,
            gmr = NA,
            gmr_lo = NA,
            gmr_hi = NA,
            gmr_label = NA
          )
          if ( length(oby) > 0 ){
            tmp_res$by <- by_level
          }
          res <- rbind(res, tmp_res)

        } else {
          tmp_gm_ci <- gm_ci(test)
          tmp_ci <- try(
            {gmr_ci(test, ref, paired = paired, ci = ci)},
            silent = TRUE
          )
          if ( class(tmp_ci) == 'try-error'){
            tmp_ci <- rep(NA, 2)
          }
          if (ilevel == 1){
            tmp_ci <- rep(1, 2)
          }
          tmp_res <- data.frame(
            x_var = x_var,
            y_var = var,
            y_label = var,
            value = test_level,
            n = length(test),
            reference = ilevel == 1,
            gm = gm(test),
            gm_lo = tmp_gm_ci[1],
            gm_hi = tmp_gm_ci[2],
            gm_label = NA,
            gmr = gm(test)/gm(ref),
            gmr_lo = tmp_ci[1],
            gmr_hi = tmp_ci[2],
            gmr_label = NA
          )
          if ( length(oby) > 0 ){
            tmp_res$by <- by_level
          }
          res <- rbind(res, tmp_res)

        }
      }
    }

  }

  # Reorder
  if ( length(oby) > 0 ){
    res <- res[, c(1:3, 15, 4:14)]
  }

  # Set y_var, y_label, and by as a factor to facilitate ordering of plotting
  res$y_var <- factor(res$y_var, levels = unique(res$y_var))
  res$y_label <- factor(res$y_label, levels = unique(res$y_label))
  levels(res$y_label) <- labels
  if ( length(oby) > 0 ){
    if ( inherits(data[, by], 'factor') ){
      res$by <- factor(res$by, levels = levels(data[, by]))
    } else {
      res$by <- factor(res$by)
    }
  }

  # Set value as a factor with inverse order levels to facilitate plotting
  #res$value <- factor(res$value, levels = unique(rev(res$value)))
  res$value <- factor(res$value, levels = unique(res$value))

  # Make the GM and GMR label
  res$gm_label <- ifelse(
    res$n == 0,
    'NA',
    sprintf(
      '%s [%s, %s]',
      signif(res$gm, digits),
      signif(res$gm_lo, digits),
      signif(res$gm_hi, digits)
    )
  )
  res$gmr_label <- ifelse(
    res$n == 0,
    'NA',
    sprintf(
      '%s [%s, %s]',
      signif(res$gmr, digits),
      signif(res$gmr_lo, digits),
      signif(res$gmr_hi, digits)
    )
  )

  # Get the length of gmr_label based upon assuming default font
  # 0-9: 0.5625
  # [ ] . and spaces: 0.25
  # tab character: 2
  find_str_len <- function(x){
    if (length(x) > 1){
      sapply(x, find_str_len, simplify = TRUE)
    } else {
      exploded <- unlist(strsplit(x, ''))
      len <- ifelse(
        grepl('[.]|\\s|\\[|\\]', exploded),
        0.25,
        0.5625
      )
      sum(len)
    }
  }
  gmr_label_len <- find_str_len(res$gmr_label)

  # Determine the number of tab characters to add to align the n's
  n_tabs <- max(gmr_label_len %/% 2) + 1 - (gmr_label_len %/% 2)

  # Add n's to gmr_label
  res$gmr_n_label <- ifelse(
    res$n == 0,
    'NA',
    sprintf(
      '%s [%s, %s]%sn = %s',
      signif(res$gmr, digits),
      signif(res$gmr_lo, digits),
      signif(res$gmr_hi, digits),
      sapply(
        n_tabs,
        function(x, sep){
          if ( sep == '\t' ){
            paste(rep(sep, x), collapse = '')
          } else {
            sep
          }
        },
        sep
      ),
      res$n
    )
  )

  attr(res$y_label, 'expression') <- label_expressions
  rownames(res) <- as.character(1:nrow(res))

  return(res)

}

#' Create a ggplot2 forest plot
#'
#' @description
#'
#' This function is intended to create forest plots based upon a data.frame
#' typically created by the \code{make_gmr_data} function. The output is a
#' ggplot2 object.
#'
#' @param data a data.frame; typically created using \code{make_gmr_data}
#' @param x,xmin,xmax,y the mapping variables represented by
#' \code{geom_pointrange}
#' @param color the mapping variable providing the coloring information
#' @param reference the mapping variable defining whether levels of the
#' \code{y} variable are reference ones.
#' @param label the mapping variable providing the information displaying on
#' the secondary Y axis
#' @param facet a single facetting variable (can be a vector of expression)
#' @param vline_primary,vline_secondary the X-axis values for which a primary
#' and secondary vertical lines are drawn
#' @param xlb,ylb,title optional axis and plot titles (can be expressions)
#' @param n_label a character value defining how individuals are defined
#' in the abbreviation in the table footnote, that is, what is used in the
#' definition of 'n' (eg, 'n, number of individuals'). By default, a 's'
#' will be added to the entered value.
#' @param fatten a scaling factor of the points defined by \code{x}
#' @param small_font a logical value indicating whether a smaller font size
#' should be used
#' @param is_facet_expression a logical value indicating whether the levels of
#' the \code{facet} variable are expression (TRUE) or character (FALSE) objects;
#' if set to NULL, \code{make_gmr_data} tries to make an educated guess
#'
#' @export
#'
#' @return a ggplot2 plot object. See \code{vignette('forest', package = 'ggcognigen')}
#' more details of the different design variants that can be created.
#'
#' @examples
#' \dontrun{
#' # Use expo dataset provided in the ggcognigen package
#'
#' gmrs <- make_gmr_data(
#'   x_var = c('CMAXSS', 'AUCSS', 'CMINSS'),
#'   data = expo,
#'   id_var = 'ID',
#'   by = 'DOSE',
#'   covariates = c('AGE', 'WTKG', 'BMI', 'SEXF', 'RFCAT', 'CPCAT'),
#'   labels = c(
#'     expression('Age'~'(y)'),
#'     expression('Body'~'Weight'~'(kg)'),
#'     expression('Body'~'Mass'~'Index'~'(kg/'*m^2*')'),
#'     expression('Sex'),
#'     expression('Renal'~'Function'),
#'     expression('Hepatic'~'Function')
#'   ),
#'   ref_levels = c(2, 3, 2, 1, 1, 1),
#'   digits = 3,
#'   silent = TRUE
#' )
#'
#' make_forestplot(
#'   data = subset(gmrs, x_var == 'AUCSS'),
#'   y = 'value', x = 'gmr', xmin = 'gmr_lo', xmax = 'gmr_hi',
#'   label = 'gmr_n_label',
#'   facet = 'y_label',
#'   color = 'by',
#'   vline_primary = 1,
#'   vline_secondary = c(0.8, 1.25),
#'   xlb = 'Geometric Mean Ratio [90% confidence interval]',
#'   title = expression(C[trough]~'(nmol/L)'),
#'   fatten = 2,
#'   small_font = TRUE
#' ) +
#'   scale_discrete_cognigen(n = 4, geom = 'point')
#' }
#'

make_forestplot <- function(
  data,
  x, xmin, xmax, y,
  color,
  reference,
  label,
  facet,
  vline_primary = 1,
  vline_secondary = c(0.8, 1.25),
  xlb,
  ylb,
  title,
  n_label = 'individual',
  fatten = 4,
  small_font = FALSE,
  is_facet_expression = NULL
){

  # Functions
  check_var <- function(x, data){
    is.character(x) && length(x) == 1 && x %in% names(data)
  }
  check_title <- function(x){
    is.character(x) | is.expression(x) | is.null(x)
  }
  discretise <- function(x, group) {
    x <- split(x, group)
    x <- lapply(x, function(y) {
      match(y, levels(droplevels(y)))
    })
    x <- unsplit(x, group)
    x
  }
  # Validate inputs
  if ( missing(data) ){
    stop('data argment is missing')
  } else {
    if ( !is.data.frame(data) ){
      stop('data is not a data.frame')
    }
  }

  if ( missing(x) | missing(xmin) | missing(xmax) | missing(y) | missing(facet) ){
    stop('x, xmin, xmax, y, and facet are required arguments')
  }
  if ( !check_var(x, data) | !check_var(xmin, data) | !check_var(xmax, data) |
       !check_var(y, data) | !check_var(facet, data) ){
    stop('x, xmin, xmax, y, and facet must be variables present in data')
  }

  if ( missing(label) || length(label) == 0 ){
    label <- NULL
  } else if ( !check_var(label, data) ){
    stop('label must be a variable present in data')
  }

  if ( missing(color) || length(color) == 0 ){
      color <- NULL
  } else if ( !check_var(color, data) ){
    stop('color must be a variable present in data')
  }

  if ( missing(reference) || length(reference) == 0 ){
    reference <- NULL
  } else {
    if ( !check_var(reference, data) ){
      stop('reference must be a variable present in data')
    }
    if ( !is.logical(data[, reference]) ){
      stop('reference must be a logical variable')
    }
  }

  if ( !(is.numeric(vline_primary) | is.null(vline_primary)) ){
    stop('vline_primary must be numeric or NULL')
  }
  vline_primary <- vline_primary[1]

  if ( !(is.numeric(vline_secondary) | is.null(vline_secondary)) ){
    stop('vline_secondary must be numeric or NULL')
  }

  if ( !missing(xlb) && !check_title(xlb) ){
    stop('xlb must be a character value, expression, or NULL')
  }
  if ( !missing(ylb) && !check_title(ylb) ){
    stop('ylb must be a character value, expression, or NULL')
  }
  if ( !missing(title) && !check_title(title) ){
    stop('title must be a character value, expression, or NULL')
  }
  if ( missing(n_label) ){
    if ( !is.character(n_label)){
      stop('n_label must be a character value')
    }
    n_label <- n_label[1]
  }
  if ( !(is.numeric(fatten) && all(fatten > 0)) ){
    stop('fatten must be a positive numeric value')
  }

  if ( !is.logical(small_font) ){
    stop('small font must be a logical value')
  }

  if ( length(is_facet_expression) > 0 ){
    if ( !is.logical(is_facet_expression) ){
      stop('is_facet_expression must be a logical value or NULL')
    }
    is_facet_expression <- is_facet_expression[1]
  } else {
    is_facet_expression <- NULL
  }

  # Coerce to factor
  if ( !inherits(data[, y], 'factor') ){
    data[, y] <- factor(data[, y])
  }
  if ( !inherits(data[, facet], 'factor') ){
    data[, facet] <- factor(data[, facet])
  }
  if ( length(color) > 0 && !inherits(data[, color], 'factor') ){
    data[, color] <- factor(data[, color])
  }

  # Detect if facet levels are expression
  if ( is.null(is_facet_expression) ){
    if ( length(attr(data[, facet], 'expression')) > 0 && attr(data[, facet], 'expression') ){
      is_facet_expression <- attr(data[, facet], 'expression')
    } else {
      is_facet_expression <- any( is.expression(levels(data[, facet])) ) |
        any(
          ( grepl('~', levels(data[, facet]) ) & grepl('"', levels(data[, facet]) ) ) |
            ( grepl('*', levels(data[, facet]) ) & grepl('"', levels(data[, facet]) ) )
        )
    }

  }

  # Change label for reference group
  if ( !missing(label) & length(reference) > 0 ){
    data[data$reference, label] <- gsub(
      '1 \\[1, 1\\]\t',
      'Reference',
      data[data$reference, label]
    )
  }


  # Capture if coloring is for group stratification or simple coloring of data
  if ( length(color) > 0 ) {
    group_stratification <- nrow(data[!duplicated(data[, c(y, facet)]),]) !=
      nrow(data[!duplicated(data[, c(y, facet, color)]),])
  } else {
    group_stratification <- FALSE
  }

  # Need to reverse order of y variable if no stratification
  if ( !group_stratification ){
    data[, y] <- factor(data[, y], levels = rev(levels(data[, y])))
  }

  # Modify y variable to avoid duplicate secondary y-axis break labels
  data[, 'y_mod'] <- droplevels(interaction(data[, y], data[, facet]))
  y_mod <- 'y_mod'
  y_mod_group <- 'y_mod'

  # Set different grouping variable if stratification is required
  # and set panel-specific secondary axis
  if ( group_stratification ){
    data[, 'y_mod'] <- discretise(data[, y], data[, facet])
    data[, 'y_mod_group'] <- droplevels(interaction(data[, y], data[, color]))
    y_mod_group <- 'y_mod_group'

    panel_sec_yaxis <- list()
    if ( !missing(label) ){

      nstrata <- nlevels(data[, color])
      for ( fac in levels(data[, facet]) ){
        fac_labels1 <- levels( droplevels( data[data[, facet]==fac, y] ) )
        fac_breaks1 <- seq_along(fac_labels1)

        fac_labels2 <- data[data[, facet]==fac, label]
        fac_range <- 0.9*(nstrata-1)/nstrata
        fac_breaks2 <- as.vector(
          outer(
            fac_breaks1,
            seq(0, 1, length.out = nstrata) * fac_range - fac_range/2,
            FUN = '+'
          )
        )

        panel_sec_yaxis <- c(
          panel_sec_yaxis,
          eval(
            parse(
              text = sprintf(
                paste(
                  '%s == \'%s\' ~ ggplot2::scale_y_continuous(',
                  '  breaks = c(%s),',
                  '  labels = c(\'%s\'),',
                  '  trans = \'reverse\',',
                  '  sec.axis = ggplot2::dup_axis(',
                  '    guide = ggh4x::guide_axis_manual(',
                  '      breaks = c(%s),',
                  '      labels = c(\'%s\'),',
                  '      title = NULL',
                  '    )',
                  '  )',
                  ')',
                  sep = '\n'
                ),
                facet,
                fac,
                paste(fac_breaks1, collapse = ','),
                paste(fac_labels1, collapse = '\',\''),
                paste(fac_breaks2, collapse = ','),
                paste(fac_labels2, collapse = '\',\'')
              )
            )
          )
        )
      }
    }

  }

  # Create main plot components
  p <- ggplot2::ggplot(data = data) +
    ggplot2::aes_string(y = y_mod, x = x, xmin = xmin, xmax = xmax, group = y_mod_group)
  if ( length(reference) > 0 ){
    p <- p +
      ggplot2::aes_string(pch = reference, fill = reference)
  }
  p <- p +
    ggplot2::geom_vline(xintercept = vline_primary, size = 0.5) +
    ggplot2::geom_vline(xintercept = vline_secondary, lty = 'dashed', size = 0.5)

  if ( length(color) == 0 ){
    p <- p + ggplot2::geom_pointrange(fatten = fatten[1])
  } else {
    p <- p + ggplot2::geom_pointrange(
      ggplot2::aes_string(group = y_mod_group, color = color),
      position = ggplot2::position_dodge(width = 0.9),
      fatten = fatten[1]
    )
  }

  if ( is_facet_expression ){
    p <- p +
      ggplot2::facet_wrap(
        facet,
        scales = 'free_y',
        ncol = 1,
        labeller = ggplot2::label_parsed
      )
  } else {
    p <- p +
      ggplot2::facet_wrap(
        facet,
        scales = 'free_y',
        ncol = 1
      )
  }
  p <- p +
    ggh4x::force_panelsizes(
      rows = as.numeric(table(data[,facet])),
      cols = 1,
      respect = FALSE
    ) +
    theme_cognigen_grid(
      smaller = small_font,
      axis.ticks.length.y = ggplot2::unit(0, 'npc'),
      strip.background = ggplot2::element_rect(color = 'transparent'),
      strip.text.y = ggplot2::element_text(
        margin = ggplot2::margin(
          ifelse(small_font, 0, 2),
          0,
          ifelse(small_font, 0, 2),
          0,
          'points'
        )
      ),
      panel.spacing = ggplot2::unit(0, 'pt')
    ) +
    ggplot2::scale_y_discrete(
      breaks = data[, y_mod],
      labels = data[, y]
    )

  if ( !missing(label) ){
    if ( length(color) > 0 & group_stratification ){
      p <- p + ggh4x::facetted_pos_scales( y = panel_sec_yaxis )
    } else {
      p <- p +
        ggplot2::guides(
          y.sec = ggh4x::guide_axis_manual(
            breaks = data[, y_mod],
            labels = data[, label]
          )
        )
    }
  }

  if ( length(reference) > 0 ){
    p <-p +
      ggplot2::scale_shape_manual(
        values = c(19, 21)
      ) +
      ggplot2::scale_fill_manual(
        values = c('white', 'white')
      ) +
      ggplot2::guides(shape = 'none', fill = 'none')
  }

  if ( !missing(xlb) ){
    p <- p + ggplot2::xlab(xlb[1])
  }

  if ( !missing(ylb) ){
    p <- p + ggplot2::ylab(ylb[1])
  } else {
    p <- p + ggplot2::ylab(NULL)
  }

  if ( !missing(title) ){
    p <- p + ggplot2::ggtitle(title[1])
  }

  if ( any( grepl('^(|^\\[|)$|\\]$', data[, y]) ) ){
    if ( any( grepl('n', data[ label]) ) ){
      p <- p + ggplot2::labs(
        caption = paste(
          glue::glue('n is the number of {n_label}s in each group'),
          '[ or ] indicates respective limit is included in the interval',
          '( or ) indicates respective limit is not included in the interval',
          sep = '\n'
        )
      )
    } else {
      p <- p + ggplot2::labs(
        caption = paste(
          '[ or ] indicates respective limit is included in the interval',
          '( or ) indicates respective limit is not included in the interval',
          sep = '\n'
        )
      )
    }
  } else {
    if ( any( grepl('n', data[ label]) ) ){
      p <- p + ggplot2::labs(
        caption = glue::glue('n is the number of {n_label}s in each group')
      )
    }
  }

  return(p)

}

#' Format and write GMR table to html, docx, or tex files
#'
#' @description
#' \code{make_gmr_table} is intended to save tables of geometric mean (GMs) and geometric
#' mean rations (GMRs) into a report-ready table in various formats.
#'
#' @param data a data.frame; typically a truncated version of a data.frame
#' created using \code{make_gmr_data} in which only a select list of
#' variables are kept
#' @param file paths to one or more files to create
#' @param format the format(s) of the output file, either 'html', 'word', or
#' 'latex'. Length must match that of \code{file}
#' @param headers a vector of character strings providing the column headers
#' to be displayed in the output table
#' @param title a title for the output table
#' @param orientation control the page orientation for the output table,
#' either 'landscape' or 'portrait'
#' @param abbreviations a named list of abbreviations and abbreviation
#' definition
#' @param n_label a character value defining how individuals are defined
#' in the abbreviation in the table footnote, that is, what is used in the
#' definition of 'n' (eg, 'n, number of individuals'). By default, a 's'
#' will be added to the entered value.
#' #' @export
#'
#' @return this function is used for its side-effects and does not return
#' any object
#'
#' @examples
#' \dontrun{
#' # Use expo dataset provided in the ggcognigen package
#'
#' gmrs <- make_gmr_data(
#'   x_var = c('CMAXSS', 'AUCSS', 'CMINSS'),
#'   data = expo,
#'   id_var = 'ID',
#'   by = 'DOSE',
#'   covariates = c('AGE', 'WTKG', 'BMI', 'SEXF', 'RFCAT', 'CPCAT'),
#'   labels = c(
#'     expression('Age'~'(y)'),
#'     expression('Body'~'Weight'~'(kg)'),
#'     expression('Body'~'Mass'~'Index'~'(kg/'*m^2*')'),
#'     expression('Sex'),
#'     expression('Renal'~'Function'),
#'     expression('Hepatic'~'Function')
#'   ),
#'   ref_levels = c(2, 3, 2, 1, 1, 1),
#'   digits = 3,
#'   silent = TRUE
#' )
#'
#' gmrs2 <- gmrs[, c('x_var', 'y_label', 'value', 'by', 'n', 'gm_label','gmr_label')]
#' levels(gmrs2[, 'y_label']) <- expr2char(levels(gmrs2[, 'y_label']))
#'
#' make_gmr_table(
#'   data = gmrs2,
#'   file = c(
#'     file.path(tempdir(), 'gmtable.html'),
#'     file.path(tempdir(), 'gmtable.docx')
#'  ),
#'   format = c('html', 'word'),
#'   headers = c(
#'     'Exposure Measure',
#'     'Covariate',
#'     'Group',
#'     'Dose',
#'     'n',
#'     'Geometric Mean [90% CI]',
#'     'Geometric Mean Ratio [90% CI]'
#'   ),
#'   abbreviations = list(
#'     CI = 'confidence interval'
#'   ),
#'   n_label = 'patient'
#' )
#' }

make_gmr_table <- function(
  data,
  file,
  format,
  headers,
  title,
  orientation = 'landscape',
  abbreviations,
  n_label = 'individual'
){

  # Create inputs
  if ( missing(data) ){
    stop('data argment is missing')
  } else {
    if ( !is.data.frame(data) ){
      stop('data is not a data.frame')
    }
  }

  if ( missing(file) ){
    file <- 'R.html'
  } else {
    if ( !is.character(file)){
      stop('file must be a character value')
    }
    #file <- file[1]
  }

  if ( missing(format) ){
    format <- 'html'
  } else {
    if ( !is.character(format)){
      stop('format must be a character value')
    }
  }

  if ( length(format) != length(file) ){
    stop('legnth of file and format arguments must match')
  }

  if (length(format) > 1){
    for (i in seq_along(format)){
      make_gmr_table(
        data = data,
        file = file[i],
        format = format[i],
        headers = headers,
        title = title,
        orientation = orientation,
        abbreviations = abbreviations,
        n_label = n_label
      )
      next()
    }
    return(invisible())
  }

  format <- match.arg(format, c('html', 'word', 'latex'))

  extension <- tools::file_ext(file)

  if ( format == 'html' ){
    if ( extension != 'html' ){
      stop('.html is the expected extension in file argument')
    }
  } else if ( format == 'word' ){
    if ( extension != 'docx' ){
      stop('.docx is the expected extension in file argument')
    }
  } else {
    if ( extension != 'tex' ){
      stop('.tex is the expected extension in file argument')
    }
  }

  if ( !missing(orientation) ){
    if ( !is.character(orientation)){
      stop('orientation must be a character value')
    }
    orientation <- match.arg(orientation[1], c('landscape', 'portrait'))
  }

  if ( missing(n_label) ){
    if ( !is.character(n_label)){
      stop('n_label must be a character value')
    }
    n_label <- n_label[1]
  }

  # Check and process abbreviations
  if ( missing(abbreviations) ){
    abbreviations <- sprintf(
      'Abbreviations: CI, confidence interval; n, number of %ss.',
      n_label
    )
  } else {
    if ( !is.list(abbreviations)){
      stop('abbreviations must be a list')
    }
    abbreviations <- c(
      abbreviations,
      list(
        CI = 'confidence interval',
        n = sprintf('number of %ss', n_label)
      )
    )
    abbreviations <- abbreviations[ sort(names(abbreviations)) ]
    tmp <- c()
    for ( symbol in names(abbreviations) ){
      tmp <- c(
        tmp,
        sprintf('%s, %s', symbol, abbreviations[[symbol]])
      )
    }
    abbreviations <- sprintf(
      'Abbreviations: %s.',
      paste(tmp, collapse = '; ')
    )
  }

  # Convert table content to character
  for ( icol in 1:ncol(data) ){
    data[, icol] <- as.character(data[, icol])
  }

  # Add headers
  if ( !missing(headers) ){
    if ( !is.character(headers)){
      stop('headers must be a character vector')
    }
  } else {
    headers <- names(data)
  }
  headers <- rep(headers, length.out = ncol(data))
  headers <- as.data.frame( matrix(headers, nrow  = 1) )
  names(headers) <- names(data)
  data <- rbind(
    headers,
    data
  )

  # Convert data to huxtable
  ht <- huxtable::as_huxtable(data, add_colnames = FALSE)

  # Add title
  if ( !missing(title) ){

    if ( !is.character(title)){
      stop('title must be a character value')
    }
    title <- title[1]

    if ( format %in% c('html', 'latex') ){
      huxtable::caption(ht) <- title
    } else {
      # Titles are part of the table in Word export
      tmp <- names(ht)
      ht <- huxtable::add_rows(
        ht,
        rep(title, ncol(ht)),
        after = 0
      )
      names(ht) <- tmp
      ht <- kiwiexport::merge_repeated_cells(ht, 1, 1:ncol(ht))
    }
  }

  # Add last row
  abbreviations <- gsub(
    ', ',
    switch(
      format,
      'html' = ',&nbsp;',
      'word' = ',\u00A0',
      'latex' = ',~'
    ),
    abbreviations
  )
  ht <- huxtable::add_rows(
    ht,
    rep(abbreviations, ncol(ht))
  )
  huxtable::escape_contents(ht)[nrow(ht), ] <- FALSE
  ht <- huxtable::set_bold(ht, row = nrow(ht), col = 1:ncol(ht), value = FALSE)
  ht <- huxtable::set_align(ht, row = nrow(ht), col = 1:ncol(ht), value = 'left')

  # Merge cells
  ht <- kiwiexport::merge_repeated_cells(ht, 1:nrow(ht), 1)
  ht <- kiwiexport::merge_repeated_cells(ht, 1:nrow(ht), 2)
  ht <- kiwiexport::merge_repeated_cells(ht, nrow(ht), 1:ncol(ht))

  # Formatting
  if ( format %in% c('html', 'latex') ){
    huxtable::wrap(ht) <- TRUE
    huxtable::position(ht) <- 'center'
    ht <- kiwiexport::theme_border(
      ht,
      bold_rows = 1,
      bold_cols = 1,
      position = 'center',
      format = format
    )
    huxtable::width(ht) <- ifelse( format == 'html', 0.75, 1 )
  } else {
    if ( !missing(title) ){
      ht <- kiwiexport::theme_border_flextable(ht, header_rows = 1:2)
    } else {
      ht <- kiwiexport::theme_border_flextable(ht, header_rows = 1)
    }
  }

  # Create table file
  if ( format == 'html' ){
    kiwiexport::make_html(
      ht,
      file = file
    )
  } else if ( format == 'word' ){
    kiwiexport::make_docx(
      ht,
      file = file,
      orientation = orientation
    )
  } else {
    kiwiexport::make_latex(
      ht,
      file = file,
      options = list(
        header_rows = 1,
        widths = list(NULL),
        orientation = orientation
      )
    )
  }

  invisible()

}

#' @rdname make_gmr_table
#'
#' @description
#' \code{expr2char} is a utility function to convert expression into character
#' values which can be called prior to calling \code{make_grm_table} (see examples)
#'
#' @param x a vector of expressions
#'
#' @export
#'
#' @return a character vector
#'
expr2char <- function(x){

  xx <- gsub('^"|"$', '', as.character(x))
  xx <- gsub('" ~ "', ' ', xx)
  xx <- gsub('" [*] | [*] "', '', xx)
  xx

}
