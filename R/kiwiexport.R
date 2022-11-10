# the functions defined here are copied from the kiwiexport 1.1.0 source code.
# since kiwiexport is currently proprietary, depending on kiwiexport would
# result in most users not being able to install ggcognigen.

#' Merge cells (copied from kiwiexport)
#'
#' The \code{merge_repeated_cells} function automatically detects cells with
#' identical content in huxtables and merge theme. The cells must be contiguous
#' and form a matrix shape within the main table.
#'
#'
#' @param ht A huxtable object.
#' @param row A vector of rows to be scanned for mergeable cells.
#' @param col A vector of columns to be scanned for mergeable cells.
#' @param no_merge_row A vector of rows within \code{row} which would not be
#' scanned for mergeable cells.
#' @param no_merge_col A vector of columns within \code{row} which would not be
#' scanned for mergeable cells.
#' @param merge_first Whether cells are merged by column (default) or row
#' first.  This affect how irregular shapes of identical cells are merged.
#' @return \code{merge_repeated_cells} returns a huxtable object with merged
#' cells.
#' @author Sebastien Bihorel
#' @keywords methods internal
merge_repeated_cells <- function(
    ht,
    row,
    col,
    no_merge_row = NULL,
    no_merge_col = NULL,
    merge_first = c('column', 'row')
)
{

  assertthat::assert_that(huxtable::is_huxtable(ht))

  mod_ht <- as.matrix(ht)

  if (missing(row)){
    row <- 1:nrow(mod_ht)
  }
  if (missing(col)){
    col <- 1:ncol(mod_ht)
  }
  if (!is.numeric(row)){
    row <- 1:nrow(mod_ht)
  } else {
    row <- sort( unique(row) )
    row[1] <- max( c(1, min(row)) )
    row[length(row)] <- min( c(nrow(mod_ht), max(row)) )
  }
  if (!is.numeric(col)){
    col <- 1:ncol(mod_ht)
  } else {
    col <- sort(unique(col))
    col[1] <- max( c(1, min(col)) )
    col[length(col)] <- min( c(ncol(mod_ht), max(col)) )
  }
  if (!is.numeric(no_merge_row)){
    no_merge_row <- NULL
  } else {
    no_merge_row <- sort( unique(no_merge_row) )
    no_merge_row[1] <- max( c(1, min(no_merge_row)) )
    no_merge_row[length(no_merge_row)] <- min( c(nrow(mod_ht), max(no_merge_row)) )
  }
  if (!is.numeric(no_merge_col)){
    no_merge_col <- NULL
  } else {
    no_merge_col <- sort(unique(no_merge_col))
    no_merge_col[1] <- max( c(1, min(no_merge_col)) )
    no_merge_col[length(no_merge_col)] <- min( c(ncol(mod_ht), max(no_merge_col)) )
  }
  merge_first <- match.arg(merge_first)

  if (length(row) == 1 && length(col) == 1 && row == 1 && col == 1){
    return(ht)
  }

  all_same <- function(x) {
    xx <- as.vector(x)
    all(xx == xx[1])
  }

  # Transpose mod_ht if necessary
  if (merge_first == 'row'){
    mod_ht <- t(mod_ht)
    tmp <- row
    row <- col
    col <- tmp
    rm(tmp)
  }

  # Initiate mapping objects
  mapping_matrix <- matrix(
    0,
    nrow = nrow(mod_ht),
    ncol = ncol(mod_ht)
  )
  mapping_matrix[no_merge_row, no_merge_col] <- -99
  coords <- list()
  block <- 1

  for (irow in row){

    for (icol in col){

      if ( mapping_matrix[irow, icol] == 0 ){

        # Initiate tokens
        hstop <- vstop <- FALSE
        hshift <- vshift <- 1

        # Expansion of search rectangles
        while ( !hstop | !vstop ) {

          # Search algorithm
          if ( (icol + hshift) %in% col & !hstop ){
            if ( all_same(mod_ht[irow:(irow+vshift-1), icol:(icol+hshift)]) &
                 all(mapping_matrix[irow:(irow+vshift-1), icol:(icol+hshift)] == 0) ){
              hshift <- hshift + 1
            } else {
              hstop <- TRUE
            }
          } else {
            hstop <- TRUE
          }

          if ( (irow + vshift) %in% row & !vstop ){
            if ( all_same(mod_ht[irow:(irow+vshift), icol:(icol+hshift-1)]) &
                 all(mapping_matrix[irow:(irow+vshift), icol:(icol+hshift-1)] == 0) ){
              vshift <- vshift + 1
            } else {
              vstop <- TRUE
            }
          } else {
            vstop <- TRUE
          }
        }

        # Store information
        if ( hshift > 1 | vshift > 1 ){
          coords[[block]] <- list(
            row = irow:(irow + vshift - 1),
            col = icol:(icol + hshift - 1)
          )
          mapping_matrix[coords[[block]]$row, coords[[block]]$col] <- block
          block <- block + 1
        }

      }
    }
  }

  # Merge cells
  if ( length(coords) > 0 ){
    for ( iblock in 1:length(coords) ){
      if (merge_first == 'column'){
        ht <- huxtable::merge_cells(ht, coords[[iblock]]$row, coords[[iblock]]$col)
      } else {
        ht <- huxtable::merge_cells(ht, coords[[iblock]]$col, coords[[iblock]]$row)
      }
    }
  }

  return(ht)

}


#' Theme functions (copied from kiwiexport)
#'
#' The \code{kiwiexport} packages provides theming functions to format tables.
#' The \code{theme_border} function is intended for huxtable objects, while the
#' \code{theme_border_flextable} transform a huxtable object into a flextable
#' object prior to applying the same theme.
#'
#' @name kiwiexport_theme_border
#' @aliases theme_border theme_border_flextable
#' @param ht A huxtable object.
#' @param bold_rows The rows to be formatted with bold font face.
#' @param bold_cols The columns to be formatted with bold font face.
#' @param position The position of the table within the page. A length-one
#' character vector which may be "left", "center", "right", "wrapleft" or
#' "wrapright".
#' @param format Either "html", "word", or "latex"
#' @param header_rows A vector of integers defining the rows containing the
#' table headers.
#' @return \code{theme_border} returns a formatted huxtable object.
#' \code{theme_border_flextable} returns a formatted flextable object, intended
#' for export into .docx (Microsoft Word) documents.
#' @author Sebastien Bihorel
#' @seealso \code{\link[huxtable]{position}},
#' @keywords methods internal
NULL

theme_border <- function(ht, bold_rows = 1, bold_cols = 1, position = 'left', format){

  ht <- huxtable::set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), ifelse( format == 'html', 1, 0.5))
  ht <- huxtable::set_all_border_colors(
    ht,
    huxtable::everywhere,
    huxtable::everywhere,
    value = ifelse( format == 'html', grDevices::rgb(204/256, 204/256, 204/256, 1), 'black')
  )
  ht <- huxtable::set_left_border(ht, nrow(ht), huxtable::everywhere, FALSE)
  ht <- huxtable::set_right_border(ht, nrow(ht), huxtable::everywhere, FALSE)
  ht <- huxtable::set_bottom_border(ht, nrow(ht), huxtable::everywhere, FALSE)
  ht <- huxtable::set_bold(ht, bold_rows, huxtable::everywhere, TRUE)
  ht <- huxtable::set_bold(ht, huxtable::everywhere, bold_cols, TRUE)
  if ( format == 'html' ){
    ht <- huxtable::set_background_color(ht, bold_rows, huxtable::everywhere, value = grDevices::rgb(240/256, 240/256, 240/256, 1))
  }
  ht <- huxtable::set_position(ht, position)

  if ( format == 'latex' ){
    dims <- dim(ht)
    ht <- huxtable::set_left_padding(ht, huxtable::everywhere, huxtable::everywhere, 0)
    ht <- huxtable::set_right_padding(ht, huxtable::everywhere, huxtable::everywhere, 0)
    ht <- huxtable::set_top_padding(ht, huxtable::everywhere, huxtable::everywhere, 0)
    ht <- huxtable::set_bottom_padding(ht, huxtable::everywhere, huxtable::everywhere, 0)
    # ht <- huxtable::set_align(ht, huxtable::everywhere, huxtable::everywhere, 'left')
    ht <- huxtable::set_valign(ht, huxtable::everywhere, huxtable::everywhere, 'top')
    # ht <- huxtable::set_valign(ht, bold_rows, huxtable::everywhere, 'bottom')
  }

  return(ht)

}


theme_border_flextable <- function(ht, header_rows) {
  o_ht <- ht
  ht <- huxtable::set_position(ht, 'left')
  ht <- huxtable::set_left_padding(ht, 1:nrow(ht), 1:ncol(ht), 0)
  ht <- huxtable::set_right_padding(ht, 1:nrow(ht), 1:ncol(ht), 0)
  ht <- huxtable::set_bottom_padding(ht, 1:nrow(ht), 1:ncol(ht), 2)
  ht <- huxtable::set_top_padding(ht, 1:nrow(ht), 1:ncol(ht), 2)
  ht <- huxtable::as_flextable(ht)
  ht <- flextable::style(
    ht,
    i = 1:nrow(o_ht),
    j = 1:ncol(o_ht),
    pr_t = officer::fp_text(color = 'black', font.size = 10, font.family = 'Times New Roman'),
    pr_c = officer::fp_cell(margin.left = 6, margin.right = 6),
    part = 'all'
  )
  ht <- flextable::bold(ht, bold = FALSE)
  ht <- flextable::bold(ht, i = header_rows, j = 1:ncol(o_ht), bold = TRUE)
  ht <- flextable::align(ht, align = 'left')
  ht <- flextable::valign(ht, valign = 'top')

  ht <- flextable::border(
    ht,
    i = header_rows,
    j = 1:ncol(o_ht),
    border.bottom = officer::fp_border(color = 'black', width = 0.5),
    part = 'all'
  )
  spans <- huxtable::rowspan(o_ht)[,1]
  if ( any(spans > 2) ){
    for ( multi_span in which(spans > 2) ){
      spans[ (multi_span+1):(multi_span+spans[multi_span]-2) ] <- 0
    }
  }
  ht <- flextable::border(
    ht,
    i = which(spans == 1),
    j = 1:ncol(o_ht),
    border.bottom = officer::fp_border(color = 'black', width = 0.5),
    part = 'all'
  )
  ht <- flextable::border(
    ht,
    part='all',
    i = nrow(o_ht),
    j = 1:ncol(o_ht),
    border.bottom = officer::fp_border(color = 'white', width = 0)
  )
  ht <- flextable::set_table_properties(ht, width = 1, layout = 'autofit')
  ht
}


#' Create export files (copied from kiwiexport)
#'
#' The \code{kiwiexport} packages provides utility functions to print huxtable
#' or flextable objects to either .html, .docx (Microsoft Office), .tex
#' (LaTeX), and .xlsx (Microsoft Excel) document.
#'
#' All \code{make_...} functions do not return any value but all have the
#' side-effect of create a file (either .docx, .html, .tex, or .xlsx)
#' containing one or more tables.
#'
#' @name kiwiexport_make
#' @aliases make_docx make_html make_latex make_xlsx
#' @param ... One or more huxtable or flextable (only for \code{make_docx})
#' objects.
#' @param file The path of the file to be created.
#' @param borders A single integer defining the width of the table borders.
#' @param orientation Defines the page orientation; either 'portrait' or
#' 'landscape'.
#' @param options A list of LaTeX formatting options. The length of the list
#' must match the number of huxtable objects provided to \code{make_latex}.
#' Each list element should include the following levels:
#' \itemize{
#' \item header_rows A vector of integer defining the index of the header rows
#' in the huxtable object,
#' \item widths A numeric vector defining the relative widths of the table
#' columns,
#' \item orientation Defines the page orientation; either 'portrait' or 'landscape', and
#' \item pese A logical value defining whether the huxtable object is a
#' parameter estimates and standard errors table (TRUE) or not (FALSE).
#' }
#' @author Sebastien Bihorel
#' @keywords methods internal
NULL

make_html <- function (
    ...,
    file = 'huxtable-output.html',
    borders = 0.4)
{

  assertthat::assert_that(assertthat::is.number(borders))
  force(file)
  hts <- huxtable:::huxtableize(list(...)[!sapply(list(...), is.null)], borders)

  sink(file)
  cat(
    paste(
      '<!DOCTYPE html>',
      '<html>',
      '<body>',
      '<style>',
      'table {',
      '  font-family: "Source Sans Pro", "Helvetica Neue", "Helvetica", "Arial", "sans-serif";',
      '  color: #333;',
      '}',
      'table td{',
      '  font-size: 11px;',
      '}',
      'table tr:hover td {',
      '  background: #E8EDF4;',
      '}',
      'caption {',
      '  font-size: 14px;',
      '  font-weight: bold;',
      '  margin-bottom: 8px;',
      '}',
      '#abbreviations, #footnotes, #shrinkage, #kiwi_runid, #subcaption {',
      '  font-size: 11px;',
      '}',
      '</style>\n',
      sep = '\n'
    )
  )

  message('  ... Creating HTML tables')

  tryCatch(
    {
      lapply(hts, function (ht) {
        huxtable::print_html(ht)
        cat("\n\n")
      })
      cat("</body>\n</html>")
    },
    error = identity,
    finally = {sink()}
  )

  #---- Correct table top and bottom margins ----
  html <- readChar(file, nchars = file.info(file)$size)
  html <- gsub(
    'margin-bottom: 2em; margin-top: 2em',
    'margin-bottom: 1em; margin-top: 0',
    html
  )

  #---- Replace characters that are problematic for Vantage
  html <- gsub('>\\[', '>&#91;',html)
  html <- gsub('\\[,', '&#91;<',html)
  html <- gsub('>\\]', '>&#93;',html)
  html <- gsub('\\]<', '&#93;<',html)
  html <- gsub('>\\(', '>&#40;',html)
  html <- gsub('\\(<', '&#40;<',html)
  html <- gsub('>\\)', '>&#41;',html)
  html <- gsub('\\)<', '&#41;<',html)


  write(html, file)

  invisible(NULL)

}


make_docx <- function(
    ...,
    file = 'huxtable-output.docx',
    borders = 0.4,
    orientation = 'portrait')
{

  assertthat::assert_that(assertthat::is.number(borders))
  force(file)
  fts <- list(...)[!sapply(list(...), is.null)]

  edit_format <- function(x){
    if (grepl('@@_', x[[1]]$txt)){

      # Break strings using start and end tags
      components <- unlist(
        strsplit(
          unlist(
            strsplit(
              x[[1]]$txt,
              '@@_'
            )
          ),
          '@_@'
        )
      )
      # Find superscripts and italic components
      is.superscript <- grepl('@_s@', components)
      is.italic <- grepl('@_i@', components)

      # Strip remaining tags
      components <- gsub('@_[[:alpha:]]@', '', components)

      # Build replacement
      replacement <- 'flextable::as_paragraph('
      for (icomponent in 1:length(components)){

        components[icomponent] <- sprintf(
          '"%s"',
          components[icomponent]
        )

        if ( is.superscript[icomponent] ){
          components[icomponent] <- sprintf(
            'flextable::as_sup(%s)',
            components[icomponent]
          )
        }

        if ( is.italic[icomponent] ){
          components[icomponent] <- sprintf(
            'flextable::as_i(%s)',
            components[icomponent]
          )
        }

        replacement <- paste0(
          replacement,
          components[icomponent],
          ifelse(icomponent == length(components), '', ', ')
        )
      }

      replacement <- paste0(replacement, ')')

      # Return replacement
      eval(parse(text = gsub('%', '\\%', replacement)))
    } else {
      x
    }
  }

  if ( orientation == 'landscape' ){
    template <- 'templates/ftr_template_landscape.docx'
  } else {
    template <- 'templates/ftr_template.docx'
  }
  if ( system.file(template, package = 'ggcognigen') == ''){
    my_doc <- officer::read_docx(file.path('~/git-workspace/ggcognigen/inst', template))
  } else {
    my_doc <- officer::read_docx(
      system.file(template, package = 'ggcognigen')
    )
  }

  for (ift in 1:length(fts)) {
    gc()
    message(
      sprintf(
        '  ... Creating Word table %s/%s',
        ift,
        length(fts)
      )
    )

    ft <- fts[[ift]]

    # Scan content for superscripts (text between @_@) and italic (text between @@_@@) make changes
    ft_data <- ft$body$content$content$data
    for (icol in 1:ncol(ft_data)){
      for (irow in 1:nrow(ft_data)){
        ft_data[irow, icol] <- edit_format(ft_data[irow, icol])
      }
    }
    ft$body$content$content$data <- ft_data

    # Insert table in document
    if ( ift > 1 ){
      my_doc <- officer::body_add_break(my_doc)
    }

    my_doc <- flextable::body_add_flextable(my_doc, ft)
    my_doc <- officer::body_add_par(my_doc, ' ')

  }

  print(my_doc, target = file)

  invisible(NULL)

}


make_latex <- function (
    ...,
    file = 'huxtable-output.tex',
    borders = 0.4,
    options = NULL)
{
  assertthat::assert_that(assertthat::is.number(borders))
  force(file)

  hts <- list(...)[!sapply(list(...), is.null)]

  open_file <- NULL

  # Initiate LaTeX header
  latex <- c()

  # Get LaTeX code
  for ( ihux in 1:length(hts) ){
    gc()
    message(
      sprintf(
        '  ... Creating LaTeX table %s/%s',
        ihux,
        length(hts)
      )
    )

    if ( length(hts[[ihux]]) > 0 ){

      # Get table code
      latex <- c(
        latex,
        if ( ihux > 1 ){
          ''
        },
        get_pharmtex_code(
          hts[[ihux]],
          header_rows = options$header_rows[[ihux]],
          widths = options$widths[[ihux]],
          orientation = options$orientation[[ihux]],
          pese = options$pese[[ihux]]
        )
      )
    }
  }


  # Print LaTeX code
  write(paste(latex, collapse = '\n'), file)

  invisible(NULL)

}


get_pharmtex_code <- function(
    ht = NULL,
    header_rows = NULL,
    widths = NULL,
    orientation = 'portrait',
    pese = FALSE
){

  #----- Collapse 1st and 2nd column of pese when they have the same content -----
  if ( pese && all(ht[,1] == ht[,2]) ){
    if ( length(widths) > 1 ){
      widths[1] <- widths[1]+widths[2]
      widths <- widths[-2]
    }

    # Delete column and adjust rowspan and colspan properties (not ideal
    # with huxtable 5.0.0 use delte_cols
    ht <- ht[, -2]
    col2_index <- which(attr(ht, 'colspan')[, 1] == 2)
    attr(ht, 'colspan')[col2_index, 1] <- 1
  }

  #----- Extract description content -----
  description <- ht[nrow(ht), 1]

  #----- Process dimensions -----
  ht <- ht[-nrow(ht), ]
  nrows <- nrow(ht)
  ncols <- ncol(ht)
  rowspans <- o_rowspans <- huxtable::rowspan(ht)
  colspans <- o_colspans <- huxtable::colspan(ht)

  if ( length(widths) == 0 ){
    widths <- rep(1, ncols)
  } else {
    widths <- widths[1:ncols]
  }

  for ( irow in 1:nrows ){
    # Find ranges of spanning cells
    for ( icol in 1:ncols ){
      if ( rowspans[irow, icol] > 0 & colspans[irow, icol] > 0) {
        if (rowspans[irow, icol] > 1 | colspans[irow, icol] > 1) {
          rowspans[irow:(irow + o_rowspans[irow, icol] - 1), icol:(icol + o_colspans[irow, icol] - 1)] <- 0
          colspans[irow:(irow + o_rowspans[irow, icol] - 1), icol:(icol + o_colspans[irow, icol] - 1)] <- 0
          rowspans[irow, icol] <- o_rowspans[irow, icol]
          colspans[irow, icol] <- o_colspans[irow, icol]
        }
      }
    }
    # Get rid of zeros of the right side of the 1st non zero
    nonzero <- which.max(rowspans[irow, ] > 0)
    index <- which(rowspans[irow, ] == 0 & (1:ncols) > nonzero)
    if ( length(index) > 0 ){
      rowspans[irow, index] <- -Inf
    }
  }

  #----- Table start -----
  if ( orientation == 'landscape') {
    latex <- c(
      '\\begin{landscape}',
      '\\pmxtextable'
    )
  } else {
    latex <- '\\pmxtextable'
  }


  #----- Add number of header rows -----
  latex <- c(latex,
             '[',
             '%@headnum_start@',
             length(header_rows),
             '%@headnum_end@',
             ']'
  )

  #----- Add alignment information -----
  latex <- c(latex,
             '{',
             '%@align_start@',
             sprintf(
               paste(
                 sprintf('L{%s}', widths),
                 collapse = ' '
               )
             ),
             '%@align_end@',
             '}'
  )

  #----- Add default table label -----
  latex <- c(latex, '{tab:label@}')

  #----- Add caption -----
  latex <- c(latex,
             '{',
             '%@caption_start@',
             huxtable:::make_caption(ht, 'latex'),
             '%@caption_end@',
             '}'
  )

  #----- Add description footnote information -----
  latex <- c(latex,
             '{',
             '%@description_start@',
             description,
             '%@description_end@',
             '}'
  )

  #----- Add table body -----
  latex <- c(latex,
             '{',
             '%@table_start@'
  )

  # Build content row by row
  header_latex_rows <- c()
  for (irow in 1:nrows){
    latex_row <- c()
    for (icol in 1:ncols){
      if ( rowspans[irow, icol] > 0 & colspans[irow, icol] > 0 ) {
        latex_row <- c(
          latex_row,
          sprintf(
            '%s%s%s',
            ifelse(
              colspans[irow, icol] > 1,
              sprintf(
                '\\multicolumn{%s}{=}{',
                colspans[irow, icol]
              ),
              ''
            ),
            ifelse(
              rowspans[irow, icol] > 1,
              sprintf(
                '\\multirow[t]{%s}{=}{%s%s%s}',
                rowspans[irow, icol],
                ifelse( irow %in% header_rows, '\\textbf{', '' ),
                ht[irow, icol],
                ifelse( irow %in% header_rows, '}', '' )
              ),
              sprintf(
                '%s%s%s',
                ifelse( irow %in% header_rows, '\\textbf{', '' ),
                ht[irow, icol],
                ifelse( irow %in% header_rows, '}', '' )
              )
            ),
            ifelse(
              colspans[irow, icol] > 1,
              '}',
              ''
            )
          )
        )
      } else if ( rowspans[irow, icol] == 0) {
        latex_row <- c(latex_row, '')
      }
    }

    # Add row content
    latex_row <- sprintf(
      '%s%s%s',
      paste(latex_row, collapse = ' & '),
      ifelse( irow < nrows, ' \\\\', ''),
      ifelse(
        irow > max(header_rows) & irow < nrows,
        ifelse(
          all( rowspans[irow+1,] != 0 ),
          ifelse(
            # Check that right side of rowspan is > 1 in pese tables
            pese &
              any( rowspans[irow,] > 1) &
              grepl(
                '^(FALSE)+(TRUE)+$',
                paste(rowspans[irow,] > 1, collapse = '')
              ),
            sprintf(
              ' \\cmidrule{1-%s}',
              which.max(rowspans[irow,] > 1) - 1
            ),
            ' \\midrule'
          ),
          ''
        ),
        ''
      )
    )

    # Store header latex
    if ( irow %in% header_rows ){

      # Add partial line break
      if ( irow != header_rows[length(header_rows)] ){
        latex_row <- sprintf(
          '%s \\cmidrule{%s-%s}',
          latex_row,
          which.max(rowspans[irow+1,]!=0),
          ncols
        )
      } else {
        latex_row <- sprintf(
          '%s \\midrule',
          latex_row
        )
      }
      latex <- c(
        latex,
        latex_row
      )

    } else {
      latex <- c(
        latex,
        latex_row
      )
    }
  }

  # Finalize table
  latex <- c(
    latex,
    '%@table_end@',
    '}'
  )

  if ( orientation == 'landscape') {
    latex <- c(
      latex,
      '\\end{landscape}'
    )
  }

  return (latex)

}
