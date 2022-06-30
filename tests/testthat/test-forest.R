# example(s) from: man/make_gmr_data.Rd

gmrs <- make_gmr_data(
  x_var = c('CMAXSS', 'AUCSS', 'CMINSS'),
  data = expo,
  id_var = 'ID',
  by = 'DOSE',
  covariates = c('AGE', 'WTKG', 'BMI', 'SEXF', 'RFCAT', 'CPCAT'),
  labels = c(
    expression('Age'~'(y)'),
    expression('Body'~'Weight'~'(kg)'),
    expression('Body'~'Mass'~'Index'~'(kg/'*m^2*')'),
    expression('Sex'),
    expression('Renal'~'Function'),
    expression('Hepatic'~'Function')
  ),
  ref_levels = c(2, 3, 2, 1, 1, 1),
  digits = 3,
  silent = TRUE
)

test_that(
  'calculate GMRs from expo dataset',
  {
    expect_s3_class(
      make_gmr_data(
        x_var = c('CMAXSS', 'AUCSS', 'CMINSS'),
        data = expo,
        id_var = 'ID',
        by = 'DOSE',
        covariates = c('AGE', 'WTKG', 'BMI', 'SEXF', 'RFCAT', 'CPCAT'),
        labels = c(
          expression('Age'~'(y)'),
          expression('Body'~'Weight'~'(kg)'),
          expression('Body'~'Mass'~'Index'~'(kg/'*m^2*')'),
          expression('Sex'),
          expression('Renal'~'Function'),
          expression('Hepatic'~'Function')
        ),
        ref_levels = c(2, 3, 2, 1, 1, 1),
        digits = 3,
        silent = TRUE
      )
      , 'data.frame'
    )
  }
)

test_that(
  'check variables in make_gmr_table',
  {
    expect_identical(
      names(gmrs),
      c('x_var', 'y_var', 'y_label', 'by', 'value', 'n', 'reference', 'gm',
        'gm_lo', 'gm_hi', 'gm_label', 'gmr', 'gmr_lo', 'gmr_hi',
        'gmr_label', 'gmr_n_label')
    )
  }
)


test_that(
  'check type of variable in make_grm_table',
  {
    expect_equivalent(
      sapply(gmrs, class),
      c('character', 'factor', 'factor', 'factor', 'factor', 'integer', 'logical',
        'numeric', 'numeric', 'numeric', 'character', 'numeric', 'numeric',
        'numeric', 'character', 'character')
    )
  }
)

test_that(
  'check relative values of GMs',
  {
    expect_true(
      all( gmrs$gm_lo <= gmrs$gm & gmrs$gm <= gmrs$gm_hi )
    )
  }
)

test_that(
  'check relative values of GMRs',
  {
    expect_true(
      all( gmrs$gmr_lo <= gmrs$gmr & gmrs$gmr <= gmrs$gmr_hi )
    )
  }
)

# example(s) from: man/make_gmr_table.Rd

test_that(
  'check expr2char',
  expect_type(
    expr2char(levels(gmrs[, 'y_label'])),
    'character'
  )
)

test_that(
  'check creation of files',
  {
    gmrs2 <- gmrs[, c('x_var', 'y_label', 'value', 'by', 'n', 'gm_label','gmr_label')]
    levels(gmrs2[, 'y_label']) <- expr2char(levels(gmrs2[, 'y_label']))

    make_gmr_table(
      data = gmrs2,
      file = c(
        file.path(tempdir(), 'gmtable.html'),
        file.path(tempdir(), 'gmtable.docx')
      ),
      format = c('html', 'word'),
      headers = c(
        'Exposure Measure',
        'Covariate',
        'Group',
        'Dose',
        'n',
        'Geometric Mean [90% CI]',
        'Geometric Mean Ratio [90% CI]'
      ),
      abbreviations = list(
        CI = 'confidence interval'
      )
    )

    expect_true(
      all(
        file.exists(
          file.path(tempdir(), 'gmtable.html'),
          file.path(tempdir(), 'gmtable.docx')
        )
      )
    )
  }
)

# example(s) from: man/make_forestplot/Rd
test_that(
  'create forest plot',
  expect_s3_class(
    make_forestplot(
      data = subset(gmrs, x_var == 'AUCSS'),
      y = 'value', x = 'gmr', xmin = 'gmr_lo', xmax = 'gmr_hi',
      label = 'gmr_n_label',
      facet = 'y_label',
      color = 'by',
      vline_primary = 1,
      vline_secondary = c(0.8, 1.25),
      xlb = 'Geometric Mean Ratio [90% confidence interval]',
      title = expression(C[trough]~'(nmol/L)'),
      fatten = 2,
      small_font = TRUE
    ),
    'ggplot'
  )
)
