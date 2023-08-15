library(ggplot2)

# example(s) from: man/format_continuous_cognigen.Rd


min <- -5000
max <- 5000
set.seed(123)

data <- data.frame(
  x = runif(1000, min, max),
  y = runif(1000, min, max)
)

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  scale_y_continuous(labels = format_continuous_cognigen)

min <- 0
max.x <- 1000
max.y <- 3
set.seed(123)

data <- data.frame(
  x = runif(1000, min, max.x),
  y = rlnorm(1000, min, max.y)
)

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  theme_cognigen_grid(minor.x = TRUE, minor.y = TRUE) +
  scale_y_log10(minor_breaks = minor_breaks_log, labels = format_continuous_cognigen)


# example(s) from: man/geom_barcount.Rd


p <- ggplot(mpg)
p +
  aes(x = class) +
  geom_bar() +
  geom_barcount()

# Map class to y instead to flip the orientation
p +
  aes(y = class) +
  geom_bar() +
  geom_barcount()

# For stacked position
p <- ggplot(diamonds, aes(color, fill = cut))
p +
  geom_bar(position = 'stack') +
  geom_barcount()
p +
  geom_bar(position = 'stack') +
  geom_barcount(overall.stack = FALSE)

# For dodged position
p +
  geom_bar(position = 'dodge') +
  geom_barcount(position = position_dodge(width = 0.9))

# For fill position
p +
  geom_bar(position = 'fill') +
  geom_barcount(position = position_fill())

# For fillpercent position
p +
  geom_bar(position = 'fillpercent') +
  geom_barcount(position = position_fillpercent()) +
  ylab('count (%)')



# example(s) from: man/geom_boxcount.Rd


p <- ggplot(mpg, aes(class, hwy))
p +
  geom_boxplot2() +
  geom_boxcount()

# For display on log axis scale, use the scale_y_continuous function
# Using coord_trans(y ='log10') would display the counts at the wrong place
p +
  geom_boxplot2() +
  geom_boxcount() +
  scale_y_continuous(trans = 'log10')


# example(s) from: man/geom_boxplot2.Rd


p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot2()
p + geom_boxplot2(outlier.position = 'identity', coef = 90)
# Orientation follows the discrete axis
ggplot(mpg, aes(hwy, class)) + geom_boxplot2()

p + geom_boxplot2(notch = TRUE)
p + geom_boxplot2(whisker.cap = TRUE)
p + geom_boxplot2(varwidth = TRUE)
p + geom_boxplot2(fill = "white", colour = "#3366FF")

# Boxplots are automatically dodged when any aesthetic is a factor
p + geom_boxplot2(aes(colour = drv))

# You can also use boxplots with continuous x, as long as you supply
# a grouping variable. cut_width is particularly useful
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot2()
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot2(aes(group = cut_width(carat, 0.25)))
# Adjust the transparency of outliers using outlier.alpha
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot2(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)

# It's possible to draw a boxplot with your own computations if you
# use stat = "identity":
y <- rnorm(100)
df <- data.frame(
  x = 1,
  y0 = min(y),
  y25 = quantile(y, 0.25),
  y50 = median(y),
  y75 = quantile(y, 0.75),
  y100 = max(y)
)
ggplot(df, aes(x)) +
  geom_boxplot2(
   aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
   stat = "identity"
 )


# example(s) from: man/geom_crossbar2.Rd


# Create a simple example dataset
df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(df, aes(trt, resp, colour = group))
p + geom_crossbar2(aes(ymin = lower, ymax = upper), width = 0.2)


# example(s) from: man/geom_histcount.Rd


p <- ggplot(diamonds)

# Histogram for continuous variable count
p +
  aes(x = price) +
  geom_histogram() +
  geom_histcount()

# Map class to y instead to flip the orientation
p +
  aes(y = price) +
  geom_histogram() +
  geom_histcount()

# Histogram with a fill aesthetic
p +
  aes(x = price, fill = clarity) +
  geom_histogram() +
  geom_histcount()

# Histogram for continuous variable density
p +
  aes(x = price) +
  geom_histogram(aes(y = after_stat(density)), stat = 'bin2', bins = 15) +
  geom_histcount(aes(y = after_stat(density), label = after_stat(density_label)), bins = 15)

# Histogram for continuous variable percentage using the bin2 stat
p +
  aes(x = price) +
  geom_histogram(aes(y = after_stat(percent)), stat = 'bin2', bins = 15) +
  geom_histcount(aes(y = after_stat(percent), label = after_stat(percent_label)), bins = 15) +
  ylab('percent (%)')


# example(s) from: man/get_device_size.Rd


dims <- get_device_size(nplots = 4,
                        units = 'in',
                        dpi = 300)

plot <- ggplot(mpg, aes(class, hwy)) +
  geom_point()

ggsave(filename = 'plot.png',
       plot = plot,
       path = tempdir(),
       width = dims['width'],
       height = dims['height'],
       units = 'in',
       dpi = 300)


# example(s) from: man/get_style_colors.Rd

get_style_colors()
get_style_colors(element = 'bar')
get_style_colors(style = cognigen_purple_style())

# example(s) from: man/ggsave_multiple.Rd


library(ggplot2)
library(ggforce)

g1 <- ggplot(data = diamonds) +
  aes(x = carat, y = price) +
    geom_point()

g2 <- ggplot(data = diamonds) +
  aes(x = carat, y = price) +
  geom_point() +
  facet_wrap_paginate(vars(clarity), nrow = 2, ncol = 2, page = NULL)

g3 <- ggplot(diamonds) +
  aes(carat, price) +
  geom_point(alpha = 0.2) +
  facet_grid_paginate(color ~ cut, ncol = 3, nrow = 3, page = NULL)

g4 <- ggplot(data = diamonds) +
  aes(x = carat, y = price, color = clarity) +
  geom_point(alpha = 0.2)

g5 <- ggplot(data = diamonds) +
  aes(x = carat, y = price, color = cut) +
  geom_point(alpha = 0.2)

gs <- ggpubr::ggarrange(g1, g4, g5, nrow = 2, ncol = 2)

ggsave_multiple(
  filenames = c('plot_g1.png', 'plot_g2.png', 'plot_g3.png', 'plot_gs.png'),
  plots = list(g1, g2, g3, gs),
  path = tempdir()
)




# example(s) from: man/make_forestplot.Rd


# Use expo dataset provided in the ggcognigen package

gmrs <- expo %>%
  make_gmr_data(
    x_var = c(CMAXSS, AUCSS, CMINSS),
    id_var = ID,
    by = DOSE,
    covariates = c(AGE, WTKG, BMI, SEXF, RFCAT, CPCAT),
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

make_forestplot(
  data = gmrs %>% dplyr::filter(x_var == "AUCSS"),
  x = gmr,
  xmin = gmr_lo,
  xmax = gmr_hi,
  y = value,
  color = by,
  label = gmr_n_label,
  facet = y_label,
  vline_primary = 1,
  vline_secondary = c(0.8, 1.25),
  xlb = 'Geometric Mean Ratio [90% confidence interval]',
  title = expression(C[trough]~'(nmol/L)'),
  fatten = 2,
  small_font = TRUE
) +
  scale_discrete_cognigen(n = 4, geom = 'point')



# example(s) from: man/make_gmr_data.Rd


# Use expo dataset provided in the ggcognigen package

make_gmr_data(
  data = expo,
  x_var = c('CMAXSS', 'AUCSS', 'CMINSS'),
  id_var = 'ID',
  by = NULL,
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

# same example with tidyverse syntax
expo %>%
  make_gmr_data(
    x_var = c(CMAXSS, AUCSS, CMINSS),
    id_var = ID,
    by = NULL,
    covariates = c(AGE, WTKG, BMI, SEXF, RFCAT, CPCAT),
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


# example(s) from: man/make_gmr_table.Rd


# Use expo dataset provided in the ggcognigen package

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
  ),
  n_label = 'patient'
)


# example(s) from: man/scale_continuous_cognigen.Rd


df <- data.frame(
  value = seq(1, 100),
  x = runif(100),
  y = runif(100),
  z = rnorm(100)
)

# color
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(color = z), pch = 19) +
  scale_continuous_cognigen()

# fill
ggplot(df, aes(x = value, y = y)) +
  geom_bar(aes(fill = z), stat = "identity") +
  scale_continuous_cognigen()


# example(s) from: man/scale_discrete_cognigen.Rd


# Use the xydata dataset provided in the ggcognigen package

xydata$DOSE <- as.factor(xydata$DOSE)

# scatter plot
ggplot(data = xydata) +
  aes(x = TIME, y = CONCENTRATION, colour = DOSE, fill = DOSE, shape = DOSE) +
  geom_point() +
  scale_discrete_cognigen(n = 10, geom = 'point')

ggplot(data = xydata) +
  aes(x = TIME, y = CONCENTRATION) +
  geom_point() +
  scale_discrete_cognigen(n = 10, geom = 'point')

ggplot(data = xydata) +
  aes(x = TIME, y = CONCENTRATION, colour = DOSE, fill = DOSE, shape = DOSE) +
  geom_point() +
  theme_cognigen() +
  scale_discrete_cognigen(style = cognigen_purple_style(), n = 10, geom = 'point')

# Lineplot
linedata <- subset(xydata, REP == 1)
ggplot(data = linedata) +
  aes(x = TIME, y = CONCENTRATION, group = DOSE) +
  geom_line()

# Barchart
ggplot(data = bardata) +
  aes(x = STUDY, y = COUNT, fill = GROUP) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 1) +
  scale_discrete_cognigen(n = 10, geom = 'bar')

ggplot(data = bardata) +
  aes(x = STUDY, y = COUNT, fill = GROUP) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 1) +
  theme_cognigen() +
  scale_discrete_cognigen(style = cognigen_purple_style(), n = 10, geom = 'bar')

# Boxplot
ggplot(data = boxdata) +
  aes(x = GROUP, y = CONTINUOUS, colour = CATEGORICAL) +
  geom_boxplot2(
    notch = TRUE,
    coef = 90,
    fill = 'white',
    #outlier.colour = NA,
    outlier.jitter = TRUE,
    outlier.size = 3,
    position = position_dodge(width = 0.9),
    na.rm = TRUE
  ) +
  theme_cognigen() +
  scale_discrete_cognigen(
    style = cognigen_purple_style(),
    n = 10,
    geom = 'boxplot'
  )

ggplot(data = boxdata) +
  aes(x = GROUP, y = CONTINUOUS, colour = CATEGORICAL) +
  geom_boxplot2(
    notch = TRUE,
    coef = 90,
    fill = 'white',
    outlier.jitter = TRUE,
    outlier.size = 3,
    position = position_dodge(width = 0.9),
    na.rm = TRUE
  ) +
  theme_cognigen() +
  scale_discrete_cognigen(10)

# Histogram



# example(s) from: man/set_default_style.Rd


set_default_style()

ggplot(mpg, aes(class, hwy)) +
  geom_point()

set_default_style(style = 'ggplot2')
ggplot(mpg, aes(class, hwy)) +
  geom_point()



# example(s) from: man/stat_bin2.Rd


# Count
ggplot(diamonds, aes(carat)) +
  geom_histogram()
# Percent
ggplot(diamonds, aes(carat)) +
  geom_histogram(aes(y = after_stat(percent)), stat = 'bin2')


# example(s) from: man/stat_count2.Rd


# Count
ggplot(diamonds, aes(clarity)) +
  geom_bar()
# Percent
ggplot(diamonds, aes(clarity)) +
  geom_bar(aes(y = after_stat(percent)), stat = 'count2')



# example(s) from: man/theme_cognigen.Rd


p <- ggplot(mpg, aes(class, hwy)) +
  geom_point()

p + theme_cognigen()

p + theme_cognigen_grid()


