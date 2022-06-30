#' Simulated pharmacokinetic data for scatter and line plots
#'
#' @format A data frame with 720 rows and 5 variables:
#' \describe{
#'   \item{REP}{A simulation replicate index}
#'   \item{TIME}{Time since dose}
#'   \item{CONCENTRATION}{Drug concentration}
#'   \item{GROUP}{Dose group for stratification}
#'   \item{DOSE}{Dose amount for stratification}
#' }
"xydata"

#' Simulated data for barcharts
#'
#' @format A data frame with 50 rows and 4 variables:
#' \describe{
#'   \item{STUDY}{Study number}
#'   \item{COUNT}{Count variable}
#'   \item{GROUP}{Group variable for stratification}
#'   \item{DOSE}{Dose amoung for stratification}
#' }
"bardata"

#' Simulated data for box and whisker plots
#'
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{CONTINUOUS}{Continuous variable}
#'   \item{EXP}{Exponentiated values of CONTINUOUS}
#'   \item{CATEGORICAL}{Categorical variable}
#'   \item{GROUP}{Group variable for stratification}
#'   \item{SPLIT}{Another group variable for stratification}
#'   \item{DOSE}{Dose amoung for stratification}
#' }
"boxdata"

#' Simulated data for histograms
#'
#' @format A data frame with 1000 rows and 3 variables:
#' \describe{
#'   \item{RANDOM}{Random continuous variable}
#'   \item{GROUP}{Group variable for stratification}
#'   \item{DOSE}{Dose amoung for stratification}
#' }
"histdata"

#' Model-prediction exposures
#'
#' @format A data frame with 2436 rows and 17 variables, providing the exposures
#' for 812 individuals at 3 different dose levels
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{DOSE}{Dose (mg)}
#'   \item{AGE}{Age (y)}
#'   \item{WTKG}{Body weight (kg)}
#'   \item{BMI}{Body mass index (kg/m2)}
#'   \item{SEXF}{Sex}
#'   \item{RFCAT}{Renal function category}
#'   \item{CPCAT}{Child-Pugh category}
#'   \item{HFM}{Meal status}
#'   \item{CL}{Elimination clearance (L/h)}
#'   \item{V}{Volume of distribution (L)}
#'   \item{KA}{First-order absorption rate (1/h)}
#'   \item{F1}{Relative bioavailability (-)}
#'   \item{AUCSS}{Steady-state area under the curve (nmol*h/L)}
#'   \item{TMAXSS}{Steady-state time to maximum concentration (h)}
#'   \item{CMAXSS}{Steady-state maximum concentration (nmol/L)}
#'   \item{CMINSS}{Steady-state trough concentration (nmol/L)}
#' }
"expo"

