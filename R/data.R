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
