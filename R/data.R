#' Number of Removals in Wenatchee
#'
#' A dataset containing how many fish were removed by origin, location and year (2014-2019)
#'
#' @format A data frame with 36 rows and 5 variables:
#' \describe{
#'   \item{Year}{Spawn year}
#'   \item{Source}{Where the removal happened}
#'   \item{Origin}{Hatchery or natural}
#'   \item{rem}{How many fish were removed}
#'   \item{Area}{Which area in the DABOM model does this correspond to}
#'   ...
#' }
#' @source Michael Hughes at WDFW
"removal_df"

#' Mean Thalweg CV
#'
#' A dataset containing the mean thalweg CV of each reach in the Wenatchee, by using all measurements across all years
#'
#' @format A data frame with 10 rows and 4 variables:
#' \describe{
#'   \item{Reach}{Reach label}
#'   \item{n_yrs}{Number of years measurements taken in that reach}
#'   \item{n_meas}{Number of distinct thalweg measurements taken in that reach}
#'   \item{MeanThalwegCV}{average CV of thalweg depth}
#'   ...
#' }
#' @source Michael Hughes at WDFW
"thlwg_summ"
