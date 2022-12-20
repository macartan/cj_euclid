#' Covid freedoms conjoint data
#'
#' Data from "Trading Liberties: Estimating policy ideal points and indifference curves from conjoint data”
#' Felix Hartmann, Macartan Humphreys, Ferdinand Geissler, Heike Klüver, and Johannes Giesecke
#'
#' @docType data
#'
#' @usage data(covid_policy_evaluations)
#'
#' @format
#' A data frame with 41480  rows and 12 columns:
#' \describe{
#'   \item{ID}{Anonymized unique subject ID}
#'   \item{round}{First or second round}
#'   \item{vignette}{First or second displayed vignette}
#'   \item{vaccinated}{Binary: whether vaccinated or not}
#'   \item{party.id}{}
#'   \item{severity}{}
#'   \item{stringency}{}
#'   \item{universal}{}
#'   \item{rating}{}
#'   \item{choice}{}
#'   \item{trust}{}
#'   \item{vaccination_probability}{Probability of getting vaccinated}
#' }
#'
#' @keywords datasets
#'
#' @references Hartman et al. (2023)
#'
#'
"covid_policy_evaluations"
