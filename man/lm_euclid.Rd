% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{lm_euclid}
\alias{lm_euclid}
\title{Fit Euclidean model}
\usage{
lm_euclid(formula, data, fixed_effects = NULL, vcov = "hetero", ...)
}
\arguments{
\item{formula}{A formula indicating lhs and rhs vars, e.g. Y ~ A + B + C}

\item{data}{A \code{data.frame}.}

\item{fixed_effects}{optional RHS formula}

\item{...}{other arguments passed to fixest::feols}
}
\value{
A  \code{list}
}
\description{
Fit Euclidean model
}
\examples{
library(tidyverse)
library(DeclareDesign)
data(covid_policy_evaluations)
M <- lm_euclid(rating ~ stringency + universal,
  data = covid_policy_evaluations,
  fixed_effects = "ID")
M
model <- lm_euclid(rating ~ universal + stringency  + severity,
   data = covid_policy_evaluations,
   fixed_effects = "ID")$model
}
