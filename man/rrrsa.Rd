% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrrsa.R
\docType{package}
\name{rrrsa}
\alias{rrrsa}
\alias{rrrsa-package}
\title{run Rational Speech Act (RSA) models}
\description{
rrrsa provides tools for running RSA models.
These tools are grouped below for basic RSA reasoning functionality
as well as handling data frames.
}
\details{
Reasoning

rrrsa's reasoning is broken down into model components made available to users.
Calculating informativity \code{\link{rsa.informativity}}, calculating
speaker utility \code{\link{rsa.utility}}, running one full RSA recursion
\code{\link{rsa.fullRecursion}}, running multiple recursions \code{\link{rsa.reason}}.

Data frames and tuning hypeparameters

rrrsa provides easy functionality for data in data frames with \code{\link{rsa.runDf}}
}
\references{
Frank & Goodman (2012); Goodman & Stuhmuller, (2014); Peloquin & Frank (2016)

\url{https://github.com/benpeloquin7/rrrsa}
}

