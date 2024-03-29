% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{diagnosticResiduals}
\alias{diagnosticResiduals}
\title{a function for representing diagnostic graphics for each trait}
\usage{
diagnosticResiduals(datain, xfitted, yresidual)
}
\arguments{
\item{datain}{a dataframe to explore}

\item{xfitted}{character, name of the column of fitted values}

\item{yresidual}{character, name of the column of residual values}
}
\value{
a graphic of diagnostic for linear model
}
\description{
a function for representing diagnostic graphics for each trait
}
\examples{
\donttest{
 diagnosticResiduals(datain,xfitted,yresidual)
}
}
