% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesianSTmodel.R
\name{fitCARBayesST}
\alias{fitCARBayesST}
\title{fitCARBayesST}
\usage{
fitCARBayesST(datain, xvar, trait, k = NULL, graphDist, burnin = 500,
  n.sample = 1500, formulaModel, typeModel = "anova", verbose)
}
\arguments{
\item{datain}{input dataframe of parameters}

\item{xvar}{character, time variable (ex: thermalTime)}

\item{trait}{character, parameter of interest (ex: plantHeight)}

\item{k}{numeric, number of nearest neighbours to be returned}

\item{graphDist}{logical, display distance graph}

\item{burnin}{The number of MCMC samples to discard as the burn-in period}

\item{n.sample}{The number of MCMC samples to generate}

\item{formulaModel}{A formula for the covariate part of the model using the syntax of the lm() function. see  \code{\link[=ST.CARanova]{ST.CARanova}} help}

\item{typeModel}{character, choice of the model, \code{anova}, \code{linear} or \code{ar}}

\item{verbose}{logical FALSE by default, if TRUE display information about the progress}
}
\value{
a list with a spatio-temporal object (CARBayesST) and a dataframe of the formated data
}
\description{
a function to model curves using a spatio-temporal bayesian modelling using \code{\link[=CARBayesST]{CARBayesST}} library
}
\details{
the input dataset must contain Position,Line,Ref,scenario,genotypeAlias columns. The function is not generic and needs 
         specific columns names in the input data set. Please have a look of the struture of the data set used in the example.
}
\examples{
\donttest{
 library(phisStatR)
 mydata<-plant1
 mydata<-filter(mydata,!is.na(mydata$thermalTime))
 str(mydata)
 test<-fitCARBayesST(datain=mydata,xvar="thermalTime",trait="plantHeight",k=2,
     graphDist=TRUE,burnin=10,n.sample=110,
     formulaModel=as.formula(plantHeight~scenario+genotypeAlias),
     typeModel="anova",verbose=FALSE)
}
}
\seealso{
\code{\link[=CARBayesST]{CARBayesST}}, \code{\link[=ST.CARanova]{ST.CARanova}} and \code{\link[=spdep]{spdep}}
}
