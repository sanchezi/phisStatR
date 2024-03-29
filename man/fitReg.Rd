% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelisation.R
\name{fitReg}
\alias{fitReg}
\title{a function to model the phyllocron using a linear regression}
\usage{
fitReg(datain, trait, xtime, myylim, reference, myxlab)
}
\arguments{
\item{datain}{input dataframe (wide format, as many columns as variables)}

\item{trait}{variable to modelize (example visible_leaf, F_visible)}

\item{xtime}{character, name of thermal time column in the input dataset (example thermaltime)}

\item{myylim}{maximum value of y-axis for graphic}

\item{reference}{character, a column name indicating an unique identifiant of plant
(example: Ref == concatenation of experimentAlias-Line-Position-scenario)}

\item{myxlab}{character, a x label for the output graphics}
}
\value{
a list of two elements and the graphics of the regression

1: a dataframe of the predictions of the slope for each plant (phyllocron)

2: a list of fitted models for each plant for diagnostics purposes
}
\description{
this function models each plant of a dataset using a regression
to extract the slope (i.e. phyllocron: the rate of leaf appearance)
}
\details{
the input dataset must contain a thermalTime column (numeric data) and a Ref column (unique id)
}
\examples{
\donttest{
# Take a while...
 resu<-fitReg(datain=plant2,trait="F_visible",xtime="thermalTime",
              myylim=22,reference="Ref",
              myxlab="Thermal Time degD")
 str(resu)
}
}
