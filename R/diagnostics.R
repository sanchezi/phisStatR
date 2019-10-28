#------------------------------------------------------------------
# Program: diagnostics.R
# Objective: graphical functions for diagnostics of linear models
# Author: I.Sanchez
# Creation: 28/07/2016
# Update: 22/03/2018
#------------------------------------------------------------------

#' a function for representing the residuals of a linear model
#' @param fitin is a list containing objects of class lm
#' @return 2 graphs in one page: a graph to check the homoscedasticity of residuals
#'         the second to check the normality of the residuals
#' @examples
#' \donttest{
#'  diagnosticLM(fitin=aListResult[[2]])
#' }
#' @export
diagnosticLM<-function(fitin){
  par(mfrow=c(2,2))
  for (i in 1:length(fitin)){
    plot(fitin[[i]],which=1,main=names(fitin)[i])
    plot(fitin[[i]],which=2)
  }
  par(mfrow=c(1,1))
}

#' a function for representing diagnostic graphics for each trait
#' @param datain a dataframe to explore
#' @param xfitted character, name of the column of fitted values
#' @param yresidual character, name of the column of residual values
#' @return a graphic of diagnostic for linear model
#' @importFrom ggplot2 ggplot geom_point geom_hline aes_string
#' @examples
#' \donttest{
#'  diagnosticResiduals(datain,xfitted,yresidual)
#' }
#' @export
#---------------------------------------------------
diagnosticResiduals<-function(datain,xfitted,yresidual){
  g<-ggplot(data=datain,ggplot2::aes_string(x=xfitted,y=yresidual)) +
         geom_point(size=1) +
         geom_hline(yintercept = 2,col="red") + geom_hline(yintercept = -2,col="red")
  print(g)
}
