#-------------------------------------------------------------------------------
# Program: geneticModels.R
# Objective: functions for genetics
# Author: I.Sanchez
# Creation: 09/09/2016
# Update: 22/03/2018
#-------------------------------------------------------------------------------

##' a function to modelize a trait in different trials taking into account the
##' position in the greenhouse (spatial correlation)
##' @description this function
##' @param datain input dataframe of parameters
##' @param trait character, trait of interest to model (example biovolume24, PH24 ...)
##' @details This function needs in input a dataframe with the following description columns
##' \describe{
##' \item{Manip}{Id of Manip column}
##' \item{Genotype}{Genotype column}
##' \item{Line}{line column in the greenhouse}
##' \item{Position}{position column in the greenhouse}
##' \item{Repsce}{combination of Repetition and Scenario}
##' }
##' and require asreml library
##' @return a class asreml object
##'
##' @importFrom dplyr arrange
##'
##' @examples
##' # not run
##' # library(asreml)
##' # df1<-fmGlobal(datain=,trait="biovolume24")
##' @export
fmGlobal<-function(datain,trait){
  datain<-as.data.frame(datain)
  datain<-arrange(datain,Position,Line)
  tp<-asreml(fixed=as.formula(paste(trait,"~ Manip",sep="")),
                random=~Genotype + at(Manip):Position +
                        at(Manip):Line + at(Manip):Repsce,
                rcov=~at(Manip):(ar1(Line):ar1(Position)),
                na.method.X="include",data=datain,maxiter=35)
  return(tp)
}
