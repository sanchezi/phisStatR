#------------------------------------------------------------------
# Program: printExperiment.R
# Objective: print the description of an experiment
# Author: I.Sanchez
# Creation: 28/07/2016
# Update: 21/10/2016
#------------------------------------------------------------------

#' a function for experiment description
#' @param datain a dataframe to describe
#' @param manip an identifiant of an experiment (example ZA16)
#'
#' @details the input dataframe must contain the following columns: experimentAlias,
#' genotypeAlias, scenario, Repsce and Ref
#' @return a description
#'
#' @examples
#' \donttest{
#' # Not run
#'  printExperiment(datain=toto,manip="ZB12")
#' }
#' @export
printExperiment<-function(datain,manip){
  datain<-as.data.frame(datain)
  cat("Genotypes:",length(unique(as.character((datain[datain[,"experimentAlias"]==manip,"genotypeAlias"])))),"\n")
  print(unique(as.character((datain[datain[,"experimentAlias"]==manip,"genotypeAlias"]))))
  cat("Scenario:",length(unique(as.character((datain[datain[,"experimentAlias"]==manip,"scenario"])))),"\n")
  print(unique(as.character((datain[datain[,"experimentAlias"]==manip,"scenario"]))))
  cat("Repsce:",length(unique(as.character((datain[datain[,"experimentAlias"]==manip,"Repsce"])))),"\n")
  print(unique(as.character((datain[datain[,"experimentAlias"]==manip,"Repsce"]))))
  cat("Ref (number of plants):",length(unique(as.character((datain[datain[,"experimentAlias"]==manip,"Ref"])))),"\n")
}

