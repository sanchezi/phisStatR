#------------------------------------------------------------------
# Program: printExperiment.R
# Objective: print the description of an experiment
# Author: I.Sanchez
# Creation: 28/07/2016
# Update: 14/11/2019
#------------------------------------------------------------------

#' a function for experiment description
#' @param datain a dataframe to describe containing a unique experiment
#'
#' @details the input dataframe must contain the following columns: experimentAlias,
#' Genotype, scenario, coordinates and pot.
#' @return a description
#' @importFrom tidyr unite
#'
#' @examples
#' \donttest{
#'   printExperiment(datain=plant3)
#' }
#' @export
printExperiment<-function(datain){
  datain<-as.data.frame(datain)
  
  #------------------------------------------
  # renames columns if necessary:
  #------------------------------------------
  tmpname<-names(datain)
  tmpname[grep("^Line",tmpname,perl=TRUE)]<-"Line"
  tmpname[grep("^line",tmpname,perl=TRUE)]<-"Line"
  tmpname[grep("^x",tmpname,perl=TRUE)]<-"Line"
  tmpname[grep("^Pos",tmpname,perl=TRUE)]<-"Position"
  tmpname[grep("^pos",tmpname,perl=TRUE)]<-"Position"
  tmpname[grep("^y",tmpname,perl=TRUE)]<-"Position"
  tmpname[grep("^geno",tmpname,perl=TRUE)]<-"Genotype"
  tmpname[grep("^Geno",tmpname,perl=TRUE)]<-"Genotype"
  tmpname[grep("^pot",tmpname,perl=TRUE)]<-"Pot"
  tmpname[grep("^Pot",tmpname,perl=TRUE)]<-"Pot"
  tmpname[grep("^repe",tmpname,perl=TRUE)]<-"repetition"
  tmpname[grep("^Repe",tmpname,perl=TRUE)]<-"repetition"
  tmpname[grep("^scen",tmpname,perl=TRUE)]<-"scenario"
  tmpname[grep("^Scen",tmpname,perl=TRUE)]<-"scenario"
  tmpname[grep("^exp",tmpname,perl=TRUE)]<-"experiment"
  tmpname[grep("^Exp",tmpname,perl=TRUE)]<-"experiment"
  names(datain)<-tmpname
  #------------------------------------------
  
  datain<-unite(datain,col="repetsce",repetition,scenario,sep="-",remove=FALSE)
  
  if (grep("experiment",names(datain),perl=TRUE) > 0) {
    cat("Experiment:",unique(as.character((datain[,"experiment"]))),"\n")
  }
  
  if (grep("Genotype",names(datain),perl=TRUE) > 0) {
    cat("Genotypes:",length(unique(as.character((datain[,"Genotype"])))),"\n")
    print(unique(as.character((datain[,"Genotype"]))))
  }  
  
  if (grep("scenario",names(datain),perl=TRUE) > 0) {
    cat("Scenario:",length(unique(as.character((datain[,"scenario"])))),"\n")
    print(unique(as.character((datain[,"scenario"]))))
  }
  
  if (grep("repetsce",names(datain),perl=TRUE) > 0) {
    cat("Repetition-scenario:",length(unique(as.character((datain[,"repetsce"])))),"\n")
    print(unique(as.character((datain[,"repetsce"]))))
  }
  
  if (grep("Pot",names(datain),perl=TRUE) > 0) {
    cat("Pots (number of plants):",length(unique(as.character((datain[,"Pot"])))),"\n")
  }
  
  if (grep("Line",names(datain),perl=TRUE) > 0) {
    cat("Line:",length(unique(as.character((datain[,"Line"])))),"\n")
  }

  if (grep("Position",names(datain),perl=TRUE) > 0) {
    cat("Position:",length(unique(as.character((datain[,"Position"])))),"\n")
  }
  
}

