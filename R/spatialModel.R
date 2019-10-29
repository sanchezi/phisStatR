#-------------------------------------------------------------------------------
# Program: spatialModel.R
# Objective: modelling of curves according to a spatial model
#            require SpATS package
# Author: I.Sanchez
# Creation: 04/05/2018
# Update: 29/10/2019
#-------------------------------------------------------------------------------

#' fitSpATS
#'
#' a function to model curves using a spatial P-Spline modelling using \code{\link[=SpATS]{SpATS}} library
#' @param datain input dataframe of parameters
#' @param trait character, parameter of interest (ex: Biomass24)
#' @param genotypeId character, column name of genotype alias
#' @param rowId character, column name of the row id in the lattice (greenhouse or field)
#' @param colId character, column name of the column id in the lattice (greenhouse or field)
#' @param typeModel character, choice of the spatial P-Spline model, \code{anova} or \code{sap}
#' @param genotype.as.random logical. If TRUE, the genotype is included as random effect in the model. The default is FALSE.
#' @param nseg numerical vector of length 2 containing the number of segments for each marginal (strictly nseg - 1 is the
#'             number of internal knots in the domain of the covariate).
#'             Atomic values are also valid, being recycled. Default set to c(14,30)
#' @param verbose logical FALSE by default, if TRUE display information about the progress
#' @details the input dataset must contain Position,Line,Ref,scenario,genotypeAlias columns
#' @seealso \code{\link[=SpATS]{SpATS}}, \code{\link[=PSANOVA]{PSANOVA}} and \code{\link[=SAP]{SAP}}
#' @importFrom SpATS SpATS PSANOVA SAP
#'
#' @return a SpATS object
#' @examples
#' \donttest{
#'  library(phisStatR)
#'  mydata<-plant4
#'  test<-fitSpATS(datain=mydata,trait="Biomass24",genotypeId="genotypeAlias",rowId="Line",
#'        colId="Position",typeModel="anova",genotype.as.random=FALSE,nseg=c(14,30),verbose)
#' }
#' @export
fitSpATS<-function(datain,trait,genotypeId,rowId,colId,typeModel="anova",genotype.as.random=FALSE,nseg=c(14,30),verbose){
  datain$R<-as.factor(datain[,rowId])
  datain$C<-as.factor(datain[,colId])
  
  # I need to have these column names in the model
  colnames(datain)[which(colnames(datain)==rowId)] <-"Line"
  colnames(datain)[which(colnames(datain)==colId)] <-"Position"

  # spatial P-Spline ANOVA modelling
  if (typeModel=="anova"){
    model<-SpATS(response = trait, spatial = ~ PSANOVA(Line, Position, nseg = nseg),
                 genotype = genotypeId, random = ~ C + R, genotype.as.random = genotype.as.random,
                 data = datain, control =  list(tolerance = 1e-03))
  } else if (typeModel=="sap"){
    model<-SpATS(response = trait, spatial = ~ SAP(Line, Position, nseg = nseg),
                 genotype = genotypeId, random = ~ C + R, genotype.as.random = genotype.as.random,
                 data = datain, control =  list(tolerance = 1e-03))
  }

  return(model)
}

