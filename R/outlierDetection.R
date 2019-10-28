#-------------------------------------------------------------------------------
# Program: outlierDetection.R
# Objective: functions for outliers detection according to biological meaning
# Author: I.Sanchez
# Creation: 05/09/2016
# Update: 02/11/2016
#-------------------------------------------------------------------------------

##' a function for several outlier criteria
##' @description this function calculates outlier criteria on plants for each parameter
##' @param datain input dataframe of parameters
##' @param typeD type of datain dataframe (1==wide, 2==long)
##' @param residin input dataframe of residuals
##' @param typeR type of residin dataframe (1==wide, 2==long)
##' @param trait character, trait of interest to model (example biovolume24, PH24 ...)
##' @param resRawName character, names of the raw residual in datain
##' @param resStdName character, names of the standardized residual in datain
##' @param threshold, numeric threshold for the normal quantile in raw criteria
##' @details This function needs in input a dataframe with residuals extracted from
##' a mixed linear model (for instance using asreml or nlme libraries) and an another dataframe
##' with the estimated parameters (biovolume, plantHeight, leafArea etc...). Several criteria
##' will be calculated using different types of residuals. The 2 input dataframe must contain
##' the following columns names: "experimentAlias","Line" and "Position".
##' \describe{
##' \item{raw  and quartile criteria}{use raw residuals}
##' \item{influence criterion}{uses standardized residuals}
##' }
##' The function must be executed for each parameter of interest: biovolume, plantHeight and phy.
##' Each criteria will be used according with some rules:
##' \describe{
##' \item{Small plant}{biovolume and phy}
##' \item{Big plants}{biovolume and plantHeight}
##' }
##' @return a dataframe with columns identifiying criteria used to detect outlier plants
##' with 1==plant OK - 0==plant KO to suppress
##' \describe{
##' \item{critraw}{raw criterion, critci: quartiles criterion}
##' \item{critinfl}{influence criterion with standardized residuals}
##' }
##'
##' @importFrom stats IQR qnorm quantile sd lm na.omit as.formula
##'
##' @examples
##' # Not run
##' # dt1<-outlierCriteria()
##'
##' @export
outlierCriteria<-function(datain,typeD,residin,typeR,trait,resRawName,resStdName,threshold){
  # Create a dataframe with raw data, fitted and residuals for each paramater
  datain<-as.data.frame(datain)
  if (typeD==1){ # wide format column==differents traits
    tmp1<-dplyr::select_(datain,"Ref","Genosce","Line","Position","genotypeAlias","experimentAlias",
                         "scenario","repetition","potAlias",trait)
  } else if (typeD==2){ # long format 1 column Trait, 1 column value
    tmp1<-dplyr::filter(datain, Trait==trait)
    tmp1<-dplyr::rename_(tmp1,trait="Trait")
  }
  residin<-as.data.frame(residin)
  if (typeR==1){ # wide format column==differents traits
    tmp2<-residin
  } else if (typeR==2){ # long format 1 column Trait, 1 column value
    tmp2<-dplyr::filter(residin, Trait==trait)
  }
  # merge datain and residin by experimentAlias, line and position (unique key)
  tmp<-dplyr::left_join(tmp1,tmp2,by=c("experimentAlias","Line","Position"))
  # mean and sd of residuals
  tmp<-dplyr::mutate(tmp, mean.res=mean(tmp[,resRawName],na.rm=TRUE),
                     sd.res=sd(tmp[,resRawName],na.rm=TRUE))
  #--- raw cleaning
  tmp<-dplyr::mutate(tmp, lower.res=mean.res - sd.res*qnorm(threshold),
                     upper.res=mean.res + sd.res*qnorm(threshold))
  tmp<-dplyr::mutate(tmp,lower.critraw=ifelse(tmp[,resRawName]-tmp[,"lower.res"]>0,yes=1,no=0),
                     upper.critraw=ifelse(tmp[,resRawName]-tmp[,"upper.res"]<0,yes=1,no=0))
  #--- Quantiles cleaning
  tmp<-dplyr::mutate(tmp, Q1.res=quantile(tmp[,resRawName],probs=0.25,na.rm=TRUE)-1.5*IQR(tmp[,resRawName],na.rm=TRUE),
                     Q3.res=quantile(tmp[,resRawName],probs=0.75,na.rm=TRUE)+1.5*IQR(tmp[,resRawName],na.rm=TRUE))
  tmp<-dplyr::mutate(tmp,lower.critci=ifelse(tmp[,resRawName]-tmp[,"Q1.res"]>0,yes=1,no=0),
                     upper.critci=ifelse(tmp[,resRawName]-tmp[,"Q3.res"]<0,yes=1,no=0))
  #--- influence with standardized residuals
  tmp<-dplyr::mutate(tmp,lower.critinfl=ifelse(tmp[,resStdName]>=-2,yes=1,no=0),
                     upper.critinfl=ifelse(tmp[,resStdName]<=2,yes=1,no=0))
  # output
  return(tmp)
}
