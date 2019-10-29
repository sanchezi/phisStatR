#-------------------------------------------------------------------------------
# Program: smmothingSplinesModel.R
# Objective: modelling of curves according to smoothing splines
#            require gss package
# Author: I.Sanchez
# Creation: 28/07/2016
# Update: 29/10/2019
#-------------------------------------------------------------------------------

##' a function to model curves using smoothing splines anova using \code{gss} library
##' @description this function models each curve of genotype using smoothing
##' splines anova
##' @param datain input dataframe
##' @param trait character, trait of interest to model (example biovolume, PH ...)
##' @param loopId a column name that contains ident of Genotype-Scenario
##' @details the input dataframe must contain the following columns: the trait to model,
##' the ident of Genotype-Scenario, thermalTime, repetition columns
##' 
##' @details Each time course is modelled by a nonparametric smoothing spline. This is a piecewise cubic polynomial
##' (Eubank, 1999). Then a functional ANOVA decomposition (Gu, 2014) of all the fitted splines for each
##' genotype by environmental treatment combination is realised, by taking into account the replicate effect and
##' a temporal functional effect. The smoothing spline fitting and the functional ANOVA decompositions are be
##' performed with the gss R package. 
##'
##' @return a list containing 2 objects
##' \describe{
##' \item{}{a list of each output of \code{\link[=ssanova]{ssanova}}}
##' \item{}{a dataframe of kullback-Leibler projection}
##' }
##'
##' @seealso \code{\link[=project.ssanova]{project.ssanova}}, \code{\link[=ssanova]{ssanova}}
##' 
##' @examples
##' \donttest{
##'  fm1<-fitGSS(datain=plant1,trait="biovolume")
##' }
##' @export
fitGSS<-function(datain,trait,loopId){
  tmpdata<-as.data.frame(datain)
  tmpdata<-dplyr::select_(tmpdata,.dots=c(loopId,"thermalTime","repetition",trait))
  fm<-list()
  resu<-NULL
  genosceId<-unique(tmpdata[,loopId])
  for(i in 1:length(genosceId)){
    tmp<-na.omit(tmpdata[tmpdata[,loopId]==genosceId[i],])
    fm[[i]]<-gss::ssanova(as.formula(paste0(trait,"~repetition + thermalTime + repetition:thermalTime")),data=tmp,seed=1234)
    tpproj<-cbind.data.frame(genosceId[i],t(unlist(gss::project(fm[[i]],inc=c("thermalTime","repetition")))))
    names(tpproj)[1]<-loopId
    resu<-rbind.data.frame(resu,tpproj)
  }
  result <- list(fm=fm,projKL=resu)
  class(result) = c("phisStatR")
  return(invisible(result))
}

#' a function for gss analysis description
#' @param object a dataframe to describe from fitGSS() function
#' @param threshold numeric, a threshold for Kullback-Leibler projection
#'
#' @seealso \code{\link[=ssanova]{ssanova}}
#' @details the input object is the 2nd element of a fitGSS result, a dataframe with the Kullback-Leibler projection
#'          colnames of this dataframe are Genosce, ratio, kl, check. The outlier curves are identified with the Kullback-Lleiber 
#'          distance higher than a given threshold, see (Gu, 2014). Final identification of outlier is done by an operator over
#'          genotypes when the test is significant.
#' @return a description
#'
#' @examples
#' \donttest{
#'  fm1<-fitGSS(datain=plant1,trait="biovolume")
#'  printGSS(object=fm1,threshold=0.05)
#' }
#' @export
printGSS<-function(object,threshold){
  tmp<-as.data.frame(object)
  if (!(is.null(threshold)))  tmp<-filter(tmp,ratio > threshold)
  return(tmp)
}
