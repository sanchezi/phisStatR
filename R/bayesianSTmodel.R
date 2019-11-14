#-------------------------------------------------------------------------------
# Program: bayesianSTmodel.R
# Objective: modelling of curves according to a bayesian spatio-temporal model
#            require CARBayesST package
# Author: I.Sanchez
# Creation: 20/06/2017
# Update: 14/11/2019
#-------------------------------------------------------------------------------

#' fitCARBayesST
#'
#' a function to model curves using a spatio-temporal bayesian modelling using \code{\link[=CARBayesST]{CARBayesST}} library
#' @param datain input dataframe of parameters
#' @param xvar character, time variable (ex: thermalTime)
#' @param trait character, parameter of interest (ex: plantHeight)
#' @param k numeric, number of nearest neighbours to be returned
#' @param graphDist logical, display distance graph
#' @param burnin The number of MCMC samples to discard as the burn-in period
#' @param n.sample The number of MCMC samples to generate
#' @param formulaModel A formula for the covariate part of the model using the syntax of the lm() function. see  \code{\link[=ST.CARanova]{ST.CARanova}} help
#' @param typeModel character, choice of the model, \code{anova}, \code{linear} or \code{ar}
#' @param verbose logical FALSE by default, if TRUE display information about the progress
#' @details the input dataset must contain Position,Line,Ref,scenario,genotypeAlias columns. The function is not generic and needs 
#'          specific columns names in the input data set. Please have a look of the struture of the data set used in the example.
#' 
#' @seealso \code{\link[=CARBayesST]{CARBayesST}}, \code{\link[=ST.CARanova]{ST.CARanova}} and \code{\link[=spdep]{spdep}}
#' @importFrom dplyr arrange_ distinct_ distinct filter_ full_join group_by_ summarise_
#' @importFrom spdep knearneigh knn2nb dnearneigh make.sym.nb nb2listw nb2mat nbdists
#' @importFrom CARBayesST ST.CARanova ST.CARar ST.CARlinear
#'
#' @return a list with a spatio-temporal object (CARBayesST) and a dataframe of the formated data
#' @examples
#' \donttest{
#'  library(phisStatR)
#'  mydata<-plant1
#'  mydata<-filter(mydata,!is.na(mydata$thermalTime))
#'  str(mydata)
#'  test<-fitCARBayesST(datain=mydata,xvar="thermalTime",trait="plantHeight",k=2,
#'      graphDist=TRUE,burnin=10,n.sample=110,
#'      formulaModel=as.formula(plantHeight~scenario+genotypeAlias),
#'      typeModel="anova",verbose=FALSE)
#' }
#' @export
fitCARBayesST<-function(datain,xvar,trait,k=NULL,graphDist,burnin=500,
                        n.sample=1500,formulaModel,typeModel="anova",verbose){
  # 1/ the time's vector
  mytime<-sort(unique(datain[,xvar]))

  # 2/ coords and greenhouse (lattice)
  # retrieve the coordinates and rename them to make coincide with names used in CARBayesST library!
  mycoord<-distinct(datain,Ref,.keep_all =TRUE)
  mycoord<-select(mycoord,Position,Line,Ref,scenario,genotypeAlias)
  dim(mycoord)

  # Create a lattice with all the coordinates and all the times
  mycoord2<-mycoord[rep(seq(nrow(mycoord)), rep(length(mytime),nrow(mycoord))), ]
  mylattice<-cbind.data.frame(mycoord2,rep(mytime,nrow(mycoord)))
  names(mylattice)[ncol(mylattice)]<-xvar

  # 3/ Complete datain that contains missing data, using a merge between times and mylattice
  tmpData<-full_join(mylattice,select(datain,-Ref,-scenario,-genotypeAlias),by=c("Position","Line",xvar))

  # This dataset musn't contain times where data are missing! Check, retrieve and suppress
  tmpNA<-as.data.frame(summarise_(group_by_(tmpData,.dots=c(xvar)),countNA=paste0("sum(!is.na(",trait,"))")  ))
  # I suppress from tmpData these dates.
  tmpSelectTime<-tmpNA[tmpNA$countNA==0,xvar]
  if (length(tmpSelectTime) > 0){
    tmpData<-filter_(tmpData,paste0("!(",xvar," %in% ",tmpSelectTime,")"))
  }
  tmpData<-distinct_(tmpData,"Position","Line",xvar,"Ref", .keep_all =TRUE)

  # Take care to well sort the dataset for the spatio-temporal object!!!
  # Sorting by Temporality then Spatiality
  tmpData<-arrange_(tmpData,xvar,"Line","Position")

  # 4/ Calculate the les distances
  # Transform mycoord in matrix object!!!!
  k1 <- make.sym.nb(knn2nb(knearneigh(as.matrix(select(mycoord,Position,Line)),k=k)))

  all.linked <- max(unlist(nbdists(k1, as.matrix(select(mycoord,Position,Line)))))
  col.nb.0.all <- dnearneigh(as.matrix(select(mycoord,Position,Line)), 0, all.linked)
  if (graphDist){
    plot(as.matrix(select(mycoord,Position,Line)))
    plot(col.nb.0.all, as.matrix(select(mycoord,Position,Line)), add=TRUE)
  }

  # I tranform the distances in a matrix of acurate dimension according to the used lattice
  W<-nb2mat(k1,style="B")
  # idem in a list object for the Moran's test
  #W.list<-nb2listw(k1,style="B")

  # Bayesian ANOVA modelling
  if (typeModel=="anova"){
    model<-ST.CARanova(formula=formulaModel,family="gaussian",W=W,burnin=burnin,n.sample=n.sample,
                       data=tmpData,verbose=verbose)

  } else if (typeModel=="ar"){
    model<-ST.CARar(formula=formulaModel,family="gaussian",W=W,burnin=burnin,n.sample=n.sample,
                    data=tmpData,verbose=verbose)
  } else if (typeModel=="linear"){
    model<-ST.CARlinear(formula=formulaModel,family="gaussian",W=W,burnin=burnin,n.sample=n.sample,
                    data=tmpData,verbose=verbose)
  }
  # large object - memory cleaning needed...
  #rm(W,W.list,k1,col.nb.0.all,mycoord2)
  rm(W,k1,col.nb.0.all,mycoord2)
  print(gc(reset=TRUE))
  return(list(model,tmpData))
}



#' printCARBayesST
#'
#' a function for CARBayesST analysis description
#' @param modelin a CARBayesST object to describe
#'
#' @return a summary of a CARBayesST object (spatio-temporal bayesian modelling)
#' @seealso \code{\link[=CARBayesST]{CARBayesST}}, \code{\link[=ST.CARanova]{ST.CARanova}}
#' @examples
#' \donttest{
#'  test<-fitCARBayesST(datain=mydata,xvar="thermalTime",trait="plantHeight",k=2,
#'      graphDist=TRUE,burnin=10,n.sample=110,
#'      formulaModel=as.formula(plantHeight~scenario+genotypeAlias),
#'      typeModel="anova",verbose=FALSE)
#'  printCARBayesST(modelin=test[[1]])
#' }
#' @export
printCARBayesST<-function(modelin){
  # print the result of the bayesian modelling
  tp<-modelin$summary.results[,-c(4,5,6)]
  print(tp)

  print(modelin$modelfit)
}



#' outlierCARBayesST
#'
#' a function to detect outlier point in time-course using a CARBayesST modelling
#' @param modelin a CARBayesST object
#' @param datain input dataframe of parameters used in the CARBayesST modelling
#' @param threshold, numeric threshold for influence with standardized residuals
#' @param trait, character, trait of interest to model (example plantHeight...)
#' @seealso \code{\link[=CARBayesST]{CARBayesST}}, \code{\link[=ST.CARanova]{ST.CARanova}}
#'
#' @return a plot of fitted vs. residuals and a dataset with a column identifying the outlier points
#' @examples
#' \donttest{
#'  test<-fitCARBayesST(datain=mydata,xvar="thermalTime",trait="plantHeight",k=2,
#'      graphDist=TRUE,burnin=10,n.sample=110,
#'      formulaModel=as.formula(plantHeight~scenario+genotypeAlias),
#'      typeModel="anova",verbose=FALSE)
#'  test2<-outlierCARBayesST(modelin=test[[1]],datain=test[[2]],threshold=4,trait="plantHeight")
#' }
#' @export
outlierCARBayesST<-function(modelin,datain,threshold,trait){
  # retrieve the residuals
  myresid<-modelin$residuals
  myfitted<-modelin$fitted.values
  plot(myfitted,myresid$pearson,main=paste0("CARBayesST analysis - diagnosis graphic: ",trait))

  outtmp<-cbind.data.frame(datain,myresid)
  # influence with standardized residuals
  crit<-ifelse(abs(outtmp$pearson)>threshold,yes=0,no=1)
  outtmp<-cbind.data.frame(outtmp,crit)
  return(outtmp)
}

