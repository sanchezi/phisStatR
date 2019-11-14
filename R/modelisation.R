#-------------------------------------------------------------------------------
# Program: modelisation.R
# Objective: modelize elaborate variables (Biovolume, plantheight and leafarea)
#            according to thermal time and estimate phyllocron
# Author: I.Sanchez
# Creation: 28/07/2016
# Update: 14/11/2019
#-------------------------------------------------------------------------------

#' @title a function to model curve of a dataset using a local regression
#' @description this function models each curve of a dataset using a local regression
#' on biovolume, plant height or leaf area trait and extracts predictions at specific thermal
#' times
#' @param datain input dataframe
#' @param trait character, trait of interest to model (example biovolume, PH ...)
#' @param xtime character, name of the thermal time column in the input dataset (example thermaltime)
#' @param myylim numeric, max value of y-axis , varies according to trait
#' @param tt numeric vector of thermal times at which predictions are recovered
#' @param reference character, a column name indicating an unique identifiant of plant in the input dataset
#'        (example: Ref == concatenation of experimentAlias-Line-Position-scenario)
#' @param myxlab character, a x label for the output graphics
#' 
#' @details the input dataset must contain a thermalTime column (numeric data) and a Ref column (unique id)
#' @return a dataframe of the predictions at tt thermal times for each plant of the input data set
#' and the graphics of the smoothing curves
#'
#' @examples
#' \donttest{
#' # Take a while...
#'  myThermalTimes<-c(24,30)
#'  resu<-fitLocfit(datain=plant1,trait="biovolume",xtime="thermalTime",
#'                  myylim=700,tt=myThermalTimes,reference="Ref",
#'                  myxlab="Thermal Time degD")
#'  str(resu)
#' }
#' @export
fitLocfit<-function(datain,trait,xtime,myylim,tt,reference,myxlab){
  datain<-as.data.frame(datain)
  myfm<-list()
  tabpred<-NULL    # dataframe
  for (i in levels(factor(datain[,reference]))){
    tmp<-datain[datain[,reference]==i,]
    tmp<-na.omit(tmp)
    y<-tmp[,trait]
    x<-tmp[,xtime]
    
    # bandwidth choice according to the number of data by plant
    #--------------------------------------
    if (length(x) <=4){
      plot(x,y,main=i,xlab=myxlab,ylab=trait,ylim=c(0,myylim),xlim=c(0,60))
      next
    }
    else if (length(x) <= 7) {
      mylocfit<-30
    } else {
      mylocfit<-15
    }
    
    # locfit model
    #--------------------------------------
    myfm<-locfit::locfit(y ~ locfit::lp(x, h = mylocfit, deg = 2))
    # Prediction at specific thermal times
    xpred<-tt
    ypred<-predict(myfm,newdata=list(x=xpred))
    # Graph locfit model
    plot(x,y,main=i,xlab=myxlab,ylab=trait,ylim=c(0,myylim),xlim=c(0,60))
    lines(myfm)
    points(xpred,ypred,col="red",pch=19)
    # join results into dataframe
    tppred<-cbind.data.frame(rep(i,length(xpred)),xpred,ypred)
    tabpred<-rbind.data.frame(tabpred,tppred)
  }
  names(tabpred)[1]<-reference
  
  # expert knowledge on predictions
  #--------------------------------------
  for (i in 1:nrow(tabpred)){
    if (tabpred[i,"xpred"] <=10 & tabpred[i,"ypred"] < -1) tabpred[i,"ypred"]<-0
    if (tabpred[i,"xpred"] >=24 & tabpred[i,"ypred"] < -1) tabpred[i,"ypred"]<-NA
  }
  return(tabpred)
}

#-------------------------------------------------------------------
#' @title a function to model the phyllocron using a linear regression
#' @description this function models each plant of a dataset using a regression
#' to extract the slope (i.e. phyllocron: the rate of leaf appearance)
#'
#' @param datain input dataframe (wide format, as many columns as variables)
#' @param trait variable to modelize (example visible_leaf, F_visible)
#' @param xtime character, name of thermal time column in the input dataset (example thermaltime)
#' @param myylim maximum value of y-axis for graphic
#' @param reference character, a column name indicating an unique identifiant of plant
#'        (example: Ref == concatenation of experimentAlias-Line-Position-scenario)
#' @param myxlab character, a x label for the output graphics
#' 
#' @details the input dataset must contain a thermalTime column (numeric data) and a Ref column (unique id)
#' @return a list of two elements and the graphics of the regression
#' @return 1: a dataframe of the predictions of the slope for each plant (phyllocron)
#' @return 2: a list of fitted models for each plant for diagnostics purposes
#'
#' @examples
#' \donttest{
#' # Take a while...
#'  resu<-fitReg(datain=plant2,trait="F_visible",xtime="thermalTime",
#'               myylim=22,reference="Ref",
#'               myxlab="Thermal Time degD")
#'  str(resu)
#' }
#' @export
fitReg<-function(datain,trait,xtime,myylim,reference,myxlab){
  datain<-as.data.frame(datain)
  myfm<-list()
  tpphy<-numeric()
  tabpred<-NULL    # dataframe
  
  for (i in levels(factor(datain[,reference]))){
    tmp<-datain[datain[,reference]==i,]
    y<-tmp[,trait]
    x<-tmp[,xtime]
    # linear regression
    myfm[[i]]<-lm(y~x)
    # slope of the linear model
    tpphy[i]<-summary(myfm[[i]])$coefficients[2,1]
    # Graph fit model
    plot(x,y,main=i,xlab=myxlab,ylab=trait,ylim=c(0,myylim),
         xlim=c(0,60))
    text(x=10*max(x)/100,y=20,cex=0.85,
         labels=paste("R2adj= ",round(summary(myfm[[i]])$adj.r.squared,3),sep=""))
    abline(myfm[[i]])
    abline(v=24,col="pink",lty=2)
  }
  
  tabpred<-cbind.data.frame(levels(factor(datain[,reference])),tpphy)
  names(tabpred)<-c(reference,"phy")
  
  return(list(tabpred,myfm))
}


#----------------------- End of file --------------------------------
