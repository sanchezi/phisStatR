#-------------------------------------------------------------------------------
# Program: pointCleaning.R
# Objective: detection of outlier points for time courses using a
#            locfit smoothing and an interval of prediction
# Author: I.Sanchez
# Creation: 13/04/2018
# Update  : 29/10/2019
#-------------------------------------------------------------------------------

#' flagPointLocfit
#'
#' detection of outlier points in time courses
#' @param datain input dataframe of parameters
#' @param trait character, parameter of interest (ex: plantHeight)
#' @param xvar character, time variable (ex: thermalTime)
#' @param loopID character, ID on which to make the loop
#' @param locfit.h numeric, the constant component of the smoothing parameter
#' @param threshold numeric, threshold to detect on the prediction interval
#'
#' @return a list
#' \describe{
#' \item{1}{prediction and detection of outlier on observed data}
#' \item{2}{prediction on regular abscissa data}
#' \item{3}{time courses with not enough point to be evaluated}
#' }
#' @export
#'
#' @importFrom dplyr select_ bind_rows
#' 
#' @examples
#' \donttest{
#' # Example
#' }
flagPointLocfit<-function(datain,trait,xvar,loopID,locfit.h,threshold){

  # Initialisation
  #myfm<-list()
  tabpred<-NULL    # dataframe on observed data
  tabpredreg<-NULL    # dataframe on regular abscissa data
  tabNotEnoughPoint<-NULL # dataframe of observed data with not enough point by loopId

  for (i in levels(factor(datain[,loopID]))){
    ### select the data
    tmp<-datain[datain[,loopID]==i,]
    tmp<-na.omit(select_(tmp,trait,xvar))
    y<-tmp[,trait]
    x<-tmp[,xvar]
    # abscisse reguliere
    xreg<-seq(0,max(x,na.rm=TRUE),1)

    ### Choix du pas suivant le nombre de donnees, car certaines courbes peuvent avoir peu de points...
    if (length(x) <=4){
      next
    } else if (length(x) <= 7) {
      if (is.null(locfit.h)) mylocfit<-30
      else mylocfit<-locfit.h
    } else {
      if (is.null(locfit.h)) mylocfit<-15
      else mylocfit<-locfit.h
    }

    ### lissage et intervalles de prediction
    if (length(na.omit(y)) <=4){
      tabNotEnoughPoint<-bind_rows(tabNotEnoughPoint,
                                    select_(datain[datain[,loopID]==i,],loopID,xvar,trait))
      names(tabNotEnoughPoint)<-c("Ref","x","y")
      next
    } else {
      # modele locfit
      myfm<-locfit(y ~ lp(x, h = mylocfit, deg = 2))
      #-- Recuperation des predictions pour l'intervalle sur vecteur regulier
      ypred<-predict(myfm,newdata=list(x=xreg),se.fit=TRUE)
      lwr<-ypred[[1]]-threshold*ypred[[2]]
      upr<-ypred[[1]]+threshold*ypred[[2]]
      #-- Recuperation des predictions pour l'intervalle sur x observe
      ypred2<-predict(myfm,newdata=list(x=x),se.fit=TRUE)
      lwr2<-ypred2[[1]]-threshold*ypred2[[2]]
      upr2<-ypred2[[1]]+threshold*ypred2[[2]]

      #-- concatenation des resus en dataframe
      tppred<-cbind.data.frame(rep(i,length(x)),x,y,ypred2[[1]],ypred2[[2]],lwr2,upr2)
      tppred<-mutate(tppred,outlier=if_else(y < upr2 & y > lwr2,0,1))
      tabpred<-rbind.data.frame(tabpred,tppred)

      tppredreg<-cbind.data.frame(rep(i,length(xreg)),xreg,ypred[[1]],ypred[[2]],lwr,upr)
      tabpredreg<-rbind.data.frame(tabpredreg,tppredreg)
    }
  }
  names(tabpred)<-c("Ref","x","y","ypred","ypred_se","lwr","upr","outlier")
  names(tabpredreg)<-c("Ref","xreg","ypred","ypred_se","lwr","upr")
  return(list(tabpred,tabpredreg,tabNotEnoughPoint))
}

#' plotFlagPoint
#'
#' plot detection of outlier points in time courses
#' @param smoothin the 1st element of a flagPointLocfit() list result. smoothin contains the prediction on observed data and the detected outlier
#' @param loopID character, name of the column of the time courses' ID
#' @param myselect character, a vector of ID
#'
#' @return a graph
#' @export
#'
#' @examples
#' \donttest{
#' # Example
#' }
plotFlagPoint<-function(smoothin,loopID,myselect){
  # filtering smoothin dataframe for some time courses
  tmp<-smoothin[smoothin[,loopID] %in% myselect,]
  g<-ggplot(data=tmp,aes(x=x,y=y)) +
    geom_point() +
    geom_line(aes(y=ypred,colour="red")) +
    geom_line(aes(y=upr,colour="green")) +
    geom_line(aes(y=lwr,colour="green")) +
    geom_point(data=subset(tmp, outlier == 1),colour="blue",shape=2,size=2) +
    facet_wrap(loopID)
  print(g)
}

#,nbCurveByPage,xlabel
#plot(x,y,main=i,xlab=xlabel,ylab=trait,cex.main=0.7)
# vecteur pour disposition des graphs dans page
# if (nbCurveByPage ==1) mypage<-c(1,1)
# else if (nbCurveByPage ==2) mypage<-c(2,1)
# else if (nbCurveByPage %in% c(3,4)) mypage<-c(2,2)
# else if (nbCurveByPage %in% c(5,6)) mypage<-c(2,2)
# else if (nbCurveByPage %in% c(7,8)) mypage<-c(4,2)
# else if (nbCurveByPage ==9) mypage<-c(3,3)
# par(mfrow=mypage)
# Graph locfit modele + IC
#plot(x,y,main=i,xlab=xlabel,pch=19,cex.main=0.7,cex=0.8)
#lines(xreg,ypred[[1]],col="red")
#lines(xreg,upr,col="green")
#lines(xreg,lwr,col="green")
#points(filter(tppred,outlier==1)[,"x"],filter(tppred,outlier==1)[,"y"],col="blue",pch=1,cex=1.5)

