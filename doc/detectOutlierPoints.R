## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  library(lubridate)
  library(dplyr)
  library(locfit)
  library(phisStatR)

## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  mydata<-plant1
  str(mydata)

  mydata<-filter(mydata,!is.na(mydata$thermalTime))

## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  resu1<-flagPointLocfit(datain=mydata,trait="biovolume",xvar="thermalTime",loopID="Ref",
                         locfit.h=30,threshold=8)

## ---- echo = TRUE,message=FALSE, warning=FALSE,fig.height=10,fig.width=12-----
  myindex<-as.character(unique(resu1[[1]][,"Ref"]))
  myindex<-myindex[1:30]
  for (i in seq(1,length(myindex),by=15)){ 
      myvec<-myindex[seq(i,i+14,1)]
      plotFlagPoint(smoothin=resu1[[1]],loopID="Ref",myselect=myvec)
  }

## ---- echo = TRUE,message=FALSE, warning=FALSE,fig.height=10,fig.width=12-----
  filter(resu1[[1]],outlier==1)

## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  # Please change the Ref column by the one in your dataframe
  if(is.null(resu1[[3]])){
    print("All the time courses have more than 4 points.")
  } else {
    ggplot(data=resu1[[3]],aes(x=x,y=y)) +
    geom_point() + facet_wrap(~Ref)
  }

## ----session,echo=FALSE,message=FALSE, warning=FALSE--------------------------
  sessionInfo()

