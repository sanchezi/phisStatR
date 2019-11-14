## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  library(lubridate)
  library(dplyr)
  library(phisStatR)

  myReport<-substr(now(),1,10)

  mydata<-plant1
  str(mydata)

## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  # We suppress observations with missing data in time variable (here thermalTime)
  mydata<-filter(mydata,!is.na(mydata$thermalTime))
  model<-fitCARBayesST(datain=mydata,xvar="thermalTime",trait="plantHeight",k=2,
          graphDist=TRUE,burnin=10,n.sample=110,
          formulaModel=as.formula(plantHeight~scenario+genotypeAlias),typeModel="anova",verbose=TRUE)


## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-----------------------
  # print the result of the bayesian modelling
  printCARBayesST(modelin=model[[1]])

## ---- echo = TRUE,message=FALSE, warning=FALSE--------------------------------
  outtmp<-outlierCARBayesST(modelin=model[[1]],datain=model[[2]],threshold=4,trait="plantHeight")

## ---- echo = TRUE,message=FALSE, warning=FALSE,fig.height=10,fig.width=12-----
  mygeno<-as.character(unique(model[[2]][,"genotypeAlias"]))
  mygeno<-mygeno[1:6]
  for (i in seq(1,length(mygeno),by=15)){ 
      myvec<-seq(i,i+14,1)
      plotCARBayesST(datain=model[[2]],outlierin=outtmp,myselect=myvec,trait="plantHeight",xvar="thermalTime")
  }
  

## ----session,echo=FALSE,message=FALSE, warning=FALSE--------------------------
  sessionInfo()

