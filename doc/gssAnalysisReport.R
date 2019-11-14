## ----echo=TRUE,message=FALSE, warning=FALSE-----------------------------------
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(dplyr)
  library(gss)
  library(phisStatR)

  myreport<-substr(now(),1,10)

## ----oneprint,echo=TRUE,message=FALSE, warning=FALSE--------------------------
  data(plant3)
  cat("-------------- plant3 dataset ---------------\n")  
  printExperiment(datain=plant3)

## ----echo=TRUE,message=FALSE, warning=FALSE-----------------------------------
  # Import data, here is a dataset in the phisStatR package, You have to import your own dataset
  # using a read.table() statement or a request to the web service
  # You can add some datamanagement statements...
  #------------------------------------------------------------------------
  # Please, add the 'Ref' and 'Genosce' columns if don't exist. 
  # 'Ref' is the concatenation of experimentAlias-Line-Position-scenario
  # 'Genosce' is the concatenation of experimentAlias-genotypeAlias-scenario
  #------------------------------------------------------------------------
  
  mydata<-unite(plant3,Genosce,experimentAlias,genotypeAlias,scenario,sep="-",remove=FALSE)
  mydata<-arrange(mydata,Genosce)
  

## ----algo,echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # For one parameter, for example biovolume
  resbio<-fitGSS(datain=mydata,trait="biovolume",loopId="Genosce") 

## ----echo=TRUE,message=FALSE, warning=FALSE-----------------------------------
  outlierbio<-printGSS(object=resbio[[2]],threshold = 0.05)
  klbio<-printGSS(object=resbio[[2]],threshold = NULL)
  
  cat("Detection of outlier curve with KL projection:\n")
  print(outlierbio)
  
  #------------------------------------------------
  # You can export these two datasets
  # suppress the comments
  #------------------------------------------------
  #write.table(outlierbio,paste0(myreport,"outlier_gss_biovolume.csv"),row.names = FALSE,sep="\t")
  #write.table(klbio,paste0(myreport,"KLprojection_gss_biovolume.csv"),row.names = FALSE,sep="\t")

## ----graph,echo=TRUE,message=FALSE, warning=FALSE,fig.width=12, fig.height=16----
  # plot of the smoothing splines by genotype-scenario  
  for(i in seq(1,length(unique(mydata[,"Genosce"])),by=12)){ 
    myvec<-seq(i,i+11,1)
    myvec<-myvec[myvec<=length(unique(mydata[,"Genosce"]))]
    print(plotGSS(datain=mydata,modelin=resbio[[1]],trait="biovolume",myvec=myvec,lgrid=50))
    cat("\n\n")
  }


## ----session,echo=FALSE,message=FALSE, warning=FALSE--------------------------
  sessionInfo()

