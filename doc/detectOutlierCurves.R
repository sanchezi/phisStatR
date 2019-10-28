## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE------------------
  library(dplyr)
  library(tidyr)
  library(phisStatR)
  library(ggplot2)

## ----echo=TRUE,message=FALSE, warning=FALSE,error=FALSE------------------
  mydata<-plant4
  str(mydata)

## ----spatmodel1,echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-------
  spat.biomass<-fitSpATS(datain=mydata,trait="Biomass24",genotypeId="genotypeAlias",rowId="Line",colId="Position",
                 typeModel="anova",genotype.as.random=FALSE,nseg=c(14,30),verbose)

## ----spatplot1,echo=TRUE,message=FALSE, warning=FALSE,error=FALSE--------
  plot(spat.biomass)

## ----spatmodel2,echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-------
  spat.ph<-fitSpATS(datain=mydata,trait="PH24",genotypeId="genotypeAlias",rowId="Line",colId="Position",
                 typeModel="anova",genotype.as.random=FALSE,nseg=c(14,30),verbose)

## ----spatplot2,echo=TRUE,message=FALSE, warning=FALSE,error=FALSE--------
  plot(spat.ph)

## ----spatmodel3,echo=TRUE,message=FALSE, warning=FALSE,error=FALSE-------
  spat.phy<-fitSpATS(datain=mydata,trait="Phy",genotypeId="genotypeAlias",rowId="Line",colId="Position",
                 typeModel="anova",genotype.as.random=FALSE,nseg=c(14,30),verbose)

## ----spatplot3,echo=TRUE,message=FALSE, warning=FALSE,error=FALSE--------
  plot(spat.phy)

## ----proc,echo=TRUE,message=FALSE, warning=FALSE-------------------------
  # concatenate raw data and residuals calculated with the SpATS model
  devResBio<-spat.biomass$residuals
  devResPH<-spat.ph$residuals
  devResPhy<-spat.phy$residuals
  myglobal<-cbind.data.frame(mydata,devResBio,devResPH,devResPhy)
  threshold<-0.95
  
  myglobal<-mutate(myglobal,mean.devBio=mean(devResBio,na.rm=TRUE),
                          sd.devBio=sd(devResBio,na.rm=TRUE),
                          mean.devPH=mean(devResPH,na.rm=TRUE),
                          sd.devPH=sd(devResPH,na.rm=TRUE),
                          mean.devPhy=mean(devResPhy,na.rm=TRUE),
                          sd.devPhy=sd(devResPhy,na.rm=TRUE),
                          lower.devBio=mean.devBio - sd.devBio*qnorm(threshold),
                          lower.devPH=mean.devPH - sd.devPH*qnorm(threshold),
                          lower.devPhy=mean.devPhy - sd.devPhy*qnorm(threshold),
                          upper.devBio=mean.devBio + sd.devBio*qnorm(threshold),
                          upper.devPH=mean.devPH + sd.devPH*qnorm(threshold),
                          upper.devPhy=mean.devPhy + sd.devPhy*qnorm(threshold),
                          # yes=1 == OK
                          # no =0 == problem
                          lower.critraw.spatsBio=ifelse(devResBio - lower.devBio > 0,yes=1,no=0),
                          lower.critraw.spatsPH=ifelse(devResPH - lower.devPH > 0,yes=1,no=0),
                          lower.critraw.spatsPhy=ifelse(devResPhy - lower.devPhy > 0,yes=1,no=0),
                          upper.critraw.spatsBio=ifelse(devResBio - upper.devBio < 0,yes=1,no=0),
                          upper.critraw.spatsPH=ifelse(devResPH - upper.devPH < 0,yes=1,no=0),
                          upper.critraw.spatsPhy=ifelse(devResPhy - upper.devPhy < 0,yes=1,no=0),
                          Q1.devBio=quantile(devResBio,probs=0.25,na.rm=TRUE) - 
                                    1.5*(quantile(devResBio,probs=0.75,na.rm=TRUE) - 
                                         quantile(devResBio,probs=0.25,na.rm=TRUE)),
                          Q1.devPH=quantile(devResPH,probs=0.25,na.rm=TRUE) - 
                                    1.5*(quantile(devResPH,probs=0.75,na.rm=TRUE) - 
                                         quantile(devResPH,probs=0.25,na.rm=TRUE)),
                          Q1.devPhy=quantile(devResPhy,probs=0.25,na.rm=TRUE) - 
                                    1.5*(quantile(devResPhy,probs=0.75,na.rm=TRUE) - 
                                         quantile(devResPhy,probs=0.25,na.rm=TRUE)),
                          Q3.devBio=quantile(devResBio,probs=0.75,na.rm=TRUE) + 
                                    1.5*(quantile(devResBio,probs=0.75,na.rm=TRUE) - 
                                         quantile(devResBio,probs=0.25,na.rm=TRUE)),
                          Q3.devPH=quantile(devResPH,probs=0.75,na.rm=TRUE) + 
                                    1.5*(quantile(devResPH,probs=0.75,na.rm=TRUE) - 
                                         quantile(devResPH,probs=0.25,na.rm=TRUE)),
                          Q3.devPhy=quantile(devResPhy,probs=0.75,na.rm=TRUE) + 
                                    1.5*(quantile(devResPhy,probs=0.75,na.rm=TRUE) - 
                                         quantile(devResPhy,probs=0.25,na.rm=TRUE)),
                          lower.critci.spatsBio=ifelse(devResBio - Q1.devBio > 0,yes=1,no=0),
                          lower.critci.spatsPH=ifelse(devResPH - Q1.devPH > 0,yes=1,no=0),
                          lower.critci.spatsPhy=ifelse(devResPhy - Q1.devPhy > 0,yes=1,no=0),
                          upper.critci.spatsBio=ifelse(devResBio - Q3.devBio < 0,yes=1,no=0),
                          upper.critci.spatsPH=ifelse(devResPH - Q3.devPH < 0,yes=1,no=0),
                          upper.critci.spatsPhy=ifelse(devResPhy - Q3.devPhy < 0,yes=1,no=0)
                    )
    
  # small plants
  # yes=1 == OK
  # no =0 == problem
  myglobal<-mutate(myglobal,
                  flagLowerRawSpats=ifelse(lower.critraw.spatsBio + lower.critraw.spatsPhy==0,yes=0,no=1),
                  flagLowerCiSpats=ifelse(lower.critci.spatsBio + lower.critci.spatsPhy==0,yes=0,no=1)
                  )
  
  # big plants
  # yes=1 == OK
  # no =0 == problem
  myglobal<-mutate(myglobal,
                  flagUpperRawSpats=ifelse(upper.critraw.spatsBio + upper.critraw.spatsPH==0,yes=0,no=1),
                  flagUpperCiSpats=ifelse(upper.critci.spatsBio + upper.critci.spatsPH==0,yes=0,no=1)
                  )
  

## ----export,echo=TRUE,eval=FALSE,message=FALSE, warning=FALSE------------
#    write.table(myglobal,"OutputFile_detectOutlierCurves_SpATS.csv",row.names=FALSE,sep="\t")

## ----listing1,echo=TRUE,message=FALSE, warning=FALSE---------------------
  cat("Lower raw outlier:")
  myglobal %>% select(Ref,genotypeAlias,repetition,Biomass24,PH24,Phy,flagLowerRawSpats) %>%
                filter(flagLowerRawSpats !=1)
   
  cat("Upper raw outlier:")
  myglobal %>% select(Ref,genotypeAlias,repetition,Biomass24,PH24,Phy,flagUpperRawSpats) %>%
                filter(flagUpperRawSpats !=1)

## ----listing2,echo=TRUE,message=FALSE, warning=FALSE---------------------
  cat("Lower quantile outlier:")
  myglobal %>% select(Ref,genotypeAlias,repetition,Biomass24,PH24,Phy,flagLowerCiSpats) %>%
                filter(flagLowerCiSpats !=1)
   
  cat("Upper quantile outlier:")
  myglobal %>% select(Ref,genotypeAlias,repetition,Biomass24,PH24,Phy,flagUpperCiSpats) %>%
                filter(flagUpperCiSpats !=1)
    

## ----dm,echo=TRUE,message=FALSE, warning=FALSE---------------------------
  outlierId<-filter(myglobal,flagLowerRawSpats !=1 | flagUpperRawSpats !=1)  %>% 
             select(genotypeAlias,scenario,repetition,flagLowerRawSpats,flagUpperRawSpats) %>%
             unite("geno_sce",genotypeAlias,scenario,sep="_",remove=FALSE)

  mycurve<-plant1 %>%
           unite("geno_sce",genotypeAlias,scenario,sep="_",remove=FALSE) %>%
           left_join(outlierId,mycurve,by=c("geno_sce","repetition"))
  
  # flagLower ou upper == 0 problem
  # flagLower ou upper == 1 OK
  # flag == "lower" problem en lower
  # flag == "upper" problem en upper
  # flag == "OK" plant OK
  tmp<-filter(mycurve,geno_sce %in% outlierId[,"geno_sce"]) %>%
       mutate(flagLowerRawSpats=ifelse(is.na(flagLowerRawSpats),1,flagLowerRawSpats),
         flagUpperRawSpats=ifelse(is.na(flagUpperRawSpats),1,flagUpperRawSpats),
         flag=case_when(flagLowerRawSpats == 0 ~ "lower",
                        flagUpperRawSpats == 0 ~ "upper",
                        flagUpperRawSpats + flagLowerRawSpats ==2 ~ "OK"
         ))

## ----plotoutlier,echo=TRUE,message=FALSE, warning=FALSE,fig.height=10,fig.width=10----
  ggplot(data=tmp,aes(x=thermalTime,y=biovolume,color=as.factor(repetition))) + geom_line() +
    geom_point(aes(shape=flag,color=as.factor(repetition))) + scale_shape_manual(values = c(0,19,2)) +
    facet_wrap(~geno_sce) 

## ----plotoutlier2,echo=TRUE,message=FALSE, warning=FALSE,fig.height=10,fig.width=10----
  ggplot(data=tmp,aes(x=thermalTime,y=plantHeight,color=as.factor(repetition))) + geom_line() +
    geom_point(aes(shape=flag,color=as.factor(repetition))) + scale_shape_manual(values = c(0,19,2)) +
    facet_wrap(~geno_sce) 

## ----session,echo=FALSE,message=FALSE, warning=FALSE---------------------
  sessionInfo()

