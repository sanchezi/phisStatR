#-------------------------------------------------------------------------------
# Program: thermalTime.R
# Objective: calculation of thermal time according to several methods
# Author: I.Sanchez
# Creation: 12/12/2016
# Update  : 14/11/2019
#-------------------------------------------------------------------------------

##' a function to calculate thermal time
##' @description this function calculates the thermal time for an experiment
##'              according to several methods
##' @param datain input dataframe of meteo data from phis web service
##' @param inSpecie character, studied specie
##' @param method character, a method of thermal time's calculation ("parent","baseline")
##' @param inDateS a date of sowing or thinning etc... start event ("YYYY-MM-DD")
##' @param inDateE a date of harvesting ("YYYY-MM-DD")
##' @param inTemp numeric, a baseline temperature for baseline's method
##' @details Parent et. al. (2010). Modelling temperature-compensated physiological rates,
##'    based on the co-ordination of responses to temperature of developmental processes.
##'    Journal of Experimental Botany. 61 (8):2057-2069
##'
##'    if the Parent's model is chosen inTemp must be NULL
##'
##'    The input dataframe is extracted from phis web service (getEnvironment function)
##'    and is structured as follow: date, value, sensor, codeVariable and facility
##' @return a dataframe
##'
##' @importFrom lubridate yday ymd
##' @importFrom dplyr mutate arrange summarise filter
##'
##' @examples
##' \donttest{
##' # Example for the model of Parent 2010
##' library(phisWSClientR)
##' # connectToPHISWS and getEnvironment are functions of the phisWSClientR library
##' connectToPHISWS(apiID="ws_1_public", username = "guestphis@supagro.inra.fr",
##'                 password = "guestphis")
##' tpCount<-getEnvironment(experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2017-03-30",
##'               facility="http://www.phenome-fppn.fr/m3p/es2",
##'               variables="leaf temperature_thermocouple sensor_degree celsius")$totalCount
##' myMeteo<-getEnvironment(experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2017-03-30",
##'               facility="http://www.phenome-fppn.fr/m3p/es2",
##'               variables="leaf temperature_thermocouple sensor_degree celsius",
##'               pageSize=tpCount)$data
##' test<-thermalTime(datain=myMeteo,inSpecie="maize",method="parent",inDateS="2017-04-02",
##'                   inDateE="2017-06-15",inTemp=NULL)
##' }
##' @export
thermalTime<-function(datain,inSpecie,method,inDateS=NULL,inDateE=NULL,inTemp=NULL){
  #---------------------------------------------------------
  #-- 1/ datamanagement of meteo data from phis-si
  #---------------------------------------------------------
  myMeteo<-as.data.frame(datain)
  myMeteo$myDate<-lubridate::ymd_hms(myMeteo[,"date"])
  # 1a: take mean of sensors data by date
  # we have 1 value per quarter hour!
  myMeteoMean<-summarise(group_by(myMeteo,myDate),tMean=mean(value,na.rm=TRUE))
  myMeteoMean<-arrange(myMeteoMean,myDate)

  # 1b: Day retrieve the yyyymmdd in myDate and nDay gives what is the nth day of myDate
  myMeteoMean<-mutate(myMeteoMean,Day=as.character(ymd(substr(myDate,1,10))),
                      nDay=yday(myDate))
  # filter on inDataS and inDateE: start and end of events!
  myMeteoMean<-filter(myMeteoMean,Day >= inDateS,Day <= inDateE)


  #---------------------------------------------------------
  #-- 2/ calculation of TT according to the chosen method
  #---------------------------------------------------------
  if (method == "parent"){
  #------------------------
    # Initialisation of parameters' model
    R<-8.134
    if (inSpecie=="maize"){
      Ha<-76800
      Hd<-285000
      Sd<-933
    } else if (inSpecie=="rice"){
      Ha<-87500
      Hd<-333000
      Sd<-1090
    } else if (inSpecie=="arabidopsis"){
      Ha<-63100
      Hd<-358000
      Sd<-1180
    }

    # calculation of DAS or DAE or DAT
    myMeteoMean$pDate<-yday(inDateS)
    myMeteoMean<-mutate(myMeteoMean,DAT=nDay - pDate)
    myMeteoMean<-filter(myMeteoMean,DAT >= 0)

    # Temperature in kelvin unit
    myMeteoMean<-mutate(myMeteoMean,tKelvin=tMean + 273)

    # Theoretical function at 20 degree C
    f20<-(293*exp(-Ha/(R*293))) / (1+exp((Sd/R)-(Hd/(R*293))))

    # Time calculation (number of days at 20 degree C) on temperatures per quarter hour
    myMeteoMean<-mutate(myMeteoMean,ft=(tKelvin*exp(-Ha/(R*tKelvin))) /
                          (1+exp((Sd/R)-(Hd/(R*tKelvin)))) )

    # t20 by number of records per day
    tp<-summarise((group_by(myMeteoMean,Day)),countT=n())
    myMeteoMean<-dplyr::left_join(myMeteoMean,tp,by="Day")
    myMeteoMean<-mutate(myMeteoMean,t20=(ft/f20)*(1/countT),
                               t20Cumul=cumsum(t20))

    dataout<-as.data.frame(summarise(group_by(myMeteoMean,Day),TT=max(t20Cumul,na.rm=TRUE)))
  #----------------------------------
  } else if (method == "baseline"){
  #----------------------------------
    myMeteoMean<-summarise(group_by(myMeteoMean,Day),tMean=mean(tMean,na.rm=TRUE))
    myMeteoMean<-arrange(myMeteoMean,Day)
    myMeteoMean$baselineTemp<-inTemp

    dataout<-as.data.frame(mutate(myMeteoMean,TT=cumsum(tMean-baselineTemp)))
  }

  return(dataout)
}
