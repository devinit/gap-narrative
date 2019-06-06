required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

#Source of save first
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#Query povcal at all thresholds for given countries and years; this formula is inefficient and can be optimised
povcal.threshold.national <- function(targetHC,countries="all",year="all",lowerguess=0, upperguess=5,precision=0.01){
  data.list <- list()
  PLs <- seq(lowerguess,upperguess,by=precision)
  #pb <- txtProgressBar(min=0, max=length(PLs),style=3)
    for (i in 1:length(PLs)){
      message(PLs[i])
      param <- paste0("Countries=",paste(countries,collapse=","),"&RefYears=",paste(year,collapse=","),"&PovertyLine=",PLs[i])
      url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
      temp <- tryCatch({read.csv(url,header=T)},error=function(e){return(NULL)})
      data.list[[i]] <- temp
      #setTxtProgressBar(pb,i)
    }
  #close(pb)
  data <- rbindlist(data.list)
  data$diff <- abs(targetHC-data$HeadCount)
  datadt <- as.data.table(data)
  datadt$CountryYear <- paste0(datadt$CountryName,datadt$CoverageType,datadt$RequestYear)
  datadt <- datadt[datadt[, .I[diff == min(diff)], by=CountryYear]$V1]
  datadt <- datadt[datadt[, .I[PovertyLine==min(PovertyLine)], by=CountryYear]$V1]
  return(datadt)
}

req.years <- c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015)

#VNR countries, excluding those without PovcalNet data: Cambodia, Eritrea, Kuwait, Liechtenstein, Nauru, New Zealand, Oman and Palau 
req.countries <- c("DZA","AZE","BIH","BFA","CMR","CAF","TCD","CHL","COG","CIV","HRV","SLV","SWZ","FJI","GHA","GTM","GUY","ISL","IDN","IRQ","ISR","KAZ","LSO","MRT","MUS","MNG","PAK","PHL","RWA","LCA","SRB","SLE","ZAF","TLS","TON","TUN","TUR","TKM","GBR","TZA","VUT")

#This takes a LONG time to run
P20.thresholds.national <- povcal.threshold.national(targetHC = 0.2, countries=req.countries, year=req.years, lowerguess=0, upperguess=45, precision=0.01)
