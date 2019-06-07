############################################
## THIS SCRIPT TAKES SEVERAL HOURS TO RUN ##
############################################

#Query povcal at all thresholds for given countries and years; this formula is inefficient and can be optimised
povcal.threshold.national <- function(countries="all",year="all",lowerguess=0, upperguess=5,precision=0.01){
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
  return(data)
}


req.years <- c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015)
req.countries <- "all"

thresholds.national <- povcal.threshold.national(countries=req.countries, year=req.years, lowerguess=0, upperguess=45, precision=0.01)

thresholds.national1 <- thresholds.national[1:ceiling(nrow(thresholds.national)/3)]
thresholds.national2 <- thresholds.national[(nrow(thresholds.national1)+1):(2*ceiling(nrow(thresholds.national)/3))]
thresholds.national3 <- thresholds.national[(nrow(thresholds.national1)+nrow(thresholds.national2)+1):nrow(thresholds.national)]

saveRDS(thresholds.national1, "project-data/thresholds1.rds")
saveRDS(thresholds.national2, "project-data/thresholds2.rds")
saveRDS(thresholds.national3, "project-data/thresholds3.rds")
