required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

#Query povcal at all poverty lines globally.
povcal.threshold.global <- function(targetHC,year="all",lowerguess=0, upperguess=10,precision=0.005){
  data.list <- list()
  PLs <- seq(lowerguess,upperguess,by=precision)
  pb <- txtProgressBar(min=0, max=length(PLs),style=3)
  for (i in 1:length(PLs)){
    param <- paste0("Countries=all&RefYears=",year,"&PovertyLine=",PLs[i])
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=a")
    temp <- read.csv(url,header=T)
    data.list[[i]] <- temp
    setTxtProgressBar(pb,i)
  }
  close(pb)
  data <- rbindlist(data.list)
  data$diff <- abs(targetHC-data$hc)
  thresholds <- as.data.table(data)
  thresholds <- thresholds[thresholds[, .I[diff == min(diff)], by=.(requestYear)]$V1]
  #thresholds <- thresholds[thresholds[, .I[povertyLine==min(povertyLine)], by=.(requestYear)]$V1] #This line probably isn't required for global aggregates
  return(thresholds)
}

GP20.thresholds <- povcal.threshold.global(targetHC=0.2,year="all",lowerguess=1,upperguess=3,precision=0.01)
GP20.thresholds <- GP20.thresholds.[,c(1,4)]

#Query povcal at global poverty lines nationally. Input is a dataframe of years and poverty lines
povcal.national <- function(year.PLs){
  data.list <- list()
  pb <- txtProgressBar(min=0, max=nrow(year.PLs),style=3)
  for (i in 1:nrow(year.PLs)){
    param <- paste0("Countries=all&RefYears=",year.PLs[i,1],"&PovertyLine=",year.PLs[i,2])
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
    temp <- read.csv(url,header=T)
    data.list[[i]] <- temp
    setTxtProgressBar(pb,i)
  }
  close(pb)
  data <- rbindlist(data.list)
  return(data)
}

GP20.national <- povcal.national(GP20.thresholds)
GP20.national.headcounts <- GP20.national[,c(3,4,7,13,21)]

write.csv(GP20.national.headcounts,"project-data/GP20 headcounts.csv", row.names = F)

          