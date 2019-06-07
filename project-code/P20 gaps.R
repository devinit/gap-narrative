required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

#Source of save first
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd("..")

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

#This takes a LONG time to run
thresholds.national <- povcal.threshold.national(countries=req.countries, year=req.years, lowerguess=0, upperguess=45, precision=0.01)

thresholds.national1 <- thresholds.national[1:ceiling(nrow(thresholds.national)/3)]
thresholds.national2 <- thresholds.national[(nrow(thresholds.national1)+1):(2*ceiling(nrow(thresholds.national)/3))]
thresholds.national3 <- thresholds.national[(nrow(thresholds.national1)+nrow(thresholds.national2)+1):nrow(thresholds.national)]

saveRDS(thresholds.national1, "project-data/thresholds1.rds")
saveRDS(thresholds.national2, "project-data/thresholds2.rds")
saveRDS(thresholds.national3, "project-data/thresholds3.rds")

targetHC <- 0.2
P20.thresholds.national <- thresholds.national
P20.thresholds.national$diff <- abs(targetHC-P20.thresholds.national$HeadCount)
P20.thresholds.national <- as.data.table(P20.thresholds.national)
P20.thresholds.national <- P20.thresholds.national[P20.thresholds.national[, .I[diff == min(diff)], by=.(RequestYear,CountryName)]$V1]
P20.thresholds.national <- P20.thresholds.national[P20.thresholds.national[, .I[PovertyLine==min(PovertyLine)], by=.(RequestYear,CountryName)]$V1]

#Calculate mean consumptions and gaps
P20.thresholds.national$p20.mean <- P20.thresholds.national$PovertyLine*(1-P20.thresholds.national$PovGap/P20.thresholds.national$HeadCount)
P20.thresholds.national$non.p20.mean <- (P20.thresholds.national$Mean*12/365.25-P20.thresholds.national$p20.mean*P20.thresholds.national$HeadCount)/(1-P20.thresholds.national$HeadCount)
P20.thresholds.national$p20.income.gap <- P20.thresholds.national$non.p20.mean - P20.thresholds.national$p20.mean

#Calculate growth in income gap by country
P20.gap.growth <- P20.thresholds.national[, (coefficients(lm(p20.income.gap~RequestYear))[2]),by=CountryCode]
P20.gap.growth$V2 <- P20.thresholds.national[, (coefficients(lm(p20.income.gap/Mean~RequestYear))[2]),by=CountryCode]$V1

#VNR countries, excluding those without PovcalNet data: Cambodia, Eritrea, Kuwait, Liechtenstein, Nauru, New Zealand, Oman and Palau 
vnr.countries <- c("DZA","AZE","BIH","BFA","CMR","CAF","TCD","CHL","COG","CIV","HRV","SLV","SWZ","FJI","GHA","GTM","GUY","ISL","IDN","IRQ","ISR","KAZ","LSO","MRT","MUS","MNG","PAK","PHL","RWA","LCA","SRB","SLE","ZAF","TLS","TON","TUN","TUR","TKM","GBR","TZA","VUT")

vnr.P20.gap.growth <- subset(P20.gap.growth, CountryCode %in% vnr.countries)

