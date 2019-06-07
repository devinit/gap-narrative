required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

#Source on save first
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd("..")

thresholds.national1 <- readRDS("project-data/thresholds1.rds")
thresholds.national2 <- readRDS("project-data/thresholds2.rds")
thresholds.national3 <- readRDS("project-data/thresholds3.rds")

thresholds.national <- do.call("rbind",list(thresholds.national1,thresholds.national2,thresholds.national3))

rm(list=c("thresholds.national1","thresholds.national2","thresholds.national3"))

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

#Count VNR countries with increasing or decreasing gaps in nominal and relative terms
in.de <- as.data.frame(c("Increasing","Decreasing"))
names(in.de) <- "Gap count"
vnr.P20.gap.counts <- cbind(in.de,rbind(vnr.P20.gap.growth[, .(nominal=sum(V1>0),relative=sum(V2>0))],vnr.P20.gap.growth[, .(nominal=sum(V1<0),relative=sum(V2<0))]))
rm("in.de")

