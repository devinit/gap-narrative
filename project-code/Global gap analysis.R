required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd("..")

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

dhs.years <- unique(dhs[,c(5,6,7)])

povcal.years <- unique(GP20$RequestYear)

for(year in povcal.years){
  dhs.years[, as.character(year)] <- abs(dhs.years$survey_year - year)
}

dhs.years <- melt(dhs.years, id.vars = c("filename","iso3","survey_year"))
dhs.years <- dhs.years[dhs.years[, .I[value == min(value)], by=.(iso3,variable)]$V1]
dhs.years <- dhs.years[complete.cases(dhs.years)]
dhs.years$variable<- as.numeric(levels(dhs.years$variable))[dhs.years$variable]

dhs.years <- merge(dhs.years,dhs,by=c("filename","iso3","survey_year"),allow.cartesian=T)
dhs.years <- merge(dhs.years,GP20, by.x=c("iso3","variable.x"),by.y=c("CountryCode","RequestYear"))
dhs.years[which(p20=="FALSE")]$HeadCount <- 1-dhs.years[which(p20=="FALSE")]$HeadCount

dhs.dt <- dhs.years[type=="statistic",.(value=HeadCount*ReqYearPopulation*value.y,pop=HeadCount*ReqYearPopulation),by=.(filename,variable.x,p20,variable.y)]
dhs.dt[is.na(dhs.dt)] <- 0 
dhs.dt <- dhs.dt[,(value=sum(value)/sum(pop)),by=.(variable.x,variable.y,p20)]
