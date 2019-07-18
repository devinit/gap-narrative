required.packages <- c("reshape2","ggplot2","data.table","WDI")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/danw/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhsmf.csv")

u5.pop <- as.data.table(WDI(country=unique(dhs$iso3), indicator = c("SP.POP.0004.MA.5Y","SP.POP.0004.FE.5Y","SP.POP.TOTL.MA.ZS","SP.POP.TOTL.FE.ZS"), start=1990, end=2015, extra = T))
u5.pop <- u5.pop[,.(
  total = (SP.POP.0004.MA.5Y*SP.POP.TOTL.MA.ZS + SP.POP.0004.FE.5Y*SP.POP.TOTL.FE.ZS)/10000,
  male = (SP.POP.0004.MA.5Y*SP.POP.TOTL.MA.ZS)/10000,
  female = (SP.POP.0004.FE.5Y*SP.POP.TOTL.FE.ZS)/10000
),by=.(year, iso3c, country)]
u5.pop.br <- cbind(u5.pop,"registration")
u5.pop.st <- cbind(u5.pop, "stunting")
u5.pop <- rbind(u5.pop.br,u5.pop.st)

f.1549.fert <- as.data.table(WDI(country=unique(dhs$iso3), indicator = c("SP.POP.1519.FE.5Y","SP.POP.2024.FE.5Y","SP.POP.2529.FE.5Y","SP.POP.3034.FE.5Y","SP.POP.3539.FE.5Y","SP.POP.4044.FE.5Y","SP.POP.4549.FE.5Y","SP.POP.TOTL.FE.ZS","SP.DYN.TFRT.IN"), start=1990, end=2015, extra=T))
f.1549.fert <- f.1549.fert[,.(
  total = ((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN,
  male = (((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN)/2,
  female = (((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN)/2
),by=.(year, iso3c, country)]
f.1549.fert <- cbind(f.1549.fert, "mortality")

o20.pop  <- as.data.table(WDI(country=unique(dhs$iso3), indicator = c("SP.POP.2024.FE.5Y","SP.POP.2529.FE.5Y","SP.POP.3034.FE.5Y","SP.POP.3539.FE.5Y","SP.POP.4044.FE.5Y","SP.POP.4549.FE.5Y","SP.POP.5054.FE.5Y","SP.POP.5559.FE.5Y","SP.POP.6064.FE.5Y","SP.POP.TOTL.FE.ZS","SP.POP.2024.MA.5Y","SP.POP.2529.MA.5Y","SP.POP.3034.MA.5Y","SP.POP.3539.MA.5Y","SP.POP.4044.MA.5Y","SP.POP.4549.MA.5Y","SP.POP.5054.MA.5Y","SP.POP.5559.MA.5Y","SP.POP.6064.MA.5Y","SP.POP.TOTL.MA.ZS", "SP.POP.65UP.FE.ZS","SP.POP.65UP.MA.ZS"), start=1990, end=2015,extra=T))
o20.pop <- o20.pop[,.(
  total = ((SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y + SP.POP.5054.FE.5Y + SP.POP.5559.FE.5Y + SP.POP.6064.FE.5Y)*SP.POP.TOTL.FE.ZS + (SP.POP.2024.MA.5Y + SP.POP.2529.MA.5Y + SP.POP.3034.MA.5Y + SP.POP.3539.MA.5Y + SP.POP.4044.MA.5Y + SP.POP.4549.MA.5Y + SP.POP.5054.MA.5Y + SP.POP.5559.MA.5Y + SP.POP.6064.MA.5Y)*SP.POP.TOTL.MA.ZS)/10000 + SP.POP.65UP.MA.ZS/100 + SP.POP.65UP.FE.ZS/100,
  male = ((SP.POP.2024.MA.5Y + SP.POP.2529.MA.5Y + SP.POP.3034.MA.5Y + SP.POP.3539.MA.5Y + SP.POP.4044.MA.5Y + SP.POP.4549.MA.5Y + SP.POP.5054.MA.5Y + SP.POP.5559.MA.5Y + SP.POP.6064.MA.5Y)*SP.POP.TOTL.MA.ZS)/10000 + SP.POP.65UP.MA.ZS/100,
  female = ((SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y + SP.POP.5054.FE.5Y + SP.POP.5559.FE.5Y + SP.POP.6064.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000 + SP.POP.65UP.FE.ZS/100
),by=.(year, iso3c, country)]
o20.pop <- cbind(o20.pop,"education")

pop.weights <- rbind(u5.pop,f.1549.fert,o20.pop)
rm("u5.pop","f.1549.fert","o20.pop","u5.pop.st","u5.pop.br","pop.weights")

#M and F weights
pop.weights$male <- pop.weights$male/pop.weights$total
pop.weights$female <- pop.weights$female/pop.weights$total
pop.weights <- melt(pop.weights[,c(1:4,5:7)], id.vars=c("year","iso3c","country","V2","total"))

#P20 and U80 weights
dhs$p20weight <- 0.8
dhs[which(dhs$p20==TRUE)]$p20weight <- 0.2

#Reaggregate to national level
dhsout <- dhs[type=="statistic", .(value=sum(p20weight*value)), by=.(iso3, povcal_year, variable, sex)]
dhsout <- merge(dhsout, pop.weights, by.x=c("iso3","povcal_year","variable","sex"), by.y=c("iso3c","year","V2","variable"))
dhsout <- unique(dhsout[,.(value=sum(value.x*value.y),weight=total), by=.(iso3,country,variable,povcal_year)])

dhsout.melt <- melt(dhsout[povcal_year >= 1999], id.vars = c("iso3","country","variable","povcal_year"))
dhsout.cast <- dcast.data.table(dhsout.melt[variable.1=="value"], iso3 + country + variable ~ povcal_year)
weighting.cast <- dcast.data.table(dhsout.melt[variable.1=="weight"], iso3 + country + variable ~ povcal_year)
combine.cast <- dcast.data.table(dhsout.melt, iso3 + country + variable + variable.1 ~ povcal_year)

combine.cast <- combine.cast[order(variable.1)]

fwrite(combine.cast,"output/national social indicators DHS.csv")

