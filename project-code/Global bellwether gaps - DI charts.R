required.packages <- c("reshape2","ggplot2","data.table","WDI")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/danw/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhsmf.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

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
  male = ((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN,
  female = ((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN
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
pop.weights <- merge(pop.weights, GP20, by.x=c("iso3c","year"),by.y=c("CountryCode","RequestYear"))
pop.weights <- pop.weights[,c(1:7,10)]
pop.weights.melt <- melt(pop.weights, id.vars = c("iso3c","V2","country","year","ReqYearPopulation"))
pop.weights.melt$variable <- as.character(pop.weights.melt$variable)
names(pop.weights.melt)[which(names(pop.weights.melt) == "variable")] <- "sex"
names(pop.weights.melt)[which(names(pop.weights.melt) == "value")] <- "weight"

rm("u5.pop","f.1549.fert","o20.pop","u5.pop.st","u5.pop.br","pop.weights")

dhs.melt <- melt(dhs, id.vars=c("variable","p20","iso3","type","povcal_year","survey_year","sex"))
dhs.total <- dhs.melt[, .(sex = "total", value = sum(value)), by=.(variable,p20,iso3,type,povcal_year,survey_year,variable.1)]
dhs.melt <- rbind(subset(dhs.melt, type != "statistic"), subset(dhs.total, type != "statistic"))
dhs.cast<- dcast.data.table(dhs.melt, variable + iso3 + povcal_year + sex ~ p20 + type)
dhs.cast$P20.share <- dhs.cast$TRUE_numerator/(dhs.cast$TRUE_denominator+dhs.cast$FALSE_denominator)
dhs.cast$U80.share <- dhs.cast$FALSE_numerator/(dhs.cast$TRUE_denominator+dhs.cast$FALSE_denominator)

dhs.cast <- merge(dhs.cast, pop.weights.melt, by.x=c("variable","iso3","povcal_year","sex"), by.y=c("V2","iso3c","year","sex"))

dhs.dt <- dhs.cast[, .(P20.value = P20.share*ReqYearPopulation*weight, U80.value = U80.share*ReqYearPopulation*weight, P20.pop = ReqYearPopulation*weight*(TRUE_denominator/(FALSE_denominator+TRUE_denominator)), U80.pop=ReqYearPopulation*weight*(FALSE_denominator/(FALSE_denominator+TRUE_denominator))), by=.(country, povcal_year, variable, sex)]
dhs.dt <- dhs.dt[,.(P20.value=sum(P20.value, na.rm=T)/sum(P20.pop, na.rm=T),U80.value=sum(U80.value, na.rm=T)/sum(U80.pop, na.rm=T)),by=.(povcal_year,variable,sex)]

#Cast to pretty data.table
dhs.dt <- melt(dhs.dt, id.vars=c("povcal_year","variable","sex"))
dhs.dt$gp20 <- "P20"
dhs.dt[which(dhs.dt$variable.1 == "U80.value")]$gp20 <- "Rest of population"
dhs.dt <- dcast.data.table(dhs.dt, povcal_year + gp20 + sex ~ variable)

dhs.dt <- dhs.dt[order(dhs.dt[,"sex"],dhs.dt[,"gp20"])]

colnames(dhs.dt) <- c("year","gp20","sex","Secondary education","Under-5 mortality","Birth registration","Stunting")

dhs.dt$`Birth registration`[which(dhs.dt$year < 1999)] <- NA

write.csv(dhs.dt, "output/DHS gaps analysis.csv", row.names = F)

#Gap analysis
dhs.gaps <- melt(dhs.dt, id.vars=c("year","gp20","sex"))
dhs.gaps <- dcast.data.table(dhs.gaps, year + sex + variable ~ gp20)
dhs.gaps$gap <- dhs.gaps$`Rest of population` - dhs.gaps$P20
dhs.gaps.growth <- dhs.gaps[.(sex != "total", year >= 1999), .(gapgrowth = coefficients(lm(gap ~ year))[2]), by=.(sex,variable)]
dhs.gaps.change <- merge(dhs.gaps[(sex != "total" & year == 1999), .(gap1990 = gap), by=.(sex,variable)],dhs.gaps[(sex != "total" & year == 2015), .(gap2015 = gap), by=.(sex,variable)])
dhs.gaps.change$change <- dhs.gaps.change$gap2015-dhs.gaps.change$gap1990

dark.grey <- "#A0ADBB"
DIred <- "#E84439"
DIred2 <- "#F8C1B2"
DIgrey <- "#443E42"
DIgrey2 <- "#6A6569"
simple_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.line = element_line(colour = "black")
    ,text = element_text()
  )

for(var in colnames(dhs.dt[,4:7])){
    varen <- as.name(var)
    varen <- enquo(varen)
    p <- ggplot(subset(dhs.dt, year >=1999 & sex != "total"),aes(year))+
      geom_smooth(aes(y=!!varen, colour=paste(gp20,sex)),method=loess,se=F, size=1.2)+
      labs(x=NULL, y=NULL, colour="")+
      simple_style+
      scale_colour_manual(values=c(DIred,DIred2,DIgrey,DIgrey2))+
      scale_y_continuous(
        limits = c(0,round(max(dhs.dt[,..var])*1.3,2)),
        expand = c(0,0),
        labels = NULL)+
      scale_x_continuous(
        expand = c(0.2,0.2))+
      coord_cartesian(
        xlim=c(2000,2015)
        ,expand = T)+
      theme(
        legend.position="bottom"
        ,legend.box = "horizontal"
        ,legend.text = element_text(size=12,color=DIgrey)
        ,legend.direction="horizontal"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_text(size=12,color=DIgrey)
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=DIgrey, size = 0.8)
        ,axis.text.y = element_text(size=12,color=DIgrey,margin=margin(t=0,r=0,b=0,l=20))
        ,axis.text.x = element_text(size=12,color=DIgrey,margin=margin(t=20,r=0,b=0,l=0))
        ,panel.grid.major.y = element_line(color=dark.grey)
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.width = unit(1,"cm")
      )
    ggsave(paste0("output/",var," combined.png"), width = 8.27, height = 5.83)
  }


