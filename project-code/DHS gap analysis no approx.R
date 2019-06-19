required.packages <- c("reshape2","ggplot2","data.table","WDI")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/dan-w/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

u5.pop <- as.data.table(WDI(country=unique(dhs$iso3), indicator = c("SP.POP.0004.MA.5Y","SP.POP.0004.FE.5Y","SP.POP.TOTL.MA.ZS","SP.POP.TOTL.FE.ZS"), start=1990, end=2015, extra = T))
u5.pop <- u5.pop[,.(weight = (SP.POP.0004.MA.5Y*SP.POP.TOTL.MA.ZS + SP.POP.0004.FE.5Y*SP.POP.TOTL.FE.ZS)/10000), by=.(year, iso3c, country)]
u5.pop.br <- cbind(u5.pop,"registration")
u5.pop.st <- cbind(u5.pop, "stunting")
u5.pop <- rbind(u5.pop.br,u5.pop.st)

f.1549.fert <- as.data.table(WDI(country=unique(dhs$iso3), indicator = c("SP.POP.1519.FE.5Y","SP.POP.2024.FE.5Y","SP.POP.2529.FE.5Y","SP.POP.3034.FE.5Y","SP.POP.3539.FE.5Y","SP.POP.4044.FE.5Y","SP.POP.4549.FE.5Y","SP.POP.TOTL.FE.ZS","SP.DYN.TFRT.IN"), start=1990, end=2015, extra=T))
f.1549.fert <- f.1549.fert[,.(weight = ((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN),by=.(year, iso3c, country)]
f.1549.fert <- cbind(f.1549.fert, "mortality")

o20.pop  <- as.data.table(WDI(country=unique(dhs$iso3), indicator = c("SP.POP.2024.FE.5Y","SP.POP.2529.FE.5Y","SP.POP.3034.FE.5Y","SP.POP.3539.FE.5Y","SP.POP.4044.FE.5Y","SP.POP.4549.FE.5Y","SP.POP.5054.FE.5Y","SP.POP.5559.FE.5Y","SP.POP.6064.FE.5Y","SP.POP.TOTL.FE.ZS","SP.POP.2024.MA.5Y","SP.POP.2529.MA.5Y","SP.POP.3034.MA.5Y","SP.POP.3539.MA.5Y","SP.POP.4044.MA.5Y","SP.POP.4549.MA.5Y","SP.POP.5054.MA.5Y","SP.POP.5559.MA.5Y","SP.POP.6064.MA.5Y","SP.POP.TOTL.MA.ZS", "SP.POP.65UP.TO.ZS"), start=1990, end=2015,extra=T))
o20.pop <- o20.pop[,.(weight = ((SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y + SP.POP.5054.FE.5Y + SP.POP.5559.FE.5Y + SP.POP.6064.FE.5Y)*SP.POP.TOTL.FE.ZS + (SP.POP.2024.MA.5Y + SP.POP.2529.MA.5Y + SP.POP.3034.MA.5Y + SP.POP.3539.MA.5Y + SP.POP.4044.MA.5Y + SP.POP.4549.MA.5Y + SP.POP.5054.MA.5Y + SP.POP.5559.MA.5Y + SP.POP.6064.MA.5Y)*SP.POP.TOTL.MA.ZS)/10000 + SP.POP.65UP.TO.ZS/100),by=.(year, iso3c, country)]
o20.pop <- cbind(o20.pop,"education")

pop.weights <- rbind(u5.pop,f.1549.fert,o20.pop)
pop.weights <- merge(pop.weights, GP20, by.x=c("iso3c","year"),by.y=c("CountryCode","RequestYear"))

rm("u5.pop","f.1549.fert","o20.pop","u5.pop.st","u5.pop.br")

dhs.melt <- melt(dhs, id.vars=c("variable","p20","iso3","type","povcal_year","survey_year"))
dhs.cast<- dcast.data.table(dhs.melt, variable + iso3 + povcal_year ~ p20 + type)
dhs.cast$P20.share <- dhs.cast$TRUE_numerator/(dhs.cast$TRUE_denominator+dhs.cast$FALSE_denominator)
dhs.cast$U80.share <- dhs.cast$FALSE_numerator/(dhs.cast$TRUE_denominator+dhs.cast$FALSE_denominator)

dhs.cast <- merge(dhs.cast, pop.weights, by.x=c("variable","iso3","povcal_year"), by.y=c("V2","iso3c","year"))

dhs.dt <- dhs.cast[, .(P20.value = P20.share*ReqYearPopulation*weight, U80.value = U80.share*ReqYearPopulation*weight, P20.pop = ReqYearPopulation*weight*(TRUE_denominator/(FALSE_denominator+TRUE_denominator)), U80.pop=ReqYearPopulation*weight*(FALSE_denominator/(FALSE_denominator+TRUE_denominator))), by=.(country, povcal_year, variable)] 
dhs.dt <- dhs.dt[,.(P20.value=sum(P20.value, na.rm=T)/sum(P20.pop, na.rm=T),U80.value=sum(U80.value, na.rm=T)/sum(U80.pop, na.rm=T)),by=.(povcal_year,variable)]

#Cast to pretty data.table
dhs.dt <- melt(dhs.dt, id.vars=c("povcal_year","variable"))
dhs.dt$gp20 <- "P20"
dhs.dt[which(dhs.dt$variable.1 == "U80.value")]$gp20 <- "Rest of population"
dhs.dt <- dcast.data.table(dhs.dt, povcal_year + gp20 ~ variable)

dhs.dt <- dhs.dt[order(dhs.dt[,"gp20"])]

colnames(dhs.dt) <- c("year","gp20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(dhs.dt, "output/DHS gaps analysis.csv", row.names = F)

dark.grey <- "#A0ADBB"
DIred <- "#E84439"
DIgrey <- "#443E42"
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

for(var in colnames(dhs.dt[,3:6])){
  varen <- as.name(var)
  varen <- enquo(varen)
  p <- ggplot(subset(dhs.dt, year >=1990),aes(year))+
    geom_smooth(aes(y=!!varen, colour=gp20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(dhs.dt[,..var])*1.3,2)),
      expand = c(0,0),
      labels = scales::percent)+
  #  coord_cartesian(
  #    xlim=c(2000,2015)
  #  )+
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
  ggsave(paste0("output/",var,"7.png"), width = 8.27, height = 5.83)
}


WDI(country="WLD","SP.POP.TOTL", start = 1990, end = 2015)
(unique(dhs.cast[,c("povcal_year","iso3","ReqYearPopulation")]))[, (sum=sum(ReqYearPopulation)), by=.(povcal_year)]