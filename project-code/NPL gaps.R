required.packages <- c("reshape2","ggplot2","data.table","zoo")
lapply(required.packages, require, character.only=T)

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

povcal.years <- as.data.table(c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015))
names(povcal.years) <- "survey_year"

npl.melt <- melt(subset(dhs, iso3 == "NPL"), id.vars=c("variable","p20","iso3","type","povcal_year","survey_year"))

u5.pop <- as.data.table(WDI(country="NP", indicator = c("SP.POP.0004.MA.5Y","SP.POP.0004.FE.5Y","SP.POP.TOTL.MA.ZS","SP.POP.TOTL.FE.ZS"), start=1990, end=2015, extra = T))
u5.pop <- u5.pop[,.(weight = (SP.POP.0004.MA.5Y*SP.POP.TOTL.MA.ZS + SP.POP.0004.FE.5Y*SP.POP.TOTL.FE.ZS)/10000), by=.(year, iso3c, country)]
u5.pop.br <- cbind(u5.pop,"registration")
u5.pop.st <- cbind(u5.pop, "stunting")
u5.pop <- rbind(u5.pop.br,u5.pop.st)

f.1549.fert <- as.data.table(WDI(country="NP", indicator = c("SP.POP.1519.FE.5Y","SP.POP.2024.FE.5Y","SP.POP.2529.FE.5Y","SP.POP.3034.FE.5Y","SP.POP.3539.FE.5Y","SP.POP.4044.FE.5Y","SP.POP.4549.FE.5Y","SP.POP.TOTL.FE.ZS","SP.DYN.TFRT.IN"), start=1990, end=2015, extra=T))
f.1549.fert <- f.1549.fert[,.(weight = ((SP.POP.1519.FE.5Y + SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y)*SP.POP.TOTL.FE.ZS)/10000*SP.DYN.TFRT.IN),by=.(year, iso3c, country)]
f.1549.fert <- cbind(f.1549.fert, "mortality")

o20.pop  <- as.data.table(WDI(country="NP", indicator = c("SP.POP.2024.FE.5Y","SP.POP.2529.FE.5Y","SP.POP.3034.FE.5Y","SP.POP.3539.FE.5Y","SP.POP.4044.FE.5Y","SP.POP.4549.FE.5Y","SP.POP.5054.FE.5Y","SP.POP.5559.FE.5Y","SP.POP.6064.FE.5Y","SP.POP.TOTL.FE.ZS","SP.POP.2024.MA.5Y","SP.POP.2529.MA.5Y","SP.POP.3034.MA.5Y","SP.POP.3539.MA.5Y","SP.POP.4044.MA.5Y","SP.POP.4549.MA.5Y","SP.POP.5054.MA.5Y","SP.POP.5559.MA.5Y","SP.POP.6064.MA.5Y","SP.POP.TOTL.MA.ZS", "SP.POP.65UP.TO.ZS"), start=1990, end=2015,extra=T))
o20.pop <- o20.pop[,.(weight = ((SP.POP.2024.FE.5Y + SP.POP.2529.FE.5Y + SP.POP.3034.FE.5Y + SP.POP.3539.FE.5Y + SP.POP.4044.FE.5Y + SP.POP.4549.FE.5Y + SP.POP.5054.FE.5Y + SP.POP.5559.FE.5Y + SP.POP.6064.FE.5Y)*SP.POP.TOTL.FE.ZS + (SP.POP.2024.MA.5Y + SP.POP.2529.MA.5Y + SP.POP.3034.MA.5Y + SP.POP.3539.MA.5Y + SP.POP.4044.MA.5Y + SP.POP.4549.MA.5Y + SP.POP.5054.MA.5Y + SP.POP.5559.MA.5Y + SP.POP.6064.MA.5Y)*SP.POP.TOTL.MA.ZS)/10000 + SP.POP.65UP.TO.ZS/100),by=.(year, iso3c, country)]
o20.pop <- cbind(o20.pop,"education")

pop.weights <- rbind(u5.pop,f.1549.fert,o20.pop)

npl.cast <- dcast.data.table(npl.melt, variable + iso3 + povcal_year ~ p20 + type)
npl.cast$P20.share <- npl.cast$TRUE_numerator/(npl.cast$TRUE_denominator+npl.cast$FALSE_denominator)
npl.cast$U80.share <- npl.cast$FALSE_numerator/(npl.cast$TRUE_denominator+npl.cast$FALSE_denominator)

npl.cast <- merge(npl.cast, pop.weights, by.x=c("variable","iso3","povcal_year"), by.y=c("V2","iso3c","year"))

npl.dt <- npl.cast[, .(P20.value = P20.share*weight, U80.value = U80.share*weight, P20.pop = weight*(TRUE_denominator/(FALSE_denominator+TRUE_denominator)), U80.pop=weight*(FALSE_denominator/(FALSE_denominator+TRUE_denominator))), by=.(country, povcal_year, variable)] 
npl.dt <- npl.dt[,.(P20.value=sum(P20.value, na.rm=T)/sum(P20.pop, na.rm=T),U80.value=sum(U80.value, na.rm=T)/sum(U80.pop, na.rm=T)),by=.(povcal_year,variable)]

#Cast to pretty data.table
npl.dt <- melt(npl.dt, id.vars=c("povcal_year","variable"))
npl.dt$gp20 <- "P20"
npl.dt[which(npl.dt$variable.1 == "U80.value")]$gp20 <- "Rest of population"
npl.dt <- dcast.data.table(npl.dt, povcal_year + gp20 ~ variable)

colnames(npl.dt) <- c("year","gp20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(npl.dt, "output/NPL gaps analysis.csv", row.names = F)

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

for(var in colnames(npl.dt[,3:6])){
  varen <- as.name(var)
  varen <- enquo(varen)
  p <- ggplot(subset(npl.dt, year >=1990),aes(year))+
    geom_smooth(aes(y=!!varen, colour=gp20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(npl.dt[,..var])*1.2,2)),
      expand = c(0,0),
      labels = scales::percent)+
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
  ggsave(paste0("output/Nepal ",var,".png"), width = 8.27, height = 5.83)
}
