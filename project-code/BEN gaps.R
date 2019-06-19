required.packages <- c("reshape2","ggplot2","data.table","zoo")
lapply(required.packages, require, character.only=T)

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

povcal.years <- as.data.table(c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015))
names(povcal.years) <- "survey_year"

ben.melt <- melt(subset(dhs, type=="statistic" & iso3 == "BEN"), id.vars=c("variable","p20","iso3","type","povcal_year","survey_year"))
#ben.melt <- rbind(ben.melt, povcal.years, fill=T)
ben.melt <- dcast.data.table(ben.melt, p20 + variable ~ povcal_year)

#Convert to zoo via matrix
ben.mat <- zoo(t(ben.melt[,c(3:17)]))
index(ben.mat) <- as.numeric(rownames(ben.mat))

#Fill NAs by interpolation and extension (not extrapolation)
ben.approx <- na.fill(na.approx(ben.mat,na.rm=F),c("extend","NA","extend"))
ben.approx <- t(as.data.frame(ben.approx))

#Rejoin to character data
ben.approx <- cbind(ben.melt[,c(1:2)],ben.approx)

#Recast to long
ben.approx <- melt(ben.approx, id.vars = c("p20","variable"))
ben.approx$variable.1 <- as.numeric(levels(ben.approx$variable.1))[ben.approx$variable.1]
ben.dt <- ben.approx[complete.cases(ben.approx)]

ben.dt$P20 <- "P20"
ben.dt[which(ben.dt$p20 == "FALSE")]$P20 <- "Rest of population"

ben.dt <- dcast.data.table(ben.dt, variable.1 + P20 ~ variable)

colnames(ben.dt) <- c("year","p20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(ben.dt, "output/Nepal gaps analysis.csv")

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

for(var in colnames(ben.dt[,3:5])){
  varen <- as.name(var)
  varen <- enquo(varen)
  p <- ggplot(ben.dt,aes(year))+
    geom_line(aes(y=!!varen, colour=p20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(ben.dt[,..var])*1.2,2)),
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
  ggsave(paste0("output/Benin ",var,".png"))
}
