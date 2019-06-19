required.packages <- c("reshape2","ggplot2","data.table","zoo")
lapply(required.packages, require, character.only=T)

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

povcal.years <- as.data.table(c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015))
names(povcal.years) <- "survey_year"

uga.melt <- melt(subset(dhs, type=="statistic" & iso3 == "UGA"), id.vars=c("variable","p20","iso3","type","povcal_year","survey_year"))
#uga.melt <- rbind(uga.melt, povcal.years, fill=T)
uga.melt <- dcast.data.table(uga.melt, p20 + variable ~ povcal_year)

#Convert to zoo via matrix
uga.mat <- zoo(t(uga.melt[,c(3:17)]))
index(uga.mat) <- as.numeric(rownames(uga.mat))

#Fill NAs by interpolation and extension (not extrapolation)
uga.approx <- na.fill(na.approx(uga.mat,na.rm=F),c("extend","NA","extend"))
uga.approx <- t(as.data.frame(uga.approx))

#Rejoin to character data
uga.approx <- cbind(uga.melt[,c(1:2)],uga.approx)

#Recast to long
uga.approx <- melt(uga.approx, id.vars = c("p20","variable"))
uga.approx$variable.1 <- as.numeric(levels(uga.approx$variable.1))[uga.approx$variable.1]
uga.dt <- uga.approx[complete.cases(uga.approx)]

uga.dt$P20 <- "P20"
uga.dt[which(uga.dt$p20 == "FALSE")]$P20 <- "Rest of population"

uga.dt <- dcast.data.table(uga.dt, variable.1 + P20 ~ variable)

colnames(uga.dt) <- c("year","p20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(uga.dt, "output/Nepal gaps analysis.csv")

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

for(var in colnames(uga.dt[,3:5])){
  varen <- as.name(var)
  varen <- enquo(varen)
  p <- ggplot(uga.dt,aes(year))+
    geom_line(aes(y=!!varen, colour=p20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(uga.dt[,..var])*1.2,2)),
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
  ggsave(paste0("output/Uganda ",var,".png"))
}
