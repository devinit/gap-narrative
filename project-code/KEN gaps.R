required.packages <- c("reshape2","ggplot2","data.table","zoo")
lapply(required.packages, require, character.only=T)

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

povcal.years <- as.data.table(c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015))
names(povcal.years) <- "survey_year"

ken.melt <- melt(subset(dhs, type=="statistic" & iso3 == "KEN"), id.vars=c("variable","p20","iso3","type","povcal_year","survey_year"))
#ken.melt <- rbind(ken.melt, povcal.years, fill=T)
ken.melt <- dcast.data.table(ken.melt, p20 + variable ~ povcal_year)

#Convert to zoo via matrix
ken.mat <- zoo(t(ken.melt[,c(3:17)]))
index(ken.mat) <- as.numeric(rownames(ken.mat))

#Fill NAs by interpolation and extension (not extrapolation)
ken.approx <- na.fill(na.approx(ken.mat,na.rm=F),c("extend","NA","extend"))
ken.approx <- t(as.data.frame(ken.approx))

#Rejoin to character data
ken.approx <- cbind(ken.melt[,c(1:2)],ken.approx)

#Recast to long
ken.approx <- melt(ken.approx, id.vars = c("p20","variable"))
ken.approx$variable.1 <- as.numeric(levels(ken.approx$variable.1))[ken.approx$variable.1]
ken.dt <- ken.approx[complete.cases(ken.approx)]

ken.dt$P20 <- "P20"
ken.dt[which(ken.dt$p20 == "FALSE")]$P20 <- "Rest of population"

ken.dt <- dcast.data.table(ken.dt, variable.1 + P20 ~ variable)

colnames(ken.dt) <- c("year","p20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(ken.dt, "output/Nepal gaps analysis.csv")

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

for(var in colnames(ken.dt[,3:5])){
  varen <- as.name(var)
  varen <- enquo(varen)
  p <- ggplot(ken.dt,aes(year))+
    geom_line(aes(y=!!varen, colour=p20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(ken.dt[,..var])*1.2,2)),
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
  ggsave(paste0("output/Kenya ",var,".png"))
}
