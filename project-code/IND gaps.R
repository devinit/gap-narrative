required.packages <- c("reshape2","ggplot2","data.table","zoo")
lapply(required.packages, require, character.only=T)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

povcal.years <- as.data.table(c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015))
names(povcal.years) <- "povcal_year"

IND.melt <- melt(subset(dhs[,survey_year:=NULL], type=="statistic" & iso3 == "IND"), id.vars=c("variable","p20","iso3","type","povcal_year"))
IND.melt <- rbind(IND.melt, povcal.years, fill=T)
IND.melt <- dcast.data.table(IND.melt, p20 + variable ~ povcal_year)

#Convert to zoo via matrix
IND.mat <- zoo(t(IND.melt[,c(3:17)]))
index(IND.mat) <- as.numeric(rownames(IND.mat))

#Fill NAs by interpolation and extension (not extrapolation)
IND.approx <- na.fill(na.approx(IND.mat,na.rm=F),c("extend","NA","extend"))
IND.approx <- t(as.data.frame(IND.approx))

#Rejoin to character data
IND.approx <- cbind(IND.melt[,c(1:3)],IND.approx)

#Recast to long
IND.approx <- melt(IND.approx, id.vars = c("p20","variable"))
IND.approx$variable.1 <- as.numeric(levels(IND.approx$variable.1))[IND.approx$variable.1]
IND.dt <- unique(IND.approx[complete.cases(IND.approx)])

IND.dt$P20 <- "P20"
IND.dt[which(IND.dt$p20 == "FALSE")]$P20 <- "Rest of population"

IND.dt <- dcast.data.table(IND.dt, variable.1 + P20 ~ variable)

colnames(IND.dt) <- c("year","p20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(IND.dt, "output/INDya gaps analysis.csv")

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

for(var in colnames(IND.dt[,3:6])){
  varen <- as.name(var)
  varen <- enquo(varen)
  p <- ggplot(IND.dt,aes(year))+
    geom_smooth(aes(y=!!varen, colour=p20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(IND.dt[,..var])*1.2,2)),
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
  ggsave(paste0("output/INDIA ",var,".png"))
}
