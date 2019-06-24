required.packages <- c("reshape2","ggplot2","data.table","zoo")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/dan-w/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")

dhs.melt <- melt(subset(dhs, type=="statistic"), id.vars=c("variable","p20","iso3","type","povcal_year","survey_year"))
dhs.melt <- dcast.data.table(dhs.melt, iso3 + p20 + variable ~ povcal_year)

#Convert to zoo via matrix
dhs.mat <- zoo(t(dhs.melt[,c(4:18)]))
index(dhs.mat) <- as.numeric(rownames(dhs.mat))

#Fill NAs by interpolation and extension (not extrapolation)
dhs.approx <- na.fill(na.approx(dhs.mat,na.rm=F),c("extend","NA","extend"))
dhs.approx <- t(as.data.frame(dhs.approx))

#Rejoin to character data
dhs.approx <- cbind(dhs.melt[,c(1:3)],dhs.approx)

#Join to GP20 data
dhs.approx <- melt(dhs.approx, id.vars = c("iso3","p20","variable"))
dhs.approx$variable.1 <- as.numeric(levels(dhs.approx$variable.1))[dhs.approx$variable.1]
dhs.approx <- merge(dhs.approx,GP20, by.x=c("iso3","variable.1"),by.y=c("CountryCode","RequestYear"))
dhs.approx[which(p20=="FALSE")]$HeadCount <- 1-dhs.approx[which(p20=="FALSE")]$HeadCount

#Calculate population weighted variables
dhs.dt <- dhs.approx[,.(value=HeadCount*ReqYearPopulation*value,pop=HeadCount*ReqYearPopulation),by=.(iso3,variable.1,p20,variable)]
#dhs.dt[is.na(dhs.dt)] <- 0 
dhs.dt <- dhs.dt[,(value=sum(value, na.rm=T)/sum(pop)),by=.(variable.1,variable,p20)]

#Cast to pretty data.table
dhs.dt <- melt(dhs.dt, id.vars=c("variable.1","variable","p20"))
dhs.dt$gp20 <- "P20"
dhs.dt[which(dhs.dt$p20 == "FALSE")]$gp20 <- "Rest of population"
dhs.dt <- dcast.data.table(dhs.dt, variable.1 + gp20 ~ variable)

colnames(dhs.dt) <- c("year","gp20","Secondary education","Under-5 mortality","Birth registration","Stunting")

write.csv(dhs.dt, "output/DHS gaps analysis.csv")

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
    geom_line(aes(y=!!varen, colour=gp20),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=var, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(dhs.dt[,..var])*1.2,2)),
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
  ggsave(paste0("output/",var,"5.png"), width = 8.27, height = 5.83)
}
