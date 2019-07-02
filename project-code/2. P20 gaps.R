required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

#Source on save first
setwd("C:/Users/danw/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

thresholds.national1 <- readRDS("project-data/thresholds1.rds")
thresholds.national2 <- readRDS("project-data/thresholds2.rds")
thresholds.national3 <- readRDS("project-data/thresholds3.rds")

thresholds.national <- do.call("rbind",list(thresholds.national1,thresholds.national2,thresholds.national3))

rm(list=c("thresholds.national1","thresholds.national2","thresholds.national3"))

targetHC <- 0.2
P20.thresholds.national <- thresholds.national
P20.thresholds.national$diff <- abs(targetHC-P20.thresholds.national$HeadCount)
P20.thresholds.national <- as.data.table(P20.thresholds.national)
P20.thresholds.national <- P20.thresholds.national[P20.thresholds.national[, .I[diff == min(diff)], by=.(RequestYear,CountryName)]$V1]
P20.thresholds.national <- P20.thresholds.national[P20.thresholds.national[, .I[PovertyLine==min(PovertyLine)], by=.(RequestYear,CountryName)]$V1]

#Calculate mean consumptions and gaps
P20.thresholds.national$p20.mean <- P20.thresholds.national$PovertyLine*(1-P20.thresholds.national$PovGap/P20.thresholds.national$HeadCount)
P20.thresholds.national$non.p20.mean <- (P20.thresholds.national$Mean*12/365.25-P20.thresholds.national$p20.mean*P20.thresholds.national$HeadCount)/(1-P20.thresholds.national$HeadCount)
P20.thresholds.national$p20.income.gap <- P20.thresholds.national$non.p20.mean - P20.thresholds.national$p20.mean

#Calculate growth in income gap by country
P20.gap.growth <- P20.thresholds.national[, (coefficients(lm(p20.income.gap~RequestYear))[2]),by=CountryCode]
P20.gap.growth$V2 <- P20.thresholds.national[, (coefficients(lm(p20.income.gap/Mean~RequestYear))[2]),by=CountryCode]$V1

#VNR countries, excluding those without PovcalNet data: Cambodia, Kuwait, Liechtenstein, Nauru, New Zealand, Oman and Palau 
vnr.countries <- c("DZA","AZE","BIH","BFA","CMR","CAF","TCD","CHL","COG","CIV","HRV","SWZ","FJI","GHA","GTM","GUY","ISL","IDN","IRQ","ISR","KAZ","LSO","MRT","MUS","MNG","PAK","PHL","RWA","LCA","SRB","SLE","ZAF","TLS","TON","TUN","TUR","TKM","GBR","TZA","VUT")
vnr.P20.gap.growth <- subset(P20.gap.growth, CountryCode %in% vnr.countries)
write.csv(vnr.P20.gap.growth,"output/VNR p20 gap growth.csv", row.names = F)

#Count VNR countries with increasing or decreasing gaps in nominal and relative terms
in.de <- as.data.frame(c("Increasing","Decreasing"))
names(in.de) <- "Gap count"
vnr.P20.gap.counts <- cbind(in.de,rbind(vnr.P20.gap.growth[, .(nominal=sum(V1>0),relative=sum(V2>0))],vnr.P20.gap.growth[, .(nominal=sum(V1<0),relative=sum(V2<0))]))
rm("in.de")
write.csv(vnr.P20.gap.counts, "output/VNR P20 gap counts.csv", row.names = F)

#Chart P20 and non-P20 means
P20.thresholds.melt <- P20.thresholds.national[,c(3,4,7,34,35)]
P20.thresholds.melt <- melt(P20.thresholds.melt, id.vars=c(1:3))
P20.thresholds.melt[P20.thresholds.melt$variable=="p20.mean"]$variable <- "P20"
P20.thresholds.melt[P20.thresholds.melt$variable=="non.p20.mean"]$variable <- "Rest of population"

y.lab <- "Average daily consumption per capita (2011PPP)"
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

i <- 1
for (countrycode in vnr.countries){
  p <- ggplot(subset(P20.thresholds.melt, CountryCode==countrycode),aes(x=RequestYear))+
    geom_line(aes(y=value, colour=variable),method=loess,se=F, size=1.2)+
    labs(x=NULL, y=y.lab, colour="")+
    simple_style+
    scale_colour_manual(values=c(DIred,DIgrey))+
    scale_y_continuous(
      limits = c(0,round(max(subset(P20.thresholds.melt, CountryCode==countrycode)$value, na.rm=T)*1.1,0)),
      expand = c(0,0))+
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
  message(paste0("Saving ",countrycode," (",i,"/",length(vnr.countries),")"))
  i <- i + 1
  ggsave(paste0("output/VNR charts/",countrycode,"_p20_gap.png"),p,height=5.83,width=8.27)
}
