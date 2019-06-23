required.packages <- c("reshape2","ggplot2","data.table","WDI","rgdal","scales")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/danw/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")
WDI <- WDI(country = "all", "NY.GNP.PCAP.CD", extra =T)
vnr.gaps <- fread("output/vnr P20 gap growth.csv")

###Circle sets
vnr.countries <- c("KHM","KWT","LIE","NRU","NZL","OMN","PLW","DZA","AZE","BIH","BFA","CMR","CAF","TCD","CHL","COG","CIV","HRV","SWZ","FJI","GHA","GTM","GUY","ISL","IDN","IRQ","ISR","KAZ","LSO","MRT","MUS","MNG","PAK","PHL","RWA","LCA","SRB","SLE","ZAF","TLS","TON","TUN","TUR","TKM","GBR","TZA","VUT")
vnr.countries.df <- data.frame(iso=vnr.countries, poverty.data=NA, DHS.data=NA, recent.data=NA)
vnr.countries.df$poverty.data[which(vnr.countries.df$iso %in% GP20$CountryCode)] <- 1
vnr.countries.df$DHS.data[which(vnr.countries.df$iso %in% dhs$iso3)] <- 1
vnr.countries.df$recent.data[which(vnr.countries.df$iso %in% subset(dhs, survey_year >= 2014)$iso3)] <- 1

###Map
world <- readOGR("project-data/world/ne_110m_admin_0_countries.shp")
world.f <- fortify(world, region='SOV_A3')
world.f$vnrgap <- NA
world.f$vnrgap[which(world.f$id %in% vnr.countries)] <- -1 
world.f$vnrgap[which(world.f$id %in% subset(vnr.gaps, V1 >= 0)$CountryCode)] <- 1
world.f$vnrgap[which(world.f$id %in% subset(vnr.gaps, V1 < 0)$CountryCode)] <- 0
palbins = c(1,0,-1)
names(palbins)=c("Increasing","Decreasing","No data")
colscale <- c("#E84439","#0089CC","#A9A6AA")
ggplot(world.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=vnrgap,color="#EEEEEE",size=0.1))+
  coord_fixed(1) + # 1 to 1 ratio for longitude to latitude
  # or coord_cartesian() +
  scale_fill_gradientn(
    na.value="#EEEEEE",
    guide="legend",
    breaks=palbins,
    colors=colscale,
    values=rescale(palbins)
  ) +
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=world.f$long,y=world.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  guides(fill=guide_legend(title=""))+
  labs(x="",y="")

ggsave("output/VNR gaps.svg")

###Income group analysis
WDI <- subset(WDI, iso3c %in% vnr.countries & year == 2011)[,c(5,10)]
WDI <- rbind(WDI, data.frame(iso3c="SWZ",income="Lower middle income")) #WDI doesn't pick up Swaziland correctly
vnr.gaps <- merge(vnr.gaps, WDI, by.x="CountryCode", by.y="iso3c")
vnr.gaps.val <- vnr.gaps[, .(count=length(V1), increasing=sum(V1>=0), share=sum(V1>=0)/length(V1)), by=income]
write.csv(vnr.gaps.val, "output/VNR gaps by income.csv", row.names = F)
