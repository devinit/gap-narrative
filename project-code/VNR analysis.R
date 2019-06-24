required.packages <- c("reshape2","ggplot2","data.table","WDI","rgdal","scales")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/danw/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")
vnr.gaps <- fread("output/vnr P20 gap growth.csv")

WDI <- WDI(country = "all", "NY.GNP.PCAP.CD", extra =T)
iso.name <- unique(WDI[,c("iso3c","country")])
iso.name <- rbind(iso.name, data.frame(iso3c="SWZ",country="Eswatini")) #WDI doesn't pick up Eswatini correctly

param <- paste0("Countries=all&SurveyYears=all&PovertyLine=1")
url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
povcal <- fread(url,header=T)
povcal <- unique(povcal[,c("CountryCode","DataYear")])
povcal <- povcal[povcal[, .I[which(DataYear==max(DataYear))],by=CountryCode]$V1]

###Circle sets
vnr.countries <- c("KHM","KWT","LIE","NRU","NZL","OMN","PLW","DZA","AZE","BIH","BFA","CMR","CAF","TCD","CHL","COG","CIV","HRV","SWZ","FJI","GHA","GTM","GUY","ISL","IDN","IRQ","ISR","KAZ","LSO","MRT","MUS","MNG","PAK","PHL","RWA","LCA","SRB","SLE","ZAF","TLS","TON","TUN","TUR","TKM","GBR","TZA","VUT")
vnr.countries.df <- data.frame(iso=vnr.countries, poverty.data=NA, DHS.data=NA, recent.dhs=NA, recent.povcal=NA)
vnr.countries.df$poverty.data[which(vnr.countries.df$iso %in% GP20$CountryCode)] <- 1
vnr.countries.df$DHS.data[which(vnr.countries.df$iso %in% dhs$iso3)] <- 1
vnr.countries.df$recent.dhs[which(vnr.countries.df$iso %in% subset(dhs, survey_year >= 2015)$iso3)] <- 1
vnr.countries.df$recent.povcal[which(vnr.countries.df$iso %in% subset(povcal, DataYear >= 2015)$CountryCode)] <- 1
vnr.countries.df <- merge(vnr.countries.df,iso.name, by.x="iso", by.y="iso3c")
vnr.countries.df.sums <- colSums(vnr.countries.df[,c(2:5)],na.rm=T)
fwrite(vnr.countries.df, "output/vnr countries data gaps.csv")

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
  coord_fixed(1) +
  coord_cartesian() +
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

no.datas <- setdiff(vnr.countries,vnr.gaps$CountryCode)
vnr.gaps <- rbind(vnr.gaps, data.frame(CountryCode=no.datas,V1=as.numeric(NA),V2=as.numeric(NA)))
vnr.gaps$gap <- "No data"
vnr.gaps$gap[which(vnr.gaps$V1>=0)] <- "Increasing"
vnr.gaps$gap[which(vnr.gaps$V1<0)] <- "Decreasing"
fwrite(vnr.gaps[,c("CountryCode","gap")],"output/VNR gaps map data.csv")

###Income group analysis
income.groups <- subset(WDI, iso3c %in% vnr.countries & year == 2011)[,c(5,10)]
income.groups <- rbind(income.groups, data.frame(iso3c="SWZ",income="Lower middle income")) #WDI doesn't pick up Eswatini correctly
vnr.gaps <- merge(vnr.gaps, income.groups, by.x="CountryCode", by.y="iso3c")
vnr.gaps.val <- vnr.gaps[, .(count=length(V1), increasing=sum(V1>=0), share=sum(V1>=0)/length(V1)), by=income]
write.csv(vnr.gaps.val, "output/VNR gaps by income.csv", row.names = F)
