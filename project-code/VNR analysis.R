required.packages <- c("reshape2","ggplot2","data.table","WDI")
lapply(required.packages, require, character.only=T)

setwd("C:/Users/dan-w/Box/Gap Narrative (ITEP), June 2019/git/gap-narrative")

dhs <- fread("project-data/historical_dhs.csv")
GP20 <- fread("project-data/GP20 headcounts.csv")
WDI <- WDI(country = "all", "NY.GNP.PCAP.CD", extra =T)
vnr.gaps <- fread("output/vnr P20 gap growth.csv")

###Circle sets
vnr.countries <- c("KHM","KWT","LIE","NRU","NZL","OMN","PLW","DZA","AZE","BIH","BFA","CMR","CAF","TCD","CHL","COG","CIV","HRV","SWZ","FJI","GHA","GTM","GUY","ISL","IDN","IRQ","ISR","KAZ","LSO","MRT","MUS","MNG","PAK","PHL","RWA","LCA","SRB","SLE","ZAF","TLS","TON","TUN","TUR","TKM","GBR","TZA","VUT")
vnr.countries.povertydata <- intersect(GP20$CountryCode,vnr.countries)
vnr.countries.povertydata.dhs <- intersect(vnr.countries.povertydata, dhs$iso3)
vnr.countries.povertydata.dhs.recent <- unique(subset(dhs, (iso3 %in% vnr.countries.povertydata.dhs))[,c(4,7)])



###Income group analysis
WDI <- subset(WDI, iso3c %in% vnr.countries & year == 2011)[,c(5,10)]
WDI <- rbind(WDI, data.frame(iso3c="SWZ",income="Lower middle income")) #WDI doesn't pick up Swaziland correctly
vnr.gaps <- merge(vnr.gaps, WDI, by.x="CountryCode", by.y="iso3c")
vnr.gaps.val <- vnr.gaps[, .(count=length(V1), increasing=sum(V1>=0), share=sum(V1>=0)/length(V1)), by=income]
write.csv(vnr.gaps.val, "output/VNR gaps by income.csv", row.names = F)
