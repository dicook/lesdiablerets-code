# Code originally from Halldor Bjornsson
library(ggplot2)
library(lubridate)
library(plyr)

#Read in data. Based on file provided, but taken directly off server.
CO2.bcs<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_bcs.csv", sep=",", skip=69)
colnames(CO2.bcs)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.bcs$lat<-23.3
CO2.bcs$lon<-(-110.2)
CO2.bcs$stn<-"bcs"

CO2.chr<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_chr.csv", sep=",", skip=69)
colnames(CO2.chr)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.chr$lat<-2.0
CO2.chr$lon<-(-157.3)
CO2.chr$stn<-"chr"

CO2.ker<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_ker.csv", sep=",", skip=69)
colnames(CO2.ker)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.ker$lat<-(-29.2)
CO2.ker$lon<-(-177.9)
CO2.ker$stn<-"ker"

CO2.kum<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_kum.csv", sep=",", skip=69)
colnames(CO2.kum)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.kum$lat<-19.5
CO2.kum$lon<-(-154.8)
CO2.kum$stn<-"kum"

CO2.ljo<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_ljo.csv", sep=",", skip=69)
colnames(CO2.ljo)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.ljo$lat<-32.9
CO2.ljo$lon<-(-117.3)
CO2.ljo$stn<-"ljo"

CO2.mlf<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_mlf.csv", sep=",", skip=69)
colnames(CO2.mlf)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.mlf$lat<-19.5
CO2.mlf$lon<-(-155.6)
CO2.mlf$stn<-"mlf"

CO2.nzd<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_nzd.csv", sep=",", skip=69)
colnames(CO2.nzd)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.nzd$lat<-(-41.4)
CO2.nzd$lon<-174.9
CO2.nzd$stn<-"nzd"

CO2.ptb<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_ptb.csv", sep=",", skip=69)
colnames(CO2.ptb)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.ptb$lat<-71.3
CO2.ptb$lon<-(-156.6)
CO2.ptb$stn<-"ptb"

CO2.sam<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_sam.csv", sep=",", skip=69)
colnames(CO2.sam)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.sam$lat<- (-14.2)
CO2.sam$lon<-(-170.6)
CO2.sam$stn<-"sam"

CO2.spo<-read.table("http://scrippsco2.ucsd.edu/data/flask_co2_and_isotopic/daily_co2/fldav_spo.csv", sep=",", skip=69)
colnames(CO2.spo)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.spo$lat<- (-90.0)
CO2.spo$lon<-0
CO2.spo$stn<-"spo"

# Combine
CO2.all<-rbind(CO2.bcs,CO2.chr,CO2.ker,CO2.kum,CO2.ljo,CO2.mlf,CO2.nzd,CO2.ptb,CO2.sam,CO2.spo)
# Fix dates
CO2.all$date<-as.Date(CO2.all$date)

# first plot; plot data that is not flagged. Reduce size of yticklabels
qplot(date, co2, data=subset(CO2.all, flg < 2), colour=stn, geom="line",xlab="Year",ylab="CO2 (ppm)") + 
		facet_wrap(~stn, ncol=1) + theme(axis.text.y=element_text(size = 6))
#ggsave("CO2_all.pdf")
#ggsave("CO2_all.png")
# This plot shows two things. One is that one station (bcs) has very short data. Another thing worth noting is that
# there is a great deal of variability in amplitude of the annual cycle. It seems that the stations with the 
# bigger annual cycle are in the north and the damped annual cycle stations are to the south.
# Check on this by reordering factors. Also drop station bcs.


#drop short station and data that does not pass QC
CO2.notall=subset(CO2.all, flg<2 & stn != "bcs")
# reorder factors from south to north
# reorder factors
CO2.notall$invlat=-1*CO2.notall$lat
CO2.notall$stn=reorder(CO2.notall$stn,CO2.notall$invlat)


# plot data that is not flagged (not use data with flag >1, see data manual)
qplot(date, co2, data=CO2.notall, colour=stn, geom="line",xlab="Year",ylab="CO2 (ppm)",
      main="CO2 concentrations at several measuring stations \n Order is from north (top) to south (bottom)") + 
      facet_wrap(~stn, ncol=1) + 
  theme(axis.text.y=element_text(size = 12), axis.text.x=element_text(size = 12), 
        axis.title=element_text(size = 12), legend.text=element_text(size = 12))
#ggsave("CO2_Ordered.pdf")
#ggsave("CO2_Ordered.png")

# This plot confirms that the seasonal cycle appears to be enhanced to the north.
# Examine this by plotting the time series of max,min year by year for each station

CO2.notall$year=year(CO2.notall$date)
co2_range <- ddply(CO2.notall, c("year","stn"),summarise,r=range(co2))

qplot(year,r,data=co2_range,color=stn,
		ylab="Range (ppm)",xlabel="Year",main="Range of CO2 seasonal cycle")
#ggsave("CO2_range.pdf")
#ggsave("CO2_range.png")

# This plot shows that there is a core of southerly stations (South Pole, New Zealand, Kergulen) 
# that have very low sesonal cycle, but other stations with larger seasonal cycle

#This is further confirmed by looking at the actual amplitude as a function of year for each station

#Calculate amplitude of the seasonal cycle per year 
co2_rd=ddply(co2_range,c("year","stn"),summarise,rd=diff(r))

# join latitude of station and the average amplitude of the CO2 cycle.
latamp=join(ddply(CO2.notall,c("stn"),summarize, unique(lat)),
				 ddply(co2_rd,c("stn"),summarize, mean(rd)),
				by=c("stn"),type="inner")
names(latamp) <- c("stn","lat","amplitude")

# plot average amplitude vs. latitude, and add a smoothing line. 
# Since stations are colored differently the smoothing line has to be added separately.
qplot(lat,amplitude,data=latamp,geom=c("point"),
		ylab="Amplitude (ppm)",xlab="Latitude (N positive)",
		main="Amplitude of CO2 seasonal cycle vs. latitude",color=stn) + geom_smooth(aes(group=1))
#ggsave("LatAmp.pdf")
#ggsave("LatAmp.png")

# This plot shows that the seasonal cycle is has amplitude below 3 ppm in the southern hemisphere
# but increases northwards in the nothern hemisphere.

# The sharp transition places limits on the air exchange on the seasonal timescale btw southern and
# northern hemisphere. 


#####ADDED ON THU 18 AUG###
# A discussion with Hadley on the error bars on the smoother lead to a new version of the LatAmp plot
# where instead of using average amplitude for each location the full distribution of amplitudes is used.
# A new data frame was created and a boxplot used. 
# This plot was created with a lot of help from Hadley and Dianne
stn_lats=ddply(CO2.notall,c("stn"),summarize, unique(lat))
names(stn_lats) <-c("stn","lat")
co2_rd_lats=join(co2_rd,stn_lats,by="stn",type="inner")
qplot(lat, rd, data = co2_rd_lats, geom = "boxplot",group=lat,color=stn, width =I(10),
	ylab="Amplitude (ppm)",xlab="Latitude (N positive)",
	main="Amplitude of CO2 seasonal cycle vs. latitude") + geom_smooth(aes(group=1))
	
#ggsave("LatAmpBoxplot.pdf")
#ggsave("LatAmpBoxplot.png")
	
##### end of added material

# How about the time evolution of the amplitude?
qplot(year,rd,data=co2_rd,geom=c("point","smooth"),color=stn,
		ylab="Amplitude (ppm)",xlabel="Year",main="Amplitude of CO2 seasonal cycle")
#ggsave("AmpYears.pdf")
#ggsave("AmpYears.png")

# Plot stations
world <- getMap(resolution = "low")
world.polys <- extractPolygons(world) # function defined in PISA data analysis
locations <- unique(CO2.all[,8:9])
locations$name <- unique(CO2.all$stn)
locations$name <- factor(locations$name, levels=locations$name[order(locations$lat)])
ggplot(data=world.polys) + geom_path(aes(x=X1, y=X2, order=order, group=group), colour=I("grey90")) + 
  geom_point(data=locations, mapping=aes(x=lon, y=lat, colour=name), size=5) + 
  geom_text(data=locations, mapping=aes(x=lon, y=lat, colour=name, label=name), vjust=-0.3, hjust=-0.5, size=6) +
  new_theme_empty + theme(legend.position="none", aspect.ratio=0.6) #+ coord_map()

# Examine times of peaks
CO2.ptb$date <- as.Date(CO2.ptb$date)
CO2.ptb$month <- month(CO2.ptb$date)
CO2.ptb$year <- year(CO2.ptb$date)
CO2.ptb.mave <- ddply(CO2.ptb[CO2.ptb$flg<2,], c("year","month"), summarise, co2=mean(co2, na.rm=T))
qplot(month, co2, data=CO2.ptb.mave, geom="line", colour=year, group=year) + scale_x_continuous(breaks=seq(1,12,3))
qplot(month, co2, data=CO2.ptb.mave, geom="line", colour=year, group=year) + 
  scale_x_continuous(breaks=seq(1,12,3)) + facet_wrap(~year)
CO2.ptb.mave[CO2.ptb.mave$year==1978,]

# Examine trend using loess for kum & spo
CO2.kum$date <- as.Date(CO2.kum$date)
CO2.kum$month <- month(CO2.kum$date)
CO2.kum$year <- year(CO2.kum$date)
CO2.kum.clean <- subset(CO2.kum, flg<2)
qplot(date, co2, data=CO2.kum.clean, alpha=I(0.5)) + geom_smooth(se=F, size=3)

CO2.spo$date <- as.Date(CO2.spo$date)
CO2.spo$month <- month(CO2.spo$date)
CO2.spo$year <- year(CO2.spo$date)
CO2.spo.clean <- subset(CO2.spo, flg<2)
qplot(date, co2, data=CO2.spo.clean, alpha=I(0.5)) + geom_smooth(se=F, size=3)

# Now for all
CO2.notall$year <- year(CO2.notall$date)
CO2.notall$month <- factor(month(CO2.notall$date))
qplot(date, co2, data=subset(CO2.notall, year<1995&year>1985), colour=stn, group=stn, geom="smooth", se=F) 

# Calculate the slope of the trend in CO2 emissions
CO2_trend1 <- dlply(CO2.notall, c("stn"),
                   function(df) {
                     lm(co2 ~ year, data = df)
                   })
CO2_trend2 <- dlply(CO2.notall, c("stn"),
                    function(df) {
                      lm(co2 ~ year+month, data = df)
                    })
CO2_trend3 <- dlply(CO2.notall, c("stn"),
                   function(df) {
                     lm(co2 ~ year+month+year^3, data = df)
                   })
CO2_trend4 <- dlply(CO2.notall, c("stn"),
                    function(df) {
                      nls(co2 ~ exp(a+b*year), data = df, start = list(a = 0, b = 0, c=0))
                    })


mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))


# list slope
print(ldply(CO2_trend1, coef))
print(ldply(CO2_trend2, coef))

# residuals
CO2_resid1 <- melt(llply(CO2_trend1, residuals))
colnames(CO2_resid1) <- c("resid", "stn")
CO2_resid1 <- CO2_resid1[order(CO2_resid1$stn),]
CO2_resid1$date <- CO2.notall$date
CO2_resid1$stn=reorder(CO2_resid1$stn,CO2.notall$invlat)
qplot(date, resid, data=CO2_resid1) + facet_wrap(~stn, ncol=1) + geom_smooth(se=F, size=3)
qplot(date, resid, data=CO2_resid1) + facet_wrap(~stn) + geom_smooth(se=F, size=3)

CO2_resid2 <- melt(llply(CO2_trend2, residuals))
colnames(CO2_resid2) <- c("resid", "stn")
CO2_resid2 <- CO2_resid2[order(CO2_resid2$stn),]
CO2_resid2$date <- CO2.notall$date
qplot(date, resid, data=CO2_resid2) + facet_wrap(~stn)

CO2_resid3 <- melt(llply(CO2_trend3, residuals))
colnames(CO2_resid3) <- c("resid", "stn")
CO2_resid3 <- CO2_resid3[order(CO2_resid3$stn),]
CO2_resid3$date <- CO2.notall$date
qplot(date, resid, data=CO2_resid3) + facet_wrap(~stn)

CO2_resid4 <- melt(llply(CO2_trend4, residuals))
colnames(CO2_resid4) <- c("resid", "stn")
CO2_resid4 <- CO2_resid4[order(CO2_resid4$stn),]
CO2_resid4$date <- CO2.notall$date
qplot(date, resid, data=CO2_resid4) + facet_wrap(~stn)





