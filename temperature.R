library(reshape)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(stringr)
library(nullabor)

# Let's pull in records for near LesDiablerets
# Name  COL DU GRAND ST BERNARD, IT
# ID	GHCND:SZ000006717 (Geneve is 8440, Lugano 9480, Zurich 3700, Basel 1940, Saentis 2220)
# Lat/Lon	45.8667, 7.1667
# PERIOD OF RECORD
# Start/End	1864-01-01 to 2014-12-01

ch <- read.fwf("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/SZ000006717.dly", 
                     c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
ch.g <- read.fwf("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/SZ000008440.dly", 
               c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
ch.l <- read.fwf("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/SZ000009480.dly", 
               c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
ch.z <- read.fwf("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/SZ000003700.dly", 
                 c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
ch.b <- read.fwf("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/SZ000001940.dly", 
                 c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
ch.s <- read.fwf("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/SZ000002220.dly", 
                 c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)

ch.maxt <- ch[ch$V4 == "TMAX", c(1:3, seq(5, 128, 4))]
colnames(ch.maxt) <- c("stn", "year", "month", 1:31)
ch.maxt.m <- melt(ch.maxt, id.vars=c("stn", "year", "month"))
ch.maxt.m$value[ch.maxt.m$value== -9999] <- NA
colnames(ch.maxt.m)[4] <- "day"
colnames(ch.maxt.m)[5] <- "temp"
ch.maxt.m$temp <- ch.maxt.m$temp/10
summary(ch.maxt.m)

ch.maxt.m2 <- arrange(ch.maxt.m, year, month, day)
ch.maxt.m2$date <- as.Date(paste(ch.maxt.m2$year,"/",ch.maxt.m2$month,"/",
                                 ch.maxt.m2$day, sep=""), format="%Y/%m/%d")

qplot(month, temp, data=ch.maxt.m)
qplot(factor(month), temp, data=ch.maxt.m, geom="boxplot")
qplot(year, temp, data=ch.maxt.m)
qplot(date, temp, data=ch.maxt.m2)

ch.maxt.m2.post1950 <- subset(ch.maxt.m2, year>1950)
qplot(date, temp, data=ch.maxt.m2.post1950, alpha=I(0.1)) + geom_smooth(se=F, size=2)
qplot(date, temp, data=ch.maxt.m2.post1950, geom="smooth", se=F, size=I(2)) 

ch.maxt.m2.post1950$tempsmooth <- ch.maxt.m2.post1950$temp
sm3 <- smooth(ch.maxt.m2.post1950$temp[!is.na(ch.maxt.m2.post1950$temp)], kind="3R")
sm3 <- runmed(ch.maxt.m2.post1950$temp[!is.na(ch.maxt.m2.post1950$temp)], k=9)
sm3 <- runmed(ch.maxt.m2.post1950$temp[!is.na(ch.maxt.m2.post1950$temp)], k=5001)
ch.maxt.m2.post1950$tempsmooth[!is.na(ch.maxt.m2.post1950$tempsmooth)] <- sm3
qplot(date, temp, data=ch.maxt.m2.post1950, alpha=I(0.1)) + geom_line(aes(y=tempsmooth), size=2) 
qplot(date, tempsmooth, data=ch.maxt.m2.post1950, geom="line", size=I(2)) 

qplot(year, temp, data=lineup(null_permute("year"), ch.maxt.m2.post1950), geom="smooth", se=F) + 
  facet_wrap(~.sample, ncol=4) 

ch.mint <- ch[ch$V4 == "TMIN", c(1:3, seq(5, 128, 4))]
colnames(ch.mint) <- c("stn", "year", "month", 1:31)
ch.mint.m <- melt(ch.mint, id.vars=c("stn", "year", "month"))
ch.mint.m$value[ch.mint.m$value== -9999] <- NA
colnames(ch.mint.m)[4] <- "day"
colnames(ch.mint.m)[5] <- "temp"
ch.mint.m$temp <- ch.mint.m$temp/10
summary(ch.mint.m)

ch.mint.m2 <- arrange(ch.mint.m, year, month, day)
ch.mint.m2$date <- as.Date(paste(ch.mint.m2$year,"/",ch.mint.m2$month,"/",
                                 ch.mint.m2$day, sep=""), format="%Y/%m/%d")

qplot(month, temp, data=ch.mint.m)
qplot(factor(month), temp, data=ch.mint.m, geom="boxplot")
qplot(year, temp, data=ch.mint.m)
qplot(date, temp, data=ch.mint.m2)

ch.mint.m2.post1950 <- subset(ch.mint.m2, year>1950)
qplot(date, temp, data=ch.mint.m2.post1950, alpha=I(0.1)) + geom_smooth(se=F, size=2)
qplot(date, temp, data=ch.mint.m2.post1950, geom="smooth", se=F, size=I(2)) 
qplot(year, temp, data=lineup(null_permute("year"), ch.mint.m2.post1950), geom="smooth", se=F) + 
  facet_wrap(~.sample, ncol=4) 

ch.mint.m2.post1950$tempsmooth <- ch.mint.m2.post1950$temp
sm3 <- smooth(ch.mint.m2.post1950$temp[!is.na(ch.mint.m2.post1950$temp)])
sm3 <- runmed(ch.mint.m2.post1950$temp[!is.na(ch.mint.m2.post1950$temp)], k=9)
sm3 <- runmed(ch.mint.m2.post1950$temp[!is.na(ch.mint.m2.post1950$temp)], k=5001)
ch.mint.m2.post1950$tempsmooth[!is.na(ch.mint.m2.post1950$tempsmooth)] <- sm3
qplot(date, temp, data=ch.mint.m2.post1950, alpha=I(0.1)) + geom_line(aes(y=tempsmooth), size=2) 
qplot(date, tempsmooth, data=ch.mint.m2.post1950, geom="line", size=I(2)) 

ch.prcp <- ch[ch$V4 == "PRCP", c(1:3, seq(5, 128, 4))]
colnames(ch.prcp) <- c("stn", "year", "month", 1:31)
ch.prcp.m <- melt(ch.prcp, id.vars=c("stn", "year", "month"))
ch.prcp.m$value[ch.prcp.m$value== -9999] <- NA
colnames(ch.prcp.m)[4] <- "day"
colnames(ch.prcp.m)[5] <- "prcp"
ch.prcp.m$prcp <- ch.prcp.m$prcp/10
summary(ch.prcp.m)

ch.prcp.m2 <- arrange(ch.prcp.m, year, month, day)
ch.prcp.m2$date <- as.Date(paste(ch.prcp.m2$year,"/",ch.prcp.m2$month,"/",
                                 ch.prcp.m2$day, sep=""), format="%Y/%m/%d")

qplot(month, prcp, data=ch.prcp.m) + scale_y_sqrt()
qplot(factor(month), prcp, data=ch.prcp.m, geom="boxplot") + scale_y_sqrt()
qplot(year, prcp, data=ch.prcp.m) + scale_y_sqrt()
qplot(date, prcp, data=ch.prcp.m2) + scale_y_sqrt()

ch.prcp.m2.post1933 <- subset(ch.prcp.m2, year>1925)
qplot(factor(month), prcp, data=ch.prcp.m2.post1933, geom="boxplot") + scale_y_sqrt()

ch.prcp.m2.post1933.mth <- summarise(group_by(ch.prcp.m2.post1933, year, month), sp = sum(prcp, na.rm=T), 
                                     n = length(prcp[!is.na(prcp)]))
ch.prcp.m2.post1933.mth$adjsum <- ch.prcp.m2.post1933.mth$sp/ch.prcp.m2.post1933.mth$n*30.5
ch.prcp.m2.post1933.mth$time <- (ch.prcp.m2.post1933.mth$year-1934)*12+ch.prcp.m2.post1933.mth$month
qplot(time, adjsum, data=ch.prcp.m2.post1933.mth, alpha=I(0.5)) + scale_y_sqrt() + geom_smooth(se=F, size=2)
qplot(factor(month), adjsum, data=ch.prcp.m2.post1933.mth, geom="boxplot", ylab="Monthly precip total (sqrt scale)") + scale_y_sqrt() 

ch.prcp.m2.post1933.mth$date <- as.Date(paste("01/",ch.prcp.m2.post1933.mth$month,"/",ch.prcp.m2.post1933.mth$year, sep=""), 
                                        format="%d/%m/%Y")
qplot(date, adjsum, data=ch.prcp.m2.post1933.mth, alpha=I(0.5), ylab="Monthly precip total (sqrt scale)") + scale_y_sqrt() + geom_smooth(se=F, size=2)

ch.prcp.m2.post1933.mth$prcpsmooth <- ch.prcp.m2.post1933.mth$adjsum
sm3 <- smooth(ch.prcp.m2.post1933.mth$adjsum)
sm3 <- runmed(ch.prcp.m2.post1933.mth$adjsum, k=97, endrule="constant")
ch.prcp.m2.post1933.mth$prcpsmooth <- sm3
qplot(date, adjsum, data=ch.prcp.m2.post1933.mth, alpha=I(0.5), ylab="Monthly precip total (sqrt scale)") + 
  scale_y_sqrt() + geom_line(aes(y=prcpsmooth),size=2)
qplot(date, adjsum, data=ch.prcp.m2.post1933.mth, alpha=I(0.5), ylab="Monthly precip total (sqrt scale)") + 
  scale_y_sqrt() + geom_smooth(se=F, size=2)

qplot(date, adjsum, data=ch.prcp.m2.post1933.mth, geom="line") + scale_y_sqrt()

qplot(year, adjsum, data=ch.prcp.m2.post1933.mth) + scale_y_sqrt()

qplot(year, adjsum, data=lineup(null_permute("year"), ch.prcp.m2.post1933.mth), geom="smooth", se=F) + 
  facet_wrap(~.sample, ncol=4) + scale_y_sqrt()

qplot(factor(year), adjsum, data=lineup(null_permute("year"), ch.prcp.m2.post1933.mth)) + 
  facet_wrap(~.sample, ncol=4) + scale_y_sqrt()


# Blank for so people have some idea where labels are
qplot(year, temp, data=jan.max.lineup, xlab="x", ylab="y", size=I(0)) + 
  facet_wrap(~.sample, ncol=4) + 
  theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank())  
ggsave("lineup-form.pdf", width=4, height=3)

# Look at a different station - Geneve
ch.g.maxt <- ch.g[ch.g$V4 == "TMAX", c(1:3, seq(5, 128, 4))]
colnames(ch.g.maxt) <- c("stn", "year", "month", 1:31)
ch.g.maxt.m <- melt(ch.g.maxt, id.vars=c("stn", "year", "month"))
ch.g.maxt.m$value[ch.maxt.m$value== -9999] <- NA
colnames(ch.g.maxt.m)[4] <- "day"
colnames(ch.g.maxt.m)[5] <- "temp"
ch.g.maxt.m$temp <- ch.g.maxt.m$temp/10
summary(ch.g.maxt.m)
ch.g.maxt.m <- subset(ch.g.maxt.m, temp > -250)

ch.g.maxt.m2 <- arrange(ch.g.maxt.m, year, month, day)
ch.g.maxt.m2$date <- as.Date(paste(ch.g.maxt.m2$year,"/",ch.g.maxt.m2$month,"/",
                                 ch.g.maxt.m2$day, sep=""), format="%Y/%m/%d")
ch.g.maxt.m2 <- subset(ch.g.maxt.m2, year>1965&year<2010)

#qplot(date, temp, data=ch.g.maxt.m2 alpha=I(0.1)) + geom_smooth(se=F, size=2)
qplot(year, temp, data=lineup(null_permute('year'), ch.g.maxt.m2), 
            geom="smooth", se=F, size=I(2), xlab="", ylab="") + 
        facet_wrap(~.sample, ncol=5) + 
        theme_bw() + 
        theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank()) 
# qplot(date, temp, data=ch.g.maxt.m2)

