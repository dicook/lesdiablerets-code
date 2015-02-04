library(XML)
library(scrapeR)
library(plyr)

getMatchIDs <- function(daystart,ndays) {
  df <- data.frame(day=NULL, id=NULL)
  for(i in daystart:ndays) {
    cat(i, "\n")
    url_day <- paste("http://www.ausopen.com/en_AU/scores/completed_matches/day", i, ".html", sep="")
    doc <- scrape(url_day)[[1]]
    id <- as.numeric(sapply(as.character(sapply(getNodeSet(doc, path='//*[@id="results"]/div/div/div[@class="linkItem matchstats"]/a'), xmlAttrs)), function(j) as.numeric(gsub("ms.html", "", strsplit(j, "/")[[1]][6],fixed=TRUE))))
    
    if(length(id)>0) df <- rbind(df, data.frame(day=i, id=id[which(id<=9999)]))
  }
  df
}

matchesToGet <- getMatchIDs(6, 14)

url <- paste("http://www.ausopen.com/en_AU/scores/stats/day", matchesToGet$day, "/", 
             matchesToGet$id, "ms.html#matchStats", sep="")

formatData <- function(node, id, day){
  df <- suppressWarnings(data.frame(
    rbind(
      sapply(getNodeSet(node, path="//*/div[@id='summary']/*/div[@id='match-stats']/div[@class='statsTable']/div[@class='row']/div[@class='team team1']"), xmlValue),
      sapply(getNodeSet(node, path="//*/div[@id='summary']/*/div[@id='match-stats']/div[@class='statsTable']/div[@class='row']/div[@class='team team2']"), xmlValue)), 
    matchid=id, day=day, stringsAsFactors=FALSE))
  names(df) <- gsub(" ", ".", c(unique(sapply(getNodeSet(node, path="//*/div[@id='summary']/*/div[@id='match-stats']/div[@class='statsTable']/div[@class='row']/div[@class='statlabel bg']"), xmlValue)), "matchid", "day"), fixed=TRUE)
  df$Name <- c(xmlValue(getNodeSet(node, path="//*/div[@id='summary']/*/div[@id='match-stats']/div[@class='statsTable']/div[@class='header']/div[@class='team team1']")[[1]]), 
               xmlValue(getNodeSet(node, path="//*/div[@id='summary']/*/div[@id='match-stats']/div[@class='statsTable']/div[@class='header']/div[@class='team team2']")[[1]]))
  if(id>3000) df$PlayerURL <- NA else
    df$PlayerURL <- paste("http://www.australianopen.com",  c(xmlAttrs(getNodeSet(node, path="//*[@id='team1info']/div[@class='name singles']/a")[[1]]), xmlAttrs(getNodeSet(node, path="//*[@id='team2info']/div[@class='name singles']/a")[[1]])), "#st", sep="")
  
  df
}

getMatchData <- function(url){
  doc <- sapply(url, scrape)
  #table <- sapply(doc, function(i) getNodeSet(i, path="//*[@class='statsTable']/*[@class='row']"))
  table <- lapply(doc, function(i) getNodeSet(i, path="//*/div[@id='summary']/*/div[@id='match-stats']/div[@class='statsTable']/div[@class='row']"))
  ids <- sapply(url, function(i) as.numeric(substr(rev(strsplit(i, "/")[[1]])[1], 1, 4)))
  days <- sapply(url, function(i) as.numeric(substr(rev(strsplit(i, "/")[[1]])[2], 4, 4)))    
  
  df <- rbind.fill(lapply(1:length(table), function(i) formatData(node = table[[i]][[1]], id=ids[i], day=days[i])))
  
  df
}

matchData <- getMatchData(url)

suppressWarnings(names(matchData)[which(!is.na(as.numeric(sapply(names(matchData), 
        function(i) strsplit(i, "")[[1]][1]))))] <- paste("X", 
            names(matchData)[which(!is.na(as.numeric(sapply(names(matchData), 
        function(i) strsplit(i, "")[[1]][1]))))], sep=""))     
#matchData <- rbind.fill(old.data, matchData)
#write.csv(matchData, "./tennisstats/data/match-stats/2013/AUS-2013/AUS-raw-match-all-1.csv", row.names=FALSE)
#matchData <- read.csv("./tennisstats/data/match-stats/2013/AUS-2013/AUS-raw-match-all-1.csv", stringsAsFactors=FALSE)

formatPlayerStats <- function(url2){
  doc <- scrape(url2)
  df <- getNodeSet(doc[[1]], path='//*[@id="st"]/*[@class="module stats"]/table')
  if(length(df)==0) return(NULL)
  
  df <- melt(readHTMLTable(df[[1]], header=TRUE,stringsAsFactors=FALSE), 1)
  df$name <- strsplit(xmlAttrs(getNodeSet(doc[[1]], path='//*[@id="ausPage"]/htsearch')[[1]]), ":")$content[2]
  df$id <- gsub(".html#st", "", strsplit(url2, "/")[[1]][7])
  names(df) <- c("StatName", "Round", "value", "name", "id")
  
  df
}

write.csv(matchData, file="tennis/aus2015.csv", row.names=F)

extractpct <- function(x) {
#x <- matchData$X1st.serves.in
  x <- strsplit(x, split="(", fixed=TRUE)
  x <- unlist(x)
  x <- as.numeric(substr(x[seq(2, length(x), 2)], 1, 2))
}
matchData$X1st.serves.in <- extractpct(matchData$X1st.serves.in)
matchData$X1st.serve.points.won <- extractpct(matchData$X1st.serve.points.won)
matchData$X2nd.serve.points.won <- extractpct(matchData$X2nd.serve.points.won)
matchData$Net.points.won[is.na(matchData$Net.points.won)] <- "0/1 (0 %)"
matchData$Net.points.won <- extractpct(matchData$Net.points.won)
matchData$Break.points.won[is.na(matchData$Break.points.won)] <- "0/1 (0 %)"
matchData$Break.points.won <- extractpct(matchData$Break.points.won)
matchData$Receiving.points.won[is.na(matchData$Receiving.points.won)] <- "0/1 (0 %)"
matchData$Receiving.points.won <- extractpct(matchData$Receiving.points.won)

matchData$gender <- "atp"
matchData$gender[grep("wta",matchData$PlayerURL)] <- "wta"

matchData$Fastest.serve <- gsub(" KMH", "", matchData$Fastest.serve)
matchData$Fastest.serve <- as.numeric(matchData$Fastest.serve)
matchData$Average.1st.serve.speed <- gsub(" KMH", "", matchData$Average.1st.serve.speed)
matchData$Average.1st.serve.speed <- as.numeric(matchData$Average.1st.serve.speed)
matchData$Average.2nd.serve.speed <- gsub(" KMH", "", matchData$Average.2nd.serve.speed)
matchData$Average.2nd.serve.speed <- as.numeric(matchData$Average.2nd.serve.speed)

matchData$Aces <- as.numeric(matchData$Aces)
matchData$Double.faults <- as.numeric(matchData$Double.faults)
matchData$Winners <- as.numeric(matchData$Winners)
matchData$Unforced.errors <- as.numeric(matchData$Unforced.errors)
matchData$Total.points.won <- as.numeric(matchData$Total.points.won)

write.csv(matchData, file="tennis/aus2015.csv", row.names=F, quote=F)

# Summarize
sort(table(matchData$Name))
matchData[matchData$Name == "Tomas Berdych",]
# Only keep to round 3
matchData <- read.csv("tennis/aus2015.csv")
matchData$Round <- ddply(matchData, "Name", summarise, round = 1:length(Aces))[,2]
round3matchData <- subset(matchData, Round < 4)

averagematchData <- ddply(round3matchData, "Name", summarise, 
                  Aces = mean(Aces, na.rm=T),
                  Double.faults = mean(Double.faults, na.rm=T),
                  X1st.serves.in = mean(X1st.serves.in, na.rm=T),
                  X1st.serve.points.won = mean(X1st.serve.points.won, na.rm=T),
                  X2nd.serve.points.won = mean(X2nd.serve.points.won, na.rm=T),
                  Fastest.serve = mean(Fastest.serve, na.rm=T),
                  Average.1st.serve.speed = mean(Average.1st.serve.speed, na.rm=T),
                  Average.2nd.serve.speed = mean(Average.2nd.serve.speed, na.rm=T),
                  Net.points.won = mean(Net.points.won[Net.points.won>0]),
                  Break.points.won = mean(Break.points.won[Break.points.won>0]),
                  Receiving.points.won = mean(Receiving.points.won),
                  Winners = mean(Winners),
                  Unforced.errors = mean(Unforced.errors, na.rm=T), 
                  Total.points.won = mean(Total.points.won),
                  gender = gender[1])
Max.Round <- ddply(matchData, "Name", summarise, round = length(Aces))
averagematchData <- merge(averagematchData, Max.Round)

write.csv(averagematchData, file="tennis/aus2015-round3ave.csv", row.names=F, quote=F)

library(ggplot2)
library(nullabor)
averagematchData <- read.csv("tennis/aus2015-round3ave.csv")
atp <- subset(averagematchData, gender == "atp")
wta <- subset(averagematchData, gender == "wta")

qplot(Aces, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Double.faults, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(X1st.serves.in, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(X1st.serve.points.won, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(X2nd.serve.points.won, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Fastest.serve, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Average.1st.serve.speed, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Average.2nd.serve.speed, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Receiving.points.won, round, data=lineup(null_permute("round"), wta[wta$Receiving.points.won>0,]), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Winners, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)
qplot(Unforced.errors, round, data=lineup(null_permute("round"), wta), size=I(5), alpha=I(0.5)) + facet_wrap(~.sample)

pbinom(5, 30, prob=1/20, lower.tail=FALSE)
