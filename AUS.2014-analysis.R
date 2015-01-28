aus <- read.csv("tennis/AUS.2014.addition.csv")
sort(table(aus$name))

aus[aus$name=="Roger Federer",]
aus[aus$name=="Stanislas Wawrinka",]

aus.men <- aus[aus$match.type==1,]
aus.women <- aus[aus$match.type==2,]

library(ggplot2)
qplot(round, Winners, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Winners") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Unforced.Errors, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Unforced Errors") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Total.Double.Faults, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Double Faults") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Total.Aces, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Aces") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, First.Serves.In, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("First Serves In") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Winning.Pct.on.1st.Serve, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("First Serve Win %") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Winning.Pct.on.2nd.Serve, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Second Serve Win %") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Receiving.Points.Won, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Receiving Points Won") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

qplot(round, Fastest.Serve.Speed, data=aus.men, geom="smooth", size=I(3), se=F) + 
  xlab("Round") + ylab("Fastest Serve Speed") +
  geom_line(aes(group=name), alpha=0.3) + theme_bw()

# Look at averages by player
library(dplyr)
aus.men.av <- summarise(group_by(aus.men, name), Winners=mean(Winners, na.rm=T),
                    Unforced.Errors=mean(Unforced.Errors, na.rm=T), 
                    Double.Faults=mean(Total.Double.Faults, na.rm=T),
                    Aces=mean(Total.Aces, na.rm=T), 
                    First.Serves.In=mean(First.Serves.In, na.rm=T),
                    Winning.Pct.on.1st.Serve=mean(Winning.Pct.on.1st.Serve, na.rm=T),
                    Winning.Pct.on.2nd.Serve=mean(Winning.Pct.on.2nd.Serve, na.rm=T),
                    Receiving.Points.Won=mean(Receiving.Points.Won, na.rm=T),
                    Fastest.Serve.Speed=mean(Fastest.Serve.Speed, na.rm=T),
                    Round = max(round, na.rm=T))
dim(aus.men.av)

qplot(Winners, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Unforced.Errors, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Aces, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Double.Faults, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(First.Serves.In, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Winning.Pct.on.1st.Serve, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Winning.Pct.on.2nd.Serve, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Receiving.Points.Won, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

qplot(Fastest.Serve.Speed, Round, data=aus.men.av, size=I(5), alpha=I(0.5)) + 
  theme_bw() + theme(aspect.ratio=1)

write.csv(aus.men.av, file="tennis/AUS.2014.ave.csv", row.names=F)

# Average stats for the first three rounds only, and examine 
# these in relation to round reached - look at women
aus.women.r3 <- subset(aus.women, round < 4)
aus.women.av.r3 <- summarise(group_by(aus.women.r3, name), Winners=mean(Winners, na.rm=T),
                        Unforced.Errors=mean(Unforced.Errors, na.rm=T), 
                        Double.Faults=mean(Total.Double.Faults, na.rm=T),
                        Aces=mean(Total.Aces, na.rm=T), 
                        First.Serves.In=mean(First.Serves.In, na.rm=T),
                        Winning.Pct.on.1st.Serve=mean(Winning.Pct.on.1st.Serve, na.rm=T),
                        Winning.Pct.on.2nd.Serve=mean(Winning.Pct.on.2nd.Serve, na.rm=T),
                        Receiving.Points.Won=mean(Receiving.Points.Won, na.rm=T),
                        Fastest.Serve.Speed=mean(Fastest.Serve.Speed, na.rm=T))

x <- summarise(group_by(aus.women, name), round = max(round, na.rm=T))
aus.women.av.r3 <- left_join(aus.women.av.r3, x)

library(nullabor)
qplot(Winners, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Unforced.Errors, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Double.Faults, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Aces, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(First.Serves.In, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Winning.Pct.on.1st.Serve, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Winning.Pct.on.2nd.Serve, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Receiving.Points.Won, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  
qplot(Fastest.Serve.Speed, round, data=lineup(null_permute('round'), aus.women.av.r3), 
      xlab="", ylab="", alpha=I(0.5), size=I(3)) + 
  facet_wrap(~.sample, ncol=5) + 
  theme_bw() + 
  theme(aspect.ratio=1, axis.text = element_blank(), axis.title = element_blank())  




