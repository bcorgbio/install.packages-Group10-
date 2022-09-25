#setup library/data
library(ggplot2)
library(tidyverse)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed %>%
  left_join(speeds,by=c("speed"="vol")) %>%
  left_join(pseed.bl,by="fish")
  
  pseed2 <- pseed2 %>%
  mutate(bl.s=cm.s/bl)
  
  pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)
  
  #mean-max amp
  find.peaks <- function(x,y,mult=100){
    f <- fget(features(x = x,y=y*mult))[2:3]%>% 
      as_tibble() %>% 
      filter(curvature<0)%>% 
      mutate(peaks=round(crit.pts,0))
    return(f$peaks) 
  }

  pseed.max <- pseed.wide%>%
    group_by(fish,speed)%>%
    mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
    filter(peak==T)  

pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  mutate(max.amp.sum = T, amp.sum.mean = mean(max.amp.sum))

pseed.sum.max<- pseed.max %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se = stderror(amp.sum)) 

# custom function to compute SE // error bars
stderror <- function(x) sd(x)/sqrt(length(x))

pseed.sum.max

pseed.sum.max %>% 
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+
  geom_point()+geom_smooth(method="lm")+
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.5, colour="black")+
  theme_classic()
  
#Merge with new pseed.sum.max tibble 
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.sum.max$bl.s <- as.numeric(as.character(pseed.sum.max$bl.s))
pseed.met.rate$bl.s <- as.numeric(as.character(pseed.met.rate$bl.s))
pseed.met.rate2 <- pseed.met.rate %>% 
  select(fish,bl.s,met.rate) %>% 
  print()
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate2,by=c("fish"="fish","bl.s"="bl.s"))%>%
  print()
pseed.sum.max.met <- pseed.sum.max%>%
  left_join(pseed.met.rate2,by=c("fish"="fish","bl.s"="bl.s"))%>%
  print()
summarize(amp.met.rate=mean(met.rate))

#ggplot to plot metabolic power vs. mean max
pseed.sum.max %>% ggplot(pseed.sum.max.met, aes(x=amp.sum.mean, y=met.rate, colour=fish)) + geom_point()

