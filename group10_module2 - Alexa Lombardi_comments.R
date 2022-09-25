#Alexa Lombardi - Project 2

#Installing packages and loading library
install.packages("features")
library(features)
library(tidyverse)
library(ggplot2)

#CPK: Best not to inlcude an active line that will install something.


#Loading data using read_csv() because we are using tidyverse
pseed<-read_csv("pseed.fin.amps.csv")
pseed.bl<-read_csv("pseed.lengths.csv")
speeds<-read_csv("pseed.calibration.csv")

#1 - Combine the code so that you can establish the pseed.wide data tibble
pseed.wide<-pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>% 
  group_by(date,frame)%>%
  mutate(amp.sum=sum(amp.bl))%>% 
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl)%>%
  mutate(amp.sum=L+R)%>% 
  print()

#CPK:Excellent!

# 2 - Compute the mean maximum* of all the amp.sums for each specific swimming speed for each fish
find.peaks<-function(x,y,mult=100){
  f<-fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}
pseed.peaks<-pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

#CPK: We did want to group by data (experiment) bc cycle will be unique to each one.[-1]

pseed.sum.max<-pseed.peaks%>%
  group_by(fish,bl.s)%>%
  summarise(amp.sum.mean=mean(amp.sum))%>% 
  print()

# 3 - Create a custom function that computes the standard error of the mean (SE)
sem = function(x){
  sd(x)/sqrt(length(x))
}
pseed.sum.max<-pseed.peaks%>% 
  group_by(fish,bl.s)%>% 
  summarise(amp.sum.se=sem(amp.sum))%>% 
  left_join(pseed.sum.max, by = c('fish','bl.s'))%>% 
  print()

#CPK: No need to do SE and mean in different operation. Define the se function earlier and do them at the same time. A la... . 

pseed.sum.max<-pseed.peaks%>% 
  group_by(fish,bl.s)%>% 
  summarise(amp.sum.mean=mean(amp.sum),amp.sum.se=sem(amp.sum))



# 4 - Plot the mean amp.sum vs specific swimming speed and add error bars
pseed.sum.max%>% 
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish)) + geom_point() + geom_smooth(method="lm") + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se,ymax=amp.sum.mean+amp.sum.se), width=0.5, colour="black")+theme_classic()

#CPK: Why not color error bars by fish, too

# 5 - Download, read as tibble, and merge with pseed.sum.max
met.specs<-read_csv('pseed.met.rate.csv')
met.n.amp<-pseed.sum.max%>% 
  left_join(met.specs, by = c('bl.s','fish'))%>% 
  print()

#CPK: ^No need to specify "by" when common columns exist in each data tibble.
# 6 - Plot the metabolic power output of each fish vs. mean maximum of amp.sum
met.n.amp%>% 
  ggplot(aes(x=met.rate,y=amp.sum.mean,col=fish)) + geom_point() + geom_smooth(method="lm") + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se,ymax=amp.sum.mean+amp.sum.se), width=0.5, colour="black")+theme_classic()

#Excellent work, Alex!