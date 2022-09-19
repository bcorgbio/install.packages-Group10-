library(ggplot2)
library(tidyverse)

#CPK: No need to set the wd if you're working in an R project
setwd("~/Desktop/Group Project/Project1")
dat <- read.csv("scales.csv")

dim(dat)
head(dat)

sapply(dat,class) #does lines 10-13 at once

dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species

species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")


#CPK: This is unneed too. You needed only to produce the PDFs. [-1]
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}


pdf("Peters Species Quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: This is unneed too. [-1]
list.files(pattern=".pdf")


#CPK: Solid work. Just be sute to include what's needed an no more or less.