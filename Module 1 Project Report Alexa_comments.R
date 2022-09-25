#Alexa Lombardi - Project 1
library(ggplot2)
library(tidyverse)

#1 - A dat variable containing the scales dataset 
dat<-read.csv("scales.csv")

#2 - A line of code which reports the class of each column in the dataset
sapply(dat,class)

#3 - A line of code which reports the dimensions of the dataset
dim(dat) 

#4 - Code that produces a summary of the number of scales punctured for each species
dat %>% count(species)

#5 - Code that produces a summary of the number of specimens sampled for each species
species <- levels(as.factor(dat$species))
dat %>% 
  count(species,specimen) %>%
  count(species,name = "n.specimens")

#6 - Code that produces a PDF file containing 6 figures, one for each species that includes a boxplot of puncture force verus quadrant
pdf("species.quadrant.alexa.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
} 
dev.off()

#CPK: this is unneeded VVV
list.files(pattern=".pdf")

#Excellent work. Spot on!