library(ggplot2)
library(tidyverse)

#CPK: No need to set the wd if you're working in an R project

setwd("~jackkloster/Desktop/install.packages-Group10-/scales")
dat <- read.csv("scales.csv")
dim(dat)
sapply(dat,class)
species.n<- dat %>% 
  group_by(species) %>%
  summarise(n = n())
species.n
dat %>% 
  count(species,specimen) %>%
   print() %>%
  count(species,name = "n.specimens")

#CPK: This is unneeded too. You needed only to produce the PDFs. [-1]

for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}


dev.off()


#CPK: This is unneed too. [-1]
list.files(pattern=".pdf")

#CPK: Solid work. Just be sute to include what's needed an no more or less. And be sure to push/commit, not use file upload [-1]. It's important part of our workflow.

