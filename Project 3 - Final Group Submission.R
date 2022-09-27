#Project 3 - Group 10

#Loading Libraries 
library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)
library(phangorn)

#Loading Files
anole.eco<-read_csv("anole.eco.csv")
anole<-read_csv("anole.dat.csv")


#Step 1 - setting up anole.log tibble

anole.log<-anole %>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)


#Step 2 - creating linear moddles for perch height and diameter

#For perch height
anole.log.ph.lm<-lm(HTotal~SVL+PH, anole.log)

#For perch diameter
anole.log.pd.lm<-lm(HTotal~SVL+ArbPD, anole.log)


#Step 3 - How perch height and diameter affect Hindlimb relation

#Adding residual
anole.log<-anole.log %>%
  mutate(res.ph = residuals(anole.log.ph.lm), 
  res.pd = residuals(anole.log.pd.lm))

#Perch height and diameter
anole.log%>%
  dplyr::select(Ecomorph2,res.ph,res.pd)%>%
  pivot_longer(cols=c("res.ph","res.pd"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y")+ylab("residual")


#Step 4 - Using BM model and model for phylogenetic least squares

anole.tree<-read.tree("anole.tre")

#Model for hindlimb relationship and perch height
pgls.BM.ph <- gls(HTotal~SVL + PH, 
  correlation = corBrownian(1, phy = anole.tree, form =~Species), 
  data= anole.log, method = 'ML')

#Model for hindlimb relationship and perch diameter
pgls.BM.pd <- gls(HTotal~SVL + ArbPD, 
  correlation = corBrownian(1, phy = anole.tree, form =~Species), 
  data= anole.log, method = 'ML')

#Model for hindlimb relationship, perch height, and perch diameter
pgls.BM.ph.pd <- gls(HTotal~SVL + PH + ArbPD, 
  correlation = corBrownian(1, phy = anole.tree, form =~Species), 
  data= anole.log, method = 'ML')


#Assesment of the fit of the models 
anole.phylo.aic <- MuMIn::AICc(pgls.BM.ph, pgls.BM.pd, pgls.BM.ph.pd)
aicw(anole.phylo.aic$AICc)

# model     fit        delta        w
#   1   -64.77956   10.746149 0.003241168
#   2   -73.81081    1.714901 0.296354947
#   3   -75.52571    0.000000 0.698551002

#This phylogenetically correct regression model of perch height, diameter.
#Traits evolving under BM as the best fit.


#The results of the AICc and AICw indicate that the variables together are a 
#valid means for predicting hind limb length. This can be concluded as the 
#model with both variables has the delta value of 0.


#Step 6 - Creating our own plot
#Mutating anole.log to include best phylo plot
anole.log <- anole.log %>%
  mutate(phylo.res = residuals(pgls.BM.ph.pd))

#Creating Facet grid of variables 
anole.log %>% 
  dplyr::select(Ecomorph2,res.ph,res.pd,phylo.res) %>%
  pivot_longer(cols = c("res.ph","res.pd", "phylo.res"))%>%
  print%>%
  ggplot(aes(x = Ecomorph2,y = value)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y") + ylab("residual")


