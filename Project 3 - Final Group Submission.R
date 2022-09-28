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
anole.log.PH.LM<-lm(HTotal~SVL+PH, anole.log)

#For perch diameter
anole.log.PD.LM<-lm(HTotal~SVL+ArbPD, anole.log)


#Step 3 - How perch height and diameter affect Hindlimb relation

#Adding residual
anole.log<-anole.log %>%
  mutate(resPH = residuals(anole.log.PH.LM), 
  resPD = residuals(anole.log.PD.LM))

#Perch height and diameter
anole.log%>%
  dplyr::select(Ecomorph2,resPH,resPD)%>%
  pivot_longer(cols=c("resPH","resPD"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y")+ylab("residual")


#Step 4 - Using BM model and model for phylogenetic least squares

#Reading anole.tre
anoletree<-read.tree("anole.tre")

#Model for hindlimb relationship and perch height
pgls.BM.PH <- gls(HTotal~SVL + PH, 
correlation = corBrownian(1, phy = anoletree, form =~Species), 
data= anole.log, method = 'ML')

#Model for hindlimb relationship and perch diameter
pgls.BM.PD <- gls(HTotal~SVL + ArbPD, 
correlation = corBrownian(1, phy = anoletree, form =~Species), 
data= anole.log, method = 'ML')

#Model for hindlimb relationship, perch height, and perch diameter
pgls.BM.PH.PD <- gls(HTotal~SVL + PH + ArbPD, 
correlation = corBrownian(1, phy = anoletree, form =~Species), 
data= anole.log, method = 'ML')


#Assesment of the fit of the models 
anole.Phylo.AIC <- MuMIn::AICc(pgls.BM.PH, pgls.BM.PD, pgls.BM.PH.PD)
aicw(anole.Phylo.AIC$AICc)

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
#Mutating anole.log to include best phylo plot and creating facet grid

anole.log <- anole.log %>% 
  mutate(phylo.residual=residuals(pgls.BM.PH.PD))
  
anole.log%>%
  dplyr::select(phylo.residual,Ecomorph2, resPH,resPD)%>%
  pivot_longer(cols=c("resPH","resPD"))%>%
  print%>%
  ggplot(aes(x = Ecomorph2,y = value)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y") + ylab("residual")

