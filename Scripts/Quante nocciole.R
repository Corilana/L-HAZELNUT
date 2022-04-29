#probability of nuts to be
#what is the length of children?
#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))
#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(nnet)
library(effects)

lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
ap=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_APICALS.csv"))

TOT=rbind(lat,ap)
TOT=TOT[TOT$fate=="M",]

#1:
glm_box1 = glm(nu~Length+Length.node.+rank_node+distance_abs+tot_buds+from_+position, data = TOT, family = "poisson")
summary(glm_box1)
#2:-from_
glm_box1 = glm(nu~Length+Length.node.+rank_node+distance_abs+tot_buds+position, data = TOT, family = "poisson")
summary(glm_box1)
#3:-position
glm_box1 = glm(nu~Length+Length.node.+rank_node+distance_abs+tot_buds, data = TOT, family = "poisson")
summary(glm_box1)
#4:-length.node
glm_box1 = glm(nu~Length+rank_node+distance_abs+tot_buds, data = TOT, family = "poisson")
summary(glm_box1)
#5:-rank_node
glm_box1 = glm(nu~Length+distance_abs+tot_buds, data = TOT, family = "poisson")
summary(glm_box1)
#guardiamo se length si può togliere
source("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral/glm/permutation_glm.R")
permutation(dip = "nu", predictors = c("distance_abs", "tot_buds"), perm = "Length", data = TOT, family = "poisson")
#no non si può
plot(allEffects(glm_box1))

#proviamo a fare percentuale di 