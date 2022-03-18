#GLM: does that rank bear a sylleptic?
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)

#metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
s=grep("shoot_type",colnames(met))#store the column with the sylleptic info
met[met$shoot_type=="SYLLEPTIC",s]=1#1 is sylleptic
met[met$shoot_type=="PROLEPTIC",s]=0#0 is proleptic
met$shoot_type=as.numeric(met$shoot_type)#transform in numeric the info of sylleptic/proleptic
#change columns names to not make confusion
colnames(met)[c(20,2,6,7)]=c("is_sylleptic",
                             "parent_length_cm",
                             "parent_length_node",
                             "parent_rank_node")

#parameter: length(cm), rank node, median distance
#generate model with step()

#FULLMOD= model with all the parameters
fullmod=glm(is_sylleptic~median_distance+parent_length_cm+parent_rank_node,family = "binomial",data = met)
summary(fullmod)
#stepwise
stepmod=step(fullmod, k=2)
summary(stepmod)# the best is the one with three parameters