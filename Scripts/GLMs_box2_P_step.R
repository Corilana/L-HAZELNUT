#GLM: Existence of B (0,1)? 
#GLM: #how many V and M buds in proleptic?
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

#df with proleptic
PROL_met_scale=met[met$shoot_type==0,]

#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,16,18)]=c("length_cm",
                                             "length_nodes",
                                             "rank_node",
                                             "distance",
                                             "tot_buds",
                                             "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#rimuoviamo blind buds from the counting of m+v
#1: is blind node?####
#parameters: length(cm), length(node), rank_node, distance

#generate model with step()

#FULLMOD= model with all the parameters
fullmod=glm(b~length_cm+rank_node+distance+length_nodes,family = "binomial",data = PROL_met_scale)
#stepwise
step=step(fullmod)
summary(step)# the best is the one with three parameters

#2: how many buds?####
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#delete the blind nodes from the counting of total buds at that metamer

#parameters: length(cm), length(node), rank_node, distance
#generate model with step()
#FULLMOD= model with all the parameters
fullmod=glm(m_v~length_cm+distance+rank_node+length_nodes,family = "poisson",data = PROL_met_scale)
#stepwise
step=step(fullmod)
summary(step)# the best is the one with three parameters
