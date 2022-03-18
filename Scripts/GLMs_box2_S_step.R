#GLM: how many V and M buds in proleptic?
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

#df with sylleptic
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL_met_scale)[c(2,6,7,8,15,17)]=c("parent_length_cm",
                                            "parent_length_nodes",
                                            "parent_rank_node",
                                            "distance",
                                            "tot_buds_in_sylleptic",
                                            "m_v_in_sylleptic")
SYL_met_scale$m_v_in_sylleptic=SYL_met_scale$m_v_in_sylleptic-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS


#FULLMOD= model with all the parameters
fullmod=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance+parent_length_nodes,family = "poisson",data = SYL_met_scale)
#stepwise
step=step(fullmod)
summary(step)# the best is the one with three parameters