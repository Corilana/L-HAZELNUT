wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral"))

library(stats)
library(dplyr)
library(RColorBrewer)

lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#box4: do the bud (V and M) burst?
SYL_bud_scale=lat[lat$from_=="SYL",]
colnames(SYL_bud_scale)[c(2,6,7,15)]=c("parent_length_cm","parent_length_node","parent_rank_node","m_v")
SYL_bud_scale$m_v=SYL_bud_scale$m_v-1#catkins are present in all sylleptics. thus we eliminate them from countin total_buds

V=SYL_bud_scale[SYL_bud_scale$fate=="V",]#for fate= "V"
M=SYL_bud_scale[SYL_bud_scale$fate=="M",]#for fate= "M"
#parameters: length(cm), length(node), rank_node, distance, m, v

#1: new_shoots~Length+length(node)+rank_node+distance+m+v####
fullmod = glm(new_shoots~parent_length_cm+parent_length_node+parent_rank_node+median_distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(fullmod)
#stepwise
stepmod=step(fullmod)
summary(stepmod)# the best is the one with three parameters

#1: new_shoots~Length+length(node)+rank_node+distance+m+v####
fullmod = glm(new_shoots~parent_length_cm+parent_length_node+parent_rank_node+median_distance+m+v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(fullmod)#no
#stepwise
stepmod=step(fullmod)
summary(stepmod)# the best is the one with three parameters