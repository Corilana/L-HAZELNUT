wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)

lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#box4: do the bud (V and M) burst?
PRO_bud_scale=lat[lat$from_=="PROL",]
colnames(PRO_bud_scale)[c(2,6,7,8,16)]=c("length_cm","length_node","rank_node","distance","m_v")
PRO_bud_scale$m_v=PRO_bud_scale$m_v-PRO_bud_scale$b#ELIMINIAMO GEMME B NELLO STESSO NODO

V=PRO_bud_scale[PRO_bud_scale$fate=="V",]#for fate= "V"
M=PRO_bud_scale[PRO_bud_scale$fate=="M",]#for fate= "M"
#parameters: length(cm), length(node), rank_node, distance, m, v

#from V
fullmod = glm(new_shoots~length_cm+length_node+rank_node+distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(fullmod)#no
step(fullmod)

#fromM
fullmod = glm(new_shoots~length_cm+length_node+rank_node+distance+m+v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(fullmod)#no
step(fullmod)
