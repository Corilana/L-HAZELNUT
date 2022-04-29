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
#box4: do the bud (V and M) burst?
PRO_bud_scale=lat[lat$from_=="PROL",]
#change columns names to not make confusion
a=grep("^Length$", names(PRO_bud_scale))
b=grep(".node.$", names(PRO_bud_scale))
c=grep("rank", names(PRO_bud_scale))
d=grep("abs", names(PRO_bud_scale))
e=grep("tot_", names(PRO_bud_scale))
colnames(PRO_bud_scale)[c(a,b,c,d,e)]=c("length_cm","length_node","rank_node","distance","m_v")
MV=PRO_bud_scale[(PRO_bud_scale$fate=="M"|PRO_bud_scale$fate=="V")&PRO_bud_scale$new_shoots,]
MV$sib=MV$v+MV$m+MV$b

#parameters: length(cm), length(node), rank_node, distance, m, v
#M and V####
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")
str(MV)

colnames(MV)[24]="nodes"
#plot
plot(density(MV$nodes))
#1:length~length+lengthnode+rank+median_distance+sib+fate
glm_box1 = glm(nodes~length_cm*fate+length_node*fate+rank_node*fate+median_distance*fate+sib*fate, data = MV, family = "gaussian")
summary(glm_box1)
#2:length~length+lengthnode+rank+sib+fate
glm_box1 = glm(nodes~length_cm*fate+length_node*fate+rank_node*fate+sib*fate, data = MV, family = "gaussian")
summary(glm_box1)
#3:length~length+rank+sib+fate
glm_box1 = glm(nodes~length_cm*fate+rank_node*fate+sib*fate, data = MV, family = "gaussian")
summary(glm_box1)
#graph
plot(allEffects(glm_box1))
#4:length~length
glm_box1 = glm(nodes~length_cm*fate, data = MV, family = "gaussian")
summary(glm_box1)
#graph
plot(allEffects(glm_box1))

png("5_P.png",width=1200, height=900, res=150)# save plot
with(plot(allEffects(glm_box1)))
dev.off()
