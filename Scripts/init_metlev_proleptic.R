#set di righe che utilizzo sempre quando devo far girare un glm in proleptic

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

#import dataset_metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
#store as a variable the column with the sylleptic info
s=grep("shoot_type",colnames(met))
#rename sylleptic as 1 and proleptic as 0
met[met$shoot_type=="SYLLEPTIC",s]=1
met[met$shoot_type=="PROLEPTIC",s]=0
#transform in numeric the info of sylleptic/proleptic
met$shoot_type=as.numeric(met$shoot_type)
#subset df for proleptic
PROL_met_scale=met[met$shoot_type==0,]#df at bud scale of buds in sylleptic shoots
#change columns names to not make confusion
a=grep("^Length$", names(PROL_met_scale))
b=grep("^Length.", names(PROL_met_scale))
c=grep("^rank", names(PROL_met_scale))
d=grep("medi", names(PROL_met_scale))
e=grep("tot_buds$", names(PROL_met_scale))
f=grep("n_lat", names(PROL_met_scale))
colnames(PROL_met_scale)[c(a,b,c,d,e,f)]=c("length_cm",
                                           "length_nodes",
                                           "rank_node",
                                           "distance",
                                           "tot_buds",
                                           "m_v")
#rimuoviamo blind buds from the counting of m+v
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#rimuoviamo blind buds from the counting of m+v
