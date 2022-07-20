#set di righe che utilizzo sempre quando devo far girare un glm

#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))

#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)

#import dataset_metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
#store as a variable the column with the sylleptic info
s=grep("shoot_type",colnames(met))
#rename sylleptic as 1 and proleptic as 0
met[met$shoot_type=="SYLLEPTIC",s]=1
met[met$shoot_type=="PROLEPTIC",s]=0
#transform in numeric the info of sylleptic/proleptic
met$shoot_type=as.numeric(met$shoot_type)
#change columns names to not make confusion
a=grep("_type", names(met))
b=grep("^Length$", names(met))
c=grep("^Length.", names(met))
d=grep("rank", names(met))
colnames(met)[c(a,b,c,d)]=c("is_sylleptic",
                            "parent_length_cm",
                            "parent_length_node",
                            "parent_rank_node")
