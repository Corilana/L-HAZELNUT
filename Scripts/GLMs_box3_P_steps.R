#box3:proportion of V
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)

met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met[met$shoot_type=="SYLLEPTIC",21]=1
met[met$shoot_type=="PROLEPTIC",21]=0
met$shoot_type=as.numeric(met$shoot_type)

#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
PROL_met_scale=met[met$shoot_type==0,]#df at met scale proleptic shoots
#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,16,18)]=c("length_cm",
                                             "length_nodes",
                                             "rank_node",
                                             "distance",
                                             "tot_buds",
                                             "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#REMOVING THE COUNTING OF CATKINS BECAUSE ALL PROLLEPTIC HAS CATKINS

#FULLMOD= model with all the parameters
fullmod=glm_box1=glm(cbind(v,m)~length_cm+length_nodes+rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(fullmod)
step=step(fullmod)
summary(step)# the best is the one with three parameters