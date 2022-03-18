wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))
library(stats)
library(dplyr)
library(RColorBrewer)

met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met[met$shoot_type=="SYLLEPTIC",20]=1
met[met$shoot_type=="PROLEPTIC",20]=0
met$shoot_type=as.numeric(met$shoot_type)

#box3:proportion of V
#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL_met_scale)[c(2,6,7,8,15,17)]=c("parent_length_cm",
                                            "parent_length_nodes",
                                            "parent_rank_node",
                                            "distance",
                                            "tot_buds_in_sylleptic",
                                            "m_v_in_sylleptic")
SYL_met_scale$m_v_in_sylleptic=SYL_met_scale$m_v_in_sylleptic-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS

#parameters: length(cm), length(node), rank_node, distance

#FULLMOD= model with all the parameters
fullmod=glm(cbind(v,m)~parent_length_cm+parent_length_nodes+parent_rank_node+distance,family = "binomial",data = SYL_met_scale)
#stepwise
step=step(fullmod)
summary(step)# the best is the one with three parameters