setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)

lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat[lat$c>1,7]=1

#glm(Y~P1+P2+P3+P4+P5+P6+P7)
#Y= dependent variable: new shoots developed. it is binomial (0=NOT developet; 1= developed)
#P1-7= predictors: parent length, rank node, #c, #v, #m, #b, #tot buds

#for fate= "V"
V=lat[lat$type=="V",]
modV = glm(V$new_shoots~V$Length+V$rank_node+V$c+V$v+V$m+V$b+V$tot_buds, family="binomial",data=V)
summary(modV)

#for fate= "M"
M=lat[lat$type=="M",]
modM = glm(M$new_shoots~M$Length+M$rank_node+M$c+M$v+M$m+M$b+M$tot_buds, family="binomial",data=M)
summary(modM)
