setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)

lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat[lat$c>1,7]=1#catkins present or absent (1/0)

lat[lat$in_sylleptic=="YES",17]=1#if the bud is in sylleptic shoot-->1
lat[lat$in_sylleptic=="NO",17]=0#if the bud is in sylleptic shoot-->1
lat$in_sylleptic=as.numeric(lat$in_sylleptic)

#box1: are the rank you sylleptic?
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met[met$organ=="SYLLEPTIC",18]=1
met[met$organ=="BUD",18]=0
met$organ=as.numeric(met$organ)

#sylleptic is explained by length, rank effect?
glm_box1=glm(organ~Length+rank_node,family = "binomial",data = met)
summary(glm_box1)

#box2:how many buds? of what type?
#for sylleptic
SYL=met[met$organ==1,]#df at bud scale of buds in sylleptic shoots
#tot buds in sylleptic is explained by length, rank effect?
glm_box2_syl=glm(n_lateral_buds~Length+rank_node,family = "poisson",data = SYL)
summary(glm_box2_syl)
#is number of m related to length, rank node, or other v buds at that node?
glm_box2_syl_m=glm(m~Length+rank_node+v,family = "poisson",data = SYL)
summary(glm_box2_syl_m)
#is number of v related to length, rank node, or other m buds at that node?
glm_box2_syl_v=glm(v~Length+rank_node+m,family = "poisson",data = SYL)
summary(glm_box2_syl_v)

#box2:how many buds? of what type?
#for not sylleptic
BUD=met[met$organ==0,]#df at bud scale of buds NOT IN  sylleptic shoots
#tot buds that rank is explained by length, rank effect?
glm_box2_bud=glm(n_lateral_buds~Length+rank_node,family = "poisson",data = BUD)
summary(glm_box2_bud)
#is number of m related to length, rank node, or other v buds at that node?
glm_box2_bud_m=glm(m~Length+rank_node+v,family = "poisson",data = BUD)
summary(glm_box2_bud_m)
#is number of v related to length, rank node, or other m buds at that node?
glm_box2_bud_v=glm(v~Length+rank_node+m,family = "poisson",data = BUD)
summary(glm_box2_bud_v)
#is number of v related to length, rank node at that node?
glm_box2_bud_b=glm(b~Length+rank_node,family = "poisson",data = BUD)
summary(glm_box2_bud_b)

#glm(Y~P1+P2+P3+P4+P5+P6+P7)
#Y= dependent variable: new shoots developed. it is binomial (0=NOT developet; 1= developed)
#P1-7= predictors: parent length, rank node, #c, #v, #m, #b, #tot buds

#questio1: are the number of new shoots related to length and rank and type of organ (1=syl, 0=bud)??
glm_q1=glm(new_shoots~Length+rank_node+organ, family="poisson", data=met)
summary(glm_q1)#yes!!!

#box3: do the bud (V and M) burst?
SYL=lat[lat$in_sylleptic=="1",]
BUD=lat[lat$in_sylleptic=="0",]
#for sylleptic
V=SYL[SYL$fate=="V",]#for fate= "V"
M=SYL[SYL$fate=="M",]#for fate= "V"

null=glm(new_shoots~1, family="binomial",data=V)#null model
summary(null)

#1: new_shoots~Length+rank_node+tot_buds
modV1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(modV1)#tot buds& length
null$aic-modV1$aic#4.66

#permut_tot buds
V1=V
V1$tot_buds=sample(V$tot_buds)
modV1_p1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p1)
#AIC diff
null$aic-modV1_p1$aic#-2.36 it means that AIC permutatio is higher (worse explain the model)
#permut_length
V1=V
V1$Length=sample(V$Length)
modV1_p2 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p2)
null$aic-modV1_p2$aic#0.35 it means that AIC permutatio is higher (worse explain the model)

#scorporiamo rtot buds in v+m
#2: new_shoots~Length+rank_node+v+m+c
modV2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modV2)#length & v & rank
null$aic-modV2$aic#26.60 (it explains better than MODV1)

#permut_V
V2=V
V2$v=sample(V$v)
modV2_p1 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V2)
summary(modV2_p1)
null$aic-modV2_p1$aic#0.94 it means that AIC permutation is higher (worse explain the model)
#permut_length
V2=V
V2$Length=sample(V$Length)
modV2_p2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V2)
summary(modV2_p2)
null$aic-modV2_p2$aic#58.02 it means that AIC permutation is higher (worse explain the model)

#for fate= "M"
nullM=glm(new_shoots~1, family="binomial",data=M)#null model
summary(nullM)

#1: new_shoots~Length+rank_node+tot_buds
modM1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M)
summary(modM1)
nullM$aic-modM1$aic#28.335

#permut_tot buds
M1=M
M1$tot_buds=sample(M$tot_buds)
modM1_p1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M1)#first check tot_buds to see if siblings are impo
summary(modM1_p1)
#AIC diff
nullM$aic-modM1_p1$aic#-2.43 it means that AIC permutatio is higher (worse explain the model)

#scorporiamo rtot buds in v+m
#2: new_shoots~Length+rank_node+v+m
modM2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modM2)#m & v & rank
nullM$aic-modM2$aic#32.90 (it explains better than MODM1)

#permut_V
M2=M
M2$v=sample(M$v)
modM2_p1 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p1)
nullM$aic-modM2_p1$aic#25.89 it means that AIC permutation is higher (worse explain the model)

#permut_M
M2=M
M2$m=sample(M$m)
modM2_p2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p2)
nullM$aic-modM2_p2$aic#152.75 it means that AIC permutation is higher (worse explain the model)

#for not in sylleptic
V=BUD[BUD$fate=="V",]#for fate= "V"
M=BUD[BUD$fate=="M",]#for fate= "V"

null=glm(new_shoots~1, family="binomial",data=V)#null model
summary(null)

#1: new_shoots~Length+rank_node+tot_buds
modV1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(modV1)#tot buds& length
null$aic-modV1$aic#4.66

#permut_tot buds
V1=V
V1$tot_buds=sample(V$tot_buds)
modV1_p1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p1)
#AIC diff
null$aic-modV1_p1$aic#-2.36 it means that AIC permutatio is higher (worse explain the model)
#permut_rank
V1=V
V1$rank_node=sample(V$rank_node)
modV1_p2 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p2)
null$aic-modV1_p2$aic#0.35 it means that AIC permutatio is higher (worse explain the model)

#scorporiamo rtot buds in v+m
#2: new_shoots~Length+rank_node+v+m
modV2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modV2)#length & v & rank
null$aic-modV2$aic#28.61 (it explains better than MODV1)

#permut_V
V2=V
V2$v=sample(V$v)
modV2_p1 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V2)
summary(modV2_p1)
null$aic-modV2_p1$aic#0.94 it means that AIC permutation is higher (worse explain the model)
#permut_rank
V2=V
V2$rank_node=sample(V$rank_node)
modV2_p2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V2)
summary(modV2_p2)
null$aic-modV2_p2$aic#58.02 it means that AIC permutation is higher (worse explain the model)

#for fate= "M"
nullM=glm(new_shoots~1, family="binomial",data=M)#null model
summary(nullM)

#1: new_shoots~Length+rank_node+tot_buds
modM1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M)
summary(modM1)
nullM$aic-modM1$aic#66.06

#permut_tot buds
M1=M
M1$tot_buds=sample(M$tot_buds)
modM1_p1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M1)#first check tot_buds to see if siblings are impo
summary(modM1_p1)
#AIC diff
nullM$aic-modM1_p1$aic#-2.43 it means that AIC permutatio is higher (worse explain the model)

#permut_rank
M1=M
M1$rank_node=sample(M$rank_node)
modM1_p2 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M1)#first check tot_buds to see if siblings are impo
summary(modM1_p2)
#AIC diff
nullM$aic-modM1_p1$aic#23.54 it means that AIC permutatio is higher (worse explain the model)

#scorporiamo rtot buds in v+m
#2: new_shoots~Length+rank_node+v+m
modM2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modM2)#m & v & rank
nullM$aic-modM2$aic#64.26 (it explains better than MODM1)

#permut_V
M2=M
M2$v=sample(M$v)
modM2_p1 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p1)
nullM$aic-modM2_p1$aic#25.89 it means that AIC permutation is higher (worse explain the model)

#permut_M
M2=M
M2$m=sample(M$m)
modM2_p2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p2)

#permut_rank
M2=M
M2$rank_node=sample(M$rank_node)
modM2_p3 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M1)#first check tot_buds to see if siblings are impo
summary(modM2_p3)
#AIC diff
nullM$aic-modM2_p3$aic#23.54 it means that AIC permutatio is higher (worse explain the model)

