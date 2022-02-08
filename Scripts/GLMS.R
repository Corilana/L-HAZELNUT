setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)

lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat[lat$c>1,7]=1#catkins present or absent (1/0)

#glm(Y~P1+P2+P3+P4+P5+P6+P7)
#Y= dependent variable: new shoots developed. it is binomial (0=NOT developet; 1= developed)
#P1-7= predictors: parent length, rank node, #c, #v, #m, #b, #tot buds

V=lat[lat$fate=="V",]#for fate= "V"

#null model
null=glm(new_shoots~1, family="binomial",data=V)
summary(null)

#1: new_shoots~Length+rank_node+tot_buds
modV1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(modV1)#tot buds& length

#permut_tot buds
V1=V
V1$tot_buds=sample(V$tot_buds)
modV1_p1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p1)
#AIC diff
null$aic-modV1_p1$aic#13.53 it means that AIC permutatio is higher (worse explain the model)

#permut_length
V1=V
V1$Length=sample(V$Length)
modV1_p2 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p2)
null$aic-modV1_p2$aic#56.51 it means that AIC permutatio is higher (worse explain the model)

#permut_rank node
V1=V
V1$rank_node=sample(V$rank_node)
modV1_p3 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=V1)#first check tot_buds to see if siblings are impo
summary(modV1_p3)
null$aic-modV1_p3$aic#58.23 it means that AIC permutatio is higher (worse explain the model)

#scorporiamo rtot buds in v+c+m
#2: new_shoots~Length+rank_node+v+m+c
modV2 = glm(new_shoots~Length+rank_node+c+v+m, family="binomial",data=V)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modV2)#length & v & rank
null$aic-modV2$aic#86.24 (it explains better than MODV1)

#permut_ V
V2=V
V2$v=sample(V$v)
modV2_p1 = glm(new_shoots~Length+rank_node+v+m+c, family="binomial",data=V2)#first check tot_buds to see if siblings are impo
summary(modV2_p1)
null$aic-modV2_p1$aic#39.69 it means that AIC permutation is higher (worse explain the model)

#permut_rank
V2=V
V2$rank_node=sample(V$rank_node)
modV2_p2 = glm(new_shoots~Length+rank_node+v+m+c, family="binomial",data=V2)#first check tot_buds to see if siblings are impo
summary(modV2_p2)
null$aic-modV2_p2$aic#58.02 it means that AIC permutation is higher (worse explain the model)

#dato che, eliminando v, c risultava positivo? proviamo a tenere solo m+c
#3: new_shoots~Length+rank_node+m+c
modV3 = glm(new_shoots~Length+rank_node+c+m, family="binomial",data=V)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modV3)#C & rank
null$aic-modV3$aic#41 

#permut_rank
V3=V
V3$c=sample(V$c)
modV3_p1 = glm(new_shoots~Length+rank_node+m+c, family="binomial",data=V3)#first check tot_buds to see if siblings are impo
summary(modV3_p1)
null$aic-modV3_p1$aic#14.08 it means that AIC permutation is higher (worse explain the model)

#4: new_shoots~Length+rank_node+v+m
modV4 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modV4)#v&rank&length
null$aic-modV4$aic#87 

#permut_v
V4=V
V4$v=sample(V$v)
modV4_p1 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V4)#first check tot_buds to see if siblings are impo
summary(modV4_p1)
null$aic-modV4_p1$aic#14.08 it means that AIC permutation is higher (worse explain the model)

#permut_rank
V4=V
V4$rank_node=sample(V$rank_node)
modV4_p2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=V4)#first check tot_buds to see if siblings are impo
summary(modV4_p2)
null$aic-modV4_p2$aic#85.03 it means that AIC permutation is higher (ma di poco) (worse explain the model)

#for fate= "M"
M=lat[lat$fate=="M",]

#null model
nullM=glm(new_shoots~1, family="binomial",data=M)
summary(nullM)

#1: new_shoots~Length+rank_node+tot_buds
modM1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(modM1)

nullM$aic-modM1$aic#225.87

#permut_tot buds
M1=M
M1$tot_buds=sample(M$tot_buds)
modM1_p1 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M1)#first check tot_buds to see if siblings are impo
summary(modM1_p1)
#AIC diff
nullM$aic-modM1_p1$aic#42 it means that AIC permutatio is higher (worse explain the model)

#permut_rank
M1=M
M1$rank_node=sample(M$rank_node)
modM1_p2 = glm(new_shoots~Length+rank_node+tot_buds, family="binomial",data=M1)#first check tot_buds to see if siblings are impo
summary(modM1_p2)
#AIC diff
nullM$aic-modM1_p2$aic#216 it means that AIC permutatio is higher (worse explain the model)

#scorporiamo rtot buds in v+c+m
#2: new_shoots~Length+rank_node+v+m+c
modM2 = glm(new_shoots~Length+rank_node+c+v+m, family="binomial",data=M)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modM2)#m & v & rank
nullM$aic-modM2$aic#86.24 (it explains better than MODM1)

#permut_V
M2=M
M2$v=sample(M$v)
modM2_p1 = glm(new_shoots~Length+rank_node+v+m+c, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p1)
nullM$aic-modM2_p1$aic#205 it means that AIC permutation is higher (worse explain the model)

#permut_M
M2=M
M2$m=sample(M$m)
modM2_p2 = glm(new_shoots~Length+rank_node+v+m+c, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p2)
nullM$aic-modM2_p2$aic#152.75 it means that AIC permutation is higher (worse explain the model)

#permut_rank
M2=M
M2$rank_node=sample(M$rank_node)
modM2_p3 = glm(new_shoots~Length+rank_node+v+m+c, family="binomial",data=M2)#first check tot_buds to see if siblings are impo
summary(modM2_p3)
nullM$aic-modM2_p3$aic#152.75 it means that AIC permutation is higher (worse explain the model)

#dato che, eliminando v, c risultava positivo? proviamo a tenere solo m+c
#3: new_shoots~Length+rank_node+m+c
modM3 = glm(new_shoots~Length+rank_node+c+m, family="binomial",data=M)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modM3)#C & m & rank
nullM$aic-modM3$aic#206 

#4: new_shoots~Length+rank_node+v+m
modM4 = glm(new_shoots~Length+rank_node+v+c, family="binomial",data=M)#zooming inside the siblings fate check with is the best predictor (c,m,v)
summary(modM4)#v&rank&c
nullM$aic-modM4$aic#87 

#permut_v
M4=M
M4$v=sample(M$v)
modM4_p1 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M4)#first check tot_buds to see if siblings are impo
summary(modM4_p1)
nullM$aic-modM4_p1$aic#14.08 it means that AIC permutation is higher (worse explain the model)

#permut_rank
M4=M
M4$rank_node=sample(M$rank_node)
modM4_p2 = glm(new_shoots~Length+rank_node+v+m, family="binomial",data=M4)#first check tot_buds to see if siblings are impo
summary(modM4_p2)
nullM$aic-modm4_p2$aic#85.03 it means that AIC permutation is higher (ma di poco) (worse explain the model)

#number of buds and shoot rank and node
lat=dplyr::mutate(V, sib=rowSums(V[c(7:9,12)]), .before=tot_buds)
#bud level
sib_lat=glm(sib~Length+rank_node, family = "poisson", data=lat)
summary(sib_lat)#ns

#rank level
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")

totV=glm(new_shoots~Length+rank_node+Tree, family = "poisson", data=met)
summary(totV)#rank and length

totV_null=glm(new_shoots~1, family = "poisson", data=met1)#null model
summary(totV_null)

#permutation_length
met1=met
met1$Length=sample(met$Length)
totV_p1=glm(new_shoots~Length+rank_node+Tree, family = "poisson", data=met1)
summary(totV_p1)#rank and length
totV_null$aic-totV$aic
