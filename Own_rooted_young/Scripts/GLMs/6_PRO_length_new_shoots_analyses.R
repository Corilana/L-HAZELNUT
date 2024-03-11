#logigram:MOD6
#AIM: length of new shoots from buds in proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
library(sjlabelled);library(sjmisc);library(sjPlot)

MV.bud.PRO=MV.bud.PRO[!is.na(MV.bud.PRO$length2yo),]
MV.bud.PRO=MV.bud.PRO[!(MV.bud.PRO$fate=="M"&MV.bud.PRO$length2yo>20),]#outlier remove
str(MV.bud.PRO$length2yo)#gaussian family

names(MV.bud.PRO)
parameters = c("rank_node","fate",
               "length","abs_median_distance",
               "siblings_mv","abs_norm_median_distance","norm_median_distance")

#from V
V=MV.bud.PRO[MV.bud.PRO$fate=="V",]
average_V=mean(V$length2yo)
sterr_V = sd(V$length2yo)/sqrt(length((V$length2yo)))

#from M
M=MV.bud.PRO[MV.bud.PRO$fate=="M",]
average_M=mean(M$length2yo)
sterr_M = sd(M$length2yo)/sqrt(length((M$length2yo)))

#model1
model = glm(
  length2yo ~ rank_node +fate+length +  abs_median_distance +
    siblings_mv +abs_norm_median_distance +norm_median_distance   ,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3336.9
#remove siblings
parameters=parameters[-5]

#model2
model = glm(
  length2yo ~ rank_node +fate+length +  abs_median_distance +
    abs_norm_median_distance +norm_median_distance   ,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3335.3
#remove abs_norm_median_distance
parameters=parameters[-5]

#model3
model = glm(
  length2yo ~ rank_node +fate+length +  abs_median_distance +
    norm_median_distance   ,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3334.5
#remove rank
parameters=parameters[-1]

#model4
model = glm(
  length2yo ~ fate+length +  abs_median_distance +
    norm_median_distance   ,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3333
#remove abs_median_distance
parameters=parameters[-3]

#model5
model = glm(
  length2yo ~ fate+length +norm_median_distance   ,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3331.9

#interations
#final model
#model6
model = glm(
  length2yo ~ length:fate + 
    norm_median_distance:fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3288.7
head(MV.bud.PRO)

#save outputs
out=capture.output(summary(model))
# cat("6_new_shoots_length_PRO", out, file="Own_rooted_young/Outputs/Tables/6_new_shoots_length_PRO.txt", sep="\n")

tab_model(model,transform = NULL, show.se = T,show.aic = T,show.r2 = F,show.obs = T,show.stat = T,p.style = "scientific",digits.p = 2,dv.labels = "MOD6: New shoot length")
