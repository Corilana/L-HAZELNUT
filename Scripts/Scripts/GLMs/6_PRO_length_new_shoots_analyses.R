#logigram:MOD6
#AIM: length of new shoots from buds in proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi

source("Scripts/Modify_dataset/import_dataset.R")

MV.bud.PRO=MV.bud.PRO[!is.na(MV.bud.PRO$length2yo.cm.),]
MV.bud.PRO=MV.bud.PRO[!(MV.bud.PRO$fate=="M"&MV.bud.PRO$length2yo.cm.>20),]#outlier remove
str(MV.bud.PRO$length2yo.cm.)#gaussian family

names(MV.bud.PRO)
parameters = c("rank_node","fate",
               "Length","distance_abs",
               "siblings_mv","normal_distance","median_distance_norm")

#model1
model = glm(
  length2yo.cm. ~ Length + rank_node + distance_abs +
    median_distance_norm + siblings_mv + normal_distance + fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3336.9
#remove siblings
parameters=parameters[-5]

#model2
model = glm(
  length2yo.cm. ~ Length + rank_node + distance_abs +
    median_distance_norm + normal_distance + fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3335.3
#remove normal distance
parameters=parameters[-5]

#model3
model = glm(
  length2yo.cm. ~ Length + rank_node + distance_abs +
    median_distance_norm + fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3334.5
#remove rank
parameters=parameters[-1]

#model4
model = glm(
  length2yo.cm. ~ Length + distance_abs +
    median_distance_norm + fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3333
#remove distance
parameters=parameters[-3]

#model5
model = glm(
  length2yo.cm. ~ Length +  
    median_distance_norm + fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3331.9

#interations
#final model
#model6
model = glm(
  length2yo.cm. ~ Length:fate + 
    median_distance_norm:fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3288.7

#save outputs
out=capture.output(summary(model))
# cat("6_new_shoots_length_PRO", out, file="Outputs/Tables/6_new_shoots_length_PRO.txt", sep="\n")

