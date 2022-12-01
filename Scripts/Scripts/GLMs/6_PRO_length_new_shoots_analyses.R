#logigram:MOD6
#AIM: length of new shoots from buds in proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
<<<<<<< HEAD
=======
#Run from "HazelnutFSPM/Scripts"

>>>>>>> d13496fb0032f6db9ddebe06009d26901e80b1cc
source("Scripts/Modify_dataset/import_dataset.R")

MV.bud.PRO=MV.bud.PRO[!is.na(MV.bud.PRO$length2yo.cm.),]
MV.bud.PRO=MV.bud.PRO[!(MV.bud.PRO$fate=="M"&MV.bud.PRO$length2yo.cm.>20),]#outlier remove
str(MV.bud.PRO$length2yo.cm.)#gaussian family

names(MV.bud.PRO)
parameters = c("rank_node","fate",
               "Length","abs_median_distance",
               "siblings_mv","abs_norm_median_distance","norm_median_distance")

#model1
model = glm(
  length2yo.cm. ~ rank_node +fate+Length +  abs_median_distance +
    siblings_mv +abs_norm_median_distance +norm_median_distance   ,
  family = "gaussian",
  data = MV.bud.PRO
)
# JB: note that this is equivalent to
# model = lm(
#   length2yo.cm. ~ Length + rank_node + distance_abs +
#     median_distance_norm + siblings_mv + normal_distance + fate,
#   data = MV.bud.PRO
# )
summary(model)
#AIC:3336.9
#remove siblings
parameters=parameters[-5]

#model2
model = glm(
  length2yo.cm. ~ rank_node +fate+Length +  abs_median_distance +
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
  length2yo.cm. ~ rank_node +fate+Length +  abs_median_distance +
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
  length2yo.cm. ~ fate+Length +  abs_median_distance +
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
  length2yo.cm. ~ fate+Length +norm_median_distance   ,
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
    norm_median_distance:fate,
  family = "gaussian",
  data = MV.bud.PRO
)
summary(model)
#AIC:3288.7
head(MV.bud.PRO)

#save outputs
out=capture.output(summary(model))
# cat("6_new_shoots_length_PRO", out, file="Outputs/Tables/6_new_shoots_length_PRO.txt", sep="\n")

