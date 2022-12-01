#logigram:MOD2
#AIM: proportion of new shoots from buds in sylleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
source("Scripts/GLMs/Functions/permutation_glm.R")
source("Scripts/GLMs/Functions/shapleyplot.R")

library(ShapleyValue)

names(MV.bud.SYL)
parameters = c("m","v","parent_rank_node","fate","parent_length_cm",
               "abs_median_distance", "abs_norm_median_distance")
str(MV.bud.SYL)
MV.bud.SYL$nb_new_shoots=as.factor(MV.bud.SYL$nb_new_shoots)
#model1
model = glm(
  nb_new_shoots ~ m +v+ parent_rank_node +fate+parent_length_cm +
    abs_median_distance + abs_norm_median_distance  ,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 674.36
#remove abs_norm_median_distance
parameters = parameters[-c(7)]

#model2
model = glm(
  nb_new_shoots ~ m +v+ parent_rank_node +fate+parent_length_cm +
    abs_median_distance  ,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 674.41
#remove rank
parameters = parameters[-c(3)]

#model3
model = glm(
  nb_new_shoots ~ m +v+ fate+parent_length_cm +
    abs_median_distance  ,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 670.56
#permute distance abs
# permutation(dip = "nb_new_shoots",predictors = c("parent_length_cm","m","v","fate"),
#             perm = "abs_median_distance",family = "binomial",data = MV.bud.SYL)
#better perm=121>10
#remove abs_median_distance
parameters = parameters[-5]

#model4
model = glm(
  nb_new_shoots ~ m +v+ fate+parent_length_cm  ,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 675.06
#permute fate
# permutation(dip = "nb_new_shoots",predictors = c("parent_length_cm","m","v"),
#             perm = "fate",family = "binomial",data = MV.bud.SYL)
#better perm=352>10
#remove fate
parameters = parameters[-3]

#model5
model = glm(
  nb_new_shoots ~ m +v+ parent_length_cm  ,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 677.34
#permute length
# permutation(dip = "nb_new_shoots",predictors = c("m","v"),
#             perm = "parent_length_cm",family = "binomial",data = MV.bud.SYL)
#better perm=64>10
#remove length
parameters = parameters[-3]

#model6
model = glm(
  nb_new_shoots ~ m + v,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 683.01

#from exploratory seems that fate is important. we try to re-insert it
parameters=c(parameters, "fate")
#model7
model = glm(
  nb_new_shoots ~ m + v + fate,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC_ 680.02

#try interactions
#model8
model = glm(nb_new_shoots ~ fate * m + fate * v,
               family = "binomial", data = MV.bud.SYL)
summary(model)
#AIC:670.44

#model9
model = glm(nb_new_shoots ~ fate:m + m + v + fate:v,
               family = "binomial",
               data = MV.bud.SYL)
summary(model)
#AIC:669.25

#final model
model = glm(nb_new_shoots ~ fate:m  + fate:v,
               family = "binomial",
               data = MV.bud.SYL)
summary(model)
#AIC:669.25

plot(MV.bud.SYL$nb_new_shoots)
#coefficient
print(coef(model))
#odds (succes/insucces)
print(exp(coef(model)))
#probabilities (odds/1+odds)
print(exp(coef(model)) / (1 + exp(coef(model))))

#save outputs
out=capture.output(summary(model))
# cat("5_new_shoots_proba_SYL", out, file="Outputs/Tables/5_new_shoots_proba_SYL.txt", sep="\n")
