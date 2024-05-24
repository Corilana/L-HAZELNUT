#logigram:MOD1
#AIM: probability of having a sylleptic shoot on that rank 
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(sjlabelled);library(sjmisc);library(sjPlot)

source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

#according to exploratory
met = met[met$b == 0,]
names(met)
parameters <- c("abs_norm_median_distance","median_distance","abs_median_distance", "norm_median_distance")#select intresting parameters
#the first term of factor is taken as "failure" the second as "succes"
str(met$shoot_type)#binomial

plot(met$shoot_type~met$abs_norm_median_distance)

#model1
model= glm(
  shoot_type ~norm_median_distance+0,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1293.1

#model2
model= glm(
  shoot_type ~abs_norm_median_distance + median_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:994.68

#model3
model= glm(
  shoot_type ~ abs_norm_median_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:993.98

#FINAL MODEL
model= glm(
  shoot_type ~ abs_norm_median_distance+0,
  family = "binomial",
  data = met
)
summary(model)
#AIC:994.12

#save outputs
out=capture.output(summary(model))
# cat("3_PRO_shoot_type", out, file="Own_rooted_young/Outputs/Tables/3_PRO_shoot_type.txt", sep="\n")

tab_model(model,transform = NULL,show.se = T,show.stat = T,show.aic = T,show.obs = T,p.style = "scientific", digits.p = 2,dv.labels = "MOD1: Has that node a sylleptic shoot?",pred.labels = "|normalized distance from median rank node|",show.r2 = F,show.reflvl = T)
