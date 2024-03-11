#logigram:MOD5
#AIM: proportion of new shoots from buds from proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
source("Own_rooted_young/Scripts/GLMs/Functions/permutation_glm.R")
source("Own_rooted_young/Scripts/GLMs/Functions/shapleyplot.R")

library(sjlabelled);library(sjmisc);library(sjPlot)

names(MV.bud.PRO)
str(MV.bud.PRO)#binomial
MV.bud.PRO$nb_new_shoots=factor(MV.bud.PRO$nb_new_shoots)#presente nuovo germoglio o no
parameters = c("rank_node", "length","siblings_mv","fate","norm_median_distance")

names(MV.bud.PRO)[24]="presence_new_shoot"

#model1
model = glm(
  presence_new_shoot ~ rank_node+length+siblings_mv+fate+norm_median_distance,
  family = "binomial",
  data = MV.bud.PRO
)
summary(model)
#AIC:599.27
#permute rank
# permutation(dip = "presence_new_shoot",predictors = parameters[-1],perm = parameters[1],data = MV.bud.PRO,family = "binomial")
#better perm: 22>10
#remove rank
parameters=parameters[-1]

#model2
model = glm(
  presence_new_shoot ~ length+siblings_mv+fate+norm_median_distance,
  family = "binomial",
  data = MV.bud.PRO
)
summary(model)
#AIC:606.53
#permute length
# permutation(dip = "presence_new_shoot",predictors = parameters[-1],perm = parameters[1],data = MV.bud.PRO,family = "binomial")
#better perm: 33>10
#remove length
parameters=parameters[-1]

#model3
model = glm(
  presence_new_shoot ~ siblings_mv+fate+norm_median_distance,
  family = "binomial",
  data = MV.bud.PRO
)
summary(model)
#AIC:613.48

#try with interactions
#model4
model = glm(
  presence_new_shoot ~ siblings_mv*fate+norm_median_distance*fate,
  family = "binomial",
  data = MV.bud.PRO
)
summary(model)
#AIC:613.84

#model5
model = glm(
  presence_new_shoot ~ siblings_mv*fate+norm_median_distance,
  family = "binomial",
  data = MV.bud.PRO
)
summary(model)
#AIC:612.12

#FINAL MODEL
model = glm(
  presence_new_shoot ~ siblings_mv:fate+norm_median_distance:fate,
  family = "binomial",
  data = MV.bud.PRO
)
summary(model)
#AIC:629.75
#coefficient
print(coef(model))
#odds (succes/insucces)
print(exp(coef(model)))
#probabilities (odds/1+odds)
print(exp(coef(model)) / (1 + exp(coef(model))))

head(MV.bud.PRO)
str(MV.bud.PRO)
#save outputs
out=capture.output(summary(model))
#cat("5_proba_new_shoots_PRO", out, file="Own_rooted_young/Outputs/Tables/5_proba_new_shoots_PRO.txt", sep="\n")

tab_model(model,transform = NULL, show.se = T,show.aic = T,show.r2 = F,show.obs = T,show.stat = T,p.style = "scientific",digits.p = 2,dv.labels = "MOD3: Does it burst?")
