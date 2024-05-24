#logigram:MOD1
#AIM: probability of having a blind node at that node
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(sjlabelled);library(sjmisc);library(sjPlot)

source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

#according to exploratory
names(met)
str(met$b)#binomial
met$b = factor(met$b, levels = c(0,1))
plot(met$b~met$rank_node)
str(met$shoot_type)

#model1
model= glm(
  b ~rank_node +length,
  family = "binomial",
  data = met
)
summary(model)
#AIC:581.4

#model2
model= glm(
  b ~length,
  family = "binomial",
  data = met
)
summary(model)
#AIC:746.87

#FINAL MODEL
model= glm(
  b ~rank_node +0,
  family = "binomial",
  data = met
)
summary(model)
#AIC:578.58

# #save outputs
# out=capture.output(summary(model))
# cat("2_PRO_blind_node", out, file="Own_rooted_young/Outputs/Tables/2_PRO_blind_node.txt", sep="\n")

tab_model(model,transform = NULL, show.se = T,show.stat = T,show.aic = T,show.obs = T,p.style = "scientific", digits.p = 2,dv.labels = "Is that node blind?",pred.labels = "rank node",show.r2 = F,show.reflvl = F)

          