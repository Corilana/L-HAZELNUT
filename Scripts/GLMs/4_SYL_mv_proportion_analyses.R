#logigram:"number of V"
#AIM: nb buds of V and M in sylleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(sjlabelled);library(sjmisc);library(sjPlot)
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
source("Own_rooted_young/Scripts/GLMs/Functions/permutation_glm.R")
source("Own_rooted_young/Scripts/GLMs/Functions/shapleyplot.R")

names(MV.bud.SYL)
parameters = c("parent_length_cm","parent_rank_node",
               "abs_norm_median_distance", "siblings_mv","median_distance")
str(MV.bud.SYL$fate)#glm family binomial
MV.bud.SYL$fate=factor(MV.bud.SYL$fate, levels=c("M","V"))

#model1
model = glm(fate ~ parent_length_cm + parent_rank_node +
    abs_norm_median_distance+siblings_mv + median_distance,
  family = "binomial",
  data = MV.bud.SYL
)
summary(model)
#AIC:703.47
#remove abs_norm_median_distance
parameters=parameters[-3]

#model2
model = glm(fate ~ parent_length_cm + parent_rank_node +
              siblings_mv + median_distance,
            family = "binomial",
            data = MV.bud.SYL
)
summary(model)
#AIC:703.74
#remove length
parameters=parameters[-1]

#model3
model = glm(fate ~ parent_rank_node +
              siblings_mv + median_distance,
            family = "binomial",
            data = MV.bud.SYL
)
summary(model)
#AIC:706.67
#permute median_distance
# permutation(dip = "fate",predictors = c("parent_rank_node","siblings_mv"),
            # perm = "median_distance",data = MV.bud.SYL,family = "binomial")
#better perm= 33
#remove median_distance
parameters=parameters[-3]

#model5
model = glm(fate ~ siblings_mv,
            family = "binomial",
            data = MV.bud.SYL
)
summary(model)

model = glm(fate ~ 1,
            family = "binomial",
            data = MV.bud.SYL
)
summary(model)
#AIC:715.89
#permute siblings
# permutation(dip = "fate",predictors = c(""),
#             perm = "siblings_mv",data = MV.bud.SYL,family = "binomial")
#better perm= 10
#remove siblings
parameters=parameters[-1]

tab_model(model,transform = NULL, show.se = T,show.aic = T,show.r2 = F,show.obs = T,show.stat = T,p.style = "scientific",digits.p = 2,dv.labels = "How many V?")

