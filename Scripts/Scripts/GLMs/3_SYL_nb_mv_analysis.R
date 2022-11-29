#logigram:"how many buds?"
#AIM: nb buds in sylleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
source("Scripts/GLMs/Functions/permutation_glm.R")
source("Scripts/GLMs/Functions/shapleyplot.R")

library(ShapleyValue)

names(met.sylleptic)
parameters = c("parent_length_cm","parent_rank_node","abs_norm_median_distance","abs_median_distance", "median_distance","norm_median_distance")
str(met.sylleptic$tot_buds_m.v)#glm family poisson
#model1
model = glm(
  tot_buds_m.v ~ parent_length_cm + parent_rank_node +
    abs_norm_median_distance + abs_median_distance + median_distance+norm_median_distance,
  family = "poisson",
  data = met.sylleptic
)
summary(model)
#AIC: 775.97
#remove rank
parameters
parameters = parameters[-2]

#model2
model = glm(
  tot_buds_m.v ~ parent_length_cm + 
    abs_norm_median_distance + abs_median_distance + median_distance+norm_median_distance,
  family = "poisson",
  data = met.sylleptic
)
summary(model)
#AIC: 774.04
#remove abs_median_distance
parameters=parameters[-3]

#mode3
model = glm(
  tot_buds_m.v ~ parent_length_cm + 
    abs_norm_median_distance + median_distance+norm_median_distance,
  family = "poisson",
  data = met.sylleptic
)
summary(model)
#AIC: 772.34
#remove median distance norm
parameters=parameters[-4]

#model4
model = glm(
  tot_buds_m.v ~ parent_length_cm + 
    abs_norm_median_distance + median_distance,
  family = "poisson",
  data = met.sylleptic
)
summary(model)
#AIC: 771.6
#permute median distance
# permutation(dip = "tot_buds_m.v",predictors = parameters[-3],perm = parameters[3],data = met.sylleptic,family = "poisson")
#better perm: 10
#remove median distance
parameters=parameters[-3]

#FINAL MODEL
model = glm(
  tot_buds_m.v ~ parent_length_cm +
    abs_norm_median_distance,
  family = "poisson",
  data = met.sylleptic
)
summary(model)
#AIC: 777.47
#coefficient
print(coef(model))
#odds (succes/insucces)
print(exp(coef(model)))
#save outputs
out=capture.output(summary(model))
# cat("3_nb_buds_SYL", out, file="Outputs/Tables/3_nb_buds_SYL.txt", sep="\n")

#shapley to understand who affects more the model
shapley.plot(y_var = "tot_buds_m.v",x_var = c("parent_length_cm",
                                               "abs_norm_median_distance"),data =met.sylleptic )
