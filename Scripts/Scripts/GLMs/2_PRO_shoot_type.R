#logigram:MOD1
#AIM: probability of having a sylleptic shoot on that rank 
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
source("Scripts/GLMs/Functions/permutation_glm.R")
#according to exploratory
names(met)
parameters <- c("abs_norm_median_distance","median_distance","abs_median_distance", "norm_median_distance")#select intresting parameters
#the first term of factor is taken as "failure" the second as "succes"
str(met)#binomial

#model1
model= glm(
  shoot_type ~abs_norm_median_distance + median_distance+abs_median_distance+ norm_median_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1038.7
#remove abs_median_distance
parameters =parameters[-3]

#model2
model= glm(
  shoot_type ~abs_norm_median_distance + median_distance+ norm_median_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1038
#permute median_distance
# permutation(dip="shoot_type",predictors = parameters[-2],
            #perm = parameters[2],family = "binomial",data = met)
#better permutations: 13>10
#remove median_distance
parameters =parameters[-2]

#model3
model= glm(
  shoot_type ~abs_norm_median_distance + norm_median_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1045.9
#remove norm_median_distance
parameters=parameters[-2]

#FINAL MODEL
model= glm(
  shoot_type ~abs_norm_median_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1046.8
#coefficient
print(coef(model))
#odds (succes/insucces)
print(exp(coef(model)))
#probabilities (odds/1+odds)
print(exp(coef(model)) / (1 + exp(coef(model))))

#save outputs
out=capture.output(summary(model))
# cat("2_PRO_shoot_type", out, file="Outputs/Tables/2_PRO_shoot_type.txt", sep="\n")

