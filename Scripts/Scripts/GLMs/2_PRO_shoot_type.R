#logigram:MOD1
#AIM: probability of having a sylleptic shoot on that rank 
#data: DERUTA 2020
#PhD: Francesca Grisafi
#Run from "HazelnutFSPM/Scripts"
source("Scripts/Modify_dataset/import_dataset.R")
source("Scripts/GLMs/Functions/permutation_glm.R")
#according to exploratory
names(met)
parameters <- c("normal_distance","median_distance","distance_abs", "median_distance_norm")#select intresting parameters
#the first term of factor is taken as "failure" the second as "succes"
str(met)#binomial

#model1
model= glm(
  shoot_type ~normal_distance + median_distance+distance_abs+ median_distance_norm,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1038.7
#remove distance_abs
parameters =parameters[-3]

#model2
model= glm(
  shoot_type ~normal_distance + median_distance+ median_distance_norm,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1038
#permute median_distance
permutation(dip="shoot_type",predictors = parameters[-2],
            perm = parameters[2],family = "binomial",data = met)
#better permutations: 13>10
#remove median_distance
parameters =parameters[-2]

#model3
model= glm(
  shoot_type ~normal_distance + median_distance_norm,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1045.9
#remove median_distance_norm
parameters=parameters[-2]

#FINAL MODEL
model= glm(
  shoot_type ~normal_distance,
  family = "binomial",
  data = met
)
summary(model)
#AIC:1046.8

#save outputs
out=capture.output(summary(model))
# cat("2_PRO_shoot_type", out, file="Outputs/Tables/2_PRO_shoot_type.txt", sep="\n")

