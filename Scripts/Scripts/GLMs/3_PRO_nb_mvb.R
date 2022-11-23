#logigram:"how many buds?"
#AIM: nb buds in proleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
source("Scripts/GLMs/Functions/permutation_glm.R")
source("Scripts/GLMs/Functions/shapleyplot.R")

library(ShapleyValue)

str(met.proleptic$tot_buds_mvb)#glm family poisson

names(met.proleptic)
parameters = c("Length","rank_node","normal_distance",
               "distance_abs", "Length.node.", "median_distance",
               "median_distance_norm")
#model1
model = glm(
  tot_buds_mvb ~ Length + rank_node + Length.node.+
    normal_distance + distance_abs + median_distance+median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1824.5
#remove median_distance
parameters = parameters[-6]

#model2
model = glm(
  tot_buds_mvb ~ Length + rank_node + Length.node.+
    normal_distance + distance_abs + median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1824.5
#remove rank
parameters=parameters[-2]

#model3
model = glm(
  tot_buds_mvb ~ Length + Length.node.+
    normal_distance + distance_abs + median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1822.7
#remove normal distance
parameters = parameters[-2]

#model4
model = glm(
  tot_buds_mvb ~ Length + Length.node.+
    distance_abs + median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC:1821
#remove distance_abs
parameters = parameters[-2]

#model5
model = glm(
  tot_buds_mvb ~ Length + Length.node.+
    median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1819
#remove length.node.
parameters = parameters[-2]

#model6
model = glm(
  tot_buds_mvb ~ Length + 
    median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1818.1
#remove Length
parameters = parameters[-1]

#model7
model = glm(
  tot_buds_mvb ~ median_distance_norm,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1816.7
#remove median distance norm
parameters = parameters[-1]

#final model
model = glm(
  tot_buds_mvb ~ 1,
  family = "poisson",
  data = met.proleptic
)
summary(model)
#AIC: 1815.9

#save outputs
out=capture.output(summary(model))
cat("3_nb_buds_PRO", out, file="Outputs/Tables/3_nb_buds_PRO.txt", sep="\n")

exp(coef(model)[1])

#graph
perc=prop.table(table(met.proleptic$tot_buds_mvb))
png("Outputs/Plots/3_PROL_nb_buds.png",width=1200, height=900, res=150)# save plot
barplot(perc, xlab="nv of buds(mvb)in proleptic shoots")
dev.off()
