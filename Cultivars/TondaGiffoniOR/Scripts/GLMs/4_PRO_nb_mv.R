#logigram:"how many buds?"
#AIM: nb buds in proleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(sjlabelled);library(sjmisc);library(sjPlot)
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

met = met[met$shoot_type=="PROLEPTIC" & met$b ==0,]
met$tot_buds_mv = met$m + met$v

str(met$tot_buds_mv)#glm family poisson

names(met)
parameters = c("length","rank_node","abs_norm_median_distance",
               "abs_median_distance", "length_node", "median_distance",
               "norm_median_distance")
#model1
model = glm(
  tot_buds_mv ~ length + rank_node + abs_norm_median_distance + abs_median_distance +
    `Length(node)`+ median_distance+norm_median_distance,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1575.25

#model2
model = glm(
  tot_buds_mv ~ length + rank_node + abs_norm_median_distance + abs_median_distance +
    `Length(node)`+ norm_median_distance,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1575.5

#model3
model = glm(
  tot_buds_mv ~ length + abs_norm_median_distance + abs_median_distance +
    `Length(node)`+ norm_median_distance,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1573.8

#model4
model = glm(
  tot_buds_mv ~ length + abs_norm_median_distance + abs_median_distance +
    `Length(node)`,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1572.8

#model5
model = glm(
  tot_buds_mv ~ length + abs_median_distance +
    `Length(node)`,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1570.6

#model6
model = glm(
  tot_buds_mv ~ length + 
    `Length(node)`,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1538.7

#model7
model = glm(
  tot_buds_mv ~ length+0,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1566.5

#final model
model = glm(
  tot_buds_mv ~ 1,
  family = "poisson",
  data = met
)
summary(model)
#AIC: 1566.2

#save outputs
out=capture.output(summary(model))
# cat("4_nb_buds_PRO", out, file="Own_rooted_young/Outputs/Tables/4_nb_buds_PRO.txt", sep="\n")

exp(coef(model)[1])

#graph
perc=prop.table(table(met$tot_buds_mv))
# png("Own_rooted_young/Outputs/Plots/4_PROL_nb_buds.png",width=1200, height=900, res=150)# save plot
barplot(perc, xlab="nv of buds(mvb)in proleptic shoots")
# dev.off()

tab_model(model,transform = NULL, show.se = T,show.aic = T,show.r2 = F,show.obs = T,show.stat = T,p.style = "scientific",digits.p = 2,dv.labels = "How many buds?")