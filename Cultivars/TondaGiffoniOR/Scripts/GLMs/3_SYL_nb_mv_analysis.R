#logigram:"how many buds?"
#AIM: nb buds in sylleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(sjlabelled);library(sjmisc);library(sjPlot)

source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
source("Own_rooted_young/Scripts/GLMs/Functions/permutation_glm.R")
source("Own_rooted_young/Scripts/GLMs/Functions/shapleyplot.R")

names(met.sylleptic)
parameters = c("parent_length_cm","parent_rank_node","abs_norm_median_distance")
str(met.sylleptic$tot_buds_syl_m_v)#glm family poisson
#model1
model = glm(
  tot_buds_syl_m_v ~ parent_length_cm + parent_rank_node +
    abs_norm_median_distance,
  family = "poisson",
  data = met.sylleptic
)
summary(model)
#AIC: 771.49
#remove rank
parameters
parameters = parameters[-2]

#FINAL MODEL
model = glm(
  tot_buds_syl_m_v ~ parent_length_cm + abs_norm_median_distance,
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
# cat("3_nb_buds_SYL", out, file="Own_rooted_young/Outputs/Tables/3_nb_buds_SYL.txt", sep="\n")

tab_model(model,transform = NULL,show.se = T,show.stat = T,show.aic = T,show.obs = T,p.style = "scientific", digits.p = 2,dv.labels = "How many buds?",pred.labels = c("Intercept","parent length(cm)","|normalized distabce from median rank node|"),show.r2 = F,show.reflvl = T)
