#logigram:MOD4
#AIM: nb buds of V and M in proleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(sjlabelled);library(sjmisc);library(sjPlot)
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

library(nnet)

bud.proleptic = droplevels(bud.proleptic[bud.proleptic$fate!="B",])
names(bud.proleptic)
parameters = c("length","rank_node","`Length(node)`",
               "abs_norm_median_distance","median_distance")
bud.proleptic$nb_buds = bud.proleptic$siblings_mv + 1
str(bud.proleptic$fate)

#model1
model = multinom(fate ~ nb_buds+ abs_norm_median_distance
                   , data = bud.proleptic)
summary(model)$AIC
#AIC:1040.474
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#try to make the model just with rank <16 because they are the more frequent
prop.table(table(bud.proleptic$rank_node))
data = droplevels(bud.proleptic[bud.proleptic$rank_node <= 16, ])
#model6
model = multinom(fate ~ rank_node, data = data)
summary(model)$AIC
#AIC:1040.474
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#create polynomial with rank less than 16
data.poly = bud.proleptic[bud.proleptic$rank_node <= 16, ]
rank_node = data.poly$rank_node
data.poly = cbind(data.poly,
                  rank_node ** 0.5,
                  rank_node ** 2,
                  rank_node ** 3,
                  rank_node ** 4)
ndata.poly = dim(data.poly)[2]
names(data.poly)[(ndata.poly - 3):ndata.poly] = c("rank_node0.5", "rank_node2", "rank_node3", "rank_node4")

#final model
model = multinom(fate ~ rank_node + rank_node0.5 + rank_node2 + rank_node3 +
                  rank_node4,
                data = data.poly)
summary(model)
#AIC: 963.3681

str(data.poly)
exp(coef(model))
pred=as.data.frame(predict(model, "probs", newdata=data.poly))
pred_V=pred$V
pred_M=pred$M
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#save outputs
out=capture.output(summary(model))
# cat("4_propMV_PRO", out, file="Own_rooted_young/Outputs/Tables/4_propMV_PRO.txt", sep="\n")


tab_model(model,transform = NULL, show.se = T,show.aic = T,show.r2 = F,show.obs = T,show.stat = T,p.style = "scientific",digits.p = 2,pred.labels = c("Intercept", "rank node", "rank node^(0.5)", "rank node^(2)", "rank node^(3)", "rank node^(4)"),dv.labels = "MOD2: Proportion of M and V buds",show.reflvl = T,string.est = "Coefficients")

