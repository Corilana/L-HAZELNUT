#logigram:MOD4
#AIM: nb buds of B, V and M in proleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")

library(nnet)

names(bud.proleptic)
parameters = c("Length","rank_node","Length.node.", "normal_distance","median_distance")
str(bud.proleptic$fate)#glm family binomial

#model1
model = multinom(fate ~ median_distance + rank_node + normal_distance +Length + Length.node., data = bud.proleptic)
summary(model)$AIC
#AIC:1560.052
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#renove Length

#model2
model = multinom(fate ~ median_distance + rank_node + normal_distance  + Length.node., data = bud.proleptic)
summary(model)$AIC
#AIC:1575.955
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#renove normal distance

#model3
model = multinom(fate ~ median_distance + rank_node   + Length.node., data = bud.proleptic)
summary(model)$AIC
#AIC:1657.289
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#remove length node

#model4
model = multinom(fate ~ median_distance + rank_node, data = bud.proleptic)
summary(model)$AIC
#AIC:1657.289
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#remove media_distance

#model5
model = multinom(fate ~ rank_node, data = bud.proleptic)
summary(model)$AIC
#AIC:1657.482
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
# #permutate RANK
# null_1=multinom(fate~1,data = bud.proleptic)
# dif=model$AIC-null_1$AIC
# met_nul=bud.proleptic
# df=data.frame(matrix(nrow=0, ncol=0))
# for (i in 1:10000) {
#   met_nul$rank_node=sample(bud.proleptic$rank_node)
#   perm=multinom(fate~rank_node,data = met_nul)
#   a=perm$AIC-null_1$AIC
#   b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# better_perm=length(which(df$b==1))#times better perm!!!
#rank makes sense with the permutations

#try to make the model just with rank <16 because they are the more frequent
prop.table(table(bud.proleptic$rank_node))
data = droplevels(bud.proleptic[bud.proleptic$rank_node <= 16, ])
#model6
model = multinom(fate ~ rank_node, data = data)
summary(model)$AIC
#AIC:1570.779
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#create polinomial with rank less than 16
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
#AIC: 1482.388

str(data.poly)
exp(coef(model))
#computing p value
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#save outputs
out=capture.output(summary(model))
cat("4_propMVB_PRO", out, file="Outputs/Tables/4_propMVB_PRO.txt", sep="\n")
