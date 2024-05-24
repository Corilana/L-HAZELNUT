#logigram:MOD5
#AIM: proportion of new shoots from buds from proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

library(nnet);library(effects);library(sjlabelled);library(sjmisc);library(sjPlot)

#remove blind nodes and sylleptic and not sprouted ones
met.proleptic=droplevels(met.proleptic[met.proleptic$b==0&met.proleptic$sylleptic==0,])
#if the bud is 1 the shoot number cannot be more than 1
met.proleptic[met.proleptic$tot_buds_mv==1&met.proleptic$nb_new_shoots>1,"nb_new_shoots"]=1
data_buds_sprout=droplevels(met.proleptic[met.proleptic$nb_new_shoots!=0,])

# nb_new shoots considering also 0 ----------------------------------------
head(met.proleptic)
str(met.proleptic)
#plot data
hist(met.proleptic$nb_new_shoots)

#ratio nb shoots/nb mv buds
met.proleptic$ratio=met.proleptic$nb_new_shoots/met.proleptic$tot_buds_mv
plot(met.proleptic$ratio~met.proleptic$tot_buds_mv, pch=19)

plot(met.proleptic$nb_new_shoots~met.proleptic$tot_buds_mv,
     xlim=c(0,9),ylim=c(0,9), pch=19)
lines(x=seq(0,8),y=seq(0,8),lwd=2)

#è vero che il numero di nuovi germogli aumenta con l'aumentare delle gemme.
#ma la proporzione nb germogli/nb gemme diminuisce quando il numero gi gemme è >3

# frequence distribution with rank_node -----------------------------------
prop = met.proleptic[0, 0]
for (q in unique(sort(met.proleptic$tot_buds_mv))) {
  mv = q
  df=droplevels(met.proleptic[met.proleptic$tot_buds_mv == q, ])
  av = mean(df$nb_new_shoots)
  se = sd(df$nb_new_shoots) / sqrt(length(df$nb_new_shoots))
  tot = cbind(mv, av, se)
  prop = rbind(prop, tot)
}
prop

#in general nel germoglio
average=mean(met.proleptic$nb_new_shoots)
serr = sd(met.proleptic$nb_new_shoots)/sqrt(length(met.proleptic$nb_new_shoots))

# glm of nb_new shoots ----------------------------------------------------
#model0
model0=glm(nb_new_shoots~rank_node, met.proleptic, family = "poisson")
summary(model0)
#AIC: 1440.5
plot(allEffects(model0))

#model1
model1=glm(nb_new_shoots~rank_node+tot_buds_mv, met.proleptic, family = "poisson")
summary(model1)
#AIC: 1429.3
plot(allEffects(model1))

#model2
model2=glm(nb_new_shoots~rank_node:tot_buds_mv, met.proleptic, family = "poisson")
summary(model2)
#AIC: 1432.2
plot(allEffects(model2))

#model3
model3=glm(nb_new_shoots~rank_node+v+m, met.proleptic, family = "poisson")
summary(model3)
#AIC: 1430.3

#model4
model4=glm(nb_new_shoots~v+m, met.proleptic, family = "poisson")
summary(model4)
#AIC: 1438.1
plot(allEffects(model4))

#model5
model5=glm(nb_new_shoots~tot_buds_mv, met.proleptic, family = "poisson")
summary(model5)
#AIC: 1436.9
plot(allEffects(model5))

#the best model is 1
predict(model1, newdata = data.frame(rank_node=1,tot_buds_mv=mean(met.proleptic$tot_buds_mv)),se.fit = T,type = "response")
predict(model1, newdata = data.frame(rank_node=23,tot_buds_mv=mean(met.proleptic$tot_buds_mv)),se.fit = T,type = "response")
predict(model1, newdata = data.frame(rank_node=mean(met.proleptic$rank_node),tot_buds_mv=1),se.fit = T,type = "response")
predict(model1, newdata = data.frame(rank_node=mean(met.proleptic$rank_node),tot_buds_mv=9),se.fit = T,type = "response")

tab_model(model1,transform = NULL,show.se = T,show.stat = T,show.aic = T,show.obs = T,p.style = "scientific", digits.p = 2,dv.labels = "Number of new shoots",pred.labels = c("Intercept","rank","number of MV in the same node"),show.r2 = F,show.reflvl = T)

# nb_new shoots NOT considering 0 -----------------------------------------
head(data_buds_sprout)
str(data_buds_sprout)

#plot data
hist(data_buds_sprout$nb_new_shoots)

#ratio nb shoots/nb mv buds
data_buds_sprout$ratio=data_buds_sprout$nb_new_shoots/data_buds_sprout$tot_buds_mv
plot(data_buds_sprout$ratio~data_buds_sprout$tot_buds_mv, pch=19)

plot(data_buds_sprout$nb_new_shoots~data_buds_sprout$tot_buds_mv,
     xlim=c(0,9),ylim=c(0,9), pch=19)
lines(x=seq(0,8),y=seq(0,8),lwd=2)

prop_sprout = data_buds_sprout[0, 0]
for (q in unique(sort(data_buds_sprout$tot_buds_mv))) {
  mv = q
  df=droplevels(data_buds_sprout[data_buds_sprout$tot_buds_mv == q, ])
  av = mean(df$nb_new_shoots)
  sd = sd(df$nb_new_shoots) 
  tot = cbind(mv, av, sd)
  prop_sprout = rbind(prop_sprout, tot)
}
prop_sprout

#in general nel germoglio
average_sprout=mean(data_buds_sprout$nb_new_shoots)
sterr_sprout = sd(data_buds_sprout$nb_new_shoots)/sqrt(length((data_buds_sprout$nb_new_shoots)))

#model0
model0=glm(nb_new_shoots~rank_node, data_buds_sprout, family = "poisson")
summary(model0)
#AIC: 1310.8

#model1
model1=glm(nb_new_shoots~+rank_node+tot_buds_mv, data_buds_sprout, family = "poisson")
summary(model1)
#AIC: 1302.9
plot(allEffects(model1))

#model2
model2=glm(nb_new_shoots~tot_buds_mv, data_buds_sprout, family = "poisson")
summary(model2)
#AIC: 1300.9
plot(allEffects(model2))

#model3
model3=glm(nb_new_shoots~tot_buds_mv+norm_median_distance, data_buds_sprout, family = "poisson")
summary(model3)
#AIC: 1302.8

#model4
model4=glm(nb_new_shoots~v+m, data_buds_sprout, family = "poisson")
summary(model4)
#AIC: 1302.8


#
min=min(data_buds_sprout$tot_buds_mv)
predict(model1, newdata = data.frame(tot_buds_mv=min),se.fit = T,type = "response")
max=max(data_buds_sprout$tot_buds_mv)
predict(model1, newdata = data.frame(tot_buds_mv=max),se.fit = T,type = "response")


#the best model is 2
min=min(data_buds_sprout$tot_buds_mv)
predict(model2, newdata = data.frame(tot_buds_mv=min),se.fit = T,type = "response")
max=max(data_buds_sprout$tot_buds_mv)
predict(model2, newdata = data.frame(tot_buds_mv=max),se.fit = T,type = "response")


tab_model(model2,transform = NULL,show.se = T,show.stat = T,show.aic = T,show.obs = T,p.style = "scientific", digits.p = 2,dv.labels = "Number of new shoots",pred.labels = c("Intercept","number of MV in the same node"),show.r2 = F,show.reflvl = T)

          