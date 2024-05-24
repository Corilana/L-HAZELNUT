#AIM: how many nuts?
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/GLMs/Have_clusters.R")

library(RColorBrewer);library(nnet);library(plotfunctions)

names(mix)
str(mix$nu)#glm family poisson

mix$norm_median_distance = round(mix$median_distance/mix$Length_node,2)

#select just the ones that HAVE clusters
mix = droplevels(mix[mix$cl>0,])

plot(mix$nu ~ mix$length, pch = 19)
plot(mix$nu ~ mix$siblings_mv, pch = 19)
plot(mix$nu ~ mix$abs_median_distance, pch = 19)
plot(mix$nu ~ mix$class, pch = 19)
plot(mix$nu ~ mix$norm_median_distance, pch = 19)

#model1:
model = glm(nu ~ length + norm_median_distance +siblings_mv + shoot_type + 0,
  data = mix,
  family = "poisson")
summary(model)
#AIC= 1080.9

#model1:
model = glm(nu ~ length + norm_median_distance +siblings_mv + shoot_type,
            data = mix,
            family = "poisson")
summary(model)
#AIC= 1080.9

#model2:
model = glm(nu ~ class + norm_median_distance +siblings_mv + shoot_type +0,
            data = mix,
            family = "poisson")
summary(model)
#AIC= 1085

#model3:
model = glm(nu ~ length + norm_median_distance + shoot_type + 0 ,
            data = mix,
            family = "poisson")
summary(model)
#AIC= 1079.5

#model4:
model = glm(nu ~ length + norm_median_distance : shoot_type + 0 ,
            data = mix,
            family = "poisson")
summary(model)
#AIC= 1149.1

#model5:
model = glm(nu ~ norm_median_distance + shoot_type + 0 ,
            data = mix,
            family = "poisson")
summary(model)
#AIC= 1082.1

#model6:
model = glm(nu ~ norm_median_distance,
            data = mix,
            family = "poisson")
summary(model)
#AIC= 1080.6

#save outputs
out=capture.output(summary(model))
#cat("nb_nuts", out, file="Own_rooted_young/Outputs/Tables/nb_nuts.txt", sep="\n")

#divide in groups
mix$class_norm = NA

class = 4
min = round(min(mix$norm_median_distance), 2)
max = round(max(mix$norm_median_distance), 2)
seq_normdist = c(-Inf, round(seq(min, max, (max-min)/class),2), Inf)

mix$class_norm_group = cut(mix$norm_median_distance, seq_normdist)
mix$class_norm = cut(mix$norm_median_distance, seq_normdist, labels = F)

#simulated
seq = sort(unique(mix$norm_median_distance))
norm_median_distance = rep(seq, 2)

sim = data.frame(norm_median_distance)
sim$class_norm_group = cut(sim$norm_median_distance, seq_normdist)
sim$class_norm = cut(sim$norm_median_distance, seq_normdist, labels = F)

pred = predict(model, newdata = sim, type = "response", se.fit = T)
sim$predict = pred$fit

# sim$lw = pred$fit + qnorm(0.025) * pred$se.fit
# sim$up = pred$fit + qnorm(0.975) * pred$se.fit

#real and sim (mean and se)
mean.freq = data.frame()
for (i in sort(unique(mix$class_norm))) {
  class = levels(droplevels(mix[mix$class_norm==i,"class_norm_group"]))
  df=droplevels(mix[mix$class_norm==i,])
  av_norm_dist = round(mean(df$norm_median_distance),2)
  n = nrow(df)
  se_norm_dist= round(sd(df$norm_median_distance)/sqrt(n),2)
  av_nuts = round(mean(df$nu),2)
  av_se = round(sd(df$nu)/sqrt(n),2)
  
  nw_df = cbind(class, n, av_norm_dist, se_norm_dist, av_nuts, av_se)
  
  df=droplevels(sim[sim$class_norm==i,])
  av_norm_dist_sim = round(mean(df$norm_median_distance),2)
  n_sim = nrow(df)
  se_norm_dist_sim= round(sd(df$norm_median_distance)/sqrt(n),2)
  av_nuts_sim = round(mean(df$predict),2)
  se_sim = round(sd(df$predict)/sqrt(n),2)
  
  nw_df = cbind(nw_df, n_sim,av_norm_dist_sim,se_norm_dist_sim, av_nuts_sim, se_sim )
  mean.freq = rbind(mean.freq, nw_df)
}

str(mean.freq)
mean.freq[-1]=lapply(mean.freq[-1], as.numeric)

# graph
# png("Own_rooted_young/Outputs/Plots/7_nb_nuts.png",width=1200, height=900, res=150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
transp<-alphaPalette(rbPal, rep(0.25,6))
par(mfrow=c(1,1))
par(mar=c(5,4,1,0)+0.1)

plot(av_nuts~av_norm_dist,data=mean.freq, ylab = "nb_nuts", xlab = "normalized distance from median rank node",
     ylim=c(0,5), pch=19,cex=1, xlim=c(-0.5,+0.5))
plotCI(mean.freq$av_norm_dist, y = mean.freq$av_nuts,
       ui = mean.freq$av_norm_dist + mean.freq$se_norm_dist, 
       li = mean.freq$av_norm_dist - mean.freq$se_norm_dist, err = "x", add = T)
plotCI(mean.freq$av_norm_dist, y = mean.freq$av_nuts,
       ui = mean.freq$av_nuts + mean.freq$av_se, 
       li = mean.freq$av_nuts - mean.freq$av_se, err = "y", add = T)
points(av_nuts_sim~av_norm_dist_sim,data=mean.freq, ylab = "", xlab = "",col="red",
     ylim=c(0,5), pch=19,cex=1, xlim=c(-0.5,+0.5))
plotCI(mean.freq$av_norm_dist_sim, y = mean.freq$av_nuts_sim,col="red",
       ui = mean.freq$av_norm_dist_sim + mean.freq$se_norm_dist_sim, 
       li = mean.freq$av_norm_dist_sim - mean.freq$se_norm_dist_sim, err = "x", add = T)
plotCI(mean.freq$av_norm_dist_sim, y = mean.freq$av_nuts_sim,
       ui = mean.freq$av_nuts_sim + mean.freq$se_sim, col="red",
       li = mean.freq$av_nuts_sim - mean.freq$se_sim, err = "y", add = T)
legend("topright", legend = c("real", "predicted"),pch = c(19,19), col = c("black", "red") )
# dev.off()

