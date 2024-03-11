#logigram:MOD3
#AIM: length of new shoots from buds in sylleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
source("Own_rooted_young/Scripts/GLMs/Functions/permutation_glm.R")
source("Own_rooted_young/Scripts/GLMs/Functions/shapleyplot.R")

library(knitr);library(fitdistrplus)

str(MV.bud.SYL$length2yo)#gaussian family
MV.bud.SYL=MV.bud.SYL[!(is.na(MV.bud.SYL$length2yo)),]

names(MV.bud.SYL)
parameters = c("m","v","parent_rank_node","fate",
              "parent_length_cm","abs_median_distance",
              "siblings_mv","abs_norm_median_distance","norm_median_distance")

#model1
model = glm(length2yo ~  m + v +parent_rank_node+
              fate +  parent_length_cm+abs_median_distance+
              siblings_mv + abs_norm_median_distance+norm_median_distance,
  family = "gaussian",
  data = MV.bud.SYL
)
summary(model)
#AIC:1016.2
#remove siblings mv
parameters = parameters[-7]

#model2
model = glm(length2yo ~  m + v +parent_rank_node+
              fate +  parent_length_cm+abs_median_distance+
              abs_norm_median_distance+norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1016.2
#remove abs_norm_median_distance
parameters = parameters[-7]

#model3
model = glm(length2yo ~  m + v +parent_rank_node+
              fate +  parent_length_cm+abs_median_distance+
              norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1014.2
#remove v
parameters = parameters[-2]

#model4
model = glm(length2yo ~  m + parent_rank_node+
              fate +  parent_length_cm+abs_median_distance+
              norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1012.4
#remove m
parameters = parameters[-1]

#model5
model = glm(length2yo ~  parent_rank_node+
              fate +  parent_length_cm+abs_median_distance+
              norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1010.6
#remove abs_median_distance
parameters = parameters[-4]

#model6
model = glm(length2yo ~  parent_rank_node+
              fate +  parent_length_cm+
              norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1009.4
#remove fate
parameters = parameters[-2]

#model7
model = glm(length2yo ~  parent_rank_node+
              parent_length_cm+
              norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1008.4
#remove rank
parameters = parameters[-1]

#model8
model = glm(length2yo ~ parent_length_cm+
              norm_median_distance,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1008.9
#remove norm_median_distance
parameters = parameters[-2]

#model9
model = glm(length2yo ~  parent_length_cm,
            family = "gaussian",
            data = MV.bud.SYL
)
summary(model)
#AIC:1007.5
#remove length
parameters = parameters[-1]

#BECAUSE NON OF THE MODEL WAS SATYISFING, AND BECAUSE THERE ARE NOT MUCH INFO ON SYLLEPTIC SHOOT,
#THE LENGTH OF NEW SHOOTS WILL BE PLOTTED AS DISTRIBUTION LENGTH
#find best dist
descdist(MV.bud.SYL$length2yo, discrete = FALSE)

fit.weibull <- fitdist(MV.bud.SYL$length2yo, "weibull")
fit.weibull$aic
#AIC:876.9911
plot(fit.weibull)

fit.gamma <- fitdist(MV.bud.SYL$length2yo, "gamma")
fit.gamma$aic
#AIC:856.0129
plot(fit.gamma)

#save outputs
out=capture.output(summary(fit.gamma))
# cat("6_new_shoots_length_SYL", out, file="Own_rooted_young/Outputs/Tables/6_new_shoots_length_SYL.txt", sep="\n")

#FINAL DISTRIBUTION: GAMMA DISTRIBUTION
#gamma has the best aic. exstracte gamma value for each shoot length
q = data.frame(
  "x" = seq(0, max(MV.bud.SYL$length2yo), by = 0.1),
  "y" = dgamma(
    seq(0, max(MV.bud.SYL$length2yo), by = 0.1),
    shape = fit.gamma$estimate[1],
    rate = fit.gamma$estimate[2]
  )
)
#histogram
h = hist(MV.bud.SYL$length2yo, breaks = 10)
h$counts <- h$counts / sum(h$counts)

#graph
# png("Own_rooted_young/Outputs/Plots/6_SYL_length_dist.png",width=1200, height=900, res=150)# save plot
with(MV.bud.SYL, plot(h, main="from M or V(gamma dist)",
              freq=T,
              ylim=c(0,0.5),
              ylab="relative frequency",
              col="grey",
              xlab = "Length new shoots (cm)"))
with(q,lines(y~x, type="l", lwd=2))
# dev.off()

Parameters = fit.gamma$estimate
parameter_names <- names(Parameters)
Std.Error = fit.gamma$sd
Observations = fit.gamma$n
AIC = fit.gamma$aic
df <- data.frame(Value = parameter_values,Error = Std.Error)
df[3:4,]=NA
rownames(df)[3:4] = c("Observations", "AIC")
df[3:4,1]= c(Observations,AIC)
kable(df)
