#logigram:"how many nodes"
#AIM: relationship between length and number of nodes 
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(stats);library(mgcv)
library(sjlabelled);library(sjmisc);library(sjPlot)

source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

#data visualization
plot(node~length, shoot)

# model 1 -----------------------------------------------------------------
model = nls(node ~ a * (length ^ b) + 0,
               data = shoot,
               start = c(a = 1, b = 0.5))
sum = summary(model)
print(sum)
AIC(model)

# model 2 -----------------------------------------------------------------
model_2 = glm(node~log(length), data = shoot,family = "poisson")
sum_2 = summary(model_2)
# print(sum_2)
AIC(model_2)
#AIC: 449.29

# model 3 -----------------------------------------------------------------
model_3 = glm(node~log(length)+length, data = shoot,family = "poisson")
sum_3 = summary(model_3)
# print(sum_3)
AIC(model_3)
#AIC: 464.77

# model 4 -----------------------------------------------------------------
model_4 = glm(node~length, data = shoot,family = "poisson")
sum_4 = summary(model_4)
# print(sum_4)
AIC(model_4)

# model 5 -----------------------------------------------------------------
model_5 = gam(node~s(length), data = shoot,family = "poisson")
sum_5 = summary(model_5)
# print(sum_5)
AIC(model_5)

# model 6 -----------------------------------------------------------------
model_6 = lm(log(node)~length+0, data = shoot)
sum_6 = summary(model_6)
print(sum_6)
AIC(model_6)

# model 7 -----------------------------------------------------------------
shoot$length_05 = shoot$length**0.5
shoot$length_2 = shoot$length**2
shoot$length_3 = shoot$length**3
shoot$length_4 = shoot$length**4

model_7 = lm(node~length+length_05+length_2+length_3+length_4+0, data = shoot)
sum_7 = summary(model_7)
print(sum_7)
AIC(model_7)

model_8 = lm(node~length_05+length_2+0, data = shoot)
sum_8= summary(model_8)
print(sum_8)
AIC(model_8)

# visualize
# png("Own_rooted_young/Outputs/Plots/1_length~#nodes.png",width = 1200,height = 900,res = 150)
plot(shoot$node ~ shoot$length,
    pch = 19,
    main = "length(cm) vs length(node)",
    xlab = "length(cm)",
    ylab = "length(node)")
new_x = seq(0, max(shoot$length), 1)
#mod6
new_y = predict(model_8, newdata = data.frame(length = new_x, length_05 =new_x**0.5, length_2=new_x**2,length_3=new_x**3,length_4=new_x**4))
pred = predict(model_8, newdata = data.frame(length = new_x, length_05 =new_x**0.5, length_2=new_x**2,length_3=new_x**3,length_4=new_x**4),type = "response",se.fit = T)
lines(new_x,new_y,lwd = 5, col = "black")
upr <- pred$fit + qnorm(0.975)*pred$se.fit
lwr <- pred$fit + qnorm(0.025)*pred$se.fit
cols = adjustcolor("black", alpha.f=0.5)
polygon(c(rev(new_x), new_x), c(rev(upr), lwr), col = cols, border = NA)
# dev.off()

n= length(shoot$tesi)
max_nodes=max(shoot$node)
min_nodes=1
#save outputs
# out=capture.output(summary(model_8))
#cat("1_length~#nodes", paste0("n=",n),
 #   paste0("max_nodes =",max_nodes),paste0("min_nodes =",min_nodes), out, file="Own_rooted_young/Outputs/Tables/1_length~#nodes.txt", sep="\n")

tab_model(model_8,show.se = T,show.stat = T,show.df = T,show.aic = T,show.obs = T,p.style = "scientific", digits.p = 2,dv.labels = "How many nodes?",pred.labels = c("Length^(0.5)","Length^(2)"))