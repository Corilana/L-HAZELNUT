#how many nuts?
#set working directory
wd = "C:/Users/franc/My Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))
#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(nnet)
library(effects)
library(plotfunctions)

TOT = read.csv(paste0(wd, "DF/auto/mtp use/bud_level.csv"))
TOT = TOT[TOT$fate == "M", ]
TOT$siblings = TOT$tot_buds - TOT$c - 1#rimuovo i catkin e la gemma in questione

#1:
glm_box1 = glm(
  nu ~ Length + Length.node. + rank_node + distance_abs + siblings + from_,
  data = TOT,
  family = "poisson"
)
summary(glm_box1)#remove from_
#2:-from_
glm_box1 = glm(
  nu ~ Length + Length.node. + rank_node + distance_abs + siblings,
  data = TOT,
  family = "poisson"
)
summary(glm_box1)#remove length.node
#3:-length.node
glm_box1 = glm(nu ~ Length + rank_node + distance_abs + siblings,
               data = TOT,
               family = "poisson")
summary(glm_box1)#distance doesn't make sense because there is rank node and because M buds are in the middle
#4:-distance_abs
glm_box1 = glm(nu ~ Length + rank_node + siblings,
               data = TOT,
               family = "poisson")
summary(glm_box1)
#4:-rank_node
glm_box1 = glm(nu ~ Length + siblings, data = TOT, family = "poisson")
summary(glm_box1)

#real data
summary(TOT$siblings)
nline = dim(TOT)[1]
a = 0
b = 2
c = 4
d = 6
for (i in 1:nline) {
  if (TOT[i, "siblings"] == a) {
    TOT[i, "class_siblings"] = "0"
  }
  if (TOT[i, "siblings"] > a &
      TOT[i, "siblings"] <= b) {
    TOT[i, "class_siblings"] = "2"
  }
  if (TOT[i, "siblings"] > b &
      TOT[i, "siblings"] <= c) {
    TOT[i, "class_siblings"] = "4"
  }
  if (TOT[i, "siblings"] > c &
      TOT[i, "siblings"] <= d) {
    TOT[i, "class_siblings"] = "6"
  }
  if (TOT[i, "siblings"] > d) {
    TOT[i, "class_siblings"] = "8"
  }
}
TOT$class_siblings = as.factor(TOT$class_siblings)
#mean of values with the same length
mean.freq = TOT[0, 0]

for (i in unique(sort(TOT$Length))) {
  Length = i
  df = TOT[TOT$Length == Length, ]
  for (j in df$class_siblings) {
    sib = j
    DF = df[df$class_siblings == j, ]
    nuts = mean(DF$nu)
    se = std.error(DF$nu)
    mean.freq = rbind(mean.freq, cbind(Length, sib, nuts, se))
  }
}

mean.freq$Length = as.numeric(mean.freq$Length)
mean.freq$sib = as.factor(mean.freq$sib)
mean.freq$nuts = as.numeric(mean.freq$nuts)
mean.freq$se = as.numeric(mean.freq$se)
#create a sequence with random numbers between 1 and maximum siblings
summary(TOT$siblings)
for (j in seq(0, 8, 2)) {
  mean.freq = cbind(mean.freq, j)
}

#rename columns
colnames(mean.freq)[5:9] = "siblings"
conf_int = mean.freq
#predict model
for (i in grep("siblings", colnames(mean.freq))) {
  pred = predict(glm_box1,
                 newdata = mean.freq[c(1, i)],
                 type = "response",
                 se.fit = T)
  mean.freq = cbind(mean.freq, pred$fit)
}
#rename columns
colnames(mean.freq)[10:14] = seq(0, 8, 2)
#remove negative values
dupli = mean.freq[10:14]
dupli[dupli < 0] = NA
mean.freq[10:14] = dupli

#confidence intervel
for (i in grep("siblings", colnames(mean.freq))) {
  pred = predict(glm_box1,
                 newdata = mean.freq[c(1, i)],
                 type = "response",
                 se.fit = T)
  lw = pred$fit + qnorm(0.025) * pred$se.fit
  up = pred$fit + qnorm(0.975) * pred$se.fit
  lim = as.data.frame(cbind(lw, up))
  conf_int = cbind(conf_int, lim)
}
#remove negative values
dupl = conf_int[c(10:19)]
dupl[dupl < 0] = 0
conf_int[c(10:19)] = dupl

# # graph
# png("nuts2.png",width=1200, height=900, res=150)# save plot
# rbPal <- brewer.pal(n=6, name="Set1")
# transp<-alphaPalette(rbPal, rep(0.25,6))
# par(mfrow=c(1,1))
# par(mar=c(5,4,1,0)+0.1)
# #real data
# with(mean.freq,plot(nuts~Length,
#             ylab = "number of nuts",
#             xlab = "parent length(cm)",
#             ylim=c(0,5),
#             col=rbPal[mean.freq$sib],
#             pch=20,cex=2,
#             xlim=c(0,72)))
# for (i in 1:length((mean.freq)[10:13])) {
#   t=colnames(mean.freq)[10:13][i]
#   with(mean.freq,lines(mean.freq[,t]~mean.freq$Length,col=rbPal[i], lwd=3))
# }
# for (i in 1:length(grep("lw", colnames(conf_int)))) {
#   j=grep("lw", colnames(conf_int))[i]
#   t_lw=j
#   t_up=j+1
#   with(conf_int[c(1,t_lw:t_up)], polygon(x=c(rev(conf_int$Length),conf_int$Length),
#                                        y=c(rev(lw),up),
#                                        col=transp[i], border = NA))
# }
# legend("top",
#        horiz=T,
#        title="number of sibling buds",
#        xpd = TRUE,
#        legend = c(seq(0,8,2)),
#        fill = rbPal[1:6],
#        cex=1)
# dev.off()
