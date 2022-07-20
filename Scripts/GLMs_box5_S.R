#box4: do the bud (V and M) burst?
#in syllepotic shoots

#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

#import lybrary
library(stats)
library(dplyr)
library(RColorBrewer)
library(effects)
library(plotrix)
library(fitdistrplus)

#import dataset
lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#subset dataset
SYL_bud_scale=lat[lat$from_=="SYL",]
#changing column names
colnames(SYL_bud_scale)[c(2,7,8,18)]=c("parent_length_cm","parent_length_node","parent_rank_node","m_v")
#removing the catkins from the count of M and V
SYL_bud_scale$m_v=SYL_bud_scale$m_v-1
#subset dataset_ from V&M
MV=SYL_bud_scale[(SYL_bud_scale$fate=="M"|SYL_bud_scale$fate=="V")&SYL_bud_scale$new_shoots!=0,]
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")

#is,length depending on bud fate?
mod=glm(MV$length2yo.cm.~MV$fate)
summary(mod)#no

#thus, df will be keept together
#length distribution
#plot density distribution to understand
plot(density(MV$length2yo.cm.))
qqnorm(MV$length2yo.cm.)#NON è NORMALE
qqline(MV$length2yo.cm., col = "steelblue", lwd = 2)
shapiro.test(MV$length2yo.cm.)#NON è NORMALE

#che distribuzione è?
descdist(MV$length2yo.cm., discrete = FALSE)

fit.weibull<-fitdist(MV$length2yo.cm., "weibull")
fit.gamma<-fitdist(MV$length2yo.cm., "gamma")

plot(fit.weibull)
plot(fit.gamma)

fit.weibull$aic
fit.gamma$aic
#gamma has the best aic
q=data.frame("x"=seq(0,max(MV$length2yo.cm.), by=0.1),
           "y"=dgamma(seq(0,max(MV$length2yo.cm.), by=0.1),
                      shape = fit.gamma$estimate[1],
                      rate = fit.gamma$estimate[2]))
#histogram
h=hist(MV$length2yo.cm.,breaks = 10)
h$counts <- h$counts / sum(h$counts)

# #graph
# png("51_S.png",width=1200, height=900, res=150)# save plot
# with(MV, plot(h, main="from M or V(gamma dist)",
#               freq=T,
#               ylim=c(0,0.5),
#               ylab="relative frequency",
#               col="grey",
#               xlab = "Length new shoots (cm)"))
# with(q,lines(y~x, type="l", lwd=1.5))
# dev.off()


