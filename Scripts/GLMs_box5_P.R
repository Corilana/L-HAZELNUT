#what is the length of children?
#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))
#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(nnet)
library(effects)

lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#box4: do the bud (V and M) burst?
PRO_bud_scale=lat[lat$from_=="PROL",]
#change columns names to not make confusion
a=grep("^Length$", names(PRO_bud_scale))
b=grep(".node.$", names(PRO_bud_scale))
c=grep("rank", names(PRO_bud_scale))
d=grep("abs", names(PRO_bud_scale))
e=grep("tot_", names(PRO_bud_scale))
colnames(PRO_bud_scale)[c(a,b,c,d,e)]=c("length_cm","length_node","rank_node","distance","m_v")
MV=PRO_bud_scale[(PRO_bud_scale$fate=="M"|PRO_bud_scale$fate=="V")&PRO_bud_scale$new_shoots,]
MV$sib=MV$v+MV$m+MV$b
MV$normal_median=MV$median_distance/MV$length_node

#parameters: length(cm), length(node), rank_node, distance, m, v
#M and V####
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")
str(MV)

colnames(MV)[24]="nodes"
#plot
plot(density(MV$length2yo.cm.))
#1:length~length+lengthnode+rank+median_distance+sib+fate####
glm_box1 = glm(length2yo.cm.~length_cm*fate+rank_node*fate+normal_median*fate+sib*fate, data = MV, family = "gaussian")
summary(glm_box1)
#remove sib
#2:length~length+lengthnode+rank+median_distance+fate####
glm_box1 = glm(length2yo.cm.~length_cm*fate+rank_node*fate+normal_median*fate, data = MV, family = "gaussian")
summary(glm_box1)
#remove rank
#3:length~length+lengthnode+median_distance+fate####
glm_box1 = glm(length2yo.cm.~length_cm*fate+normal_median*fate, data = MV)
summary(glm_box1)

#save results as table
library(xtable)
out=xtable(glm_box1)
print.xtable(out, type="html", file="tab4.html")
library(pagedown)
chrome_print("tab4.html", output = "tab4.pdf")

# #plot all effects
# png("51_P.png",width=1200, height=900, res=150)# save plot
# with(plot(allEffects(glm_box1)))
# dev.off()

#create real data
df=MV[0,0]

nfate=unique(MV$fate)
class_length=seq(0,75, by=0.5)
for (j in nfate) {
  fate=j
  for (i in class_length) {
    length_cm=i
    length2yo.cm.=mean(MV[MV$fate==j&MV$length_cm==i,"length2yo.cm."])
    sd=sd(MV[MV$fate==j&MV$length_cm==i,"length2yo.cm."])
    df=rbind(df,cbind(fate,length2yo.cm.,sd,length_cm))
  }
}

df$length_cm=as.numeric(df$length_cm)
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate normal_median)
for (j in c(-0.5, 0, 0.5)) {
  df=cbind(df, data.frame(rep(j, length(df$fate))))
}
#rename columns
colnames(df)[5:7]="normal_median"

conf_int=df
#predict model according to normal_median, for each type of sequence of distance (0-10)
for (i in grep("normal_median", colnames(df))) {
  pred=predict(glm_box1,newdata = df[c(1,4,i)],type="response", se.fit=T)
  df=cbind(df,pred$fit)
}
#rename columns
colnames(df)[8:10]=c(-0.5, 0, 0.5)

dupli=df[8:10]
dupli[dupli<0]=NA
df[8:10]=dupli

#confidence intervel
for (i in grep("normal_median", colnames(df))) {
  pred=predict(glm_box1,newdata = conf_int[c(1,4,i)],se.fit = T)
  lw=pred$fit+qnorm(0.025)*pred$se.fit
  up=pred$fit+qnorm(0.975)*pred$se.fit
  lim=as.data.frame(cbind(lw,up))
  conf_int=cbind(conf_int,lim)
}

M=df[df$fate=="M",]
V=df[df$fate=="V",]

dupl=conf_int[c(8:13)]
dupl[dupl<0]=0
conf_int[c(8:13)]=dupl

conf_M=conf_int[conf_int$fate=="M",]
conf_V=conf_int[conf_int$fate=="V",]

library(plotfunctions)
#graph
# png("5c1_P.png",width=1200, height=900, res=150)# save plot
# cols<-brewer.pal(n=3,name="Set2")
# rbPal <- brewer.pal(n=6, name="Set1")[c(1,3,5)]
# transp<-alphaPalette(rbPal, rep(0.25,3))
# par(mfrow=c(1,2))
# par(mar=c(5,4,1,0)+0.1)
# #real data
# with(M,plot(length2yo.cm.~length_cm,
#             main="fate M",
#               ylab = "new shoot length (cm)",
#               xlab = "parent length(cm)",
#             ylim=c(0,12),
#             xlim=c(0,75),
#               pch=19,
#               col=cols[1]))
# for (i in 1:length((M)[8:10])) {
#   t=colnames(M)[8:10][i]
#   with(M,lines(M[,t]~M$length_cm,col=rbPal[i], lwd=3))
# }
# for (i in 1:length(grep("lw", colnames(conf_M)))) {
#     j=grep("lw", colnames(conf_M))[i]
#     t_lw=j
#     t_up=j+1
#     with(conf_M[c(1,t_lw:t_up)], polygon(x=c(seq(75,0, by=-0.5),
#                                                 seq(0,75, by=0.5)),
#                       y=c(rev(lw),up),
#                       col=transp[i], border = NA))
# }
# legend("top",
#        horiz=F,
#        xpd = TRUE,c("real", "norm_dist=-0.5","norm_dist=0","norm_dist=+0.5"),
#        pch = c(19,NA,NA,NA),lty=c(NA,1,1,1), lwd=5,
#        col = c(cols[1], rbPal),
#        cex=0.9)
# #fateV
# par(mar=c(5,0,1,4)+0.1)
# with(V,plot(length2yo.cm.~length_cm,
#             main="fate V",
#             ylim=c(0,12),
#             xlab = "parent length(cm)",
#             yaxt='n',
#             ylab=NA,
#             xlim=c(0,75),
#             pch=19,
#             col=cols[2]))
# for (i in 1:length((V)[8:10])) {
#   t=colnames(V)[8:10][i]
#   with(V,lines(V[,t]~V$length_cm,col=rbPal[i], lwd=3))
# }
# for (i in 1:length(grep("lw", colnames(conf_V)))) {
#   j=grep("lw", colnames(conf_V))[i]
#   t_lw=j
#   t_up=j+1
#   with(conf_V[c(1,t_lw:t_up)], polygon(x=c(seq(75,0, by=-0.5),
#                                            seq(0,75, by=0.5)),
#                                        y=c(rev(lw),up),
#                                        col=transp[i], border = NA))
# }
# legend("top",
#        horiz=F,
#        xpd = TRUE,c("real", "norm_dist=-0.5","norm_dist=0","norm_dist=+0.5"),
#        pch = c(19,NA,NA,NA),lty=c(NA,1,1,1), lwd=5,
#        col = c(cols[2], rbPal),
#        cex=0.9)
# dev.off()
