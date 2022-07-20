#box4: do the bud (V and M) burst?
#objectiv: using interaction with fate to understand the % of bursting
#in syllepotic shoots

#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

#import lybrary
library(stats)
library(dplyr)
library(RColorBrewer)
library(effects)
library(data.table)

#import dataset
lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#subset dataset
SYL_bud_scale=lat[lat$from_=="SYL",]
#changing column names
colnames(SYL_bud_scale)[c(2,7,8,18)]=c("parent_length_cm","parent_length_node","parent_rank_node","m_v")
#removing the catkins from the count of M and V
SYL_bud_scale$m_v=SYL_bud_scale$m_v-1
#subset dataset_ from V
V=SYL_bud_scale[SYL_bud_scale$fate=="V",]
#subset dataset_ from M
M=SYL_bud_scale[SYL_bud_scale$fate=="M",]
#subset dataset_ from V&M
MV=SYL_bud_scale[SYL_bud_scale$fate=="M"|SYL_bud_scale$fate=="V",]
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")
str(MV)

#parameters: length(cm), length(node), rank_node, distance, m, v, fate
#every time 1 parameter is not sig. or permutation shows difference >1% i discard it

#1: new_shoots~fate*parent_length_cm+fate*parent_length_node+fate*parent_rank_node+fate*median_distance+fate*m+fate*v+fate*m_v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*parent_length_node+fate*parent_rank_node+fate*median_distance+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#2: new_shoots~fate*parent_length_cm+fate*parent_rank_node+fate*median_distance+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*parent_length_node+fate*parent_rank_node+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#3: new_shoots~fate*parent_length_cm+fate*median_distance+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*parent_rank_node+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#4: new_shoots~fate*parent_length_cm+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#null model
null_1=glm(new_shoots~fate+fate*m+fate*v+1, family="binomial",data=MV)
#difference: real_aic-null_aic
dif=glm_box1$aic-null_1$aic
#new df
met_nul=MV
#empty df for permutations
df=data.frame(matrix(nrow=0, ncol=0))
#permuting length 10.000 times
for (i in 1:10000) {
  met_nul$parent_length_cm=sample(MV$parent_length_cm)
  perm=glm(new_shoots~fate+fate*parent_length_cm+fate*m+fate*v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}
#variable for how many times perm_aic-null_aic was < of real_aic-null_aic
better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~fate*m+fate*v####
colnames(MV)[c(13,14)]=c("other_V", "other_M")
glm_box1 = glm(new_shoots~fate*other_M+fate*other_V, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)
# #save results as table
# library(xtable)
# out=xtable(glm_box1)
# print.xtable(out, type="html", file="tab1.html")
# library(pagedown)
# chrome_print("tab1.html", output = "tab1.pdf")
#coefficient
coef=coef(glm_box1)
#odds (succes/insucces)
ods=exp(coef)
#probabilities (odds/1+odds)
prob=ods/(1+ods)

#graph of the predictions_
# png("4c_S.png",width=1200, height=900, res=150)# save plot
# plot(allEffects(glm_box1))
# dev.off()

#create real data
#fateM with other M
MV=SYL_bud_scale[SYL_bud_scale$fate=="M"|SYL_bud_scale$fate=="V",]
M_m=data.frame("new shoots"=prop.table(table(M$m, M$new_shoots),1)[,2])
#fateV with other M
V_m=data.frame("new shoots"=prop.table(table(V$m, V$new_shoots),1)[,2])
#generate a glm without "other V"
mod1= glm(new_shoots~fate*m, family="binomial",data=MV)
summary(mod1)

#fateM with other V
M_v=data.frame("new shoots"=prop.table(table(M$v, M$new_shoots),1)[,2])
#fateV with other V
V_v=data.frame("new shoots"=prop.table(table(V$v, V$new_shoots),1)[,2])
#generate a glm without "other M"
mod2= glm(new_shoots~fate*v, family="binomial",data=MV)
summary(mod2)

# #graph
# png("4c1_S.png",width=1200, height=900, res=150)# save plot
# par(mfrow=c(1,2))
# cols<-brewer.pal(n=3,name="Set2")
# par(mar=c(5,4,1,0)+0.1)
# #real data
# with(M_m,plot(new.shoots~rownames(M_m), ylim=c(0,1),
#               ylab = "new shoot proportion",
#               xlab = "other M buds",
#               pch=19,
#               col=cols[1]))
# #predict fate M without other v
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="M", "other_M"=seq(0,5),
#                                               "other_V"=mean(M$v)),
#                          type="response"),col=cols[1]))
# #compute confidence interval
# pred=predict(glm_box1,newdata = data.frame("fate"="M",
#                                            "other_M"=seq(0,5),"other_V"=mean(M$v)),se.fit = T)
# lw<-with(pred, plogis(fit + qnorm(0.025)*se.fit))
# up<-with(pred, plogis(fit + qnorm(0.975)*se.fit))
# #cofidence intervals
# with(M_m, polygon(x=c(seq(5,0),seq(0,5)),
#                   y=c(rev(lw),up),
#                   col=rgb(0,1,0.8,0.5), border = NA))
# #fateV
# with(V_m,points(V_m$new.shoots~rownames(V_m), pch=19,
#                 ylim=c(0,1),
#                 col=cols[2]))
# #legend
# legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# par(mar=c(5,0,1,4)+0.1)
# with(M_v,plot(new.shoots~rownames(M_v), ylim=c(0,1),
#               ylab=NA,
#               xlab="other V buds",pch=19,
#               yaxt='n',
#               col=cols[1]))
# with(V_v,points(new.shoots~rownames(V_v), pch=19,
#                 col=cols[2],
#                 ylim=c(0,1)))
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                col=cols[2],
#                y=predict(glm_box1, newdata = data.frame("fate"="V", "other_M"=mean(V$m), "other_V"=seq(0,5)),
#                          type="response")))
# #compute confidence interval
# pred=predict(glm_box1,newdata = data.frame("fate"="V", "other_V"=seq(0,5),"other_M"=mean(V$m)),se.fit = T)
# lw<-with(pred, plogis(fit + qnorm(0.025)*se.fit))
# up<-with(pred, plogis(fit + qnorm(0.975)*se.fit))
# #cofidence intervals
# with(V_m, polygon(x=c(seq(5,0),seq(0,5)),
#                   y=c(rev(lw),up),
#                   col=rgb(1,0.5,0,0.5), border = NA))
# legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# dev.off()

# #graph
# png("4c2_S.png",width=1200, height=900, res=150)# save plot
# par(mfrow=c(1,2))
# cols<-brewer.pal(n=3,name="Set2")
# par(mar=c(5,4,1,0)+0.1)
# with(M_m,plot(new.shoots~rownames(M_m), ylim=c(0,1), 
#               ylab = "new shoot proportion",
#               xlab = "other M buds",
#               pch=19,
#               col=cols[1]))
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="M", "m"=seq(0,5),
#                                               "v"=0),
#                          type="response"),col="red"))
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="M", "m"=seq(0,5),
#                                               "v"=mean(M$v)),
#                          type="response"),col="blue"))
# with(M_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod1,
#                          newdata = data.frame("fate"="M", "m"=seq(0,5)),
#                          type="response"),col="black"))
# with(V_m,points(V_m$new.shoots~rownames(V_m), pch=19,
#                 ylim=c(0,1),
#                 col=cols[2]))
# with(V_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="V", "m"=seq(0,5),
#                                               "v"=0),
#                          type="response"),col="red"))
# with(V_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="V", "m"=seq(0,5),
#                                               "v"=mean(V$v)),
#                          type="response"),col="blue"))
# with(V_m,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod1,
#                          newdata = data.frame("fate"="V", "m"=seq(0,5)),
#                          type="response"),col="black"))
# legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# with(M_m, legend("bottomleft", horiz = F, 
#                  legend=c("otherV=0", "otherV=mean(otherV)", "newglm !othrerV"),
#                  pch=c(19,19,19),col=c("red", "blue", "black"), cex=0.5))
# par(mar=c(5,0,1,4)+0.1)
# with(M_v,plot(new.shoots~rownames(M_v), ylim=c(0,1),
#               ylab=NA,
#               xlab="other V buds",pch=19,
#               yaxt='n',
#               col=cols[1]))
# with(M_v,lines(x=seq(0,5),
#                lwd=2,
#                col="red",
#                y=predict(glm_box1, newdata = data.frame("fate"="M", "m"=0, "v"=seq(0,5)),
#                          type="response")))
# with(M_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="M", "v"=seq(0,5),
#                                               "m"=mean(M$m)),
#                          type="response"),col="blue"))
# with(M_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod2,
#                          newdata = data.frame("fate"="M", "v"=seq(0,5)),
#                          type="response"),col="black"))
# with(V_v,points(new.shoots~rownames(V_v), pch=19,
#                 col=cols[2],
#                 ylim=c(0,1)))
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                col="red",
#                y=predict(glm_box1, newdata = data.frame("fate"="V", "m"=0, "v"=seq(0,5)),
#                          type="response")))
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(glm_box1,
#                          newdata = data.frame("fate"="V", "v"=seq(0,5),
#                                               "m"=mean(V$m)),
#                          type="response"),col="blue"))
# with(V_v,lines(x=seq(0,5),
#                lwd=2,
#                y=predict(mod2,
#                          newdata = data.frame("fate"="V", "v"=seq(0,5)),
#                          type="response"),col="black"))
# with(M_m, legend("bottomleft", horiz = F, 
#                  legend=c("otherM=0", "otherM=mean(otherM)", "newglm !othrerM"),
#                  pch=c(19,19,19),col=c("red", "blue", "black"), cex=0.5))
# legend("top", horiz = T, legend=c("fate M", "fate V"), pch=c(19,19),col=cols[c(1,2)], cex=0.8)
# dev.off()
