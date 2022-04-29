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
MV=SYL_bud_scale[SYL_bud_scale$fate=="M"|SYL_bud_scale$fate=="V",]#for fate= "M"
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")
str(MV)
#parameters: length(cm), length(node), rank_node, distance, m, v, fate
#every time 1 parameter is not sig. or permutation shows difference >1% i discard it
#1: new_shoots~fate*parent_length_cm+fate*parent_length_node+fate*parent_rank_node+fate*median_distance+fate*m+fate*v+fate*m_v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*parent_length_node+fate*parent_rank_node+fate*median_distance+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#2: new_shoots~fate*parent_length_cm+fate*parent_rank_node+fate*median_distance+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*parent_rank_node+fate*median_distance+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#3: new_shoots~fate*parent_length_cm+fate*median_distance+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*median_distance+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#4: new_shoots~fate*parent_length_cm+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*parent_length_cm+fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)
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

#5: new_shoots~new_shoots~fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*m+fate*v, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)
#coefficient
coef=coef(glm_box1)
#odds (succes/insucces)
ods=exp(coef)
#probabilities (odds/1+odds)
prob=ods/(1+ods)
#graph of the predictions_
png("4c_S.png",width=1200, height=900, res=150)# save plot
with(plot(allEffects(glm_box1)))
dev.off()
#real_data_from M
dfM=as.data.frame(prop.table(table(M$m,M$new_shoots),1)[,2])
colnames(dfM)="real"
#predicted_data
#create seq from 0-5 (number of v)
for (i in seq(0,5, by = 1)) {
  dfM=cbind(dfM, data.frame(rep(i, length(unique(M$m)))))
}
#name the new columns
colnames(dfM)[2:7]="v"
#make new column with number of m (0-5)
dfM$m=as.numeric(rownames(dfM)) 
#make new column with fate M
dfM$fate="M"
#predictmodel
for (i in 2:7) {dfM=cbind(dfM,predict(glm_box1,newdata = dfM[,c(i,8,9)],type="response"))}
#name the new columns with number of m (0-5)
colnames(dfM)[10:15]=seq(0,5, by = 1)
#graph
png("4d_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(dfM, plot(dfM$real~dfM$m, ylim=c(0,1), col=cols[1], type="h", lwd=5,
               main= "new_shoots from M in sylleptic",
               xlab="other M buds in the same sylleptic",
               ylab="% new shoots"))
for (i in 1:length(grep("[0-9]", colnames(dfM)))) {
  t=grep("[0-9]", colnames(dfM))[i]
  with(dfM,lines(dfM[,t]~dfM$m,col=rbPal[i], lwd=3))
}
legend("top",
       horiz=T,
       xpd = TRUE,c("real", "v=0","v=1","v=2","v=3","v=4","v=5"),
       pch = c(19,NA,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1,1), lwd=5,
       col = c(cols[1], rbPal),
       cex=0.6)
dev.off()
#real_data_from V
dfV=as.data.frame(prop.table(table(V$v,V$new_shoots),1)[,2])
colnames(dfV)="real"
#predicted_data
#create seq from 0-5 (number of m)
for (i in seq(0,3, by = 1)) {
  dfV=cbind(dfV, data.frame(rep(i, length(unique(V$v)))))
}
#name the new columns
colnames(dfV)[2:5]="m"
#make new column with number of m (0-5)
dfV$v=as.numeric(rownames(dfV)) 
#make new column with fate V
dfV$fate="V"
#predictmodel
for (i in 2:5) {dfV=cbind(dfV,predict(glm_box1,newdata = dfV[,c(i,6,7)],type="response"))}
#name the new columns with number of  (0-5)
colnames(dfV)[8:11]=seq(0,3, by = 1)
#graph
png("4e_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(dfV, plot(dfV$real~dfV$v, ylim=c(0,1), col=cols[1], type="h", lwd=5,
               main= "new_shoots from V in sylleptic",
               xlab="other V buds in the same sylleptic",
               ylab="% new shoots"))
for (i in 1:length(grep("[0-9]", colnames(dfV)))) {
  t=grep("[0-9]", colnames(dfV))[i]
  with(dfV,lines(dfV[,t]~dfV$v,col=rbPal[i], lwd=3))
}
legend("top",
       horiz=T,
       xpd = TRUE,c("real", "m=0","m=1","m=2","m=3"),
       pch = c(19,NA,NA,NA,NA),lty=c(NA,1,1,1,1), lwd=5,
       col = c(cols[1], rbPal),
       cex=0.6)
dev.off()

#6: new_shoots~new_shoots~fate*m+fate*v####
MV$sib=MV$m+MV$v
glm_box1 = glm(new_shoots~fate*sib, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

