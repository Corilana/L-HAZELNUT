setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)
library(RColorBrewer)

met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met[met$shoot_type=="SYLLEPTIC",20]=1#sylleptic is now 1
met[met$shoot_type=="PROLEPTIC",20]=0#proleptic is now 0
met$shoot_type=as.numeric(met$shoot_type)

#box2:how many buds v and m?
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL_met_scale)[c(2,6,7,8,15,17)]=c("parent_length_cm",
                               "parent_length_nodes",
                               "parent_rank_node",
                               "distance",
                               "tot_buds_in_sylleptic",
                               "m_v_in_sylleptic")
SYL_met_scale$m_v_in_sylleptic=SYL_met_scale$m_v_in_sylleptic-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS

#1: m+v buds ~ parent lenght (cm)?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#m+v buds ~ parent lenght (cm)?
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_length_cm))
mv=grep("^m_v", colnames(SYL_met_scale))
for (i in 1:nline) {
  p_length=unique(SYL_met_scale$parent_length_cm)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_cm==p_length,mv])
  prop=rbind(prop, cbind(p_length,MV))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df:predict~length
pred=as.data.frame.matrix(cbind(SYL_met_scale$parent_length_cm,predict(glm_box1,type="response")))
df=met[0,0]#empty df
nline=length(unique(pred$V1))
for (i in 1:nline) {
  p_length=unique(pred$V1)[i]
  MV=sum(pred[pred$V1==p_length,2])
  df=rbind(df, cbind(p_length,MV))
}
pred=df
pred=pred[with(pred, order(p_length)), ]
if (all.equal(pred$p_length,prop$p_length)){prop$predict=pred$MV}

#histogram
png("box2_1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_length,
               col = cols[1],
               main="# of m+v in sylleptic~parent length(cm)",
               xlab= "parent length(cm)", 
               ylab="# m+v in sylleptic", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#permutations
null_1=glm(m_v_in_sylleptic~1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_cm=sample(SYL_met_scale$parent_length_cm)
  perm=glm(m_v_in_sylleptic~parent_length_cm,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2: m+v buds ~ parent lenght (node)?####
glm_box1=glm(m_v_in_sylleptic~parent_length_nodes,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#m+v buds ~ parent lenght (node)?
prop=met[0,0]#empry df
nline=length(unique(SYL_met_scale$parent_length_nodes))
mv=grep("^m_v", colnames(SYL_met_scale))
for (i in 1:nline) {
  p_length=unique(SYL_met_scale$parent_length_nodes)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_nodes==p_length,mv])
  prop=rbind(prop, cbind(p_length,MV))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df:predict~length
pred=as.data.frame.matrix(cbind(SYL_met_scale$parent_length_nodes,predict(glm_box1,type="response")))
df=met[0,0]#empry df
nline=length(unique(pred$V1))
for (i in 1:nline) {
  p_length=unique(pred$V1)[i]
  MV=sum(pred[pred$V1==p_length,2])
  df=rbind(df, cbind(p_length,MV))
}
pred=df
pred=pred[with(pred, order(p_length)), ]
if (all.equal(pred$p_length,prop$p_length)){prop$predict=pred$MV}

#histogram 
png("box2_2.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_length,
               col = cols[1],
               main="# of m+v in sylleptic~parent length(nodes)",
               xlab= "parent length(nodes)", 
               ylab="# m+v in sylleptic", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#permutations
null_1=glm(m_v_in_sylleptic~1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_nodes=sample(SYL_met_scale$parent_length_nodes)
  perm=glm(m_v_in_sylleptic~parent_length_nodes,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!


#3: m+v buds ~ distance?####
glm_box1=glm(m_v_in_sylleptic~distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#m+v buds ~ distance
prop=met[0,0]#empry df
nline=length(unique(SYL_met_scale$distance))
mv=grep("^m_v", colnames(SYL_met_scale))
for (i in 1:nline) {
  distance=unique(SYL_met_scale$distance)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$distance==distance,mv])
  prop=rbind(prop, cbind(distance,MV))
}

prop=prop[with(prop, order(distance)),]#order according to parenth length

#df:predict~distance
pred=as.data.frame.matrix(cbind(SYL_met_scale$distance,predict(glm_box1,type="response")))
df=met[0,0]#empry df
nline=length(unique(pred$V1))
for (i in 1:nline) {
  distance=unique(pred$V1)[i]
  MV=sum(pred[pred$V1==distance,2])
  df=rbind(df, cbind(distance,MV))
}
pred=df
pred=pred[with(pred, order(distance)), ]
if (all.equal(pred$distance,prop$distance)){prop$predict=pred$MV}

#histogram 
png("box2_3.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$distance,
               col = cols[1],
               main="# of m+v in sylleptic~distance from median node",
               xlab= "distance from median node", 
               ylab="# m+v in sylleptic", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#permutations
null_1=glm(m_v_in_sylleptic~1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$distance=sample(SYL_met_scale$distance)
  perm=glm(m_v_in_sylleptic~distance,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: m+v buds ~ parent lenght(cm)+rank_node?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#df: m+v buds ~ parent lenght(cm)
prop=met[0,0]#empry df
nline=length(unique(SYL_met_scale$parent_length_cm))
mv=grep("^m_v", colnames(SYL_met_scale))
for (i in 1:nline) {
  p_length=unique(SYL_met_scale$parent_length_cm)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_cm==p_length,mv])
  prop=rbind(prop, cbind(p_length,MV))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df: predict~ parent lenght(cm)+rank_node
pred=as.data.frame.matrix(cbind(SYL_met_scale$parent_length_cm,predict(glm_box1,type="response")))
df=met[0,0]#empry df
nline=length(unique(pred$V1))
for (i in 1:nline) {
  p_length=unique(pred$V1)[i]
  MV=sum(pred[pred$V1==p_length,2])
  df=rbind(df, cbind(p_length,MV))
}
pred=df
pred=pred[with(pred, order(p_length)), ]
if (all.equal(pred$p_length,prop$p_length)){prop$predict_length=pred$MV}

#histogram 
png("box2_4.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_length,
               col = cols[1],
               main="# of m+v in syllepotic~parent_length(cm)",
               xlab= "parent_length(cm)", 
               ylab="# m+v in sylleptic", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#m+v buds ~ rank_node 
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_rank_node))
mv=grep("^m_v", colnames(SYL_met_scale))
for (i in 1:nline) {
  p_rank=unique(SYL_met_scale$parent_rank_node)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_rank_node==p_rank,mv])
  prop=rbind(prop, cbind(p_rank,MV))
}

prop=prop[with(prop, order(p_rank)),]#order according to parenth length

#df: predict~parent lenght(cm)+rank_node
pred=as.data.frame.matrix(cbind(SYL_met_scale$parent_rank_node,predict(glm_box1,type="response")))
df=met[0,0]#empty df
nline=length(unique(pred$V1))
for (i in 1:nline) {
  p_rank=unique(pred$V1)[i]
  MV=sum(pred[pred$V1==p_rank,2])
  df=rbind(df, cbind(p_rank,MV))
}
pred=df
pred=pred[with(pred, order(p_rank)), ]
if (all.equal(pred$p_rank,prop$p_rank)){prop$predict_rank=pred$MV}

#histogram 
png("box2_5.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_rank,
               col = cols[1],
               main="# of m+v in sylleptic~parent_rank node",
               xlab= "parent_rank node", 
               ylab="# m+v in sylleptic", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

# permutations
null_1=glm(m_v_in_sylleptic~parent_length_cm+1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(SYL_met_scale$parent_rank_node)
  perm=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#6: m+v buds ~ distance+rank_node?####
glm_box1=glm(m_v_in_sylleptic~distance+parent_rank_node,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#no