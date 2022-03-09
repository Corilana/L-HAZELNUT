setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)
library(RColorBrewer)

met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met[met$shoot_type=="SYLLEPTIC",20]=1#sylleptic is now 1
met[met$shoot_type=="PROLEPTIC",20]=0#proleptic is now 0
met$shoot_type=as.numeric(met$shoot_type)

PROL_met_scale=met[met$shoot_type==0,]

#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,15,17)]=c("parent_length_cm",
                                            "parent_length_nodes",
                                            "parent_rank_node",
                                            "distance",
                                            "tot_buds",
                                            "m_v")
#BOX.2.0 existence(0.1)
#1:b~length?####
glm_box1=glm(b~parent_length_cm,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#df %B~length
prop=as.data.frame.matrix(table(PROL_met_scale$parent_length_cm,PROL_met_scale$b))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_blind buds","si_bind buds")
prop$length=as.numeric(rownames(prop))
head(prop)

#df: predict~length
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_length_cm,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$length)){prop$predict=pred$V2}

#histogram
png("box2_1p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%blind nodes (#b/tot_nodes) vs parent length(cm)",
               xlab= "parent length(cm)",
               ylab="%blind nodes",
               ylim=c(0,1),
               names.arg = prop$length)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()

#permutations
null_1=glm(b~1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_cm=sample(PROL_met_scale$parent_length_cm)
  perm=glm(b~parent_length_cm,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2: b~length?####
glm_box1=glm(b~parent_length_nodes,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#df: %B~length
prop=as.data.frame.matrix(table(PROL_met_scale$parent_length_nodes,PROL_met_scale$b))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_blind buds","si_bind buds")
prop$length=as.numeric(rownames(prop))
head(prop)

#df:predict~length
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_length_nodes,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$length)){prop$predict=pred$V2}

#histogram
png("box2_2p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%blind nodes (#b/tot_nodes) vs parent length(nodes)",
               xlab= "parent length(nodes)",
               ylab="%blind nodes",
               ylim=c(0,1),
               names.arg = prop$length)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()

#permutations
null_1=glm(b~1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_nodes=sample(PROL_met_scale$parent_length_nodes)
  perm=glm(b~parent_length_nodes,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3:b~length+rank?####
glm_box1=glm(b~parent_length_nodes+parent_rank_node,family = "binomial",data =PROL_met_scale)
summary(glm_box1)#yes

#4:b~length+rank?####
glm_box1=glm(b~parent_rank_node,family = "binomial",data =PROL_met_scale)
summary(glm_box1)#yes

#df: %b~rank
prop=as.data.frame.matrix(table(PROL_met_scale$parent_rank_node,PROL_met_scale$b))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_blind buds","si_bind buds")
prop$rank=as.numeric(rownames(prop))
head(prop)

#df:predict~length
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_rank_node,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$rank)){prop$predict=pred$V2}

#histogram
png("box2_3p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%blind nodes (#b/tot_nodes) vs parent rank",
               xlab= "parent rank",
               ylab="%blind nodes",
               ylim=c(0,1),
               names.arg = prop$rank)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(b~1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(PROL_met_scale$parent_rank_node)
  perm=glm(b~parent_rank_node,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!


#BOX2
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#delete the blind nodes from the counting of total buds at that metamer

#1_m+v buds ~ parent lenght (cm)?####
glm_box1=glm(m_v~parent_length_cm,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#df: #m_v~length
prop=met[0,0]#empry df
nline=length(unique(PROL_met_scale$parent_length_cm))
mv=grep("^m_v", colnames(PROL_met_scale))
for (i in 1:nline) {
  p_length=unique(PROL_met_scale$parent_length_cm)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$parent_length_cm==p_length,mv])
  prop=rbind(prop, cbind(p_length,MV))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df: predicted~length
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_length_cm,predict(glm_box1,type="response")))
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
png("box2_4p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_length,
               col = cols[1],
               main="# of m+v~parent length(cm)",
               xlab= "parent length(cm)", 
               ylab="# m+v", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(m_v~1,family = "poisson",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_cm=sample(PROL_met_scale$parent_length_cm)
  perm=glm(m_v~parent_length_cm,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2: m+v buds ~ parent lenght (node)?####
glm_box1=glm(m_v~parent_length_nodes,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#df: #m_v~length
prop=met[0,0]#empry df
nline=length(unique(PROL_met_scale$parent_length_nodes))
mv=grep("^m_v", colnames(PROL_met_scale))
for (i in 1:nline) {
  p_length=unique(PROL_met_scale$parent_length_nodes)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$parent_length_nodes==p_length,mv])
  prop=rbind(prop, cbind(p_length,MV))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df: predict~length
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_length_nodes,predict(glm_box1,type="response")))
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
png("box2_5p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_length,
               col = cols[1],
               main="# of m+v~parent length(nodes)",
               xlab= "parent length(nodes)", 
               ylab="# m+v", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(m_v~1,family = "poisson",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_nodes=sample(PROL_met_scale$parent_length_nodes)
  perm=glm(m_v~parent_length_nodes,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3: m+v buds ~ distance?####
glm_box1=glm(m_v~distance,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#no

#df: m+v~distance
prop=met[0,0]#empry df
nline=length(unique(PROL_met_scale$distance))
mv=grep("^m_v", colnames(PROL_met_scale))
for (i in 1:nline) {
  distance=unique(PROL_met_scale$distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$distance==distance,mv])
  prop=rbind(prop, cbind(distance,MV))
}

prop=prop[with(prop, order(distance)),]#order according to parenth length

#histogram
png("box2_6p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$MV,
               names.arg = prop$distance,
               col = cols[1],
               main="# of m+v~distance from median node",
               xlab= "distance from median node", 
               ylab="# m+v", 
               ylim=c(0,max(prop$MV)+10))

dev.off()

#4: m+v buds ~ parent lenght(cm)+rank_node?####
glm_box1=glm(m_v~parent_length_cm+parent_rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#5: m+v buds ~ rank_node?####
glm_box1=glm(m_v~parent_rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#df: m+v ~ rank node
prop=met[0,0]#empry df
nline=length(unique(PROL_met_scale$parent_rank_node))
mv=grep("^m_v", colnames(PROL_met_scale))
for (i in 1:nline) {
  p_length=unique(PROL_met_scale$parent_rank_node)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$parent_rank_node==p_length,mv])
  prop=rbind(prop, cbind(p_length,MV))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df:predict~length
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_rank_node,predict(glm_box1,type="response")))
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
png("box2_7p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(3,4)]
dfbar<-barplot(prop$MV,
               names.arg = prop$p_length,
               col = cols[1],
               main="# of m+v~rank_node",
               xlab= "rank_node", 
               ylab="# m+v", 
               ylim=c(0,max(prop$MV)+10))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(m_v~1,family = "poisson",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(PROL_met_scale$parent_rank_node)
  perm=glm(m_v~parent_rank_node,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#6: m+v buds ~ distance+rank_node?####
glm_box1=glm(m_v~distance+parent_rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#no
