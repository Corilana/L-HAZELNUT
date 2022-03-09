setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)
library(RColorBrewer)

#metamer level
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
s=grep("shoot_type",colnames(met))#store the column with the sylleptic info
met[met$shoot_type=="SYLLEPTIC",s]=1#1 is sylleptic
met[met$shoot_type=="PROLEPTIC",s]=0#0 is proleptic
met$shoot_type=as.numeric(met$shoot_type)#transform in numeric the info of sylleptic/proleptic
#change columns names to not make confusion
colnames(met)[c(20,2,6,7)]=c("is_sylleptic",
                             "parent_length_cm",
                             "parent_length_node",
                             "parent_rank_node")

#BOX1: DOES THAT RANK BEAR A SYLLEPTIC?

#1: is sylleptic~length(cm)####
glm_box1=glm(is_sylleptic~parent_length_cm,family = "binomial",data = met)
summary(glm_box1)#yes

#df % sylleptic ~ length
prop=as.data.frame.matrix(table(met$parent_length_cm,met$is_sylleptic))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("proleptic","sylleptic")
prop$length=as.numeric(rownames(prop))
head(prop)

#df:  predict ~ length
pred=as.data.frame.matrix(cbind(met$parent_length_cm,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$length)){prop$predict=pred$V2}

#histogram
png("box1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
           col = cols[1],
           main="%sylleptic (sylleptic/tot rank) vs parent length(cm)",
           xlab="Parent length (cm)",
           ylab="%sylleptic ",
           ylim=c(0,1),
           yaxp=c(0,1,4),
           names.arg = prop$length)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(is_sylleptic~1,family = "binomial",data = met)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length=sample(met$parent_length_cm)
  perm=glm(is_sylleptic~parent_length_cm,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2: sylleptic~length(cm)+rank####
glm_box1=glm(is_sylleptic~parent_length_cm+parent_rank_node,family = "binomial",data = met)
summary(glm_box1)#yes

#df: sylleptic~parent_length_cm
prop=as.data.frame.matrix(table(met$parent_length_cm,met$is_sylleptic))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_syl","is_nsyl")
prop$length=as.numeric(rownames(prop))
head(prop)

#df: predict~parent_length_cm
pred=as.data.frame.matrix(cbind(met$parent_length_cm,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]

nline=length(unique(pred$V1))
df=pred[0,0]
for (i in 1:nline) {
  I=unique(pred$V1)[i]
  av=mean(pred[pred$V1==I,2])
  se=std.error(pred[pred$V1==I,2])
  df=rbind.fill(df, cbind(prop[prop$length==I,],av,se))
}

prop=df
prop[is.na(prop$se),6]=0

#histogram
png("box1_2.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%sylleptic (sylleptic/tot rank) vs parent length(cm)",
               xlab="parent length(cm)",
               ylab="%%sylleptic",
               ylim=c(0,1),
               names.arg = prop$length)
lines(x=dfbar,y=prop$av,lwd=5, col=cols[2])
lines(x=dfbar, y=prop$av+prop$se, lty = 'dashed',col=cols[2])#+se
lines(x=dfbar, y=prop$av-prop$se, lty = 'dashed',col=cols[2])#-se
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#df: sylleptic~rank_node
prop=as.data.frame.matrix(table(met$parent_rank_node,met$is_sylleptic))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_syl","is_nsyl")
prop$rank=as.numeric(rownames(prop))
head(prop)

#df: predict~rank_node
pred=as.data.frame.matrix(cbind(met$parent_rank_node,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]

nline=length(unique(pred$V1))
df=pred[0,0]
for (i in 1:nline) {
  I=unique(pred$V1)[i]
  av=mean(pred[pred$V1==I,2])
  se=std.error(pred[pred$V1==I,2])
  df=rbind.fill(df, cbind(prop[prop$rank==I,],av,se))
}

prop=df
prop[is.na(prop$se),6]=0

#histogram
png("box1_3.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%sylleptic (sylleptic/tot shoots) vs rank_node",
               xlab="rank_node",
               ylab="sylleptic",
               ylim=c(0,1),
               names.arg = prop$rank)
lines(x=dfbar,y=prop$av,lwd=5, col=cols[2])
lines(x=dfbar, y=prop$av+prop$se, lty = 'dashed',col=cols[2])#+se
lines(x=dfbar, y=prop$av-prop$se, lty = 'dashed',col=cols[2])#-se
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#permutations
null_1=glm(is_sylleptic~parent_length_cm+1,family = "binomial",data = met)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(met$parent_rank_node)
  perm=glm(is_sylleptic~parent_length_cm+parent_rank_node,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!


#solution: 
# Because sylleptic are distributed in a median zone along the parent shoot,
#the rank does not have a linear effect:
#we compute for each bud the distance to the shoot extremities,
#with reference 0 at the median rank of each shoot

#3: sylleptic~length(cm)+distance####
glm_box1=glm(is_sylleptic~parent_length_cm+median_distance,family = "binomial",data = met)
summary(glm_box1)#no

#permutations
null_1=glm(is_sylleptic~parent_length+1,family = "binomial",data = met)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$distance=sample(met$distance)
  perm=glm(is_sylleptic~parent_length+distance,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4: sylleptic~distance
glm_box1=glm(is_sylleptic~median_distance,family = "binomial",data = met)
summary(glm_box1)#yes

#df: %sylleptic~distance
prop=as.data.frame.matrix(table(met$median_distance,met$is_sylleptic))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("proleptic","sylleptic")
prop$distance=as.numeric(rownames(prop))
head(prop)

#histogram
png("box1_4.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=3,name="Set2")[1:2]
x<-barplot(t(prop[1:2]),col = cols,
           main="#sylleptic/proleptic~distance",
           xlab= "distance from median(cm)",
           ylab="# sylleptic/proleptic",
           ylim=c(0,max(prop[1:2])+50))
legend("top",horiz=T,inset=c(0,-0.02),xpd = TRUE, legend = colnames(prop[1:2]),fill = cols, cex=0.6)
dev.off()

#df: predict~length
pred=as.data.frame.matrix(cbind(met$median_distance,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$distance)){prop$predict=pred$V2}

#histogram 
png("box1_5.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%sylleptic (sylleptic/tot shoots) vs distance from median node",
               xlab="distance from median(cm)",
               ylab="%sylleptic",
               ylim=c(0,1),
               yaxp=c(0,1,4),
               names.arg = prop$distance)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#permutations
null_1=glm(is_sylleptic~1,family = "binomial",data = met)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$distance=sample(met$median_distance)
  perm=glm(is_sylleptic~median_distance,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!