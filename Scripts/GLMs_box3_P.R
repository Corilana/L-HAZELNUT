setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)
library(RColorBrewer)

met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met[met$shoot_type=="SYLLEPTIC",20]=1
met[met$shoot_type=="PROLEPTIC",20]=0
met$shoot_type=as.numeric(met$shoot_type)

#box3:proportion of V
#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
PROL_met_scale=met[met$shoot_type==0,]#df at met scale proleptic shoots
#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,15,17)]=c("parent_length_cm",
                                            "parent_length_nodes",
                                            "parent_rank_node",
                                            "distance",
                                            "tot_buds",
                                            "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#REMOVING THE COUNTING OF CATKINS BECAUSE ALL PROLLEPTIC HAS CATKINS

#1:proportion of V ~length?####
glm_box1=glm(cbind(v,m)~parent_length_cm,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#df: real %v ~ length
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$parent_length_cm))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  p_length=unique(PROL_met_scale$parent_length_cm)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$parent_length_cm==p_length,mv])
  V=sum(PROL_met_scale[PROL_met_scale$parent_length_cm==p_length,v])
  ratio=V/MV
  prop=rbind(prop, cbind(p_length,V,MV, ratio))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#histogram
png("box3_1p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=3,name="Set2")[1]
x<-barplot(prop$ratio,col = cols,
           names.arg = prop$p_length,
           main="%v (v/m+v)~parent length(cm)",xlab= "Parent length (cm)", ylab="%v (v/m+v)", ylim=c(0,1))
dev.off()

#2:proportion of V ~length?#####
glm_box1=glm(cbind(v,m)~parent_length_nodes,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#df:real %v ~ length
prop=met[0,0]#empry df
nline=length(unique(PROL_met_scale$parent_length_nodes))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  p_length=unique(PROL_met_scale$parent_length_nodes)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$parent_length_nodes==p_length,mv])
  V=sum(PROL_met_scale[PROL_met_scale$parent_length_nodes==p_length,v])
  ratio=V/MV
  prop=rbind(prop, cbind(p_length,V,MV, ratio))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#histogram
png("box3_2p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,col = cols[1],
               names.arg = prop$p_length,
               main="%v (v/m+v)~parent length(node)",xlab= "Parent length (node)", ylab="%v (v/m+v)", ylim=c(0,1))
dev.off()

#3:proportion of V ~ distance?####
glm_box1=glm(cbind(v,m)~distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#df:real %v ~ distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  distance=unique(PROL_met_scale$distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$distance==distance,mv])
  V=sum(PROL_met_scale[PROL_met_scale$distance==distance,v])
  ratio=V/MV
  prop=rbind(prop, cbind(distance,V,MV, ratio))
}

prop=prop[with(prop, order(distance)),]#order according to distance

#df: the predict ~ distance
pred=as.data.frame.matrix(cbind(PROL_met_scale$distance,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$distance)){prop$predict=pred$V2}

#histogram 
png("box3_3p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,col = cols[1],
           names.arg = prop$distance,
           main="%v (v/m+v)~distance from median node",xlab= "distance from median node", ylab="%v (v/m+v)", ylim=c(0,1))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(cbind(v,m)~1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$distance=sample(PROL_met_scale$distance)
  perm=glm(cbind(v,m)~distance,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4:proportion of V ~ rank node?####
glm_box1=glm(cbind(v,m)~parent_rank_node,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#df:real %v ~ rank
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$parent_rank_node))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$parent_rank_node)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$parent_rank_node==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$parent_rank_node==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

prop=prop[with(prop, order(rank)),]#order according to rank

#df:predict %v ~ rank
pred=as.data.frame.matrix(cbind(PROL_met_scale$parent_rank_node,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$rank)){prop$predict=pred$V2}

#histogram
png("box3_4p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,col = cols[1],
               names.arg = prop$rank,
               main="%v (v/m+v)~rank node",
               xlab= "rank node",
               ylab="%v (v/m+v)", ylim=c(0,1))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(cbind(v,m)~1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(PROL_met_scale$parent_rank_node)
  perm=glm(cbind(v,m)~parent_rank_node,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!
