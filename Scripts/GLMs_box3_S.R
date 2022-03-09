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
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL_met_scale)[c(2,6,7,8,15,17)]=c("parent_length_cm",
                                            "parent_length_nodes",
                                            "parent_rank_node",
                                            "distance",
                                            "tot_buds_in_sylleptic",
                                            "m_v_in_sylleptic")
SYL_met_scale$m_v_in_sylleptic=SYL_met_scale$m_v_in_sylleptic-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS

#1:proportion of V ~length?####
glm_box1=glm(cbind(v,m)~parent_length_cm,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#no

#df: real %v ~ length
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_length_cm))
mv=grep("^m_v", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  p_length=unique(SYL_met_scale$parent_length_cm)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_cm==p_length,mv])
  V=sum(SYL_met_scale[SYL_met_scale$parent_length_cm==p_length,v])
  ratio=V/MV
  prop=rbind(prop, cbind(p_length,V,MV, ratio))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#histogram
png("box3_1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=3,name="Set2")[1]
x<-barplot(prop$ratio,col = cols,
           names.arg = prop$p_length,
           main="%v (v/m+v)~parent length(cm)",xlab= "Parent length (cm)", ylab="%v (v/m+v)", ylim=c(0,1))
dev.off()

#2:proportion of V ~length?#####
glm_box1=glm(cbind(v,m)~parent_length_nodes,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes

#df:real %v ~ length
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_length_nodes))
mv=grep("^m_v", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  p_length=unique(SYL_met_scale$parent_length_nodes)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_nodes==p_length,mv])
  V=sum(SYL_met_scale[SYL_met_scale$parent_length_nodes==p_length,v])
  ratio=V/MV
  prop=rbind(prop, cbind(p_length,V,MV, ratio))
}

prop=prop[with(prop, order(p_length)),]#order according to parenth length

#df: the predict ~ length
pred=as.data.frame.matrix(cbind(SYL_met_scale$parent_length_nodes,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$p_length)){prop$predict=pred$V2}

#histogram
png("box3_2.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,col = cols[1],
           names.arg = prop$p_length,
           main="%v (v/m+v)~parent length(node)",xlab= "Parent length (node)", ylab="%v (v/m+v)", ylim=c(0,1))
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
dev.off()

#permutation
null_1=glm(cbind(v,m)~1,family = "binomial",data = SYL_met_scale)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length_nodes=sample(SYL_met_scale$parent_length_nodes)
  perm=glm(cbind(v,m)~parent_length_nodes,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3:proportion of V ~ distance?####
glm_box1=glm(cbind(v,m)~distance,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#no

#df:real %v ~ distance
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$distance))
mv=grep("^m_v", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  distance=unique(SYL_met_scale$distance)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$distance==distance,mv])
  V=sum(SYL_met_scale[SYL_met_scale$distance==distance,v])
  ratio=V/MV
  prop=rbind(prop, cbind(distance,V,MV, ratio))
}

prop=prop[with(prop, order(distance)),]#order according to distance

#histogram
png("box3_3.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=3,name="Set2")[1]
x<-barplot(prop$ratio,col = cols,
           names.arg = prop$distance,
           main="%v (v/m+v)~distance from median node",xlab= "distance from median node", ylab="%v (v/m+v)", ylim=c(0,1))
dev.off()

#4:proportion of V ~ rank node?####
glm_box1=glm(cbind(v,m)~parent_length_nodes+parent_rank_node,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#no

#df:real %v ~ rank
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_rank_node))
mv=grep("^m_v", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  rank=unique(SYL_met_scale$parent_rank_node)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_rank_node==rank,mv])
  V=sum(SYL_met_scale[SYL_met_scale$parent_rank_node==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

prop=prop[with(prop, order(rank)),]#order according to rank

#histogram
png("box3_4.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,col = cols[1],
               names.arg = prop$rank,
               main="%v (v/m+v) in syleptic ~parental rank node",
               xlab= "parental rank node",
               ylab="%v (v/m+v)", ylim=c(0,1))
dev.off()
