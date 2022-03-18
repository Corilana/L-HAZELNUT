#box3:proportion of V
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)

met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met[met$shoot_type=="SYLLEPTIC",21]=1
met[met$shoot_type=="PROLEPTIC",21]=0
met$shoot_type=as.numeric(met$shoot_type)

#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
PROL_met_scale=met[met$shoot_type==0,]#df at met scale proleptic shoots
#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,16,18)]=c("length_cm",
                                            "length_nodes",
                                            "rank_node",
                                            "distance",
                                            "tot_buds",
                                            "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#REMOVING THE COUNTING OF CATKINS BECAUSE ALL PROLLEPTIC HAS CATKINS

#parameters: length(cm), length(node), rank_node, distance
#1:proportion of V ~length+length+rank+distance?####
glm_box1=glm(cbind(v,m)~length_cm+length_nodes+rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#2:proportion of V ~length+rank+distance?####
glm_box1=glm(cbind(v,m)~length_cm+rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#3:proportion of V ~rank+distance?####
glm_box1=glm(cbind(v,m)~rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#permut_rank
null_1=glm(cbind(v,m)~distance+1,family = "binomial",data = PROL_met_scale)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$rank_node=sample(PROL_met_scale$rank_node)
  perm=glm(cbind(v,m)~rank_node+distance,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#we have to chose between rank and distance
#rank
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$rank_node))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$rank_node)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$rank_node==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$rank_node==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

#plot
png("3_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs rank_node",
                xlab= "rank_node",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$distance==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$distance==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

#plot
png("3a_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs distance",
                xlab= "distance",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#4:proportion of V ~distance?####
glm_box1=glm(cbind(v,m)~distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$distance==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$distance==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

prop=prop[order(prop$rank),]

prop$pred=predict(glm_box1,
                newdata = data.frame(distance=seq(0, max(prop$rank), length.out = length(prop$rank))),
                type="response")


#plot
png("3b_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs distance",
                xlab= "distance",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
with(prop, lines(prop$pred~prop$rank,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#1:proportion of V ~length+length+rank+normal distance?####
glm_box1=glm(cbind(v,m)~length_cm+length_nodes+rank_node+normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#2:proportion of V ~length+rank+normal distance?####
glm_box1=glm(cbind(v,m)~length_nodes+rank_node+normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#3:proportion of V ~rank+normal distance?####
glm_box1=glm(cbind(v,m)~rank_node+normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#4:proportion of V ~distance?####
glm_box1=glm(cbind(v,m)~normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$normal_distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$normal_distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$normal_distance==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$normal_distance==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

prop=prop[order(prop$rank),]

prop$pred=predict(glm_box1,
                  newdata = data.frame(normal_distance=seq(0, max(prop$rank), length.out = length(prop$rank))),
                  type="response")


#plot
png("3c_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs normal_distance",
                xlab= "normal_distance",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
with(prop, lines(prop$pred~prop$rank,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

