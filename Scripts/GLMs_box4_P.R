#are you bursting?

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

wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

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
MV=PRO_bud_scale[PRO_bud_scale$fate=="M"|PRO_bud_scale$fate=="V",]#for fate= "M"
MV$sib=MV$v+MV$m+MV$b

V=MV[MV$fate=="V",]#for fate= "V"
M=MV[MV$fate=="M",]#for fate= "M"
#parameters: length(cm), length(node), rank_node, distance, m, v
#M and V####
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")
str(MV)
#1: new_shoots~fate+fate*parent_length_cm+fate*parent_length_node+fate*parent_rank_node+fate*median_distance+fate*m+fate*v####
glm_box1 = glm(new_shoots~sib+length_cm+fate*distance, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#2: new_shoots~fate+fate*parent_length_cm+fate*parent_rank_node+fate*median_distance+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate+fate*sib+fate*length_cm+fate*rank_node+fate*distance, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#permut_ rank
null_1=glm(new_shoots~fate+fate*length_cm+fate*distance+fate*v+1, family="binomial",data=MV)
dif=glm_box1$aic-null_1$aic
met_nul=MV

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$rank_node=sample(MV$rank_node)
  perm=glm(new_shoots~fate+fate*length_cm+fate*rank_node+fate*distance+fate*v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!
#3: new_shoots~fate+fate*parent_length_cm+fate*median_distance+fate*sib####
glm_box1 = glm(new_shoots~fate+fate*sib+fate*length_cm+fate*distance, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

coef=coef(glm_box1)
odd=exp(coef)
prob=odd/(1+odd)

#graph
png("4p_P.png",width=1200, height=900, res=150)# save plot
with(plot(allEffects(glm_box1)))
dev.off()

#manteniamo m+v ma togliamo le interazioni####
#1: new_shoots~Length+length(node)+rank_node+distance+m+v####
glm_box1 = glm(new_shoots~length_cm+length_node+rank_node+distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#2: new_shoots~Length+rank_node+distance+m+v####
glm_box1 = glm(new_shoots~length_cm+rank_node+distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#3: new_shoots~Length+rank_node+distance+v####
glm_box1 = glm(new_shoots~length_cm+rank_node+distance+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ rank
null_1=glm(new_shoots~length_cm+distance+v+1, family="binomial",data=V)
dif=glm_box1$aic-null_1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$rank_node=sample(V$rank_node)
  perm=glm(new_shoots~length_cm+rank_node+distance+v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#we have to choose between parameters 
#rank
prop=as.data.frame.matrix(table(V$rank_node,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4a_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot v) buds in proleptic vs rank_node",
                xlab= "rank_node",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#length
prop=as.data.frame.matrix(table(V$length_cm,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4b_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot v) buds in proleptic vs length_cm",
                xlab= "length_cm",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#distance
prop=as.data.frame.matrix(table(V$distance,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4c_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot v) buds in proleptic vs distance",
                xlab= "distance",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#v
prop=as.data.frame.matrix(table(V$v,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4d_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot v) buds in proleptic vs other V",
                xlab= "other V",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#normal_distance
prop=as.data.frame.matrix(table(V$normal_distance,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4e_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot v) buds in proleptic vs normal_distance",
                xlab= "normal_distance",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()
#4: new_shoots~v####
glm_box1 = glm(new_shoots~v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#v
prop=as.data.frame.matrix(table(V$v,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

prop=prop[order(prop$rank),]

prop$pred=predict(glm_box1,
                  newdata = data.frame(v=seq(0, max(prop$rank), length.out = length(prop$rank))),
                  type="response")

#plot
png("4f_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot v) buds in proleptic vs other V",
                xlab= "other V",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
with(prop, lines(prop$pred~prop$rank,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

###using sibling buds(m+v) as predictor
#1: new_shoots~Length+length(node)+rank_node+distance+m_v####
glm_box1 = glm(new_shoots~length_cm+length_node+rank_node+distance+sib, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#2: new_shoots~Length+length(node)+rank_node+m_v####
glm_box1 = glm_box1 = glm(new_shoots~length_cm+length_node+rank_node+sib, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no
#3: new_shoots~Length+rank_node+m_v####
glm_box1 = glm(new_shoots~length_cm+rank_node+sib, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#4: new_shoots~rank_node+m_v####
glm_box1 = glm(new_shoots~length_cm+rank_node+sib, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ rank
null_1=glm(new_shoots~m_v+1, family="binomial",data=V)
dif=glm_box1$aic-null_1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$rank_node=sample(V$rank_node)
  perm=glm(new_shoots~rank_node+m_v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

# df PROleptic~rank
prop=as.data.frame.matrix(table(M$rank_node,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4g_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot m) buds in proleptic vs rank_node",
                xlab= "rank_node",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

# df PROleptic~mv
prop=as.data.frame.matrix(table(M$m_v,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#plot
png("4h_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot m) buds in proleptic vs m+v",
                xlab= "m+v",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

# df PROleptic~rank+m_v
prop=as.data.frame.matrix(table(M$rank_node,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#create a sequence with random numbers between 1 and maximum length (72cm)
df=data.frame(rank_node=seq(1,max(M$rank_node),length.out = length(unique(M$rank_node))))
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate distance)
for (i in seq(1,10, by = 2)) {
  df=cbind(df, data.frame(rep(i, length(df$rank_node))))
}
colnames(df)[2:6]="m_v"

#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
}

colnames(df)[7:11]=seq(1,10, by = 2)

#plot
png("4i_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%new_shoots (#shoots/tot m) buds in proleptic vs rank_node",
                xlab= "rank_node",
                ylab="%new_shoots",
                ylim=c(0,1), type="h", lwd=5))
for (i in 1:length(grep("[0-9]", colnames(df)))) {
  t=grep("[0-9]", colnames(df))[i]
  with(df,lines(df[,t]~df$rank_node,col=rbPal[i], lwd=5))
}
legend("top",
       horiz=T,
       xpd = TRUE,c("real", "m+v=1","m+v=3","m+v=5","m+v=7","m+v=9"),
       pch = c(19,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1), lwd=5,
       col = c(cols[1], rbPal),
       cex=0.6)
dev.off()
