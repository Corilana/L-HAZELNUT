#glm
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Apical"))
#COMPOSIZIONE DATASET
#importiamo la liberia
library(dplyr)
library(rstatix)
library(RColorBrewer)
library(nnet)
library(effects)
library(plotrix)
#APICAL SHOOT: bud SCALE
#bud level
ap=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_APICALS.csv"))
colnames(ap)[24]="nodes"
#consider just apicals M and V because if C it means that is a sylleptic
ap=ap[ap$fate!="C",]
ap[ap$fate=="V","fate"]=1
ap[ap$fate=="M","fate"]=0
ap$fate=as.factor(ap$fate)
ap$fate=relevel(ap$fate, "1")
levels(ap$fate)#domina1
#what is the proportion of buds?
q1=table(ap$fate)
q1.p=round(prop.table(q1)*100, digit=2)
#grafico3
png("freqMV.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=3,name="Set2")
x=barplot(q1.p,col = cols[as.factor(rownames(q1.p))], names.arg = rownames(q1.p), main="frequence apical buds type in annual shoot", xlab = "buds type", ylab="%", ylim = c(0,100))
text(x,q1.p+3 , paste0(q1.p,"%"), cex = 1)
dev.off()
#sono diversi? no
prop.test(q1)
#trattiamo fate come se fosse una binomial distribution 1=M 0=V
#1
glm_box1=glm(fate~Length+Length.node.+median_distance, data = ap, family = "binomial")
summary(glm_box1)
#2
glm_box1=glm(fate~Length+Length.node., data = ap, family = "binomial")
summary(glm_box1)
#3
glm_box1=glm(fate~Length.node., data = ap, family = "binomial")
summary(glm_box1)
#anche se sembra sign. in realtà non lo è
#4
glm_box1=glm(fate~1, data = ap, family = "binomial")
summary(glm_box1)

prM=exp(coefficients(glm_box1))/(1+exp(coefficients(glm_box1)))
prV=1-prM

proba=cbind(prV,prM)
rbinom(1,1,proba)
a=NA
b=NA
for (i in 1:10000) {
  a=rbinom(1,1,proba)
  b=rbind(b,a)
}
length(b[b==1,])#proba che sia v

#what is the length of new shoots?####
plot(density(ap$nodes))
#1:length~lengthnode
glm_box1 = glm(nodes~Length.node.,data = ap, family = "gaussian")
summary(glm_box1)
#create table with mean and se of number of nodes per distance
dis=sort(unique(ap$Length.node.))
df=ap[0,0]
for (i in dis) {
  av=mean(ap[ap$Length.node.==i,"nodes"])
  se=std.error(ap[ap$Length.node.==i,"nodes"])
  av_se=cbind(i,av,se)
  df=rbind(df,av_se)
}
colnames(df)[1]="Length.node."
plot(df$av~df$Length.node.)

df$pred=predict(glm_box1,
                newdata = data.frame(Length.node.=df$Length.node.), "response")
#grapg
png("new.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
r=plot(df$Length.node.,df$av,
       col = cols[1],pch=19,
       xlab="nb nodes",
       main="average #node children vs nb_nodes parent shoot",
       ylab="average #node children",
       ylim=c(0,13))
with(arrows(x0 = df$Length.node.,# Add error bars
            y0 = df$av + df$se,
            y1 = df$av - df$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
with(lines(df$Length.node.,df$pred, col=cols[2], lwd=3))
dev.off()
