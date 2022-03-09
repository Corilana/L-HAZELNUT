setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)
library(RColorBrewer)

lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
#box4: do the bud (V and M) burst?
SYL_bud_scale=lat[lat$is_in_sylleptic=="YES",]
colnames(SYL_bud_scale)[c(2,6,7,15)]=c("parent_length_cm","parent_length_node","parent_rank_node","m_v")
SYL_bud_scale$m_v=SYL_bud_scale$m_v-1#catkins are present in all sylleptics. thus we eliminate them from countin total_buds

V=SYL_bud_scale[SYL_bud_scale$fate=="V",]#for fate= "V"

#1: new_shoots~Length####
glm_box1 = glm(new_shoots~parent_length_cm, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#df: %new shoots~length
prop=as.data.frame.matrix(table(V$parent_length_cm,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$length=as.numeric(rownames(prop))
head(prop)

#histogram 
png("box4_1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols,
               main="%new shoots from v (new shoots/totv) vs Parent length (cm)",
               xlab= "Parent length (cm)",
               ylab="%new shoots from v",
               ylim=c(0,1),
               names.arg = prop$length)
dev.off()

#2: new_shoots~rank####
glm_box1 = glm(new_shoots~parent_rank_node, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#df: new_shoots~rank
prop=as.data.frame.matrix(table(V$parent_rank_node,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#histogram 
png("box4_2.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols,
               main="%new shoots from v (new shoots/totv) vs Parent rank node",
               xlab= "Parent rank node",
               ylab="%new shoots from v",
               ylim=c(0,1),
               names.arg = prop$rank)
dev.off()

#3: new_shoots~ distance####
glm_box1 = glm(new_shoots~median_distance, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#df: %new_shoots~ distance
prop=as.data.frame.matrix(table(V$median_distance,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$distance=as.numeric(rownames(prop))
head(prop)

#histogram 
png("box4_3.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols,
               main="%new shoots from v (new shoots/totv) vs distance from the median",
               xlab= "distance from the median node",
               ylab="%new shoots from v",
               ylim=c(0,1),
               names.arg = prop$distance)
dev.off()

#4: new_shoots~tot buds(m+v)####
V$m_v=V$m_v-1#subctracting the subject bud from the count
colnames(V)[15]="sibling_buds_mv"

glm_box1 = glm(new_shoots~sibling_buds_mv, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#df: %new_shoots~tot buds(m+v)
prop=as.data.frame.matrix(table(V$sibling_buds_mv,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$siblings=as.numeric(rownames(prop))
head(prop)

#df: predict~length
pred=as.data.frame.matrix(cbind(V$sibling_buds_mv,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$siblings)){prop$predict=pred$V2}

#histogram
png("box4_4.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%new shoots from v (new shoots/totv) vs # sibling buds",
               xlab= "# sibling buds",
               ylab="%new shoots from v",
               ylim=c(0,1),
               names.arg = prop$siblings)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(new_shoots~1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$sibling_buds_mv=sample(V$sibling_buds_mv)
  perm=glm(new_shoots~sibling_buds_mv, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~v####
colnames(V)[10]="siblings_V"

glm_box1 = glm(new_shoots~siblings_V, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#df: new_shoots~v
prop=as.data.frame.matrix(table(V$siblings_V,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$siblings_V=as.numeric(rownames(prop))
head(prop)

#df: predict~length
pred=as.data.frame.matrix(cbind(V$siblings_V,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$siblings_V)){prop$predict=pred$V2}

#histogram
png("box4_5.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%new shoots from v (new shoots/totv) vs # sibling V",
               xlab= "# sibling V",
               ylab="%new shoots from v",
               ylim=c(0,1),
               names.arg = prop$siblings_V)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(new_shoots~1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$siblings_V=sample(V$siblings_V)
  perm=glm(new_shoots~siblings_V, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#6: new_shoots~m####
colnames(V)[11]="siblings_M"

glm_box1 = glm(new_shoots~siblings_V+siblings_M, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#NO

#df: %new_shoots~m
prop=as.data.frame.matrix(table(V$siblings_M,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$siblings_M=as.numeric(rownames(prop))
head(prop)

#histogram
png("box4_6.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%new shoots from v (new shoots/totv) vs # sibling M",
               xlab= "# sibling M",
               ylab="%new shoots from v",
               ylim=c(0,1),
               names.arg = prop$siblings_M)
dev.off()

#for fate= "M"####
M=SYL_bud_scale[SYL_bud_scale$fate=="M",]#for fate= "V"

#1: new_shoots~Length####
glm_box1 = glm(new_shoots~parent_length_cm, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#df: new_shoots~Length
prop=as.data.frame.matrix(table(M$parent_length_cm,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$length=as.numeric(rownames(prop))
head(prop)

#histogram 
png("box4_1m.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols,
               main="%new shoots from M (new shoots/totm) vs Parent length (cm)",
               xlab= "Parent length (cm)",
               ylab="%new shoots from M",
               ylim=c(0,1),
               names.arg = prop$length)
dev.off()

#2: new_shoots~rank####
glm_box1 = glm(new_shoots~parent_rank_node, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#df: new_shoots~rank
prop=as.data.frame.matrix(table(M$parent_rank_node,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$rank=as.numeric(rownames(prop))
head(prop)

#histogram 
png("box4_2m.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols,
               main="%new shoots from M (new shoots/totm) vs Parent rank node",
               xlab= "Parent rank node",
               ylab="%new shoots from M",
               ylim=c(0,1),
               names.arg = prop$rank)
dev.off()

#3: new_shoots~ distance####
glm_box1 = glm(new_shoots~median_distance, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#df: new_shoots~ distance
prop=as.data.frame.matrix(table(M$median_distance,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$distance=as.numeric(rownames(prop))
head(prop)

#df:predict~length
pred=as.data.frame.matrix(cbind(M$median_distance,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$distance)){prop$predict=pred$V2}

#histogram 
png("box4_3m.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%new shoots from M (new shoots/totm) vs distance from the median",
               xlab= "distance from the median node",
               ylab="%new shoots from M",
               ylim=c(0,1),
               names.arg = prop$distance)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$median_distance=sample(M$median_distance)
  perm=glm(new_shoots~median_distance, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4: new_shoots~tot buds(m+M)####
M$m_v=M$m_v-1#subctracting the subject bud from the count
colnames(M)[15]="sibling_buds_mv"

glm_box1 = glm(new_shoots~sibling_buds_mv, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#df: new_shoots~tot buds(m+M)
prop=as.data.frame.matrix(table(M$sibling_buds_mv,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$siblings=as.numeric(rownames(prop))
head(prop)

#df: predict~length
pred=as.data.frame.matrix(cbind(M$sibling_buds_mv,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$siblings)){prop$predict=pred$V2}

#histogram 
png("box4_4m.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%new shoots from M (new shoots/totm) vs # sibling buds",
               xlab= "# sibling buds",
               ylab="%new shoots from M",
               ylim=c(0,1),
               names.arg = prop$siblings)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#permutations
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$sibling_buds_mv=sample(M$sibling_buds_mv)
  perm=glm(new_shoots~sibling_buds_mv, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~M####
colnames(M)[10]="siblings_V"

glm_box1 = glm(new_shoots~siblings_V, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#df: new_shoots~M
prop=as.data.frame.matrix(table(M$siblings_V,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$siblings_V=as.numeric(rownames(prop))
head(prop)

#histogram 
png("box4_5m.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[1]
dfbar<-barplot(prop$ratio,
               col = cols,
               main="%new shoots from M (new shoots/totm) vs # sibling M",
               xlab= "# sibling M",
               ylab="%new shoots from M",
               ylim=c(0,1),
               names.arg = prop$siblings_V)
dev.off()

#6: new_shoots~m####
colnames(M)[11]="siblings_M"

glm_box1 = glm(new_shoots~siblings_M, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#df: new_shoots~m
prop=as.data.frame.matrix(table(M$siblings_M,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_new_shoots","si_new_shoots")
prop$siblings_M=as.numeric(rownames(prop))
head(prop)

#df: predict~length
pred=as.data.frame.matrix(cbind(M$siblings_M,predict(glm_box1, type="response")))
pred=unique(pred)
pred=pred[with(pred, order(V1)), ]
if (all.equal(pred$V1,prop$siblings_M)){prop$predict=pred$V2}

#histogram 
png("box4_6m.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
dfbar<-barplot(prop$ratio,
               col = cols[1],
               main="%new shoots from M (new shoots/totm) vs # sibling M",
               xlab= "# sibling M",
               ylab="%new shoots from M",
               ylim=c(0,1),
               names.arg = prop$siblings_M)
lines(x=dfbar,y=prop$predict,lwd=5, col=cols[2])
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()

#permutations
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$siblings_M=sample(M$siblings_M)
  perm=glm(new_shoots~siblings_M, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!
