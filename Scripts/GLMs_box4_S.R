wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)

lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#box4: do the bud (V and M) burst?
SYL_bud_scale=lat[lat$from_=="SYL",]
colnames(SYL_bud_scale)[c(2,6,7,15)]=c("parent_length_cm","parent_length_node","parent_rank_node","m_v")
SYL_bud_scale$m_v=SYL_bud_scale$m_v-1#catkins are present in all sylleptics. thus we eliminate them from countin total_buds

V=SYL_bud_scale[SYL_bud_scale$fate=="V",]#for fate= "V"
M=SYL_bud_scale[SYL_bud_scale$fate=="M",]#for fate= "M"
#parameters: length(cm), length(node), rank_node, distance, m, v

#1: new_shoots~Length+length(node)+rank_node+distance+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+parent_length_node+parent_rank_node+median_distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#1.1: new_shoots~Length+length(node)+rank_node+distance+m_v
glm_box1 = glm(new_shoots~parent_length_cm+parent_length_node+parent_rank_node+median_distance+m_v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#2: new_shoots~Length+rank_node+distance+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+parent_rank_node+median_distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#2.1: new_shoots~Length+length(node)+distance+m_v
glm_box1 = glm(new_shoots~parent_length_cm+parent_length_node+median_distance+m_v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3: new_shoots~Length+distance+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+median_distance+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3.1: new_shoots~Length+distance+m_v
glm_box1 = glm(new_shoots~parent_length_cm+median_distance+m_v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#4: new_shoots~Length+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+m+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ M
null_1=glm(new_shoots~parent_length_cm+v+1, family="binomial",data=V)
dif=glm_box1$aic-null_1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$m=sample(V$m)
  perm=glm(new_shoots~parent_length_cm+m+v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4.1: new_shoots~Length+m_v
glm_box1 = glm(new_shoots~parent_length_cm+m_v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_length
null_1=glm(new_shoots~m_v+1, family="binomial",data=V)
dif=glm_box1$aic-null_1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_length_cm=sample(V$parent_length_cm)
  perm=glm(new_shoots~parent_length_cm+m_v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~Length+v####
glm_box1 = glm(new_shoots~parent_length_cm+v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ length
null_1=glm(new_shoots~v+1, family="binomial",data=V)
dif=glm_box1$aic-null_1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_length_cm=sample(V$parent_length_cm)
  perm=glm(new_shoots~parent_length_cm+v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5.1: new_shoots~Length+m_v
glm_box1 = glm(new_shoots~m_v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#6: new_shoots~v####
glm_box1 = glm(new_shoots~v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ V
null_1=glm(new_shoots~1, family="binomial",data=V)
dif=glm_box1$aic-null_1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$v=sample(V$v)
  perm=glm(new_shoots~v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

# df sylleptic~v
prop=as.data.frame.matrix(table(V$v,V$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$v=as.numeric(rownames(prop))
head(prop)

#df: predict~v
prop$pred=predict(glm_box1,
                  newdata = data.frame(v=seq(0, max(prop$v), length.out = length(prop$v))),
                  type="response")

#histogram
png("4_s.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(prop, plot(prop$ratio~prop$v,
                col = cols[1],
                main="%new_shoots (#new_shoots/tot_v_buds) vs # of V in the same sylleptic",
                xlab= "# of V in the same sylleptic",
                ylab="%new_shoots",
                ylim=c(0,1),
                type="h",
                lwd=8))
with(prop, lines(prop$pred~prop$v,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()


#1: new_shoots~Length+length(node)+rank_node+distance+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+parent_length_node+parent_rank_node+median_distance+m+v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#1.1: new_shoots~Length+length(node)+rank_node+distance+m_v
glm_box1 = glm(new_shoots~parent_length_cm+parent_length_node+parent_rank_node+median_distance+m_v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#2: new_shoots~Length+rank_node+distance+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+parent_rank_node+median_distance+m+v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#2.1: new_shoots~Length+rank_node+distance+m_v
glm_box1 = glm(new_shoots~parent_length_cm+parent_rank_node+median_distance+m_v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3: new_shoots~Length+distance+m+v####
glm_box1 = glm(new_shoots~parent_length_cm+median_distance+m+v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_V
null_1=glm(new_shoots~parent_length_cm+median_distance+m+1, family="binomial",data=M)
dif=glm_box1$aic-null_1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$v=sample(M$v)
  perm=glm(new_shoots~parent_length_cm+median_distance+m+v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3.1: new_shoots~Length+distance+m_v
glm_box1 = glm(new_shoots~parent_length_cm+median_distance+m_v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_length
null_1=glm(new_shoots~median_distance+m_v+1, family="binomial",data=M)
dif=glm_box1$aic-null_1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_length_cm=sample(M$parent_length_cm)
  perm=glm(new_shoots~parent_length_cm+median_distance+m_v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4: new_shoots~Length+distance+m####
glm_box1 = glm(new_shoots~parent_length_cm+median_distance+m, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_LENGTH
null_1=glm(new_shoots~median_distance+m+1, family="binomial",data=M)
dif=glm_box1$aic-null_1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_length_cm=sample(M$parent_length_cm)
  perm=glm(new_shoots~parent_length_cm+median_distance+m, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4.1: new_shoots~distance+m_v
glm_box1 = glm(new_shoots~median_distance+m_v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_distance
null_1=glm(new_shoots~m_v+1, family="binomial",data=M)
dif=glm_box1$aic-null_1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$median_distance=sample(M$median_distance)
  perm=glm(new_shoots~median_distance+m_v, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~distance+m####
glm_box1 = glm(new_shoots~median_distance+m, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ distance
null_1=glm(new_shoots~m+1, family="binomial",data=M)
dif=glm_box1$aic-null_1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$median_distance=sample(M$median_distance)
  perm=glm(new_shoots~median_distance+m, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#6: new_shoots~m####
glm_box1 = glm(new_shoots~m, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#permut_ M
null_1=glm(new_shoots~1, family="binomial",data=M)
dif=glm_box1$aic-null_1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$m=sample(M$m)
  perm=glm(new_shoots~m, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

# df sylleptic~distance
prop=as.data.frame.matrix(table(M$m,M$new_shoots))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_shoots","si_shoots")
prop$m=as.numeric(rownames(prop))
head(prop)

#df: predict~distance
prop$pred=predict(glm_box1,
                  newdata = data.frame(m=seq(0, max(prop$m), length.out = length(prop$m))),
                  type="response")

#histogram
png("4b_s.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(prop, plot(prop$ratio~prop$m,
                col = cols[1],
                main="%new_shoots (#new_shoots/tot_m_buds) vs # of M in the same sylleptic",
                xlab= "# of M in the same sylleptic",
                ylab="%new_shoots",
                ylim=c(0,1),
                type="h",
                lwd=8))
with(prop, lines(prop$pred~prop$m,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()
