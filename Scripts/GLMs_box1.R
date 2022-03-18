#GLM: does that rank bear a sylleptic?
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)

#metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
s=grep("shoot_type",colnames(met))#store the column with the sylleptic info
met[met$shoot_type=="SYLLEPTIC",s]=1#1 is sylleptic
met[met$shoot_type=="PROLEPTIC",s]=0#0 is proleptic
met$shoot_type=as.numeric(met$shoot_type)#transform in numeric the info of sylleptic/proleptic
#change columns names to not make confusion
colnames(met)[c(21,2,6,7)]=c("is_sylleptic",
                             "parent_length_cm",
                             "parent_length_node",
                             "parent_rank_node")

#1: sylleptic~length(cm)+rank+distance####
glm_box1=glm(is_sylleptic~parent_length_cm+parent_rank_node+median_distance,family = "binomial",data = met)
summary(glm_box1)#yes

#permutat_rank (because is the less sig)
null_1=glm(is_sylleptic~parent_length_cm+median_distance+1,family = "binomial",data = met)
dif=glm_box1$aic-null_1$aic
met_nul=met

#permute rank
df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_rank_node=sample(met$parent_rank_node)
  perm=glm(is_sylleptic~parent_length_cm+parent_rank_node+median_distance,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#because the probability that rank node explain by chance the model is ~5% (>1% of significance) we remove rank node from the equation

#2: sylleptic~length(cm)+distance####
glm_box1=glm(is_sylleptic~parent_length_cm+median_distance,family = "binomial",data = met)
summary(glm_box1)#yes

#permutations
null_1=glm(is_sylleptic~median_distance+1,family = "binomial",data = met)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
#significance=1% (max 100 better permut)
for (i in 1:10000) {
  met_nul$parent_length_cm=sample(met$parent_length_cm)
  perm=glm(is_sylleptic~parent_length_cm+median_distance,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3: sylleptic~distance####
glm_box1=glm(is_sylleptic~median_distance,family = "binomial",data = met)
summary(glm_box1)#yes

#permutations
null_1=glm(is_sylleptic~1,family = "binomial",data = met)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
#significance=1% (max 100 better permut)
for (i in 1:10000) {
  met_nul$median_distance=sample(met$median_distance)
  perm=glm(is_sylleptic~median_distance,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

# df sylleptic~distance
prop=as.data.frame.matrix(table(met$median_distance,met$is_sylleptic))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_syl","si_syl")
prop$dist=as.numeric(rownames(prop))
head(prop)

#df: predict~distance
prop$pred=predict(glm_box1,
                  newdata = data.frame(median_distance=seq(0, max(prop$dist), length.out = length(prop$dist))),
                  type="response")

#histogram
png("1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(prop, plot(prop$ratio~prop$dist,
          col = cols[1],
          main="%sylleptic (#sylleptic/tot_nodes) vs distance from median node(node)",
          xlab= "distance from median node(node)",
          ylab="%sylleptic",
          ylim=c(0,1),
          type="h",
          lwd=4))
with(prop, lines(prop$pred~prop$dist,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()

#1: sylleptic~length(cm)+rank+distance__same analysis but with distance normalized (distance/tot nodes)####
glm_box1=glm(is_sylleptic~parent_length_cm+parent_rank_node+normal_distance,family = "binomial",data = met)
summary(glm_box1)#yes

#2: sylleptic~length(cm)+distance####
glm_box1=glm(is_sylleptic~parent_length_cm+normal_distance,family = "binomial",data = met)
summary(glm_box1)#yes

#remove length because less sig

#3: sylleptic~distance####
glm_box1=glm(is_sylleptic~normal_distance,family = "binomial",data = met)
summary(glm_box1)#yes

# df sylleptic~distance
prop=as.data.frame.matrix(table(met$normal_distance,met$is_sylleptic))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_syl","si_syl")
prop$dist=as.numeric(rownames(prop))
head(prop)

#df: predict~distance
prop$pred=predict(glm_box1,
                  newdata = data.frame(normal_distance=seq(0, max(prop$dist), length.out = length(prop$dist))),
                  type="response")

#histogram
png("1a.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(prop, plot(prop$ratio~prop$dist,
                col = cols[1],
                main="%sylleptic (#sylleptic/tot_nodes) vs distance from median node normalized",
                xlab= "distance from median node normalized",
                ylab="%sylleptic",
                ylim=c(0,1),
                type="h",
                lwd=4))
with(prop, lines(prop$pred~prop$dist,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)

dev.off()

