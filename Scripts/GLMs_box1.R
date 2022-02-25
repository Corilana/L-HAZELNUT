#BOX1: DOES THAT RANK BEAR A SYLLEPTIC?
setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)

met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met[met$shoot_type=="SYLLEPTIC",18]=1
met[met$shoot_type=="PROLEPTIC",18]=0
met$shoot_type=as.numeric(met$shoot_type)

#change columns names to not make confusion
colnames(met)[18]="is_sylleptic"
colnames(met)[2]="parent_length"
colnames(met)[6]="parent_rank_node"
#sylleptic is explained by length?
glm_box1=glm(is_sylleptic~parent_length,family = "binomial",data = met)
summary(glm_box1)#yes
#permut_length
null_1=glm(is_sylleptic~1,family = "binomial",data = met)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length=sample(met$parent_length)
  perm=glm(is_sylleptic~parent_length,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!
  
#sylleptic is explained by length & rank?
glm_box1=glm(is_sylleptic~parent_length+parent_rank_node,family = "binomial",data = met)
summary(glm_box1)#yes
#permut_length
null_1=glm(is_sylleptic~parent_length+1,family = "binomial",data = met)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=met

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(met$parent_rank_node)
  perm=glm(is_sylleptic~parent_length+parent_rank_node,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#solution: 
# Because sylleptic are distributed in a median zone along the parent shoot, the rank does not have a linear effect:
#we compute for each bud the distance to the shoot extremities, with reference 0 at the middle rank of each shoot
met=dplyr::mutate(met, distance=NA, .after=parent_rank_node)#add new column for the distance
met=dplyr::mutate(met, mean_rank=NA, .after=parent_rank_node)#add new column for mean

nshoot=length(unique(met$shoot))
ran=grep("_rank_",colnames(met))
mran=grep("rank$",colnames(met))
dis=grep("dis",colnames(met))
nline=length(met$tesi)

for (i in 1:nshoot) {#writing a column with the middle rank
  s=unique(met$shoot)[i]
  m=median(met[met$shoot==s,ran])
  met[met$shoot==s,mran]=m
}
for (i in 1:nline) {#write a column with the absolute value of distance with rank mean
  met[i,dis]=abs(met[i,ran]-met[i,mran])
  }

#run again the model
glm_box1=glm(is_sylleptic~parent_length+distance,family = "binomial",data = met)
summary(glm_box1)#no

#permut_DISTANCE
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