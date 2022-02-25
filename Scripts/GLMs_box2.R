setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)

met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met[met$shoot_type=="SYLLEPTIC",18]=1
met[met$shoot_type=="PROLEPTIC",18]=0
met$shoot_type=as.numeric(met$shoot_type)

# Because sylleptic are distributed in a median zone along the parent shoot, the rank does not have a linear effect:
#we compute for each bud the distance to the shoot extremities, with reference 0 at the middle rank of each shoot
met=dplyr::mutate(met, distance=NA, .after=rank_node)#add new column for the distance
met=dplyr::mutate(met, mean_rank=NA, .after=rank_node)#add new column for mean

nshoot=length(unique(met$shoot))
ran=grep("rank_",colnames(met))
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

#box2:how many buds? ####
#for sylleptic
SYL=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL)[2]="parent_length"
colnames(SYL)[6]="parent_rank_node"
SYL$n_lateral_buds=SYL$n_lateral_buds-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS
colnames(SYL)[15]="m_v"

#m+v buds are explained by parent_length?
glm_box1=glm(m_v~parent_length,family = "poisson",data = SYL)
summary(glm_box1)#yes

#permut_length
null_1=glm(m_v~1,family = "poisson",data = SYL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_length=sample(SYL$parent_length)
  perm=glm(m_v~parent_length,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#m+v buds are explained by parent_length?
glm_box1=glm(m_v~parent_length+parent_rank_node,family = "poisson",data = SYL)
summary(glm_box1)#yes

#permut_rank
null_1=glm(m_v~parent_length+1,family = "poisson",data = SYL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(SYL$parent_rank_node)
  perm=glm(m_v~parent_length+parent_rank_node,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#box2:how many buds?####
#for not sylleptic
PROL=met[met$shoot_type==0,]

#BOX.2.0# existence(0.1)
#explained by length?
glm_box1=glm(b~Length,family = "binomial",data = PROL)
summary(glm_box1)#yes

#permut_rank
null_1=glm(b~1,family = "binomial",data = PROL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$Length=sample(PROL$Length)
  perm=glm(b~Length,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#explained by length+rank?
glm_box1=glm(b~Length+rank_node,family = "binomial",data = PROL)
summary(glm_box1)#yes

#box2:HOW MANY BUDS
colnames(PROL)[15]="m_v"
PROL$m_v=PROL$m_v-PROL$b

#m+v buds are explained by Length?
glm_box1=glm(m_v~Length,family = "poisson",data = PROL)
summary(glm_box1)#yes

#permut_length
null_1=glm(m_v~1,family = "poisson",data = PROL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$Length=sample(PROL$Length)
  perm=glm(m_v~Length,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#m+v buds are explained by rank?
glm_box1=glm(m_v~Length+rank_node,family = "poisson",data = PROL)
summary(glm_box1)#yes

#permut_rank
null_1=glm(m_v~1,family = "poisson",data = PROL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$rank_node=sample(PROL$rank_node)
  perm=glm(m_v~rank_node,family = "poisson",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#m+v buds are explained by distance?
glm_box1=glm(m_v~Length+distance,family = "poisson",data = PROL)
summary(glm_box1)#no
