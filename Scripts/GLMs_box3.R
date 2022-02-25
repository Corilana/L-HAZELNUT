setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)

# lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
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

#box3:proportion of V####
#for sylleptic
SYL=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL)[2]="parent_length"
colnames(SYL)[6]="parent_rank_node"
SYL$tot_buds=SYL$n_lateral_buds-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS
colnames(SYL)[15]="m_v"

#proportion of V related to parent length??
glm_box1=glm(cbind(v,m)~parent_length,family = "binomial",data = SYL)
summary(glm_box1)#no

#proportion of V related to parent rank??
glm_box1=glm(cbind(v,m)~parent_rank_node,family = "binomial",data = SYL)
summary(glm_box1)#yes

#permut_rank
null_1=glm(cbind(v,m)~1,family = "binomial",data = SYL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=SYL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$parent_rank_node=sample(SYL$parent_rank_node)
  perm=glm(cbind(v,m)~parent_rank_node,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#proportion of V related to DISTANCE??
glm_box1=glm(cbind(v,m)~distance,family = "binomial",data = SYL)
summary(glm_box1)#no

#box3:proportion of V####
#for PROLEPTIC
PROL=met[met$shoot_type==0,]#df at bud scale of buds in PROLEPTIC shoots

#change columns names to not make confusion
PROL$tot_buds=PROL$n_lateral_buds-PROL$b#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS
colnames(PROL)[15]="m_v"

PROL=PROL[PROL$m_v!=0,]

#proportion of V related to parent length??
glm_box1=glm(cbind(v,m)~Length,family = "binomial",data = PROL)
summary(glm_box1)#no

#proportion of V related to parent rank??
glm_box1=glm(cbind(v,m)~rank_node,family = "binomial",data = PROL)
summary(glm_box1)#yes

#permut_rank
null_1=glm(cbind(v,m)~1,family = "binomial",data = PROL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$rank_node=sample(PROL$rank_node)
  perm=glm(cbind(v,m)~rank_node,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#proportion of V related to parent rank??
glm_box1=glm(cbind(v,m)~distance,family = "binomial",data = PROL)
summary(glm_box1)#yes

#permut_distance
null_1=glm(cbind(v,m)~1,family = "binomial",data = PROL)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=PROL

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$distance=sample(PROL$distance)
  perm=glm(cbind(v,m)~distance,family = "binomial",data = met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!
