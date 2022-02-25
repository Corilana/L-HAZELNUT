setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
library(stats)
library(dplyr)

lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")

# Because sylleptic are distributed in a median zone along the parent shoot, the rank does not have a linear effect:
#we compute for each bud the distance to the shoot extremities, with reference 0 at the middle rank of each shoot
lat=dplyr::mutate(lat, distance=NA, .after=rank_node)#add new column for the distance
lat=dplyr::mutate(lat, mean_rank=NA, .after=rank_node)#add new column for mean

nshoot=length(unique(lat$shoot))
ran=grep("rank_",colnames(lat))
mran=grep("rank$",colnames(lat))
dis=grep("dis",colnames(lat))
nline=length(lat$tesi)

for (i in 1:nshoot) {#writing a column with the middle rank
  s=unique(lat$shoot)[i]
  m=median(lat[lat$shoot==s,ran])
  lat[lat$shoot==s,mran]=m
}

for (i in 1:nline) {#write a column with the absolute value of distance with rank mean
  lat[i,dis]=abs(lat[i,ran]-lat[i,mran])
}

PROL=lat[lat$is_in_sylleptic=="NO",]

#box4: do the bud (V and M) burst?
SYL=lat[lat$is_in_sylleptic=="YES",]
SYL$tot_buds=SYL$tot_buds-1#catkins are present in all sylleptics. thus we eliminate them from countin total_buds
#change columns names to not make confusion
colnames(SYL)[2]="parent_length"
colnames(SYL)[6]="parent_rank_node"

#for sylleptic####
V=SYL[SYL$fate=="V",]#for fate= "V"
M=SYL[SYL$fate=="M",]#for fate= "V"

#1: new_shoots~Length
glm_box1 = glm(new_shoots~parent_length, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#2: new_shoots~rank
glm_box1 = glm(new_shoots~parent_rank_node, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3: new_shoots~ distance
glm_box1 = glm(new_shoots~distance, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#4: new_shoots~tot buds(m+v)
glm_box1 = glm(new_shoots~tot_buds, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#permut_tot buds
null_1=glm(new_shoots~1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$tot_buds=sample(V$tot_buds)
  perm=glm(new_shoots~tot_buds, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~v
glm_box1 = glm(new_shoots~v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#permut_v
null_1=glm(new_shoots~1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$v=sample(V$v)
  perm=glm(new_shoots~v, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#6: new_shoots~m
glm_box1 = glm(new_shoots~v+m, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#NO

#for fate= "M"
#1: new_shoots~Length
glm_box1 = glm(new_shoots~parent_length, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#2: new_shoots~rank
glm_box1 = glm(new_shoots~parent_rank_node, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3: new_shoots~ distance
glm_box1 = glm(new_shoots~distance, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#4: new_shoots~tot buds
glm_box1 = glm(new_shoots~tot_buds, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#permut_tot buds
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$tot_buds=sample(M$tot_buds)
  perm=glm(new_shoots~tot_buds, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~v
glm_box1 = glm(new_shoots~v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#6: new_shoots~m
glm_box1 = glm(new_shoots~m, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#permut_m
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$m=sample(M$m)
  perm=glm(new_shoots~m, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#for not in sylleptic####
V=PROL[PROL$fate=="V",]#for fate= "V"
M=PROL[PROL$fate=="M",]#for fate= "V"

#1: new_shoots~Length
glm_box1 = glm(new_shoots~Length, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#permut_length
null_1=glm(new_shoots~1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$Length=sample(V$Length)
  perm=glm(new_shoots~Length, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2: new_shoots~rank+length
glm_box1 = glm(new_shoots~Length+rank_node, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3: new_shoots~distance+length
glm_box1 = glm(new_shoots~Length+distance, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#4: new_shoots~distance
glm_box1 = glm(new_shoots~distance, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#5: new_shoots~tot buds
glm_box1 = glm(new_shoots~Length+tot_buds, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#permut_tot_buds
null_1=glm(new_shoots~Length+1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$tot_buds=sample(V$tot_buds)
  perm=glm(new_shoots~Length+tot_buds, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#6: new_shoots~v
glm_box1 = glm(new_shoots~v, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#permut_v
null_1=glm(new_shoots~1,family="binomial",data=V)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=V

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$v=sample(V$v)
  perm=glm(new_shoots~v, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#7: new_shoots~m
glm_box1 = glm(new_shoots~v+m, family="binomial",data=V)#first check tot_buds to see if siblings are impo
summary(glm_box1)#NO

#for fate= "M"
#1: new_shoots~Length
glm_box1 = glm(new_shoots~Length, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#permut_length
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$Length=sample(M$Length)
  perm=glm(new_shoots~Length, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2: new_shoots~rank
glm_box1 = glm(new_shoots~Length+rank_node, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#3: new_shoots~distance+length
glm_box1 = glm(new_shoots~Length+distance, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#no

#4: new_shoots~distance
glm_box1 = glm(new_shoots~distance, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#permut_distance
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$distance=sample(M$distance)
  perm=glm(new_shoots~distance, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~tot buds
glm_box1 = glm(new_shoots~Length+tot_buds, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#YES

#permut_tot buds
null_1=glm(new_shoots~Length+1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$tot_buds=sample(M$tot_buds)
  perm=glm(new_shoots~Length+tot_buds, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#4: new_shoots~v
glm_box1 = glm(new_shoots~v, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#permut_tot buds
null_1=glm(new_shoots~1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$v=sample(M$v)
  perm=glm(new_shoots~v, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#5: new_shoots~m
glm_box1 = glm(new_shoots~v+m, family="binomial",data=M)#first check tot_buds to see if siblings are impo
summary(glm_box1)#yes

#permut_m
null_1=glm(new_shoots~v+1,family="binomial",data=M)
summary(null_1)
dif=null_1$aic-glm_box1$aic
met_nul=M

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:100) {
  met_nul$m=sample(M$m)
  perm=glm(new_shoots~v+m, family="binomial",data=met_nul)
  a=null_1$aic-perm$aic
  b=a>dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!
