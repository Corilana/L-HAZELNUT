#create shoot_level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
library(dplyr)

#count number of nodes,c,m,v,b,clusters,nuts, per each shoot
ann=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/2020DFAUTO.csv")

nline=length(ann$shoot)#how many lines
colnode=grep("node$",colnames(ann))#column node
colc=grep("c$",colnames(ann))#column catkins
colv=grep("v$",colnames(ann))#column vegetative
colm=grep("m$",colnames(ann))#column mixed
colcl=grep("cl$",colnames(ann))#column cluster
colnu=grep("nu$",colnames(ann))#column nuts
colb=grep("b$",colnames(ann))#column blind

ann=dplyr::mutate(ann, "tot_buds_m+v+b+c"=NA, .after = b)#new column

for (i in 1:nline) {#making the sum of nodes, #catkins,mixed,veg,blind,nuts,cluster per shoot
  ann[i,colnode]=sum(ann[i,grep("node.[0-9]*$", colnames(ann))]>0, na.rm=TRUE)#node
  ann[i,colc]=sum(ann[i,grep("c[^a-z][0-9]*$", colnames(ann))]>0, na.rm=TRUE)#catkins
  ann[i,colv]=rowSums(ann[i,grep("v[^a-z][0-9]*$", colnames(ann))], na.rm = T)#v
  ann[i,colm]=rowSums(ann[i,grep("m[^a-z][0-9]*$", colnames(ann))], na.rm = T)#m
  ann[i,colcl]=rowSums(ann[i,grep("^cluster", colnames(ann))], na.rm = T)#cluster
  ann[i,colnu]=rowSums(ann[i,grep("^nut", colnames(ann))], na.rm = T)#nut
  ann[i,colb]=sum(ann[i,grep("b[^a-z][0-9]*$", colnames(ann))]>0, na.rm=TRUE)#blind
  ann[i,colb+1]=sum(ann[i,c(colc:colm,colb)])#sum of obs
  }

# adding the plant ID to the dataset
old=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/Original/2020Inne_autor.csv")
ann=dplyr::mutate(ann, Tree=NA, .after = shoot)#new column
coltree=grep("Tree$",colnames(ann))#taking columns named Tree
colold=grep("pianta$",colnames(old))#taking columns named pianta

for (i in 1:nline) {#adding the number of tree to the shoots df
  s=ann$shoot[i]
  t=old[old$tesi=="auto"&old$shoot==s,colold]
  ann[i,coltree]=t
}

write.csv(ann, "auto/2020shoot_level.csv", row.names = F)

