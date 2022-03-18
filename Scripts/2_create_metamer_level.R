#create metamer level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
library(dplyr)
library(plyr)

ann=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/2020shoot_level.csv")

met=data.frame(matrix(ncol = 0, nrow=0))#empty df to create metamer level
col_i=grep("tesi$", colnames(ann))#column thesis
col_f=grep("^node$", colnames(ann))#column tree
c=grep(paste0("^","c","$"), colnames(ann))#column catkins
b=grep(paste0("b","$"), colnames(ann))#column blind

#duplicate rows according to mumber of nodes and adding node information
nline=length(ann$shoot)
for (i in 1:nline) {
  n=ann$node[i]
  for (rank_node in 1:n) {
    s=ann[col_i:col_f][i,]
    st=paste0("c.",rank_node,"$")
    fin=paste0("b.",rank_node,"$")
    col_st=grep(st,colnames(ann))
    col_fin=grep(fin,colnames(ann))
    s=cbind(s,rank_node,ann[col_st:col_fin][i,])
    s_i=grep(st,colnames(s))
    s_f=grep(fin,colnames(s))
    colnames(s)[s_i:s_f]=colnames(ann)[c:b]
    met=rbind.fill(met, s)
  }
}

#change name of the columns of met dataframe
col_f=grep("^node$", colnames(met))#column tree
colnames(met)[col_f]="Length(node)"

#create distance with the mean rank (mean rank=0)
met=dplyr::mutate(met, median_distance=NA, .after=rank_node)#add new column for the distance
nshoot=length(unique(met$shoot))
ran=grep("rank_",colnames(met))
len=grep(")$",colnames(met))

lat=met[0,0]#df to store the median
for (i in 1:nshoot) {#writing a column with the median rank
  s=unique(met$shoot)[i]
  l=unique(met[met$shoot==s,len])
  m=median(met[met$shoot==s,ran])
  a=as.data.frame(rep(m, each = l))
  lat=rbind(lat,a)
}

dis=grep("dis",colnames(met))

nline=length(met$tesi)
for (i in 1:nline) {#write a column with the absolute value of distance with rank mean
  met[i,dis]=abs(met[i,ran]-lat[i,1])
}

met=dplyr::mutate(met, normal_distance=NA, .after=median_distance)#add new column for the distance
met$normal_distance=round(met$median_distance/met$`Length(node)`, digits = 2)

v=grep(paste0("v","$"), colnames(met))#column vegetative
m=grep(paste0("m","$"), colnames(met))#column mixed
b=grep(paste0("b","$"), colnames(met))#column blind
c=grep(paste0("^c","$"), colnames(met))#column blind

met[met$c>1,c]=1#making catkins equal to 1 because we count that as present or absent
met$tot_buds=rowSums(met[c(c:m,b)])

write.csv(met, "auto/2020metamer_level.csv",row.names = F)
