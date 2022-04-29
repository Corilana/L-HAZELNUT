#create metamer level
#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/DF/"
setwd(wd)
#import library
library(dplyr)
library(plyr)

#import dataset
ann=read.csv(paste0(wd, "auto/2020shoot_level.csv"))
#empty df
met=ann[0,0]
#store column_namse
col_i=grep("tesi$", colnames(ann))#column thesis
col_f=grep("^node$", colnames(ann))#column tree
c=grep(paste0("^","c","$"), colnames(ann))#column catkins
b=grep(paste0("b","$"), colnames(ann))#column blind
#duplicate rows according to number of nodes and adding node information
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
#add new column for the distance
met=dplyr::mutate(met, median_distance=NA, .after=rank_node)
nshoot=length(unique(met$shoot))
#store df columns
ran=grep("rank_",colnames(met))
len=grep(")$",colnames(met))
#df to store the median
lat=met[0,0]
for (i in 1:nshoot) {#writing a column with the median rank
  s=unique(met$shoot)[i]
  l=unique(met[met$shoot==s,len])
  m=median(met[met$shoot==s,ran])
  a=as.data.frame(rep(m, each = l))
  lat=rbind(lat,a)
}
#store the column name
dis=grep("dis",colnames(met))
#for loop for the distance with the rank means
nline=length(met$tesi)
for (i in 1:nline) {#write a column with the absolute value of distance with rank mean
  met[i,dis]=met[i,ran]-lat[i,1]
}
#new column for distance with absolute value
met=dplyr::mutate(met, distance_abs=NA, .after=median_distance)#add new column for the distance
met$distance_abs=abs(met$median_distance)
#new column for normalized distance
met=dplyr::mutate(met, normal_distance=NA, .before=median_distance)#add new column for the distance
#normalized distance
met$normal_distance=round(met$distance_abs/met$`Length(node)`, digits = 2)

#store colu,ns
v=grep(paste0("v","$"), colnames(met))#column vegetative
m=grep(paste0("^m","$"), colnames(met))#column mixed
b=grep(paste0("b","$"), colnames(met))#column blind
c=grep(paste0("^c","$"), colnames(met))#column blind
#making catkins equal to 1 because we count that as present or absent
met[met$c>1,c]=1
met$tot_buds=rowSums(met[c(c:m,b)])

write.csv(met, "auto/2020metamer_level.csv",row.names = F)
