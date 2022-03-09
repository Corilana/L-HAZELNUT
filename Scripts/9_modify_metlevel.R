#Modify the metamer level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")
library(dplyr)
#adding number of laterals and apicals buds per each metamer

met=read.csv("2020metamer_level_DEVELOPED.csv")
bud=read.csv("bud_level_FINAL.csv")

met=dplyr::mutate(met, n_lateral_buds=NA)
met=dplyr::mutate(met, n_apical_buds=NA)
met=dplyr::mutate(met, fate_apicals=NA)
met=dplyr::mutate(met, shoot_type=NA)#the buds type of shoot that bears the buds at that rank (sylleptic/proleptic)

nline=length(unique(met$shoot))
nrank=max(met$rank_node)
po=grep("position",colnames(bud))
new=grep("new_",colnames(bud))
new_s=grep("new_",colnames(met))
fa=grep("fate",colnames(bud))
nlat=grep("n_lat",colnames(met))
nap=grep("n_ap",colnames(met))
wh=grep("fate_a",colnames(met))
insy=grep("from_",colnames(bud))
org=grep("shoot_type",colnames(met))

for (i in 1:nline) {
  s=unique(met$shoot)[i]
  for(j in 1:nrank) {
    l=length(bud[bud$shoot==s&bud$rank_node==j&bud$position=="LATERAL",po])#count #laterals
    a=length(bud[bud$shoot==s&bud$rank_node==j&bud$position=="AP",po])#count # apicals
    n=sum(bud[bud$shoot==s&bud$rank_node==j,new])#count # new shoots
    w=bud[bud$shoot==s&bud$rank_node==j&bud$position=="AP",fa]#say apical fate
    if (length(w)!=0) {
      met[met$shoot==s&met$rank_node==j,wh]=w
    }
    met[met$shoot==s&met$rank_node==j,nlat]=l
    met[met$shoot==s&met$rank_node==j,nap]=a
    met[met$shoot==s&met$rank_node==j,new_s]=n
  }
}

#modify the metamer df to remove the fate of apical buds from the count of laterals buds
fate=c("M","V","B","C")
ca=grep("^c$", colnames(met))
nline=length(met$tesi)
for (i in 1:nline) {
  f=met$fate_apicals[i]
  if (!is.na(f)) {
    col=which(colnames(met)== casefold(f))
    met[i,col]=met[i,col]-1
    met[i,new_s]=met[i,new_s]-1
  }
}

met=met[met$n_lateral_buds!=0,]#remove apicals

nline=length(met$tesi)
for (i in 1:nline) {
  c=met$c[i]
  if(c!=0){met$shoot_type[i]="SYLLEPTIC"}else{met$shoot_type[i]="PROLEPTIC"}
}

write.csv(met, "mtp use/met_level_develop_lateralbuds.csv", row.names = F)
