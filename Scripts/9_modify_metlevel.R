#Modify the metamer level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")
library(dplyr)
#adding number of laterals and apicals buds per each metamer
met=read.csv("2020metamer_level_DEVELOPED.csv")
bud=read.csv("bud_level_FINAL.csv")

met=dplyr::mutate(met, shoot_type=NA)

#number of new shoots from that rank
nline=length(unique(met$shoot))
nrank=max(met$rank_node)
new=grep("new_",colnames(bud))
new_s=grep("new_",colnames(met))
fa=grep("fate",colnames(bud))
insy=grep("from_",colnames(bud))
org=grep("shoot_type",colnames(met))

for (i in 1:nline) {
  s=unique(met$shoot)[i]
  for(j in 1:nrank) {
    n=sum(bud[bud$shoot==s&bud$rank_node==j,new])#count # new shoots
    met[met$shoot==s&met$rank_node==j,new_s]=n
  }
}

nline=length(met$tesi)
for (i in 1:nline) {
  c=met$c[i]
  if(c!=0){met$shoot_type[i]="SYLLEPTIC"}else{met$shoot_type[i]="PROLEPTIC"}
}

write.csv(met, "mtp use/met_level_develop_lateralbuds.csv", row.names = F)
