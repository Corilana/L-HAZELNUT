#modify shoot level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
library(dplyr)

#adding number of laterals and apicals buds per each shoot
ann=read.csv("auto/2020shoot_level_DEVELOPED.csv")
bud=read.csv("auto/bud_level_FINAL.csv")

ann=dplyr::mutate(ann, buds_in_sylleptic=NA, .after = found_next_yea)

nline=length(ann$shoot)
fa=grep("fate", colnames(bud))
sy=grep("from_", colnames(bud))
n_s=grep("n_s", colnames(ann))

for (i in 1:nline) {
  s=ann$shoot[i]#identify the shoot
  syl=length(bud[bud$shoot==s&bud$from_=="SYL",sy])#many sylleptic
  ann[ann$shoot==s,n_s]=syl
}

ann=ann[2:17]
write.csv(ann, "auto/mtp use/shoot_level_develop_lateralbuds.csv", row.names = F)
