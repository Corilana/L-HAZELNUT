#modify shoot level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
library(dplyr)

#add to the original the info if the shoot was found or not
ann=read.csv("auto/2020shoot_level.csv")
met=read.csv("auto/2020metamer_level.csv")

ann=dplyr::mutate(ann, found_next_yea=NA, .after=tot_buds_m.v.b.c)#new column to store the new info
ne=grep("new", colnames(met))#column "new shoots"
fo=grep("^foun", colnames(ann))#column "found"
nline=length(ann$tesi)#number of line

for (i in 1:nline) {#store, for each shoot, if it was found next year (tag did not felt down)
  s=ann$shoot[i]
  n=max(met[met$shoot==s,ne])
  if(!is.na(n)) {n="YES"}
  else {n="NO"}
  ann[ann$shoot==s,fo]=n
}

ann=ann[ann$found_next_yea=="YES",]#keeping just the ones that we fount (= have a story)

write.csv(ann, "auto/2020shoot_level_DEVELOPED.csv", row.names = F)
