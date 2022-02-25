#Modify the metamer level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")
library(dplyr)

#adding the number of new shoots per each metamer
met=read.csv("2020metamer_level.csv")
new=read.csv("DFAUTO_new.shootlevel.csv")
new=unique(new[1:12])
nline=length(new$shoot1yo)#number lines
met$number_new_shoots=NA#new column

for (i in 1:nline) {#storing number of new shoots for each rank
  n=new$X.newshoot2yo[i]
  s=new$shoot1yo[i]
  r=new$rank1yo[i]
  met[met$shoot==s&met$rank_node==r,ncol(met)]=n

}

write.csv(met, "2020metamer_level.csv", row.names = F)