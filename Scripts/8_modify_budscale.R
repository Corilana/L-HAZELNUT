#splitting bud fate into two dataframes: laterals and apicals
#Modify the metamer level
library(dplyr)
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
bud=read.csv("auto/bud_level_FINAL.csv")
lat=bud[bud$position=="LATERAL",]
ap=bud[bud$position=="AP",]

write.csv(lat, "auto/mtp use/bud_level_LATERALS.csv", row.names = F)
write.csv(ap, "auto/mtp use/bud_level_APICALS.csv", row.names = F)
