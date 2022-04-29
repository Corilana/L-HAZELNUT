#splitting bud fate into two dataframes: laterals and apicals
#remove repetition of nuts and clustes
library(dplyr)
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/")
bud=read.csv("auto/bud_level_FINAL.csv")

#in pratica cambio il numero di nocciole che sono ripetute peri  vari ranghi
#il criterio è:
#se il rango è uguale guardo se il fate è diverso da M.

for (i in 1:(nrow(bud)+1)) {
  sh=bud[i,"shoot"]
  ra=bud[i,"rank_node"]
  cl=bud[i,"cl"]
  if (!is.na(cl)) {
    if(nrow(bud[bud$shoot==sh&bud$rank_node==ra,])>1){
      if (cl!=0) {
        fa=bud[bud$shoot==sh&bud$rank_node==ra,"fate"]
        for (j in 1:cl) {
          m=grep("M",fa)[j]
          if (!is.na(m)) {
            bud[bud$shoot==sh&bud$rank_node==ra,c("cl","nu")][-m,]=0
          }
        }
      }
    }
  }
}

lat=bud[bud$position=="LATERAL",]
ap=bud[bud$position=="AP",]

write.csv(bud, "auto/bud_level_FINAL.csv", row.names = F)
write.csv(lat, "auto/mtp use/bud_level_LATERALS.csv", row.names = F)
write.csv(ap, "auto/mtp use/bud_level_APICALS.csv", row.names = F)
