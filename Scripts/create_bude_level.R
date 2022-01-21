#script 20/01/2022
#Modify the "met_level_develop.csv" to duplicate lines as many buds for that rank!!!
#the new database will be names "bud_level_developed"

setwd("C:/Users/franc/Google Drive/PhD/Deruta/auto/")

met.level<-read.csv("C:/Users/franc/Google Drive/PhD/Deruta/2020/DFAUTO_metamerlevel.csv")
met.level<-met.level[!is.na(met.level$X.newshoots),]#delete the shoots that where not found in 2021
write.csv(met.level, "met_level_develop.csv")

bud.level<-met.level[FALSE,]#create df in which each row is a bud
nline=nrow(met.level[1])

for (i in 1:nline) {
    x=met.level$X.oss[i]
    bud.level=rbind(bud.level, met.level[rep(i, each = x), ])
}

write.csv(bud.level, "bud_level_develop.csv")
