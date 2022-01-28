#Create bud_level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")

met=read.csv("2020metamer_level_DEVELOPED.csv")

bud<-met[FALSE,]#create df in which each row is a bud

nline=nrow(met[1])
for (i in 1:nline) {
    x=met$tot_buds[i]
    bud=rbind(bud, met[rep(i, each = x), ])
}

write.csv(bud, "bud_level_develop.csv", row.names = F)
