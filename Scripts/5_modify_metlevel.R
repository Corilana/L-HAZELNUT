#Modify the metamer level
setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")
library(dplyr)

#2_create df with shoots that developed
met.level<-read.csv("2020metamer_level.csv")
met.level<-met.level[!is.na(met.level$number_new_shoots),]#delete the shoots that where not found in 2021
write.csv(met.level, "2020metamer_level_DEVELOPED.csv", row.names = F)
