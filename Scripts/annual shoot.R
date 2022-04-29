#EXPLORATORY ANALISYS_annual shoot
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/AnnualShoot/"))

library(RColorBrewer)
library(dplyr)
#shoot level df
auto=read.csv(paste0(wd, "df/auto/mtp use/2020shoot_level_DEVELOPED_fin.csv"))
auto$class=factor(auto$class, levels = c("Sh","Me", "Lo","VLo"))

#summary table of dataset
df=as.data.frame(table(auto$class))#nb_shoots per class

for (i in 1:4) {
  L=levels(auto$class)[i]
  df[df$Var1==L,"c"]=sum(auto[auto$class==L, "c"])
  df[df$Var1==L,"v"]=sum(auto[auto$class==L, "v"])
  df[df$Var1==L,"m"]=sum(auto[auto$class==L, "m"])
  df[df$Var1==L,"b"]=sum(auto[auto$class==L, "b"])
  df[df$Var1==L,"tot_buds"]=sum(auto[auto$class==L, "tot_buds_m.v.b.c"])
  df[df$Var1==L,"nuts"]=sum(auto[auto$class==L, "nu"])
}

df["sums",-1]=colSums(df[,-1])

pdf("dataset.annual.pdf", height = 5,width = 12)
grid.table(df)
dev.off()

#1_ what is the length(nodes) distribution?
#how many shoots with that length?
max=max(auto$node)#max nodes
df=as.data.frame(seq(1:max))
for (i in 1:max) {df[i,"nb_shoots"]=length(auto[auto$node==i,"node"])}
df=as.matrix(df)

#distribution length (number of nodes)
png("frequencelength.png",width=1200, height=900, res=150)# save plot
cols=colors()[577]
barplot(df[,2],names.arg = df[,1],
        col = cols,main="distribution of annual shoot",
        xlab = "length(number of nodes)", ylab="number of shoots", ylim = c(0,13))
dev.off()

#from this graph we can think about making just 2 class length (from 0 to 11 nodes and >11 nodes)

#2_ what is the relationship between length in nodes and in cm?
#relationship between parent length and number of nodes
png("node~length.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,3))
col=brewer.pal(4, name="Set2")
plot(auto$node~auto$Length, pch=16,cex=1.2,main="relationship #node/length", xlab="parent length(cm)", ylab="parent #nodes", col=col[factor(auto$class, levels = c("Sh","Me", "Lo", "VLo"))])
legend("topleft", legend=c("Sh", "Me", "Lo", "VLo"), lwd=5, cex= 1, col=col)
dev.off()
