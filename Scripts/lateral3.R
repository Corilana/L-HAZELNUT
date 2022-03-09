#EXPLORATORY ANALYSIS
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/exploratory/"))

#importiamo la liberia
library(dplyr)
library(tibble)
library(RColorBrewer)
library(corrplot)
library(rstatix)
library(gridExtra)

#bud level
lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
lat <- dplyr::mutate(lat, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
lat <- dplyr::mutate(lat, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))
#met.scale
met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#in proleptic df, we have to put the observations inside the sylleptic as 0
#and indicate the presence/absence of sylleptic shoots, at that node, with 0(abs) or 1(pres)
proleptic=met#copy df metamer level

sh=grep("^shoot$", colnames(proleptic))
sy=grep("^syl", colnames(proleptic))
v=grep("^v$", colnames(proleptic))
m=grep("^m$", colnames(proleptic))
b=grep("^b$", colnames(proleptic))

for (q in 1:nrow(proleptic)) {
  if (proleptic[q,c]==1) {
    proleptic[q,c(m,v,b)]=0#trasform to zero the values of the buds in sylleptic
  }
  
}
colnames(proleptic)[c]="sylleptic"#CHANGE THE name of COLUMN "C" WITH "SYLLEPTIC"

#1_ % lateral buds/syllepotic in proleptic shoots####
#) (-->df:proleptic)
#~rank node
TAB_PRO=met[0,0]
nline=length(unique(sort(proleptic$rank_node)))
for (q in 1:nline) {
  Q=unique(sort(proleptic$rank_node))[q]
  TAB_PRO[paste0(Q),"rank_node"]=Q
  TAB_PRO[paste0(Q),"shoots_with_tank"]=length(proleptic[proleptic$rank_node==Q,sh])
  TAB_PRO[paste0(Q),"sylleptic"]=sum(proleptic[proleptic$rank_node==Q,sy])#sylleptic for each parental node?
  TAB_PRO[paste0(Q),"v"]=sum(proleptic[proleptic$rank_node==Q,v])#v for each parental node?
  TAB_PRO[paste0(Q),"m"]=sum(proleptic[proleptic$rank_node==Q,m])#m for each parental node?
  TAB_PRO[paste0(Q),"b"]=sum(proleptic[proleptic$rank_node==Q,b])#b for each parental node?
}

TAB_PRO["sums",2:6]=colSums(TAB_PRO[2:6])#sums each observations(obs)
TAB_PRO[,"sum_obs"]=rowSums(TAB_PRO[3:6])#sum obserbations per each node

#proportions of observations (# bud/ tot obs in that node)
for (i in 1:nline) {TAB_PRO[i,8:11]=round((TAB_PRO[i,3:6]/TAB_PRO[i,2])*100,digit=2)}
colnames(TAB_PRO)[8:11]=c("%sylleptic","%V","%M","%B")

#write pdf with the table
pdf("lat_obs_in_pro_shoots~rank.pdf",height = 8,width = 10 )
grid.table(TAB_PRO)
dev.off()

#graph
png("3.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,4))
col=brewer.pal(n=4,name="Set1")
with(TAB_PRO[1:16,], plot(`%sylleptic`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,100), main="frequence buds/shoots in proleptic <own-rooted> parentals", xlab="rank nodes", ylab="%", type="o"))
with(TAB_PRO[1:16,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,type="o"))
with(TAB_PRO[1:16,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,type="o"))
with(TAB_PRO[1:16,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,type="o"))
legend("topleft", legend=c("%sylleptic", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
dev.off()

#2_ % buds in sylleptic shoots####
#~rank node of the proleptic it sprouts in

#subset sylleptic(met level)
syl=met[met$shoot_type=="SYLLEPTIC",]

sh=grep("^shoot$", colnames(syl))
c=grep("^c$", colnames(syl))
v=grep("^v$", colnames(syl))
m=grep("^m$", colnames(syl))
b=grep("^b$", colnames(syl))

TAB_SYL=met[0,0]
nline=length(unique(sort(syl$rank_node)))
for (q in 1:nline) {
  Q=unique(sort(syl$rank_node))[q]
  TAB_SYL[paste0(Q),"rank_node"]=Q
  TAB_SYL[paste0(Q),"shoots_with_tank"]=length(syl[syl$rank_node==Q,sh])
  TAB_SYL[paste0(Q),"c"]=sum(syl[syl$rank_node==Q,c])#c for each parental node?
  TAB_SYL[paste0(Q),"v"]=sum(syl[syl$rank_node==Q,v])#v for each parental node?
  TAB_SYL[paste0(Q),"m"]=sum(syl[syl$rank_node==Q,m])#m for each parental node?
  TAB_SYL[paste0(Q),"b"]=sum(syl[syl$rank_node==Q,b])#b for each parental node?
}

TAB_SYL["sums",2:6]=colSums(TAB_SYL[2:6])#sums each observations(obs)
TAB_SYL[,"sums"]=rowSums(TAB_SYL[3:6])#sum obserbations per each node

#proportions of observations (# bud/ tot obs in that node)
for (i in 1:nline) {TAB_SYL[i,8:11]=round((TAB_SYL[i,3:6]/TAB_SYL[i,2])*100,digit=2)}
colnames(TAB_SYL)[8:11]=c("%C","%V","%M","%B")

#write pdf with the table
pdf("buds_in_syl_shoots~rank.pdf",height = 8,width = 10 )
grid.table(TAB_PRO)
dev.off()

#graph
png("3a.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,4))
col=brewer.pal(n=4,name="Set1")
with(TAB_SYL[1:16,], plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,200), main="frequence buds in syllepotic", xlab="rank nodes of proleptic", ylab="%", type="o"))
with(TAB_SYL[1:16,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,type="o"))
with(TAB_SYL[1:16,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,type="o"))
with(TAB_SYL[1:16,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,type="o"))
legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
dev.off()
