#EXPLORATORY ANALYSIS
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/exploratory/"))

#importiamo la liberia
library(dplyr)
library(tibble)
library(RColorBrewer)
library(tidyr)
library(rstatix)

#bud scale
lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
lat <- dplyr::mutate(lat, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
lat <- dplyr::mutate(lat, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))
lat=lat[order(lat$length.newshoots),]

#1: how many V and M buds developed in proleptic shoots?
#dataset solo con i PROLEPTIC
prol=subset(lat, lat$from_=="PROL")

drtot=table(prol$rank_node,prol$new_shoots,prol$fate)#all shoots
Vtot=drtot[,, "V"]#fate=V
Mtot=drtot[,, "M"]#fate=M
Vtot.p=prop.table(Vtot,margin=1)*100#proportions
Mtot.p=prop.table(Mtot,margin=1)*100#proportions

TOT=as.data.frame.matrix(cbind(Vtot.p[,2],Mtot.p[,2]))#%of buds developed per rank node
colnames(TOT)=c("V","M")

#graph
png("6.png",width=1200, height=900, res=150)# save plot
col=brewer.pal(n=4,name="Set2")
with(TOT[1:16,], plot(V, pch=19, cex=1.2,col=col[2], main="proportion of proleptic buds developed", xlab="rank nodes", ylab="%", type="o", ylim=c(0,100)))
with(TOT[1:16,], points(M, pch=19, cex=1.2,col=col[3], type="o", ylim=c(0,100)))
legend("bottom",inset = c(-0.2,0),xpd = TRUE, legend= c("vegetative buds", "mixed buds"), lwd=3, cex= 0.7, col=col[c(2,3)])
dev.off()

#2: what is the length of new shoots from V?####
df=lat[lat$new_shoots!=0,]
df=df[df$from_=="PROL",1:23]
dr=table(df$rank_node,df$length.newshoots,df$fate)
V=as.data.frame.matrix(dr[,, "V"])#developed from v

#graph
png("6a.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(V[1:16,], plot(Sh, pch=19, cex=1.2,col=col[1], main="numbers of laterals from vegetative bud in proleptic shoots", xlab="parental rank nodes", ylab="#", type="o", ylim=c(0,55)))
with(V[1:16,], points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(V[1:16,], points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(V[1:16,], points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(V), lwd=3, cex= 0.8, col=col)
dev.off()

V.p=as.data.frame.matrix(prop.table(dr[,, "V"],margin=1)*100)

#graph
png("6C.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(V.p[1:16,], plot(Sh, pch=19, cex=1.2,col=col[1], main="% of laterals from vegetative bud in proleptic shoots", xlab="parental rank nodes", ylab="%", type="o", ylim=c(0,100)))
with(V.p[1:16,], points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(V.p[1:16,], points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(V.p[1:16,], points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(V.p), lwd=3, cex= 0.8, col=col)
dev.off()

#3: what is the length of new shoots from M?####
M=as.data.frame.matrix(dr[,, "M"])
M=M[1:16,]
#graph
png("6b.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(M, plot(Sh, pch=19, cex=1.2,col=col[1], main="numbers of laterals from mixed bud in proleptic shoots", xlab="parental rank nodes", ylab="#", type="o",ylim=c(0,55)))
with(M, points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(M, points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(M, points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(M), lwd=3, cex= 0.8, col=col)
dev.off()

M.p=as.data.frame.matrix(prop.table(dr[,, "M"],margin=1)*100)
M.p=M.p[1:16,]
#graph6b
png("6d.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(M.p, plot(Sh, pch=19, cex=1.2,col=col[1], main="% of laterals from mixed bud in proleptic shoots", xlab="parental rank nodes", ylab="%", type="o",ylim=c(0,100)))
with(M.p, points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(M.p, points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(M.p, points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(M.p), lwd=3, cex= 0.8, col=col)
dev.off()