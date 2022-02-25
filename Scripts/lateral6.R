setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
#COMPOSIZIONE DATASET
#importiamo la liberia
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(corrplot)

#carico df dei laterali
lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat <- dplyr::mutate(lat, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
lat <- dplyr::mutate(lat, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))
lat=lat[order(lat$length.newshoots),]

# newshoot=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/DFAUTO_new.shootlevel.csv")
# newshoot=newshoot[newshoot$X.newshoot2yo!=0,]#1019(884lat +134ap+30 do not know(?))
# #can we ignore "?"?
# (length(newshoot[newshoot$fate=="?",14])/length(newshoot$fate))*100#2.9%  we can avoid using this df because ? is negligeble
# 
# #errors in derived from B or C (in df without ?)
# (length(newshoot[newshoot$fate=="C"|newshoot$fate=="B",15])/length(newshoot$fate))*100#4.6% negligeble!

#in proleptic
prol=lat[lat$is_in_sylleptic=="NO",1:19]
#percentuale gemme V e M che si sono sviluppate
mv=length(prol[prol$fate=="V"|prol$fate=="M",1])#tot m+v
son=sum(prol[prol$fate=="V"|prol$fate=="M",15])#germogli sviluppati da m e v
(son/mv)*100#79.942%!!!

#how many developed in proleptic?
drtot=table(prol$rank_node,prol$new_shoots,prol$fate)#all shoots
Vtot=drtot[,, "V"]
Mtot=drtot[,, "M"]
Vtot.p=prop.table(Vtot,margin=1)*100
Mtot.p=prop.table(Mtot,margin=1)*100

TOT=as.data.frame.matrix(cbind(Vtot.p[,2],Mtot.p[,2]))
colnames(TOT)=c("V","M")

#graph6c
png("6c.png",width=1200, height=900, res=150)# save plot
col=brewer.pal(n=4,name="Set2")
with(TOT, plot(V, pch=19, cex=1.2,col=col[2], main="proportion of proleptic buds developed", xlab="parental rank nodes", ylab="%", type="o", ylim=c(0,100)))
with(TOT, points(M, pch=19, cex=1.2,col=col[3], type="o", ylim=c(0,100)))
legend("bottom",inset = c(-0.2,0),xpd = TRUE, legend= c("vegetative buds", "mixed buds"), lwd=3, cex= 0.7, col=col[c(2,3)])
dev.off()

#what is the length of new shoots from V and M?
df=lat[lat$new_shoots!=0,]
df=df[df$is_in_sylleptic=="NO",1:19]
dr=table(df$rank_node,df$length.newshoots,df$fate)#only developed shoots
V=as.data.frame.matrix(dr[,, "V"])#developed from v
#graph6a
png("6a.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(V, plot(Sh, pch=19, cex=1.2,col=col[1], main="numbers of laterals from vegetative bud in proleptic shoots", xlab="parental rank nodes", ylab="#", type="o", ylim=c(0,55)))
with(V, points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(V, points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(V, points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(V), lwd=3, cex= 0.8, col=col)
dev.off()

M=as.data.frame.matrix(dr[,, "M"])
#graph6b
png("6b.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(M, plot(Sh, pch=19, cex=1.2,col=col[1], main="numbers of laterals from mixed bud in proleptic shoots", xlab="parental rank nodes", ylab="#", type="o",ylim=c(0,55)))
with(M, points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(M, points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(M, points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(M), lwd=3, cex= 0.8, col=col)
dev.off()

V.p=as.data.frame.matrix(prop.table(dr[,, "V"],margin=1)*100)
M.p=as.data.frame.matrix(prop.table(dr[,, "M"],margin=1)*100)

#graph6a_prop
png("6a_prop.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(V.p, plot(Sh, pch=19, cex=1.2,col=col[1], main="% of laterals from vegetative bud in proleptic shoots", xlab="parental rank nodes", ylab="%", type="o", ylim=c(0,100)))
with(V.p, points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(V.p, points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(V.p, points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(V.p), lwd=3, cex= 0.8, col=col)
dev.off()

#graph6b
png("6b_prop.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
col=brewer.pal(n=4,name="Set1")
with(M.p, plot(Sh, pch=19, cex=1.2,col=col[1], main="% of laterals from mixed bud in proleptic shoots", xlab="parental rank nodes", ylab="%", type="o",ylim=c(0,100)))
with(M.p, points(Me, pch=19, col=col[2],cex=1.2,type="o"))
with(M.p, points(Lo, pch=19, col=col[3],cex=1.2,type="o"))
with(M.p, points(VLo, pch=19, col=col[4],cex=1.2,type="o"))
legend("topright",inset = c(-0.2,0),xpd = TRUE, legend= colnames(M.p), lwd=3, cex= 0.8, col=col)
dev.off()