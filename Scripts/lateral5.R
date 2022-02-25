setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
#COMPOSIZIONE DATASET

#importiamo la liberia
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(corrplot)

#bud scale
lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat <- dplyr::mutate(lat, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
lat <- dplyr::mutate(lat, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))
lat=lat[order(lat$length.newshoots),]

met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#proleptic: realizzo una tabella di contingenza
#in cui per ogni rank node parentale, metto la classe di lunghezza dei laterali sono più laterali per nodo)
prol=subset(lat, lat$is_in_sylleptic=="NO")#dataset solo con i NON sylleptic
nshoot=length(lat$shoot)
nrank=max(lat$rank_node)

ndf=data.frame(matrix(ncol=3, nrow = length(met$tesi)))#new def (rank~newshoots)
ndf[c(1,2)]=met[c(4,6)]
colnames(ndf)[c(1,2)]=colnames(met)[c(4,6)]

lne=grep("^length.ne", colnames(prol))
for (i in 1:nshoot) {
  I=lat$shoot[i]
  for (j in 1:nrank) {
    m=prol[prol$shoot==I&prol$rank_node==j,lne]
    q=paste0(m[!is.na(m)], collapse = "+")
    ndf[ndf$shoot==I&ndf$rank_node==j,3]=q
  }
}

ndf=ndf[ndf$X3!="",]

r=table(ndf$rank_node,ndf$X3)
t=prop.table(r,margin=1)*100
#graph5a
png("5a_prol.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(r)),name="Set3")
x<-barplot(t(r),col = cols,main="combinations of laterals from proleptic buds", xlab= "Rank nodes", ylab="# of child class")
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(r)),fill = cols, cex=0.6)
dev.off()

#graph5b
png("5b_prol.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(t)),name="Set3")
x<-barplot(t(t),col = cols,main="combinations of laterals from proleptic buds", xlab= "Rank nodes", ylab="% of child class", ylim=c(0,100))
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(t)),fill = cols, cex=0.6)
dev.off()

#sylleptic: realizzo una tabella di contingenza
#in cui per ogni rank node parentale, metto la classe di lunghezza dei laterali sono più laterali per nodo)
sil=subset(lat, lat$is_in_sylleptic=="YES")#dataset solo con i sylleptic
nshoot=length(lat$shoot)
nrank=max(lat$rank_node)

ndf=data.frame(matrix(ncol=3, nrow = length(met$tesi)))#new def (rank~newshoots)
ndf[c(1,2)]=met[c(4,6)]
colnames(ndf)[c(1,2)]=colnames(met)[c(4,6)]

lne=grep("^length.ne", colnames(sil))
for (i in 1:nshoot) {
  I=lat$shoot[i]
  for (j in 1:nrank) {
    m=sil[sil$shoot==I&sil$rank_node==j,lne]
    q=paste0(m[!is.na(m)], collapse = "+")
    ndf[ndf$shoot==I&ndf$rank_node==j,3]=q
  }
}

ndf=ndf[ndf$X3!="",]

r=table(ndf$rank_node,ndf$X3)
t=prop.table(r,margin=1)*100
#graph5a
png("5a_sylleptic.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(r)),name="Set3")
x<-barplot(t(r),col = cols,main="combinations of laterals from sylleptic buds", xlab= "Rank nodes of parental", ylab="# of child class")
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(r)),fill = cols, cex=0.6)
dev.off()

#graph5b
png("5b_sylleptic.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(t)),name="Set3")
x<-barplot(t(t),col = cols,main="combinations of laterals from sylleptic buds", xlab= "Rank nodes of parental", ylab="% of child class", ylim=c(0,100))
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(t)),fill = cols, cex=0.6)
dev.off()

