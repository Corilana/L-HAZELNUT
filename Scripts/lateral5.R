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

#met.scale
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#1: what is the combination of lateral childs in proleptic?
#dataset solo con i PROLEPTIC
prol=subset(lat, lat$from_=="PROL")

lne=grep("^length.ne", colnames(prol))#adress the column with the class of the child
TAB=lat[0,0]#empty df

nshoot=length(unique(sort(lat$shoot)))#first loop length
nrank=length(unique(sort(lat$rank_node)))#second loop length
for (i in 1:nshoot) {
  I=unique(sort(lat$shoot))[i]
  for (j in 1:nrank) {
    J=unique(sort(lat$rank_node))[j]
    m=prol[prol$shoot==I&prol$rank_node==J,lne]
    if (length(m)!=0) {
      q=paste0(m[!is.na(m)], collapse = "+")
      bi=cbind("shoot"=I, "rank_node"=J, "laterals"=q)
      TAB=rbind(TAB, bi)
    }
  }
}

TAB$rank_node=as.numeric(TAB$rank_node)
TAB=TAB[TAB$laterals!="",]
TAB=TAB[order(TAB$rank_node),]

r=table(TAB$rank_node,TAB$laterals)
t=prop.table(r,margin=1)*100
#graph
png("5.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(r[1:16,])),name="Set3")
x<-barplot(t(r[1:16,]),col = cols,main="combinations of laterals from proleptic buds", xlab= "Rank nodes", ylab="# of child class")
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(r[1:16,])),fill = cols, cex=0.6)
dev.off()

#graph
png("5a.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(t[1:16,])),name="Set3")
x<-barplot(t(t[1:16,]),col = cols,main="combinations of laterals from proleptic buds", xlab= "Rank nodes", ylab="% of child class", ylim=c(0,100))
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(t[1:16,])),fill = cols, cex=0.6)
dev.off()

#2: what is the combination of lateral childs in proleptic?####
sil=subset(lat, lat$from_=="SYL")

lne=grep("^length.ne", colnames(sil))#adress the column with the class of the child
TAB=lat[0,0]#empty df

nshoot=length(unique(sort(lat$shoot)))#first loop length
nrank=length(unique(sort(lat$rank_node)))#second loop length
for (i in 1:nshoot) {
  I=unique(sort(lat$shoot))[i]
  for (j in 1:nrank) {
    J=unique(sort(lat$rank_node))[j]
    m=sil[sil$shoot==I&sil$rank_node==J,lne]
    if (length(m)!=0) {
      q=paste0(m[!is.na(m)], collapse = "+")
      bi=cbind("shoot"=I, "rank_node"=J, "laterals"=q)
      TAB=rbind(TAB, bi)
    }
  }
}

TAB$rank_node=as.numeric(TAB$rank_node)
TAB=TAB[TAB$laterals!="",]
TAB=TAB[order(TAB$rank_node),]

r=table(TAB$rank_node,TAB$laterals)
t=prop.table(r,margin=1)*100

#graph
png("5c.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(t(r[1:16,]))),name="Set3")
x<-barplot(t(r[1:16,]),col = cols,main="combinations of laterals from sylleptic buds", xlab= "Rank nodes of parental", ylab="# of child class")
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(r[1:16,])),fill = cols, cex=0.6)
dev.off()

#graph5b
png("5d.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-brewer.pal(n=length(colnames(t[1:16,])),name="Set3")
x<-barplot(t(t[1:16,]),col = cols,main="combinations of laterals from sylleptic buds", xlab= "Rank nodes of parental", ylab="% of child class", ylim=c(0,100))
legend("topright",inset=c(-0.2,-0.15),xpd = TRUE, legend = rownames(t(t[1:16,])),fill = cols, cex=0.6)
dev.off()