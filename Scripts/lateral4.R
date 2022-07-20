#EXPLORATORY ANALYSIS
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/exploratory/"))

#importiamo la liberia
library(dplyr)
library(tibble)
library(RColorBrewer)
library(tidyr)
library(rstatix)
library(gridExtra)

#metamer level
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#in proleptic df, we have to put the observations inside the sylleptic as 0
#and indicate the presence/absence of sylleptic shoots, at that node, with 0(abs) or 1(pres)
proleptic=met#copy df metamer level

c=grep("^c$", colnames(proleptic))
v=grep("^v$", colnames(proleptic))
m=grep("^m$", colnames(proleptic))
b=grep("^b$", colnames(proleptic))
nl=grep("tot_", colnames(proleptic))

for (q in 1:nrow(proleptic)) {
  if (proleptic[q,c]==1) {
    proleptic[q,c(m,v,b)]=0#trasform to zero the values of the buds in sylleptic
    proleptic[q,nl]=1#change the sum of lat buds at that rank with 1 when there are sylleptic
  }
  
}

colnames(proleptic)[c]="sylleptic"#CHANGE THE name of COLUMN "C" WITH "SYLLEPTIC"

#1:multiple buds ~ rank node in prolepotic?####
ranksyl=table(proleptic$rank_node, proleptic$tot_buds)

#graph
png("4.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksyl[1:16,])),name="Set3")
x<-barplot(t(ranksyl[1:16,]),col = cols,main="number of buds/sylleptic per rank in proleptic shoots", xlab= "Rank nodes", ylab="# of multiple buds/sylleptic")
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksyl[1:16,])),fill = cols, cex=0.8)
dev.off()

#proportions
ranksyl=prop.table(ranksyl,margin=1)*100#propporzioni per ogni rango nodo delle differenti combinazioni di numero di gemme
head(ranksyl,20)

#graph
png("4a.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksyl[1:16,])),name="Set3")
x<-barplot(t(ranksyl[1:16,]),col = cols, main="% of buds/sylleptic per rank in proleptic shoots", xlab= "Rank nodes", ylab="% of multiple buds", ylim=c(0,100))
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksyl[1:16,])),fill = cols, cex=0.8)
dev.off()

#2: what is the composition of multiple buds in proleptic?####
nline=length(proleptic$tesi)
s=grep("^sylleptic", colnames(proleptic))
tot=c(s,v,m,b)

#change the number of buds with the letter (es. 1v =v)
for (i in 1:nline) {
  for (j in 1:length(tot)) {
    J=tot[j]
    l=proleptic[i,J]
    if (l!=0) {
      name=colnames(proleptic)[J]
      proleptic[i,J]=paste0(rep(name, each = l), collapse = "+")
    } else {proleptic[i,J]=NA}
  }
}

#merge with + es( v and m = v+m)
ne=ncol(proleptic)+1
nline=length(unique(proleptic$shoot))#TOT SHOOTS
nliner=length(unique(proleptic$rank_node))#TOT RANKS

for (i in 1:nline){
  I=unique(sort(proleptic$shoot))[i]
  for (t in 1:nliner){
    K= unique(sort(proleptic$rank_node))[t]
    proleptic[proleptic$shoot==I & proleptic$rank_node==K,ne]<-unite(proleptic[proleptic$shoot==I & proleptic$rank_node==K,c(c,v,m,b)],
                                                                     col="merge", na.rm = TRUE, sep = '+')
    
  }
}

#3: what is the possible combinations of lateral buds per node rank?####
bind=as.data.frame.matrix(table(proleptic$rank_node,proleptic$merge))
head(bind,20)

#graph
png("4b.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c(brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 7))
x<-barplot(t(bind[1:16,]),col = cols,main="# combination of buds/sylleptic in proleptic shoots ", xlab= "Rank nodes", ylab="# of observation")
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(bind[1:16,])),fill = cols, cex=0.6)
dev.off()

merg=as.data.frame.matrix(prop.table(table(proleptic$rank_node,proleptic$merge), margin=1)*100)
head(merg,20)

#graph
png("4c.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c(brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 7))
x<-barplot(t(merg[1:16,]),col = cols,main="% combination of buds/sylleptic in proleptictic shoots ", xlab= "Rank nodes", ylab="% of observation", ylim=c(0,100))
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(merg[1:16,])),fill = cols, cex=0.6)
dev.off()

#4: multiple buds ~ rank node in sylleptic?####
sil=subset(met, shoot_type=="SYLLEPTIC")#dataset solo con i sylleptic 
ranksi=table(sil$rank_node, sil$tot_buds)

#graph
png("4d.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksi)),name="Set3")
x<-barplot(t(ranksi),col = cols,main="number of buds into sylleptic", xlab= "Rank nodes of parental", ylab="# of buds in sylleptic")
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksi)),fill = cols, cex=0.8)
dev.off()

ranksi=prop.table(ranksi,margin=1)*100#propporzioni per ogni rango nodo delle differenti combinazioni di numero di gemme
head(ranksi,20)

#graph4h
png("4e.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksi)),name="Set3")
x<-barplot(t(ranksi),col = cols, main="% of buds in sylleptic", xlab= "Rank nodes of parental", ylab="% of buds in sylleptic", ylim=c(0,100))
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksi)),fill = cols, cex=0.8)
dev.off()

#5: what is the composition of multiple buds in sylleptic?####
nline=length(sil$tesi)

s=grep("^c$", colnames(sil))
v=grep("^v$", colnames(sil))
m=grep("^m$", colnames(sil))
b=grep("^b$", colnames(sil))
tot=c(s,v,m,b)

#change the number of buds with the letter (es. 1v =v)
for (i in 1:nline) {
  for (j in 1:length(tot)) {
    J=tot[j]
    l=sil[i,J]
    if (l!=0) {
      name=colnames(sil)[J]
      sil[i,J]=paste0(rep(name, each = l), collapse = "+")
    } else {sil[i,J]=NA}
  }
}

#merge with + es( v and m = v+m)
ne=ncol(sil)+1
nline=length(unique(sil$shoot))
nliner=length(unique(sil$rank_node))

for (i in 1:nline){
  
  I=unique(sort(sil$shoot))[i]
  
  for (t in 1:nliner){
    
    K= unique(sort(sil$rank_node))[t]
    sil[sil$shoot==I & sil$rank_node==K,ne]<-unite(sil[sil$shoot==I & sil$rank_node==K,c(c,v,m,b)], col="merge", na.rm = TRUE, sep = '+')
    
  }
}

#6: what is the possible combinations of buds per proleptic node rank?####
bindsi=as.data.frame.matrix(table(sil$rank_node,sil$merge))
head(bindsi,20)

#graph
png("4f.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c("black", "blue","red","white","green","pink","yellow","gray","brown",brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 6))
x<-barplot(t(bindsi[,1:22]),col = cols,main="combination of buds in sylleptic  shoots", xlab= "Rank nodes of parental", ylab="# of observation")
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(bindsi[,1:22])),fill = cols, cex=0.6)
dev.off()

mergsi=as.data.frame.matrix(prop.table(table(sil$rank_node,sil$merge), margin=1)*100)
head(mergsi,20)

#graph
png("4g.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c("black", "blue","red","white","green","pink","yellow","gray","brown",brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 6))
x<-barplot(t(mergsi[,1:22]),col = cols,main="combination of buds in sylleptic  shoots", xlab= "Rank nodes of parental", ylab="% of observation", ylim=c(0,100))
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(mergsi[,1:22])),fill = cols, cex=0.6)
dev.off()

