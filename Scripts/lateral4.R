setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
#COMPOSIZIONE DATASET

#importiamo la liberia
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(corrplot)

#metamer level
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#proleptic shoots
prolep=met
#elimino tutte le gemme associate con i sylleptic
c=grep("^c$", colnames(prolep))
v=grep("^v$", colnames(prolep))
m=grep("^m$", colnames(prolep))
b=grep("^b$", colnames(prolep))
nl=grep("^n_la", colnames(prolep))
for (i in 1:nrow(prolep)) {
  if (prolep[i,c]==1) {
    prolep[i,c(v,m,b)]=0
    prolep[i,nl]=1#change the number of buds at that rank with 1 when there are sylleptic
  }
}

#how many buds per node in proleptic shoots?
ranksyl=table(prolep$rank_node, prolep$n_lateral_buds)

#in questo grafico le gemme nei sylleptic NON vengono contate
png("4b.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksyl)),name="Set3")
x<-barplot(t(ranksyl),col = cols,main="number of buds/sylleptic per rank in proleptic shoots", xlab= "Rank nodes", ylab="# of multiple buds/sylleptic")
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksyl)),fill = cols, cex=0.8)
dev.off()

ranksyl=prop.table(ranksyl,margin=1)*100#propporzioni per ogni rango nodo delle differenti combinazioni di numero di gemme
head(ranksyl,20)

#graph4d
png("4d.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksyl)),name="Set3")
x<-barplot(t(ranksyl),col = cols, main="% of buds/sylleptic per rank in proleptic shoots", xlab= "Rank nodes", ylab="% of multiple buds", ylim=c(0,100))
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksyl)),fill = cols, cex=0.8)
dev.off()

# #what is the possible number of buds per node rank?
# #considerando catkin e gemme associate come sylleptic
# comb_prolep=as.data.frame.matrix(table(prolep$v,prolep$m))
# colnames(comb_prolep)=c("0m", "1m", "2m", "3m", "4m", "5m")
# rownames(comb_prolep)=c("0v","1v", "2v", "3v", "4v")
# comb_prolep
# 
# #m e v sono correlate?
# # chisq=chisq.test(combi)#p_value <0.05 quindi m e v sono dipendenti
# # corrplot(chisq$residuals, is.corr = F)

#what is the composition of mmultiple buds in proleptic?
#modifico prolep in modo tale da generare le comb_prolep
prolep$c <- gsub("1", "sylleptic", prolep$c)
prolep$v <- gsub("1", "v", prolep$v)
prolep$m <- gsub("1", "m", prolep$m)
prolep$b <- gsub("1", "b", prolep$b)
prolep$v <- gsub("2", "v+v", prolep$v)
prolep$m <- gsub("2", "m+m", prolep$m)
prolep$v <- gsub("3", "v+v+v", prolep$v)
prolep$m <- gsub("3", "m+m+m", prolep$m)
prolep$v <- gsub("4", "v+v+v+v", prolep$v)
prolep$m <- gsub("4", "m+m+m+m", prolep$m)
prolep$m <- gsub("5", "m+m+m+m+m", prolep$m)
prolep[prolep$c==0,c]=NA
prolep[prolep$v==0,v]=NA
prolep[prolep$m==0,m]=NA
prolep[prolep$b==0,b]=NA
head(prolep,20)

ne=ncol(prolep)+1
nline=length(unique(prolep$shoot))
nliner=length(unique(prolep$rank_node))
for (i in 1:nline){
  
  I=unique(sort(prolep$shoot))[i]
  
  for (t in 1:nliner){
    
    K= unique(sort(prolep$rank_node))[t]
    prolep[prolep$shoot==I & prolep$rank_node==K,ne]<-unite(prolep[prolep$shoot==I & prolep$rank_node==K,c(c,v,m,b)], col="merge", na.rm = TRUE, sep = '+')
    
  }
}

head(prolep,20)

#what is the possible combinations of lateral buds per node rank?
bind=as.data.frame.matrix(table(prolep$rank_node,prolep$merge))
head(bind,20)

#graph4e_numbers in proleptic
png("4e.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c(brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 7))
x<-barplot(t(bind[1:17,]),col = cols,main="# combination of buds/sylleptic in proleptic shoots ", xlab= "Rank nodes", ylab="# of observation")
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(bind)),fill = cols, cex=0.6)
dev.off()

merg=as.data.frame.matrix(prop.table(table(prolep$rank_node,prolep$merge), margin=1)*100)
head(merg,20)

#graph4f_percentage in proleptic
png("4f.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c(brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 7))
x<-barplot(t(merg[1:17,]),col = cols,main="% combination of buds/sylleptic in proleptic shoots ", xlab= "Rank nodes", ylab="% of observation", ylim=c(0,100))
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(merg)),fill = cols, cex=0.6)
dev.off()

#what is the variability in sylleptic shoots?
#df just for buds inside silleptic
sil=subset(met, shoot_type=="SYLLEPTIC")#dataset solo con i sylleptic 

ranksi=table(sil$rank_node, sil$n_lateral_buds)
#in questo grafico conto catkin e le gemme nei sylleptic
png("4g.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksi)),name="Set3")
x<-barplot(t(ranksi),col = cols,main="number of buds into sylleptic", xlab= "Rank nodes of parental", ylab="# of buds in sylleptic")
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksi)),fill = cols, cex=0.8)
dev.off()

ranksi=prop.table(ranksi,margin=1)*100#propporzioni per ogni rango nodo delle differenti combinazioni di numero di gemme
head(ranksi,20)

#graph4h
png("4h.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(ranksi)),name="Set3")
x<-barplot(t(ranksi),col = cols, main="% of buds in sylleptic", xlab= "Rank nodes of parental", ylab="% of buds in sylleptic", ylim=c(0,100))
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(ranksi)),fill = cols, cex=0.8)
dev.off()

#combination of buds in sylleptic shoots
#modifico b in modo tale da generare le combinazioni 
c=grep("^c$", colnames(sil))
v=grep("^v$", colnames(sil))
m=grep("^m$", colnames(sil))
b=grep("^b$", colnames(sil))
sil$c <- gsub("1", "c", sil$c)
sil$v <- gsub("1", "v", sil$v)
sil$m <- gsub("1", "m", sil$m)
sil$b <- gsub("1", "b", sil$b)
sil$v <- gsub("2", "v+v", sil$v)
sil$m <- gsub("2", "m+m", sil$m)
sil$v <- gsub("3", "v+v+v", sil$v)
sil$m <- gsub("3", "m+m+m", sil$m)
sil$v <- gsub("4", "v+v+v+v", sil$v)
sil$m <- gsub("4", "m+m+m+m", sil$m)
sil$m <- gsub("5", "m+m+m+m+m", sil$m)
sil$v <- gsub("5", "v+v+v+v+v", sil$v)
sil$m <- gsub("6", "m+m+m+m+m+m", sil$m)
sil[sil$v==0,v]=NA
sil[sil$m==0,m]=NA
sil[sil$b==0,b]=NA

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

#what is the possible combinations of buds in sylleptic shoots per node rank?
bindsi=as.data.frame.matrix(table(sil$rank_node,sil$merge))
head(bindsi,20)

#graph4e
png("4i.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c("black", "blue","red","white","green","pink","yellow","gray","brown",brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 6))
x<-barplot(t(bindsi[,1:22]),col = cols,main="combination of buds in sylleptic  shoots", xlab= "Rank nodes of parental", ylab="# of observation")
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(bindsi[,1:22])),fill = cols, cex=0.6)
dev.off()

mergsi=as.data.frame.matrix(prop.table(table(sil$rank_node,sil$merge), margin=1)*100)
head(mergsi,20)

#graph4f
png("4l.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 10))
cols<-c("black", "blue","red","white","green","pink","yellow","gray","brown",brewer.pal(name="Spectral", n=11)[c(2:5,9:11)],brewer.pal(name="Dark2", n = 6))
x<-barplot(t(mergsi[,1:22]),col = cols,main="combination of buds in sylleptic  shoots", xlab= "Rank nodes of parental", ylab="% of observation", ylim=c(0,100))
legend("topright",inset = c(-0.3, +0.01),xpd = TRUE, legend = rownames(t(mergsi[,1:22])),fill = cols, cex=0.6)
dev.off()
