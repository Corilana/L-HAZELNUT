#analisi delle gemme apicali
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Apical"))
#COMPOSIZIONE DATASET

#importiamo la liberia
library(dplyr)
library(rstatix)
library(RColorBrewer)

#APICAL SHOOT: SHOOT SCALE
#bud level
ap=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_APICALS.csv"))
ap <- dplyr::mutate(ap, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
ap <- dplyr::mutate(ap, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))

#store index column "shoot id"
s=grep("^shoot$",colnames(ap))
#store index column "parent class"
c=grep("^class$",colnames(ap))
#store index column "length parent"
lpar=grep("^Length$",colnames(ap))
#store index column "length new shots"
lnew=grep(".new",colnames(ap))
#store index column "fate parent bud"
f=grep("fate$",colnames(ap))

#1Parental class
TAB1=cbind(as.data.frame(table(unique(ap[c:s])[1])),#nb_parental shoot per class
           (as.data.frame(table(ap[c]))[2]),##nb_buds in parentals per class
           (as.data.frame(table(unique(ap[ap$new_shoots!=0,c:s])[1]))[2]),#nb_ parental with children per class
           as.data.frame(table(ap[ap$new_shoots!=0,c(c:s,lnew)][3]))#nb_children per class
)
colnames(TAB1)=c("Class","parental_freq","tot_ap_buds","parental_with_children_freq","child_class","children_freq")
TAB1["Sum",c(2:4,6)]=colSums(TAB1[c(2:4,6)])

#write pdf with the table
pdf("class_frequences.pdf",height = 4,width = 13 )
grid.table(TAB1)
dev.off()

#how many m&v apical
sum(table(ap$fate)[2:3])#102 M+V

#CLASS FREQ  (#child/#parentinthatlength)
nb=as.matrix(table(ap$class,ap$length.newshoots))
prop=as.data.frame.matrix(round(prop.table(table(ap$class,ap$length.newshoots), margin=1)*100, digit=2))
prop

#graph
png("relationparentchild_AP.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(prop)),name="Set2")
x<-barplot(t(prop),beside= T,col = cols, main="succession",xlab= "Parent length", ylab="% child length(es. #childSh/totalchildSh)", ylim=c(0,110))
legend("top",horiz=T,inset=c(0,-0.02),xpd = TRUE, legend = rownames(t(prop)),fill = cols, cex=0.6)
text(x+0.2, t(prop)+3.5, paste(t(prop),"%"), cex = 0.7)
text(x[1,], t(prop[,1])+8, c("a","a","ab","b"), cex = 0.7, col = cols[1])
text(x[2,], t(prop[,2])+8, c("b","b","ab","a"), cex = 0.7, col = cols[2])
dev.off()

#is sh child proportion different according to parental length?
prop.test(x = nb[,1], rowSums(nb))#yes
pairwise.prop.test(x = nb[,1], rowSums(nb), p.adjust.method = "none")# VLo è diverso da me e da sh
#is me child proportion different according to parental length?
prop.test(x = nb[,2], rowSums(nb))#yes
pairwise.prop.test(x = nb[,2], rowSums(nb), p.adjust.method = "none")# VLo è diverso da me e da sh
#is lo child proportion different according to parental length?
prop.test(x = nb[,3], rowSums(nb))#no
#is vlo child proportion different according to parental length?
prop.test(x = nb[,4], rowSums(nb))#no

#METAMER SCALE:
#come è la composizione alle gemme apicali in ogni classe di lunghezza degli annual shoot?
df=table(ap$class,ap$fate)
df1as.data.frame.matrix(df)
df1["sums",]=colSums(df1)#sums each observations(obs)
df1[,"sums"]=rowSums(df1)#sum obserbations per each class

df1.p=round(prop.table(df,margin=1)*100, digit=2)

#graph2
png("apicalbuds_AS.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(df1.p)),name="Set2")
x<-barplot(as.matrix(t(df1.p)),beside = T,
           col = cols, main="apical buds type", xlab="parental classe length", ylab="apical buds type (%)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = colnames(df1.p),fill = cols, cex=0.8)
dev.off()

#quale è la percentuale gemme indipendentemente dalla classe di lunghezza??
q1=table(ap$fate)
q1.p=round(prop.table(q1)*100, digit=2)

#grafico3
png("frequenceapicalbuds_AS.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=3,name="Set2")
x=barplot(q1.p,col = cols[as.factor(rownames(q1.p))], names.arg = rownames(q1.p), main="frequence apical buds type in annual shoot", xlab = "buds type", ylab="%", ylim = c(0,100))
text(x,q1.p+3 , paste0(q1.p,"%"), cex = 1)
text(x,q1.p+8 , c("c","b", "a"), cex = 1)
dev.off()

#le tre proporzioni sono diverse?
prop.test(as.numeric(q1), rep(sum(q1),3))#si!
#pairwise per confrontarli tutti insieme
pairwise.prop.test(as.numeric(q1), rep(sum(q1),3))

# #dove si trovano gli apical?
# w=table(ap$rank_node, ap$length.newshoots)
# w.p=round(prop.table(w,margin=1)*100, digit=2)
# #graph
# png("numberswhereapical_AP.png",width=1200, height=900, res=150)# save plot
# par(mar = c(5, 5, 4, 6))
# cols<-brewer.pal(n= ncol(w),name="Set2")
# x<-barplot(t(w), ylim=c(0,12),col=cols,main="apical type distribution",xlab="paerntal length",ylab="number of apical type",)
# legend("topright",inset=c(0,0.1),xpd = TRUE,legend = rownames(t(w)),fill = cols, cex=0.6)
# dev.off()
# 
# png("whereapical_AP.png",width=1200, height=900, res=150)# save plot
# par(mar = c(5, 5, 4, 5))
# x<-barplot(t(w.p), col=cols,main="frequence of apical type among parental length",xlab="length",ylab="% (number of apical type/total apical per node)")
# legend("topright",inset=c(-0.06,0),xpd = TRUE, legend = rownames(t(w.p)),fill = cols, cex=0.6)
# dev.off()
