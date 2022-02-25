setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Apical")
#COMPOSIZIONE DATASET

#importiamo la liberia
library(dplyr)
library(rstatix)
library(RColorBrewer)

#APICAL SHOOT: SHOOT SCALE
#bud level
ap=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_APICALS.csv")
ap <- dplyr::mutate(ap, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
ap <- dplyr::mutate(ap, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))

length(unique(ap$shoot))#104 numero totale germogli analizzati per successione
table(ap$class)#28LO, 25Me, 26Sh, 25VLo sudivisione nelle 4 categorie di lunghezza

table(ap$length.newshoots)#how many apicals for each length category? 2Lo, 26Me, 75Sh, 1VLo

#how many buds per parental class length?
tot_=grep("tot_buds", colnames(ap))
sum(ap[ap$class=="Sh",tot_])#32 totbuds in parental sh
sum(ap[ap$class=="Me",tot_])#27 totbuds in parental me
sum(ap[ap$class=="Lo",tot_])#31 totbuds in parental lo
sum(ap[ap$class=="VLo",tot_])#45 totbuds in parental VLo

sum(table(ap$fate)[2:3])#102 M+V

new=table(ap$class,ap$length.newshoots)#parent length ~ apical length~
new.p=round(prop.table(new, margin = 1)*100,digit=2)
colnames(new.p)=c("child_sh","child_me","child_lo","child_vlo")

#graph
png("relationparentchild_AP.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(new.p)),name="Set2")
x<-barplot(t(new.p),beside= T,col = cols, main="succession",xlab= "Parent length", ylab="% child length(es. #childSh/totalchildSh)", ylim=c(0,110))
legend("top",horiz=T,inset=c(0,-0.02),xpd = TRUE, legend = rownames(t(new.p)),fill = cols, cex=0.6)
text(x[1:2,]+0.2, t(new.p[,1:2])+3.5, paste(t(new.p[,1:2]),"%"), cex = 0.7)
text(x[2,], t(new.p[,2])+8, c("b","b","ab","a"), cex = 0.7)
dev.off()

#has the medium lateral different proportions according to parental length?
chisq.test(new.p[,2])#i rami medi NON sono uguali per ogni classe parentale
pairwise_chisq_gof_test(new.p[,2])#sh!=Vlo; Me!=VLo

#METAMER SCALE:
#come è la composizione alle gemme apicali in ogni classe di lunghezza degli annual shoot?
df1=table(ap$class,ap$fate)
df1.p=round(prop.table(df1,margin=1)*100, digit=2)

#graph2
png("apicalbuds_AS.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(df1.p)),name="Set2")
x<-barplot(as.matrix(t(df1.p)), col = cols, main="apical buds type", xlab="parental classe length", ylab="apical buds type (%)")
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
text(x,q1.p+8 , c("b","a", "a"), cex = 1)
dev.off()

#le tre proporzioni sono diverse?
chisq.test(q1.p)#si!
#pairwise per confrontarli tutti insieme
pairwise_chisq_gof_test(q1.p)#c!=v; c!=m; v=m; 

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
