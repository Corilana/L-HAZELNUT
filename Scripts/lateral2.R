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

#met.scale
met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
#in proleptic df, we have to put the observations inside the sylleptic as 0
#and indicate the presence/absence of sylleptic shoots, at that node, with 0(abs) or 1(pres)
proleptic=met#copy df metamer level
c=grep("^c$", colnames(proleptic))
v=grep("^v$", colnames(proleptic))
m=grep("^m$", colnames(proleptic))
b=grep("^b$", colnames(proleptic))
for (q in 1:nrow(proleptic)) {
  if (proleptic[q,c]==1) {
    proleptic[q,c(m,v,b)]=0#trasform to zero the values of the buds in sylleptic
  }

}
colnames(proleptic)[c]="sylleptic"#CHANGE THE name of COLUMN "C" WITH "SYLLEPTIC"

sy=grep("^shoot_type", colnames(proleptic))

#1_ % lateral buds or sylleptic in proleptic shoots####
#(-->df:proleptic)

TAB_PRO=met[0,0]#empty df
#create a table to store the frequence of each obs in the shoots
for (i in 1:4) {
  prol_class=levels(proleptic$class)[i]
  TAB_PRO[i,"class"]=prol_class
  TAB_PRO[i, "nb_sylleptic"]=sum(proleptic[proleptic$class==prol_class,"sylleptic"])
  TAB_PRO[i, "nb_v"]=sum(proleptic[proleptic$class==prol_class,"v"])
  TAB_PRO[i, "nb_m"]=sum(proleptic[proleptic$class==prol_class,"m"])
  TAB_PRO[i, "nb_b"]=sum(proleptic[proleptic$class==prol_class,"b"])
}
TAB_PRO["sums",-1]=colSums(TAB_PRO[,-1])#sums each observations(obs)
TAB_PRO[,"sums"]=rowSums(TAB_PRO[-1])#sum obserbations per each class

#write pdf with the table
pdf("lat_obs_in_pro_shoots~class.pdf",height = 3,width = 5 )
grid.table(TAB_PRO)
dev.off()

#proportions of observations (# bud/ tot obs in that class)
qnew=t(round(prop.table(as.matrix(TAB_PRO[1:4,2:5]),1)*100, 2))
colnames(qnew)=TAB_PRO[1:4,1]

#grafico2
png("2.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(qnew,col = cols,beside = T,
          names.arg = colnames(qnew), main="frequence lateral buds class in proleptic parental", xlab = "parent class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
text(x[1,], qnew[1,]+5, c("a","a","a","b"), col = cols[1])
text(x[2,], qnew[2,]+5, c("a","ab","ab","a"),col = cols[2])
text(x[3,], qnew[3,]+5, c("ab","b","a","a"),col = cols[3])
text(x[4,], qnew[4,]+5, c("a","a","b","b"),col = cols[4])
dev.off()

#pprop.test per la differenza tra proporzioni
#c'è differenza nella proporzione dei sillettici nelle diverse classi?
prop.test(TAB_PRO[1:4,2], TAB_PRO[1:4,6])#si
pairwise.prop.test(TAB_PRO[1:4,2], TAB_PRO[1:4,6])#vlo diverso da sh, da me da lo
#c'è differenza nella proporzione di v nelle diverse classi?
prop.test(TAB_PRO[1:4,3], TAB_PRO[1:4,6])#si
pairwise.prop.test(TAB_PRO[1:4,3], TAB_PRO[1:4,6])#vlo diverso da da me da lo
#c'è differenza nella proporzione di m nelle diverse classi?
prop.test(TAB_PRO[1:4,4], TAB_PRO[1:4,6])#si
pairwise.prop.test(TAB_PRO[1:4,4], TAB_PRO[1:4,6])#vlo diverso da sh, da me. lo diverso da sh e da me
#c'è differenza nella proporzione di b nelle diverse classi?
prop.test(TAB_PRO[1:4,5], TAB_PRO[1:4,6])#si
pairwise.prop.test(TAB_PRO[1:4,5], TAB_PRO[1:4,6])#vlo diverso da sh, me. lo diverso da me

#stesso grafico ma con numeri al posto delle proporzioni
png("2b.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(TAB_PRO[1:4,2:5])),name="Set2")
x<-barplot(as.matrix(t(TAB_PRO[1:4,2:5])),names.arg = TAB_PRO[1:4,1], beside=T,col = cols, main="lateral buds or shoot class", xlab="parental class length", ylab="lateral buds or shoot class (#)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#in generale senza la suddivisione in classi
q1new=round(prop.table(as.matrix(TAB_PRO[5,2:5]),1)*100, 2)
q1new

#graph%
png("2a.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(q1new,col = cols,beside = T, names.arg = colnames(q1new), main="frequence lateral buds/sylleptic in proleptic parental", xlab = "buds/sylleptic", ylab="%", ylim = c(0,100))
text(x,q1new+3 , paste0(round(q1new, digit=2),"%"), cex = 1)
text(x,q1new+10, c("c","b","a","d"), cex = 1)
dev.off()

#sono differenti?
prop.test(as.numeric(TAB_PRO[5,2:5]), rep(TAB_PRO[5,6],4))
pairwise.prop.test(as.numeric(TAB_PRO[5,2:5]), rep(TAB_PRO[5,6],4))

#subset sylleptic(met level)
syl=met[met$shoot_type=="SYLLEPTIC",]
#2_ % m&v in sylleptic shoots####
v=grep("^v$", colnames(syl))
m=grep("^m$", colnames(syl))

TAB_SYL=met[0,0]
for (i in 1:4) {
  syl_class=levels(syl$class)[i]
  TAB_SYL[i,"class"]=syl_class
  TAB_SYL[i, "nb_v"]=sum(syl[syl$class==syl_class,"v"])
  TAB_SYL[i, "nb_m"]=sum(syl[syl$class==syl_class,"m"])
}
TAB_SYL["sums",-1]=colSums(TAB_SYL[,-1])#sums each observations(obs)
TAB_SYL[,"sums"]=rowSums(TAB_SYL[-1])#sum obserbations per each class

#write pdf with the table
pdf("mv_in_sylleptic~class.pdf",height = 3,width = 5 )
grid.table(TAB_SYL)
dev.off()

#SYLportions of observations (# bud/ tot obs in that class)
qnew=t(round(prop.table(as.matrix(TAB_SYL[5,2:3]),1)*100, 2))
colnames(qnew)="% in sylleptic"
rownames(qnew)=c("%v","%m")

# syl count
png("2c.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(2,3)]
x=barplot(qnew,col = cols, names.arg = rownames(qnew),beside = T, main="frequence buds in sylleptic", xlab = "buds", ylab="%", ylim = c(0,100))
text(x,qnew+3 , paste0(round(qnew, digit=2),"%"), cex = 1)
dev.off()

#sono differenti?
prop.test(as.numeric(TAB_SYL[5,2:3]), rep(TAB_SYL[5,4],2))#no

#3_ % buds in sylleptic shoots####
c=grep("^c$", colnames(syl))
b=grep("^b$", colnames(syl))

TAB_SYL=met[0,0]
for (i in 1:4) {
  syl_class=levels(syl$class)[i]
  TAB_SYL[i,"class"]=syl_class
  TAB_SYL[i, "nb_c"]=sum(syl[syl$class==syl_class,"c"])
  TAB_SYL[i, "nb_v"]=sum(syl[syl$class==syl_class,"v"])
  TAB_SYL[i, "nb_m"]=sum(syl[syl$class==syl_class,"m"])
  TAB_SYL[i, "nb_b"]=sum(syl[syl$class==syl_class,"b"])
}
TAB_SYL["sums",-1]=colSums(TAB_SYL[,-1])#sums each observations(obs)
TAB_SYL[,"sums"]=rowSums(TAB_SYL[-1])#sum obserbations per each class

#SYLportions
qnew=t(round(prop.table(as.matrix(TAB_SYL[1:4,2:5]),1)*100, 2))
colnames(qnew)=TAB_SYL[1:4,1]

#write pdf with the table
pdf("buds_in_sylleptic~class.pdf",height = 3,width = 5 )
grid.table(TAB_SYL)
dev.off()

#NUMERI
png("2d.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(qnew,col = cols, names.arg = colnames(qnew),beside = T, main="frequence buds in sylleptic", xlab = "parent proleptic class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()
#PROBABILITA'
png("2e.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(TAB_SYL[1:4,2:5])),name="Set2")
x<-barplot(as.matrix(t(TAB_SYL[1:4,2:5])),names.arg = TAB_SYL[1:4,1], beside=T,col = cols, main="lateral buds", xlab="parental proleptic class length", ylab="lateral buds(#)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#how many lateral buds develop into shoots?
round((sum(met$number_new_shoots)/(sum(met[13:14]))*100), digit=2)#72.46%gemme germogliate(numero germogli laterali/numero totali gemme laterali M+V)
