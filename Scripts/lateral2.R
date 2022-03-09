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
#subset sylleptic(met level)
syl=met[met$shoot_type=="SYLLEPTIC",]

#in proleptic df, we have to put the observations inside the sylleptic as 0
#and indicate the presence/absence of sylleptic shoots, at that node, with 0(abs) or 1(pres)
proleptic=met#copy df metamer level
for (q in 1:nrow(proleptic)) {
  if (proleptic[q,c]==1) {
    proleptic[q,c(m,v,b)]=0#trasform to zero the values of the buds in sylleptic
  }

}
colnames(proleptic)[c]="sylleptic"#CHANGE THE name of COLUMN "C" WITH "SYLLEPTIC"

sy=grep("^syl", colnames(proleptic))
v=grep("^v$", colnames(proleptic))
m=grep("^m$", colnames(proleptic))
b=grep("^b$", colnames(proleptic))

#1_ % lateral buds/syllepotic in proleptic shoots####
#) (-->df:proleptic)

TAB_PRO=met[0,0]#empty df

#create a table to store the frequence of each obs in the shoots
nline=length(unique(sort(proleptic$class)))
for (q in 1:nline) {
  Q=unique(sort(proleptic$class))[q]
  TAB_PRO[paste0(Q),"sylleptic"]=sum(proleptic[proleptic$class==Q,sy])#how many sylleptic for each parental class?
  TAB_PRO[paste0(Q),"v"]=sum(proleptic[proleptic$class==Q,v])#how many v for each parental class?
  TAB_PRO[paste0(Q),"m"]=sum(proleptic[proleptic$class==Q,m])#how many m for each parental class?
  TAB_PRO[paste0(Q),"b"]=sum(proleptic[proleptic$class==Q,b])#how many b for each parental class?
}

TAB_PRO["sums",]=colSums(TAB_PRO)#sums each observations(obs)
TAB_PRO[,"sums"]=rowSums(TAB_PRO)#sum obserbations per each class

#proportions of observations (# bud/ tot obs in that class)
for (i in 1:4) {TAB_PRO[i,6:9]=round((TAB_PRO[i,1:4]/TAB_PRO[i,5])*100,digit=2)}
colnames(TAB_PRO)[6:9]=c("%sylleptic","%v","%m","%b")
TAB_PRO$"%sums"=round(rowSums(TAB_PRO[6:9]), digits = 0)

#write pdf with the table
pdf("lat_obs_in_pro_shoots~class.pdf",height = 3,width = 5 )
grid.table(TAB_PRO[1:4,6:10])
dev.off()

qnew=as.matrix(t(TAB_PRO[c(6:9)][1:4,]))#df delle sole probabilità
qnew

#grafico2c
png("2.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(qnew,col = cols, names.arg = colnames(qnew), main="frequence lateral buds class in proleptic parental", xlab = "parent class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#sylcount
q1new=as.matrix(as.data.frame((TAB_PRO[5,1:4]/TAB_PRO[5,5])*100))
colnames(q1new)=c("sylleptic","v","m", "b")
q1new

#graph%
png("2a.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(q1new[1:4],col = cols, names.arg = colnames(q1new), main="frequence lateral buds/sylleptic in proleptic parental", xlab = "buds/sylleptic", ylab="%", ylim = c(0,100))
text(x,q1new+3 , paste0(round(q1new, digit=2),"%"), cex = 1)
text(x,q1new+10, c("ab","ab","a","b"), cex = 1)
dev.off()

chisq.test(q1new[1:4])
pairwise_chisq_gof_test(q1new[1:4])

znew=t(as.matrix(as.data.frame(TAB_PRO[c(1:4)][1:4,])))#df dei soli numeri
znew

png("2b.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(znew)),name="Set2")
x<-barplot(znew, beside=T,col = cols, main="lateral buds or shoot class", xlab="parental class length", ylab="lateral buds or shoot class (#)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#2_ % m&v in sylleptic shoots####
v=grep("^v$", colnames(syl))
m=grep("^m$", colnames(syl))

TAB_SYL=met[0,0]

nline=length(unique(sort(syl$class)))
for (q in 1:nline) {
  Q=unique(sort(syl$class))[q]
  TAB_SYL[paste0(Q),"v"]=sum(syl[syl$class==Q,v])#how many v for each parental class?
  TAB_SYL[paste0(Q),"m"]=sum(syl[syl$class==Q,m])#how many m for each parental class?
}

TAB_SYL["sums",]=colSums(TAB_SYL)#sums each m/v(obs)
TAB_SYL[,"sums"]=rowSums(TAB_SYL)#sum m/v per each class

#proportions
for (i in 1:4) {TAB_SYL[i,4:5]=round((TAB_SYL[i,1:2]/TAB_SYL[i,3])*100,digit=2)}
colnames(TAB_SYL)[4:5]=c("%v","%m")
TAB_SYL$"%sums"=round(rowSums(TAB_SYL[4:5]), digits = 0)

#write pdf with the table
pdf("mv_in_sylleptic~class.pdf",height = 3,width = 5 )
grid.table(TAB_SYL[1:4,4:6])
dev.off()

q=as.matrix(as.data.frame(TAB_SYL[c(4:5)]))#df delle sole probabilità
q

# syl count
png("2c.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(2,3)]
x=barplot(q[1:2],col = cols, names.arg = colnames(q), main="frequence buds in sylleptic", xlab = "buds", ylab="%", ylim = c(0,100))
text(x,q+3 , paste0(round(q, digit=2),"%"), cex = 1)
dev.off()

#3_ % buds in sylleptic shoots####
c=grep("^c$", colnames(syl))
b=grep("^b$", colnames(syl))

TAB_SYL=met[0,0]

nline=length(unique(sort(syl$class)))
for (q in 1:nline) {
  Q=unique(sort(syl$class))[q]
  TAB_SYL[paste0(Q),"c"]=sum(syl[syl$class==Q,c])#how many c for each parental class?
  TAB_SYL[paste0(Q),"v"]=sum(syl[syl$class==Q,v])#how many v for each parental class?
  TAB_SYL[paste0(Q),"m"]=sum(syl[syl$class==Q,m])#how many m for each parental class?
  TAB_SYL[paste0(Q),"b"]=sum(syl[syl$class==Q,b])#how many b for each parental class?
}

TAB_SYL["sums",]=colSums(TAB_SYL)
TAB_SYL[,"sums"]=rowSums(TAB_SYL)

#SYLportions
for (i in 1:4) {TAB_SYL[i,6:9]=round((TAB_SYL[i,1:4]/TAB_SYL[i,5])*100,digit=2)}
colnames(TAB_SYL)[6:9]=c("%c","%v","%m","%b")
TAB_SYL$"%sums"=round(rowSums(TAB_SYL[6:9]), digits = 0)

#write pdf with the table
pdf("buds_in_sylleptic~class.pdf",height = 3,width = 5 )
grid.table(TAB_SYL[1:4,4:6])
dev.off()

q=as.matrix(as.data.frame(TAB_SYL[c(6:9)]))#df delle sole probabilità
q

qnew=as.matrix(t(TAB_SYL[c(6:9)][1:4,]))#df delle sole probabilità
qnew

#grafico2c
png("2d.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(qnew,col = cols, names.arg = colnames(qnew), main="frequence buds in sylleptic", xlab = "parent proleptic class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

znew=t(as.matrix(as.data.frame(TAB_SYL[c(1:4)][1:4,])))#df dei soli numeri
znew

png("2d.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(znew)),name="Set2")
x<-barplot(znew, beside=T,col = cols, main="lateral buds", xlab="parental proleptic class length", ylab="lateral buds(#)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#how many lateral buds develop into shoots?
round((sum(met$number_new_shoots)/(sum(met[10:11]))*100), digit=2)#72.54%gemme germogliate(numero germogli laterali/numero totali gemme laterali M+V)
