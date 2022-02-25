setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral/")
#COMPOSIZIONE DATASET

#importiamo la liberia
library(dplyr)
library(RColorBrewer)
library(rstatix)
library(corrplot)

#met.scale
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
c=grep("^c$", colnames(met))
v=grep("^v$", colnames(met))
m=grep("^m$", colnames(met))
b=grep("^b$", colnames(met))

#catkins are sylleptic
syl=met[met$shoot_type=="SYLLEPTIC",]#new df just for sylleptic shoots

nuovo=met#new metamer level df to delete values buds in sylleptic shoots
for (q in 1:nrow(nuovo)) {
  if (nuovo[q,c]==1) {
    nuovo[q,c(m,v,b)]=0#trasform to zero the values of the buds in sylleptic
  }

}
colnames(nuovo)[c]="sylleptic"#CHANGE THE COLUMN "C" WITH SYLLEPTIC

#1_type of lateral buds ~ length parent
ann=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/shoot_level_develop_lateralbuds.csv")
ann <- dplyr::mutate(ann, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#IN PROLEPTIC (no not count buds INSIDE sylleptic) (df:nuovo)
#1c_lateral buds in proleptic (the rank with sylleptic is counted as sylleptic)
dfpro=data.frame(matrix(ncol=9, nrow=length(unique(sort(nuovo$class)))))
colnames(dfpro)=c("#sylleptic", "#V","#M", "#B","sum", "sylleptic", "v", "m", "b")
rownames(dfpro)=unique(sort(nuovo$class))#put as rows the parental class length
sy=grep("^syl", colnames(nuovo))
v=grep("^v$", colnames(nuovo))
m=grep("^m$", colnames(nuovo))
b=grep("^b$", colnames(nuovo))
for (q in 1:length(unique(sort(nuovo$class)))) {
  Q=unique(sort(nuovo$class))[q]
  dfpro[q,1]=sum(nuovo[nuovo$class==Q,sy])#how many sylleptic for each parental class?
  dfpro[q,2]=sum(nuovo[nuovo$class==Q,v])#how many v for each parental class?
  dfpro[q,3]=sum(nuovo[nuovo$class==Q,m])#how many m for each parental class?
  dfpro[q,4]=sum(nuovo[nuovo$class==Q,b])#how many b for each parental class?
}

#1c
dfpro$sum=rowSums(dfpro[1:4])
dfpro["Sum",1:5]=colSums(dfpro[1:5])
dfpro[1,6:9]=round((dfpro[1,1:4]/dfpro[1,5])*100,digit=2)
dfpro[2,6:9]=round((dfpro[2,1:4]/dfpro[2,5])*100,digit=2)
dfpro[3,6:9]=round((dfpro[3,1:4]/dfpro[3,5])*100,digit=2)
dfpro[4,6:9]=round((dfpro[4,1:4]/dfpro[4,5])*100,digit=2)
dfpro#nb_ sum of bud is not 1594 but 1051 because buds INSIDE THE SYLLEPTIC are not counted

qnew=as.matrix(t(dfpro[c(6:9)][1:4,]))#df delle sole probabilità
qnew

#grafico2c
png("2c.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(qnew,col = cols, names.arg = colnames(qnew), main="frequence lateral buds class in proleptic parental", xlab = "parent class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#sylcount
q1new=as.matrix(as.data.frame((dfpro[5,1:4]/dfpro[5,5])*100))
colnames(q1new)=c("sylleptic","v","m", "b")
q1new

# syl count
png("2csylcount.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(q1new[1:4],col = cols, names.arg = colnames(q1new), main="frequence lateral buds/sylleptic in proleptic parental", xlab = "buds/sylleptic", ylab="%", ylim = c(0,100))
text(x,q1new+3 , paste0(round(q1new, digit=2),"%"), cex = 1)
text(x,q1new+10, c("ab","ab","a","b"), cex = 1)
dev.off()

chisq.test(q1new[1:4])
pairwise_chisq_gof_test(q1new[1:4])

znew=t(as.matrix(as.data.frame(dfpro[c(1:4)][1:4,])))#df dei soli numeri
znew

png("2bsylcount.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(znew)),name="Set2")
x<-barplot(znew, beside=T,col = cols, main="lateral buds or shoot class", xlab="parental class length", ylab="lateral buds or shoot class (#)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

#IN SYLLEPTIC (IT DOESNT'HAVE MUCH SENSE BECAUSE I DO NOT HAVE THE LENGTH OF SYLLEPTIC)
#1a_all lateral buds (catkins considered as a bud)####
latbuds_syl=data.frame(matrix(ncol=5, nrow=1))
colnames(latbuds_syl)=c("#V","#M", "sum", "v", "m")
rownames(latbuds_syl)="tot"

v=grep("^v$", colnames(ann))
m=grep("^m$", colnames(ann))
u=c(v,m)

for (q in 1:length(u)) {
  Q=u[q]
  latbuds_syl[q]=sum(syl[Q])
}

#1a
latbuds_syl$sum=rowSums(latbuds_syl[1:2])
latbuds_syl[1,4:5]=round((latbuds_syl[1,1:2]/latbuds_syl[1,3])*100,digit=2)
latbuds_syl

q=as.matrix(as.data.frame(latbuds_syl[c(4:5)]))#df delle sole probabilità
q

# syl count
png("2insidesyl.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(2,3)]
x=barplot(q[1:2],col = cols, names.arg = colnames(q), main="frequence buds in sylleptic", xlab = "buds", ylab="%", ylim = c(0,100))
text(x,q+3 , paste0(round(q, digit=2),"%"), cex = 1)
dev.off()

#how many for each parental class?
dfpro=data.frame(matrix(ncol=9, nrow=length(unique(sort(syl$class)))))
colnames(dfpro)=c("#c", "#V","#M", "#B","sum", "c", "v", "m", "b")
rownames(dfpro)=unique(sort(syl$class))#put as rows the parental class length
sy=grep("^c$", colnames(syl))
v=grep("^v$", colnames(syl))
m=grep("^m$", colnames(syl))
b=grep("^b$", colnames(syl))
for (q in 1:length(unique(sort(syl$class)))) {
  Q=unique(sort(syl$class))[q]
  dfpro[q,1]=sum(syl[syl$class==Q,sy])#how many c for each parental class?
  dfpro[q,2]=sum(syl[syl$class==Q,v])#how many v for each parental class?
  dfpro[q,3]=sum(syl[syl$class==Q,m])#how many m for each parental class?
  dfpro[q,4]=sum(syl[syl$class==Q,b])#how many b for each parental class?
}
#1c
dfpro$sum=rowSums(dfpro[1:4])
dfpro["Sum",1:5]=colSums(dfpro[1:5])
dfpro[1,6:9]=round((dfpro[1,1:4]/dfpro[1,5])*100,digit=2)
dfpro[2,6:9]=round((dfpro[2,1:4]/dfpro[2,5])*100,digit=2)
dfpro[3,6:9]=round((dfpro[3,1:4]/dfpro[3,5])*100,digit=2)
dfpro[4,6:9]=round((dfpro[4,1:4]/dfpro[4,5])*100,digit=2)
dfpro#nb_ sum of bud is not 1594 but 1051 because buds INSIDE THE SYLLEPTIC are not counted

qnew=as.matrix(t(dfpro[c(6:9)][1:4,]))#df delle sole probabilità
qnew

#grafico2c
png("2_sillept.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(qnew,col = cols, names.arg = colnames(qnew), main="frequence buds in sylleptic", xlab = "parent proleptic class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()

znew=t(as.matrix(as.data.frame(dfpro[c(1:4)][1:4,])))#df dei soli numeri
znew

png("2_sillept_numb.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(znew)),name="Set2")
x<-barplot(znew, beside=T,col = cols, main="lateral buds", xlab="parental proleptic class length", ylab="lateral buds(#)")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(qnew),fill = cols, cex=0.8)
dev.off()


#how many lateral buds develop into shoots?
round((sum(met$number_new_shoots)/(sum(met[8:9]))*100), digit=2)#72.54%gemme germogliate(numero germogli laterali/numero totali gemme laterali M+V)