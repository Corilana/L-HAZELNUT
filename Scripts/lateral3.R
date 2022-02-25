setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
#COMPOSIZIONE DATASET

#importiamo la liberia
library(dplyr)
library(RColorBrewer)

#bud level
lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat <- dplyr::mutate(lat, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
lat <- dplyr::mutate(lat, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))
#met level
met=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/met_level_develop_lateralbuds.csv")

syl=met[met$shoot_type=="SYLLEPTIC",]#df just sylleptic
c=grep("^c$", colnames(met))
v=grep("^v$", colnames(met))
m=grep("^m$", colnames(met))
b=grep("^b$", colnames(met))

nuovo=met#df in wich the buds in the sylleptics are not considered
for (q in 1:nrow(nuovo)) {
  if (nuovo[q,c]==1) {
    nuovo[q,c(v,m,b)]=0
  }
  
}
colnames(nuovo)[c]="sylleptic"

# #distribution among rank nodes for every length of annual shoot #all buds ####
# #df with c considered as a bud
# df=data.frame(matrix(nrow=length(unique(met$rank1yo)), ncol=6)) #frequence of observation of parent rank node
# colnames(df)=c("rank_node","#shoots", "#c","#v", "#m", "#b")
# 
# for (i in 1:length(unique(met$rank_node))) {
#   I=unique(met$rank_node)[i]
#   df[i,1]=I
#   df[i,2]=length(met[met$rank_node==I,4])
#   df[i,3]=sum(met[met$rank_node==I,c])
#   df[i,4]=sum(met[met$rank_node==I,v])
#   df[i,5]=sum(met[met$rank_node==I,m])
#   df[i,6]=sum(met[met$rank_node==I,b])
# }
# 
# df$num.oss=rowSums(df[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
# df[24,3:7]=colSums(df[3:7])#tot buds:
# df$mean_buds=round((df[7]/df[2])*100, digit=2)
# df[9]=round((df[3]/df[2])*100, digit=2)
# df[10]=round((df[4]/df[2])*100,digit=2)
# df[11]=round((df[5]/df[2])*100,digit=2)
# df[12]=round((df[6]/df[2])*100,digit=2)
# colnames(df)[9:12]=c("%C", "%V", "%M", "%B")
# df
# 
# #grafico3a
# #_gemme/rango nodo (NB_ tengo i nodi dall' 1 al 17 perchè poi le frequenze di osservazione di quel rango nodo sono minori)
# png("3a.png",width=1200, height=900, res=150)# save plot
# par(mar=c(5,5,3,4))
# col=brewer.pal(n=4,name="Set1")
# with(df[1:17,], plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,max(df[1:17,9:12])), main="freq_buds_for all lengths", xlab="rank nodes", ylab="%", type="o"))
# with(df[1:17,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,type="o"))
# with(df[1:17,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,type="o"))
# with(df[1:17,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,type="o"))
# legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
# dev.off()

#counting c as sylleptic and not counting the buds in sylleptic shoots ####
dfnew=data.frame(matrix(nrow=length(unique(nuovo$rank_node)), ncol=6)) #frequence of observation of parent rank node
colnames(dfnew)=c("rank_node","#shoots", "#sylleptic","#v", "#m", "#b")
sy=grep("^syl", colnames(nuovo))
v=grep("^v$", colnames(nuovo))
m=grep("^m$", colnames(nuovo))
b=grep("^b$", colnames(nuovo))

for (i in 1:length(unique(nuovo$rank_node))) {
  I=unique(nuovo$rank_node)[i]
  dfnew[i,1]=I
  dfnew[i,2]=length(nuovo[nuovo$rank_node==I,4])
  dfnew[i,3]=sum(nuovo[nuovo$rank_node==I,sy])
  dfnew[i,4]=sum(nuovo[nuovo$rank_node==I,v])
  dfnew[i,5]=sum(nuovo[nuovo$rank_node==I,m])
  dfnew[i,6]=sum(nuovo[nuovo$rank_node==I,b])
}

dfnew$num.oss=rowSums(dfnew[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
dfnew[24,3:7]=colSums(dfnew[3:7])#tot buds: #S=197, #V=310, #M=387, #B=123, #BUDS=1017
dfnew$mean_buds=round((dfnew[7]/dfnew[2])*100, digit=2)
dfnew[9]=round((dfnew[3]/dfnew[2])*100, digit=2)
dfnew[10]=round((dfnew[4]/dfnew[2])*100,digit=2)
dfnew[11]=round((dfnew[5]/dfnew[2])*100,digit=2)
dfnew[12]=round((dfnew[6]/dfnew[2])*100,digit=2)
colnames(dfnew)[9:12]=c("%sylleptic", "%V", "%M", "%B")
dfnew

#catkins count as 1 and the buds associated to catkins as 0
png("3aproleptic.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,4))
col=brewer.pal(n=4,name="Set1")
with(dfnew[1:17,], plot(`%sylleptic`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,100), main="freq_buds_,for all lengths, in proleptic parental", xlab="rank nodes", ylab="%", type="o"))
with(dfnew[1:17,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,type="o"))
with(dfnew[1:17,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,type="o"))
with(dfnew[1:17,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,type="o"))
legend("topleft", legend=c("%sylleptic", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
dev.off()

#describing the variability of buds inside sylleptic shoots #### (we can't do that by rank node because is rank node of proleptic)
def_syl=data.frame(matrix(nrow=1, ncol=5)) #frequence of observation of parent rank node
colnames(def_syl)=c("shoots", "#c","#v", "#m", "#b")
sh=grep("^shoot$", colnames(syl))
sy=grep("^c$", colnames(syl))
v=grep("^v$", colnames(syl))
m=grep("^m$", colnames(syl))
b=grep("^b$", colnames(syl))
col=c(sy,v,m,b)

def_syl[1]=length(syl$shoot)
for (i in 2:5) {
  t=col[i-1]
  def_syl[i]=sum(syl[t])
}

#just buds in sylleptic ####
dfnew=data.frame(matrix(nrow=length(unique(syl$rank_node)), ncol=6)) #frequence of observation of parent rank node
colnames(dfnew)=c("rank_node","#shoots", "#c","#v", "#m", "#b")
sy=grep("^c$", colnames(syl))
v=grep("^v$", colnames(syl))
m=grep("^m$", colnames(syl))
b=grep("^b$", colnames(syl))

for (i in 1:length(unique(syl$rank_node))) {
  I=unique(syl$rank_node)[i]
  dfnew[i,1]=I
  dfnew[i,2]=length(syl[syl$rank_node==I,4])
  dfnew[i,3]=sum(syl[syl$rank_node==I,sy])
  dfnew[i,4]=sum(syl[syl$rank_node==I,v])
  dfnew[i,5]=sum(syl[syl$rank_node==I,m])
  dfnew[i,6]=sum(syl[syl$rank_node==I,b])
}

dfnew$num.oss=rowSums(dfnew[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
dfnew[20,3:7]=colSums(dfnew[3:7])#tot buds: #S=197, #V=310, #M=387, #B=123, #BUDS=1017
dfnew$mean_buds=round((dfnew[7]/dfnew[2])*100, digit=2)
dfnew[9]=round((dfnew[3]/dfnew[2])*100, digit=2)
dfnew[10]=round((dfnew[4]/dfnew[2])*100,digit=2)
dfnew[11]=round((dfnew[5]/dfnew[2])*100,digit=2)
dfnew[12]=round((dfnew[6]/dfnew[2])*100,digit=2)
colnames(dfnew)[9:12]=c("%C", "%V", "%M", "%B")
dfnew

dfnew=dfnew[with(dfnew,order(rank_node)),]
#catkins count as 1 and the buds associated to catkins as 0
png("3sylleptic.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,4))
col=brewer.pal(n=4,name="Set1")
with(dfnew, plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,200), main="freq_buds_,for all lengths, in sylleptic", xlab="parental rank nodes", ylab="%", type="o"))
with(dfnew, points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,type="o"))
with(dfnew, points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,type="o"))
with(dfnew, points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,type="o"))
legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
dev.off()

# #distribution among rank nodes for short annual shoot####
# #distribution among rank nodes for medium annual shoot
# #distribution among rank nodes for long annual shoot
# #distribution among rank nodes for Very long annual shoot
# Sh= met[met$class=="Sh",]
# length(unique(Sh$shoot))#25
# Me=met[met$class=="Me",]
# length(unique(Me$shoot))#25
# Lo=met[met$class=="Lo",]
# length(unique(Lo$shoot))#28
# VLo=met[met$class=="VLo",]
# length(unique(VLo$shoot))#25
# 
# #sh
# df1=data.frame(matrix(nrow=length(unique(Sh$rank_node)), ncol=6)) #frequence of observation of parent rank node
# df1[1]=unique(Sh$rank_node)
# colnames(df1)=c("rank_node","#shoots", "#c","#v", "#m", "#b")
# 
# c=grep("^c$", colnames(Sh))
# v=grep("^v$", colnames(Sh))
# m=grep("^m$", colnames(Sh))
# b=grep("^b$", colnames(Sh))
# for (i in 1:length(unique(Sh$rank_node))) {
#   I=unique(Sh$rank_node)[i]
#   df1[df1$rank_node==I,2]=length(Sh[Sh$rank_node==I,5])
#   df1[df1$rank_node==I,3]=sum(Sh[Sh$rank_node==I,c])
#   df1[df1$rank_node==I,4]=sum(Sh[Sh$rank_node==I,v])
#   df1[df1$rank_node==I,5]=sum(Sh[Sh$rank_node==I,m])
#   df1[df1$rank_node==I,6]=sum(Sh[Sh$rank_node==I,b])
# }
# 
# df1$num.oss=rowSums(df1[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
# df1[nrow(df1)+1,3:7]=colSums(df1[3:7])
# df1$mean_buds=round((df1[7]/df1[2])*100, digit=2)
# df1[9]=round((df1[3]/df1[2])*100, digit=2)
# df1[10]=round((df1[4]/df1[2])*100,digit=2)
# df1[11]=round((df1[5]/df1[2])*100,digit=2)
# df1[12]=round((df1[6]/df1[2])*100,digit=2)
# colnames(df1)[9:12]=c("%C", "%V", "%M", "%B")
# 
# df1[6,7]#107 number of Sh buds
# 
# #grafico3b
# png("3b.png",width=1200, height=900, res=150)# save plot
# par(mar=c(5,5,3,4))
# col=brewer.pal(n=4,name="Set1")
# with(df1[1:4,], plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,max(df1[1:4,9:12])), main="freq_buds_Sh shoots", xlab="rank nodes", ylab="%", class="o"))
# with(df1[1:4,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,class="o"))
# with(df1[1:4,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,class="o"))
# with(df1[1:4,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,class="o"))
# legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
# dev.off()
# 
# #Me
# df2=data.frame(matrix(nrow=length(unique(Me$rank_node)), ncol=6)) #frequence of observation of parent rank node
# df2[1]=unique(Me$rank_node)
# colnames(df2)=c("rank_node","#shoots", "#c","#v", "#m", "#b")
# c=grep("^c$", colnames(Me))
# v=grep("^v$", colnames(Me))
# m=grep("^m$", colnames(Me))
# b=grep("^b$", colnames(Me))
# for (i in 1:length(unique(Me$rank_node))) {
#   I=unique(Me$rank_node)[i]
#   df2[df2$rank_node==I,2]=length(Me[Me$rank_node==I,3])
#   df2[df2$rank_node==I,3]=sum(Me[Me$rank_node==I,c])
#   df2[df2$rank_node==I,4]=sum(Me[Me$rank_node==I,v])
#   df2[df2$rank_node==I,5]=sum(Me[Me$rank_node==I,m])
#   df2[df2$rank_node==I,6]=sum(Me[Me$rank_node==I,b])
# }
# 
# df2$num.oss=rowSums(df2[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
# df2[nrow(df2)+1,3:7]=colSums(df2[3:7])#tot buds: #C=39, #V=83, #M=96, #B=37, #BUDS=255
# df2$mean_buds=round((df2[7]/df2[2])*100, digit=2)
# df2[9]=round((df2[3]/df2[2])*100, digit=2)
# df2[10]=round((df2[4]/df2[2])*100,digit=2)
# df2[11]=round((df2[5]/df2[2])*100,digit=2)
# df2[12]=round((df2[6]/df2[2])*100,digit=2)
# colnames(df2)[9:12]=c("%C", "%V", "%M", "%B")
# 
# df2[9,7]#271 number of me buds
# 
# #grafico3c
# png("3c.png",width=1200, height=900, res=150)# save plot
# col=brewer.pal(n=4,name="Set1")
# with(df2[1:6,], plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,max(df2[1:6,9:12])), main="freq_buds_Me shoots", xlab="rank nodes", ylab="%", class="o"))
# with(df2[1:6,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,class="o"))
# with(df2[1:6,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,class="o"))
# with(df2[1:6,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,class="o"))
# legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
# dev.off()
# 
# #Lo
# df3=data.frame(matrix(nrow=length(unique(Lo$rank_node)), ncol=6)) #frequence of observation of parent rank node
# df3[1]=unique(Lo$rank_node)
# colnames(df3)=c("rank_node","#shoots", "#c","#v", "#m", "#b")
# c=grep("^c$", colnames(Lo))
# v=grep("^v$", colnames(Lo))
# m=grep("^m$", colnames(Lo))
# b=grep("^b$", colnames(Lo))
# for (i in 1:length(unique(Lo$rank_node))) {
#   I=unique(Lo$rank_node)[i]
#   df3[df3$rank_node==I,2]=length(Lo[Lo$rank_node==I,3])
#   df3[df3$rank_node==I,3]=sum(Lo[Lo$rank_node==I,c])
#   df3[df3$rank_node==I,4]=sum(Lo[Lo$rank_node==I,v])
#   df3[df3$rank_node==I,5]=sum(Lo[Lo$rank_node==I,m])
#   df3[df3$rank_node==I,6]=sum(Lo[Lo$rank_node==I,b])
# }
# 
# df3$num.oss=rowSums(df3[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
# df3[nrow(df3)+1,3:7]=colSums(df3[3:7])#tot buds: #C=80, #V=193, #M=230, #B=36, #BUDS=539
# df3$mean_buds=round((df3[7]/df3[2])*100, digit=2)
# df3[9]=round((df3[3]/df3[2])*100, digit=2)
# df3[10]=round((df3[4]/df3[2])*100,digit=2)
# df3[11]=round((df3[5]/df3[2])*100,digit=2)
# df3[12]=round((df3[6]/df3[2])*100,digit=2)
# colnames(df3)[9:12]=c("%C", "%V", "%M", "%B")
# 
# df3[17,7]#549 number of lo buds
# 
# #grafico3d
# png("3d.png",width=1200, height=900, res=150)# save plot
# par(mar=c(5,5,3,4))
# col=brewer.pal(n=4,name="Set1")
# with(df3[1:12,], plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,max(df3[1:12,9:12])), main="freq_buds_Lo shoots", xlab="rank nodes", ylab="%", class="o"))
# with(df3[1:12,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,class="o"))
# with(df3[1:12,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,class="o"))
# with(df3[1:12,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,class="o"))
# legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
# dev.off()
# 
# #VLo
# df4=data.frame(matrix(nrow=length(unique(VLo$rank_node)), ncol=6)) #frequence of observation of parent rank node
# df4[1]=unique(VLo$rank_node)
# colnames(df4)=c("rank_node","#shoots", "#c","#v", "#m", "#b")
# c=grep("^c$", colnames(VLo))
# v=grep("^v$", colnames(VLo))
# m=grep("^m$", colnames(VLo))
# b=grep("^b$", colnames(VLo))
# for (i in 1:length(unique(VLo$rank_node))) {
#   I=unique(VLo$rank_node)[i]
#   df4[df4$rank_node==I,2]=length(VLo[VLo$rank_node==I,3])
#   df4[df4$rank_node==I,3]=sum(VLo[VLo$rank_node==I,c])
#   df4[df4$rank_node==I,4]=sum(VLo[VLo$rank_node==I,v])
#   df4[df4$rank_node==I,5]=sum(VLo[VLo$rank_node==I,m])
#   df4[df4$rank_node==I,6]=sum(VLo[VLo$rank_node==I,b])
# }
# 
# df4$num.oss=rowSums(df4[3:6])#inserisco la somma di tutte le gemme per quel rango nodo
# df4[nrow(df4)+1,3:7]=colSums(df4[3:7])#tot buds: #C=66, #V=259, #M=270, #B=33, #BUDS=628
# df4$mean_buds=round((df4[7]/df4[2])*100, digit=2)
# df4[9]=round((df4[3]/df4[2])*100, digit=2)
# df4[10]=round((df4[4]/df4[2])*100,digit=2)
# df4[11]=round((df4[5]/df4[2])*100,digit=2)
# df4[12]=round((df4[6]/df4[2])*100,digit=2)
# colnames(df4)[9:12]=c("%C", "%V", "%M", "%B")
# 
# df4[24,7]#646 number of VLo buds
# 
# #grafico3f
# png("3f.png",width=1200, height=900, res=150)# save plot
# col=brewer.pal(n=4,name="Set1")
# par(mar=c(5,5,3,4))
# with(df4[1:17,], plot(`%C`~rank_node, pch=15, cex=1.2,col=col[1],ylim = c(0,max(df4[1:17,9:12])), main="freq_buds_VLo shoots", xlab="rank nodes", ylab="%", class="o"))
# with(df4[1:17,], points(`%V`~rank_node, pch=16, col=col[2],cex=1.2,class="o"))
# with(df4[1:17,], points(`%M`~rank_node, pch=17, col=col[3], cex=1.2,class="o"))
# with(df4[1:17,], points(`%B`~rank_node, pch=18, col=col[4], cex=1.2,class="o"))
# legend("topleft", legend=c("%C", "%V", "%M", "%B"), lwd=3, cex= 0.8, col=col)
# dev.off()

