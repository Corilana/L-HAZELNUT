setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/Lateral")
#EXPLORATORY ANALYSIS

#importiamo la liberia
library(dplyr)
library(tibble)
library(RColorBrewer)
library(corrplot)
library(rstatix)

#shot level
ann=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/shoot_level_develop_lateralbuds.csv")
#bud level
lat=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/bud_level_LATERALS.csv")
lat <- dplyr::mutate(lat, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
lat <- dplyr::mutate(lat, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))

#how many parentals having at least 1 lateral bud?
length(unique(lat$shoot))# 103
#how many of them have at least 1 lateral child? 
length(unique(lat[lat$new_shoots==1,4]))# 99
#which was the length class of parentals? 
table(unique(lat[lat$new_shoots==1,3:4])[1]) #28LO, 25Me, 21Sh, 25VLo
#what is the length class of new shoots?
table(lat[lat$new_shoots==1,c(3:4,18)][3])# 3Lo, 63Me, 818Sh
#how many new shoots from sylleptic and how many from proleptic?
table(lat[lat$new_shoots==1,c(3:4,16)][3])[2]#303 from buds in Sylleptic 
table(lat[lat$new_shoots==1,c(3:4,16)][3])[1]#582 Proleptic
#how many lateral buds in sylleptic and how many in proleptic?
table(lat[16])#751 in Sylleptic an 822 in Proleptic
#how many M in sylleptic and how many in proleptic
msyl=table(lat[lat$fate=="M",16])[2]#250 in Syl and 385 in Prol
mpro=table(lat[lat$fate=="M",16])[1]
#how many V in sylleptic and how many in proleptic
vsyl=table(lat[lat$fate=="V",16])[2]#272 in Syl and 313 in Prol
vpro=table(lat[lat$fate=="V",16])[1]
vm_syl=msyl+vsyl
vm_pro=mpro+vpro
#how many new shoots from M in sylleptic or proleptic
new_m_syl=table(lat[lat$new_shoots==1&lat$fate=="M",16])[2]#117
new_m_pro=table(lat[lat$new_shoots==1&lat$fate=="M",16])[1]#335
#how many new shoots from V in sylleptic or proleptic
new_v_syl=table(lat[lat$new_shoots==1&lat$fate=="V",16])[2]#165
new_v_pro=table(lat[lat$new_shoots==1&lat$fate=="V",16])[1]#223

new_syl=new_m_syl+new_v_syl
new_pro=new_m_pro+new_v_pro
#% of M or V buds developed in sylleptic and in proleptic
ef_syl=(new_syl/vm_syl)*100#54%
ef_pro=(new_pro/vm_pro)*100#80%

#how many errors? developed from B or C?
new_c=length(lat[lat$new_shoots==1&lat$fate=="C",1])
new_b=length(lat[lat$new_shoots==1&lat$fate=="B",1])
tot_newshoot=length(lat[lat$new_shoots==1,1])

er=((new_b+new_c)/tot_newshoot)*100# 5.08%

#what is the relationship between length of parents (PROLEPTIC) and length of lateral?
PRO=lat[lat$is_in_sylleptic=="NO",]#subset for proleptic
table(unique(PRO[PRO$new_shoots==1,3:4])[1]) #28LO, 22Me, 17Sh, 25VLo
q1=table(PRO$class,PRO$length.newshoots)

#how many lateral buds IN proleptic?
sum(PRO$tot_buds)#1081_gemme laterali

#how many buds per parental class length?
tot=grep("tot_buds", colnames(PRO))
sh=sum(PRO[PRO$class=="Sh",tot])#79 totbuds in parental sh
me=sum(PRO[PRO$class=="Me",tot])#132 totbuds in parental me
lo=sum(PRO[PRO$class=="Lo",tot])#330 totbuds in parental lo
vlo=sum(PRO[PRO$class=="VLo",tot])#540 totbuds in parental VLo

#Question: how many of those were vegetative and mixed?(only one that can burst)
v=grep("^v$", colnames(PRO))
m=grep("^m$", colnames(PRO))
sh_mv=sum(PRO[PRO$class=="Sh",v:m])#22 budsM+ budsV in parental SH
me_mv=sum(PRO[PRO$class=="Me",v:m])#20 budsM+ budsV in parental me
lo_mv=sum(PRO[PRO$class=="Lo",v:m])#63 budsM+ budsV in parental lo
vlo_mv=sum(PRO[PRO$class=="VLo",v:m])#154 budsM+ budsV in parental VLo

#Question: quanti children delle differenti classi di lunghezza, per ogni classe di parentale????
parlen.chillen=as.data.frame.matrix(q1)
colnames(parlen.chillen)=c("child_Sh", "child_Me", "Child_Lo", "Child_VLo")#nomi colonne
rownames(parlen.chillen)=c("parent_Sh", "parent_Me", "parent_Lo", "parent_VLo")#nomi righe
parlen.chillen=add_column(parlen.chillen, parental_lat_shoot=c(17,22,28,25), .before = "child_Sh")#numero parents per ogni categoria
parlen.chillen=add_column(parlen.chillen, tot_parental_buds=c(sh,me,lo,vlo), .before = "child_Sh")#numero gemme laterali per ogni categoria
parlen.chillen=add_column(parlen.chillen, parental_M_V=c(sh_mv,me_mv,lo_mv,vlo_mv), .before = "child_Sh")#numero gemme M+V per ogni categoria
parlen.chillen$sum_child=rowSums(parlen.chillen[4:7])#numero totale laterali per ogni categoria di lunghezza
parlen.chillen["Sums",]=colSums(parlen.chillen)#numero totale laterali
#quali sono le proporzioni PER OGNI CLASSE DI LUNGHEZZA PARENTALE?? (#child/#parentinthatlength)
prop=as.data.frame.matrix(round(prop.table(q1, margin=1)*100, digit=2))
prop

#graph1
png("1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(prop)),name="Set2")
x<-barplot(t(prop),beside= T,col = cols, main="lateral child length from proleptic",xlab= "Parent length (proleptic)", ylab="% child length(es. #child/totalchildSh)", ylim=c(0,110))
legend("top",horiz=T,inset=c(0,-0.02),xpd = TRUE, legend = rownames(t(prop)),fill = cols, cex=0.6)
text(x[1:2,]+0.2, t(prop[,1:2])+3.5, paste(t(prop[,1:2]),"%"), cex = 0.7)
text(x[2,], t(prop[,2])+8.5, c("ab","b", "ab", "a"), cex = 0.7)
dev.off()

#has the medium lateral different proportions according to parental length?
chisq.test(prop[,2])#i rami medi nonsono uguali per tutte le lunghezze
pairwise_chisq_gof_test(prop[,2])#ci sono più medi nei VLO e meno nei ME
