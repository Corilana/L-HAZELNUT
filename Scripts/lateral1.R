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

#shot level
ann=read.csv(paste0(wd, "DF/auto/shoot_level_develop_lateralbuds.csv"))
#bud level
lat=read.csv(paste0(wd, "DF/auto/mtp use/bud_level_LATERALS.csv"))
lat$class=factor(lat$class, levels=c("Sh", "Me", "Lo", "VLo"))
lat$length.newshoots=factor(lat$length.newshoots, levels=c("Sh", "Me", "Lo", "VLo"))

#store index column "shoot id"
s=grep("^shoot$",colnames(lat))
#store index column "parent class"
c=grep("^class$",colnames(lat))
#store index column "length parent"
lpar=grep("^Length$",colnames(lat))
#store index column "length new shots"
lnew=grep(".new",colnames(lat))
#store index column "fate parent bud"
f=grep("fate$",colnames(lat))


#1Parental class
TAB1=cbind(as.data.frame(table(unique(lat[c:s])[1])),#nb_parental shoot per class
           (as.data.frame(table(lat[c]))[2]),##nb_buds in parentals per class
           (as.data.frame(table(unique(lat[lat$new_shoots!=0,c:s])[1]))[2]),#nb_ parental with children per class
           (as.data.frame(table(lat[lat$new_shoots!=0,c(c,f)][1]))[2]),#nb_buds in parentals with children per class
           as.data.frame(table(lat[lat$new_shoots!=0,c(c:s,lnew)][3]))#nb_children per class
)
colnames(TAB1)=c("Class","parental_freq","tot_lat_buds","parental_with_children_freq","tot_buds_in_parent_with_child","child_class","children_freq")
TAB1["Sum",c(2:5,7)]=colSums(TAB1[c(2:5,7)])

#write pdf with the table
pdf("class_frequences.pdf",height = 4,width = 13 )
grid.table(TAB1)
dev.off()

#store index column "type of parent shoot"
p=grep("from_$",colnames(lat))

#table with info regerding parent type
TAB2=cbind(as.data.frame(c("proleptic","sylleptic")),
          (as.data.frame(table(lat[lat$new_shoots!=0,c(c:s,p)][3]))[2]),#CHILDS from sylleptic(2) or proleptic(1) 
          (as.data.frame(table(lat[lat$fate=="M",p]))[2]),#M from sylleptic(2) or proleptic(1)
          (as.data.frame(table(lat[lat$fate=="V",p]))[2]),#V from sylleptic(2) or proleptic(1)
          (as.data.frame(table(lat[lat$new_shoots!=0&lat$fate=="M",p]))[2]),#childs from M
          (as.data.frame(table(lat[lat$new_shoots!=0&lat$fate=="V",p]))[2]),#childs from V
          (as.data.frame(table(lat[lat$new_shoots!=0&lat$fate=="C",p]))[2]),#childs from C
          (as.data.frame(table(lat[lat$new_shoots!=0&lat$fate=="B",p]))[2])#childs from B
          )
colnames(TAB2)=c("Shoot_type",
                 "#child","#M_buds","#V_buds",
                 "childs_from_M","childs_from_V",
                 "childs_from_C","childs_from_B")
C=grep("C", colnames(TAB2))
B=grep("B", colnames(TAB2))
if(TAB2[TAB2$Shoot_type=="proleptic",C]!=0){TAB2[TAB2$Shoot_type=="proleptic",C]=0}#there are no catkins in proleptic
if(TAB2[TAB2$Shoot_type=="sylleptic",B]!=0){TAB2[TAB2$Shoot_type=="sylleptic",B]=0}#there are no blinds in sylleptic
TAB2["Sum",2:8]=colSums(TAB2[2:8])
TAB2$mv=rowSums(TAB2[3:4])
TAB2$childs_mv=rowSums(TAB2[5:6])
TAB2$ef=round(TAB2$childs_mv/TAB2$mv, digits = 2)
TAB2$err[3]=round(sum(TAB2[3,7:8])/TAB2[3,2], digits = 2)

#write pdf with the table
pdf("type_shoots.pdf", height = 5,width = 12)
grid.table(TAB2)
dev.off()

#childs from PROLEPTIC
#what is the relationship between length of parents (PROLEPTIC) and length of lateral?
PRO=lat[lat$from_=="PROL",]#subset for buds in proleptic shoots
f=grep("fate", colnames(PRO))
TAB_P=as.data.frame(table(unique(PRO[PRO$new_shoots!=0,c:s])[1]))#class_proleptic
colnames(TAB_P)=c("proleptic_class", "parental_frequence")

TAB_P$MV=NA
TAB_P$TOT_BUDS=NA

for (i in 1:4) {
  I=TAB_P$proleptic_class[i]
  tt=length(PRO[PRO$class==I,f])#totbuds in parental
  mv=length(grep(paste0("V","|","M"),PRO[PRO$class==I,f]))#mv in parental
  TAB_P[i,3]=mv
  TAB_P[TAB_P$proleptic_class==I,4]=tt
}
          
TAB_P["Sums",2:4]=colSums(TAB_P[2:4])

#parent length ~ child length
par_child=as.data.frame.matrix(table(PRO$class,PRO$length.newshoots))
colnames(par_child)=c("child_Sh", "child_Me", "Child_Lo", "Child_VLo")#nomi colonne
rownames(par_child)=c("parent_Sh", "parent_Me", "parent_Lo", "parent_VLo")#nomi righe
par_child=add_column(par_child, freq_class=TAB_P[1:4,2], .before = "child_Sh")#numero parents per ogni categoria
par_child=add_column(par_child, tot_parental_buds=TAB_P[1:4,4], .before = "child_Sh")#numero gemme laterali per ogni categoria
par_child=add_column(par_child, parental_M_V=TAB_P[1:4,3], .before = "child_Sh")#numero gemme M+V per ogni categoria
par_child$sum_child=rowSums(par_child[4:7])#numero totale laterali per ogni categoria di lunghezza
par_child["Sums",]=colSums(par_child)#numero totale laterali

#write pdf with the table
pdf("childs_from_proleptic.pdf", height = 5,width = 12)
grid.table(par_child)
dev.off()

#childs from SYLLEPTIC
#what is the relationship between length of parents (SYLLEPTIC) and length of lateral?
SYL=lat[lat$from_=="SYL",]#subset for buds in sylleptic shoots
f=grep("fate", colnames(SYL))
TAB_P=as.data.frame(table(unique(SYL[SYL$new_shoots!=0,c:s])[1]))#class_sylleptic
colnames(TAB_P)=c("sylleptic_class", "parental_frequence")

TAB_P$MV=NA
TAB_P$TOT_BUDS=NA

for (i in 1:4) {
  I=TAB_P$sylleptic_class[i]
  tt=length(SYL[SYL$class==I,f])#totbuds in parental
  mv=length(grep(paste0("V","|","M"),SYL[SYL$class==I,f]))#mv in parental
  TAB_P[i,3]=mv
  TAB_P[TAB_P$sylleptic_class==I,4]=tt
}

TAB_P["Sums",2:4]=colSums(TAB_P[2:4])

#parent length ~ child length
par_child=as.data.frame.matrix(table(SYL$class,SYL$length.newshoots))
colnames(par_child)=c("child_Sh", "child_Me", "Child_Lo", "Child_VLo")#nomi colonne
rownames(par_child)=c("parent_Sh", "parent_Me", "parent_Lo", "parent_VLo")#nomi righe
par_child=add_column(par_child, freq_class=TAB_P[1:4,2], .before = "child_Sh")#numero parents per ogni categoria
par_child=add_column(par_child, tot_parental_buds=TAB_P[1:4,4], .before = "child_Sh")#numero gemme laterali per ogni categoria
par_child=add_column(par_child, parental_M_V=TAB_P[1:4,3], .before = "child_Sh")#numero gemme M+V per ogni categoria
par_child$sum_child=rowSums(par_child[4:7])#numero totale laterali per ogni categoria di lunghezza
par_child["Sums",]=colSums(par_child)#numero totale laterali

#write pdf with the table
pdf("childs_from_sylleptic.pdf", height = 5,width = 12)
grid.table(par_child)
dev.off()

#CLASS FREQ  (#child/#parentinthatlength)
nb=as.matrix(table(PRO$class,PRO$length.newshoots))
prop=as.data.frame.matrix(round(prop.table(table(PRO$class,PRO$length.newshoots), margin=1)*100, digit=2))
prop

#graph1
png("1.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(prop)),name="Set2")
x<-barplot(t(prop),beside= T,col = cols, main="lateral child length from proleptic",xlab= "Parent length (proleptic)", ylab="% child length(es. #child/totalchildSh)", ylim=c(0,110))
legend("top",horiz=T,inset=c(0,-0.02),xpd = TRUE, legend = rownames(t(prop)),fill = cols, cex=0.6)
text(x[1:2,]+0.2, t(prop[,1:2])+3.5, paste(t(prop[,1:2]),"%"), cex = 0.7)
text(x[2,], t(prop[,2])+8.5, c("ab","b", "ab", "a"), cex = 0.7)
dev.off()

#is sh child proportion different according to parental length?
prop.test(x = nb[,1], rowSums(nb))
pairwise.prop.test(x = nb[,1], rowSums(nb), p.adjust.method = "none")# VLo è diverso da me e da lo
#is me child proportion different according to parental length?
prop.test(x = nb[,2], rowSums(nb))
pairwise.prop.test(x = nb[,2], rowSums(nb), p.adjust.method = "none")# VLo è diverso da me e da lo
