#21/10/2022:
#overwriting "bud_level_developed" dataframe:
#1_ choosing the type of the central bud and
#2_ adding the number of sibling buds
#3_ 
#4_adding the position of the central bud (apical or lateral is given).
#when multiple child per node, for the APICAL, I choose the longest)

setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")

library(plyr)
library(dplyr)
library("stringr")

met.level<-read.csv("2020metamer_level_DEVELOPED.csv")
newshot.level<-read.csv("DFAUTO_new.shootlevel.csv")
newshot.level=newshot.level[newshot.level$fate!="?",] #remove those who had more than 1 shoot but i recorded just one buds!!!
newshot.level=newshot.level[1:22]
bud.level<-read.csv("bud_level_develop.csv")
# tot.buds=sum(met.level$tot_buds)#1677
# tot.newshoots=sum(met.level$number_new_shoots)#1018

#SCRIPT TO CHECK IF SOME NEW SHOOT WAS IN A SYLLEPTIC####
# #1_is the new shoot developed from a sylleptic?
# newshot.level$from_sylleptic=NA
# nline=length(newshot.level$thesis)
# 
# for(i in 1:nline){
#   if(newshot.level$c[i]>=1){
#     newshot.level$from_sylleptic[i]="YES"}
#   else{newshot.level$from_sylleptic[i]="NO"}
#   
# }
# 
# #2_is the new shoot at the highest node?
# newshot.level$position=NA
# nline=length(newshot.level$thesis)
# 
# for(i in 1:nline){
#   s=newshot.level$shoot1yo[i]
#   ra=grep("^ran", colnames(newshot.level))
#   max=max(newshot.level[newshot.level$shoot1yo==s,ra])
#   if(newshot.level$rank1yo[i]==max){
#     newshot.level$position[i]="AP"}
#   else{newshot.level$position[i]="LATERAL"}
#   
# }
# 
# #3_ ARE THERE ANY APICALS IN SYLLEPTIC?
# ap_syl=newshot.level[newshot.level$from_sylleptic=="YES"&newshot.level$position=="AP",]
# #yes there are. Thus, I need to extract from those sylleptic the bud the apical bud that generated the succession

#_create new df in wich each line is a single bud
m.df=data.frame(matrix(ncol = 0, nrow=0))#create an empty dataframe
v.df=m.df#create an empty dataframe
c.def=m.df#create an empty dataframe
b.def=m.df#create an empty dataframe

nline=length(bud.level[,1])#total lines
for (i in 1:nline){#creating a dataframe with all the possible values for each bud in letters (es in multiple buds there are more than 1)
  m=bud.level$m[i]
  if(m!=0){
    m.df=rbind.fill(m.df, as.data.frame(t(rep("M",m))))
  } else {
    m.df=rbind.fill(m.df, as.data.frame(t(NA)))
  }
  v=bud.level$v[i]
  if(v!=0){
    v.df=rbind.fill(v.df, as.data.frame(t(rep("V",v))))
  } else {
    v.df=rbind.fill(v.df, as.data.frame(t(NA)))
  }
  c=bud.level$c[i]
  if(c!=0){
    c.def=rbind.fill(c.def, as.data.frame(t(rep("C",1))))
  } else {
    c.def=rbind.fill(c.def, as.data.frame(t(NA)))
  }
  b=bud.level$b[i]
  if(b!=0){
    b.def=rbind.fill(b.def, as.data.frame(t(rep("B",1))))
  } else {
    b.def=rbind.fill(b.def, as.data.frame(t(NA)))
  }

}

dest=cbind(m.df,v.df,c.def,b.def)#merging all the df of the for loop
bud.level=dplyr::mutate(bud.level, fate=NA, .after=tot_buds)#creating a new column where i store the real value of each bud

#storing the type of each bud into the new column
u=1
for (i in 1:nline) {
  oss=bud.level$tot_buds[u]
  q=grep("V|B|C|M", dest[u,], value = TRUE)
  lenq=length(q)
  for (j in 1:lenq) {
    k=q[lenq]
    bud.level$fate[u]=k
    lenq=lenq-1
    u=u+1
  }
  print(u)
}

#substracting the unique value to the sibilngs
v=grep("v", colnames(bud.level))#select column v
m=grep("^m$", colnames(bud.level))#select column m
b=grep("b$", colnames(bud.level))#select column b
c=grep("^c$", colnames(bud.level))#select column c

V=bud.level[bud.level$fate=="V",v]#buds havind "V" as fate
M=bud.level[bud.level$fate=="M",m]#buds havind "M" as fate
B=bud.level[bud.level$fate=="B",b]#buds havind "B" as fate

bud.level[bud.level$fate=="V",v]=V-1#substracting V from the count of buds
bud.level[bud.level$fate=="M",m]=M-1#substracting m from the count of buds
bud.level[bud.level$fate=="B",b]=B-1#substracting b from the count of buds
bud.level[bud.level$fate=="C",c]=0

#adding to each fate the "number of new shoots" information
nshoot=max(unique(met.level$shoot))
nrank=max(unique(met.level$rank_node))
maxnew=max(bud.level$number_new_shoots)

new_n=grep("fate", colnames(newshot.level))#select column fate
new_b=grep("fate", colnames(bud.level))#select column fate
tot_b=grep("number_", colnames(bud.level))#select column number of new shoots

#adding to each fate the "number of new shoots" information
for (i in 1:nshoot) {
  for (j in 1:nrank) {
    for (u in 1:maxnew) {
      F_sh=newshot.level[newshot.level$shoot1yo==i&newshot.level$rank1yo==j&newshot.level$X.newshoot2yo==u,new_n]#fate parental bud in new.shoot df
      F_bu=bud.level[bud.level$shoot==i&bud.level$rank_node==j&bud.level$number_new_shoots==u,new_b]#fate parental bud in bud df
      diff=setdiff(F_bu,F_sh)#difference between fates?
      if (length(diff)!=0) {#if yes
        for (uww in 1:length(diff)) {#compute the length of the differences
          ul=diff[uww]#find the differences
          bud.level[bud.level$shoot==i&bud.level$rank_node==j&bud.level$number_new_shoots==u&bud.level$fate==ul,tot_b]=0#zero new shoots
        }
      }
      if (length(F_sh)!=0) {#if there is more than 1 bud at that rank
        len=abs(length(F_bu)-length(F_sh))#are the two fate different?
        if(len>=1){#if yess
          Z=intersect(F_bu,F_sh)#different bud
          KQ=as.data.frame(table(grep(paste(Z,collapse="|"),F_bu, value = T)))
          Kq=as.data.frame(table(grep(paste(Z,collapse="|"),F_sh, value = T)))
          p=abs(KQ$Freq-Kq$Freq)#gemma in più nel dafabase bud.fat
          if (length(p)!=0&sum(p)>=1) {
            d = which(p>=1,arr.ind=T)
            for (x in 1:length(d)) {
              e=as.character(KQ[d[x],1])#gemma in più
              bud.level[bud.level$shoot==i&bud.level$rank_node==j&bud.level$number_new_shoots==u&bud.level$fate==e,tot_b][1:p[d[x]]]=0
            }
          }
        }
      }
    }
  }
}

#adding the informations regarding the new shoots
new=newshot.level[newshot.level$X.newshoot2yo!=0,]#eliminiamo righe senza nuovi germogli
bud=bud.level[bud.level$number_new_shoots!=0,]#eliminiamo righe senza nuovi germogli
new=new[with(new, order(shoot1yo, rank1yo, fate)), ]#make the same order so that i can paste
bud=bud[with(bud, order(shoot, rank_node, fate)), ]#make the same order so that i can paste
all.equal(bud$rank_node,new$rank1yo)#check if they are actually identical
sacco=data.frame(matrix(nrow = 0, ncol = 0))
sacco=cbind(bud, new[14:22])
colnames(sacco)[c(27,28,29)]=paste0(colnames(sacco)[c(27,28,29)],".1")
pieno=bud.level[bud.level$number_new_shoots==0,]#eliminiamo righe con nuovi germogli
bud.level=rbind.fill(sacco,pieno)#unisci i df con germogli (sacco) e senza (pieno)
bud.level=bud.level[with(bud.level, order(shoot, rank_node)), ]#order

#changing NUMBER of new shoots to 0 (not developed) or 1 (developed)
colnames(bud.level)[tot_b]=c("new_shoots")
bud.level[bud.level$new_shoots>1,tot_b]=1
bud.level=dplyr::mutate(bud.level, from_=NA, .before=length2yo.cm.)
nline=length(bud.level$tesi)
for (i in 1:nline) {
  f=bud.level$fate[i]
  c=bud.level$c[i]
  po=bud.level$position[i]
  if(f=="C"|c>=1){bud.level$from_[i]="SYL"} else{bud.level$from_[i]="PROL"} 
  #CI SONO SOLO DUE APICALI CHE SONO SILLEPTICI
}

write.csv(bud.level,"bud_level_FINAL.csv", row.names = FALSE)#FINAAAAAAAAAAAAL
