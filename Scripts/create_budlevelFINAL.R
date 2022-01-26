#21/10/2022:
#overwriting "bud_level_developed" dataframe:
#1_ choosing the fate of the central bud and
#2_ adding the number of sibling buds
#3_ adding the position of the central bud (apical or lateral is given).
#when multiple child per node, for the APICAL, I choose the longest)

setwd("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/")

library(plyr)
library("stringr")

met.level<-read.csv("2020metamer_level_DEVELOPED.csv")
newshot.level<-read.csv("DFAUTO_new.shootlevel.csv")
newshot.level=newshot.level[newshot.level$derived.from!="?",] #remove those who had more than 1 shoot but i recorded just one buds!!!
bud.level<-read.csv("bud_level_develop.csv")

catkin=bud.level$c#store catkin variable
catkin[catkin>1]=1#catkins is put to 1 even though they are more

# tot.buds=sum(met.level$tot_buds)#1676
# tot.newshoots=sum(met.level$new_shoots)#1018

m.df=data.frame(matrix(ncol = 0, nrow=0))#create an empty dataframe
v.df=m.df#create an empty dataframe
c.def=m.df#create an empty dataframe
b.def=m.df#create an empty dataframe

nline=length(bud.level[,1])#total lines
for (i in 1:nline){#creating a dataframe with all the possible values for each buds in letters (es in multiple buds there are more than 1)
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
  c=catkin[i]
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

dest=cbind(m.df,v.df,c.def,b.def)#creating one df storing values of for loop
bud.level$fate=NA#creating a new column where i store the real value of each bud

u=1
for (i in 1:nline) {#storing the fate of each bud into the new column
  oss=bud.level$X.oss[u]
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
v=grep("v", colnames(bud.level))
m=grep("m", colnames(bud.level))
b=grep("b", colnames(bud.level))
c=grep("c$", colnames(bud.level))

V=bud.level[bud.level$fate=="V",v]#buds havind "V" as fate
M=bud.level[bud.level$fate=="M",m]
B=bud.level[bud.level$fate=="B",b]

bud.level[bud.level$fate=="V",v]=V-1#substracting V from the count of buds
bud.level[bud.level$fate=="M",m]=M-1
bud.level[bud.level$fate=="B",b]=B-1
bud.level[bud.level$fate=="C",c]=0

nshoot=max(unique(met.level$shoot))
nrank=max(unique(met.level$rank_node))
maxnew=max(bud.level$new_shoots)

new_n=grep("^de", colnames(newshot.level))
new_b=grep("fa", colnames(bud.level))
tot_b=grep("new", colnames(bud.level))

#removing duplication of "new_shoots"
for (i in 1:nshoot) {
  for (j in 1:nrank) {
    for (u in 1:maxnew) {
      F_sh=newshot.level[newshot.level$shoot1yo==i&newshot.level$rank1yo==j&newshot.level$X.newshoot2y==u,new_n]#fate at that rank, that shoot, that new shoot
      F_bu=bud.level[bud.level$shoot==i&bud.level$rank_node==j&bud.level$new_shoots==u,new_b]#fate at that rank, that shoot, that new shoot
      #troviamo le gemme diverse
      diff=setdiff(F_bu,F_sh)
      if (length(diff)!=0) {
        for (uww in 1:length(diff)) {
          ul=diff[uww]#gemma diversa
          bud.level[bud.level$shoot==i&bud.level$rank_node==j&bud.level$new_shoots==u&bud.level$fate==ul,tot_b]=0 
        }
      }
      if (length(F_sh)!=0) {
        len=length(F_bu)-length(F_sh)#differenza tra i due df
        if(len>=1){
          Z=intersect(F_bu,F_sh)
          KQ=as.data.frame(table(grep(paste(Z,collapse="|"),F_bu, value = T)))
          Kq=as.data.frame(table(grep(paste(Z,collapse="|"),F_sh, value = T)))
          p=KQ$Freq-Kq$Freq#gemma in più nel dafabase bud.fat
          if (length(p)!=0&sum(p)>=1) {
            a = which(p>=1,arr.ind=T)
            for (x in 1:length(a)) {
              b=as.character(KQ[a[x],1])#gemma in più 
              bud.level[bud.level$shoot==i&bud.level$rank_node==j&bud.level$new_shoots==u&bud.level$fate==b,tot_b][1:p[a[x]]]=0
            }
          }
        }
      }
    }
  }
}

bud.level$position="LATERAL"

#adding the informations regarding the new shoots
new=newshot.level[newshot.level$X.newshoot2yo!=0,]
nshoot=length(unique(bud.level$shoot))
nrank=length(unique(bud.level$rank_node))
sacco=data.frame(matrix(ncol = 0, nrow=0))
len=grep("^length2yo", colnames(new))
max=ncol(new)

for (i in 1:nshoot) {
  p=unique(bud.level$shoot)[i]
  for (j in 1:nrank) {
    A=new[new$shoot1yo==p&new$rank1yo==j,new_n]
    B=bud.level[bud.level$shoot==p&bud.level$new_shoots!=0&bud.level$rank_node==j,new_b]
    Z=setdiff(A,B)
    if(length(Z)==0){
      K=cbind(bud.level[bud.level$shoot==p&bud.level$new_shoots!=0&bud.level$rank_node==j,],new[new$shoot1yo==p&new$rank1yo==j,len:max])
      sacco=rbind.fill(sacco, K)
      }
  }
}

pieno=bud.level[bud.level$new_shoots==0,]
bud.level=rbind.fill(sacco,pieno)
bud.level=bud.level[order(bud.level$shoot), ]

#write if that bud is apical or not
nshoot=length(unique(bud.level$shoot))
pos=grep("position", colnames(bud.level))
l=grep("length2yo", colnames(bud.level))
n=grep("X.nodes", colnames(bud.level))
r=grep("rank_", colnames(bud.level))
for (i in 1:nshoot) {
  p=sort(unique(bud.level$shoot))[i]
  m=max(bud.level[bud.level$shoot==p,r], na.rm = T)
  len=max(bud.level[bud.level$shoot==p&bud.level$rank_node==m&bud.level$new_shoots>0,l], na.rm = T)
  nod=max(bud.level[bud.level$shoot==p&bud.level$rank_node==m&bud.level$new_shoots>0,n], na.rm = T)
  bud.level[bud.level$shoot==p&bud.level$rank_node==m&bud.level$length2yo.cm.==len&bud.level$X.nodes2yo==nod&bud.level$new_shoots>0,pos]="AP"
}

bud.level[bud.level$new_shoots>1,tot_b]=1

write.csv(bud.level,"bud_level_FINAL.csv", row.names = FALSE)#FINAAAAAAAAAAAAL
