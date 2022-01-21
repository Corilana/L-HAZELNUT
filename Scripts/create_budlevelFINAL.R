#21/10/2022:
#overwriting "bud_level_developed" dataframe:
#1_ choosing the fate of the central bud and
#2_ adding the number of sibling buds
#3_ adding the position of the central bud (apical or lateral is given).
#when multiple child per node, for the APICAL, I choose the longest)

setwd("C:/Users/franc/Google Drive/PhD/Deruta/auto/")

library("plyr")
library( "stringr" )

met.level<-read.csv("met_level_develop.csv")
newshot.level<-read.csv("DFAUTO_new.shootlevel.csv")
newshot.level=newshot.level[newshot.level$derived.from!="?",] #remove those who had more than 1 shoot but i recorded just one buds!!!
bud.level<-read.csv("bud_level_develop.csv")

catkin=bud.level$c#store catkin variable
catkin[catkin>1]=1#catkins is put to 1 even though they are more

tot.buds=sum(met.level$X.oss)#1685
tot.newshoots=sum(met.level$X.newshoots)#1021

m.df=data.frame(matrix(ncol = 0, nrow=0))#create an empty dataframe
v.df=data.frame(matrix(ncol = 0, nrow=0))#create an empty dataframe
c.def=data.frame(matrix(ncol = 0, nrow=0))#create an empty dataframe
b.def=data.frame(matrix(ncol = 0, nrow=0))#create an empty dataframe

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
for (i in 1:nline) {#storing the unique value of each bud into the new column
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
V=bud.level[bud.level$fate=="V",9]
M=bud.level[bud.level$fate=="M",10]
B=bud.level[bud.level$fate=="B",11]

bud.level[bud.level$fate=="V",9]=V-1
bud.level[bud.level$fate=="M",10]=M-1
bud.level[bud.level$fate=="B",11]=B-1
bud.level[bud.level$fate=="C",8]=0

nshoot=max(unique(met.level$shoot1yo))
nrank=max(unique(met.level$rank1yo))
maxnew=max(bud.level$X.newshoots)

#removing duplication of "new_shoots"
for (i in 1:nshoot) {
  for (j in 1:nrank) {
    for (u in 1:maxnew) {
      Uq=newshot.level[newshot.level$shoot1yo==i&newshot.level$rank1yo==j&newshot.level$X.newshoot2y==u,14]
      UQ=bud.level[bud.level$shoot1yo==i&bud.level$rank1yo==j&bud.level$X.newshoots==u,14]
      #troviamo le gemme diverse
      uw=setdiff(UQ,Uq)
      if (length(uw)!=0) {
        for (uww in 1:length(uw)) {
          ul=uw[uww]
          bud.level[bud.level$shoot1yo==i&bud.level$rank1yo==j&bud.level$X.newshoots==u&bud.level$fate==ul,13]=0 
        }
      }
      if (length(Uq)!=0) {
        len=length(UQ)-length(Uq)#differenza tra i due df
        if(len>=1){
          Z=intersect(UQ,Uq)
          KQ=count(grep(paste(Z,collapse="|"),UQ, value = T))
          Kq=count(grep(paste(Z,collapse="|"),Uq, value = T))
          p=KQ$freq-Kq$freq#gemma in più nel dafabase bud.fat
          if (length(p)!=0&sum(p)>=1) {
            a = which(p>=1,arr.ind=T)
            for (x in 1:length(a)) {
              b=KQ[a[x],1]#gemma in più 
            bud.level[bud.level$shoot1yo==i&bud.level$rank1yo==j&bud.level$X.newshoots==u&bud.level$fate==b,13][1:p[a[x]]]=0
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

nshoot=length(unique(bud.level$shoot1yo))
nrank=length(unique(bud.level$rank1yo))
sacco=data.frame(matrix(ncol = 0, nrow=0))
for (i in 1:nshoot) {
  p=unique(bud.level$shoot1yo)[i]
  for (j in 1:nrank) {
    A=new[new$shoot1yo==p&new$rank1yo==j,14]
    B=bud.level[bud.level$shoot1yo==p&bud.level$X.newshoots!=0&bud.level$rank1yo==j,14]
    Z=setdiff(A,B)
    if(length(Z)==0){
      K=cbind(bud.level[bud.level$shoot1yo==p&bud.level$X.newshoots!=0&bud.level$rank1yo==j,],new[new$shoot1yo==p&new$rank1yo==j,15:207])
      sacco=rbind.fill(sacco, K)
      }
  }
}

pieno=bud.level[bud.level$X.newshoots==0,]
bud.level=rbind.fill(sacco,pieno)
bud.level=bud.level[order(bud.level$X), ]

#write if that bud is apical or not
nshoot=length(unique(bud.level$shoot1yo))
for (i in 1:nshoot) {
  p=sort(unique(bud.level$shoot1yo))[i]
  m=max(bud.level[bud.level$shoot1yo==p,7], na.rm = T)
  len=max(bud.level[bud.level$shoot1yo==p&bud.level$rank1yo==m&bud.level$X.newshoots>0,16], na.rm = T)
  nod=max(bud.level[bud.level$shoot1yo==p&bud.level$rank1yo==m&bud.level$X.newshoots>0,18], na.rm = T)
  bud.level[bud.level$shoot1yo==p&bud.level$rank1yo==m&bud.level$length2yo.cm.==len&bud.level$X.nodes2yo==nod&bud.level$X.newshoots>0,15]="AP"
}

bud.level[bud.level$X.newshoots==0,16]=0
colnames(bud.level)[13]="tot.buds"

write.csv(bud.level,"bud_level_FINAL.csv")#FINAAAAAAAAAAAAL
#subset the two df
ap <- bud.level[bud.level$position=="AP",]
lat <-bud.level[bud.level$position=="LATERAL",]

#save them for the successive analysis
write.csv(ap, "C:/Users/franc/Google Drive/PhD/Deruta/auto/apicals.csv")
write.csv(lat, "C:/Users/franc/Google Drive/PhD/Deruta/auto/laterals.csv")

