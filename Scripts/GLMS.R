#create glms model
setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/Lateral/")
library(stats)
plant <-read.csv("C:/Users/franc/Google Drive/PhD/Deruta/2020/Original/2020Inne_autor1.csv")
all <- read.csv("C:/Users/franc/Google Drive/PhD/Deruta/auto/all.csv")
all[all$c>1,8]=1
all[all$b>1,11]=1

# df=data.frame(matrix(ncol=14, nrow = 0))#create an empty df
# colnames(df)=c(colnames(all)[c(5,7)],"TreeID",colnames(all)[c(6,4,8:11)],"sum_buds",colnames(all)[17],"dev_fromV","dev_fromM", "dev_fromother")

df=all[FALSE,]#EMPTY DF

#adding ID shoots and Ranks node to dataframe
# df[1:2]=unique(all[5:13])[c(1,3)]

s.prev=0#preavious shoot
r.prev=0#preavious rank
p.prev=0#previous position
d.prev=0#previous derived from
l.prev=0
nline=dim(all[1])[1]#lines af all.csv

#what to do?
#write a new dataframe with row repetition as many buds per rank
#if a rank is apical remove derived from apical

for (i in 1:nline) {
  s=all$shoot1yo[i]
  r=all$rank1yo[i]
  d=all$derived.from[i]
  p=all$position[i]
  l=all$length2yo.cm.[i]
  # p=all$position[i]
  if ((s!=s.prev)&(r!=r.prev)) {#situation1: shoot and rank are different from previous. So first node
    x=all$X.oss[i]#x=number of buds
    df=rbind(df, all[rep(i, each = x), ])
  } 
  if (s==s.prev) {#situation2: same shoot
    x=all$X.oss[i]
    if(p=="AP"){
        x=0
    }
    if(r!=r.prev){
      if(r==all$rank1yo[i+1]){
        x=x
      } else {
        if ((l==all$length2yo.cm.[i+1])&(d==all$derived.from[i+1])){
          x=x-1
        }
        x=x
      }
      }
    if(r==r.prev){
      if(d==d.prev&l==l.prev){
        x=0
        } else {
          x=1
        }
      }
    df=rbind(df, all[rep(i, each = x), ])
    }
  s.prev=s
  r.prev=r
  d.prev=d
  p.prev=p
  l.prev=l
  }


#questo per duplicare
for (i in 1:nline) {
  s=all$shoot1yo[i]
  r=all$rank1yo[i]
 # p=all$position[i]
  if ((s!=s.prev)&(r!=r.prev)) {#situation1: shoot and rank are different from previous. 
    x=all$X.oss[i]#x=number of buds
    df=rbind(df, all[rep(i, each = x), ])
    } 
  
  if ((s==s.prev)&(r!=r.prev)) {#situation2: s is the same and r is different 
    x=all$X.oss[i]
    
    if((i==nline-1)|(all$shoot1yo[i+2]!=s)){#when r is the last one
      x=x-1#x=-1 because one bud is the apica
      }
    
    df=rbind(df, all[rep(i, each = x), ])} 
  else {#situation3: when there is duplication
    x=0
    df=rbind(df, all[rep(i, each = x), ])}
  
   s.prev=s
   r.prev=r
}
  

   
          if (count==x){
        m=all$m[i]
        v=all$v[i]
        
        for (p in 1:m) {
          
          
        }
      }
      }else {
      m.dev=0
      v.dev=0
      if(all$derived.from[i]=="M"){
        m.dev=m.dev+1#count number of developing mixed
      } if(all$derived.from[i]=="V"){
        v.dev=v.dev+1#count number of developing mixed
      }}
    
  }
  
  # you add a line with "developing" = 1 and "derived.from" = M
  # 
  # For each value of v.dev you add a line with "developing" = 1 and "derived.from" = V
  # For each value of "total number of v" -v.dev you add a line with "developing" = 0 and "derived.from" = "N" (none)
  # You just have to substract 1 to v.dev is the apical is a V or the apical is M
  #You know you reached the apical whenever i==nline or ((r.prev != r) & (s.prev != s)).
  # And yes, the point I forgot is that you have to read all$Apical or course.
  
  


#(sum(df[14])/sum(df$buds_developed))*100#10.5 errore che ho fatto io nell'attribuire la provenienza delle gemme

modV = glm(Y~F1+F2+F3+F4+F5+F6, family="binomial",data=glmV)
summary(modV)
