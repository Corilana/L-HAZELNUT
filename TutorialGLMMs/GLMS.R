#create glms model
setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/Lateral/")
library(stats)
plant <-read.csv("C:/Users/franc/Google Drive/PhD/Deruta/2020/Original/2020Inne_autor1.csv")
all <- read.csv("C:/Users/franc/Google Drive/PhD/Deruta/auto/all.csv")
all[all$c>1,8]=1
all[all$b>1,11]=1
# a=nrow(unique(all[5:13])[1])#creaiamo una variabile con il numero di righe
df=data.frame(matrix(ncol=14, nrow = 0))#creiamo un nuovo df 
colnames(df)=c(colnames(all)[c(5,7)],"TreeID",colnames(all)[c(6,4,8:11)],"sum_buds",colnames(all)[17],"dev_fromV","dev_fromM", "dev_fromother")

#adding ID shoots and Ranks node to dataframe
# df[1:2]=unique(all[5:13])[c(1,3)]


s.prev=-1
r.prev=-1
nline=dim(all[1])
for (i in 1:nline) {
  
  s=all$shoot1yo[i]
  r=all$rank1yo[i]
  if ((s!=s.prev)&(r!=r.prev)) {#begin new set of buds with new counter indicating number of duplications
    count=1
    x=all$X.newshoot2yo[i] 
    if((i==nline)|(all$shoot1yo[i+1]!=s)){#the last rank in the shoot could have some apical buds
      x=x-1
      }
  }else { 
    if (r==r.prev) {
      count=count+1
      if(all$derived.from[i]=="M"){
        m.dev=m.dev+1#count number of developing mixed
      } if(all$derived.from[i]=="V"){
        v.dev=v.dev+1#count number of developing mixed
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
  
  I=sort(unique(bud_fate$shoot1yo))[i]
  for (q in 1:length(sort(unique(bud_fate$rank1yo)))) {
    Q=sort(unique(bud_fate$rank1yo))[q]
    df[df$shoot1yo==I&df$rank1yo==Q,3]=plant[plant$shoot==I&plant$tesi=="auto",6]
    df[df$shoot1yo==I&df$rank1yo==Q,4:11]=unique(bud_fate[bud_fate$shoot1yo==I&bud_fate$rank1yo==Q,c(6,4,8:11,13,17)])
    df[df$shoot1yo==I&df$rank1yo==Q,12]=length(bud_fate[bud_fate$shoot1yo==I&bud_fate$rank1yo==Q&bud_fate$derived.from=="M",18])
    df[df$shoot1yo==I&df$rank1yo==Q,13]=length(bud_fate[bud_fate$shoot1yo==I&bud_fate$rank1yo==Q&bud_fate$derived.from=="V",18])
    df[df$shoot1yo==I&df$rank1yo==Q,14]=length(bud_fate[bud_fate$shoot1yo==I&bud_fate$rank1yo==Q&bud_fate$derived.from!="M"&bud_fate$derived.from!="V",18])
    
  }
  
}
df[df$X.newshoot2yo==0,12:14]=0
df$buds_developed=rowSums(df[12:14])

df$w=rowSums(df[7:8])

for (i in 1:nrow(df)) {
  
  n=df$w[i]-1
  if (n>1) {
    df1=do.call("rbind", replicate(n, df, simplify = FALSE))
  }
  
}


#(sum(df[14])/sum(df$buds_developed))*100#10.5 errore che ho fatto io nell'attribuire la provenienza delle gemme

modV = glm(Y~F1+F2+F3+F4+F5+F6, family="binomial",data=glmV)
summary(modV)
