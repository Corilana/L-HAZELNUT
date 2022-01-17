#create glms model
setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/Lateral/")
library(stats)
plant <-read.csv("C:/Users/franc/Google Drive/PhD/Deruta/2020/Original/2020Inne_autor1.csv")
all <- read.csv("C:/Users/franc/Google Drive/PhD/Deruta/auto/all.csv")
all[all$c>1,8]=1
all[all$b>1,11]=1

df=data.frame(matrix(ncol=14, nrow = 0))#create an empty df
colnames(df)=c(colnames(all)[c(5,7)],"TreeID",colnames(all)[c(6,4,8:11)],"sum_buds",colnames(all)[17],"dev_fromV","dev_fromM", "dev_fromother")

#adding ID shoots and Ranks node to dataframe
# df[1:2]=unique(all[5:13])[c(1,3)]

s.prev=-1
r.prev=-1
nline=dim(all[1])
for (i in 1:nline) {
  s=all$shoot1yo[i]
  r=all$rank1yo[i]
  if ((s!=s.prev)&(r!=r.prev)) {#situation1: shoot and rank are different from previous. 
    count=1
    x=all$X.newshoot2yo[i]#x=number of duplication
    if((i==nline)|(all$shoot1yo[i+1]!=s)){#situation1a: s and r are different but r is the last one
      x=x-1#x=-1 because there is the apical
      }
    } else {
      if ((s==s.prev)&(r==r.prev)) { #situation2: shoot is the same and rank is the same as precedent
      count=count+1
      
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
  
  
#step by step
#1_ changing apical buds
apical=all[all$position=="AP",]
lateral=all[all$position!="AP",]
df1=data.frame(matrix(ncol=14, nrow = 938))
colnames(df1)=colnames(df)
df1[1:2]=unique(lateral[4:7])[c(2,4)]
s=lateral$shoot1yo
nline=length(unique(s))
r=lateral$rank1yo
ls=df1$shoot1yo
lr=df1$rank1yo

for (i in 1:nline) {
  S=unique(s)[i]
  
  for (q in 1:length(unique(r))) {
    R=unique(r)[q]
    
    df1[ls==S&lr==R,3]=plant[plant$shoot==S&plant$tesi=="auto",6]
    df1[ls==S&lr==R,4:11]=unique(lateral[s==S&r==R,c(6,4,8:11,13,17)])
    df1[ls==S&lr==R,12]=length(lateral[s==S&r==R&lateral$derived.from=="M",18])
    df1[ls==S&lr==R,13]=length(lateral[s==S&r==R&lateral$derived.from=="V",18])
    df1[ls==S&lr==R,14]=length(lateral[s==S&r==R&lateral$derived.from!="M"&lateral$derived.from!="V",18])
    
  }
  
}

df1[df1$X.newshoot2yo==0,12:14]=0
df1$buds_developed=rowSums(df1[12:14])

df1$w=rowSums(df1[7:8])

df.expanded <- df1[rep(row.names(df1), df1$w),]



#(sum(df[14])/sum(df$buds_developed))*100#10.5 errore che ho fatto io nell'attribuire la provenienza delle gemme

modV = glm(Y~F1+F2+F3+F4+F5+F6, family="binomial",data=glmV)
summary(modV)
