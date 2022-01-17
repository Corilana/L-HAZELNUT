#create glms model
setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/Lateral/")
library(stats)
plant <-read.csv("C:/Users/franc/Google Drive/PhD/Deruta/2020/Original/2020Inne_autor1.csv")
bud_fate <- read.csv("C:/Users/franc/Google Drive/PhD/Deruta/auto/bud_fate.csv")
bud_fate[bud_fate$c>1,8]=1
bud_fate[bud_fate$b>1,11]=1
a=nrow(unique(bud_fate[5:13])[1])#creaiamo una variabile con il numero di righe
df=data.frame(matrix(ncol=14, nrow = a))#creiamo un nuovo df 
df[1:2]=unique(bud_fate[5:13])[c(1,3)]
colnames(df)=c(colnames(bud_fate)[c(5,7)],"TreeID",colnames(bud_fate)[c(6,4,8:11)],"sum_buds",colnames(bud_fate)[17],"dev_fromV","dev_fromM", "dev_fromother")

for (i in 1:length(sort(unique(bud_fate$shoot1yo)))) {
  
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

for (i in 1:nrow(df)) {
  
  n=df$buds_developed[i]-1
  if (n>1) {
    df1=do.call("rbind", replicate(n, df, simplify = FALSE))
  }
  
}


#(sum(df[14])/sum(df$buds_developed))*100#10.5 errore che ho fatto io nell'attribuire la provenienza delle gemme

modV = glm(Y~F1+F2+F3+F4+F5+F6, family="binomial",data=glmV)
summary(modV)
