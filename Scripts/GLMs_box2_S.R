#GLM: how many V and M buds in sylleptic?
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(Hmisc)

#metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
s=grep("shoot_type",colnames(met))#store the column with the sylleptic info
met[met$shoot_type=="SYLLEPTIC",s]=1#1 is sylleptic
met[met$shoot_type=="PROLEPTIC",s]=0#0 is proleptic
met$shoot_type=as.numeric(met$shoot_type)#transform in numeric the info of sylleptic/proleptic

#df with sylleptic
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots

#change columns names to not make confusion
colnames(SYL_met_scale)[c(2,6,7,8,16,18)]=c("parent_length_cm",
                               "parent_length_nodes",
                               "parent_rank_node",
                               "distance",
                               "tot_buds_in_sylleptic",
                               "m_v_in_sylleptic")
SYL_met_scale$m_v_in_sylleptic=SYL_met_scale$m_v_in_sylleptic-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS

#parameters: length(cm), rank, distance, length(node)

#1: m+v buds ~ parent_length_cm+parent_rank_node+distance+parent length(node)?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance+parent_length_nodes,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#2: m+v buds ~ parent_length_cm+parent_rank_node+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#permutations
null_1=glm(m_v_in_sylleptic~parent_length_cm+distance+1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_rank_node=sample(SYL_met_scale$parent_rank_node)
  perm=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance,family = "poisson",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3: m+v buds ~ parent_length_cm+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#permutations
null_1=glm(m_v_in_sylleptic~parent_length_cm+1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$distance=sample(SYL_met_scale$distance)
  perm=glm(m_v_in_sylleptic~parent_length_cm+distance,family = "poisson",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#plot1
#create a sequence with random numbers between 1 and maximum length (72cm)
df=data.frame(parent_length_cm=seq(1,max(SYL_met_scale$parent_length_cm),length.out = length(unique(SYL_met_scale$parent_length_cm))))
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate distance)
for (i in seq(0,10, by = 2)) {
  df=cbind(df, data.frame(rep(i, length(df$parent_length_cm))))
}
colnames(df)[2:7]="distance"

#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
}

colnames(df)[8:13]=seq(0,10, by = 2)

#because this graph is difficoult to interpret, plot mean value with se for each length
nline=length(unique(sort(SYL_met_scale$parent_length_cm)))
mv=grep("m_v", colnames(SYL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(SYL_met_scale$parent_length_cm))[i]
  av=round(mean(SYL_met_scale[SYL_met_scale$parent_length_cm==L,mv]), digits = 2)
  n=length(SYL_met_scale[SYL_met_scale$parent_length_cm==L,mv])
  se=round(std.error(SYL_met_scale[SYL_met_scale$parent_length_cm==L,mv]), digits=2)
  if(n==1){
    se=0
    marg=0}
  else{
    t=qt(0.975,df=n-1)
    #confidence interval = coef * se
    marg=se*t
    }
  d=cbind(L,av,se, marg)
  dt=rbind(dt,d)
}

#plot
png("2_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(dt, plot(dt$av~dt$L,
                         pch=19,col = cols[1],
                         main="average+-(se)_m+v buds in sylleptic vs parent_length(cm)",
                         xlab= "parent_length(cm)",
                         ylab="average+-(se) m+v buds",
              ylim=c(0,7)))
with(arrows(x0 = dt$L,                           # Add error bars
       y0 = dt$av + dt$se,
       y1 = dt$av - dt$se,
       angle = 90,
       code = 3,
       length = 0.05,
       col=cols[1]))
for (i in 1:length(grep("[0-9]", colnames(df)))) {
  t=grep("[0-9]", colnames(df))[i]
  with(df,lines(df[,t]~df$parent_length_cm,col=rbPal[i], lwd=3))
}
legend("top",
       horiz=T,
       xpd = TRUE,c("real", "dist=0","dis=2","dis=4","dis=6","dis=8","dis=10"),
       pch = c(19,NA,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1,1), lwd=5,
       col = c(cols[1], rbPal),
       cex=0.6)
dev.off()

#1: m+v buds ~ parent_length_cm+parent_rank_node+normalized_distance+parent length(node)?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+normal_distance+parent_length_nodes,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#2: m+v buds ~ parent_length_cm+parent_rank_node+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+normal_distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#permutations
null_1=glm(m_v_in_sylleptic~parent_length_cm+normal_distance+1,family = "poisson",data = SYL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=SYL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$parent_rank_node=sample(SYL_met_scale$parent_rank_node)
  perm=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+normal_distance,family = "poisson",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#3: m+v buds ~ parent_length_cm+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+normal_distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes

#plot1
#create a sequence with random numbers between 1 and maximum length (72cm)
df=data.frame(parent_length_cm=seq(1,max(SYL_met_scale$parent_length_cm),length.out = length(unique(SYL_met_scale$parent_length_cm))))
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate distance)
for (i in seq(0,0.5, by = (0.5/5))) {
  df=cbind(df, data.frame(rep(i, length(df$parent_length_cm))))
}
colnames(df)[2:7]="normal_distance"

#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
}

colnames(df)[8:13]=seq(0,0.5, by = (0.5/5))

#because this graph is difficoult to interpret, plot mean value viwht se for each length
nline=length(unique(sort(SYL_met_scale$parent_length_cm)))
mv=grep("m_v", colnames(SYL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(SYL_met_scale$parent_length_cm))[i]
  av=round(mean(SYL_met_scale[SYL_met_scale$parent_length_cm==L,mv]), digits = 2)
  n=length(SYL_met_scale[SYL_met_scale$parent_length_cm==L,mv])
  se=round(std.error(SYL_met_scale[SYL_met_scale$parent_length_cm==L,mv]), digits=2)
  if(n==1){
    se=0
    marg=0}
  else{
    t=qt(0.975,df=n-1)
    #confidence interval = coef * se
    marg=se*t
  }
  d=cbind(L,av,se, marg)
  dt=rbind(dt,d)
}

#plot
png("2b_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(dt, plot(dt$av~dt$L,
              pch=19,col = cols[1],
              main="average+-(se)_m+v buds in sylleptic vs parent_length(cm)",
              xlab= "parent_length(cm)",
              ylab="average+-(se) m+v buds",
              ylim=c(0,7)))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
for (i in 1:length(grep("[0-9]", colnames(df)))) {
  t=grep("[0-9]", colnames(df))[i]
  with(df,lines(df[,t]~df$parent_length_cm,col=rbPal[i], lwd=3))
}
legend("top",
       horiz=T,
       xpd = TRUE,c("real", "norm_dist=0","norm_dist=0.1","norm_dist=0.2","norm_dist=0.3","norm_dist=0.4","norm_dist=0.5"),
       pch = c(19,NA,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1,1), lwd=5,
       col = c(cols[1], rbPal),
       cex=0.6)
dev.off()
