#box2:how many V and M buds in sylleptic?
#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))
#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(Hmisc)

#import dataset_metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
#store as a variable the column with the sylleptic info
s=grep("shoot_type",colnames(met))
#rename sylleptic as 1 and proleptic as 0
met[met$shoot_type=="SYLLEPTIC",s]=1
met[met$shoot_type=="PROLEPTIC",s]=0
#transform in numeric the info of sylleptic/proleptic
met$shoot_type=as.numeric(met$shoot_type)
#subset df for sylleptic
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots
#change columns names to not make confusion
a=grep("^Length$", names(SYL_met_scale))
b=grep("^Length.", names(SYL_met_scale))
c=grep("rank_", names(SYL_met_scale))
d=grep("distance_abs", names(SYL_met_scale))
e=grep("tot_", names(SYL_met_scale))
f=grep("n_lat", names(SYL_met_scale))
colnames(SYL_met_scale)[c(a,b,c,d,e,f)]=c("parent_length_cm",
                               "parent_length_nodes",
                               "parent_rank_node",
                               "distance",
                               "tot_buds_in_sylleptic",
                               "m_v_in_sylleptic")
#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS
SYL_met_scale$m_v_in_sylleptic=SYL_met_scale$m_v_in_sylleptic-1
#parameters: length(cm), rank, distance, length(node)
#1: m+v buds ~ parent_length_cm+parent_rank_node+distance+parent length(node)?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance+parent_length_nodes,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#
#2: m+v buds ~ parent_length_cm+parent_rank_node+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)
#null model
null_1=glm(m_v_in_sylleptic~parent_length_cm+distance+1,family = "poisson",data = SYL_met_scale)
summary(null_1)
#difference: real_aic-null_aic
dif=glm_box1$aic-null_1$aic
#new df
met_nul=SYL_met_scale
#empty df
df=data.frame(matrix(nrow=0, ncol=0))
# #permuting 10.000 times
# for (i in 1:10000) {
#   met_nul$parent_rank_node=sample(SYL_met_scale$parent_rank_node)
#   perm=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+distance,family = "poisson",data = met_nul)
#   a=perm$aic-null_1$aic
#   b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# #variable for how many times perm_aic-null_aic was < of real_aic-null_aic
# better_perm=length(which(df$b==1))
#3: m+v buds ~ parent_length_cm+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#yes
# #null model
# null_1=glm(m_v_in_sylleptic~parent_length_cm+1,family = "poisson",data = SYL_met_scale)
# summary(null_1)
# #difference: real_aic-null_aic
# dif=glm_box1$aic-null_1$aic
# #new df
# met_nul=SYL_met_scale
# #empty df
# df=data.frame(matrix(nrow=0, ncol=0))
# #permuting 10.000 times
# for (i in 1:10000) {
#   met_nul$distance=sample(SYL_met_scale$distance)
#   perm=glm(m_v_in_sylleptic~parent_length_cm+distance,family = "poisson",data = met_nul)
#   a=perm$aic-null_1$aic
#   b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# #variable for how many times perm_aic-null_aic was < of real_aic-null_aic
# better_perm=length(which(df$b==1))#times better perm!!!
#graps
#real data
df=data.frame(parent_length_cm=seq(1,max(SYL_met_scale$parent_length_cm),length.out = length(unique(SYL_met_scale$parent_length_cm))))
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate distance)
for (i in seq(0,10, by = 2)) {
  df=cbind(df, data.frame(rep(i, length(df$parent_length_cm))))
}
#rename columns
colnames(df)[2:7]="distance"
#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
}
#rename columns
colnames(df)[8:13]=seq(0,10, by = 2)
#because this graph is difficoult to interpret, plot mean value with se for each length
nline=length(unique(sort(SYL_met_scale$parent_length_cm)))
#store variable 
mv=grep("m_v", colnames(SYL_met_scale))
#new df
dt=met[0,0]
#for loop to store mean and se
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
#graph
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
#graph
#because this graph is difficoult to interpret, plot mean value with se for each length
nline=length(unique(sort(SYL_met_scale$parent_length_cm)))
mline=length(unique(sort(SYL_met_scale$distance)))
mv=grep("m_v", colnames(SYL_met_scale))
dt=met[0,0]
#for loop to store mean and se
for (i in 1:nline) {
  L=unique(sort(SYL_met_scale$parent_length_cm))[i]
  for (j in 1:mline) {
      D=unique(sort(SYL_met_scale$distance))[j]
      n=length(SYL_met_scale[SYL_met_scale$parent_length_cm==L&SYL_met_scale$distance==D,mv])
      av=round(mean(SYL_met_scale[SYL_met_scale$parent_length_cm==L&SYL_met_scale$distance==D,mv]), digits = 2)
      if(n>1) {
        se=round(std.error(SYL_met_scale[SYL_met_scale$parent_length_cm==L&SYL_met_scale$distance==D,mv]), digits=2)
        t=qt(0.975,df=n-1)
        #confidence interval = coef * se
        marg=round(se*t,2)
      }
      if (n==0) {
        av=NA
        se=NA
        t=NA
        marg=NA
        }
      if(n==1){
        se=0
        marg=0
        }
      
      d=cbind(L,D,av,se, marg)
      dt=rbind(dt,d)
  }
}

nline=length(seq(0,10, by = 2))
for (i in 1:nline) {
  a=seq(0,10, by = 2)[i]
  z=seq(0,10, by = 2)[i+1]
  if (a==10) {z=12}
  dt[dt$D>=a&dt$D<z,2]=a  }
#graph
png("2a_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(dt, plot(dt$av~dt$L,
              pch=19,col = rbPal[factor(dt$D)],
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
            col=rbPal[factor(dt$D)]))
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
# #null model
# null_1=glm(m_v_in_sylleptic~parent_length_cm+normal_distance+1,family = "poisson",data = SYL_met_scale)
# summary(null_1)
# #difference real aic null aic
# dif=glm_box1$aic-null_1$aic
# #new df
# met_nul=SYL_met_scale
# #empty df
# df=data.frame(matrix(nrow=0, ncol=0))
# #permuting 10.000 timse
# for (i in 1:10000) {
#   met_nul$parent_rank_node=sample(SYL_met_scale$parent_rank_node)
#   perm=glm(m_v_in_sylleptic~parent_length_cm+parent_rank_node+normal_distance,family = "poisson",data = met_nul)
#   a=perm$aic-null_1$aic
#   b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# #variable for how many times perm_aic-null_aic was < of real_aic-null_aic
# better_perm=length(which(df$b==1))#times better perm!!!

#3: m+v buds ~ parent_length_cm+distance?####
glm_box1=glm(m_v_in_sylleptic~parent_length_cm+normal_distance,family = "poisson",data = SYL_met_scale)
summary(glm_box1)#
#create a sequence with random numbers between 1 and maximum length (72cm)
df=data.frame(parent_length_cm=seq(1,max(SYL_met_scale$parent_length_cm),length.out = length(unique(SYL_met_scale$parent_length_cm))))
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate distance)
for (i in seq(0,0.5, by = (0.5/5))) {
  df=cbind(df, data.frame(rep(i, length(df$parent_length_cm))))
}
#rename columns
colnames(df)[2:7]="normal_distance"
#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
}
#rename columns
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

#graph
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
