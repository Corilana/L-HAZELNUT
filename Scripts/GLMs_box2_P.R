#GLM: Existence of B (0,1)? 
#GLM: #how many V and M buds in proleptic?
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)

#metamer level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
s=grep("shoot_type",colnames(met))#store the column with the sylleptic info
met[met$shoot_type=="SYLLEPTIC",s]=1#1 is sylleptic
met[met$shoot_type=="PROLEPTIC",s]=0#0 is proleptic
met$shoot_type=as.numeric(met$shoot_type)#transform in numeric the info of sylleptic/proleptic

#df with proleptic
PROL_met_scale=met[met$shoot_type==0,]

#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,16,18)]=c("length_cm",
                                            "length_nodes",
                                            "rank_node",
                                            "distance",
                                            "tot_buds",
                                            "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#rimuoviamo blind buds from the counting of m+v

#parameters: length(cm), length(node), rank_node, distance
#length_cm~length_nodes####
#i will delete parent_length nodes because they are highly related
glm_box1=nls(length_nodes~a*(length_cm^b),data = PROL_met_scale,start = c(a=1, b=0.5))
summary(glm_box1)

df=data.frame(length_cm=seq(1,max(PROL_met_scale$length_cm),by=1))
df$pred=predict(glm_box1,
                newdata = df,
                type="response")

png("2a.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(PROL_met_scale, plot(PROL_met_scale$length_nodes~PROL_met_scale$length_cm,
                          pch=19,col = cols[1],
                          main="length(cm) vs length(node)",
                          xlab= "length(cm)",
                          ylab="length(node)"),)
with(df,lines(df$pred~df$length_cm,col=cols[2], lwd=5))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#1:b~length+rank+distance+length?####
glm_box1=glm(b~length_cm+rank_node+distance+length_nodes,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#permut_ distance
null_1=glm(b~length_cm+rank_node+length_nodes+1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$distance=sample(PROL_met_scale$distance)
  perm=glm(b~length_cm+rank_node+distance+length_nodes,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#2:b~length+rank+distance?####
glm_box1=glm(b~length_cm+rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#3:b~length+rank?####
glm_box1=glm(b~length_cm+rank_node,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#4:b~rank?####
glm_box1=glm(b~rank_node,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#permutations_rank_node
null_1=glm(b~1,family = "binomial",data = PROL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$rank_node=sample(PROL_met_scale$rank_node)
  perm=glm(b~rank_node,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

# df b~rank
prop=as.data.frame.matrix(table(PROL_met_scale$rank_node,PROL_met_scale$b))
prop$ratio=round((prop$`1`)/(rowSums(prop[1:2])), digit=2)
colnames(prop)[1:2]=c("no_b","si_b")
prop$rank=as.numeric(rownames(prop))
head(prop)

#df: predict~v
prop$pred=predict(glm_box1,
                  newdata = data.frame(rank_node=seq(1, max(prop$rank), length.out = length(prop$rank))),
                  type="response")

#histogram
png("2a_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%b (#b/tot_nodes) vs rank node",
                xlab= "rank node",
                ylab="%b",
                ylim=c(0,1),
                type="h",
                lwd=5))
with(prop, lines(prop$pred~prop$rank,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#1: m+v buds ~ parent lenght(cm)+distance+ rank_node+length(node)?####
glm_box1=glm(m_v~length_cm+distance+rank_node+length_nodes,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#2: m+v buds ~ parent lenght(cm)+distance+ rank_node?####
glm_box1=glm(m_v~length_cm+distance+rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#permutations_length
null_1=glm(m_v~distance+rank_node+1,family = "poisson",data = PROL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$length_cm=sample(PROL_met_scale$length_cm)
  perm=glm(m_v~length_cm+distance+rank_node,family = "poisson",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#permutations_distance
null_1=glm(m_v~length_cm+rank_node+1,family = "poisson",data = PROL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$distance=sample(PROL_met_scale$distance)
  perm=glm(m_v~length_cm+distance+rank_node,family = "poisson",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#we have to choose if weeping distance or rank node or length
#rank
nline=length(unique(sort(PROL_met_scale$rank_node)))
mv=grep("m_v", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$rank_node))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$rank_node==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$rank_node==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$rank_node==L,mv]), digits=2)
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

#histogram
png("2b_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
                col = cols[1],
                main="average #m+v vs rank node",
                xlab= "rank node",
                ylab="average #m+v",
                pch=19))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
dev.off()

#distance
nline=length(unique(sort(PROL_met_scale$distance)))
mv=grep("m_v", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$distance))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$distance==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$distance==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$distance==L,mv]), digits=2)
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

#histogram
png("2c_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
             col = cols[1],
             main="average #m+v vs distance",
             xlab= "distance",
             ylab="average #m+v",
             pch=19))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
dev.off()

#length
nline=length(unique(sort(PROL_met_scale$length_cm)))
mv=grep("m_v", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$length_cm))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$length_cm==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$length_cm==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$length_cm==L,mv]), digits=2)
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

#histogram
png("2d_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
             col = cols[1],
             main="average #m+v vs length_cm",
             xlab= "length_cm",
             ylab="average #m+v",
             pch=19))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
dev.off()

#3: m+v buds ~ rank_node?####
glm_box1=glm(m_v~rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#rank
nline=length(unique(sort(PROL_met_scale$rank_node)))
mv=grep("m_v", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$rank_node))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$rank_node==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$rank_node==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$rank_node==L,mv]), digits=2)
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

#df: predict~m+v
dt$pred=predict(glm_box1,
                  newdata = data.frame(rank_node=seq(0, max(dt$L), length.out = length(dt$L))),
                  type="response")

#histogram
png("2e_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
             col = cols[1],
             main="average #m+v vs rank node",
             xlab= "rank node",
             ylab="average #m+v",
             pch=19))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
with(dt, lines(dt$pred~dt$L,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#1: m+v buds ~ parent lenght(cm)+normal normal_distance+ rank_node+length(node)?####
glm_box1=glm(m_v~length_cm+normal_distance+rank_node+length_nodes,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#2: m+v buds ~ parent lenght(cm)+normal_distance+ rank_node?####
glm_box1=glm(m_v~length_cm+normal_distance+rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#3: m+v buds ~ normal_distance+ rank_node?####
glm_box1=glm(m_v~normal_distance+rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#we have to choose if weeping normal_distance or rank node 

#normal_distance
nline=length(unique(sort(PROL_met_scale$normal_distance)))
mv=grep("m_v", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$normal_distance))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$normal_distance==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$normal_distance==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$normal_distance==L,mv]), digits=2)
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

#histogram
png("2f_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
             col = cols[1],
             main="average #m+v vs normal_distance",
             xlab= "normal_distance",
             ylab="average #m+v",
             pch=19))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
dev.off()

#4: m+v buds ~ normal_distance?####
glm_box1=glm(m_v~normal_distance,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes

#permutations_distance
null_1=glm(m_v~1,family = "poisson",data = PROL_met_scale)
summary(null_1)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$normal_distance=sample(PROL_met_scale$normal_distance)
  perm=glm(m_v~normal_distance,family = "poisson",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)>diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#normal_distance
nline=length(unique(sort(PROL_met_scale$normal_distance)))
mv=grep("m_v", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$normal_distance))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$normal_distance==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$normal_distance==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$v==L,mv]), digits=2)
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

#df: predict~m+v
dt$pred=predict(glm_box1,
                newdata = data.frame(normal_distance=seq(0, max(dt$L), length.out = length(dt$L))),
                type="response")

#histogram
png("2g_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(dt,plot(dt$av~dt$L,
             col = cols[1],
             main="average #m+v vs normal_distance",
             xlab= "normal_distance",
             ylab="average #m+v",
             pch=19))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
with(dt, lines(dt$pred~dt$L,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()
