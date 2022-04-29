#box3:proportion of V
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

library(stats)
library(dplyr)
library(RColorBrewer)

#metamer scale ####
met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met[met$shoot_type=="SYLLEPTIC",21]=1
met[met$shoot_type=="PROLEPTIC",21]=0
met$shoot_type=as.numeric(met$shoot_type)

#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
PROL_met_scale=met[met$shoot_type==0,]#df at met scale proleptic shoots
#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,16,18)]=c("length_cm",
                                             "length_nodes",
                                             "rank_node",
                                             "distance",
                                             "tot_buds",
                                             "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#REMOVING THE COUNTING OF CATKINS BECAUSE ALL PROLLEPTIC HAS CATKINS

#1: m+v buds ~ parent lenght(cm)+distance+ rank_node?####
glm_box1=glm(m_v~length_cm+distance+rank_node,family = "poisson",data = PROL_met_scale)

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
#because it seems that <rank5 there are not m+v and after they start i make the analysis <5 and >5####
#rank <5=0 and rank>5=1
#1: m+v buds ~ ~length_cm+distance+rank_node?####
PROL_met_scale[PROL_met_scale$rank_node<5,7]=0
PROL_met_scale[PROL_met_scale$rank_node>=5,7]=1
glm_box1=glm(m_v~length_cm+distance+rank_node,family = "poisson",data = PROL_met_scale)
summary(glm_box1)#yes
#2: m+v buds ~ rank_node?####
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
                newdata = data.frame(rank_node=seq(0, 1)),
                type="response")
#histogram
png("2h_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
gr=barplot(dt$av~dt$L,
           col = cols[1],
           main="average #m+v vs rank node",
           xlab= "rank node (0--><5; 1-->>=5)",
           ylab="average #m+v",
           pch=19,
           ylim=c(0,2))
with(arrows(x0 = gr,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.5,
            col=cols[1],
            lwd=5))
with(dt, points(x = gr, y = dt$pred,lwd=5, col=cols[2], type="h"))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

t.test(m_v~rank_node, data=PROL_met_scale)#yes

#1:b~length+rank+distance+length?####
glm_box1=glm(b~length_cm+rank_node+distance+length_nodes,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#2:b~length+rank+length?####
glm_box1=glm(b~length_cm+rank_node+length_nodes,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#we have to choose if keeping distance or rank node or length
#rank
nline=length(unique(sort(PROL_met_scale$rank_node)))
mv=grep("^b$", colnames(PROL_met_scale))
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
png("2i_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
gr=barplot(dt$av~dt$L,
           col = cols[1],
           main="average #b vs rank node",
           xlab= "rank node (0--><5; 1-->>=5)",
           ylab="average #b",
           pch=19,
           ylim=c(0,2))
with(arrows(x0 = gr,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.5,
            col=cols[1],
            lwd=5))
dev.off()

#length_cm
nline=length(unique(sort(PROL_met_scale$length_cm)))
mv=grep("^b$", colnames(PROL_met_scale))
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
png("2j_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(plot(dt$av~dt$L,
          col = cols[1],
          main="average #b vs length(cm)",
          xlab= "length(cm)",
          ylab="average #b",
          pch=19,
          ylim=c(0,2)))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.01,
            col=cols[1],
            lwd=1))
dev.off()

#length_node
nline=length(unique(sort(PROL_met_scale$length_nodes)))
mv=grep("^b$", colnames(PROL_met_scale))
dt=met[0,0]

for (i in 1:nline) {
  L=unique(sort(PROL_met_scale$length_nodes))[i]
  av=round(mean(PROL_met_scale[PROL_met_scale$length_nodes==L,mv]), digits = 2)
  n=length(PROL_met_scale[PROL_met_scale$length_nodes==L,mv])
  se=round(std.error(PROL_met_scale[PROL_met_scale$length_nodes==L,mv]), digits=2)
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
png("2k_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
with(plot(dt$av~dt$L,
          col = cols[1],
          main="average #b vs length_nodes",
          xlab= "length_nodes",
          ylab="average #b",
          pch=19,
          ylim=c(0,2)))
with(arrows(x0 = dt$L,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.01,
            col=cols[1],
            lwd=1))
dev.off()


#3:b~length+rank?####
glm_box1=glm(b~length_cm+rank_node,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes
#4:b~rank?####
glm_box1=glm(b~rank_node,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#yes

#rank
nline=length(unique(sort(PROL_met_scale$rank_node)))
mv=grep("^b$", colnames(PROL_met_scale))
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
                newdata = data.frame(rank_node=seq(0, 1)),
                type="response")
#histogram
png("2l_p.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
gr=barplot(dt$av~dt$L,
           col = cols[1],
           main="average #b vs rank node",
           xlab= "rank node (0--><5; 1-->>=5)",
           ylab="average #b",
           pch=19,
           ylim=c(0,2))
with(arrows(x0 = gr,                           # Add error bars
            y0 = dt$av + dt$se,
            y1 = dt$av - dt$se,
            angle = 90,
            code = 3,
            length = 0.5,
            col=cols[1],
            lwd=5))
with(dt, points(x = gr, y = dt$pred,lwd=5, col=cols[2], type="h"))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

t.test(b~rank_node, data=PROL_met_scale)#yes



met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met[met$shoot_type=="SYLLEPTIC",21]=1
met[met$shoot_type=="PROLEPTIC",21]=0
met$shoot_type=as.numeric(met$shoot_type)

#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
PROL_met_scale=met[met$shoot_type==0,]#df at met scale proleptic shoots
#change columns names to not make confusion
colnames(PROL_met_scale)[c(2,6,7,8,16,18)]=c("length_cm",
                                            "length_nodes",
                                            "rank_node",
                                            "distance",
                                            "tot_buds",
                                            "m_v")
PROL_met_scale$m_v=PROL_met_scale$m_v-PROL_met_scale$b#REMOVING THE COUNTING OF CATKINS BECAUSE ALL PROLLEPTIC HAS CATKINS

#parameters: length(cm), length(node), rank_node, distance
#1:proportion of V ~length+length+rank+distance?####
glm_box1=glm(cbind(v,m)~length_cm+length_nodes+rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#2:proportion of V ~length+rank+distance?####
glm_box1=glm(cbind(v,m)~length_cm+rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#3:proportion of V ~rank+distance?####
glm_box1=glm(cbind(v,m)~rank_node+distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#permut_rank
null_1=glm(cbind(v,m)~distance+1,family = "binomial",data = PROL_met_scale)
dif=glm_box1$aic-null_1$aic
met_nul=PROL_met_scale

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$rank_node=sample(PROL_met_scale$rank_node)
  perm=glm(cbind(v,m)~rank_node+distance,family = "binomial",data = met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

#we have to chose between rank and distance
#rank
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$rank_node))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$rank_node)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$rank_node==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$rank_node==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

#plot
png("3_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs rank_node",
                xlab= "rank_node",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$distance==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$distance==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

#plot
png("3a_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs distance",
                xlab= "distance",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
dev.off()

#4:proportion of V ~distance?####
glm_box1=glm(cbind(v,m)~distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$distance==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$distance==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

prop=prop[order(prop$rank),]

prop$pred=predict(glm_box1,
                newdata = data.frame(distance=seq(0, max(prop$rank), length.out = length(prop$rank))),
                type="response")


#plot
png("3b_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs distance",
                xlab= "distance",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
with(prop, lines(prop$pred~prop$rank,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()


#1:proportion of V ~length+length+rank+normal distance?####
glm_box1=glm(cbind(v,m)~length_cm+length_nodes+rank_node+normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#2:proportion of V ~length+rank+normal distance?####
glm_box1=glm(cbind(v,m)~length_nodes+rank_node+normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#3:proportion of V ~rank+normal distance?####
glm_box1=glm(cbind(v,m)~rank_node+normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#4:proportion of V ~distance?####
glm_box1=glm(cbind(v,m)~normal_distance,family = "binomial",data = PROL_met_scale)
summary(glm_box1)#no

#distance
prop=met[0,0]#empty df
nline=length(unique(PROL_met_scale$normal_distance))
mv=grep("^m_v", colnames(PROL_met_scale))
v=grep("^v", colnames(PROL_met_scale))
for (i in 1:nline) {
  rank=unique(PROL_met_scale$normal_distance)[i]
  MV=sum(PROL_met_scale[PROL_met_scale$normal_distance==rank,mv])
  V=sum(PROL_met_scale[PROL_met_scale$normal_distance==rank,v])
  ratio=V/MV
  prop=rbind(prop, cbind(rank,V,MV, ratio))
}

prop=prop[order(prop$rank),]

prop$pred=predict(glm_box1,
                  newdata = data.frame(normal_distance=seq(0, max(prop$rank), length.out = length(prop$rank))),
                  type="response")


#plot
png("3c_P.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=6, name="Set1")
with(prop, plot(prop$ratio~prop$rank,
                col = cols[1],
                main="%V (#V/tot m+v) buds in proleptic vs normal_distance",
                xlab= "normal_distance",
                ylab="%V",
                ylim=c(0,1), type="h", lwd=5))
with(prop, lines(prop$pred~prop$rank,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

