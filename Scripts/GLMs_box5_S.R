#box4: do the bud (V and M) burst?
#in syllepotic shoots

#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

#import lybrary
library(stats)
library(dplyr)
library(RColorBrewer)
library(effects)
library(plotrix)

#import dataset
lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#subset dataset
SYL_bud_scale=lat[lat$from_=="SYL",]
#changing column names
colnames(SYL_bud_scale)[c(2,7,8,18)]=c("parent_length_cm","parent_length_node","parent_rank_node","m_v")
#removing the catkins from the count of M and V
SYL_bud_scale$m_v=SYL_bud_scale$m_v-1
#subset dataset_ from V&M
MV=SYL_bud_scale[(SYL_bud_scale$fate=="M"|SYL_bud_scale$fate=="V")&SYL_bud_scale$new_shoots!=0,]
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")
str(MV)

colnames(MV)[24]="nodes"
#plot
plot(density(MV$nodes))

#1:length~length+lengthnode+rank+distance_abs+m_v+fate
glm_box1 = glm(nodes~parent_length_cm*fate+parent_length_node*fate+parent_rank_node*fate+distance_abs*fate+m_v*fate, data = MV, family = "gaussian")
summary(glm_box1)
#2:length~length+lengthnode+rank+distance_abs+fate
glm_box1 = glm(nodes~parent_length_cm*fate+parent_length_node*fate+parent_rank_node*fate+distance_abs*fate, data = MV, family = "gaussian")
summary(glm_box1)
#3:length~length+lengthnode+rank+distance_abs+fate
glm_box1 = glm(nodes~parent_length_cm*fate+parent_length_node+parent_rank_node+distance_abs, data = MV, family = "gaussian")
summary(glm_box1)
#4:length~lengthnode+rank+distance_abs+fate
glm_box1 = glm(nodes~parent_length_node+parent_rank_node+distance_abs, data = MV, family = "gaussian")
summary(glm_box1)
#5:length~rank+distance_abs+fate
glm_box1 = glm(nodes~parent_rank_node+distance_abs, data = MV, family = "gaussian")
summary(glm_box1)
#6:length~distance_abs+fate
glm_box1 = glm(nodes~distance_abs, data = MV, family = "gaussian")
summary(glm_box1)
#graph
plot(allEffects(glm_box1))
#graph
png("5_S.png",width=1200, height=900, res=150)# save plot
with(plot(allEffects(glm_box1)))
dev.off()
#create table with mean and se of number of nodes per distance
dis=sort(unique(MV$distance_abs))
df=lat[0,0]
for (i in dis) {
  av=mean(MV[MV$distance_abs==i,"nodes"])
  se=std.error(MV[MV$distance_abs==i,"nodes"])
  av_se=cbind(i,av,se)
  df=rbind(df,av_se)
}
colnames(df)[1]="distance_abs"
df$pred=predict(glm_box1,
                newdata = data.frame(distance_abs=df$distance_abs), "response")
#grapg
png("52_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
r=plot(df$distance_abs,df$av,
       col = cols[1],pch=19,
       xlab="Distance from median node",
       main="average #node children vs distance",
       ylab="average #node children",
       ylim=c(0,7))
with(arrows(x0 = df$distance_abs,# Add error bars
            y0 = df$av + df$se,
            y1 = df$av - df$se,
            angle = 90,
            code = 3,
            length = 0.05,
            col=cols[1]))
with(lines(df$distance_abs,df$pred, col=cols[2], lwd=3))
dev.off()

#info su length_new shoots in sylleptic
mean(MV$nodes)
std.error(MV$nodes)

#length new shoots is a constant of mean+- se
boxplot(MV$nodes)

