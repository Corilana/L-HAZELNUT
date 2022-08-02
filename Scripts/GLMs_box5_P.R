#what is the length of children?
#set working directory
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd, "R/auto/Lateral/glm"))
#import library
library(stats)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(plyr)
library(nnet)
library(effects)
library(plotfunctions)

lat=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
#box4: do the bud (V and M) burst?
PRO_bud_scale=lat[lat$from_=="PROL",]
#change columns names to not make confusion
a=grep("^Length$", names(PRO_bud_scale))
b=grep(".node.$", names(PRO_bud_scale))
c=grep("rank", names(PRO_bud_scale))
d=grep("abs", names(PRO_bud_scale))
e=grep("tot_", names(PRO_bud_scale))
colnames(PRO_bud_scale)[c(a,b,c,d,e)]=c("length_cm","length_node","rank_node","distance","m_v")
MV=PRO_bud_scale[(PRO_bud_scale$fate=="M"|PRO_bud_scale$fate=="V")&PRO_bud_scale$new_shoots,]

MV$sib=MV$v+MV$m+MV$b
MV$normal_median=MV$median_distance/MV$length_node

#parameters: length(cm), length(node), rank_node, distance, m, v
#M and V####
MV$fate=factor(MV$fate, levels = c("V","M"))
str(MV)

colnames(MV)[24]="nodes"
#plot
# plot(density(MV$length2yo.cm.))
#1:length~length+lengthnode+rank+median_distance+sib+fate####
glm_box1 = glm(length2yo.cm.~length_cm*fate+rank_node*fate+normal_median*fate+sib*fate, data = MV, family = "gaussian")
summary(glm_box1)
#remove sib
#2:length~length+lengthnode+rank+median_distance+fate####
glm_box1 = glm(length2yo.cm.~length_cm*fate+rank_node*fate+normal_median*fate, data = MV, family = "gaussian")
summary(glm_box1)
#remove rank
#3:length~length+lengthnode+median_distance+fate####
glm_box1 = glm(length2yo.cm.~length_cm*fate+normal_median*fate, data = MV)
summary(glm_box1)

# #save results as table
# library(xtable)
# out=xtable(glm_box1)
# print.xtable(out, type="html", file="tab4.html")
# library(pagedown)
# chrome_print("tab4.html", output = "tab4.pdf")

# #plot all effects
# png("51_P.png",width=1200, height=900, res=150)# save plot
# with(plot(allEffects(glm_box1)))
# dev.off()

#create real data
#real_data
#add a column with division according to normalized distance from median node
summary(MV$normal_median)
nline=dim(MV)[1]
a=-0.5
b=-0.25
c=0.25
d=0.5
for (i in 1:nline) {
  if (MV[i,"normal_median"]>=a&MV[i,"normal_median"]<b) {MV[i,"class_distance"]="proximal"}
  if (MV[i,"normal_median"]>=b&MV[i,"normal_median"]<=c) {MV[i,"class_distance"]="median"}
  if (MV[i,"normal_median"]>c&MV[i,"normal_median"]<=d) {MV[i,"class_distance"]="distal"}
}
MV$class_distance=as.factor(MV$class_distance)

#real data
prop=MV[0,0]
for (j in levels(MV$fate)){
  fate=j
  df=MV[MV$fate==fate,]
  for(i in 1:length(levels(MV$class_distance))){
    prop_df=as.data.frame(table(df$length2yo.cm.,df$length_cm,df$class_distance)[,,i])
    pos=levels(MV$class_distance)[i]
    prop_df["position"]=pos
    prop_df["fate"]=fate
    prop=rbind(prop, prop_df)
  }
}

names(prop)[1:3]=c("length_new_shoots","length_parent","freq")
prop$length_new_shoots=as.numeric(as.character(prop$length_new_shoots))
prop$length_parent=as.numeric(as.character(prop$length_parent))
prop$position=factor(prop$position, levels = c("distal", "median", "proximal"))
prop$fate=factor(prop$fate, levels = c("V", "M"))

#mean of values with the same length
mean.freq=prop[0,0]

for (q in levels(prop$fate)) {
  fate=q
  for (j in levels(prop$position)) {
  position=j
    for (i in unique(sort(prop$length_parent))) {
      length=i
      new_shoots=mean(rep(prop[prop$fate==q&prop$position==j&prop$length_parent==i,1],prop[prop$fate==q&prop$position==j&prop$length_parent==i,3]))
      sd=sd(rep(prop[prop$fate==q&prop$position==j&prop$length_parent==i,1],prop[prop$fate==q&prop$position==j&prop$length_parent==i,3]))
      mean.freq=rbind(mean.freq,cbind(fate,position,new_shoots,sd,length))
    }
  }
}

mean.freq=mean.freq[!is.nan(as.numeric(mean.freq$new_shoots)),]
mean.freq$length=as.numeric(mean.freq$length)
mean.freq$new_shoots=as.numeric(mean.freq$new_shoots)
mean.freq$position=factor(mean.freq$position, levels = c("distal", "median", "proximal"))
mean.freq$fate=factor(mean.freq$fate, levels = c("V", "M"))

#create a sequence with random numbers between 1 and maximum length_cm 
length.parent=unique(sort(prop$length_parent))
df=data.frame(length_cm=rep(length.parent,2))
df$fate=c(rep("V",length(length.parent)),rep("M",length(length.parent)))
df$fate=factor(df$fate, levels = c("V", "M"))

#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate normal_median)
levels(prop$position)
#quindi metto prima +0.5(distal) e poi proximal(-0.5)
for (j in c(0.5,0.25, 0,-0.25, -0.5)) {
  df=cbind(df, data.frame(rep(j, length(df$fate))))
}

#rename columns
colnames(df)[3:7]="normal_median"
conf_int=df
#predict model according to normal_median, for each type of sequence of distance (0-10)
for (i in grep("normal_median", colnames(df))) {
  pred=predict(glm_box1,newdata = df[c(1,2,i)],type="response", se.fit=T)
  df=cbind(df,pred$fit)
}
#rename columns
colnames(df)[8:12]=c(0.5,0.25, 0,-0.25, -0.5)
#remove negative values
dupli=df[8:12]
dupli[dupli<0]=NA
df[8:12]=dupli

df_M=df[df$fate=="M",]
df_V=df[df$fate=="V",]

#confidence intervel
for (i in grep("normal_median", colnames(df))) {
  pred=predict(glm_box1,newdata = conf_int[c(1,2,i)],se.fit = T)
  lw=pred$fit+qnorm(0.025)*pred$se.fit
  up=pred$fit+qnorm(0.975)*pred$se.fit
  lim=as.data.frame(cbind(lw,up))
  conf_int=cbind(conf_int,lim)
}
#remove negative values
dupl=conf_int[c(8:17)]
dupl[dupl<0]=0
conf_int[c(8:17)]=dupl

conf_int=conf_int[c(1,2,10,9,14,11,16,15)]
colnames(conf_int)[c(3,5,7)]="lw"
colnames(conf_int)[c(4,6,8)]="up"

conf_M=conf_int[conf_int$fate=="M",]
conf_V=conf_int[conf_int$fate=="V",]

M=mean.freq[mean.freq$fate=="M",]
V=mean.freq[mean.freq$fate=="V",]

# graph
png("5c1_P.png",width=1200, height=900, res=150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
transp<-alphaPalette(rbPal, rep(0.25,6))
par(mfrow=c(1,2))
par(mar=c(5,4,1,0)+0.1)
#real data
with(M,plot(new_shoots~length,
            main="fate M",
              ylab = "new shoot length (cm)",
              xlab = "parent length(cm)",
            col = rbPal[M$position],
            ylim=c(0,20),
            xlim=c(0,75),
            pch=c(15:17)[M$position]))
for (i in 1:length((df_M)[8:10])) {
  t=colnames(df_M)[8:10][i]
  with(df_M,lines(df_M[,t]~df_M$length_cm,col=rbPal[i], lwd=3))
}
for (i in 1:length(grep("lw", colnames(conf_M)))) {
    j=grep("lw", colnames(conf_M))[i]
    t_lw=j
    t_up=j+1
    with(conf_M[c(1,t_lw:t_up)], polygon(x=c(rev(conf_M$length_cm),conf_M$length_cm),
                      y=c(rev(lw),up),
                      col=transp[i], border = NA))
}
legend("top",
       horiz=T,
       title="position in the shoot",
       xpd = TRUE,
       c("distal","median","proximal"),
       pch=c(15:17),
       col = rbPal[1:3],
       cex=1)
#real data
par(mar=c(5,0,1,4)+0.1)
with(V,plot(new_shoots~length,
            main="fate V",
            yaxt='n',
            ylab=NA,
            xlab = "parent length(cm)",
            col = rbPal[V$position],
            ylim=c(0,20),
            xlim=c(0,75),
            pch=c(15:17)[V$position]))
for (i in 1:length((df_V)[8:10])) {
  t=colnames(df_V)[8:10][i]
  with(df_V,lines(df_V[,t]~df_V$length_cm,col=rbPal[i], lwd=3))
}
for (i in 1:length(grep("lw", colnames(conf_V)))) {
  j=grep("lw", colnames(conf_V))[i]
  t_lw=j
  t_up=j+1
  with(conf_V[c(1,t_lw:t_up)], polygon(x=c(rev(conf_V$length_cm),conf_V$length_cm),
                                       y=c(rev(lw),up),
                                       col=transp[i], border = NA))
}
legend("top",
       horiz=T,
       title="position in the shoot",
       xpd = TRUE,
       c("distal","median","proximal"),
       pch=c(15:17),
       col = rbPal[1:3],
       cex=1)
dev.off()
