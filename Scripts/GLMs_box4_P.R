#are you bursting?

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

wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))

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
MV=PRO_bud_scale[PRO_bud_scale$fate=="M"|PRO_bud_scale$fate=="V",]#for fate= "M"
MV$sib=MV$v+MV$m+MV$b
MV$normal_median=MV$median_distance/MV$length_node

V=MV[MV$fate=="V",]#for fate= "V"
M=MV[MV$fate=="M",]#for fate= "M"
#parameters: length(cm), length(node), rank_node, distance, m, v
#M and V
MV$fate=as.factor(MV$fate)
MV$fate=relevel(MV$fate, "V")

#1: new_shoots~fate*sib+fate*parent_length_cm+fate*parent_rank_node+fate*normal_median+fate*m+fate*v####
glm_box1 = glm(new_shoots~fate*sib+fate*length_cm+fate*rank_node+fate*normal_median, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)

#fate is not significant

#2: new_shoots~sib+parent_length_cm+parent_rank_node+normal_median####
glm_box1 = glm(new_shoots~sib+length_cm+rank_node+normal_median, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)

#permut_ rank####
null_1=glm(new_shoots~sib+length_cm+1+normal_median, family="binomial",data=MV)
dif=glm_box1$aic-null_1$aic
met_nul=MV

df=data.frame(matrix(nrow=0, ncol=0))
for (i in 1:10000) {
  met_nul$distance=sample(MV$distance)
  perm=glm(new_shoots~sib+length_cm+rank_node+normal_median, family="binomial",data=met_nul)
  a=perm$aic-null_1$aic
  b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
  r=cbind(i,a, b)
  df=rbind(df,r)
}

better_perm=length(which(df$b==1))#times better perm!!!

# #graph####
# png("4p_P.png",width=1200, height=900, res=150)# save plot
# with(plot(allEffects(glm_box1)))
# dev.off()
#####
#the graph in the paper will be done with the most significant factor.
#we found it with shapley function
library(shapr)
x_var <-c("sib", "length_cm", "rank_node", "normal_median")
y_var <- "new_shoots"

x_train <- as.matrix(MV[-1:-6,x_var])
y_train <- MV[-1:-6,y_var]
x_test <- as.matrix(MV[1:6,x_var])

explainer <- shapr(x_train, glm_box1)
p <- mean(y_train)

explanation <- explain(x_test,
                       approach="empirical",
                       explainer = explainer,
                       prediction_zero = p)
print(explanation$dt)
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1:6))

#rimuoviamo length_cm che è quello meno impattante in tutti

#3: new_shoots~sib+parent_rank_node+normal_median####
glm_box1 = glm(new_shoots~sib+rank_node+normal_median, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)
#remove rank_node

#4: new_shoots~sib+normal_median####
glm_box1 = glm(new_shoots~sib+normal_median, family="binomial",data=MV)#first check tot_buds to see if siblings are impo
summary(glm_box1)

# png("4p_P1.png",width=1200, height=900, res=150)# save plot
# with(plot(allEffects(glm_box1)))
# dev.off()

#####
# #the graph in the paper will be done with the most significant factors.
# #we found it with shapley function
# library(shapr)
# x_var <-c("sib", "normal_median")
# y_var <- "new_shoots"
# 
# x_train <- as.matrix(MV[-1:-6,x_var])
# y_train <- MV[-1:-6,y_var]
# x_test <- as.matrix(MV[1:6,x_var])
# 
# explainer <- shapr(x_train, glm_box1)
# p <- mean(y_train)
# 
# explanation <- explain(x_test,
#                        approach="empirical",
#                        explainer = explainer,
#                        prediction_zero = p)
# print(explanation$dt)
# plot(explanation, plot_phi0 = FALSE, index_x_test = c(1:6))
# #save results as table
# library(xtable)
# out=xtable(glm_box1)
# print.xtable(out, type="html", file="tab4.html")
# library(pagedown)
# chrome_print("tab4.html", output = "tab4.pdf")
#####

#distance
prop=MV[0,0]#empty df
nline=length(unique(MV$sib))
tot_shoots=grep("_shoots", colnames(MV))
for (i in 1:nline) {
  other=unique(MV$sib)[i]
  tot=length(MV[MV$sib==other,tot_shoots])
  yes=sum(MV[MV$sib==other,tot_shoots])
  ratio=yes/tot
  prop=rbind(prop, cbind(other,yes,tot,ratio))
}

prop=prop[order(prop$other),]
#create a sequence with random numbers between 1 and maximum sib 
df=data.frame(sib=seq(min(MV$sib),
                                max(MV$sib),
                                length.out = length(unique(MV$sib))))

#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate normal_median)
for (j in c(-0.5, -0.25, 0, 0.25, 0.5)) {
  df=cbind(df, data.frame(rep(j, length(df$sib))))
}
#rename columns
colnames(df)[2:6]="normal_median"

conf_int=df
#predict model according to normal_median, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  pred=predict(glm_box1,newdata = df[c(1,h)],type="response", se.fit=T)
  df=cbind(df,pred$fit)
}
#rename columns
colnames(df)[7:11]=c(-0.5, -0.25, 0, 0.25, 0.5)
#confidence intervel
for (i in 1:length(unique(MV$sib))) {
  h=i+1
  pred=predict(glm_box1,newdata = conf_int[c(1,h)],se.fit = T)
  lw=plogis(pred$fit+qnorm(0.025)*pred$se.fit)
  up=plogis(pred$fit+qnorm(0.975)*pred$se.fit)
  lim=as.data.frame(cbind(lw,up))
  conf_int=cbind(conf_int,lim)
}

# #graph
# png("4_P2.png",width=1200, height=900, res=150)# save plot
# cols<-brewer.pal(n=4,name="Set2")[3:4]
# rbPal <- brewer.pal(n=6, name="Set1")
# transp=alphaPalette(rbPal, rep(0.25,6))
# with(prop, plot(prop$ratio~prop$other,
#                 col = cols[1],
#                 xlab= "sibling buds (V or M)",
#                 ylab="new shoots proportion",
#                 ylim=c(0,1), pch=19))
# for (i in 1:length((df)[7:11])) {
#   t=colnames(df)[7:11][i]
#   with(df,lines(df[,t]~df$sib,col=rbPal[i], lwd=3))
# }
# for (i in 1:length(grep("lw", colnames(conf_int)))) {
#   j=grep("lw", colnames(conf_int))[i]
#   id=1
#   t_lw=j
#   t_up=j+1
#   with(conf_int[c(id,t_lw:t_up)], polygon(x=c(seq(8,0, by=-2),seq(0,8, by=2)),
#                     y=c(rev(lw),up),
#                     col=transp[i], border = NA))
# }
# legend("top",
#        horiz=T,
#        xpd = TRUE,c("real", "norm_dist=-0.5","norm_dist=-0.25","norm_dist=0","norm_dist=+0.25","norm_dist=+0.5"),
#        pch = c(19,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1), lwd=5,
#        col = c(cols[1], rbPal),
#        cex=0.6)
# dev.off()

# #normal median
# prop=MV[0,0]#empty df
# nline=length(unique(MV$sib))
# tot_shoots=grep("_shoots", colnames(MV))
# for (i in 1:nline) {
#   other=unique(MV$normal_median)[i]
#   tot=length(MV[MV$normal_median==other,tot_shoots])
#   yes=sum(MV[MV$normal_median==other,tot_shoots])
#   ratio=yes/tot
#   prop=rbind(prop, cbind(other,yes,tot,ratio))
# }
# 
# prop=prop[order(prop$other),]
# #create a sequence with random numbers between 1 and maximum sib 
# df=data.frame(normal_median=c(-0.5, -0.25, 0, 0.25, 0.5))
# 
# #create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate sib)
# for (j in c(0:8)) {
#   df=cbind(df, data.frame(rep(j, length(df$normal_median))))
# }
# #rename columns
# colnames(df)[2:10]="sib"
# #predict model according to normal_median, for each type of sequence of distance (0-10)
# for (i in 1:(ncol(df)-1)) {
#   h=i+1
#   df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
# }
# #rename columns
# colnames(df)[11:19]=c(0:8)
# 
# #graph
# png("4_P3.png",width=1200, height=900, res=150)# save plot
# cols<-brewer.pal(n=4,name="Set2")[3:4]
# rbPal <- brewer.pal(n=9, name="Set1")
# with(prop, plot(prop$ratio~prop$other,
#                 col = cols[1],
#                 xlab= "sibling buds (V or M)",
#                 ylab="new shoots proportion",
#                 ylim=c(0,1), pch=19))
# for (i in 1:length(df[11:19])) {
#   t=colnames(df)[11:19][i]
#   with(df,lines(df[,t]~df$normal_median,col=rbPal[i], lwd=3))
# }
# legend("top",
#        horiz=T,
#        xpd = TRUE,c("real", "sib=0","sib=1","sib=2","sib=3","sib=4"),
#        pch = c(19,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1), lwd=5,
#        col = c(cols[1], rbPal),
#        cex=0.6)
# dev.off()
