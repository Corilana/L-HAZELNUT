wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/glm"))
library(stats)
library(dplyr)
library(RColorBrewer)
library(effects)

met=read.csv(paste0(wd,"DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
sh_ty=grep("shoot_type", names(met))
met[met$shoot_type=="SYLLEPTIC",sh_ty]=1
met[met$shoot_type=="PROLEPTIC",sh_ty]=0
met$shoot_type=as.numeric(met$shoot_type)

#box3:proportion of V
#glm (formula = cbind(Successes, Failures) ~ other variables, family = binomial, data=df)
SYL_met_scale=met[met$shoot_type==1,]#df at bud scale of buds in sylleptic shoots
#change columns names to not make confusion
a=grep("^Length$", names(SYL_met_scale))
b=grep("^Length.", names(SYL_met_scale))
c=grep("rank_", names(SYL_met_scale))
d=grep("distance_abs", names(SYL_met_scale))
e=grep("tot_", names(SYL_met_scale))
colnames(SYL_met_scale)[c(a,b,c,d,e)]=c("parent_length_cm",
                                          "parent_length_nodes",
                                          "parent_rank_node",
                                          "distance",
                                          "tot_buds_in_sylleptic")
SYL_met_scale$tot_buds_in_sylleptic=SYL_met_scale$tot_buds_in_sylleptic-1#REMOVING THE COUNTING OF CATKINS BECAUSE ALL SYLLEPTIC HAS CATKINS

#parameters: length(cm), length(node), rank_node, distance

#1:proportion of V ~length(cm), length(node), rank_node, distance, m_v ?####
glm_box1=glm(cbind(v,m)~parent_length_cm+parent_length_nodes+parent_rank_node+distance+tot_buds_in_sylleptic,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes
#2:proportion of V ~length(cm), length(node), distance, m_v ?####
glm_box1=glm(cbind(v,m)~parent_length_cm+parent_length_nodes+distance+tot_buds_in_sylleptic,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes
#3:proportion of V ~length(cm), length(node), m_v?####
glm_box1=glm(cbind(v,m)~parent_length_cm+parent_length_nodes+tot_buds_in_sylleptic,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes
# #permut_ length
# null_1=glm(cbind(v,m)~parent_length_nodes+tot_buds_in_sylleptic+1,family = "binomial",data = SYL_met_scale)
# dif=glm_box1$aic-null_1$aic
# met_nul=SYL_met_scale
# 
# df=data.frame(matrix(nrow=0, ncol=0))
# for (i in 1:10000) {
#   met_nul$parent_length_cm=sample(SYL_met_scale$parent_length_cm)
#   perm=glm(cbind(v,m)~parent_length_cm+parent_length_nodes+tot_buds_in_sylleptic,family = "binomial",data = met_nul)
#   a=perm$aic-null_1$aic
#   b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# 
# better_perm=length(which(df$b==1))#times better perm!!!

#4:proportion of V ~length(node), m_v?####
glm_box1=glm(cbind(v,m)~parent_length_nodes+tot_buds_in_sylleptic,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes
#graph
png("3d_S.png",width=1200, height=900, res=150)# save plot
with(plot(allEffects(glm_box1)))
dev.off()
#5:proportion of V ~length(node)?####
glm_box1=glm(cbind(v,m)~parent_length_cm+parent_length_nodes,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes

# df %V~length(cm)
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_length_cm))
mv=grep("^tot", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  length=unique(SYL_met_scale$parent_length_cm)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_cm==length,mv])
  V=sum(SYL_met_scale[SYL_met_scale$parent_length_cm==length,v])
  ratio=round(V/MV, digit=2)
  prop=rbind(prop, cbind(length,V,MV, ratio))
}

prop=prop[with(prop, order(length)),]#order according to distance

#df: predict~length
#create a sequence with random numbers between 1 and maximum length (72cm)
df=data.frame(parent_length_cm=seq(1,max(SYL_met_scale$parent_length_cm),length.out = length(unique(SYL_met_scale$distance))))
#create 10 repetitions of sequence with random numbers between 0 and 10 (sequence to simulate distance)
for (i in seq(1,23, by = 3)) {
  df=cbind(df, data.frame(rep(i, length(df$parent_length_cm))))
}
colnames(df)[2:9]="parent_length_nodes"

#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df)-1)) {
  h=i+1
  df=cbind(df,predict(glm_box1,newdata = df[c(1,h)],type="response"))
}

colnames(df)[10:17]=seq(1,23, by = 3)

#plot
png("3a_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=8, name="Set1")
with(prop, plot(prop$ratio~prop$length,
                col = cols[1],
                main="%V (#V/tot mv) vs length(cm) and length(node)",
                xlab= "parent length(cm)",
                ylab="%V",
                ylim=c(0,1),
                type="h",
                lwd=4))
for (i in 1:length(grep("[0-9]", colnames(df)))) {
  t=grep("[0-9]", colnames(df))[i]
  with(df,lines(df[,t]~df$parent_length_cm,col=rbPal[i], lwd=5))
}
legend("top",
       inset = c(-0.4, -0.06),
       horiz=T,
       xpd = TRUE,c("real", "length_nod=1","length_nod=4","length_nod=7","length_nod=10","length_nod=13","length_nod=16","length_nod=19","length_nod=22"),
       pch = c(19,NA,NA,NA,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1,1,1,1), lwd=5,
       col = c(cols[1], rbPal),
       cex=0.5)
dev.off()

#6:proportion of V ~length(cm)?####
glm_box1=glm(cbind(v,m)~parent_length_cm,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#no
#7:proportion of V ~length(node)?####
glm_box1=glm(cbind(v,m)~parent_length_nodes,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes

#graph
png("3e_S.png",width=1200, height=900, res=150)# save plot
with(plot(allEffects(glm_box1)))
dev.off()

# #permut_ length
# null_1=glm(cbind(v,m)~1,family = "binomial",data = SYL_met_scale)
# dif=glm_box1$aic-null_1$aic
# met_nul=SYL_met_scale
# 
# df=data.frame(matrix(nrow=0, ncol=0))
# for (i in 1:10000) {
#   met_nul$parent_length_nodes=sample(SYL_met_scale$parent_length_nodes)
#   perm=glm(cbind(v,m)~parent_length_nodes,family = "binomial",data = met_nul)
#   a=perm$aic-null_1$aic
#   b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# 
# better_perm=length(which(df$b==1))#times better perm!!!

# df %V~length(NODE)
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$parent_length_nodes))
mv=grep("^tot", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  length=unique(SYL_met_scale$parent_length_nodes)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$parent_length_nodes==length,mv])
  V=sum(SYL_met_scale[SYL_met_scale$parent_length_nodes==length,v])
  ratio=round(V/MV, digit=2)
  prop=rbind(prop, cbind(length,V,MV, ratio))
}

prop=prop[with(prop, order(length)),]#order according to distance

#df: predict~length
#create a sequence with random numbers between 1 and maximum length (72cm)
prop$pred=predict(glm_box1,
                  newdata = data.frame(parent_length_nodes=seq(1, max(prop$length), length.out = length(prop$length))),
                  type="response")

#plot
png("3_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=8, name="Set1")
with(prop, plot(prop$ratio~prop$length,
                col = cols[1],
                main="%V (#V/tot mv) vs length(node)",
                xlab= "parent length(node)",
                ylab="%V",
                ylim=c(0,1),
                type="h",
                lwd=4))
with(prop, lines(prop$pred~prop$length,lwd=5, col=cols[2]))
legend("top",
       horiz=T,
       xpd = TRUE, legend = c("real", "predicted"),fill = cols, cex=0.6)
dev.off()

#try: length and rank are related. has the ratio an effect?####
SYL_met_scale$inter=SYL_met_scale$parent_length_cm/SYL_met_scale$parent_length_nodes

#1:proportion of V ~rank_node,inter, distance ?####
glm_box1=glm(cbind(v,m)~inter+parent_rank_node+distance,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes

#2:proportion of V ~rank_node,inter?####
glm_box1=glm(cbind(v,m)~inter+parent_rank_node,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#yes

#3:proportion of V ~rank_node?####
glm_box1=glm(cbind(v,m)~parent_rank_node,family = "binomial",data = SYL_met_scale)
summary(glm_box1)#no

# #permut_ rank
# null_1=glm(cbind(v,m)~1,family = "binomial",data = SYL_met_scale)
# dif=glm_box1$aic-null_1$aic
# met_nul=SYL_met_scale
# 
# df=data.frame(matrix(nrow=0, ncol=0))
# for (i in 1:10000) {
#   met_nul$parent_rank_node=sample(SYL_met_scale$parent_rank_node)
#   perm=glm(cbind(v,m)~parent_rank_node,family = "binomial",data = met_nul)
#   a=perm$aic-null_1$aic
#   b=a<dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
#   r=cbind(i,a, b)
#   df=rbind(df,r)
# }
# 
# better_perm=length(which(df$b==1))#times better perm!!!

# df %V~length(cm)
prop=met[0,0]#empty df
nline=length(unique(SYL_met_scale$inter))
mv=grep("^tot", colnames(SYL_met_scale))
v=grep("^v", colnames(SYL_met_scale))
for (i in 1:nline) {
  length=unique(SYL_met_scale$inter)[i]
  MV=sum(SYL_met_scale[SYL_met_scale$inter==length,mv])
  V=sum(SYL_met_scale[SYL_met_scale$inter==length,v])
  ratio=round(V/MV, digit=2)
  prop=rbind(prop, cbind(length,V,MV, ratio))
}

prop=prop[with(prop, order(length)),]#order according to distance

#no rank node is not linked
#plot
png("3d_S.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[3:4]
rbPal <- brewer.pal(n=8, name="Set1")
with(prop, plot(prop$ratio~prop$length,
                col = cols[1],
                main="%V (#V/tot mv) vs internode length(cm)",
                xlab= "internode length(cm)",
                ylab="%V",
                ylim=c(0,1),
                type="h",
                lwd=4))
dev.off()

#let's keep proportion of V in sylleptic as a constant (av+-se)####
ratio=SYL_met_scale$v/SYL_met_scale$tot_buds_in_sylleptic
av=mean(ratio, na.rm = T)
se=std.error(ratio, na.rm = T)

