#EXPLORATORY ANALYSIS
wd="C:/Users/franc/Google Drive/PhD/Deruta/"
setwd(paste0(wd,"R/auto/Lateral/exploratory/"))

#importiamo la liberia
library(dplyr)
library(tibble)
library(RColorBrewer)
library(tidyr)
library(rstatix)

#met level
met=read.csv(paste0(wd, "DF/auto/mtp use/met_level_develop_lateralbuds.csv"))
met <- dplyr::mutate(met, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))

#bud level
bud=read.csv(paste0(wd,"DF/auto/mtp use/bud_level_LATERALS.csv"))
bud <- dplyr::mutate(bud, class = factor(class,levels = c("Sh", "Me", "Lo", "VLo")))
bud <- dplyr::mutate(bud, length.newshoots = factor(length.newshoots,levels = c("Sh", "Me", "Lo", "VLo")))

#1: quante nocciole per rango nodo? nei prolettici?
pro_met=met[met$shoot_type=="PROLEPTIC",]
pro=bud[bud$from_=="PROL", 1:23]
ds=table(pro$rank_node,pro$length.newshoots,pro$fate)
M=as.data.frame.matrix(ds[,, "M"])#BECAUSE ONLY M GIVE NUTS

#fare una tabella con_ numero di nocciole, numero di fiori M e numero di nuovi germogli
sh=grep("^shoot$", colnames(pro))
fa=grep("^fate$", colnames(pro))
nu=grep("^nu$", colnames(pro_met))
ne=grep("^new_", colnames(pro))
len=grep("^length2", colnames(pro))

nliner=length(as.numeric(rownames(M)))
df=bud[0,0]
for (rank in 1:nliner) {
    P=pro[pro$rank_node==rank,]
    n_sh=dim(P[sh])[1]#number shoots
    n_m=length(which(P[fa]=="M"))#number mixed buds
    n_v=length(which(P[fa]=="V"))#number vegetative buds
    Q=pro_met[pro_met$rank_node==rank,]
    n_nuts=sum(Q[nu])#number nuts
    nut_set=(n_nuts/n_m)*100#nut set
    n_new=sum(P[P$fate=="M",ne])#number new shoots from M
    len_new=mean(P[P$fate=="M",len], na.rm = T)#average length of glomerule
    X=cbind(rank,n_sh,n_m,n_v,n_nuts,nut_set,n_new,len_new)
    df=rbind(df,X)
}

df=cbind(df,M)
colnames(df)[9:11]=c("Sh_fromM","Me_fromM","Lo_fromM")

#relationship between rank node and nuts/mixed/ye/news short shoots
png("nuts.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,4))
col=brewer.pal(n=4,name="Set1")
with(df, plot(n_nuts~rank, pch=19, col=col[1],main="freq_nuts_for all lengths in proleptic", xlab="rank nodes", ylab="#/%", type="o", ylim=c(0,360)))
with(df, points(df$n_m~rank, pch=19, col=col[2],type="o"))
with(df, points(df$Sh_fromM~rank, pch=19, col=col[3],type="o"))
with(df, points(df$nut_set~rank, pch=19, col=col[4],type="o"))
legend("topright", legend=c("#nuts", "#mixed","#Sh child from M buds","nut_set*100"),pch = 19, cex= 0.75, col=col)
dev.off()

#relationship between average lenght new glomerul and number of nuts and ye per rank node
png("nuts4.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,4,4))
col=brewer.pal(n=5,name="Set1")
with(df, plot(len_new~rank, pch=19, col=col[5],xlab="rank nodes", ylab="#/%", type="o", ylim=c(0,20)))
with(df, points((df$n_nuts/10)~rank, pch=19, col=col[1],type="o"))
with(df, points((df$nut_set/100)~rank, pch=19, col=col[4],type="o"))
legend("topleft", legend=c("average length glomerul(cm)","#nuts/10","nut_set"),pch = 19, cex= 0.75, col=col[c(5,1,4)])
dev.off()