#grafici per oral oregon_2022
#autore: Francesca Grisafi

library(gridExtra);library(janitor);library(readxl);library(dplyr);
library(plotrix);library(FSA)

source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

# average number of nodes per class ---------------------------------------
len=grep("Length",names(met.all.proleptic))
names(met.all.proleptic)[len]="length_node"
df = unique(met.all.proleptic[c("class", "length_node")])
de.av = df[0, 0]

for (i in levels(df$class)) {
  class = i
  q = df[df$class == i, ]
  av = mean(q$length_node)
  tot = cbind(class, av)
  de.av = rbind(de.av, tot)
}
de.av

# distribuzione frequenza numero nodi germogli VLo-------------------------------------
pro = droplevels(met.all.proleptic[met.all.proleptic$class == "VLo", ])
dist = unique(pro[c("shoot_ID", "rank_node")])
dist.table = as.data.frame(table(dist$rank_node))
dist.table
names(dist.table) = c("rank", "nb_shoot_IDs")
dist.table$freq = dist.table$nb_shoot_IDs / length(unique(dist$shoot_ID))
dist.table

# png("images/auto_young_lengthnodedist.png",width = 1200,height = 900,res = 150)
plot(dist.table$freq, main = "YOUNG OWN-ROOTED",  ylab = "frequences of shoot_IDs",
     xlab = "rank node",ylim = c(0, 1),xlim = c(1, 24),type = "h",
     xaxt = "n",lwd = 5)
axis(1, at = seq(1, 24, 1))
# dev.off()

# % lateral buds per each rank in each class ------------------------------
TAB_length = met.all.proleptic[0, 0]
n_length = levels(met.all.proleptic$class)

for (class in n_length) {
  df = met.all.proleptic[met.all.proleptic$class == class, ]
  nline = length(unique(sort(df$rank_node)))
  average_nb_nodes = ceiling(mean(unique(df[c("shoot_ID", "length_node")])[2]$length_node))
  for (q in 1:nline) {
    rank_node = unique(sort(df$rank_node))[q]
    df.node = df[df$rank_node == rank_node, ]
    shoot_IDs_with_rank = length(df.node$shoot_ID)
    sylleptic = sum(df.node$sylleptic)#sylleptic for each parental node?
    v = sum(df.node$v)#v for each parental node?
    m = sum(df.node$m)#m for each parental node?
    b = sum(df.node$b)#b for each parental node?
    TAB_length = rbind(TAB_length,cbind(class,average_nb_nodes,
                                        rank_node,shoot_IDs_with_rank,
                                        sylleptic,v,m,b)
    )
  }
}
TAB_length
str(TAB_length)
TAB_length[2:8] = lapply(TAB_length[2:8], as.numeric)
TAB_length[, "sum_obs"] = rowSums(TAB_length[5:8])#sum obserbations per each node
tail(TAB_length)

# proportions of observations (# bud/ tot obs in that node) ---------------
nline = dim(TAB_length)[1]
names(TAB_length)
for (i in 1:nline) {
  TAB_length[i, 10:13] = round((TAB_length[i, 5:8] / TAB_length[i, 9]) * 100, digit =
                                 2)
}
colnames(TAB_length)[10:13] = c("%sylleptic", "%V", "%M", "%B")
tail(TAB_length)

# png("2022_Oregon/images/auto_difflentgh.png", width = 900,height = 1200,res = 150)# save plot
par(mfrow = c(4, 1))
par(oma = c(5, 1, 1, 1))
mat <- matrix(c(1:4), 4, 1, byrow = T)
layout(mat, widths = 1, heights = c(5, 5, 5, 5))
par(mar = c(1, 4, 0.5, 2))
for (i in unique(TAB_length$class)) {
  sh = TAB_length[TAB_length$class == i &
                    TAB_length$rank_node <= TAB_length$average_nb_nodes, ]
  with(sh,plot(`%sylleptic` ~ rank_node,pch = 1,cex = 1.2,ylim = c(0, 100),
               xlim = c(1, 18),xlab = "",xaxt = "n",ylab = "%",type = "o"))
  with(sh, points(`%V` ~ rank_node,pch = 2,cex = 1.2,type = "o"))
  with(sh, points(`%M` ~ rank_node,pch = 15,cex = 1.2,type = "o"))
  with(sh, points(`%B` ~ rank_node,pch = 18,cex = 1.2,type = "o"))
  axis(1,at = seq(1, 18, 2),labels = NA,tck = 0.05)
  text(14, 98, i)
  
}
legend("topleft",legend = c("%sylleptic", "%V", "%M", "%B"),lty = 1,
       cex = 0.8,pch = c(1, 2, 15, 18),horiz = T)
axis(1, at = seq(1, 18, 2),)
mtext("Rank node",side = 1,line = 3,cex = 1,outer = TRUE)
# dev.off()

#quindi tengo solo il VLo!!
pro_OR_young=sh

# % lateral buds per each rank in each class in sylleptic -----------------
syl_OR_young = met.all.sylleptic[0, 0]
nline = length(unique(sort(met.all.sylleptic$parent_rank_node)))
for (q in 1:nline) {
  Q = unique(sort(met.all.sylleptic$parent_rank_node))[q]
  df = met.all.sylleptic[met.all.sylleptic$parent_rank_node == Q, ]
  parent_rank_node = Q
  shoot_IDs_with_rank = length(df$shoot_ID)
  c = sum(df$c_syl)
  v = sum(df$v_syl)#v for each parental node?
  m = sum(df$m_syl)#m for each parental node?
  b = sum(df$b)#b for each parental node?
  syl_OR_young = rbind(syl_OR_young, cbind(parent_rank_node, shoot_IDs_with_rank, c, v, m, b))
}
head(syl_OR_young)
syl_OR_young["sums", 3:6] = colSums(syl_OR_young[3:6])#sums each observations(obs)
syl_OR_young[, "sum_obs"] = rowSums(syl_OR_young[3:6])#sum obserbations per each node

# proportions of observations (# bud/ tot obs in that node) ---------------
for (i in 1:nline) {
  syl_OR_young[i, 8:11] = round((syl_OR_young[i, 3:6] / syl_OR_young[i, 7]) * 100, digit =
                             2)
}
colnames(syl_OR_young)[8:11] = c("%C", "%V", "%M", "%B")
head(syl_OR_young)

toremove <- grep("^met$|met.sylleptic$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))

#create the df for the nuts####
#importo df con solo i metameri trovati l'anno successivo
met$nut_set = NA
met$clu_set = NA

met[met$shoot_type=="PROLEPTIC","nut_set"]=met[met$shoot_type=="PROLEPTIC","nu"] / met[met$shoot_type=="PROLEPTIC","m"]
met[met$shoot_type=="PROLEPTIC","clu_set"]=met[met$shoot_type=="PROLEPTIC","cl"] / met[met$shoot_type=="PROLEPTIC","m"]
met[met$shoot_type=="SYLLEPTIC","nut_set"]=met[met$shoot_type=="SYLLEPTIC","nu"] / met[met$shoot_type=="SYLLEPTIC","m_syl"]
met[met$shoot_type=="SYLLEPTIC","clu_set"]=met[met$shoot_type=="SYLLEPTIC","cl"] / met[met$shoot_type=="SYLLEPTIC","m_syl"]

met$nut_set[is.nan(met$nut_set)] <- NA
met$clu_set[is.nan(met$clu_set)] <- NA
met$nut_set[is.infinite(met$nut_set)] <- NA
met$clu_set[is.infinite(met$clu_set)] <- NA
# nut set -----------------------------------------------------------------
nut_set=data.frame(matrix(nrow = 3,ncol = 2))
colnames(nut_set)=c("media","se")
rownames(nut_set)=c("generale", "proleptic", "sylleptic")

nut_set[1,"media"] = mean(met$nut_set,na.rm = T)
nut_set[1,"se"] = sd(met$nut_set,na.rm = T)/sqrt(length(met[!is.na(met$nut_set),"nut_set"]))
nut_set[2,"media"] = mean(met[met$shoot_type=="PROLEPTIC","nut_set"],na.rm = T)
nut_set[2,"se"] = sd(met[met$shoot_type=="PROLEPTIC","nut_set"],na.rm = T)/sqrt(length(met[!is.na(met$shoot_type=="PROLEPTIC"&met$nut_set),"nut_set"]))
nut_set[3,"media"] = mean(met[met$shoot_type=="SYLLEPTIC","nut_set"],na.rm = T)
nut_set[3,"se"] = sd(met[met$shoot_type=="SYLLEPTIC","nut_set"],na.rm = T)/sqrt(length(met[!is.na(met$shoot_type=="SYLLEPTIC"&met$nut_set),"nut_set"]))
nut_set

nut_set_class=data.frame(matrix(nrow = 4,ncol = 2))
colnames(nut_set_class)=c("media","se")
rownames(nut_set_class)=levels(met$class)

for (i in levels(met$class)) {
  df=met[met$class==i,]
  nut_set_class[i,"media"]=mean(df$nut_set,na.rm = T)
  nut_set_class[i,"se"]=sd(df$nut_set,na.rm = T)/sqrt(length(df[!is.na(df$nut_set),"nut_set"]))
}
nut_set_class

# tests -------------------------------------------------------------------
plot(density(met$nut_set, na.rm = T))
plot(met$nut_set ~ met$shoot_type)

t.test(met$nut_set ~ met$shoot_type)
nut_set[1,"t-test"]="***"

plot(met$nut_set ~ met$class)

summary(aov(met$nut_set ~ met$class))
nut_set_class[1,"anova"]="***"
TukeyHSD(aov(met$nut_set ~ met$class))
nut_set_class["VLo","tukey"]="a"
nut_set_class["Lo","tukey"]="a"
nut_set_class["Me","tukey"]="b"
nut_set_class["Sh","tukey"]="b"


# graphs ------------------------------------------------------------------
x=c(0.5,1)
plot(media~x, data=nut_set[2:3,],
     xlab="class",ylab="nut set (av +- se)",
     xlim=c(0.25,1.25),xaxt="n",
     cex=1,
     ylim=c(0,3))
axis(1,at = x, labels = rownames(nut_set)[2:3])
for (i in 1:length(rownames(nut_set)[2:3])) {
  j=rownames(nut_set)[2:3][i]
  plotCI(
    x[i],
    nut_set[j,"media"],
    li = nut_set[j,"media"] - nut_set[j,"se"],
    ui = nut_set[j,"media"] + nut_set[j,"se"],
    add = T,
    sfrac=0.01,
    lwd=1.5,
    pch=NA
  )
}
for (i in 1:length(rownames(nut_set)[2:3])) {
  text(x[i],nut_set$media[i]+0.35, nut_set$`t-test`[i])
}

#graph
x=c(0.5,1,1.5,2)
plot(media~x, data=nut_set_class,
     xlab="class",ylab="nut set (av +- se)",
     xlim=c(0.25,2.25),xaxt="n",
     cex=1,
     ylim=c(0,3))
axis(1,at = x, labels = rownames(nut_set_class))
for (i in 1:length(rownames(nut_set_class))) {
  j=rownames(nut_set_class)[i]
  plotCI(
    x[i],
    nut_set_class[j,"media"],
    li = nut_set_class[j,"media"] - nut_set_class[j,"se"],
    ui = nut_set_class[j,"media"] + nut_set_class[j,"se"],
    add = T,
    sfrac=0.01,
    lwd=1.5,
    pch=NA
  )
}
for (i in 1:length(rownames(nut_set_class))) {
  text(x[i],nut_set_class$media[i]+0.35, nut_set_class$tukey[i])
}

#cluster set -------------------------------------------------------------------------
clu_set=data.frame(matrix(nrow = 3,ncol = 2))
colnames(clu_set)=c("media","se")
rownames(clu_set)=c("generale", "proleptic", "sylleptic")

clu_set[1,"media"] = mean(met$clu_set,na.rm = T)
clu_set[1,"se"] = sd(met$clu_set,na.rm = T)/sqrt(length(met[!is.na(met$clu_set),"clu_set"]))
clu_set[2,"media"] = mean(met[met$shoot_type=="PROLEPTIC","clu_set"],na.rm = T)
clu_set[2,"se"] = sd(met[met$shoot_type=="PROLEPTIC","clu_set"],na.rm = T)/sqrt(length(met[!is.na(met$shoot_type=="PROLEPTIC"&met$clu_set),"clu_set"]))
clu_set[3,"media"] = mean(met[met$shoot_type=="SYLLEPTIC","clu_set"],na.rm = T)
clu_set[3,"se"] = sd(met[met$shoot_type=="SYLLEPTIC","clu_set"],na.rm = T)/sqrt(length(met[!is.na(met$shoot_type=="SYLLEPTIC"&met$clu_set),"clu_set"]))
clu_set

clu_set_class=data.frame(matrix(nrow = 4,ncol = 2))
colnames(clu_set_class)=c("media","se")
rownames(clu_set_class)=levels(met$class)

for (i in levels(met$class)) {
  df=met[met$class==i,]
  clu_set_class[i,"media"]=mean(df$clu_set,na.rm = T)
  clu_set_class[i,"se"]=sd(df$clu_set,na.rm = T)/sqrt(length(df[!is.na(df$clu_set),"clu_set"]))
}
clu_set_class

# tests -------------------------------------------------------------------
plot(density(met$clu_set, na.rm = T))
plot(met$clu_set ~ met$shoot_type)

wilcox.test(met$clu_set ~ met$shoot_type)
clu_set[1,"t-wilcox"]="***"

plot(met$clu_set ~ met$class)

kruskal.test(met$clu_set ~ met$class)
clu_set_class[1,"kruskal"]="**"

dunnTest(met$clu_set ~ met$class, method = "holm")
clu_set_class["VLo","dunnTest"]="a"
clu_set_class["Lo","dunnTest"]="a"
clu_set_class["Me","dunnTest"]="b"
clu_set_class["Sh","dunnTest"]="b"


# graphs-------------------------------------------------------------------------
x=c(0.5,1)
plot(media~x, data=clu_set[2:3,],
     xlab="class",ylab="cluster set (av +- se)",
     xlim=c(0.25,1.25),xaxt="n",
     cex=1,
     ylim=c(0,1))
axis(1,at = x, labels = rownames(clu_set)[2:3])
for (i in 1:length(rownames(clu_set)[2:3])) {
  j=rownames(clu_set)[2:3][i]
  plotCI(
    x[i],
    clu_set[j,"media"],
    li = clu_set[j,"media"] - clu_set[j,"se"],
    ui = clu_set[j,"media"] + clu_set[j,"se"],
    add = T,
    sfrac=0.01,
    lwd=1.5,
    pch=NA
  )
}
for (i in 1:length(rownames(clu_set)[2:3])) {
  text(x[i],clu_set$media[i]+0.1, clu_set$`t-wilcox`[i])
}


#graph
x=c(0.5,1,1.5,2)
plot(media~x, data=clu_set_class,
     xlab="class",ylab="clu set (av +- se)",
     xlim=c(0.25,2.25),xaxt="n",
     cex=1,
     ylim=c(0,1))
axis(1,at = x, labels = rownames(clu_set_class))
for (i in 1:length(rownames(clu_set_class))) {
  j=rownames(clu_set_class)[i]
  plotCI(
    x[i],
    clu_set_class[j,"media"],
    li = clu_set_class[j,"media"] - clu_set_class[j,"se"],
    ui = clu_set_class[j,"media"] + clu_set_class[j,"se"],
    add = T,
    sfrac=0.01,
    lwd=1.5,
    pch=NA
  )
}
for (i in 1:length(rownames(clu_set_class))) {
  text(x[i],clu_set_class$media[i]+0.15, clu_set_class$dunnTest[i])
}

# subsetto solo i germogli lunghi -------------------------------------
met = met[met$class == "VLo", ]
#prolettici e syllettici
nuts_auto = met[0, 0]
nliner = unique(sort(met$rank_node))
for (rank in nliner) {
  P = met[met$rank_node == rank, ]
  n_sh = length(unique(P$shoot_ID))#number shoot_IDs
  n_m = sum(P$m)#number mixed mets
  n_v = sum(P$v)#number vegetative mets
  n_nuts = sum(P$nu)#number nuts
  n_cls = sum(P$cl)#number clusters
  cluster_set = n_cls / n_m#allegagione
  nut_set = n_nuts / n_m#nut set
  nuts_auto = rbind(nuts_auto,
             cbind(rank, n_sh, n_m, n_v, n_cls, cluster_set, n_nuts, nut_set))
}

#solo syllettici
nuts_auto_just_sylleptics = met.sylleptic[0, 0]
nliner = unique(sort(met.sylleptic$parent_rank_node))
for (rank in nliner) {
  P = met.sylleptic[met.sylleptic$parent_rank_node == rank, ]
  n_sh = length(unique(P$shoot_ID))#number shoot_IDs
  n_m = sum(P$m)#number mixed mets
  n_v = sum(P$v)#number vegetative mets
  n_nuts = sum(P$nu)#number nuts
  n_cls = sum(P$cl)#number clusters
  allegagione = n_cls / n_m#allegagione
  nut_set = n_nuts / n_m#nut set
  nuts_auto_just_sylleptics = rbind(nuts_auto_just_sylleptics,
             cbind(rank, n_sh, n_m, n_v, n_cls, allegagione, n_nuts, nut_set))
}

clu_set_OR=clu_set
clu_set_class_OR=clu_set_class
met_VLo_OR=met
nut_set_class_OR=nut_set_class

toremove <- grep("OR$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))
