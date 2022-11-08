#AIM1: Length from M and V
#AIM2: Relationship parent length ~ new shoots length from SYLLEPTIC
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")

library(RColorBrewer)
library(pals)
library(gridExtra)
library(tidyr)

# Length from M and V -----------------------------------------------------
#visualize plot
str(MV.bud.SYL)
plot(length2yo.cm.~parent_length_cm,MV.bud.SYL,pch=19)
plot(length2yo.cm.~parent_rank_node,MV.bud.SYL,pch=19)
plot(length2yo.cm.~normal_distance,MV.bud.SYL,pch=19)
plot(length2yo.cm.~distance_abs,MV.bud.SYL,pch=19)
plot(length2yo.cm.~fate,MV.bud.SYL,pch=19)
plot(length2yo.cm.~m,MV.bud.SYL,pch=19)
plot(length2yo.cm.~v,MV.bud.SYL,pch=19)
plot(length2yo.cm.~siblings_mv, MV.bud.SYL, pch=19)
plot(length2yo.cm.~median_distance_norm,MV.bud.SYL,pch=19)
#seems not to be correlated
#frequency table of combination neweral child in sylleptic shoots per each rank node
lne = grep("length.newshoots", colnames(MV.bud.SYL))#adress the column with the class of the child
new_MV = MV.bud.SYL[0, 0]#empty df
nshoot = length(unique(sort(MV.bud.SYL$shoot_ID)))#first loop length
nrank = length(unique(sort(MV.bud.SYL$parent_rank_node)))#second loop length
for (i in 1:nshoot) {
  I = unique(sort(MV.bud.SYL$shoot_ID))[i]
  for (j in 1:nrank) {
    J = unique(sort(MV.bud.SYL$parent_rank_node))[j]
    m = MV.bud.SYL[MV.bud.SYL$shoot_ID == I & MV.bud.SYL$parent_rank_node == J, lne]
    if (length(m) != 0) {
      q = paste0(m[!is.na(m)], collapse = "+")
      tot = cbind(
        "shoot_ID" = I,
        "parent_rank_node" = J,
        "class" = q
      )
      new_MV = rbind(new_MV, tot)
    }
  }
}
new_MV
new_MV$parent_rank_node = as.numeric(new_MV$parent_rank_node)
new_MV = new_MV[new_MV$class != "", ]
new_MV = new_MV[order(new_MV$parent_rank_node), ]
print(new_MV)
new_class = table(new_MV$parent_rank_node, new_MV$class)
print(new_class)
#graph
png("Outputs/Plots/6_SYL_fromMV_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <- rev(watlington(n = length(colnames(t(
  new_class
)))))
x <-
  barplot(
    t(new_class),
    col = cols,
    main = "combinations of newerals from sylleptic buds",
    xlab = "Rank nodes of parental",
    ylab = "# of child class"
  )
legend(
  "topright",
  inset = c(-0.2, -0.15),
  xpd = TRUE,
  legend = rownames(t(new_class)),
  fill = cols,
  cex = 0.6
)
dev.off()
#renewive frequency table of combination neweral child in proleptic shoots per each rank node
new_class.freq = prop.table(new_class, margin = 1) * 100
print(new_class.freq)
#graph
png("Outputs/Plots/6_SYL_fromMV_freq_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <- rev(watlington(n = length(colnames(t(
  new_class.freq
)))))
x <-
  barplot(
    t(new_class.freq),
    col = cols,
    main = "combinations of newerals from sylleptic buds",
    xlab = "Rank nodes of parental",
    ylab = "% of child class",
    ylim = c(0, 100)
  )
legend(
  "topright",
  inset = c(-0.2, -0.15),
  xpd = TRUE,
  legend = rownames(t(new_class.freq)),
  fill = cols,
  cex = 0.6
)
dev.off()

# AIM2: Relationship parent length ~ new shoots length from SYLLEP --------
f = grep("fate", colnames(bud.sylleptic))
c=grep("class", colnames(bud.sylleptic))
s=grep("shoot_ID", colnames(bud.sylleptic))
TAB_P = as.data.frame(table(unique(bud.sylleptic[bud.sylleptic$nb_new_shoots != 0, c:s])[1]))#class_sylleptic
colnames(TAB_P) = c("sylleptic_class", "parental_frequence")
TAB_P$MV = NA
TAB_P$TOT_BUDS = NA

for (i in 1:4) {
  I = TAB_P$sylleptic_class[i]
  tt = length(bud.sylleptic[bud.sylleptic$class == I, f])#totbuds in parental
  mv = length(grep(paste0("V", "|", "M"), bud.sylleptic[bud.sylleptic$class == I, f]))#mv in parental
  TAB_P[i, 3] = mv
  TAB_P[TAB_P$sylleptic_class == I, 4] = tt
}

TAB_P["Sums", 2:4] = colSums(TAB_P[2:4])
TAB_P

par_child = as.data.frame.matrix(table(bud.sylleptic$class, bud.sylleptic$length.newshoots))
colnames(par_child) = c("child_Sh", "child_Me")#nomi colonne
rownames(par_child) = c("parent_Sh", "parent_Me", "parent_Lo", "parent_VLo")#nomi righe
par_child = add_column(par_child, freq_class = TAB_P[1:4, 2], .before = "child_Sh")#numero parents per ogni categoria
par_child = add_column(par_child, tot_parental_buds = TAB_P[1:4, 4], .before = "child_Sh")#numero gemme laterali per ogni categoria
par_child = add_column(par_child, parental_M_V = TAB_P[1:4, 3], .before = "child_Sh")#numero gemme M+V per ogni categoria
par_child$sum_child = rowSums(par_child[4:5])#numero totale laterali per ogni categoria di lunghezza
par_child["Sums", ] = colSums(par_child)#numero totale laterali

#write pdf with the table
pdf("Outputs/Tables/SYL_newlengh~prollengh.pdf", height = 5,width = 12)
grid.table(par_child)
dev.off()