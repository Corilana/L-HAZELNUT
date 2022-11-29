#AIM1: Length from M and V
#AIM2: Relationship parent length ~ new shoots length
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")

library(RColorBrewer)
library(gridExtra)
library(tibble)

# AIM1: Length from M and V -----------------------------------------------------
#visualize plot
str(MV.bud.PRO)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$Length,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$rank_node,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$fate,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$Length.node.,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$tot_buds,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$m,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$v,pch=19)
plot(MV.bud.PRO$length2yo.cm.~MV.bud.PRO$siblings_mv,pch=19)
#seems that rank influences the fate

# length per FATE ---------------------------------------------------------
#frequency table of LENGTH OF childs fromV in proleptic shoots per each rank node
new_shoots = MV.bud.PRO[MV.bud.PRO$nb_new_shoots != 0, ]
new_shoots.rank = table(new_shoots$rank_node, new_shoots$length.newshoots, new_shoots$fate)
head(new_shoots.rank)
fromV = new_shoots.rank[, , "V"]#developed from v
fromV.matrix=as.data.frame.matrix(fromV)
head(fromV.matrix)

#graph
png("Outputs/Plots/6_PRO_fromV_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
col = brewer.pal(n = 4, name = "Set1")
with(
  fromV.matrix[1:16, ],
  plot(
    Sh,
    pch = 19,
    cex = 1.2,
    col = col[1],
    main = "numbers of laterals from vegetative bud in proleptic shoots",
    xlab = "parental rank nodes",
    ylab = "#",
    type = "o",
    ylim = c(0, 55)
  )
)
with(fromV.matrix[1:16, ], points(
  Me,
  pch = 19,
  col = col[2],
  cex = 1.2,
  type = "o"
))
with(fromV.matrix[1:16, ], points(
  Lo,
  pch = 19,
  col = col[3],
  cex = 1.2,
  type = "o"
))
with(fromV.matrix[1:16, ], points(
  VLo,
  pch = 19,
  col = col[4],
  cex = 1.2,
  type = "o"
))
legend(
  "topright",
  inset = c(-0.2, 0),
  xpd = TRUE,
  legend = colnames(fromV.matrix),
  lwd = 3,
  cex = 0.8,
  col = col
)
dev.off()

#relative frequency table of LENGTH OF childs fromV in proleptic shoots per each rank node
fromV.freq =prop.table(fromV,1) * 100
fromV.freq.matrix = as.data.frame.matrix(fromV.freq)

#graph
png("Outputs/Plots/6_PRO_fromV_freq_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
col = brewer.pal(n = 4, name = "Set1")
with(
  fromV.freq.matrix[1:16, ],
  plot(
    Sh,
    pch = 19,
    cex = 1.2,
    col = col[1],
    main = "% of laterals from vegetative bud in proleptic shoots",
    xlab = "parental rank nodes",
    ylab = "%",
    type = "o",
    ylim = c(0, 100)
  )
)
with(fromV.freq.matrix[1:16, ], points(
  Me,
  pch = 19,
  col = col[2],
  cex = 1.2,
  type = "o"
))
with(fromV.freq.matrix[1:16, ], points(
  Lo,
  pch = 19,
  col = col[3],
  cex = 1.2,
  type = "o"
))
with(fromV.freq.matrix[1:16, ], points(
  VLo,
  pch = 19,
  col = col[4],
  cex = 1.2,
  type = "o"
))
legend(
  "topright",
  inset = c(-0.2, 0),
  xpd = TRUE,
  legend = colnames(fromV.freq.matrix),
  lwd = 3,
  cex = 0.8,
  col = col
)
dev.off()

#frequency table of LENGTH OF childs from M in proleptic shoots per each rank node
fromM = new_shoots.rank[, , "M"]#developed from M
fromM.matrix=as.data.frame.matrix(fromM)
print(fromM.matrix)
fromM.matrix = fromM.matrix[1:16, ]

#graph
png("Outputs/Plots/6_PRO_fromM_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
col = brewer.pal(n = 4, name = "Set1")
with(
  fromM.matrix,
  plot(
    Sh,
    pch = 19,
    cex = 1.2,
    col = col[1],
    main = "numbers of laterals from mixed bud in proleptic shoots",
    xlab = "parental rank nodes",
    ylab = "#",
    type = "o",
    ylim = c(0, 55)
  )
)
with(fromM.matrix, points(
  Me,
  pch = 19,
  col = col[2],
  cex = 1.2,
  type = "o"
))
with(fromM.matrix, points(
  Lo,
  pch = 19,
  col = col[3],
  cex = 1.2,
  type = "o"
))
with(fromM.matrix, points(
  VLo,
  pch = 19,
  col = col[4],
  cex = 1.2,
  type = "o"
))
legend(
  "topright",
  inset = c(-0.2, 0),
  xpd = TRUE,
  legend = colnames(fromM.matrix),
  lwd = 3,
  cex = 0.8,
  col = col
)
dev.off()

#relative frequency table of LENGTH OF childs from M in proleptic shoots per each rank node
fromM.freq =prop.table(fromM,1) * 100
fromM.freq.matrix = as.data.frame.matrix(fromM.freq)
fromM.freq.matrix = fromM.freq.matrix[1:16, ]

#graph
png("Outputs/Plots/6_PRO_fromM_freq_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
col = brewer.pal(n = 4, name = "Set1")
with(
  fromM.freq.matrix,
  plot(
    Sh,
    pch = 19,
    cex = 1.2,
    col = col[1],
    main = "% of laterals from mixed bud in proleptic shoots",
    xlab = "parental rank nodes",
    ylab = "%",
    type = "o",
    ylim = c(0, 100)
  )
)
with(fromM.freq.matrix, points(
  Me,
  pch = 19,
  col = col[2],
  cex = 1.2,
  type = "o"
))
with(fromM.freq.matrix, points(
  Lo,
  pch = 19,
  col = col[3],
  cex = 1.2,
  type = "o"
))
with(fromM.freq.matrix, points(
  VLo,
  pch = 19,
  col = col[4],
  cex = 1.2,
  type = "o"
))
legend(
  "topright",
  inset = c(-0.2, 0),
  xpd = TRUE,
  legend = colnames(fromM.freq.matrix),
  lwd = 3,
  cex = 0.8,
  col = col
)
dev.off()

# MV together --------------------------------------------------------
#frequency table of combination lateral child in proleptic shoots per each rank node
lne = grep("length.newshoots", colnames(MV.bud.PRO))#adress the column with the class of the child
new_MV = MV.bud.PRO[0, 0]#empty df
nshoot = length(unique(sort(MV.bud.PRO$shoot_ID)))#first loop length
nrank = length(unique(sort(MV.bud.PRO$rank_node)))#second loop length
for (i in 1:nshoot) {
  I = unique(sort(MV.bud.PRO$shoot_ID))[i]
  for (j in 1:nrank) {
    J = unique(sort(MV.bud.PRO$rank_node))[j]
    m = MV.bud.PRO[MV.bud.PRO$shoot_ID == I & MV.bud.PRO$rank_node == J, lne]
    if (length(m) != 0) {
      class = paste0(m[!is.na(m)], collapse = "+")
      tot = cbind(
        "shoot_ID" = I,
        "rank_node" = J,
        "class" = class
      )
      new_MV = rbind(new_MV, tot)
    }
  }
}
new_MV
new_MV$rank_node = as.numeric(new_MV$rank_node)
new_MV = new_MV[new_MV$class != "", ]
new_MV = new_MV[order(new_MV$rank_node), ]
print(new_MV)

new_class = table(new_MV$rank_node, new_MV$class)
print(new_class)

#graph
png("Outputs/Plots/6_PRO_fromMV_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <- brewer.pal(n = length(colnames(new_class[1:16, ])), name = "Set3")
x <-
  barplot(
    t(new_class[1:16, ]),
    col = cols,
    main = "combinations of laterals from proleptic buds",
    xlab = "Rank nodes",
    ylab = "# of child class"
  )
legend(
  "topright",
  inset = c(-0.2, -0.15),
  xpd = TRUE,
  legend = rownames(t(new_class[1:16, ])),
  fill = cols,
  cex = 0.6
)
dev.off()
#relative frequency table of combination lateral child in proleptic shoots per each rank node
new_class.freq = prop.table(new_class,1) * 100
print(new_class.freq)

#graph
png("Outputs/Plots/6_PRO_fromMV_freq_newshootclass.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <- brewer.pal(n = length(colnames(new_class.freq[1:16, ])), name = "Set3")
x <-
  barplot(
    t(new_class.freq[1:16, ]),
    col = cols,
    main = "combinations of laterals from proleptic buds",
    xlab = "Rank nodes",
    ylab = "% of child class",
    ylim = c(0, 100)
  )
legend(
  "topright",
  inset = c(-0.2, -0.15),
  xpd = TRUE,
  legend = rownames(t(new_class.freq[1:16, ])),
  fill = cols,
  cex = 0.6
)
dev.off()

# AIM2: Relationship parent length ~ new shoots length --------------------
df.shoot=unique(bud.proleptic[bud.proleptic$nb_new_shoots != 0, 4:5])
class=as.data.frame(table(df.shoot[1]))
class 
colnames(class) = c("class_length", "nb_shoots")
class$MV = NA
class$TOT_BUDS = NA
class
#for each class write number of buds and number of V and M
for (i in 1:4) {
  I = class$class_length[i]
  tt = length(bud.proleptic[bud.proleptic$class == I, "fate"])#totbuds in parental
  mv = length(grep(paste0("V", "|", "M"), bud.proleptic[bud.proleptic$class == I, "fate"]))#mv in parental
  class[i, 3] = mv
  class[class$class_length == I, 4] = tt
}
class
class["Sums", 2:4] = colSums(class[2:4])

#parent length ~ child length
new_class=table(bud.proleptic$class, bud.proleptic$length.newshoots)
new_class.matrix = as.data.frame.matrix(new_class)
new_class.matrix
colnames(new_class.matrix) = c("child_Sh", "child_Me", "Child_Lo", "Child_VLo")#nomi colonne
rownames(new_class.matrix) = c("parent_Sh", "parent_Me", "parent_Lo", "parent_VLo")#nomi righe
new_class.matrix = add_column(new_class.matrix, freq_class = class[1:4, 2], .before = "child_Sh")#numero parents per ogni categoria
new_class.matrix = add_column(new_class.matrix, tot_parental_buds = class[1:4, 4], .before = "child_Sh")#numero gemme laterali per ogni categoria
new_class.matrix = add_column(new_class.matrix, parental_M_V = class[1:4, 3], .before = "child_Sh")#numero gemme M+V per ogni categoria
new_class.matrix$sum_child = rowSums(new_class.matrix[4:7])#numero totale laterali per ogni categoria di lunghezza
new_class.matrix["Sums", ] = colSums(new_class.matrix)#numero totale laterali
new_class.matrix
#write pdf with the table
pdf("Outputs/Tables/PRO_new_shoots~class.pdf", height = 5,width = 12)
grid.table(new_class.matrix)
dev.off()

#create a relative frequency table
new_class.freq = prop.table(new_class, margin = 1) * 100
new_class.freq.matrix = as.data.frame.matrix(round(new_class.freq, digit = 2))
print(new_class.freq.matrix)

#graph
png("Outputs/Plots/6_PRO_lenchil~lenpar.png",width=1200, height=900, res=150)# save plot
cols<-colors()[c(90,10,20,50)]
x<-barplot(t(new_class.freq.matrix),beside= T,col = cols, main="lateral child length from proleptic",xlab= "Parent length (proleptic)", ylab="% child length(es. #child/totalchildSh)", ylim=c(0,110))
legend("top",horiz=T,inset=c(0,-0.02),xpd = TRUE, legend = rownames(t(new_class.freq.matrix)),fill = cols, cex=0.6)
#is sh child proportion different according to parental length?
print(prop.test(x = new_class[, 1], rowSums(new_class)))
print(pairwise.prop.test(x = new_class[, 1],
                   rowSums(new_class),
                   p.adjust.method = "none"))
text(x[1:2,]+0.2, t(new_class.freq.matrix[,1:2])+3.5, paste(t(new_class.freq.matrix[,1:2]),"%"), cex = 0.7)
print(prop.test(x = new_class[, 2], rowSums(new_class)))
print(pairwise.prop.test(x = new_class[, 2],
                   rowSums(new_class),
                   p.adjust.method = "none"))
text(x[2,], t(new_class.freq.matrix[,2])+8.5, c("ab","b", "b", "a"), cex = 0.7)
dev.off()
