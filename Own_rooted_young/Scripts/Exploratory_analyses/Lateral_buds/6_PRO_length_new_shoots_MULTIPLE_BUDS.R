#AIM: How many multiple buds (per each fate) develop into new shoots
#data:deruta 2020
#PhD: Francesca Grisafi
library(RColorBrewer);library(gridExtra);library(tibble)

source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
#dataset
new_shoots = MV.bud.PRO[MV.bud.PRO$nb_new_shoots != 0, ]

V.bud.PRO=new_shoots[new_shoots$fate=="V",]
V.multiple=V.bud.PRO[V.bud.PRO$v!=0&V.bud.PRO$m==0,]

M.bud.PRO=new_shoots[new_shoots$fate=="M",]
M.multiple=M.bud.PRO[M.bud.PRO$siblings_mv!=0,]

#frequency table of combination lateral child in proleptic shoots per each rank node from MIXED
lne = grep("length_newshoots", colnames(M.multiple))#adress the column with the class of the child
new_M = M.multiple[0, 0]#empty df
nshoot = length(unique(sort(M.multiple$shoot_ID)))#first loop length
nrank = length(unique(sort(M.multiple$rank_node)))#second loop length
for (i in 1:nshoot) {
  I = unique(sort(M.multiple$shoot_ID))[i]
  for (j in 1:nrank) {
    J = unique(sort(M.multiple$rank_node))[j]
    m = M.multiple[M.multiple$shoot_ID == I & M.multiple$rank_node == J, lne]
    if (length(m) != 0) {
      class = paste0(m[!is.na(m)], collapse = "+")
      tot = cbind(
        "shoot_ID" = I,
        "rank_node" = J,
        "class" = class
      )
      new_M = rbind(new_M, tot)
    }
  }
}
new_M
new_M$rank_node = as.numeric(new_M$rank_node)
new_M = new_M[new_M$class != "", ]
new_M = new_M[order(new_M$rank_node), ]
print(new_M)

new_class = table(new_M$rank_node, new_M$class)
print(new_class)

#graph
# png("Own_rooted_young/Outputs/Plots/6_PRO_fromM(MULTIPLE)_newshootclass.png",width = 1200,height = 900,res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <- brewer.pal(n = length(colnames(new_class)), name = "Set3")
x <-
  barplot(
    t(new_class),
    col = cols,
    main = "combinations of laterals from M buds when other v or m buds",
    xlab = "Rank nodes",
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
# dev.off()

#frequency table of combination lateral child in proleptic shoots per each rank node from VEGETATIVE
lne = grep("length_newshoots", colnames(V.multiple))#adress the column with the class of the child
new_v = V.multiple[0, 0]#empty df
nshoot = length(unique(sort(V.multiple$shoot_ID)))#first loop length
nrank = length(unique(sort(V.multiple$rank_node)))#second loop length
for (i in 1:nshoot) {
  I = unique(sort(V.multiple$shoot_ID))[i]
  for (j in 1:nrank) {
    J = unique(sort(V.multiple$rank_node))[j]
    m = V.multiple[V.multiple$shoot_ID == I & V.multiple$rank_node == J, lne]
    if (length(m) != 0) {
      class = paste0(m[!is.na(m)], collapse = "+")
      tot = cbind(
        "shoot_ID" = I,
        "rank_node" = J,
        "class" = class
      )
      new_v = rbind(new_v, tot)
    }
  }
}
new_v
new_v$rank_node = as.numeric(new_v$rank_node)
new_v = new_v[new_v$class != "", ]
new_v = new_v[order(new_v$rank_node), ]
print(new_v)

new_class = table(new_v$rank_node, new_v$class)
print(new_class)

#graph
# png("Own_rooted_young/Outputs/Plots/6_PRO_fromV(MULTIPLE)_newshootclass.png",width = 1200,height = 900,res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <- brewer.pal(n = length(colnames(new_class)), name = "Set3")
x <-
  barplot(
    t(new_class),
    col = cols,
    main = "combinations of laterals from V buds when other v",
    xlab = "Rank nodes",
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
# dev.off()
