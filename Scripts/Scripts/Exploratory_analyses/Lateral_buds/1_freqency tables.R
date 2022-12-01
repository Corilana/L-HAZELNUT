#AIM1:create a frequency table per each class length
#AIM2:create a frequency table per each shoot type (prol/syl)
#AIM3: create a frequency table per each class length IN SYLLEPTIC
#data=shoots found in 2021 (Deruta)
#Phd=Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
library(gridExtra)

#AIM1:create a frequency table per each class length --------------------
class.length = bud[0, 0]
for (i in levels(bud$class)) {
  class.buds = bud[0, 0]
  class.newshoot = bud[0, 0]
  for (j in levels(bud$fate)) {
    class.buds[i, j] = length(bud[bud$class == i & bud$fate == j, "fate"])
  }
  for (q in levels(bud$class)) {
    class.newshoot[i, paste0("new_shoot_", q)] = sum(bud[bud$class == i &
                                                           bud$length.newshoots == q, "nb_new_shoots"], na.rm = T)
  }
  nb_shoots = length(unique(bud[bud$class == i, "shoot_ID"]))
  tot_buds_m.v.b.c = length(bud[bud$class == i, "fate"])
  nb_new_shoots = sum(bud[bud$class == i, "nb_new_shoots"])
  tot = cbind(nb_shoots,
              tot_buds_m.v.b.c,
              class.buds,
              nb_new_shoots,
              class.newshoot)
  class.length = rbind(class.length, tot)
}

print(class.length)
#write pdf with the table
pdf("Outputs/Tables/freq_class_length.pdf",
    height = 4,
    width = 13)
grid.table(class.length)
dev.off()

#AIM2:create a frequency table per each shoot type (prol/syl) ----------------------------
shoot.type = bud[0, 0]
for (i in levels(bud$shoot_type)) {
  class.buds = bud[0, 0]
  class.newshoot = bud[0, 0]
  for (j in c("M", "V")) {
    class.buds[i, j] = length(bud[bud$shoot_type == i &
                                    bud$fate == j, "fate"])
  }
  for (q in levels(bud$fate)) {
    class.newshoot[i, paste0("new_shoot_from_", q)] = sum(bud[bud$shoot_type ==
                                                                i & bud$fate == q, "nb_new_shoots"], na.rm = T)
  }
  nb_shoots = length(unique(bud[bud$shoot_type == i, "shoot_ID"]))
  nb_new_shoots = sum(bud[bud$shoot_type == i, "nb_new_shoots"])
  tot = cbind(nb_shoots, class.buds, nb_new_shoots, class.newshoot)
  shoot.type = rbind(shoot.type, tot)
}
shoot.type$bud_burst = (shoot.type$new_shoot_from_M + shoot.type$new_shoot_from_V) /
  (shoot.type$M + shoot.type$V)
shoot.type$errors = (shoot.type$new_shoot_from_C + shoot.type$new_shoot_from_B) /
  (shoot.type$nb_new_shoots)

print(shoot.type)
#write pdf with the table
pdf("Outputs/Tables/freq_shoot_type.pdf",
    height = 4,
    width = 13)
grid.table(shoot.type)
dev.off()

# AIM3: create a frequency table per each class length IN SYLLEPTIC -------
TAB_SYL = met.sylleptic[0, 0]
nline = length(unique(sort(met.sylleptic$parent_rank_node)))
for (q in 1:nline) {
  Q = unique(sort(met.sylleptic$parent_rank_node))[q]
  TAB_SYL[paste0(Q), "rank_node"] = Q
  TAB_SYL[paste0(Q), "nb_shoots"] = length(met.sylleptic[met.sylleptic$parent_rank_node ==
                                                                  Q, "shoot_ID"])
  TAB_SYL[paste0(Q), "c"] = sum(met.sylleptic[met.sylleptic$parent_rank_node ==
                                                Q, "c"])#c for each parental node?
  TAB_SYL[paste0(Q), "v"] = sum(met.sylleptic[met.sylleptic$parent_rank_node ==
                                                Q, "v"])#v for each parental node?
  TAB_SYL[paste0(Q), "m"] = sum(met.sylleptic[met.sylleptic$parent_rank_node ==
                                                Q, "m"])#m for each parental node?
  TAB_SYL[paste0(Q), "b"] = sum(met.sylleptic[met.sylleptic$parent_rank_node ==
                                                Q, "b"])#b for each parental node?
}
TAB_SYL["sums", 2:6] = colSums(TAB_SYL[2:6])#sums each observations(obs)
TAB_SYL[, "sums"] = rowSums(TAB_SYL[3:6])#sum obserbations per each node
head(TAB_SYL)

#relative frequency table of buds/shoots in sylleptic shoots per each rank node
for (i in 1:nline) {
  TAB_SYL[i, 8:11] = round((TAB_SYL[i, 3:6] / TAB_SYL[i, 2]) * 100, digit =
                             2)
}
colnames(TAB_SYL)[8:11] = c("%C", "%V", "%M", "%B")

#write pdf with the table
pdf("Outputs/Tables/SYL_obs~rank.pdf",height = 8,width = 10 )
grid.table(TAB_SYL)
dev.off()
