#AIM: split the dataframes into proleptic and sylleptic & modify proleptic
#data=shoots found in 2021 (Deruta)
#Phd=Francesca Grisafi
library(dplyr)
shoot=read.csv("Data/Modified/foundnxtyr_shoot_lev.csv",
               stringsAsFactors = T)
met = read.csv("Data/Modified/foundnxtyr_met_lev.csv",
               stringsAsFactors = T)

bud= read.csv("Data/Modified/foundnxtyr_bud_lev.csv",
               stringsAsFactors = T)

str(met)
met$class = factor(met$class, c("Sh", "Me", "Lo", "VLo"))
bud$class = factor(bud$class, c("Sh", "Me", "Lo", "VLo"))
met = dplyr::mutate(met, "median_distance_norm" = NA, .after = median_distance)
met$median_distance_norm = round((met$median_distance /
                                    met$Length.node.),
                                 1)
str(bud)
bud$length.newshoots = factor(bud$length.newshoots, c("Sh", "Me", "Lo", "VLo"))
str(shoot)
shoot$class = factor(shoot$class, c("Sh", "Me", "Lo", "VLo"))

# sylleptic ---------------------------------------------------------------
#met level
met.sylleptic=droplevels(met[met$shoot_type == "SYLLEPTIC", ])
met.sylleptic = dplyr::mutate(met.sylleptic, "tot_buds_m.v" = NA, .after = tot_buds_m.v.b.c)
met.sylleptic$tot_buds_m.v=met.sylleptic$m+met.sylleptic$v
names(met.sylleptic)
met.sylleptic=met.sylleptic[-18]
colnames(met.sylleptic)[c(2, 7, 8)] = c("parent_length_cm",
                                            "parent_length_node",
                                            "parent_rank_node")

#bud level
bud.sylleptic=droplevels(bud[bud$shoot_type=="SYLLEPTIC",])
names(bud.sylleptic)
bud.sylleptic=bud.sylleptic[-18]
bud.sylleptic = dplyr::mutate(bud.sylleptic, "median_distance_norm" = NA, .after = median_distance)
bud.sylleptic$median_distance_norm = round((bud.sylleptic$median_distance /
                                              bud.sylleptic$Length.node.),
                                           1)

colnames(bud.sylleptic)[c(2, 7, 8)] = c("parent_length_cm",
                                        "parent_length_node",
                                        "parent_rank_node")
MV.bud.SYL = droplevels(bud.sylleptic[bud.sylleptic$fate == "M" |
                                        bud.sylleptic$fate == "V", ])
MV.bud.SYL$fate = factor(MV.bud.SYL$fate, c("V", "M"))

# proleptic ---------------------------------------------------------------
#met level
#remove buds in sylleptic shoots
met.proleptic=met
colnames(met.proleptic)[13] = "sylleptic"
names(met.proleptic)
for (q in 1:nrow(met.proleptic)) {
  if (met.proleptic[q, "sylleptic"] == 1) {
    met.proleptic[q, c("m", "v", "b")] = 0
  }
}
met.proleptic = dplyr::mutate(met.proleptic, "tot_buds_mvb" = NA, .after = tot_buds_m.v.b.c)
met.proleptic = dplyr::mutate(met.proleptic, "tot_buds_mv" = NA, .after = tot_buds_mvb)
names(met.proleptic)
met.proleptic$tot_buds_mvb=rowSums(met.proleptic[c(14:15,18)])
met.proleptic$tot_buds_mv=rowSums(met.proleptic[c(14,15)])
met.proleptic=met.proleptic[-19]

#bud level
bud.proleptic = droplevels(bud[bud$shoot_type == "PROLEPTIC", ])
colnames(bud.proleptic)
bud.proleptic$fate = factor(bud.proleptic$fate, c("V", "B", "M"))

str(bud.proleptic)
bud.proleptic$median_distance_norm = round((bud.proleptic$median_distance /
                                              bud.proleptic$Length.node.),
                                           1)

MV.bud.PRO = droplevels(bud.proleptic[bud.proleptic$fate == "M" |
                                bud.proleptic$fate == "V", ])
MV.bud.PRO$siblings_mvb = MV.bud.PRO$m + MV.bud.PRO$v + MV.bud.PRO$b

