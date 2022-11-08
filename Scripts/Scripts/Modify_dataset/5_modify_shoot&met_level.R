# AIM1: add the nb of buds in SYLLEPTIC at shoot level
# AIM2: adjust nb nuts, in met and shoot level, according to bud level
library(dplyr)

shoot = read.csv("Data/Modified/foundnxtyr_shoot_lev.csv",stringsAsFactors = T)
met = read.csv("Data/Modified/foundnxtyr_met_lev.csv",stringsAsFactors = T)
bud = read.csv("Data/Modified/foundnxtyr_bud_lev.csv",stringsAsFactors = T)

# AIM1: add the nb of buds in SYLLEPTIC at shoot level ------------------
shoot = dplyr::mutate(shoot, "buds_in_sylleptic" = NA, .after = found_next_year)

for (i in shoot$shoot_ID) {
  syl = length(bud[bud$shoot_ID == i &
                     bud$shoot_type == "SYLLEPTIC", "shoot_type"])#count how many sylleptic
  shoot[shoot$shoot_ID == i, "buds_in_sylleptic"] = syl
}

# AIM2: adjust nb nuts, in met and shoot level, according to bud l --------
for (i in shoot$shoot_ID) {
  nuts = sum(bud[bud$shoot_ID == i, "nu"])
  clust = sum(bud[bud$shoot_ID == i, "cl"])
  shoot[shoot$shoot_ID == i, "nu"] = nuts
  shoot[shoot$shoot_ID == i, "cl"] = clust
}

for (i in unique(met$shoot_ID)) {
  for (j in unique(met$rank_node)) {
    nuts = sum(bud[bud$shoot_ID == i & bud$rank_node==j, "nu"])
    clust = sum(bud[bud$shoot_ID == i & bud$rank_node==j, "cl"])
    met[met$shoot_ID == i& met$rank_node==j, "nu"] = nuts
    met[met$shoot_ID == i& met$rank_node==j, "cl"] = clust
  }
}

#overvrite df
write.csv(shoot, "Data/Modified/foundnxtyr_shoot_lev.csv", row.names = F)
write.csv(met, "Data/Modified/foundnxtyr_met_lev.csv", row.names = F)
