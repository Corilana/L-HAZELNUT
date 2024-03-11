# AIM1: add the nb of buds in SYLLEPTIC at shoot level
# AIM2: adjust nb nuts, in met and shoot level, according to bud level
#author: Francesca Grisafi
#year: 2022

library(dplyr);library(plyr);library(xlsx)

source("Own_rooted_young/Scripts/Modify_dataset/4_create_bude_level_OR.R")

#add the nb of buds in SYLLEPTIC, NB NUTS, ETC 
met[(ncol(met)+1):(ncol(met)+3)]=NA
c=match("c",names(met))
v=match("v",names(met))
m=match("m",names(met))
b=match("b",names(met))

names(met)[(ncol(met)-2):(ncol(met))]=paste0(names(met)[c(c,v,m)],"_syl")
names(met)[c]="sylleptic"
met[met$sylleptic!=0,(ncol(met)-2):(ncol(met))]=met[met$sylleptic!=0,c(c,v,m)]
met[met$sylleptic!=0,c(v,m)]=0

met$tot_buds_syl_m_v=met$m_syl+met$v_syl
tot_buds=grep("tot_buds_m.v.b.c",names(met))
names(met)[tot_buds]="tot_buds_mvb"
met$tot_buds_mvb=met$m+met$v+met$b
met = dplyr::mutate(met, "tot_buds_mv" = NA, .after = tot_buds_mvb)
met$tot_buds_mv=met$m+met$v

#gemme nei rami sillettici (c+v+m) a livello del germoglio
shoot = dplyr::mutate(shoot, "buds_in_sylleptic" = NA, .after = found_next_year)

for (i in shoot$shoot_ID) {
  syl = sum(met[met$shoot_ID==i,(ncol(met)-2):(ncol(met))], na.rm = T)
  shoot[shoot$shoot_ID == i, "buds_in_sylleptic"] = syl
  nuts = sum(bud[bud$shoot_ID == i, "nu"],na.rm = T)
  clust = sum(bud[bud$shoot_ID == i, "cl"],na.rm = T)
  shoot[shoot$shoot_ID == i, "nu"] = nuts
  shoot[shoot$shoot_ID == i, "cl"] = clust
  for (j in unique(met$rank_node)) {
    nuts = sum(bud[bud$shoot_ID == i & bud$rank_node==j, "nu"],na.rm = T)
    clust = sum(bud[bud$shoot_ID == i & bud$rank_node==j, "cl"],na.rm = T)
    met[met$shoot_ID == i& met$rank_node==j, "nu"] = nuts
    met[met$shoot_ID == i& met$rank_node==j, "cl"] = clust
  }
}

#add distance from median rank node
met = dplyr::mutate(met, "norm_median_distance" = NA, .after = median_distance)
met$norm_median_distance = round((met$median_distance /met$`Length(node)`),1)

bud$class=factor(bud$class,levels = c("Sh","Me","Lo","VLo"))
bud$length_newshoots = factor(bud$length_newshoots, c("Sh", "Me", "Lo", "VLo"))

foundnxtyr_bud_lev=bud[!is.na(bud$nb_new_shoots),]
foundnxtyr_shoot_lev=shoot[shoot$found_next_year=="YES",]
foundnxtyr_met_lev=met[!is.na(met$nb_new_shoots),]

# write.xlsx(bud, "Own_rooted_young/Data/alldata_OR.xlsx",row.names = F,sheetName = "bud_lev" )
# write.xlsx(shoot, "Own_rooted_young/Data/alldata_OR.xlsx",row.names = F,sheetName = "shoot_lev",append = T)
# write.xlsx(met, "Own_rooted_young/Data/alldata_OR.xlsx",row.names = F,sheetName = "met_lev",append = T)
# 
# 
# write.xlsx(foundnxtyr_bud_lev, "Own_rooted_young/Data/foundnxtyr_OR.xlsx",row.names = F,sheetName = "bud_lev")
# write.xlsx(foundnxtyr_shoot_lev, "Own_rooted_young/Data/foundnxtyr_OR.xlsx",row.names = F,sheetName = "shoot_lev",append = T)
# write.xlsx(foundnxtyr_met_lev, "Own_rooted_young/Data/foundnxtyr_OR.xlsx",row.names = F,sheetName = "met_lev",append = T)

toremove <- grep("^shoot$|^bud$|^met$|_lev$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))


