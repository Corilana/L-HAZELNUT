#AIM1:create a metamer level dataset
#AIM2: type of shoot (SYLLEPTIC or PROLEPTIC) where the metamer comes from
#AIM3:save a dataframe, at the metamer level, with just the shoots that were found in 2021 
library(dplyr)
library(plyr)

shoot = read.csv("Data/Modified/shoots_level.csv",stringsAsFactors = T)
new_shoots = read.csv("Data/Source/2021DFAUTO.csv", sep = ";")

# AIM1: duplicate rows according to number of nodes and adding node info --------
metamer = shoot[0, 0]
nline = length(shoot$shoot_ID)
names(shoot)
col_i = match("tesi",names(shoot))
col_f = match("node",names(shoot))
c=match("c",names(shoot))
b=match("b",names(shoot))

for (i in 1:nline) {
  n = shoot$node[i]
  for (rank_node in 1:n) {
    #for each node in the shoot
    s = shoot[col_i:col_f][i,]
    st = paste0("c.", rank_node, "$")#store the rank node information
    fin = paste0("b.", rank_node, "$")
    col_st = grep(st, colnames(shoot))
    col_fin = grep(fin, colnames(shoot))
    s = cbind(s, rank_node, shoot[col_st:col_fin][i,])
    s_i = grep(st, colnames(s))
    s_f = grep(fin, colnames(s))
    colnames(s)[s_i:s_f] = colnames(shoot)[c:b]
    metamer = rbind.fill(metamer, s)
  }
}
names(metamer)
node=match("node", colnames(metamer))
colnames(metamer)[node]= "Length(node)"
# change values of catkins into binary (present or not present)
metamer[metamer$c > 1, "c"] = 1
metamer = dplyr::mutate(metamer, "tot_buds_m.v.b.c" = NA, .after = b)
c=match("c",names(metamer))
m=match("m",names(metamer))
b=match("b",names(metamer))
metamer$tot_buds_m.v.b.c = rowSums(metamer[c(c:m, b)])

# AIM1: create distance with the median rank (median rank=0) --------------------
metamer = dplyr::mutate(metamer, "median_distance" = NA, .after = rank_node)
names(metamer)

nshoot = unique(metamer$shoot_ID)
for (i in nshoot) {
  s=metamer[metamer$shoot_ID==i,]
  median=median(s$rank_node)
  for (j in s$rank_node) {
    metamer[metamer$shoot_ID==i&metamer$rank_node==j,"median_distance"]=j-median
  }
}

metamer = dplyr::mutate(metamer, "distance_abs" = NA, .after = median_distance)#add new column for the distance
metamer$distance_abs = abs(metamer$median_distance)

metamer = dplyr::mutate(metamer, "normal_distance" = NA, .before = distance_abs)#add new column for the distance
metamer$normal_distance = round(metamer$distance_abs / metamer$`Length(node)`, digits = 2)

# AIM2: type of shoot (SYLLEPTIC or PROLEPTIC) where the metamer comes from -----------------------------------
nline = dim(metamer)[1]
for (i in 1:nline) {
  c = metamer$c[i]
  if (c != 0) {
    metamer$shoot_type[i] = "SYLLEPTIC"
  } else{
    metamer$shoot_type[i] = "PROLEPTIC"
  }
}
metamer$shoot_type=factor(metamer$shoot_type)

# AIM3: number of new shoots per each metamer -----------------------------------
metamer$nb_new_shoots = NA
#column names
names(new_shoots)
col_st=match("ï..year", names(new_shoots))
col_fin=match("X.newshoot2yo", names(new_shoots))
new_shoots = unique(new_shoots[col_st:col_fin])

for (i in unique(sort(new_shoots$shoot1yo))) {
  new_met=metamer[metamer$shoot_ID==i,]
  for (j in unique(new_met$rank_node)) {
    nb=new_shoots[new_shoots$shoot1yo==i&new_shoots$rank1yo==j,"X.newshoot2yo"]
    metamer[metamer$shoot_ID==i&metamer$rank_node==j,"nb_new_shoots"]=nb
  }
}

#save
write.csv(shoot[1:14], "Data/Modified/shoots_level.csv", row.names = F)
write.csv(metamer, "Data/Modified/metamer_level.csv", row.names = F)
write.csv(metamer[!is.na(metamer$nb_new_shoots),], "Data/Modified/foundnxtyr_met_lev.csv", row.names = F)
