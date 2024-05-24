#AIM1:create a metamer level dataset
#AIM2: type of shoot (SYLLEPTIC or PROLEPTIC) where the metamer comes from
#AIM3:save a dataframe, at the metamer level, with just the shoots that were found in 2021 
#author: Francesca Grisafi
#year: 2022
library(janitor);library(readxl);library(dplyr);library(plyr)

source("Scripts/Modify_dataset/1_create_shootlevel_OR.R")

new_shoots = read_excel("Data/2021Inne_autor.xlsx",.name_repair = make_clean_names,sheet = "2021DFAUTO")
new_shoots=data.frame(new_shoots)
str(new_shoots)
new_shoots$class=factor(new_shoots$class,levels = c("Sh","Me","Lo","VLo"))

names(new_shoots)
names(new_shoots)[c(18:24)]=c("nb_nodes_new", "c_new", "v_new", "m_new", "cic_new", "cl_new", "b_new")
new_shoots = dplyr::mutate(new_shoots, "tot_buds_m+v+b+c" = NA, .after = b_new)
new_shoots = dplyr::mutate(new_shoots, "nu_new" = NA, .after = cl_new)
new_shoots[new_shoots$length2yo==0, "length2yo"] = NA

start_col = grep("x2y_o_node$", colnames(new_shoots))
fin_col = grep("b_3$", colnames(new_shoots))
len_col = (fin_col-start_col)+1

#making the sum of nodes, #catkins,mixed,veg,blind,nuts,cluster per new_shoots
nline = length(new_shoots$shoot1yo)
rep = (ncol(new_shoots)-(start_col-1))/len_col

for (i in 1:nline) {
  df = droplevels(new_shoots[i,])
  nb_nodes = NA
  nb_c = NA
  nb_v = NA
  nb_m = NA
  nb_cl = NA
  nb_nu = NA
  nb_b = NA
  
  start_col = grep("x2y_o_node$", colnames(new_shoots))
  fin_col = grep("b_3$", colnames(new_shoots))
  for (j in 1:rep) {
    dt = df[start_col:fin_col]
    print(dt)
    if (!is.na(dt[1])){
      nb_nodes = sum(nb_nodes , 1, na.rm = T)
    }
    if (!is.na(dt[2])) {
      if (dt[2]==0){
        nb_c = sum(nb_c, as.numeric(dt[2]), na.rm = T)
        nb_v = sum(nb_v, as.numeric(dt[3]), na.rm = T)
        nb_m = sum(nb_m, as.numeric(dt[4]), na.rm = T)
        nb_cl = sum(nb_cl, as.numeric(dt[6]), na.rm = T)
        nb_nu = sum(nb_nu, as.numeric(dt[7]), na.rm = T)
        nb_b = sum(nb_b, as.numeric(dt[8]), na.rm = T)
      }
      if (dt[2]>0) {
        nb_c = sum(nb_c, length(dt[2]), na.rm = T)
      }
    }
    new_shoots[i,c("nb_nodes_new","c_new","v_new","m_new","cl_new","nu_new","b_new")]=c(nb_nodes, nb_c,nb_v,nb_m,nb_cl,nb_nu,nb_b)
    start_col = fin_col + 1 
    fin_col = start_col + (len_col-1)
  }
  new_shoots[i, "tot_buds_m+v+b+c"] = sum(new_shoots[i, "c_new"],new_shoots[i, "v_new"],
                                          new_shoots[i, "m_new"],new_shoots[i, "b_new"], na.rm = T)#sum of obs
}

str(new_shoots)
id=grep("shoot1yo", colnames(new_shoots))
tesi=grep("thesis", colnames(new_shoots))
class=grep("class", colnames(new_shoots))
new_shoots[c(id,tesi,class)] = lapply(new_shoots[c(id,tesi,class)], as.factor)
shoot_id=grep("^new_shoots", colnames(new_shoots))
colnames(new_shoots)[shoot_id]="shoot_ID"

str(new_shoots)
new_shoots$class=factor(new_shoots$class,levels = c("Sh","Me","Lo","VLo"))
new_shoots$fate=factor(new_shoots$fate,levels = c("C","V","M","B"))

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
    s = shoot[col_i:col_f][i,]
    st = paste0("c_", rank_node, "$")#store the rank node information
    fin = paste0("b_", rank_node, "$")
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

v=match("v",names(metamer))
m=match("m",names(metamer))
metamer = dplyr::mutate(metamer, "tot_buds_m.v.b.c" = NA, .after = b)
metamer$tot_buds_m.v.b.c = rowSums(metamer[c(c,v,m,b)])

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

metamer = dplyr::mutate(metamer, "abs_median_distance" = NA, .after = median_distance)#add new column for the distance
metamer$abs_median_distance = abs(metamer$median_distance)

metamer = dplyr::mutate(metamer, "abs_norm_median_distance" = NA, .before = abs_median_distance)#add new column for the distance
metamer$abs_norm_median_distance = round(metamer$abs_median_distance / metamer$`Length(node)`, digits = 2)

# AIM2: type of shoot (SYLLEPTIC or PROLEPTIC) where the metamer comes from -----------------------------------
metamer$shoot_type = NA

metamer[metamer$c!=0,"shoot_type"]="SYLLEPTIC"
metamer[metamer$c==0,"shoot_type"]="PROLEPTIC"

metamer$shoot_type=factor(metamer$shoot_type)

# AIM3: number of new shoots per each metamer -----------------------------------
metamer$nb_new_shoots = NA

#column names
for (i in unique(sort(new_shoots$shoot1yo))) {
  new_met=new_shoots[new_shoots$shoot1yo==i,]
  for (j in unique(new_met$rank1yo)) {
    new_rank=new_met[new_met$rank1yo==j&(!is.na(new_met$fate)),]
    nb=sum(new_rank$number_newshoot2yo,na.rm = T)
    metamer[metamer$shoot_ID==i&metamer$rank_node==j,"nb_new_shoots"]=nb
  }
}

names(shoot)
id=grep("id",names(shoot))
buds=grep("tot_buds_",names(shoot))
shoot=shoot[id:buds]

toremove <- grep("^shoot$|^metamer$|new_shoots", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))
