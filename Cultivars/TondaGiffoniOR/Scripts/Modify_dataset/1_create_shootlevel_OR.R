# AIM: create a shoot level df from the original one
#author: Francesca Grisafi
#year: 2022

library(janitor);library(readxl);library(dplyr)

shoot = read_excel("Data/2020Inne_autor.xlsx",.name_repair = make_clean_names,sheet = "Own_rooted_young")
shoot=data.frame(shoot)
str(shoot)
shoot$class=factor(shoot$class,levels = c("Sh","Me","Lo","VLo"))

names(shoot)
shoot = dplyr::mutate(shoot, "tot_buds_m+v+b+c" = NA, .after = b)

#making the sum of nodes, #catkins,mixed,veg,blind,nuts,cluster per shoot
nline = length(shoot$shoot)
for (i in 1:nline) {
  shoot[i, "node"] = sum(shoot[i, grep("node_[0-9]*$", colnames(shoot))] > 0, na.rm =
                           TRUE)#node
  shoot[i, "c"] = sum(shoot[i, grep("c_[^a-z][0-9]*$", colnames(shoot))] > 0, na.rm =
                        TRUE)#catkins
  shoot[i, "v"] = rowSums(shoot[i, grep("v_[^a-z][0-9]*$", colnames(shoot))], na.rm = T)#v
  shoot[i, "m"] = rowSums(shoot[i, grep("m_[^a-z][0-9]*$", colnames(shoot))], na.rm = T)#m
  shoot[i, "cl"] = rowSums(shoot[i, grep("^cluster", colnames(shoot))], na.rm = T)#cluster
  shoot[i, "nu"] = rowSums(shoot[i, grep("^nut", colnames(shoot))], na.rm = T)#nut
  shoot[i, "b"] = sum(shoot[i, grep("b_[^a-z][0-9]*$", colnames(shoot))] > 0, na.rm =
                        TRUE)#blind
  shoot[i, "tot_buds_m+v+b+c"] = sum(shoot[i, "c"],shoot[i, "v"],shoot[i, "m"],shoot[i, "b"])#sum of obs
}

str(shoot)
id=grep("id",colnames(shoot))
tesi=grep("tesi", colnames(shoot))
class=grep("class", colnames(shoot))
shoot[c(id,tesi,class)] = lapply(shoot[c(id,tesi,class)], as.factor)
shoot_id=grep("shoot", colnames(shoot))
colnames(shoot)[shoot_id]="shoot_ID"

toremove <- grep("^shoot$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))
