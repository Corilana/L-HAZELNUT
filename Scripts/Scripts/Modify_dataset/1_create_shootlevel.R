# AIM: create a shoot level df from the original one
library(dplyr)

shoot = read.csv("Data/Source/2020DFAUTO.csv", sep = ";")
original_df = read.csv("Data/Source/2020Inne_autor.csv", sep=";")

shoot = dplyr::mutate(shoot, "tot_buds_m+v+b+c" = NA, .after = b)

#making the sum of nodes, #catkins,mixed,veg,blind,nuts,cluster per shoot
nline = length(shoot$shoot)
for (i in 1:nline) {
  shoot[i, "node"] = sum(shoot[i, grep("node.[0-9]*$", colnames(shoot))] > 0, na.rm =
                          TRUE)#node
  shoot[i, "c"] = sum(shoot[i, grep("c[^a-z][0-9]*$", colnames(shoot))] > 0, na.rm =
                       TRUE)#catkins
  shoot[i, "v"] = rowSums(shoot[i, grep("v[^a-z][0-9]*$", colnames(shoot))], na.rm = T)#v
  shoot[i, "m"] = rowSums(shoot[i, grep("m[^a-z][0-9]*$", colnames(shoot))], na.rm = T)#m
  shoot[i, "cl"] = rowSums(shoot[i, grep("^cluster", colnames(shoot))], na.rm = T)#cluster
  shoot[i, "nu"] = rowSums(shoot[i, grep("^nut", colnames(shoot))], na.rm = T)#nut
  shoot[i, "b"] = sum(shoot[i, grep("b[^a-z][0-9]*$", colnames(shoot))] > 0, na.rm =
                       TRUE)#blind
  shoot[i, "tot_buds_m+v+b+c"] = sum(shoot[i, "c"],shoot[i, "v"],shoot[i, "m"],shoot[i, "b"])#sum of obs
}

# adding the plant ID to the dataset
shoot = dplyr::mutate(shoot, "Tree" = NA, .after = shoot)
for (i in 1:nline) {
  s = shoot$shoot[i]
  t = original_df[original_df$tesi == "auto" & original_df$shoot == s, "pianta"]
  shoot[i, "Tree"] = t
}

str(shoot)
shoot[c(2, 5)] = lapply(shoot[c(2, 5)], as.factor)
colnames(shoot)[6]="shoot_ID"
write.csv(shoot[-1], "Data/Modified/shoots_level.csv", row.names = F)
