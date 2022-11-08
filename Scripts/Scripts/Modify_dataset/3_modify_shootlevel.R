#AIM:addin the info, at shoot level, if the shoot was found IN 2021 or not
library(dplyr)

shoot = read.csv("Data/Modified/shoots_level.csv",stringsAsFactors = T)
metamer = read.csv("Data/Modified/metamer_level.csv",stringsAsFactors = T)

shoot$found_next_year= NA
#storing the position of some columns
ne = grep("new", colnames(metamer))#column "new shoots"
fo = grep("^foun", colnames(shoot))#column "found"
nline = length(shoot$tesi)#number of line
#store, for each shoot, if it was found next year (tag did not felt down)
for (i in unique(shoot$shoot_ID)) {
  n = max(metamer[metamer$shoot_ID == i, "nb_new_shoots"])
  if (!is.na(n)) {
    n = "YES"
  }
  else {
    n = "NO"
  }
  shoot[shoot$shoot_ID == i, "found_next_year"] =n
}
shoot$found_next_year=factor(shoot$found_next_year)

#create a new df with youst the shoots that were found in 2021
write.csv(shoot[shoot$found_next_year=="YES",],
          "Data/Modified/foundnxtyr_shoot_lev.csv", row.names = F)
