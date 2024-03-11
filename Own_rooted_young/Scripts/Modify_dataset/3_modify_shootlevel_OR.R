#AIM:addin the info, at shoot level, if the shoot was found IN 2021 or not
#author: Francesca Grisafi
#year: 2022

library(janitor);library(readxl);library(dplyr);library(plyr)

source("Own_rooted_young/Scripts/Modify_dataset/2_create_metamer_level_OR.R")

shoot$found_next_year= NA
#storing the position of some columns
names(metamer)
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

toremove <- grep("^shoot$|^metamer$|new_shoots", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))
