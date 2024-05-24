# AIM: create a dataframe at bud level JUST FOR THE SHOOTS found in 2021
#author: Francesca Grisafi
#year: 2022

library(janitor);library(readxl);library(dplyr);library(plyr)
library(stringr);library(qpcR)

source("Own_rooted_young/Scripts/Modify_dataset/3_modify_shootlevel_OR.R")

met=metamer

print(sum(met$tot_buds_m.v.b.c,na.rm = T))#1871
print(sum(met$nb_new_shoots,na.rm = T))#988

# duplicate each row the number of times equal to total buds --------
bud <- met[0, 0]
nline = nrow(met[1])
for (i in 1:nline) {
  x = met$tot_buds_m.v.b.c[i]
  bud = rbind(bud, met[rep(i, each = x), ])
}

# remove those who had more than 1 shoot but i recorded just one bud --------
names(new_shoots)
length(new_shoots[is.na(new_shoots$fate), "fate"]) /length(new_shoots$fate)#2.4%

new_shoots = droplevels(new_shoots[!is.na(new_shoots$fate),])

names(new_shoots)
year=grep("year",names(new_shoots))
b=grep(".c$",names(new_shoots))
new_shoots = new_shoots[year:b]

# _create new df in which each line is a single bud -----------------------
bud = dplyr::mutate(bud, "fate" = NA, .after = tot_buds_m.v.b.c)

c_in=grep("unit_2",names(new_shoots))
c_fin=ncol(new_shoots)
bud[c((ncol(bud)+1):(ncol(bud)+(c_fin-c_in+1)))]=NA
names(bud)[(ncol(bud)-(c_fin-c_in)):ncol(bud)]=names(new_shoots)[c_in:c_fin]

bud.new=bud[0,0]
for (i in unique(sort(bud$shoot_ID))) {
  bud.shoot=droplevels(bud[bud$shoot_ID==i,])
  new.shoot=droplevels(new_shoots[new_shoots$shoot1yo==i,])
  for (j in 1:max(bud.shoot$rank_node)) {
    bud.rank=droplevels(bud.shoot[bud.shoot$rank_node==j,])
    new.rank=droplevels(new.shoot[new.shoot$rank1yo==j,])
    bud.rank$fate=toupper(rep(names(bud.rank[tolower(levels(new_shoots$fate))]),bud.rank[1,tolower(levels(new_shoots$fate))]))
    for (q in 1:nrow(bud.rank)) {
      if (bud.rank[q,tolower(bud.rank[q,"fate"])]!=0) {
        bud.rank[q,tolower(bud.rank[q,"fate"])]=bud.rank[q,tolower(bud.rank[q,"fate"])]-1
      }
    }
    fate=new.rank$fate
    diff = match(new.rank$fate, bud.rank$fate)#position of fates (in "bud")
    bud.rank[-(diff),"nb_new_shoots"]=0
    if (length(fate)!=0) {
      frequenz=data.frame(table(fate))
      for (f in frequenz$fate) {
        ripetiz=frequenz[frequenz$fate==f,"Freq"]
        new=new.rank[new.rank$fate==f,"number_newshoot2yo"]
        bud.rank[bud.rank$fate %in% f,][1:ripetiz,"nb_new_shoots"]=new
        bud.rank[bud.rank$fate %in% f & bud.rank$nb_new_shoots!=0,(ncol(bud)-(c_fin-c_in)):ncol(bud)]=new.rank[new.rank$fate==f,c_in:c_fin]
        }
      }
    bud.new=rbind(bud.new,bud.rank)
  }
}

bud=bud.new
rm(bud.new)

names(bud)[grep("unit_2",names(bud))[2]]="unit_3"

bud = dplyr::mutate(bud, "siblings_mv" = NA, .after = tot_buds_m.v.b.c)
bud$siblings_mv = bud$m + bud$v

bud$fate=factor(bud$fate,levels = c("C","V","M","B"))

# keep the info regarding nuts and clusters just for one bud (remo --------
bud.new=bud[0,0]
for (i in unique(sort(bud$shoot_ID))) {
  bud.shoot=droplevels(bud[bud$shoot_ID==i,])
  for (j in 1:max(bud.shoot$rank_node)) {
    bud.rank=droplevels(bud.shoot[bud.shoot$rank_node==j,])
    diff = match("M", bud.rank$fate)#position of fates (in "bud")
    if(!is.na(diff)){
      bud.rank[-(diff),c("cl","nu")]=0
      }
    bud.rank[bud.rank$fate!="M",c("cl","nu") ]=NA
    bud.new=rbind(bud.new,bud.rank)
  }
}

bud=bud.new

toremove <- grep("^shoot$|^bud$|^met$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))

