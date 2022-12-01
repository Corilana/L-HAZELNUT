# AIM: create a dataframe at bud level JUST FOR THE SHOOTS found in 2021
library(plyr)
library(dplyr)
library("stringr")

met = read.csv("Data/Modified/foundnxtyr_met_lev.csv", stringsAsFactors = T)
new_shoots = read.csv("Data/Source/2021DFAUTO.csv", sep = ";")

print(sum(met$tot_buds))#1677
print(sum(met$nb_new_shoots))#1018

# duplicate each row the number of times equal to total buds --------
bud <- met[0, 0]
nline = nrow(met[1])
for (i in 1:nline) {
  x = met$tot_buds[i]
  bud = rbind(bud, met[rep(i, each = x), ])
}
# choosing the type of the central bud ------------------------------------
#remove those who had more than 1 shoot but i recorded just one buds!!!
length(new_shoots[new_shoots$fate == "?", "fate"]) / length(new_shoots$fate)#2.4%
new_shoots = new_shoots[new_shoots$fate != "?", ]
new_shoots = new_shoots[1:22]
#_create new df in which each line is a single bud
bud = dplyr::mutate(bud, "fate" = NA, .after = tot_buds_m.v.b.c)
m.df = data.frame(matrix(ncol = 0, nrow = 0))
v.df = m.df
c.def = m.df
b.def = m.df
#create a df ("dest") with all the buds at that rank
nline = length(bud[, 1])
for (i in 1:nline) {
  c = bud$c[i]
  v = bud$v[i]
  m = bud$m[i]
  b = bud$b[i]
  if (m != 0) {
    m.df = rbind.fill(m.df, as.data.frame(t(rep("M", m))))
  } else {
    m.df = rbind.fill(m.df, as.data.frame(t(NA)))
  }
  if (v != 0) {
    v.df = rbind.fill(v.df, as.data.frame(t(rep("V", v))))
  } else {
    v.df = rbind.fill(v.df, as.data.frame(t(NA)))
  }
  if (c != 0) {
    c.def = rbind.fill(c.def, as.data.frame(t(rep("C", c))))
  } else {
    c.def = rbind.fill(c.def, as.data.frame(t(NA)))
  }
  if (b != 0) {
    b.def = rbind.fill(b.def, as.data.frame(t(rep("B", b))))
  } else {
    b.def = rbind.fill(b.def, as.data.frame(t(NA)))
  }
}
dest = cbind(m.df, v.df, c.def, b.def)
length(dest[, 1])
length(bud[, 1])
#storing the type of each bud into the new column
u = 1
for (i in 1:nline) {
  while (u < length(bud[, 1])) {
    oss = bud$tot_buds[u]
    q = grep("V|B|C|M", dest[u, ], value = TRUE)
    lenq = length(q)
    for (j in 1:lenq) {
      k = q[lenq]
      bud$fate[u] = k
      lenq = lenq - 1
      u = u + 1
    }
    print(u)
  }
}

rm(b.def)
rm(c.def)
rm(m.df)
rm(v.df)
rm(dest)

#substracting the unique value to the sibilngs
V = bud[bud$fate == "V", "v"]#buds havind "V" as fate
M = bud[bud$fate == "M", "m"]#buds havind "M" as fate
B = bud[bud$fate == "B", "b"]#buds havind "B" as fate

bud[bud$fate == "V", "v"] = V - 1#substracting V from the count of buds
bud[bud$fate == "M", "m"] = M - 1#substracting m from the count of buds
bud[bud$fate == "B", "b"] = B - 1#substracting b from the count of buds
bud[bud$fate == "C", "c"] = 0

#_create new df in which each line is a single bud
bud = dplyr::mutate(bud, "siblings_mv" = NA, .after = tot_buds_m.v.b.c)
bud$siblings_mv = bud$m + bud$v

# adding to each fate the "number of new shoots" information --------------
for (i in 1:max(unique(met$shoot_ID))) {
  for (j in 1:max(unique(met$rank_node))) {
    for (u in 1:max(bud$nb_new_shoots)) {
      F_sh = new_shoots[new_shoots$shoot1yo == i &
                          new_shoots$rank1yo == j &
                          new_shoots$X.newshoot2yo == u, "fate"]#fate parental bud in new.shoot df
      F_bu = bud[bud$shoot_ID == i &
                   bud$rank_node == j &
                   bud$nb_new_shoots == u, "fate"]#fate parental bud in bud df
      diff = setdiff(F_bu, F_sh)#difference between fates?
      if (length(diff) != 0) {
        #if yes
        for (uww in 1:length(diff)) {
          #compute the length of the differences
          ul = diff[uww]#find the differences
          bud[bud$shoot_ID == i &
                bud$rank_node == j &
                bud$nb_new_shoots == u &
                bud$fate == ul, "nb_new_shoots"] = 0#zero new shoots
        }
      }
      if (length(F_sh) != 0) {
        #if there is more than 1 bud at that rank
        len = abs(length(F_bu) - length(F_sh))#are the two fate different?
        if (len >= 1) {
          #if yess
          Z = intersect(F_bu, F_sh)#same bud
          KQ = as.data.frame(table(grep(
            paste(Z, collapse = "|"), F_bu, value = T
          )))
          Kq = as.data.frame(table(grep(
            paste(Z, collapse = "|"), F_sh, value = T
          )))
          p = abs(KQ$Freq - Kq$Freq)#gemma in pi? nel dafabase bud.fat
          if (length(p) != 0 & sum(p) >= 1) {
            d = which(p >= 1, arr.ind = T)
            for (x in 1:length(d)) {
              e = as.character(KQ[d[x], 1])#gemma in pi?
              bud[bud$shoot_ID == i &
                    bud$rank_node == j &
                    bud$nb_new_shoots == u &
                    bud$fate == e, "nb_new_shoots"][1:p[d[x]]] = 0
            }
          }
        }
      }
    }
  }
}

new = new_shoots[new_shoots$X.newshoot2yo != 0, ]#eliminiamo righe senza nuovi germogli
budy = bud[bud$nb_new_shoots != 0, ]#eliminiamo righe senza nuovi germogli
new = new[with(new, order(shoot1yo, rank1yo, fate)),]#make the same order so that i can paste
budy = budy[with(budy, order(shoot_ID, rank_node, fate)),]#make the same order so that i can paste
all.equal(budy$fate, new$fate)#check if they are actually identical
all.equal(budy$rank_node, new$rank1yo)#check if they are actually identical
sacco = data.frame(matrix(nrow = 0, ncol = 0))
sacco = cbind(budy, new[14:22])
colnames(sacco)[c(29:31)] = paste0(colnames(sacco)[c(29:31)], ".1")
pieno = bud[bud$nb_new_shoots == 0, ]#eliminiamo righe con nuovi germogli
bud = rbind.fill(sacco, pieno)#unisci i df con germogli (sacco) e senza (pieno)
bud = bud[with(bud, order(shoot_ID, rank_node)),]#order

rm(Kq)
rm(KQ)
rm(pieno)
rm(sacco)

#change variables type
str(bud)
bud$fate = factor(bud$fate)
#changing NUMBER of new shoots to 0 (not developed) or 1 (developed)
bud[bud$nb_new_shoots > 1, "nb_new_shoots"] = 1

# keep the info regarding nuts and clusters just for one bud (remo --------
for (i in 1:(nrow(bud) + 1)) {
  sh = bud[i, "shoot_ID"]#store the shoot
  ra = bud[i, "rank_node"]#store the rank
  cl = bud[i, "cl"]#store the number of cluster
  if (!is.na(cl)) {
    #if there is a cluster
    if (nrow(bud[bud$shoot_ID == sh &
                 bud$rank_node == ra, ]) > 1) {
      #count the number of buds in that rank
      if (cl != 0) {
        #if the number of cluster is not 0 (== if there is a nut)
        fa = bud[bud$shoot_ID == sh &
                   bud$rank_node == ra, c("fate", "nb_new_shoots", "cic.1")] #store the fate of the buds
        fa.pox = fa[fa$fate == "M" & fa$nb_new_shoots != 0 &
                      fa$cic.1 != 0, ]
        for (j in 1:cl) {
          if (dim(fa.pox)[1] != 0) {
            m = grep("M", fa.pox$fate)[j]
          } else {
            m = grep("M", fa$fate)[j]
          }
          if (!is.na(m)) {
            #if there is not mixed bud (nut=0 and cluster=0)
            bud[bud$shoot_ID == sh &
                  bud$rank_node == ra, c("cl", "nu")][-m, ] = 0
          }
        }
      }
    }
  }
}
#write a bud level df
names(bud)[22]="nb_new_shoots"
write.csv(bud, "Data/Modified/foundnxtyr_bud_lev.csv", row.names = F)
