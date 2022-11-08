#AIM1: discover where are sylleptic shoots (metamer level)
#AIM2: NB of observations (buds + sylleptic shoots) in proleptic 1yo shoots per CLASS LENGHT
#AIM3: NB of observations (buds + sylleptic shoots) in proleptic 1yo shoots per RANK
#AIM4: multiple buds in proleptic 1yo shoots per RANK
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
library(RColorBrewer)
library(tidyr)

#AIM1: discover where are sylleptic shoots (metamer level) --------------
#visualize plot
plot(shoot_type~Length,met)
plot(shoot_type~rank_node,met)
plot(shoot_type~median_distance,met)
plot(shoot_type~normal_distance,met)
plot(shoot_type~distance_abs,met)
plot(shoot_type~median_distance_norm,met)

#it seems that sylleptic shoots are more from rank 4 to 8 

# AIM2: NB of observations (buds + sylleptic shoots) in proleptic 1yo shoots per CLASS LENGHT --------------------------------------------------------
class.length = met.proleptic[0, 0]
for (i in levels(met.proleptic$class)) {
  P = met.proleptic[met.proleptic$class == i, ]
  class.length[i, "class"] = i
  class.length[i, "nb_sylleptic"] = sum(P["sylleptic"])
  class.length[i, "nb_v"] = sum(P["v"])
  class.length[i, "nb_m"] = sum(P["m"])
  class.length[i, "nb_b"] = sum(P["b"])
}
class.length["sums", -1] = colSums(class.length[, -1])#sums each observations(obs)
class.length[, "sums"] = rowSums(class.length[-1])#sum obserbations per each class
print(class.length)

#relative frequency
#nb_single obs/sum obs per each class
nline = length(levels(met.proleptic$class))
for (i in 1:nline) {
  class.length[i, 7:10] = (class.length[i, 2:5] / class.length[i, 6]) * 100
}
colnames(class.length)[7:10]
colnames(class.length)[7:10] = c("%sylleptic", "%V", "%M", "%B")

#write pdf with the table
pdf("Outputs/Tables/PRO_obs~class.pdf",height = 8,width = 10)
grid.table(class.length)
dev.off()

freq = as.matrix(t(class.length[1:4, 7:10]))
rownames(freq)
rownames(freq)=gsub("%","",rownames(freq))

#graph
png("Outputs/Plots/2_PRO_freq_obs~class.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(freq,col = cols,beside = T,
          names.arg = colnames(freq), main="nb lateral buds or shoot class", xlab = "parent class length", ylab="%", ylim = c(0,100))
legend("topright",inset=c(0,0),xpd = TRUE, legend = rownames(freq),fill = cols, cex=0.8)
#stat test
colnames(class.length)[2:5]
for (i in 2:5) {
  test=prop.test(class.length[1:4,i], class.length[1:4,6])
  print(colnames(class.length)[i])
  print(test)
  if(test$p.value<=0.05){
    print(pairwise.prop.test(class.length[1:4,i], class.length[1:4,6]))
  } else {
    print(paste0(colnames(class.length)[i]," ","p.value= ",test$p.value))
  }
}
#legend
text(x[1,], freq[1,]+5, c("a","a","a","b"), col = cols[1])
text(x[2,], freq[2,]+5, c("a","ab","ab","a"),col = cols[2])
text(x[3,], freq[3,]+5, c("b","a","a","a"),col = cols[3])
text(x[4,], freq[4,]+5, c("a","a","b","b"),col = cols[4])
dev.off()

#graph
png("Outputs/Plots/2_PRO_nb_obs~class.png",width=1200, height=900, res=150)# save plot
par(mar = c(5, 5, 4, 6))
cols<-brewer.pal(n=length(colnames(class.length[1:4,2:5])),name="Set2")
x<-barplot(as.matrix(t(class.length[1:4,2:5])),names.arg = class.length[1:4,1], beside=T,col = cols, main="freq lateral buds or shoot class", xlab="parental class length", ylab="number")
legend("topright",inset=c(-0.13,0),xpd = TRUE, legend = rownames(freq),fill = cols, cex=0.8)
dev.off()

#relative frequency 
#nb_single obs/sum obs total
freq.general = (colSums(class.length[1:4,2:5])/class.length[5,6])*100
freq.general

#graph
png("Outputs/Plots/PRO_nb_obs.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")
x=barplot(freq.general,col = cols,beside = T, names.arg = colnames(freq.general), main="freq lateral buds or shoot class", xlab = "buds/sylleptic", ylab="%", ylim = c(0,100))
text(x,freq.general+3 , paste0(round(freq.general, digit=2),"%"), cex = 1)
#test
prop.test(as.numeric(class.length[5,2:5]), rep(class.length[5,6],4))
pairwise.prop.test(as.numeric(class.length[5,2:5]), rep(class.length[5,6],4))
#etichette
text(x,freq.general+10, c("b","a","a","c"), cex = 1)
dev.off()

# AIM3: NB of observations (buds + sylleptic shoots) in proleptic 1yo shoots per RANK ----------------------------------------------------------------
class.rank = met.proleptic[0, 0]
nline = length(unique(sort(met.proleptic$rank_node)))

for (q in 1:nline) {
  Q = unique(sort(met.proleptic$rank_node))[q]
  class.rank[paste0(Q), "rank_node"] = Q
  class.rank[paste0(Q), "nb_shoots"] = length(met.proleptic[met.proleptic$rank_node == Q, "shoot_ID"])
  #sylleptic for each parental node?
  class.rank[paste0(Q), "sylleptic"] = sum(met.proleptic[met.proleptic$rank_node == Q, "sylleptic"])
  class.rank[paste0(Q), "v"] = sum(met.proleptic[met.proleptic$rank_node == Q, "v"])
  class.rank[paste0(Q), "m"] = sum(met.proleptic[met.proleptic$rank_node == Q, "m"])
  class.rank[paste0(Q), "b"] = sum(met.proleptic[met.proleptic$rank_node == Q, "b"])
}
class.rank
class.rank["sums", 2:6] = colSums(class.rank[2:6])#sums each observations(obs)
class.rank[, "sum_obs"] = rowSums(class.rank[3:6])#sum obserbations per each node
print(class.rank)

#relative frequency 
#nb_single obs/sum obs per each rank
for (i in 1:nline) {
  class.rank[i, 8:11] = round((class.rank[i, 3:6] / class.rank[i, 7]) * 100,2)
}
colnames(class.rank)[8:11]
colnames(class.rank)[8:11] = c("%sylleptic", "%V", "%M", "%B")

#write pdf with the table
pdf("Outputs/Tables/PRO_obs~rank.pdf",height = 8,width = 10 )
grid.table(class.rank)
dev.off()

#graph
png("Outputs/Plots/2_PRO_freq_obs~rank.png.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,4))
with(class.rank[1:16,], plot(`%sylleptic`~rank_node, pch=1, cex=1.2,ylim = c(0,100), main="frequence buds/shoots in proleptic <own-rooted> parentals", xlab="rank nodes", ylab="%", type="o"))
with(class.rank[1:16,], points(`%V`~rank_node, pch=2,cex=1.2,type="o"))
with(class.rank[1:16,], points(`%M`~rank_node, pch=15, cex=1.2,type="o"))
with(class.rank[1:16,], points(`%B`~rank_node, pch=18, cex=1.2,type="o"))
legend("topleft", legend=c("%sylleptic", "%V", "%M", "%B"),lty=1, cex= 0.8, pch=c(1,2,15,18))
dev.off()

# AIM4: SUM of observations (buds + sylleptic shoots) in proleptic --------
freq.rank = table(met.proleptic$rank_node, met.proleptic$tot_buds_mvb)
print(freq.rank)

#graph
png("Outputs/Plots/2_PRO_nb_multiple~rank.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(freq.rank[1:16,])),name="Set3")
x<-barplot(t(freq.rank[1:16,]),col = cols,main="nb of buds/sylleptic per rank in proleptic shoots", xlab= "Rank nodes", ylab="# of multiple buds/sylleptic")
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(freq.rank[1:16,])),fill = cols, cex=0.8)
dev.off()

#relative frequency table of number of multiple buds in proleptic shoots per each rank node
rel.freq.rank = prop.table(freq.rank, margin = 1) * 100
head(rel.freq.rank, 20)

#graph
png("Outputs/Plots/2_PRO_freq_multiple~rank.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=length(colnames(rel.freq.rank[1:16,])),name="Set3")
x<-barplot(t(rel.freq.rank[1:16,]),col = cols, main="% of buds/sylleptic per rank in proleptic shoots", xlab= "Rank nodes", ylab="% of multiple buds", ylim=c(0,100))
legend("topright",inset=c(-0.03,-0.1),xpd = TRUE, legend = rownames(t(rel.freq.rank[1:16,])),fill = cols, cex=0.8)
dev.off()

#frequency table of type of multiple buds in proleptic shoots per each rank node
nline = length(met.proleptic$tesi)
tot = c("sylleptic","v","m","b")

#change the number of buds with the letter (es. 1v =v)
for (i in 1:nline) {
  for (j in 1:length(tot)) {
    J = tot[j]
    l = met.proleptic[i, J]
    if (l != 0) {
      met.proleptic[i, J] = paste0(rep(J, each = l), collapse = "+")
    } else {
      met.proleptic[i, J] = NA
    }
  }
}
#merge with + es( v and m = v+m)
ne = ncol(met.proleptic) + 1
nline = length(unique(met.proleptic$shoot_ID))
nliner = length(unique(met.proleptic$rank_node))

for (i in 1:nline) {
  I = unique(sort(met.proleptic$shoot_ID))[i]
  for (t in 1:nliner) {
    K = unique(sort(met.proleptic$rank_node))[t]
    met.proleptic[met.proleptic$shoot_ID == I &
                    met.proleptic$rank_node == K, ne] <-unite(met.proleptic[met.proleptic$shoot_ID == I & met.proleptic$rank_node == K, tot],
                                                              col =
                                                                "merge",
                                                              na.rm = TRUE,
                                                              sep = '+')
    
  }
}

bind = as.data.frame.matrix(table(met.proleptic$rank_node, met.proleptic$merge))
head(bind, 20)

#graph
png("Outputs/Plots/2_PRO_nb_type_multiple~rank.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <-
  c(brewer.pal(name = "Spectral", n = 11)[c(2:5, 9:11)],
    brewer.pal(name = "Dark2", n = 7))
x <-
  barplot(
    t(bind[1:16, ]),
    col = cols,
    main = "# combination of buds/sylleptic in proleptic shoots ",
    xlab = "Rank nodes",
    ylab = "# of observation"
  )
legend(
  "topright",
  inset = c(-0.3,+0.01),
  xpd = TRUE,
  legend = rownames(t(bind[1:16, ])),
  fill = cols,
  cex = 0.6
)
dev.off()
#relative frequency table of type of multiple buds in proleptic shoots per each rank node
merg = as.data.frame.matrix(prop.table(table(met.proleptic$rank_node, met.proleptic$merge), margin =
                                         1) * 100)
head(merg, 20)

#graph
png("Outputs/Plots/2_PRO_freq_type_multiple~rank.png.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <-
  c(brewer.pal(name = "Spectral", n = 11)[c(2:5, 9:11)],
    brewer.pal(name = "Dark2", n = 7))
x <-
  barplot(
    t(merg[1:16, ]),
    col = cols,
    main = "% combination of buds/sylleptic in proleptictic shoots ",
    xlab = "Rank nodes",
    ylab = "% of observation",
    ylim = c(0, 100)
  )
legend(
  "topright",
  inset = c(-0.3,+0.01),
  xpd = TRUE,
  legend = rownames(t(merg[1:16, ])),
  fill = cols,
  cex = 0.6
)
dev.off()