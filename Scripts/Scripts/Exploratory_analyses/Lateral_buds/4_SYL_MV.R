#AIM: discover number of M and V in sylleptic shoots (metamer level)
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")

library(RColorBrewer)
library(gridExtra)
library(tidyr)

#visualize plot
plot(fate~parent_length_cm,MV.bud.SYL)
plot(fate~parent_rank_node,MV.bud.SYL)
plot(fate~abs_norm_median_distance,MV.bud.SYL)
plot(fate~abs_median_distance,MV.bud.SYL)
plot(fate~median_distance,MV.bud.SYL)
plot(fate~siblings_mv,MV.bud.SYL)
plot(fate~norm_median_distance,MV.bud.SYL)
#doesn't seems to have a correlation 

#frequency table of buds in sylleptic shoots per each class (count )
bud.type = MV.bud.SYL[0, 0]
for (i in 1:4) {
  syl_class = levels(met.sylleptic$class)[i]
  bud.type[i, "class"] = syl_class
  bud.type[i, "nb_v"] = sum(met.sylleptic[met.sylleptic$class == syl_class, "v"])
  bud.type[i, "nb_m"] = sum(met.sylleptic[met.sylleptic$class == syl_class, "m"])
  bud.type[i, "nb_b"] = sum(met.sylleptic[met.sylleptic$class == syl_class, "b"])
  bud.type[i, "nb_c"] = sum(met.sylleptic[met.sylleptic$class == syl_class, "c"])
}
bud.type["sums", -1] = colSums(bud.type[, -1])#sums each observations(obs)
bud.type[, "sums"] = rowSums(bud.type[-1])#sum obserbations per each class
print(bud.type)

#just m and v (that should be the only buds in sylleptic)
bud.mv = bud.type[c(1:3, 6)]
bud.mv$sums=bud.mv$nb_v+bud.mv$nb_m
#write pdf with the table
pdf("Outputs/Tables/4_SYL_mv~class.pdf",height = 3,width = 5 )
grid.table(bud.mv)
dev.off()

#are there differences between class length?
colnames(bud.mv)[2:3]
for (i in 2:3) {
  test=prop.test(bud.mv[1:4,i], bud.mv[1:4,4])
  if(test$p.value<=0.05){
    print(colnames(bud.mv)[i])
    print(pairwise.prop.test(bud.mv[1:4,i], bud.mv[1:4,4]))
  } else {
    print(paste0(colnames(bud.mv)[i]," ","p.value= ",test$p.value))
  }
}

#relative frequency table of buds/shoots in sylleptic shoots per each class
bud.mv.matrix=as.matrix(bud.mv[5, 2:3])
bud.mv.freq=prop.table(bud.mv.matrix, 1)*100
bud.mv.freq=t(round(bud.mv.freq, 2))
colnames(bud.mv.freq) = "% in sylleptic"
rownames(bud.mv.freq) = c("%v", "%m")
print(bud.mv.freq)

# syl count
png("Outputs/Plots/4_SYL_freq_MV.png",width=1200, height=900, res=150)# save plot
cols<-brewer.pal(n=4,name="Set2")[c(2,3)]
x=barplot(bud.mv.freq,col = cols, names.arg = rownames(bud.mv.freq),beside = T, main="frequence buds in sylleptic", xlab = "buds", ylab="%", ylim = c(0,100))
text(x,bud.mv.freq+3 , paste0(round(bud.mv.freq, digit=2),"%"), cex = 1)
dev.off()

