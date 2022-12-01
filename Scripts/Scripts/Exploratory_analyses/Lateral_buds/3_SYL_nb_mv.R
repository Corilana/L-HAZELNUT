#AIM: discover number of buds in sylleptic shoots (metamer level)
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")

library(RColorBrewer)
library(gridExtra)
library(tidyr)

#visualize plot
plot(tot_buds_m.v~parent_length_cm,met.sylleptic, pch=19)
plot(tot_buds_m.v~parent_rank_node,met.sylleptic, pch=19)
plot(tot_buds_m.v~abs_norm_median_distance,met.sylleptic, pch=19)
plot(tot_buds_m.v~abs_median_distance,met.sylleptic, pch=19)
plot(tot_buds_m.v~median_distance,met.sylleptic, pch=19)
plot(tot_buds_m.v~norm_median_distance,met.sylleptic, pch=19)
#doesn't seems to have a correlation 

sum(met.sylleptic$b)#0

#frequency table of number of multiple buds in sylleptic shoots per each rank node
rank.freq = table(met.sylleptic$parent_rank_node, met.sylleptic$tot_buds_m.v)

#graph
png("Outputs/Plots/3_SYL_nb_mv~rank.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
cols <- brewer.pal(n = length(colnames(rank.freq)), name = "Set3")
x <-
  barplot(
    t(rank.freq),
    col = cols,
    main = "number of mv buds into sylleptic",
    xlab = "Rank nodes of parental",
    ylab = "# of buds in sylleptic"
  )
legend(
  "topright",
  inset = c(-0.03, -0.1),
  xpd = TRUE,
  legend = rownames(t(rank.freq)),
  fill = cols,
  cex = 0.8
)
dev.off()

#relative frequency table of number of multiple buds in sylleptic shoots per each rank node
rel.rank.freq = prop.table(rank.freq, margin = 1) * 100
head(rel.rank.freq, 20)
#graph
png("Outputs/Plots/3_SYL_freq_mv~rank.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
cols <- brewer.pal(n = length(colnames(rel.rank.freq)), name = "Set3")
x <-
  barplot(
    t(rel.rank.freq),
    col = cols,
    main = "% of mv buds in sylleptic",
    xlab = "Rank nodes of parental",
    ylab = "% of buds in sylleptic",
    ylim = c(0, 100)
  )
legend(
  "topright",
  inset = c(-0.03, -0.1),
  xpd = TRUE,
  legend = rownames(t(rel.rank.freq)),
  fill = cols,
  cex = 0.8
)
dev.off()

#frequency table of type of multiple buds in sylleptic shoots per each rank node
nline = length(met.sylleptic$tesi)
tot = c("c","v","m","b")
#change the number of buds with the letter (es. 1v =v)
for (i in 1:nline) {
  for (j in 1:length(tot)) {
    J = tot[j]
    l = met.sylleptic[i, J]
    if (l != 0) {
      met.sylleptic[i, J] = paste0(rep(J, each = l), collapse = "+")
    } else {
      met.sylleptic[i, J] = NA
    }
  }
}

#merge with + es( v and m = v+m)
ne = ncol(met.sylleptic) + 1
nline = length(unique(met.sylleptic$shoot_ID))
nliner = length(unique(met.sylleptic$parent_rank_node))

for (i in 1:nline) {
  I = unique(sort(met.sylleptic$shoot_ID))[i]
  for (t in 1:nliner) {
    K = unique(sort(met.sylleptic$parent_rank_node))[t]
    met.sylleptic[met.sylleptic$shoot_ID == I &
                    met.sylleptic$parent_rank_node == K, ne] <-
      unite(met.sylleptic[met.sylleptic$shoot_ID == I & met.sylleptic$parent_rank_node == K, tot],
            col =
              "merge",
            na.rm = TRUE,
            sep = '+')
    
  }
}
bud_type=table(met.sylleptic$parent_rank_node, met.sylleptic$merge)
bud_type.matrix = as.data.frame.matrix(bud_type)
head(bud_type.matrix, 20)

#graph
png("Outputs/Plots/3_SYL_nb_mv_multiple~rank.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <-c(brewer.pal(n = 12, name = "Set3"),brewer.pal(n = 10, name = "Paired"))         
x <-
  barplot(
    t(bud_type.matrix),
    col = cols,
    main = "combination of buds in sylleptic  shoots",
    xlab = "Rank nodes of parental",
    ylab = "# of observation"
  )
legend(
  "topright",
  inset = c(-0.3,+0.01),
  xpd = TRUE,
  legend = rownames(t(bud_type.matrix)),
  fill = cols,
  cex = 0.6
)
dev.off()

#relative frequency table of type of multiple buds in sylleptic shoots per each rank node
bud_type.freq=prop.table(bud_type, margin =1) * 100
bud_type.freq.matrix = as.data.frame.matrix(bud_type.freq)
head(bud_type.freq.matrix, 20)

#graph
png("Outputs/Plots/3_SYL_freq_mv_multiple~rank.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(mar = c(5, 5, 4, 10))
cols <-c(brewer.pal(n = 12, name = "Set3"),brewer.pal(n = 10, name = "Paired"))         
x <-
  barplot(
    t(bud_type.freq.matrix),
    col = cols,
    main = "combination of buds in sylleptic  shoots",
    xlab = "Rank nodes of parental",
    ylab = "% of observation",
    ylim = c(0, 100)
  )
legend(
  "topright",
  inset = c(-0.3,+0.01),
  xpd = TRUE,
  legend = rownames(t(bud_type.freq.matrix)),
  fill = cols,
  cex = 0.6
)
dev.off()