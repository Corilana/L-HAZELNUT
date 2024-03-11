#AIM1: relationship between nuts and other parameters
#AIM2: how many nuts in Very long shoots per rank
#AIM3: relationship between average lenght new glomerul and number of nuts and ye per rank node
#data:deruta 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

library(RColorBrewer)

tot=bud[bud$fate == "M", ]
#glomerule is the new shoots from M buds
glom=bud.proleptic[bud.proleptic$fate=="M",]
names(glom)
glom=glom[c("shoot_ID","length","class","rank_node","fate","nu","cl","length2yo")]

# AIM1: relationship between nuts and other parameters
names(tot)
plot(tot$nu~tot$length)
plot(tot$nu~tot$class)
plot(tot$nu~tot$`Length(node)`)
plot(tot$nu~tot$median_distance)
plot(tot$nu~tot$abs_norm_median_distance)
plot(tot$nu~tot$abs_median_distance)
plot(tot$nu~tot$siblings_mv)
plot(tot$nu~tot$shoot_type)
plot(tot$nu~tot$length2yo)

# AIM2: how many nuts in Very long shoots per rank ------------------------
plot(glom$nu~glom$class)
V_long = glom[glom$class == "VLo", ]
#fare una tabella con_ numero di nocciole, numero di fiori M e numero di nuovi germogli
df = bud[0, 0]
nliner = unique(sort(V_long$rank_node))
for (rank in nliner) {
  P = V_long[V_long$rank_node == rank, ]
  n_sh = length(unique(P$shoot_ID))#number shoots
  n_m = length(P$fate)#number mixed buds
  n_nuts = sum(P$nu)#number nuts
  n_cls = sum(P$cl)#number clusters
  allegagione = n_cls / n_m#allegagione
  nut_set = (n_nuts / n_m)*100#nut set
  df = rbind(df,
             cbind(rank, n_sh, n_m, n_cls, allegagione, n_nuts, nut_set))
}
head(df)

# graph
# png("Own_rooted_young/Outputs/Plots/nut_freq~rank_VERYLONG.png",width = 1200,height = 900,res = 150)# save plot
par(mar = c(5, 5, 5, 4))
col = brewer.pal(n = 4, name = "Set1")
with(
  df,
  plot(
    n_nuts ~ rank,
    pch = 19,
    col = col[1],
    main = "freq_nuts_for all lengths in proleptic",
    xlab = "rank nodes",
    ylab = "#/%",
    type = "o",
    ylim = c(0, 360)
  )
)
with(df, points(
  df$n_m ~ rank,
  pch = 19,
  col = col[2],
  type = "o"
))
with(df, points(
  df$nut_set ~ rank,
  pch = 19,
  col = col[3],
  type = "o"
))
legend(
  "topright",
  legend = c("#nuts", "#mixed","nut_set*100"),
  pch = 19,
  cex = 0.75,
  col = col
)
# dev.off()

# AIM3: relationship between average lenght new glomerul and numbe --------
plot(glom$nu~glom$length2yo)

freq_glom = bud[0, 0]
nliner = unique(sort(glom$rank_node))
for (rank in nliner) {
  P = glom[glom$rank_node == rank, ]
  n_sh = length(unique(P$shoot_ID))#number shoots
  n_m = length(P$fate)#number mixed buds
  n_nuts = sum(P$nu)#number nuts
  n_cls = sum(P$cl)#number clusters
  allegagione = n_cls / n_m#allegagione
  nut_set = (n_nuts / n_m)*100#nut set
  av_len = mean(P$length2yo,na.rm = T)
  se_len = sd(P$length2yo,na.rm = T)/sqrt(length(P$length2yo))
  freq_glom = rbind(freq_glom,
             cbind(rank, n_sh, n_m, n_nuts, n_cls,allegagione, nut_set, av_len, se_len))
}
freq_glom

# png("Own_rooted_young/Outputs/Plots/nut_freq~rank_glomlength.png",width = 1200,height = 900,res = 150# save plot
par(mar = c(5, 5, 4, 4))
col = brewer.pal(n = 5, name = "Set1")
with(
  freq_glom,
  plot(
    av_len ~ rank,
    pch = 19,
    col = col[5],
    xlab = "rank nodes",
    ylab = "#/%",
    type = "o",
    ylim = c(0, 20)
  )
)
with(freq_glom, points((freq_glom$n_nuts / 10) ~ rank,
                pch = 19,
                col = col[1],
                type = "o"
))
with(df, points((df$nut_set / 100) ~ rank,
                pch = 19,
                col = col[4],
                type = "o"
))
legend(
  "topleft",
  legend = c("average length glomerul(cm)", "#nuts/10", "nut_set"),
  pch = 19,
  cex = 0.75,
  col = col[c(5, 1, 4)]
)
# dev.off()
