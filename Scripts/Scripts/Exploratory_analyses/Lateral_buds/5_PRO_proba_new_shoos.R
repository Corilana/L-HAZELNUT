#AIM: Proportion of buds that sprout into shoots
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")

library(RColorBrewer)
library(gridExtra)

#visualize plot
str(MV.bud.PRO)
MV.bud.PRO$presence_new_shoots=factor(MV.bud.PRO$presence_new_shoots)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$Length)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$rank_node)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$fate)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$Length.node.)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$tot_buds)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$m)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$v)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$siblings_mv)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$distance_abs)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$median_distance)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$median_distance_norm)
plot(MV.bud.PRO$presence_new_shoots~MV.bud.PRO$normal_distance)
#seems that rank influences the fate
from_MV = table(MV.bud.PRO$rank_node, MV.bud.PRO$presence_new_shoots, MV.bud.PRO$fate)#all shoots
from_V = from_MV[, , "V"]#fate=V
from_M = from_MV[, , "M"]#fate=M
#relative frequency table of childs from M and V in proleptic shoots per each rank node
from_V.freq = prop.table(from_V, margin = 1) * 100#proportions
from_M.freq = prop.table(from_M, margin = 1) * 100#proportions

from_MV.freq=cbind(from_V.freq[, 2], from_M.freq[, 2])
from_MV.freq.matrix= as.data.frame.matrix(from_MV.freq)#%of buds developed per rank node
head(from_MV.freq.matrix)
colnames(from_MV.freq.matrix) = c("V", "M")
print(from_MV.freq.matrix)

#graph
png("Outputs/Plots/5_PRO_proba_new_shoots.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
col = brewer.pal(n = 4, name = "Set2")
with(
  from_MV.freq.matrix[1:16, ],
  plot(
    V,
    pch = 19,
    cex = 1.2,
    col = col[2],
    main = "proportion of proleptic buds developed",
    xlab = "rank nodes",
    ylab = "%",
    type = "o",
    ylim = c(0, 100)
  )
)
with(from_MV.freq.matrix[1:16, ], points(
  M,
  pch = 19,
  cex = 1.2,
  col = col[3],
  type = "o",
  ylim = c(0, 100)
))
legend(
  "bottom",
  inset = c(-0.2, 0),
  xpd = TRUE,
  legend = c("vegetative buds", "mixed buds"),
  lwd = 3,
  cex = 0.7,
  col = col[c(2, 3)]
)
dev.off()


